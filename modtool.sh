#! /bin/sh
# http://www.shsforums.net/files/file/534-big-world-v11-english/
# mod status
# TODO««
#  - display status of all mods
#  - use getopt[s]
#  - auto-downloading to appropriate directory
#  - check lefreuit ui on "clean" install
#  - categories:
#  - rewrite install file format (e.g. putting mods in order)
#  - separate bg1/bg2
#  - add missing mod(s):
#
# * FIXES
# Area_Patcher
# BWP_Fixpack
# lolfixer
# EET_gui
#
# * MISC
# + A7-ConvenientEENPCs
# + BG1NPCSoA (including 1 new area + quest, breaks Dynaheir+Tiax)
# BWS_workspace
# BiG World Installpack
# BigWorldSetup-Fixpack
# ? EETGBThfKp (thief stronghold is annoying)
# - Isaya-bg2eetrans-d316d0c
# + Item_Patcher
# + JanQuest
# - LeUI
# * Manuals

# * ITEMS
# dakkon
# * GRAPHICS
# Sentrizeal_AA_Pack (graphics)
# big world fixpack
# bp-bgt-worldmap
# characters
# hiddenadventure
# iconv
# iwdification
# mih_ip
# mod-main-folder
# movies
# music
# override
# pcvamp
# planarspheremod
# res_fixer
# sarevokfriendship
# scripts
# sellswords
# weidu_external
# worldmap
#
# * Tactics
# stratagems_external
#
# * NPCs, install after:
# gavin_bg2 (Paladin)
# Neh'taniel (Paladin)
# NPC Recorder
# NPC Saerileth
# NPC KimPirate (late)
# SimeNPC (NPC)
# Sirene_BG2 (NPC)
# aurenaseph (NG Human Fighter)
# kelsey
# NeeraBanters
# tashia
# neera
# Coran
#
# * TO REMOVE
# UnofficialItemPack
# CandleMem
# disobedience
#»»
# Configuration variables««1
# Preferred language
use_lang=fr_FR
GAMELANG=".*Fran.* .*fran.* .*Fren.* .*fren.* .*English.* .*"

# Game location
BG1GAMEDIR=$HOME/jeux/ciopfs/bg1/game
BG2GAMEDIR=$HOME/jeux/ciopfs/bg2/game

# BWS files
BWSROOT=$HOME/jeux/ciopfs/EE-Mod-Setup-Master
BWSUSER=$BWSROOT/User.ini
# App/Config/User.ini
BWSMODLIST=$BWSROOT/App/Config/BG2EE/Mod.ini
BG2INSTALLORDER=$BWSROOT/App/Config/BG2EE/InstallOrder.ini
BG1INSTALLORDER=$BWSROOT/App/Config/BG1EE/InstallOrder.ini

# our own files
PREFIX=$HOME/jeux/ciopfs/modtool
MODDB=$PREFIX/moddb # big library of all mods
CONFIG=$PREFIX/config # user configuration

# these should be on a case-insensitive filesystem (ciopfs):
TMP=$PREFIX/tmp
DOWN=$PREFIX/down
EXTRACT=$PREFIX/extract
# -w and not -d, since e.g. "$DOWN" might be a symlink
for x in "$DOWN" "$TMP" "$EXTRACT"; do [ -w "$x" ] || mkdir "$x"; done

opt_simulate=1
opt_quiet=''
opt_pause=''
opt_bg1=''
opt_bg2=''

# Utilities ««1
modlog() { [ -n "$opt_quiet" ] || echo "$logprefix$@"; }
die() { echo "\e[31;1m$@\e[m" >&2; exit 1; }
quotemeta() { echo -n "$@"|sed -e 's/[[/*+()]/\\&/'; }
read_config() { < "$CONFIG" sed -e '/^\s*#/d;/^\s*$/d'; }
ln_relative() {
	from="$1"; file="$2"; to="$3"
	ln -s $(realpath --relative-to="$to" "$from")/$file $to
}
# do_action: either on all configured mods, or on command-line arguments««2
do_action() {
	action="$1"; shift
# 	echo "doing action '$action'($interactive) ($@)"
	if [ -n "$*" ] ; then
		for moddir in "$@" ; do
			mod_set_properties; $action
		done
		return
	fi
	[ -n "$interactive" ] && \
		die "Interactive action '$action' needs a mod name"
	nmods=$(lsmods|wc -l); modidx=0
	lsmods | while read moddir; do
		modidx=$(($modidx+1))
		mod_set_properties; $action
		echo $action "($modidx/$nmods) $moddir"
	done
}
# TP2 parsing ««1
# All the tp2_* functions read the .tp2 file on standard input.
# mod_tp2: find tp2 file associated with mod (in current directory)««2
mod_tp2() {
	mod_extract || return $?
	[ -d "$EXTRACT/$moddir" ] || return 1
	for base in "$moddir" "$moddir/$moddir" "$moddir/setup-$moddir"; do
		modtp2="$EXTRACT/$base.tp2"; [ -f "$modtp2" ] && return 0
	done
	modtp2=''; return 1
}

# tp2_words: splits a tp2 file to words««2
tp2_words() {
	tr -d '\r' | sed -e 's,//.*$,,; /\/\*/,/\*\//d; s/^\s*//; /^$/d;
		/^~/{ s/^~//; s/~\s*/\n/; P;D};
		/^"/{ s/^"//; s/"\s*/\n/; P;D};
		/\s/{ s/\s/\n/; P;D}'
}
# tp2_list_languages: list all language names in this file««2
tp2_list_languages() {
	tp2_words | while read text; do
		[ "$text" = 'LANGUAGE' ] || continue
		read line; echo "$line"
	done
}
# tp2_bestlang: find best language for tp2 given $GAMELANG ««2
tp2_bestlang() {
	rettype="$1" # if 't' then return text of this language
	nlang=0; nbest=''; pbest=999; tbest=''
	tp2_list_languages | {
	while read langname ; do
# 		echo "language $nlang is $langname"
		p=0; for pattern in $(echo $GAMELANG|tr ' ' '\n'); do
			p=$(($p+1))
			echo "$langname" |grep -q "^$pattern" || continue
			[ "$p" -gt "$pbest" ] && continue
			nbest="$nlang"; pbest=$p; tbest="$langname"
		done
		nlang=$(($nlang+1))
	done;
	if [ "$rettype" = "t" ] ; then echo "$tbest"; else echo "$nbest"; fi; }
}
# tp2_lang_dir, tp2_lang_tra: access variables for n-th language ««2
tp2_lang_dir() {
	n="$1"; i=-1
	tp2_words | while read text; do
		[ "$text" = 'LANGUAGE' ] || continue
		i=$(($i+1)); [ "$i" -eq "$n" ] || continue
		read text; read text; echo "$text"
	done
}
tp2_lang_tra() {
	n="$1"; i=-1
	tp2_words | tr '\\' '/'| while read -r text; do
		[ "$text" = 'LANGUAGE' ] || continue
		i=$(($i+1)); [ "$i" -eq "$n" ] || continue
		read -r text; read -r text;
		while read -r text; do
			[ "$text" = 'LANGUAGE' -o "$text" = 'BEGIN' ] && break
			echo "$text"
		done
	done
}
# tp2_status: list of installed components of tp2 (in current directory)««2
tp2_status() {
	tp2=$1; shift
	weidu_log="WeiDU.log"
	[ -f $weidu_log ] || return
	lcname=$(echo "$tp2"|tr 'A-Z' 'a-z')
	< $weidu_log tr -d '\r' | tr 'A-Z' 'a-z' | \
	sed -e 	's/~\(.*\)~ #\([0-9]\+\) #\([0-9]\+\).*/\1+\2+\3/;t;d' | \
	while IFS='+' read xtp2 xlang xcomp ; do
		[ "$xtp2" = "$lcname" ] || continue
		echo "$xcomp"
	done
}
# tp2_readme and tp2_lang_readme««2
tp2_readme() { tp2_words | sed -e '1,/^README$/d;/^[A-Z_]*$/Q;'; }
tp2_lang_readme() {
	langdir="$1"
	tp2_readme | sed -e "s/%LANGUAGE%/$langdir/" | while read f; do
		[ -f "$EXTRACT/$f" ] || continue
		echo "$EXTRACT/$f"; return
	done
}
# tp2_components: list all components for this mod««2
tp2_components() {
	compid=-1
	tp2_words|while read line; do
		if [ "$line" = 'BEGIN' ] ; then
			read name
			echo "$name" | grep -q '^@' || continue
			read line
			if [ "$line" = 'DESIGNATED' ] ; then
				read compid
			else
				compid=$((compid+1))
			fi
			echo "$compid;$name"
		fi
	done
}
# BWS importation ««1
# Tools ««2
ini_section() {
	file="$1"; shift
	section="$1"; shift
	pattern=$(quotemeta "$section")
	< $file tr -d '\r' | sed -e "1,/\[$pattern]/d; /^#/d; /^\[/Q"
}
bws_user_config() { ini_section "$BWSUSER" "Save"; }
bws_mod_property() { ini_section "$BWSMODLIST" "$1"|sed -e "s/$2=//;t;d"; }
# bws_mod_gameversion: find appropriate game version for this mod ««2
bws_mod_gameversion() {
	pattern=$(quotemeta "$1")
	v1=$(< "$BG1INSTALLORDER" tr -d '\r' |\
		grep -q "\\(SUB\\|STD\\|MUC\\);$pattern;" && echo 1||echo 0)
	v2=$(<"$BG2INSTALLORDER" tr -d '\r' | sed -e '1,/;EET;/d' |\
		grep -q "\\(SUB\\|STD\\|MUC\\);$pattern;" && echo 1||echo 0)
	echo "$v1\t$v2"
}

# Generate moddb ««2
generate_moddb() { write_moddb > $MODDB; }
write_moddb() {
	cat <<"EOF"
# List of all known mods.
# Mods are sorted according to some suggested (but not perfect) install
# order.
# Each line is of the form MODDIR\tBG1?\tBG2?\tZIP\tURL\tDESC
# MODDIR is the key for this mod (i.e. dir for weinstall)
# BG1?, BG2? are either 0 or 1
# ZIP is the filename of the downloaded archive
# DESC is human-readable description of mod
EOF
	done_names=','
	tac $BG1INSTALLORDER $BG2INSTALLORDER |\
	grep '^\(STD\|MUC\|SUB\);' | cut -d';' -f2 |uniq|\
	while read moddir; do
		echo "$done_names"|grep -Fqi "$moddir" && continue
		done_names="$done_names,$moddir"
		url=$(bws_mod_property "$moddir" Down)
		zip=$(bws_mod_property "$moddir" Save)
		desc=$(bws_mod_property "$moddir" Name)
		vers=$(bws_mod_gameversion "$moddir")
		[ -z "$zip" -o -z "$desc" ] && continue
		echo "$desc"|grep -q '	' && die "bad description: $desc"
		echo "$url"|grep -q '	' && die "bad description: $url"
		echo "$zip"|grep -q '	' && die "bad description: $zip"
		echo "$moddir\t$vers\t$zip\t$url\t$desc"
	done | tac
}
# Import user config ««2
# We use the same format as BWS user config's [Save] section for simplicity
import_user_config() {
	{ cat <<"EOF"
# List of desired mods, *in installation order*.
# Format: mod=[component list]
# This file was imported from a BWS config but may be edited by hand.
# Commented lines and blank lines are ignored.
# (Warning: some mods have a mesh symbol inside their names,
# so lines *cannot* be partly commented).
EOF
	bws_user_config > "$CONFIG"; }
}
# Mod database handling ««1
# Read mod properties««2
read_moddb() { sed -e '/^ *#/d;/^$/d' < "$MODDB"; }
mod_set_properties() {
# input: $moddir; output: $modisbg1, $modisbg2, $modurl, $modarchive, $moddesc
# (call this once, each time $moddir changes)
	pattern=$(quotemeta "$moddir")
	lineno=$(read_moddb|grep -n "^$pattern	"|sed -e 's/:.*//;q')
	[ -z "$lineno" ] && return 1
	IFS='	' read foo modisbg1 modisbg2 modarchive modurl moddesc <<EOF
$(read_moddb|head -$lineno|tail -1)
EOF
	echo "set_properties($moddir): foo=$foo url=$modurl archive=$modarchive"
}
# Mod downloading and extracting ««1
# mod_download ««2
mod_download() {
	[ -z "$modurl" -o "$modurl" = "Manual" ] && return
	while [ ! -f "$DOWN/$modarchive" ] ; do
		[ -n "$opt_simulate" ] && \
			echo "wget $modurl -O $DOWN/$modarchive" && return
		wget "$modurl" -O "$DOWN/$modarchive"
		[ $? -eq 0 ] && break
		echo "Failed to download: $modurl\n(o)ther adress, (a)bort, local (f)ile:"
		read action
		case "$action" in
			o*) otherurl=1;
				url=$(echo "$action"|sed -e 's/^o *//'); continue;;
			f*) file=$(echo "$action"|sed -e 's/^f *//')
				cp "$file" "$DOWN/$modarchive"; continue;;
			a*) return 1;;
		esac
	done
	if [ -n "$otherurl" ] ; then
		# update database with this (correct) url
		echo "Update url for this mod? (y)es (n)o"; read action
		case "$action" in
			y*|Y*) mod_update_url "$moddir" "$modurl";;
		esac
	fi
}
# mod_update_url ««2
mod_update_url() {
	url="$1"
	pattern=$(quotemeta "$moddir"); rep=$(quotemeta "$url")
	sed -i -e 's/\('"$pattern"'\t\(\S\+\t\)\{3\}\)\S\+\(\t\)/\1'"$rep"'\3/' \
		$MODDB
}
# download_mods ««2
# mod_extract_here ««2
mod_extract_here() {
	[ -d "$EXTRACT/$moddir" ] && return
	echo "unzipping ($moddir): $modarchive"
	case "$modarchive" in
		*.zip) unzip "$DOWN/$modarchive";;
		*.7z) 7zr x "$DOWN/$modarchive";;
		*.rar) unrar "$DOWN/$modarchive";;
		*.tar.bz2|*.tar.gz|*.tar.7z) tar xf "$DOWN/$modarchive";;
		*) die "Unknown archive format: '$DOWN/$modarchive'";;
	esac
	# find the correct subdirectory, move it to top, remove dirt
	if [ "$(ls|wc -l)" -eq 1 ] ; then
		if [ -d "$(ls)" -a ! -d "$moddir" ] ; then
			echo "only one directory exists, moving everything up"
			mv */* .; rmdir --ignore-fail-on-non-empty
		fi
	fi
}
# mod_extract ««2
mod_extract() {
	mod_download || return $?
	tmp=$(mktemp -d "$TMP/extract.XXXXXX")
	cd $tmp; mod_extract_here "$moddir"
	[ "$modisbg1" -eq 1 ] && \
		ls | while read f; do ln_relative "$EXTRACT" "$f" "$BG1GAMEDIR"; done
	[ "$modisbg2" -eq 1 ] && \
		ls | while read f; do ln_relative "$EXTRACT" "$f" "$BG2GAMEDIR"; done
	ls| while read f; do mv "$f" $EXTRACT; done
	rm -rf $tmp
	return 0
}
# Mod installation ««1
# do_install: either call weinstall, or simulate««2
do_install() {
	if [ -n "$opt_simulate" ] ; then
		echo "weinstall $@"
	else
		if [ -n "$opt_pause" ] ; then
			echo "(paused) weinstall $@"
			echo -n "paused (q or x for hasty exit): "; read a < /dev/tty
			[ "$a" = "q" -o "$a" = "x" ] && exit
		fi
		weinstall $@
	fi
}
# mod_install (desired components in $@)««2
mod_install() {
	modidx=$(($modidx+1))
	logprefix="\e[1m($modidx/$nmods $moddir)\e[m "
	mod_tp2 || return $?
	[ $? -ne 0 ] && modlog "\e[33mcannot find tp2 file\e[m" && return
	modlang="--language "$(tp2_bestlang < "$tp2")
	[ $? -ne 0 ] && \
		modlog "\e[33mcannot find an appropriate language\e[m" && return
	tp2_status "$tp2" |sort > $TMP/weidu-old
	echo "$@"|tr ' ' '\n' |sort > $TMP/weidu-new
	to_install=$(comm -13 $TMP/weidu-old $TMP/weidu-new|sort -n|tr '\n' ' ')
	to_uninstall=$(comm -23 $TMP/weidu-old $TMP/weidu-new|sort -n|tr '\n' ' ')
	to_keep="$(comm -12 $TMP/weidu-old $TMP/weidu-new|sort -n|tr '\n' ' ')"
	[ -z "$to_install" -a -z "$to_uninstall" ] && \
		modlog "\e[36mnothing to do\e[m" && return
	modlog "\e[32m+$to_install\e[m \e[31m-$to_uninstall\e[m \e[38;5;8m=$to_keep\e[m"
	do_install "$moddir" --skip-at-view --noautoupdate --no-exit-pause \
		--force-install-list $to_install \
		--force-uninstall-list $to_uninstall
}
# Process all mods in one directory««2
# input piped as 'mod\tcomponents'
process_all_mods() {
	version="$1"
	sed -e "s/^$version;//;t;d" | while IFS=';' read moddir components; do
		mod_process "$moddir" $components
	done
}
# mod_readme_file: locate readme file for this mod««2
mod_readme_file() {
	mod_tp2
	langno=$(tp2_bestlang <"$modtp2")
	langdir=$(tp2_lang_dir "$langno" <"$modtp2")
	tp2_lang_readme "$langdir" <"$modtp2"
}
mod_show_readme() {
	readme=$(mod_readme_file "$moddir")
	echo "Showing readme file: '$readme'"
# 	case "$readme" in
# 		'');;
# 		*.htm*|*.HTM*) urxvt -e w3m "$readme" &;;
# 		*) urxvt -e view "$readme" &;;
# 	esac
}
# Mod components processing ««1
# moddb_components: same for a given mod name««2
moddb_components() {
	mod_tp2
	langno=$(tp2_bestlang <"$modtp2")
	tra=$(tp2_lang_tra "$langno" <"$modtp2")
	status=$(tp2_status "$modtp2")
	echo "langno=$langno tra=$tra status=$status" >&2
	# zenity format:
# 	tp2_components <"$tp2"|while IFS=';' read id name; do
# 		grep -q "$moddir=.*\\<$id\\>" $BWSUSER && echo TRUE || echo FALSE
# 		echo $id
# 		echo "$status"|grep -q "\\<$id\\>" && echo Yes || echo No
# 		(grep "^ *$name *= *" $tra | sed -e 's/.*=\s*//;
# 			/^~/{s/^~//;s/~$//}; /^"/{s/^"//; s/"$//}'; echo "?")|head -1
# 	done
	# whiptail format:
	tp2_components <"$modtp2"|while IFS=';' read id name; do
		echo "$status"|grep -q "\\<$id\\>" && echo -n '*'
		echo $id
		(grep "^ *$name *= *" $tra | sed -e 's/.*=\s*//;
			/^~/{s/^~//;s/~$//}; /^"/{s/^"//; s/"$//}'; echo "?")|head -1
		grep -q "$moddir=.*\\<$id\\>" $BWSUSER && echo 1 || echo 0
	done
}
mod_show_components() {
	moddb_components "$1" | while read id; do
		read name; read status
		printf "%4d %s\n" $id "$name"
	done
}

# open a component selection dialog ««2
mod_select_components() {
	select_file=$TMP/weidu-select
	moddb_components "$1" | xargs -d '\n' \
	whiptail --checklist 'Select components' 25 78 15 2>$select_file
	tr -d '"*' <$select_file
# 	zenity --title "$1" --width=1000 --height=800 --list --checklist \
# 	--text="Select components from mod <b>'$1'</b>" \
# 	--column="" --column="ID" --column="Installed" --column="Description" --separator=' '
}
mod_edit_components() {
	moddir="$1"
	comp=$(mod_select_components "$moddir")
	echo "$comp"
	if [ -n "$opt_simulate" ] ; then
		sed -e "/^$moddir=/s/=.*$/=$comp\r/" $CONFIG > $tmp/config.new
		diff $CONFIG $tmp/config.new
		return
	fi
	sed -i -e "/^$moddir=/s/=.*$/=$comp\r/" $CONFIG
}
# Main program ««1
# lsmods: list all mods given opt_bg1 and opt_bg2 ««
lsmods() {
	read_config|while IFS='=' read moddir comp; do
		mod_set_properties
		[ "$opt_bg1$modisbg1" = "11" -o "$opt_bg2$modisbg2" = "11" ] && \
			echo "$moddir"
	done
}
# main loop««2
main() {
	action='mod_install'; interactive=''
	while [ -n "$*" ] ; do
		arg="$1"
		case "$arg" in -*) shift;; *)break;; esac
		case "$arg" in
	#begin_usage
		# options (pass them first):
			-1)opt_bg1=1;;
			-2)opt_bg2=1;;
			-f|--force) opt_simulate='';;  # opposite of -n
			-n|--dry-run) opt_simulate=1;; # don't change anything
			-p|--pause) opt_pause=1;;      # pause before each install
			-q|--quiet) opt_quiet=1;;      # don't print log messages

		# per-mod actions:
			-d|--download) action=mod_download;;
			-x|--extract) action=mod_extract;;
			-i|--install) action=mod_install;;
			-r|--readme) action=mod_show_readme; interactive=1;;
			-s|--show-components) action=mod_show_components; interactive=1;;
			-e|--edit-components) action=mod_edit_components; interactive=1;;

		# global actions:
			-E|--edit-config) exec $EDITOR $CONFIG;;
			-G|--generate-moddb) generate_moddb; exit;;
			-I|--generate-config) import_user_config; exit;;
			-L|--lsmods) lsmods; exit;; # todo: show config instead
			-h|--help|*)
		# end_usage
			< "$0" sed -e '1,/begin_usage/d; s/\t/ /g;s/#.*$/[36m&[m/; /end_usage/Q'; exit;; # self-documenting :-)
		esac
	done
	[ -z "$opt_bg1$opt_bg2" ] && opt_bg1=1 && opt_bg2=1
	do_action "$action" "$@"
}

# scratch zone »»1
main "$@"
