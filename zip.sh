#!/usr/bin/env zsh

if [[ -z "$1" ]]; then
    echo "error; need mode argument (either rtw or m2tw)"
fi

if [[ $1 == "rtw" ]]; then
    mode=RTW
else
    mode=M2TW
fi

mod="quicksilver$mode"

# Compute signature and append it to all data; we go into $mod/$mod to avoid
# processing the patch files as part of the signature, because patch data also
# contain the timestamp of when the patch was generated (which is totally
# irrelevant as far as the mod is concerned).

game_files=($(find $mod/$mod -type f | sort))

mod_files=($(find $mod -type f -regex ".*$mod\.\(bat\|cfg\)$" | sort))
# :A gets the absolute path of this script
# :h gets the directory name only (acts like dirname(1)).
mod_files+=(${0:A:h}/$mod-README.org)

files=($game_files $mod_files)

echo $files | xargs sha1sum

sig=$(echo $files | xargs sha1sum | panxor --stdin-hex --min-length 40)

sig_git=$(git describe)

echo

echo $sig_git
echo $sig

# bundle up into zipfile
7za a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on \
    ${mod}-${sig_git}-${sig}.7z $files
