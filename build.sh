#!/bin/zsh
# Create mod folder using vanilla M2TW 1.3 files

mkdir -vp gen/quicksilver/data
cd gen
mv -vf *.txt world sounds quicksilver/data

# vim: syntax=zsh
