#!/bin/zsh

rm -f quicksilverM2TW.7z
rm -f quicksilverRTW.7z
7za a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on \
    quicksilverM2TW.7z ${1}/quicksilverM2TW/quicksilverM2TW*
7za a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on \
    quicksilverRTW.7z ${1}/quicksilverRTW/quicksilverRTW*

# vim: syntax=zsh
