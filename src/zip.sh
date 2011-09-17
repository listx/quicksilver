#!/bin/zsh

7za a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on \
    quicksilverM2TW.7z quicksilverM2TW/quicksilverM2TW*
7za a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on \
    quicksilverRTW.7z quicksilverRTW/quicksilverRTW*

# vim: syntax=zsh
