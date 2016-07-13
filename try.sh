#!/bin/bash
#
# try to compile and run the web ui only
#
#   - easiest is to use  `eg`  from  https://github.com/ContiGuy/conti-build
#   - alternatively it can also be built using elm, go, ego
#

# if which eg > /dev/null; then
# 	EG=$(which eg)
# fi

if echo $* | grep 'build' > /dev/null ; then
	BLD=./build.sh
else
	BLD="echo ok."
fi

if echo $* | grep run > /dev/null ; then
    RUN="coligui wui"
else
	RUN="echo ok."
fi

cd wui &&
	elm make --warn Main.elm &&
	LOGXI=* coligui wui

