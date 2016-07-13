#!/bin/bash
#
# try to compile and run the web ui only
#
#   - easiest is to use  `cb`  from  https://github.com/ContiGuy/conti-build
#   - alternatively it can also be built using elm, go, ego
#

if which cb > /dev/null; then
	CB=$(which cb)
fi

if echo $* | grep 'build' > /dev/null ; then
	BLD=./build.sh
else
	BLD="echo -n ."
fi

if echo $* | grep run > /dev/null ; then
    RUN="conti-gui wui"
else
	RUN="echo ."
fi

cd wui &&
	$CB elm make --warn Main.elm &&
	LOGXI=* $RUN

