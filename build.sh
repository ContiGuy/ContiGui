#!/bin/bash
#
# build the whole selfcontained binary:
#   - easiest is to use  `cb`  from  https://github.com/ContiGuy/conti-build
#   - alternatively it can also be built using elm, go, ego
#

if which cb > /dev/null; then
	CB=$(which cb)
fi

if echo $* | grep upx > /dev/null ; then
	UPX="$CB upx -9 /go/bin/conti-gui"
	if echo $* | grep -e brute -e ultra > /dev/null ; then
		UPX="$CB upx --ultra-brute /go/bin/conti-gui"
	fi
else
	UPX="echo running conti-gui wui ..."
fi

if echo $* | grep run > /dev/null ; then
    RUN="conti-gui wui"
else
	RUN="echo ok."
fi

(cd wui && $CB go generate) &&
	$CB go install -race &&
	$UPX &&
	LOGXI=* $RUN

