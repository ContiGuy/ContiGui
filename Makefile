#
# makefile for conti-gui
#
# build the whole selfcontained binary:
#   - easiest is to use  `cb`  from  https://github.com/ContiGuy/conti-build
#   - alternatively it can also be built using elm, go, ego
#


#if which cb > /dev/null; then
#	CB=$(which cb)
#fi

all: conti-gui

conti-gui: wui
	${CB} go install -race

wui:
	cd wui && ${CB} go generate

run: conti-gui
	LOGXI=* conti-gui wui

upx: conti-gui
	${CB} upx -9 /go/bin/conti-gui

upx2: conti-gui
	${CB} upx --ultra-brute /go/bin/conti-gui

clean:
	${CB} go clean -i # -x  # -r   # -n
	rm -rf /go/pkg/*
#	ls -al /go/bin /go/pkg
