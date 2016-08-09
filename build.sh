#!/bin/bash
#
# build the whole selfcontained binary:
#   - easiest is to use  `cb`  from  https://github.com/ContiGuy/conti-build
#   - alternatively it can also be built using elm, go, ego
#

# Copyright © 2016 ContiGuy mrcs.contiguy@mailnull.com
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


# make clean all && LOGXI=* conti-gui wui



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

## (cd wui && $CB go generate) & &
(cd wui && bash -x ./embedwui.sh) &&
	$CB go install -race &&
	$UPX &&
	LOGXI=* $RUN

