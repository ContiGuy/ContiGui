#!/bin/bash
#
# try to compile and run the web ui only
#
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


#~ if which cb > /dev/null; then
	#~ CB=$(which cb)
if CB=$(which cb) ; then
	echo using cb
fi

if echo $* | grep test > /dev/null ; then
    MAIN="Test/System"
    SUBCMD=test
else
    MAIN="Main"
    SUBCMD="wui --base-folder=$HOME/tmp"
fi

if echo $* | grep 'build' > /dev/null ; then
	BLD=./build.sh
else
	BLD="echo -n ."
fi

if echo $* | grep run > /dev/null ; then
    # RUN="conti-gui $SUBCMD --base-folder=$HOME/tmp"
    RUN="conti-gui $SUBCMD"
else
	RUN="echo ."
fi

set -x
cd wui &&
	$CB elm make --warn "$MAIN.elm" &&
	LOGXI=* $RUN

