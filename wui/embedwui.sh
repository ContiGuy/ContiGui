#!/bin/bash
#
# generate wui.ego out of index.html
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

# set -x

#~ # TITLE="RSync - powered by (c) CoLiGUI"
#~ # TITLE="RSync - via CoLiGUI"
#~ TITLE="RSync"
#~
#~ elm package install --yes                                        || exit 10
#~ elm make --yes --warn "$@"                                       || exit 20
#~
#~ cat index.html | sed -e \
     #~ "s:<title>Main</title>:<title>$TITLE</title>:"  >   wui.ego || exit 30
#~
#~ echo                                                 >>  wui.ego || exit 40
#~ echo "<%! func WriteWuiHtml( w io.Writer ) error %>" >>  wui.ego || exit 50
#~ ego                                                              || exit 60


function genPage()
{
	PAGE="${1:-Main}"
	TITLE="${2:-RSync}"
	PAGE_NAME="${3:-$PAGE}"
	#~ TITLE="${2:-RSync}"
	#~ TITLE="${2:-RSync}"
	shift 2
	if [ $# -gt 0 ] ; then shift ; fi
	echo "$@"

	elm package install --yes                                                            || exit 10
	elm make --yes --warn --output "$PAGE.html"  "$@"  "$PAGE.elm"                       || exit 20

	cat "$PAGE.html" | sed -e \
		 "s:<title>Main</title>:<title>$TITLE</title>:"              >   "$PAGE.wui.ego" || exit 30

	echo                                                             >>  "$PAGE.wui.ego" || exit 40
	echo "<%! func Write${PAGE_NAME}WuiHtml( w io.Writer ) error %>" >>  "$PAGE.wui.ego" || exit 50
}

genPage Main        RSync                    "$@"
# genPage Test/System "System Test" SystemTest "$@"

ego                                                              || exit 60
