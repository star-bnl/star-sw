#!/bin/tcsh
setenv STAR_LEVEL $3
if ( -e $GROUP_DIR/group_env.csh ) then
        source $GROUP_DIR/group_env.csh
endif

root4star -l -b -q runmakeMuDstQA.C\(\"$1\",\"$2\"\)

