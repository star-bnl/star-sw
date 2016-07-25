#! /usr/local/bin/tcsh -f
ssh $1.rcf.bnl.gov <<EOF
yes
EOF
ssh rcf.rhic.bnl.gov <<EOF
yuri
EOF
exit
exit
#end
