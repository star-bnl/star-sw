#!/bin/csh

#
# test token. Works for both Solaris and Linux.
# Is silent if there is a token.
# Complains otherwise.
#
set TEST=`tokens | grep afs@rhic | sed "s/\[.*//"`
set USER=`id | sed "s/).*//" | sed "s/.*(//"`
set STATUS=0
if( "$TEST" == "") then
    set STATUS=1
    if ( -e $HOME/bin/token.csh) then
	#echo "$0 Found $HOME/bin/token.csh"
	$HOME/bin/token.csh
	set STATUS=$status
    endif
endif
 
if ($STATUS != 0) then
    echo "$0 There is no token for $USER on `hostname` at `date`" 
#else
#    echo "$0 there is a token for $USER on `hostname` at `date`" 
endif


