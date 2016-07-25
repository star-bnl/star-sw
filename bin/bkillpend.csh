#! /usr/local/bin/tcsh -f
bkill `bjobs | grep PEND | awk '{printf("%s ",$1)}'`
