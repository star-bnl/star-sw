#! /usr/local/bin/tcsh -f
set list = `ls /afs/rhic/.asis/packages/ignore_*`
foreach ignore_pkg ( $list )
    set f_pkg = `basename $ignore_pkg`
    set pkg = `echo $f_pkg | sed -e 's/ignore_//g'`
#    echo "set: $ignore_pkg $f_pkg $pkg"
    set log=$pkg.log
    if (! -r $log ) then
    echo \
    "mirdir -o -e $ignore_pkg /afs/cern.ch/asis/packages/$pkg /afs/rhic/.asis/packages/$pkg >& $log &"
     mirdir -o -e $ignore_pkg /afs/cern.ch/asis/packages/$pkg /afs/rhic/.asis/packages/$pkg >& $log &
    endif
end
# ___ EOD ____
