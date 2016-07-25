#!/bin/tcsh -f 
rm sche* *.txt paw.metafile *.dst.root *.hist.root *runco.root *tags.root *session.xml
root.exe -q -b *MuDst.root > & status.log
grep ' probably not closed' status.log | sort -u | awk '{print $5}' | awk -F. '{print "rm "$1"*"}'
