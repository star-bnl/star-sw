#! /bin/sh
# Author V.Fine <fine@bnl.gov> 08/30/2002
echo
echo Converts DOS file format into UNIX file format
echo Usage: $0 [list of files]
echo -----
echo 

for inFile in $*; do
if test  -r $inFile; then
 otFile=/tmp/`basename $inFile`.bak
 tr -d "\r"  <$inFile >$otFile
# tr -d [:"\r" "\033[0m"  "\033[31m" "\033[32m" "\033[33m" "\033[34m" "\033[35m"  <$inFile >$otFile
 mv $otFile $inFile
fi
done
