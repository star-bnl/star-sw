#! /usr/local/bin/tcsh -f

echo root4star -b -q lsFile.C\(\"$1\",\"$2\"\)
root4star -b -q $STAR/StRoot/macros/lsFile.C\(\"$1\",\"$2\"\)
#
