#!/bin/sh
#
# simple shell script to produce all (most current) 
# idl files from a database
#
#
#  2 arguments are the databasename and the servername
#
#######################################3


dbname=$1
server=$2
execdir=$STDB_SCRIPTS

if [ $execdir ]
then
 $execdir/dbMakeFiles.pl -d $dbname -s $server -o idl
else
 echo "makeIdlfiles:: ABORT:  perl script is not found"
fi


