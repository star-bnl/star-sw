#!/bin/bash

INPUT=$1
if [ "$INPUT" == "" ]; then
	echo "Error, Usage: <INPUT FILE>"	
	exit -1
fi
SCRIPT=StRoot/macros/embedding/get_embedding_xml_rcf.pl
if [ ! -e $SCRIPT ]; then
	echo "Script $SCRIPT does not exist!"
	exit -1
fi
PARAMS=`cat $INPUT | tr "\n" " "`
perl $SCRIPT $PARAMS
exit 0

