#!/bin/bash
LOGDIR=/project/projectdirs/star/embedding/*/LOG
USERNAME=`echo $USER`
echo "Deleting files in $LOGDIR for user $USERNAME..."

find $LOGDIR -type f -user $USERNAME -atime +7 -exec rm {} \;

echo ""
echo "Files deleted."
exit 0
