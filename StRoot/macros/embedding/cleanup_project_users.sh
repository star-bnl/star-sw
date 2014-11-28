#!/bin/bash
LOGDIR=/project/projectdirs/star/embedding/*/LOG
echo "Looking for files in $LOGDIR..."

USERLIST_FULL=cleanup_users_full.txt
[ ! -e $USERLIST_FULL ] || rm $USERLIST_FULL
USERLIST_TEMP=cleanup_users.txt.tmp
[ ! -e $USERLIST_TEMP ] || rm $USERLIST_TEMP
USERLIST=cleanup_users.txt
[ ! -e $USERLIST ] || rm $USERLIST

find $LOGDIR -type f -atime +7 -printf "%u\n" > $USERLIST_FULL
sort $USERLIST_FULL | uniq > $USERLIST_TEMP
for LINE in `cat $USERLIST_TEMP`; do
	NUM=`grep $LINE $USERLIST_FULL | wc -l`
	echo -e "$NUM \t $LINE" >> $USERLIST
done
rm $USERLIST_FULL $USERLIST_TEMP 
echo ""
echo "File number and users are:"
cat $USERLIST
echo ""
exit 0
