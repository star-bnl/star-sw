#!/bin/bash 
set -u ;  # exit  if you try to use an uninitialized variable

NN=`hostname -f`
line=`/sbin/ifconfig | grep 'inet 10'`=
myIP=`echo $line | cut -d ' ' -f2`
echo D inShifter:`env|grep  SHIFTER_RUNTIME`

echo  D-$NN myIP=$myIP   

#if [ ! \( -e "${DBCONF_PATH}" \) ] ; then
#     echo "ERROR user cnf file ${DBCONF_PATH} does not exist!" >&2
#     exit 1
#fi

# keep DB deamon alive until SLURM job dies
du -hs  /mysqlVault 


echo 'D-$NN  starting mysqld_safe ...'
export LD_LIBRARY_PATH=/opt/rh/rh-mysql57/root/usr/lib64
/opt/rh/rh-mysql57/root/usr/libexec/mysqld --defaults-file=/mysqlVault/my.cnf --secure-file-priv=$SCRATCH --tmpdir=/mysqlVault/tmp &


echo "DB-$NN started mysqld_safe  "`date` 
ls -l /mysqlVault 

sleep 10
ps

echo D-$NN modify  ${DBCONF_PATH}
#cat  ${DBCONF_PATH}
sed  -i-e "s/<db_server_ip>/$myIP/g"  ${DBCONF_PATH}
echo D-$NN after IP was added
cat  ${DBCONF_PATH}

echo  D-$NN   list_mysql users
mysql --socket=/mysqlVault/mysql.sock -e 'SELECT user, host FROM mysql.user;' 

echo  D-$NN   list_mysql tables
mysql --socket=/mysqlVault/mysql.sock -e 'show databases'
 
echo  D-$NN   starDB stay alive forever or main program completes
nSleep=30
totSec=0
while true ; do    
    #ps -ef |grep mysqld
    sleep $nSleep

    NL=`  mysql --socket=/mysqlVault/mysql.sock -e ' show processlist;' | wc -l`
    NC=$[ $NL - 2 ]
    echo  D-$NN   $totSec sec, nDbProc:   $NC  " "`date`
    totSec=$[ $totSec + $nSleep ]

    if [ ! \(-e "$DBCONF_PATH" \)  ] ; then
	echo " db-info $DBCONF_PATH  lost, quit DB " >&2
	exit 1
    fi
done
