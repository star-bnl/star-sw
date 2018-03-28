#!/bin/bash 
set -u ;  # exit  if you try to use an uninitialized variable

NN=`hostname -f`
line=`ifconfig | grep 'inet addr:10'`=
myIP=`echo $line | cut -f2 -d: | cut -f1 -d\ `
echo D inShifter:`env|grep  SHIFTER_RUNTIME`

echo  D-$NN myIP=$myIP   

#if [ ! \( -e "${DBCONF_PATH}" \) ] ; then
#     echo "ERROR user cnf file ${DBCONF_PATH} does not exist!" >&2
#     exit 1
#fi

# keep DB deamon alive until SLURM job dies
du -hs  /mysqlVault 

echo 'D-$NN  starting mysqld_safe ...'
/usr/bin/mysqld_safe  --defaults-file=/mysqlVault/my.cnf &

echo "DB-$NN started mysqld_safe  "`date` 
#ls -l /mysqlVault 

sleep 10
ps

echo D-$NN modify  ${DBCONF_PATH}
#cat  ${DBCONF_PATH}
sed  -i-e "s/<db_server_ip>/$myIP/g"  ${DBCONF_PATH}
echo D-$NN after IP was added
cat  ${DBCONF_PATH}

echo  D-$NN   list_mysql users
mysql -u balewski --socket=/mysqlVault/mysql.sock -pjan -e 'SELECT user, host FROM mysql.user;' 

echo  D-$NN   list_mysql tables
mysql -u balewski --socket=/mysqlVault/mysql.sock -pjan -e 'show databases'

echo  D-$NN   mysql optimizing tables
mysql -u balewski --socket=/mysqlVault/mysql.sock -pjan -e 'OPTIMIZE TABLE `Calibrations_tpc`.`tpcAnodeHVavg`'
mysql -u balewski --socket=/mysqlVault/mysql.sock -pjan -e 'OPTIMIZE TABLE `Calibrations_tof`.`tofINLSCorr`'
mysql -u balewski --socket=/mysqlVault/mysql.sock -pjan -e 'OPTIMIZE TABLE `Calibrations_tof`.`tofTDIGOnTray`'
mysql -u balewski --socket=/mysqlVault/mysql.sock -pjan -e 'OPTIMIZE TABLE `Calibrations_tof`.`tofTotbCorr`'
mysql -u balewski --socket=/mysqlVault/mysql.sock -pjan -e 'OPTIMIZE TABLE `Calibrations_tof`.`tofZbCorr`'
mysql -u balewski --socket=/mysqlVault/mysql.sock -pjan -e 'OPTIMIZE TABLE `Calibrations_tof`.`tofTOffset`'

echo  D-$NN check permission for the load balancing process
mysql -u balewski --socket=/mysqlVault/mysql.sock -pjan   -e 'show grants for "loadbalancer"@"%";'
 
echo  D-$NN   starDB stay alive forever or main program completes
nSleep=30
totSec=0
while true ; do    
    #ps -ef |grep mysqld
    sleep $nSleep

    NL=`  mysql -u balewski --socket=/mysqlVault/mysql.sock -pjan -e ' show processlist;' | wc -l`
    NC=$[ $NL - 2 ]
    echo  D-$NN   $totSec sec, nDbProc:   $NC  " "`date`
    totSec=$[ $totSec + $nSleep ]

    if [ ! \(-e "$DBCONF_PATH" \)  ] ; then
	echo " db-info $DBCONF_PATH  lost, quit DB " >&2
	exit 1
    fi
done
