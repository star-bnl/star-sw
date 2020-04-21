#!/bin/bash 
# Note - this script is NOT running in shifter

taskList=${1-badList}
logF=${2-qaBad1.log}
SEC3=`date +%s`

NN=`hostname -f`
echo Q-$NN start log-to $logF
echo  Q-$NN pwd `pwd`
date
whoami

# count avaliable hyper-cores
nCpu=`cat /proc/cpuinfo  |grep processor| wc -l`
echo Q-$NN  nHypCpu=$nCpu  and top
free -g
top ibn1

# --- function rewritten for localDb model

function queryDB {

    targetDb=`grep "port=" dbLoadBalLocConf.xml | sed 's/\"/ /g' | awk '{print "-h "$3" -P"$5}'`

    if [[ $targetDb =~ "db_server_ip" ]]
    then
        echo "db server not ready"
    else
        line=`mysql $targetDb  -e '  show processlist; '| wc -l`
        nCon=$[ $line - 2 ]
        echo "N connections = $nCon"
        totDbCon=$[ $totDbCon + $nCon ]
    fi
}


# --- obsolete in localDB model ----
#
# get credentials to STAR
#source ~/.ssh/mstardb-root.pass
#
#echo Q-$NN test connection to DB-server 
#
#queryDB mstardb02
#
#-------- end obsolete ------


nSleep=5
totSec=0
nInpTask=`nl ${taskList} |wc -l`

line=`free -g |grep "\-/+"`
echo RAM line=$line=
ramBCF_Gb=`echo $line | cut -f4 -d\ `

echo "#QAutil_1 taskList:$taskList len:$nInpTask N:${SLURM_NNODES}  node0:$NN ramBCF_Gb:$ramBCF_Gb  partition:$SLURM_JOB_PARTITION" >$logF 
echo "#QAutil_2 farmer SKEW:$SKEW  THREADS:$THREADS  NUM_EVE:$NUM_EVE" >>$logF 
echo "#" >>$logF 
echo "#format: date , totSec,  totDbCon, nDoneTask, nExeTask " >>$logF 

WNODES=$(($SLURM_JOB_NUM_NODES - 1))
SKEWTASK=$(($THREADS*$WNODES))

while true ; do    
    echo  Q-$NN   $totSec sec,  sleep  $nSleep " "`date`
    echo  Q-$NN pwd `pwd`
    sleep $nSleep

    totDbCon=0

    queryDB
    echo  Q-$NN end of query over Dbs totDbCon=$totDbCon
    pwd
    ls -l
    # farmer progress

    nExeTask=`cat fastrecovery.${taskList}.tfin |read_recovery ${taskList}.tfin |nl | wc -l`
    nDoneTask=`cat progress.${taskList}.tfin | wc -l`
    echo  Q-$NN myFarmer_update JID=$SLURM_JOBID  task count: inp=$nInpTask done=$nDoneTask exe=$nExeTask "  "`date`

    SEC4=`date +%s`
    totSec=$[ $SEC4 - $SEC3 ]
    echo `date` ",  $totSec,  $totDbCon,  $nDoneTask, $nExeTask " >>$logF     

    if [ $nExeTask -ge $SKEWTASK ] ; then
	 if [ ! -f ${WRK_DIR}/skewdone ] ; then
	    touch ${WRK_DIR}/skewdone
	 fi
    fi

    if [  $totSec -gt 2000 ] ; then 
	nSleep=30
    fi
    if [  $totSec -gt 4000 ] ; then 
	nSleep=60
    fi
    if [  $totSec -gt 8000 ] ; then 
	nSleep=120
    fi
done


echo -n Q-$NN job finished

