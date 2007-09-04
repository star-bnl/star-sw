#!/usr/local/bin/tcsh
#  $1 - $REQUESTID
#  $2 - $$PROCESSID
#  $3 - exit code
#  $4 - Start/Finish
# mysql -h heston.star.bnl.gov -u StarLogger -plogger --exec="\. $1"
mysql -B -h heston.star.bnl.gov -u StarLogger -plogger <<MYSQLCODE 
  use logger;
#--  INSERT DELAYED INTO JobDescriptionFinish  SET SequenceValue="$3", JobDescriptionID = (SELECT JobDescriptionID  FROM JobDescription WHERE  TaskRequestID_MD5="$1" AND BrokerProcessID="$2");
#--  UPDATE TaskDescription  SET TaskRemainSize=TaskRemainSize-1 WHERE TaskRequestID_MD5="$1";
  INSERT DELAYED INTO JobDescriptionFinish  SET SequenceValue="$3", JobDescriptionID = (SELECT JobDescriptionID  FROM JobDescription WHERE  TaskRequestID_MD5="$1" AND BrokerProcessID="$2");
  UPDATE Tasks  SET taskRemainSize=taskRemainSize-1 WHERE brokerTaskID="$1";
MYSQLCODE
