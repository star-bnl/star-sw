#!/usr/local/bin/tcsh
#  $1 - $REQUESTID
#  $2 - $JOBINDEX
#  $3 - exit code
#  $4 - Start/Finish
# mysql -h heston.star.bnl.gov -u StarLogger -plogger --exec="\. $1"
setenv r_id ${LOGNAME}_${REQUESTID}
if ($? ==  0) then
mysql -B -h heston.star.bnl.gov -u StarLogger -plogger <<MYSQLCODE 
  use logger;
#--  INSERT DELAYED INTO JobDescriptionFinish  SET SequenceValue="$3", JobDescriptionID = (SELECT JobDescriptionID  FROM JobDescription WHERE  TaskRequestID_MD5="$1" AND BrokerProcessID="$2");
#--  UPDATE TaskDescription  SET TaskRemainSize=TaskRemainSize-1 WHERE TaskRequestID_MD5="$1";
# -- 19/09/2007 temp remove  INSERT DELAYED INTO JobDescriptionFinish  SET SequenceValue="$3", JobDescriptionID = (SELECT JobDescriptionID  FROM JobDescription WHERE  TaskRequestID_MD5="$1" AND BrokerProcessID="$2");
  UPDATE LOW_PRIORITY Jobs   SET stateID='8' WHERE brokerJobID="$2" AND taskID=(SELECT taskID FROM Tasks WHERE brokerTaskID="$1");
  UPDATE LOW_PRIORITY Jobs_${r_id}  SET stateID='8' WHERE brokerJobID="$2" AND taskID=(SELECT taskID FROM Tasks WHERE brokerTaskID="$1");
  UPDATE LOW_PRIORITY Tasks  SET taskRemainSize=taskRemainSize-1 WHERE brokerTaskID="$1";
MYSQLCODE
else
mysql -B -h heston.star.bnl.gov -u StarLogger -plogger <<MYSQLCODE 
  use logger;
#--  INSERT DELAYED INTO JobDescriptionFinish  SET SequenceValue="$3", JobDescriptionID = (SELECT JobDescriptionID  FROM JobDescription WHERE  TaskRequestID_MD5="$1" AND BrokerProcessID="$2");
#--  UPDATE TaskDescription  SET TaskRemainSize=TaskRemainSize-1 WHERE TaskRequestID_MD5="$1";
# -- 19/09/2007 temp remove  INSERT DELAYED INTO JobDescriptionFinish  SET SequenceValue="$3", JobDescriptionID = (SELECT JobDescriptionID  FROM JobDescription WHERE  TaskRequestID_MD5="$1" AND BrokerProcessID="$2");
  UPDATE  LOW_PRIORITY Jobs   SET stateID='9' WHERE brokerJobID="$2" AND taskID=(SELECT taskID FROM Tasks WHERE brokerTaskID="$1");
  UPDATE  LOW_PRIORITY Jobs_${r_id}   SET stateID='9' WHERE brokerJobID="$2" AND taskID=(SELECT taskID FROM Tasks WHERE brokerTaskID="$1");
  UPDATE  LOW_PRIORITY Tasks  SET taskRemainSize=taskRemainSize-1 WHERE brokerTaskID="$1";
MYSQLCODE
endif  

