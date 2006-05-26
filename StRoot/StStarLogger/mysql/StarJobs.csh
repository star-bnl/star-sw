#!/usr/local/bin/tcsh
#  $1 - $REQUESTID
#  $2 - $$PROCESSID
#  $3 - exit code
#  $4 - Start/Finish
# mysql -h heston.star.bnl.gov -u StarLogger -plogger --exec="\. $1"
mysql -B -h heston.star.bnl.gov -u StarLogger -plogger <<MYSQLCODE 
use logger;
INSERT DELAYED INTO JobTracking SET stepId=NULL, StepName ="shell", jobId = (SELECT jobId FROM JobDescription WHERE  jobID_MD5="$1" AND processID="$2"), StepEventId="Job$3", StepEventValue="$4", StepContext="exit";
MYSQLCODE
