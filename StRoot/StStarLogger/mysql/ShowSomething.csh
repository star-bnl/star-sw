#!/usr/local/bin/tcsh
# $Id: ShowSomething.csh,v 1.2 2006/06/05 18:52:04 fine Exp $
# Author: Valeri Fine (fine@bnl.gov) 26.01.2006
# Create the procedure to work with  logger Db
echo Total number of the tasks:
echo --------------------------
mysql  -h heston.star.bnl.gov -u StarLogger -plogger <<MYSQLCODE
 use logger;
 SELECT COUNT(*)  FROM  logger.TaskDescription;
MYSQLCODE
echo ""
echo Total number of the tasks that belongs to the user "fine":
echo -----------------------------------------------------
mysql  -h heston.star.bnl.gov -u StarLogger -plogger <<MYSQLCODE
 use logger;
 SELECT COUNT(*)  FROM  logger.TaskDescription
    WHERE TaskDescription.TaskUser='fine';
MYSQLCODE
echo ""
echo The sorted List of the tasks per user
echo -------------------------------------
mysql  -h heston.star.bnl.gov -u StarLogger -plogger <<MYSQLCODE
 use logger;
 SELECT TaskUser,JobDescription,nProcesses, COUNT(*) FROM TaskDescription GROUP BY TaskUser,JobDescription,nProcesses;
 
MYSQLCODE
echo "---"
#echo The sorted List of the tasks
#echo ----------------------------
mysql  -h heston.star.bnl.gov -u StarLogger -plogger <<MYSQLCODE
 use logger;
# -- Show my tasks
 SELECT  TaskUser,jobID_MD5, JobDescription FROM TaskDescription WHERE TaskUser='fine';

# -- Show my completed tasks
 
  SELECT  TaskUser,TaskDescription.jobID_MD5 FROM TaskDescription, JobDescription, JobTracking
      WHERE  TaskUser = 'fine'
        AND  TaskDescription.taskId = JobDescription.taskId 
        AND  JobTracking.jobId = JobDescription.jobId 
        AND  StepEventId = 'Finish';
        
  SELECT  TaskUser,TaskDescription.jobID_MD5
         ,TaskDescription.taskId,JobDescription.taskId
         ,JobDescription.jobId, JobTracking.jobId, 
          COUNT(*)
    FROM  TaskDescription, JobDescription, JobTracking
      WHERE  TaskUser = 'fine'
                   AND TaskDescription.taskId = JobDescription.taskId 
                   AND      JobTracking.jobId = JobDescription.jobId 
                   AND            StepEventId = 'Finish' 
      GROUP BY JobDescription.taskId,JobTracking.jobId ;
                   
#  -- print the number of the completed jobs

MYSQLCODE
echo ""
echo The list of the completed tasks
echo -------------------------------
mysql  -h heston.star.bnl.gov -u StarLogger -plogger <<MYSQLCODE
 use logger;
SELECT JobDescription.JobUser, JobTracking.time, JobDescription.taskId,  JobTracking.jobId, COUNT(*) AS completed_jobs
 FROM   JobDescription, JobTracking 
 WHERE     JobTracking.StepEventId = "Finish" 
         AND JobTracking.StepContext = "ProgSize"
         AND JobTracking.jobId = JobDescription.jobId GROUP BY JobDescription.taskId;
 
# --- Print the number and time of the completed tasks

MYSQLCODE
echo ""
echo The list of the uncompleted tasks
echo ---------------------------------
mysql  -h heston.star.bnl.gov -u StarLogger -plogger <<MYSQLCODE
 use logger;
SELECT JobDescription.JobUser, current_jobs.time, JobDescription.taskId,  current_jobs.jobId, COUNT(*) AS uncompleted_jobs
  FROM   JobDescription, 
         (SELECT  JobTracking.time as time, JobTracking.jobId as jobId     
            FROM JobDescription, JobTracking 
            WHERE JobTracking.jobId = JobDescription.jobId 
         ) as current_jobs
  WHERE  "Finish" NOT IN ( SELECT current_jobs.StepEventId FROM current_jobs )
         GROUP BY JobDescription.taskId
MYSQLCODE
echo ""
echo The list of the started jobs
echo -------------------------------
mysql  -h heston.star.bnl.gov -u StarLogger -plogger <<MYSQLCODE
 use logger;
SELECT JobDescription.JobUser, JobTracking.time, JobDescription.taskId,  COUNT(*) AS completed_jobs
 FROM   JobDescription, JobTracking 
 WHERE     JobTracking.StepEventId = "Start" 
         AND JobTracking.StepContext = "ProgSize"
         AND JobTracking.jobId = JobDescription.jobId GROUP BY JobDescription.taskId;
 
# --- Print the number and time of the completed tasks

MYSQLCODE
echo "---"
echo The  List of the completed tasks
echo ----------------------------
mysql  -h heston.star.bnl.gov -u StarLogger -plogger <<MYSQLCODE
 use logger;
SELECT TaskDescription.TaskUser       AS 'Task Owner'
     , TaskDescription.JobName        AS 'Defined by SUMS' 
     , TaskDescription.jobID_MD5      AS 'Task ID'
     , TaskDescription.nProcesses     AS 'Ordered jobs'
     , TaskDescription.submissionTime AS 'Submitted'
     , tbl.FinishedTime               AS 'Completed by'
     , TIMEDIFF(tbl.FinishedTime, TaskDescription.submissionTime) AS 'Process Time'
     , TIME_TO_SEC(TIMEDIFF(tbl.FinishedTime, TaskDescription.submissionTime))/(60*tbl.completed_jobs) AS 'Avr min/job'
     , tbl.completed_jobs             AS 'Total Jobs'
FROM  TaskDescription, 
   ( SELECT  JobDescription.taskId as taskId, JobTracking.time AS FinishedTime, COUNT(*) AS completed_jobs
     FROM   JobDescription, JobTracking 
     WHERE     JobTracking.StepEventId = "Finish" 
           AND JobTracking.StepContext = "ProgSize"
           AND JobTracking.jobId = JobDescription.jobId GROUP BY JobDescription.taskId) as tbl
  WHERE  completed_jobs = TaskDescription.nProcesses 
#          AND TaskDescription.TaskUser='fine' 
          AND TaskDescription.taskId = tbl.taskId;
 
# --- Print the number of the uncompleted tasks

MYSQLCODE
echo "---"
echo The  List of the uncompleted tasks
echo ------------------------------------
mysql  -h heston.star.bnl.gov -u StarLogger -plogger <<MYSQLCODE
 use logger;
SELECT TaskDescription.TaskUser       AS 'Task Owner'
     , TaskDescription.JobName        AS 'Defined by SUMS'
     , TaskDescription.jobID_MD5
     , TaskDescription.nProcesses     AS 'Ordered jobs'
     , TaskDescription.submissionTime AS 'Submitted'
     , tbl.completed_jobs             AS 'Done'
     , TaskDescription.nProcesses-tbl.completed_jobs AS 'To be done yet'
FROM  TaskDescription, 
   ( SELECT  JobDescription.taskId as taskId, JobTracking.time AS FinishedTime, COUNT(*) AS completed_jobs
     FROM   JobDescription, JobTracking 
     WHERE     JobTracking.StepEventId = "Finish" 
          AND JobTracking.StepContext = "ProgSize"
          AND JobTracking.jobId = JobDescription.jobId GROUP BY JobDescription.taskId) as tbl
  WHERE     completed_jobs <> TaskDescription.nProcesses 
#         AND TaskDescription.TaskUser='fine' 
         AND TaskDescription.taskId = tbl.taskId ;  
MYSQLCODE
