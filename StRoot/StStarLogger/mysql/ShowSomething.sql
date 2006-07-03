#!/usr/local/bin/tcsh
# $Id: ShowSomething.sql,v 1.10 2006/07/03 04:13:38 fine Exp $
# Author: Valeri Fine (fine@bnl.gov) 26.01.2006
# Create the procedure to work with  logger Db
#mysql  -h heston.star.bnl.gov -u StarLogger -plogger <<MYSQLCODE
 use logger;

 SELECT COUNT(*)  FROM  logger.TaskDescription;

 SELECT COUNT(*)  FROM  logger.TaskDescription
    WHERE TaskDescription.TaskUser='fine';

 SELECT TaskUser,JobDescription,nProcesses, COUNT(*) FROM TaskDescription GROUP BY TaskUser,JobDescription,nProcesses;
 
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

SELECT  JobDescription.taskId, COUNT(*) AS completed_jobs
 FROM   JobDescription, JobTracking 
 WHERE   JobTracking.StepEventId = "Finish" AND JobTracking.jobId = JobDescription.jobId GROUP BY JobDescription.taskId;
 
# --- Print the number and time of the completed tasks

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
           AND JobTracking.jobId = JobDescription.jobId GROUP BY JobDescription.taskId) as tbl
  WHERE  completed_jobs = TaskDescription.nProcesses 
          AND TaskDescription.TaskUser='fine' 
          AND TaskDescription.taskId = tbl.taskId;
 
# --- Print the number of the uncompleted tasks

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
           AND JobTracking.jobId = JobDescription.jobId GROUP BY JobDescription.taskId) as tbl
  WHERE     completed_jobs <> TaskDescription.nProcesses 
         AND TaskDescription.TaskUser='fine' 
         AND TaskDescription.taskId = tbl.taskId ;  

#-  new tables         
 SELECT COUNT(*)  FROM  logger.TaskDescriptionN;

#MYSQLCODE
