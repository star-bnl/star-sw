# $Id: ShowSomething.sql,v 1.2 2006/03/30 19:40:17 fine Exp $
# Author: Valeri Fine (fine@bnl.gov) 26.01.2006
# Create the procedure to work with  logger Db
 use logger;

 SELECT COUNT(*)  FROM  logger.TaskDescription
    WHERE TaskDescription.TaskUser='fine';

 SELECT TaskUser,JobDescription,nProcesses, COUNT(*) FROM TaskDescription GROUP BY TaskUser,JobDescription,nProcesses;
 
# -- Show my tasks
 SELECT  TaskUser,jobID_MD5, JobDescription FROM TaskDescription WHERE TaskUser='fine';

 -- Show my completed tasks
 
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
                   
  SELECT  COUNT(*)
    FROM  TaskDescription, JobDescription, JobTracking
      WHERE
            SELECT (*) WHERE TaskDescription.taskId = JobDescription.taskId 
                  AND      JobTracking.jobId = JobDescription.jobId 
                  AND            StepEventId = 'Finish' 
      GROUP BY JobDescription.taskId,JobTracking.jobId ;

  #      AND  (SELECT COUNT(*) FROM  TaskDescription, JobDescription, JobTracking 
#                   )= TaskDescription.nProcesses-1 GROUP BY TaskUser,TaskDescription.jobID_MD5;   

#  -- print the number of the comlleted jobs

SELECT  JobDescription.taskId, COUNT(*) AS completed_jobs
 FROM   JobDescription, JobTracking 
 WHERE  JobTracking.jobId = JobDescription.jobId GROUP BY JobDescription.taskId;
 
 # --- Print the number of the completed tasks

SELECT TaskDescription.TaskUser, TaskDescription.jobID_MD5,TaskDescription.nProcesses,tbl.completed_jobs
FROM  TaskDescription, 
   ( SELECT  JobDescription.taskId, COUNT(*) AS completed_jobs
     FROM   JobDescription, JobTracking 
     WHERE  JobTracking.jobId = JobDescription.jobId GROUP BY JobDescription.taskId) as tbl
  WHERE  completed_jobs =  TaskDescription.nProcesses AND TaskDescription.TaskUser='fine';  
 
 # --- Print the number of the uncompleted tasks

SELECT TaskDescription.TaskUser
     , TaskDescription.jobID_MD5
     , TaskDescription.nProcesses
     , TaskDescription.submissionTime As Submitted
     , tbl.completed_jobs
FROM  TaskDescription, 
   ( SELECT  JobDescription.taskId, JobTracking.time as FinishedTime COUNT(*) AS completed_jobs
     FROM   JobDescription, JobTracking 
     WHERE  JobTracking.jobId = JobDescription.jobId GROUP BY JobDescription.taskId) as tbl
  WHERE  completed_jobs <> TaskDescription.nProcesses AND TaskDescription.TaskUser='fine';  
