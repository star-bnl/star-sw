# $Id: CreateJobTable.sql,v 1.12 2006/05/12 18:48:49 fine Exp $
# Author: Valeri Fine (fine@bnl.gov) 26.01.2006
# Create the job description table

use logger;

CREATE TABLE TaskDescription (
                 taskId          INT         NOT NULL AUTO_INCREMENT  KEY  COMMENT 'Task #id',
                 jobID_MD5       VARCHAR(32) NOT NULL                      COMMENT 'SUMS $REQUESTID',
                 nProcesses      INT                                       COMMENT 'SUMS $nProcesses - the total number of the process for the task',
                 submissionTime  DATETIME                                  COMMENT 'SUMS time stampt - the time the task was created by the user with SUMS',
                 time            TIMESTAMP                                 COMMENT 'Submission time'   ,
                 submissionNode  VARCHAR(32)                               COMMENT 'The computer node the job was submitted from'  ,  
                 TaskUser        CHAR(20)                                  COMMENT 'User name from $SUMS_USER',
                 JobName         CHAR(20)                                  COMMENT 'Job name - $SUMS_name' ,
                 JobDescription  CHAR(20)                                  COMMENT 'Job descriptor',
                 TaskJobUser     CHAR(20)                                  COMMENT 'user name (from GRID certificate), from $SUMS_AUTHENTICATED_USER',
                 CONSTRAINT UNIQUE INDEX TaskId (jobID_MD5)
                 );
CREATE TABLE JobDescription (
                 jobId           INT         NOT NULL AUTO_INCREMENT  KEY  COMMENT 'Job #id',
                 taskId          INT                                       COMMENT 'Task #id from TaskDescription',
                 jobID_MD5       VARCHAR(32) NOT NULL                      COMMENT 'SUMS $REQUESTID',
                 processID       INT                                       COMMENT 'SUMS $PROCESSID',
                 time            TIMESTAMP                                 COMMENT 'Current time'   ,
                 node            VARCHAR(32)                               COMMENT 'Computer name'  ,  
                 JobUser         CHAR(20)                                  COMMENT 'User name $USER',
                 CONSTRAINT UNIQUE INDEX JobId (jobID_MD5, processID  )                             ,
                 CONSTRAINT UNIQUE INDEX TaskJobId (taskId, processID )
                 );
CREATE TABLE JobTracking (
                 stepId          INT NOT NULL AUTO_INCREMENT   KEY  COMMENT 'Record #id',
                 jobId           INT                                COMMENT 'LAST_INSERT_ID() from the JobDescription table',
                 time            TIMESTAMP                          COMMENT 'Current time'   ,
                 Events          INT                                COMMENT 'Total # events processed' ,
                 Failed          INT                                COMMENT 'Total # events failed' ,
                 StepName        CHAR(20)                           COMMENT 'Step name'      ,  #STAR maker name
                 StepEventId     ENUM('Start','Finish','EventFinish','Run') NULL  COMMENT 'Event Id'       ,
                 StepEventValue  ENUM('Ok','Failed')    NULL        COMMENT 'Event outcome'  ,  #STAR Event return code
                 StepContext     CHAR(10)                           COMMENT 'Event context'  ,  #Field name 
                 MessageId       ENUM('=')              NULL        COMMENT 'Extra message flag',
                 ProgrammMessage VARCHAR(120)                       COMMENT 'Extra Message' 
#                , CONSTRAINT UNIQUE INDEX JobId (jobId)
                );
SHOW tables;
DESCRIBE  JobDescription;
DESCRIBE  JobTracking;
