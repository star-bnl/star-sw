# $Id: CreateJobTable.sql,v 1.7 2006/02/05 01:41:24 fine Exp $
# Author: Valeri Fine (fine@bnl.gov) 26.01.2006
# Create the job description table

use logger;
CREATE TABLE JobDescription (
                 dataId          INT         NOT NULL AUTO_INCREMENT  KEY  COMMENT 'SUMS $REQUESTID',
                 jobID_MD5       VARCHAR(32) NOT NULL                      COMMENT 'SUMS $REQUESTID',
                 processID       INT                                       COMMENT 'SUMS $PROCESSID',
                 time            TIMESTAMP                                 COMMENT 'Current time'   ,
                 node            VARCHAR(32)                               COMMENT 'Computer name'  ,  
                 JobUser         CHAR(20)                                  COMMENT 'User name $USER',
                 JobName         CHAR(20)                                  COMMENT 'Job name'       ,
                 JobDescription  CHAR(20)                                  COMMENT 'Job descriptor',
                 CONSTRAINT UNIQUE INDEX JobId (jobID_MD5, processID )
                 );
CREATE TABLE JobTracking (
                 id              INT NOT NULL AUTO_INCREMENT   KEY  COMMENT 'Unique record id',
                 dataId          INT                                COMMENT 'LAST_INSERT_ID() from the JobDescription table',
                 time            TIMESTAMP                          COMMENT 'Current time'   ,
                 Events          INT                                COMMENT 'Total # events processed' ,
                 Failed          INT                                COMMENT 'Total # events failed' ,
                 Cpu             FLOAT(10)                          COMMENT 'CPU time spent by the job' ,
                 RealTime        FLOAT(10)                          COMMENT 'Real time spent by the job' ,
                 StepName        CHAR(20)                           COMMENT 'Step name'      ,  #STAR maker name
                 StepEventId     ENUM('Start','Finish') NULL        COMMENT 'Event Id'       ,
                 StepEventValue  ENUM('Ok','Failed')    NULL        COMMENT 'Event outcome'  ,  #STAR Event return code
                 StepContext     CHAR(10)                           COMMENT 'Event context'  ,  #Field name 
                 MessageId       ENUM('=')              NULL        COMMENT 'Extra message flag',
                 ProgrammMessage VARCHAR(120)                       COMMENT 'Extra Message'  ,
                CONSTRAINT UNIQUE INDEX JobId (dataId)
                );
SHOW tables;
DESCRIBE  JobDescription;
DESCRIBE  JobTracking;
