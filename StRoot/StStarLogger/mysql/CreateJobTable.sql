# $Id: CreateJobTable.sql,v 1.2 2006/01/27 00:55:35 fine Exp $
# Author: Valeri Fine (fine@bnl.gov) 26.01.2006
# Create the job description table

use logger;
CREATE TABLE JobDescription (
                 id              MEDIUMINT NOT NULL AUTO_INCREMENT  KEY COMMENT 'Unique record id',
                 time            TIMESTAMP                          COMMENT 'current time'   ,
                 hostid          CHAR(20)                           COMMENT 'computer name'  ,  
                 JobUser         CHAR(20)                           COMMENT 'user name'      ,
                 SUMSJobId       BIGINT                             COMMENT 'SUMS Job id'    , 
                 SUMSProcessID   SMALLINT                           COMMENT 'SUMS process ID',
                 JobName         CHAR(20)                           COMMENT 'Job name'       ,
                 JobDescription  CHAR(20)                           COMMENT 'Job descriptor' ,
                 StepName        CHAR(20)                           COMMENT 'Step name'      ,  
                 StepEventId     ENUM('Start','Finish') NULL        COMMENT 'Event Id'       ,
                 StepEventValue  ENUM('Ok','Failed')    NULL        COMMENT 'Event outcome'  ,
                 StepConetxt     CHAR(10)                           COMMENT 'Event context'  ,
                 MessageId       ENUM('=')              NULL        COMMENT 'Extra message flag',
                 ProgrammMessage VARCHAR(120)                       COMMENT 'Extra Message'
              );
SHOW tables;
DESCRIBE  JobDescription;
