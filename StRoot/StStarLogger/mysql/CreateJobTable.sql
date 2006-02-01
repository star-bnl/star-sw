# $Id: CreateJobTable.sql,v 1.4 2006/02/01 02:08:36 fine Exp $
# Author: Valeri Fine (fine@bnl.gov) 26.01.2006
# Create the job description table

use logger;
CREATE TABLE JobDescription (
                 id              INT NOT NULL AUTO_INCREMENT        KEY COMMENT 'Unique record id',
                 time            TIMESTAMP                          COMMENT 'Current time'   ,
                 hostid          CHAR(20)                           COMMENT 'Computer name'  ,  
                 JobUser         CHAR(20)                           COMMENT 'User name $USER',
                 SUMSJobId       CHAR(35)                           COMMENT 'SUMS $REQUESTID', 
                 SUMSProcessID   SMALLINT                           COMMENT 'SUMS $PROCESSID',
                 JobName         CHAR(20)                           COMMENT 'Job name'       ,
                 JobDescription  CHAR(20)                           COMMENT 'Job descriptor' ,
                 StepName        CHAR(20)                           COMMENT 'Step name'      ,  #STAR maker name
                 StepEventId     ENUM('Start','Finish') NULL        COMMENT 'Event Id'       ,
                 StepEventValue  ENUM('Ok','Failed')    NULL        COMMENT 'Event outcome'  ,  #STAR Event return code
                 StepContext     CHAR(10)                           COMMENT 'Event context'  ,  #Field name 
                 MessageId       ENUM('=')              NULL        COMMENT 'Extra message flag',
                 ProgrammMessage VARCHAR(120)                       COMMENT 'Extra Message'
              );
SHOW tables;
DESCRIBE  JobDescription;
