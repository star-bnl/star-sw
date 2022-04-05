# $Id: CreateJobTable.sql,v 1.16 2007/08/28 16:59:15 fine Exp $
# Author: Valeri Fine (fine@bnl.gov) 26.01.2006
# Create the job description table

use logger;

#--
#-- TABLE: BrokerDescriptionDictionary from ucm
#--  

CREATE TABLE BrokerDescriptionDictionary IF NOT EXISTS BrokerDescriptionDictionary  ( 

  BrokerID BrokerEnum ,
  BrokerName VARCHAR(128) ,
  BrokerAlias VARCHAR(32) ,
  BrokerLocation VARCHAR(128) 

);

#--
#-- TABLE: BrokerEnum
#--  

CREATE TABLE BrokerEnum ( 

);

#--
#-- TABLE: ClassEnum
#--  

CREATE TABLE ClassEnum ( 

);

#--
#-- TABLE: int
#--  

CREATE TABLE int ( 

);

#CREATE TABLE  IF NOT EXISTS BrokerDescriptionDictionary   (
#                 BrokerID        ENUM('SUMS')    KEY                    COMMENT 'The ID of the broker, in our case it is SUMS',
#                 BrokerName      VARCHAR(120)                           COMMENT 'The broker name - star-submit',
#                 BrokerAlias     VARCHAR(32)                            COMMENT 'SUMS',
#                 BrokerLocation  VARCHAR(128)                           COMMENT 'The broker location  - I have no idea what this lcaotion stands for'
#);

CREATE TABLE  IF NOT EXISTS TaskDescription (

                 TaskDescriptionID  INT     NOT NULL AUTO_INCREMENT  KEY  COMMENT 'Task #id'                                                        ,
                 TaskRequestID_MD5  VARCHAR(64) NOT NULL                  COMMENT 'SUMS $REQUESTID. increased to meet ucm requrement'               ,
                 TaskSize           INT                                   COMMENT 'SUMS $nProcesses - the total number of the process for the task' ,
                 TaskRemainSize     INT                                   COMMENT 'The number of task to be finished yet. 0 = means the task has been completed',
                 EntryTime          DATETIME                              COMMENT 'SUMS time stampt - the time the task was created by the user with SUMS',
                 UpdateTime         TIMESTAMP                             COMMENT 'Last update time'                                                ,
                 LocationURL        VARCHAR(32)                           COMMENT 'URL where the Task description (SUMS session file) is originated and kept'  ,  
                 TaskUser           CHAR(32)                              COMMENT 'User name from $SUMS_USER'                                       ,
                 TaskName           CHAR(32)                              COMMENT 'Job name - $SUMS_name'                                           ,
                 TaskDescription    VARCHAR(32)                           COMMENT 'Task descriptor'                                                 ,
                 TaskCredential     VARCHAR(32)                           COMMENT 'user name (from GRID certificate), from $SUMS_AUTHENTICATED_USER', 
                 BrokerID           BrokerEnum                            COMMENT 'The ID of the broker, in our case it is SUMS'                    ,
                 CONSTRAINT UNIQUE  INDEX TaskDescriptionID (TaskRequestID_MD5)
                 );
                 
CREATE TABLE  IF NOT EXISTS JobDescription (
                 JobDescriptionID        INT         NOT NULL AUTO_INCREMENT  KEY  COMMENT 'Job #id'                ,
                 TaskDescriptionID       INT                              COMMENT 'Task #id from TaskDescription'  ,
                 TaskRequestID_MD5       VARCHAR(40) NOT NULL             COMMENT 'SUMS $REQUESTID'                ,
                 BrokerProcessID         INT                              COMMENT 'SUMS $PROCESSID'                ,
                 EntryTime               TIMESTAMP                        COMMENT 'Entry time'                     ,
                 JobLocationURL          VARCHAR(32)                      COMMENT 'URL where job is to be executed',  
                 JobUser                 CHAR(20)                         COMMENT 'User name $USER'                ,
                 CONSTRAINT UNIQUE INDEX JobID     (TaskRequestID_MD5, BrokerProcessID )                            ,
                 CONSTRAINT UNIQUE INDEX TaskJobID (TaskDescriptionID, BrokerProcessID )
                 );
                 
CREATE TABLE  IF NOT EXISTS JobDescriptionFinish (
                 JobDescriptionID  INT         NOT NULL AUTO_INCREMENT  KEY  COMMENT 'Job #id'                ,
                 EntryTime         TIMESTAMP                              COMMENT 'Entry time'                ,
                 SequenceID        ENUM('Finish')                         COMMENT 'Sequience definition entry',
                 MessageClass      ENUM('shell','root4star')  NOT NULL    COMMENT 'Extra message flag'        ,
                 SequenceValue     INT                                    COMMENT 'job exit code '
                 );
                 
CREATE TABLE  IF NOT EXISTS SequenceDictionary (
                 SequenceID    ENUM('Event')  NOT NULL  KEY  COMMENT 'Record #id',
                 Description   VARCHAR(120)
);

CREATE TABLE  IF NOT EXISTS JobTracking (
                 JobTrackingID   INT NOT NULL AUTO_INCREMENT   KEY        COMMENT 'Record #id'                                    ,
                 JobDescriptionID INT                                     COMMENT 'LAST_INSERT_ID() from the JobDescription table',
                 EntryTime       TIMESTAMP                                COMMENT 'Entry time'                                    ,
                 SequenceID      ENUM('Event')                            COMMENT 'Sequience definition entry'                    ,
                 SequenceValue   INT                                      COMMENT 'The current event # processed. for example '   ,
                 MessageContext  CHAR(20)                                 COMMENT 'could represent StFtpcMaker, StMake'           ,  #STAR maker name
                 StepEventID     ENUM('Start','Finish','EventFinish','Run','JobStart','JobFinish') NOT NULL  COMMENT 'Event ID' ,
                 MessageSeverity ENUM('FATAL','ERROR','WARN','DEBUG''INFO') NOT NULL  COMMENT 'Event outcome'                   ,  #STAR Event return code
                 MessageType      CHAR(10)                                COMMENT 'Event context'                                 ,  #Field name 
                 MessageClass    ENUM('=')              NULL              COMMENT 'Extra message flag'                            ,
                 Message         VARCHAR(120)                             COMMENT 'Body core of the message' 
                );
                
# Fill the dictionaries:

#INSERT INTO  BrokerDescriptionDictionary  SET 
#       BrokerID       ='SUMS',
#       BrokerAlias    ='SUMS',
#       BrokerName     ='star-submit',
#       BrokerLocation = 'rcf.rhic.bnl.gov'
#       ;
       
#--
#-- TABLE: SequenceEnum
#--  

CREATE TABLE IF NOT EXISTS SequenceEnum ( 

);


SHOW tables;
DESCRIBE   BrokerDescriptionDictionary;
DESCRIBE   TaskDescription;
DESCRIBE   JobDescription;
DESCRIBE   JobDescriptionFinish;
DESCRIBE   SequenceDictionary;
DESCRIBE   JobTracking;
