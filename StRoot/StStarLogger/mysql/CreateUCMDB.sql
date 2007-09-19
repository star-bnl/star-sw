use logger;
--
-- Creation SQL script for Database: `ucmdb` for the UCM project
--
-- Author: David Alexander
-- Date: 12 July 2007
--
-- Tech-X Corporation
-- Copyright 2007
--
--

-- --------------------------------------------------------

--
-- Create database and DB user
--

-- Commented out to prevent accidentally clobbering existing database
-- Note that PASSWORD below should be replaced with actual password

-- DROP DATABASE IF EXISTS ucmdb;
-- CREATE DATABASE ucmdb;
-- GRANT SELECT,INSERT,UPDATE,DELETE,CREATE,DROP
-- ON ucmdb.* TO 'ucm'@'localhost' IDENTIFIED by 'PASSWORD';

-- --------------------------------------------------------

-- 
-- Database: `ucmdb`
-- 

-- --------------------------------------------------------

-- 
-- Table structure for table `BrokerDictionary`
-- 

DROP TABLE IF EXISTS BrokerDictionary;
CREATE TABLE IF NOT EXISTS BrokerDictionary (
  brokerID          int(11)     NOT NULL COMMENT 'The ID of the broker as assigned by the UCM system',
  brokerName        varchar(16) NOT NULL COMMENT 'A short name for the broker assigned by the UCM system',
  brokerDescription varchar(64) NOT NULL default 'No Description' COMMENT 'A string description of the broker including the location',
  brokerAuthor      varchar(8)  NOT NULL default 'ucmAdmin' COMMENT 'The author that last updated this information',
  brokerUpdateTime  timestamp   NOT NULL default CURRENT_TIMESTAMP COMMENT 'The time that the information was last updated',
  PRIMARY KEY  (brokerID)
);

-- 
-- Dumping data for table `BrokerDictionary`
-- 

INSERT INTO BrokerDictionary (`brokerID`, `brokerName`, `brokerDescription`, `brokerAuthor`, `brokerUpdateTime`) VALUES 
(1, 'SUMS', 'STAR Unified MetaScheduler at Brookhaven National Lab', 'ucmAdmin', '2007-07-12 10:25:35'),
(2, 'LSF-BNL', 'LSF Scheduler located at Brookhaven National Lab', 'ucmAdmin', '2007-07-12 10:25:35');

-- --------------------------------------------------------

-- 
-- Table structure for table `FacilityDictionary`
-- 

DROP TABLE IF EXISTS FacilityDictionary;
CREATE TABLE IF NOT EXISTS FacilityDictionary (
  facilityID          int(11)     NOT NULL COMMENT 'ID of the facility for possible messages',
  facilityName        varchar(32) NOT NULL COMMENT 'Short name of the facility',
  facilityDescription varchar(64) NOT NULL default 'No Description' COMMENT 'A string description of the facility',
  facilityAuthor      varchar(8)  NOT NULL default 'ucmAdmin' COMMENT 'The author that last updated this information',
  facilityUpdateTime  timestamp   NOT NULL default CURRENT_TIMESTAMP COMMENT 'The time that the information was last updated',
  PRIMARY KEY  (facilityID)
);

-- 
-- Dumping data for table `FacilityDictionary`
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table `Jobs`
-- 

DROP TABLE IF EXISTS Jobs;
CREATE TABLE IF NOT EXISTS Jobs (
  jobID             int(11)      NOT NULL AUTO_INCREMENT COMMENT 'ID of job when entry is created, unique within table',
  taskID            int(11)      NOT NULL      COMMENT 'Foreign key reference to Tasks table',
  brokerJobID       int(11)      NOT NULL      COMMENT 'ID of job as assigned by Broker',
  gridJobID         varchar(64)  default NULL  COMMENT 'ID for job as assigned by Grid Resource Allocation Manager (GRAM)',
  localJobID        int(11)      default NULL  COMMENT 'ID for job as assigned by local resource manager or scheduler',
  gridSubmitTime    datetime     default NULL  COMMENT 'Time that job was submitted to the GRAM',
  localSubmitTime   datetime     default NULL  COMMENT 'Time that job was submitted to the local resource manager or scheduler',
  siteLocation      varchar(64)  NOT NULL      COMMENT 'Physical local of job, could be grid site or local cluster description',
  queue             varchar(64)  NOT NULL      COMMENT 'Name and short description of queue that shedules job',
  queuePosition     int(11)      NOT NULL      COMMENT 'Integer slot position of job in local resource manager or scheduler',
  nodeLocation      varchar(64)  NOT NULL      COMMENT 'Name of worker node that job lands on',
  startTime         datetime     default  NULL COMMENT 'Time that job started execution',
  updateTime        timestamp    NOT NULL default CURRENT_TIMESTAMP COMMENT 'Time that job execution state was last updated',
  endTime           datetime     default NULL  COMMENT 'Time that job completed execution',
  executionUserID   int(11)      default NULL  COMMENT 'A login ID on the local resource site & worker node that actually executes',
  executionUserName varchar(32)  NOT NULL      COMMENT 'A login ID on the local resource site & worker node that actually executes',
  stateID           int(11)      NOT NULL default '1' COMMENT 'Foreign key reference to StateDictionary table',
  brokerTaskID      int(11)      NOT NULL      COMMENT 'ID of task as assigned by Broker',
  PRIMARY KEY  (jobID)
);

-- 
-- Dumping data for table `Jobs`
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table `Messages`
-- 

DROP TABLE IF EXISTS Messages;
CREATE TABLE IF NOT EXISTS Messages (
  messageID        int(11)      NOT NULL AUTO_INCREMENT COMMENT 'ID of message',
  jobID            int(11)      NOT NULL COMMENT 'Job that this message is associated with',
  facilityID       int(11)      NOT NULL COMMENT 'The ID of the facility or bulk category of messages',
  severityID       int(11)      NOT NULL COMMENT 'The ID of the severity (warning, debug, error, etc.) of the message',
  content          varchar(512) NOT NULL COMMENT 'The textual content of the message',
  eventTime        timestamp    NOT NULL default CURRENT_TIMESTAMP COMMENT 'Time that event happened which corresponds to the message',
  SequenceValue   INT                                      COMMENT 'The current event # processed. for example '   ,
  MessageContext  CHAR(20)                                 COMMENT 'could represent StFtpcMaker, StMake'           ,  #STAR maker name
  StepEventID     ENUM('Start','Finish','EventFinish','Run','JobStart','JobFinish') NOT NULL  COMMENT 'Event ID'   ,
  MessageType     CHAR(10)                                COMMENT 'Event context'                                  ,  #Field name
  MessageClass    ENUM('=')              NULL              COMMENT 'Extra message flag'                            ,
  Message         VARCHAR(120)                             COMMENT 'Body core of the message'                      ,
  PRIMARY KEY  (messageID)
);

-- 
-- Dumping data for table `Messages`
-- 


-- --------------------------------------------------------

-- 
-- Table structure for table `RequesterDictionary`
-- 

DROP TABLE IF EXISTS RequesterDictionary;
CREATE TABLE IF NOT EXISTS RequesterDictionary (
  requesterID           int(10) unsigned NOT NULL COMMENT 'The broker-assigned ID of the user making the initial task request from the broker',
  requesterName         char(8)          NOT NULL COMMENT 'system or portal login of user that initially submits task to broker',
  requesterDescription  varchar(64)      NOT NULL default 'No Description' COMMENT 'A string representing the grid crendentials of the user, could be DN of X509 cert',
  requesterAuthor       varchar(8)       NOT NULL default 'ucmAdmin' COMMENT 'The author that last updated this information',
  requesterUpdateTime   timestamp        NOT NULL default CURRENT_TIMESTAMP COMMENT 'The time that the information was last updated',
  PRIMARY KEY  (requesterID)
);

-- 
-- Dumping data for table `RequesterDictionary`
-- 

INSERT INTO RequesterDictionary (`requesterID`, `requesterName`, `requesterDescription`, `requesterAuthor`, `requesterUpdateTime`) VALUES 
(1, 'alexanda', 'Subject: DC=org, DC=doegrids, OU=People, CN=David A. Alexander 7', 'ucmAdmin', '2007-07-12 10:25:35');

-- --------------------------------------------------------

-- 
-- Table structure for table `SeverityDictionary`
-- 

DROP TABLE IF EXISTS SeverityDictionary;
CREATE TABLE IF NOT EXISTS SeverityDictionary (
  severityID          int(11)     NOT NULL COMMENT 'ID of the severity for possible messages',
  severityName        varchar(32) NOT NULL COMMENT 'Short name of the severity',
  severityDescription varchar(64) NOT NULL default 'No Description' COMMENT 'A string description of the severity',
  severityAuthor      varchar(8)  NOT NULL default 'ucmAdmin' COMMENT 'The author that last updated this information',
  severityUpdateTime  timestamp   NOT NULL default CURRENT_TIMESTAMP COMMENT 'The time that the information was last updated',
  PRIMARY KEY  (severityID)
);

-- 
-- Dumping data for table `SeverityDictionary`
-- 

INSERT INTO SeverityDictionary (`severityID`, `severityName`, `severityDescription`, `severityAuthor`, `severityUpdateTime`) VALUES 
(1, 'TRACE', 'The TRACE Level designates finer-grained informational events th', 'ucmAdmin', '2007-07-12 10:25:35'),
(2, 'DEBUG', 'The DEBUG Level designates fine-grained informational events tha', 'ucmAdmin', '2007-07-12 10:25:35'),
(3, 'INFO', 'The INFO level designates informational messages that highlight ', 'ucmAdmin', '2007-07-12 10:25:35'),
(4, 'WARN', 'The WARN level designates potentially harmful situations', 'ucmAdmin', '2007-07-12 10:25:35'),
(5, 'ERROR', 'The ERROR level designates error events that might still allow t', 'ucmAdmin', '2007-07-12 10:25:35'),
(6, 'FATAL', 'The FATAL level designates very severe error events that will pr', 'ucmAdmin', '2007-07-12 10:25:35');

-- --------------------------------------------------------

-- 
-- Table structure for table `StateDictionary`
-- 

DROP TABLE IF EXISTS StateDictionary;
CREATE TABLE IF NOT EXISTS StateDictionary (
  stateID           int(11)     NOT NULL COMMENT 'ID of states of possible execution status for jobs',
  stateName         varchar(32) NOT NULL COMMENT 'Short name for the job execution state',
  stateDescription  varchar(64) NOT NULL default 'No Description' COMMENT 'A string description of the job execution state',
  stateAuthor       varchar(8)  NOT NULL default 'ucmAdmin' COMMENT 'The author that last updated this information',
  stateUpdateTime   timestamp   NOT NULL default CURRENT_TIMESTAMP COMMENT 'The time that the information was last updated',
  PRIMARY KEY  (stateID)
);

-- 
-- Dumping data for table `StateDictionary`
-- 

INSERT INTO StateDictionary (`stateID`, `stateName`, `stateDescription`, `stateAuthor`, `stateUpdateTime`) VALUES 
(1, 'Unsubmitted', 'Job received by GRAM, but not submitted to scheduler', 'ucmAdmin', '2007-07-12 10:25:35'),
(2, 'StageIn', 'Grid site staging files', 'ucmAdmin', '2007-07-12 10:25:35'),
(3, 'Pending', 'Job submitted to scheduler', 'ucmAdmin', '2007-07-12 10:25:35'),
(4, 'Active', 'Scheduler running job', 'ucmAdmin', '2007-07-12 10:25:35'),
(5, 'Suspended', 'Scheduler holding job', 'ucmAdmin', '2007-07-12 10:25:35'),
(6, 'StageOut', 'GRAM staging files', 'ucmAdmin', '2007-07-12 10:25:35'),
(7, 'CleanUp', 'GRAM cleaning up', 'ucmAdmin', '2007-07-12 10:25:35'),
(8, 'Done', 'Job finished from scheduler and GRAM', 'ucmAdmin', '2007-07-12 10:25:35'),
(9, 'Failed', 'Job failed at any point in the execution', 'ucmAdmin', '2007-07-12 10:25:35');

-- --------------------------------------------------------

-- 
-- Table structure for table `Tasks`
-- 

DROP TABLE IF EXISTS Tasks;
CREATE TABLE IF NOT EXISTS Tasks (
  taskID           int(11)     NOT NULL AUTO_INCREMENT COMMENT 'ID of task when entry is created, unique in table',
  brokerID         int(11)     NOT NULL default '2' COMMENT 'The ID of the broker that created task',
  requesterName    varchar(32) NOT NULL             COMMENT 'An requester (user) name from the TaskRequesters table',
  taskName         varchar(32) default NULL         COMMENT 'Short name of task as assigned by user through broker',
  taskDescription  varchar(64) NOT NULL             COMMENT 'Text description of task as assigned by user through broker',
  taskSize         int(11)     NOT NULL default '1' COMMENT 'Number of jobs in the task',
  taskRemainSize   int(11)     NOT NULL default '1' COMMENT 'Number of jobs no completed',
  submitTime       datetime    default NULL         COMMENT 'Wall time that task was submitted to broker',
  updateTime       timestamp   NOT NULL default CURRENT_TIMESTAMP COMMENT 'Wall time that task row was last updated',
  endTime          datetime    default  NULL        COMMENT 'Wall time that task completed execution',
  brokerTaskID     char(32)    NOT NULL             COMMENT 'ID of task as assigned by Broker',
  PRIMARY KEY  (taskID)
);

-- 
-- Dumping data for table `Tasks`
-- 

SHOW tables;

DESCRIBE   BrokerDictionary;
DESCRIBE   FacilityDictionary;
DESCRIBE   Jobs;
DESCRIBE   Messages;
DESCRIBE   RequesterDictionary;
DESCRIBE   SeverityDictionary;
DESCRIBE   StateDictionary;
DESCRIBE   Tasks;
