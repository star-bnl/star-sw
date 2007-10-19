use ucmdb;
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
-- Table structure for table `Tasks`
-- 

DROP TABLE IF EXISTS Tasks;
CREATE TABLE IF NOT EXISTS Tasks (
  taskID           int(11)     NOT NULL AUTO_INCREMENT  KEY COMMENT 'ID of task when entry is created, unique in table',
  brokerTaskID     char(40)    NOT NULL             COMMENT 'ID of task as assigned by Broker',
  brokerID         int(11)     NOT NULL default '2' COMMENT 'The ID of the broker that created task',
  requesterID      varchar(32) NOT NULL             COMMENT 'An requester ID from the RequesterDictionary table',
  taskName         varchar(32) default NULL         COMMENT 'Short name of task as assigned by user through broker',
  taskDescription  varchar(64) NOT NULL             COMMENT 'Text description of task as assigned by user through broker',
  taskSize         int(11)     NOT NULL default '1' COMMENT 'Number of jobs in the task',
  taskRemainSize   int(11)     NOT NULL default '1' COMMENT 'Number of jobs no completed',
  submitTime       datetime    default NULL         COMMENT 'Wall time that task was submitted to broker',
  updateTime       timestamp   NOT NULL default CURRENT_TIMESTAMP COMMENT 'Wall time that task row was last updated',
  CONSTRAINT UNIQUE INDEX taskID (brokerTaskID)
);

INSERT INTO `Tasks` (`taskID`, `brokerTaskID`, `requesterID`, `taskName`, `taskDescription`, `taskSize`, `taskRemainSize`, `submitTime`, `updateTime`) VALUES 
(1, 'brokerB1taskFromJoe', 1, 'taskJ1', 'Joe Task B1', 2, 1, '2007-09-20 09:49:41', '2007-09-24 09:54:40'),
(2, 'brokerB1taskFromMary', 2, 'taskM1', 'Mary Task B1', 2, 0, '2007-09-20 09:49:41', '2007-09-24 09:54:40'),
(3, 'brokerB2taskFromJoe', 1, 'taskJ2', 'Joe Task B2', 2, 2, '2007-09-20 09:51:33', '2007-09-21 09:51:42');

-- --------------------------------------------------------

-- 
-- Table structure for table `Jobs`
-- 

DROP TABLE IF EXISTS Jobs;
CREATE TABLE IF NOT EXISTS Jobs (
  jobID             int(11)      NOT NULL AUTO_INCREMENT KEY COMMENT 'ID of job when entry is created, unique within table',
  brokerJobID       int(11)      NOT NULL      COMMENT 'ID of job as assigned by Broker',
  taskID            int(11)      NOT NULL      COMMENT 'Foreign key reference to Tasks table',
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
  executionUserName varchar(32)  NOT NULL      COMMENT 'A login ID on the local resource site & worker node that actually executes',
  stateID           int(11)      NOT NULL default '1' COMMENT 'Foreign key reference to StateDictionary table',
  CONSTRAINT UNIQUE INDEX jobID (brokerJobID, taskID)
);

INSERT INTO `Jobs` (`jobID`, `brokerJobID`, `taskID`, `gridJobID`, `localJobID`, `gridSubmitTime`, `localSubmitTime`, `siteLocation`, `queue`, `queuePosition`, `nodeLocation`, `startTime`, `executionUserName`, `stateID`) VALUES 
(1, 101, 1, 'uuid:5d85bb28-c67c-11db-ac3b-00163e010001', 301, '2007-09-20 09:49:41', '2007-09-20 10:00:00', 'grid.txcorp.com', 'schedulerQueue', 17, 'node1.txcorp.com', '2007-09-20 10:01:50', 'userx', 8),
(2, 102, 1, 'uuid:5d85bb28-c67c-11db-ac3b-00163e010002', 302, '2007-09-21 10:07:05', '2007-09-21 10:10:10', 'grid.txcorp.com', 'schedulerQueue', 18, 'node2.txcorp.com', '2007-09-21 10:50:00', 'usery', 4),
(3, 103, 2, 'uuid:5d85bb28-c67c-11db-ac3b-00163e010003', 303, '2007-09-20 09:49:41', '2007-09-20 12:00:00', 'grid.txcorp.com', 'schedulerQueue', 19, 'node3.bnl.gov', '0000-00-00 00:00:00', 'userbx', 3),
(4, 104, 2, 'uuid:5d85bb28-c67c-11db-ac3b-00163e010004', 304, '2007-09-20 09:49:41', '2007-09-20 09:51:47', 'grid.txcorp.com', 'schedulerQueue', 20, 'node4.bnl.gov', '2007-09-20 13:00:40', 'userby', 9),
(5, 105, 3, 'uuid:5d85bb28-c67c-11db-ac3b-00163e010005', 404, '2007-09-20 09:49:41', '2007-09-20 13:10:15', 'grid.txcorp.com', 'schedulerQueue', 21, 'node5.txcorp.com', '2007-09-21 13:10:15', 'userx', 8),
(6, 106, 3, 'uuid:5d85bb28-c67c-11db-ac3b-00163e010006', 406, '2007-09-20 09:49:41', '2007-09-20 13:10:15', 'grid.txcorp.com', 'schedulerQueue', 22, 'node6.txcorp.com', NULL, 'userz', 1);
INSERT INTO `Jobs` (`jobID`, `brokerJobID`, `taskID`, `gridJobID`, `localJobID`, `gridSubmitTime`, `localSubmitTime`, `siteLocation`, `queue`, `queuePosition`, `nodeLocation`, `executionUserName`) VALUES 
(7, 107, 3, 'uuid:5d85bb28-c67c-11db-ac3b-00163e010007', 407, '2007-09-20 09:49:41', '2007-09-20 13:10:15', 'grid.txcorp.com', 'schedulerQueue', 23, 'node6.txcorp.com', 'userz');


-- --------------------------------------------------------

-- 
-- Table structure for table `Events`
-- 

DROP TABLE IF EXISTS JobEvents;
CREATE TABLE IF NOT EXISTS JobEvents (
  eventID           int(11)      NOT NULL AUTO_INCREMENT KEY COMMENT 'ID of event when entry is created, unique within table',
  jobID             int(11)      NOT NULL COMMENT 'Job that this message is associated with',
  levelID			  int(11)      NOT NULL COMMENT 'The ID of the log level of the event (WARNING, DEBUG, ERROR, etc.)',
  context			  CHAR(20)     NOT NULL COMMENT 'The bulk category of the log event or the facilty or code where the event happens',
  time				  datetime     NOT NULL COMMENT 'Time that event was recorded by the Tracking Library',
  stageID			  int(11)      NOT NULL COMMENT 'The ID of the logging stage of the event (i.e., START, STATUS, or END)',
  messageKey		  CHAR(20)     COMMENT 'A user defined property key or SYSTEM for system event',
  messageValue      VARCHAR(120) COMMENT 'A user defined property value or textual content of a log message for a system event'
);

INSERT INTO `JobEvents` (`eventID`, `jobID`, `levelID`, `context`, `time`, `stageID`, `messageKey`, `messageValue`) VALUES 
(1, 1, 4, 'root4star.eventrecon.calorimetry', '2007-07-12 10:25:05', 1, 'myvar', '200'),
(2, 1, 4, 'root4star.eventrecon.calorimetry', '2007-07-12 10:30:00', 3, 'myvar', '400'),
(3, 2, 3, 'root4star.app', '2007-07-12 10:15:00', 1, 'SYSTEM', 'entering analysis'),
(4, 2, 3, 'root4star.app', '2007-07-12 10:25:00', 2, 'SYSTEM', 'entering calorimeter reconstruction'),
(5, 2, 3, 'root4star.app', '2007-07-12 10:40:00', 3, 'SYSTEM', 'exiting analysis'),
(6, 3, 1, 'root4star.app.Unsubmitted', '2007-07-12 10:15:00', 1, 'SYSTEM', 'level is TRACE'),
(7, 3, 2, 'root4star.app.StageIn', '2007-07-12 10:15:00', 3,  'SYSTEM', 'level is DEBUG'),
(8, 4, 3, 'root4star.app.Pending', '2007-07-12 10:15:00', 1, 'SYSTEM', 'level is INFO'),
(9, 4, 4, 'root4star.app.Active', '2007-07-12 10:15:00', 1, 'SYSTEM', 'level is NOTICE'),
(10, 4, 5, 'root4star.app.Suspended', '2007-07-12 10:15:00', 2, 'SYSTEM', 'level is WARNING'),
(11, 4, 6, 'root4star.app.StageOut', '2007-07-12 10:15:00', 3, 'SYSTEM', 'level is ERROR'),
(12, 5, 7, 'root4star.app.CleanUp', '2007-07-12 10:15:00', 1, 'SYSTEM', 'level is CRITICAL'),
(13, 6, 8, 'root4star.app.Done', '2007-07-12 10:15:00', 1, 'SYSTEM', 'level is ALERT'),
(14, 6, 9, 'root4star.app.Failed', '2007-07-12 10:15:00', 3, 'SYSTEM', 'level is FATAL');

-- --------------------------------------------------------

-- 
-- Table structure for table `RequesterDictionary` and initial data
-- 

DROP TABLE IF EXISTS RequesterDictionary;
CREATE TABLE IF NOT EXISTS RequesterDictionary (
  requesterID           int(11)          NOT NULL AUTO_INCREMENT KEY COMMENT 'The broker-assigned ID of the user making the initial task request from the broker',
  requesterName         varchar(32)      NOT NULL COMMENT 'system or portal login of user that initially submits task to broker',
  requesterDescription  varchar(64)      NOT NULL default 'No Description' COMMENT 'A string representing the grid crendentials of the user, could be DN of X509 cert',
  requesterAuthor       varchar(8)       NOT NULL default 'ucmAdmin' COMMENT 'The author that last updated this information',
  requesterUpdateTime   timestamp        NOT NULL default CURRENT_TIMESTAMP COMMENT 'The time that the information was last updated'
);

INSERT INTO RequesterDictionary (`requesterID`, `requesterName`, `requesterDescription`, `requesterAuthor`, `requesterUpdateTime`) VALUES 
(1, 'joeScientist', 'Subject: DC=org, DC=doegrids, OU=People, CN=Joe Smith 12345', 'ucmAdmin', '2007-07-12 10:25:00'),
(2, 'maryScientist', 'Subject: DC=org, DC=doegrids, OU=People, CN=Mary Baker 67890', 'ucmAdmin', '2007-07-12 10:26:00');

-- --------------------------------------------------------

-- 
-- Table structure for table `LevelDictionary` and initial data
-- 

DROP TABLE IF EXISTS LevelDictionary;
CREATE TABLE IF NOT EXISTS LevelDictionary (
  levelID          int(11)     NOT NULL AUTO_INCREMENT KEY COMMENT 'ID of the level for possible events',
  levelName        varchar(32) NOT NULL COMMENT 'Short name of the level',
  levelDescription varchar(64) NOT NULL default 'No Description' COMMENT 'A string description of the level',
  levelAuthor      varchar(8)  NOT NULL default 'ucmAdmin' COMMENT 'The author that last updated this information',
  levelUpdateTime  timestamp   NOT NULL default CURRENT_TIMESTAMP COMMENT 'The time that the information was last updated'
);

INSERT INTO LevelDictionary (`levelID`, `levelName`, `levelDescription`, `levelAuthor`, `levelUpdateTime`) VALUES
(1, 'TRACE', 'The TRACE level is the finest granularity, similar to stepping through the component or system.', 'ucmAdmin', '2007-10-04 10:25:35'),
(2, 'DEBUG', 'The DEBUG level is lower level information concerning program logic decisions, internal state, etc.', 'ucmAdmin', '2007-10-04 10:25:35'),
(3, 'INFO', 'The INFO level designates informational messages that would be useful to a deployer or administrator ', 'ucmAdmin', '2007-10-04 10:25:35'),
(4, 'NOTICE', 'The NOTICE level designates normal but significant condition', 'ucmAdmin', '2007-10-04 10:25:35'),
(5, 'WARNING', 'The WARNING level designates roblems that are recovered from, usually.', 'ucmAdmin', '2007-10-04 10:25:35'),
(6, 'ERROR', 'The ERROR level designates errors in the component; not errors from elsewhere.', 'ucmAdmin', '2007-10-04 10:25:35'),
(7, 'CRITICAL', 'The CRITICAL level designates critical conditions on the system.', 'ucmAdmin', '2007-10-04 10:25:35'),
(8, 'ALERT', 'The ALERT level designates that cction must be taken immediately.', 'ucmAdmin', '2007-10-04 10:25:35'),
(9, 'FATAL', 'The FATAL level designates that a component cannot continue or system is unusable.', 'ucmAdmin', '2007-10-04 10:25:35');

-- --------------------------------------------------------

-- 
-- Table structure for table `StateDictionary` and initial data
-- 

DROP TABLE IF EXISTS StateDictionary;
CREATE TABLE IF NOT EXISTS StateDictionary (
  stateID           int(11)     NOT NULL AUTO_INCREMENT KEY COMMENT 'ID of states of possible execution status for jobs',
  stateName         varchar(32) NOT NULL COMMENT 'Short name for the job execution state',
  stateDescription  varchar(64) NOT NULL default 'No Description' COMMENT 'A string description of the job execution state',
  stateAuthor       varchar(8)  NOT NULL default 'ucmAdmin' COMMENT 'The author that last updated this information',
  stateUpdateTime   timestamp   NOT NULL default CURRENT_TIMESTAMP COMMENT 'The time that the information was last updated'
);

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
-- Table structure for table `BrokerDictionary` and initial data
-- 

DROP TABLE IF EXISTS BrokerDictionary;
CREATE TABLE IF NOT EXISTS BrokerDictionary (
  brokerID          int(11)     NOT NULL AUTO_INCREMENT KEY COMMENT 'The ID of the broker as assigned by the UCM system',
  brokerName        varchar(32) NOT NULL COMMENT 'A short name for the broker assigned by the UCM system',
  brokerDescription varchar(64) NOT NULL default 'No Description' COMMENT 'A string description of the broker including the location',
  brokerAuthor      varchar(8)  NOT NULL default 'ucmAdmin' COMMENT 'The author that last updated this information',
  brokerUpdateTime  timestamp   NOT NULL default CURRENT_TIMESTAMP COMMENT 'The time that the information was last updated'
);

INSERT INTO BrokerDictionary (`brokerID`, `brokerName`, `brokerDescription`, `brokerAuthor`, `brokerUpdateTime`) VALUES 
(1, 'SUMS', 'STAR Unified MetaScheduler at Brookhaven National Lab', 'ucmAdmin', '2007-07-12 10:25:35'),
(2, 'LSF-BNL', 'LSF Scheduler located at Brookhaven National Lab', 'ucmAdmin', '2007-07-12 10:25:35');

-- --------------------------------------------------------

-- 
-- Table structure for table `StageDictionary` and inital data
-- 

DROP TABLE IF EXISTS StageDictionary;
CREATE TABLE IF NOT EXISTS StageDictionary (
  stageID          int(11)     NOT NULL AUTO_INCREMENT KEY COMMENT 'ID of the stage for the event message',
  stageName        varchar(32) NOT NULL COMMENT 'Short name of the stage',
  stageDescription varchar(64) NOT NULL default 'No Description' COMMENT 'A string description of the stage',
  stageAuthor      varchar(8)  NOT NULL default 'ucmAdmin' COMMENT 'The author that last updated this information',
  stageUpdateTime  timestamp   NOT NULL default CURRENT_TIMESTAMP COMMENT 'The time that the information was last updated'
);

INSERT INTO StageDictionary (`stageID`, `stageName`, `stageDescription`, `stageAuthor`, `stageUpdateTime`) VALUES
(1, 'START', 'The START stage is for events that start some process.', 'ucmAdmin', '2007-10-04 10:25:35'),
(2, 'STATUS', 'The STATUS stage is for events that are updates or singleton events.', 'ucmAdmin', '2007-10-04 10:25:35'),
(3, 'END', 'The END stage is for events that end some process pairing up with a START stage.', 'ucmAdmin', '2007-10-04 10:25:35');

-- --------------------------------------------------------

-- 
-- Depricated Tables should be dropped.
-- 
DROP TABLE IF EXISTS SeverityDictionary;
DROP TABLE IF EXISTS FacilityDictionary;
DROP TABLE IF EXISTS Messages;


SHOW tables;

DESCRIBE   Tasks;
DESCRIBE   Jobs;
DESCRIBE   JobEvents;
DESCRIBE   RequesterDictionary;
DESCRIBE   LevelDictionary;
DESCRIBE   StateDictionary;
DESCRIBE   BrokerDictionary;
DESCRIBE   StageDictionary;
