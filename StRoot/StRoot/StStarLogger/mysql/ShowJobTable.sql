# $Id: ShowJobTable.sql,v 1.3 2006/03/30 03:19:58 fine Exp $
# Author: Valeri Fine (fine@bnl.gov) 26.01.2006
# Show the job description table
use logger;
SHOW tables;
SELECT * FROM TaskDescription,JobDescription WHERE TaskDescription.taskId=JobDescription.taskId;
DESCRIBE  JobDescription;
DESCRIBE  JobTracking;
DESCRIBE  TaskDescription;
