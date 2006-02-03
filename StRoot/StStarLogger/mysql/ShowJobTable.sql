# $Id: ShowJobTable.sql,v 1.2 2006/02/03 19:51:53 fine Exp $
# Author: Valeri Fine (fine@bnl.gov) 26.01.2006
# Show the job description table
use logger;
SHOW tables;
SELECT * FROM JobDescription,JobTracking WHERE JobDescription.dataId=JobTracking.dataId;
DESCRIBE  JobDescription;
DESCRIBE  JobTracking;
