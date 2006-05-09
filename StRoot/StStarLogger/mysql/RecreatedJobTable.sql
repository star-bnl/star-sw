# $Id: RecreatedJobTable.sql,v 1.6 2006/05/09 23:31:21 fine Exp $
# Author: Valeri Fine (fine@bnl.gov) 26.01.2006
# Recreate the job description table
use logger; SHOW tables;
DROP TABLE TaskDescription, JobDescription, JobTracking;
SHOW tables; SOURCE CreateJobTable.sql;
SHOW tables;
