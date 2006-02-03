# $Id: RecreatedJobTable.sql,v 1.4 2006/02/03 02:36:12 fine Exp $
# Author: Valeri Fine (fine@bnl.gov) 26.01.2006
# Recreate the job description table
use logger; SHOW tables;
DROP TABLE JobDescription, JobTracking;
SHOW tables; SOURCE CreateJobTable.sql;
