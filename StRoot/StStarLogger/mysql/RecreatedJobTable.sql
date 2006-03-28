# $Id: RecreatedJobTable.sql,v 1.5 2006/03/28 20:17:43 fine Exp $
# Author: Valeri Fine (fine@bnl.gov) 26.01.2006
# Recreate the job description table
use logger; SHOW tables;
DROP TABLE TaskDescription, JobDescription, JobTracking;
SHOW tables; SOURCE CreateJobTable.sql;
