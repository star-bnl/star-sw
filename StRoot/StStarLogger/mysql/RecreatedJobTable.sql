# $Id: RecreatedJobTable.sql,v 1.3 2006/01/27 00:55:35 fine Exp $
# Author: Valeri Fine (fine@bnl.gov) 26.01.2006
# Recreate the job description table
use logger;
SHOW tables;
DROP TABLE JobDescription;
SHOW tables;
SOURCE CreateJobTable.sql;
