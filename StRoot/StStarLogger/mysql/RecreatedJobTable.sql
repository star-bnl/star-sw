# $Id: RecreatedJobTable.sql,v 1.2 2006/01/26 23:56:27 fine Exp $
# Author: Valeri Fine (fine@bnl.gov) 26.01.2006
#  Recreate the job description tables
use logger;
SHOW tables;
DROP TABLE JobDescription;
SHOW tables;
SOURCE CreateJobTable.sql;
