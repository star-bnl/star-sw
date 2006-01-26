# $id$
# Author: Valeri Fine (fine@bnl.gov) 26.01.2006
#  Recreate the job description tables
use logger;
SHOW tables;
DROP TABLE JobDescription;
SHOW tables;
SOURCE CreateJobTable.sql;
