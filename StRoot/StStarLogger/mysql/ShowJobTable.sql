# $Id: ShowJobTable.sql,v 1.1 2006/01/27 00:55:35 fine Exp $
# Author: Valeri Fine (fine@bnl.gov) 26.01.2006
# Show the job description table
use logger;
SHOW tables;
SELECT * FROM JobDescription;
DESCRIBE  JobDescription;
