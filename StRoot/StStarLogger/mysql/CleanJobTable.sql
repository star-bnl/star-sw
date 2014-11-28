# $Id: CleanJobTable.sql,v 1.2 2006/01/27 00:55:35 fine Exp $
# Author: Valeri Fine (fine@bnl.gov) 26.01.2006
# Clear the job description table

use logger;
SHOW tables;
DELETE FROM JobDescription;
DESCRIBE  JobDescription;
