# $Id: RecreatedJobTable.sql,v 1.7 2006/07/05 21:29:15 fine Exp $
# Author: Valeri Fine (fine@bnl.gov) 26.01.2006
# Recreate the job description table
use logger; SHOW tables;
DROP TABLE BrokerDescriptionDictionary, 
           TaskDescription            ,
           JobDescription             ,
           JobDescriptionFinish       ,
           SequenceDictionary         ,
           JobTracking           
           ;
SHOW tables; SOURCE CreateJobTable.sql;
SHOW tables;
