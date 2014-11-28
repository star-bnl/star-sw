#!/usr/local/bin/tcsh
# $Id: DumpMessages.csh,v 1.2 2007/09/24 16:52:04 fine Exp $
# Author: Valeri Fine (fine@bnl.gov) 26.01.2006
# Create the procedure to work with  logger Db
echo "---"
echo The  dump of the logger.Messages table
echo --------------------------------------
mysql  -h heston.star.bnl.gov -u StarLogger -plogger <<MYSQLCODE
 use logger;
 #-  new tables         
 SELECT * FROM  logger.Messages;
MYSQLCODE

exit
