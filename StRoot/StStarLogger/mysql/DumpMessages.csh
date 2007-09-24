#!/usr/local/bin/tcsh
# $Id: DumpMessages.csh,v 1.1 2007/09/24 16:50:38 fine Exp $
# Author: Valeri Fine (fine@bnl.gov) 26.01.2006
# Create the procedure to work with  logger Db
echo "---"
echo The  List of the new Tables
echo ------------------------------------
mysql  -h heston.star.bnl.gov -u StarLogger -plogger <<MYSQLCODE
 use logger;
 #-  new tables         
 SELECT * FROM  logger.Messages;
MYSQLCODE

exit
