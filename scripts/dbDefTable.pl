#!/usr/bin/perl 
#
# $Id: dbDefTable.pl,v 1.4 2003/01/31 02:13:35 porter Exp $
#
# Author: R. Jeff Porter
#
#***************************************************************************
#
# Description: script to parse an XML version of a c-struct  
#              & load schema into the database
#              Calls subroutines in 
#                  'dbSubs/parseXmlTable.pl'
#                  'dbSubs/dbTableCreate.pl'
#
#****************************************************************************
# 
# $Log: dbDefTable.pl,v $
# Revision 1.4  2003/01/31 02:13:35  porter
# got rid of a couple of opsolete files and got rid of environment variable
# STDB_ADMIN
#
# Revision 1.3  2003/01/09 20:30:26  porter
# upgrade of db table structure scripts
#
# Revision 1.1  2000/04/28 14:08:03  porter
# management perl scripts for db-structure accessible from StDbLib
#
#
#####################################

use Getopt::Std;

use FindBin;
use lib "$FindBin::Bin";
require "dbSubs/parseXmlTable.pl";
require "dbSubs/dbTableCreate.pl";

getopts('f:d:s:p:n:cegh');

 $inputFile=$opt_f;
 $dbName=$opt_d;
 $debug=$opt_g;
 $helpout=$opt_h;
 $serverHost=$opt_s;
 $passwd=$opt_p;
 $cstruct=$opt_c;
my $empty=$opt_e;
my $strTable=$opt_n;

if($helpout or (!$inputFile or !$serverHost)){ Usage();};
if(!$strTable && !$cstruct && !$empty) { Usage(); }

if($debug){print $inputFile," \n";}

##########################################
# Some Global Variables
##########################################

$outfile='';
$tableName='';
$tableComment='';
$namedRef='';
@elements=();
@elengths=();
@ecomments=();
@atypes=();
@amysqltypes=();
@mysqltypes=();
@etypes=();
@erelationID=();
@emask=();
@edone=();
@eID=();
@eposition=();
@oelements=();
@ocomments=();
@orelationID=();
@omask=();
@oID=();
@oposition=();


#########################################
#
# Parse the XML db-Definition to load into
# memory
#
#########################################

parse_table(fileName=>$inputFile,DEBUG=>$debug,DataBase=>$dbName);

print "******************************\n";
print "*\n* Running dbDefTable.pl \n*\n"; 
  print "Defining TableName= ",$tableName,"\n";
  print "In DataBase = ",$dbName,"\n";

#  for($i=0; $i<=$#elements;$i++){
#      print $etypes[$i]," ",$elements[$i],"[",$elengths[$i],"]\n";
#  }

########################################
#
# Set some variables in case of 
# schema evolution
#
########################################

$#eID=$#emask=$#relationID=$#eposition=$#edone=$#elements;

for($i=0;$i<=$#elements;$i++){
    $edone[$i]=0;
    $emask[$i]=1;
}

my $passStoreTable='';
if($cstruct && $strTable){ Usage(); };
if($cstruct){
   $passStoreTable=$tableName;
}
if($strTable){
    $passStoreTable=$strTable;
}


dbTableCreate(dbHostName=>$serverHost, DEBUG=>$debug, TableName=>$tableName, dbName=>$dbName, NameRef=>$namedRef, PassWord=>$passwd, StoreTable=>$passStoreTable);

#print "*\n* End of dbDefTable.pl \n*\n";
print "**************************************************************\n";


#########################################################################
#
#
##########################################################################
sub Usage() {

    print "\n";
print "****  Usage   :: dbDefTable.pl -f xmlfile -s server [-p passwd] [-d database]  (-n storename|-c|-e) [-g|-h]\n";
print "****  Purpose :: Loads schema for a single table stored in an XML file\n";
    print "                 -f xml-file\n";
    print "                 -s server has the form 'hostname:portnumber' or simply\n";
    print "                 'hostname' if portnumber=3306\n";
    print "                 -p passwd option if one is needed to write to specified db \n";
print "                 -d database option overides database in the xml-file\n";
    print "\n               One of the following (-n store | -c | -e) is REQUIRED\n";
print "                 -n store => storage table name  \n";
print "                 -c => use tableName in xml file for storage table \n";
print "                 -e => do not create a storage tables\n\n"; 

                    
print "                 -g for debug output\n";
print "                 -h for this message \n\n";

print "****  Requires  **** Write-Access to database\n";

exit;
}








