#!/usr/bin/perl 
#
# $Id: dbMakeFiles.pl,v 1.5 2003/01/31 02:13:35 porter Exp $
#
# Author: R. Jeff Porter
#
#***************************************************************************
#
# Description: script to create header (*.h) or idl (*.idl) files
#              from database schema definitions
#              Calls subroutines in 
#                  'dbSubs/dbTableCheck.pl'
#
#****************************************************************************
# 
# $Log: dbMakeFiles.pl,v $
# Revision 1.5  2003/01/31 02:13:35  porter
# got rid of a couple of opsolete files and got rid of environment variable
# STDB_ADMIN
#
# Revision 1.4  2003/01/09 20:30:27  porter
# upgrade of db table structure scripts
#
# Revision 1.2  2000/05/03 19:00:10  porter
# fixed header file output option
#
# Revision 1.1  2000/04/28 14:08:04  porter
# management perl scripts for db-structure accessible from StDbLib
#
#
#####################################

use Getopt::Std;

#--> choose which scripts to run <--
use FindBin;
use lib "$FindBin::Bin";
require "dbSubs/dbTableCheck.pl";

getopts('n:d:i:s:o:c:gh');

$helpout=$opt_h;
$dbName=$opt_d;
$serverHost=$opt_s;

if($helpout or (!$serverHost or !$dbName)){ Usage();}

#--> get the rest of the options <--
$tableName=$opt_n;
$debug=$opt_g;
$idlout = $opt_o;
$headerout = $opt_c;
$schemaID=$opt_i;

if($idlout){   $OnlIdls=$ENV{"ONL_ROOT"};  }

if($tableName){
    print "Querying for Table: ",$tableName, " in Database: ", $dbName, " \n";
} else {
    print "will query all structures in database ",$dbName,"\n";
}


##########################################
# Some Global Variables
##########################################
$outfile='';
@elements=();
@elengths=();
@etypes=();
@erelationID=();
@emask=();
@edone=();
@eID=();
@eposition=();
@oelements=();
@oelengths=();
@oetypes=();
@orelationID=();
@omask=();
@oID=();
@oposition=();

print "******************************\n";
print "*\n* Running dbMakeFiles.pl \n*\n"; 

dbTableCheck(DEBUG=>$debug, TableName=>$tableName, dbName=>$dbName, MakeIDL=>$idlout, dbHostName=>$serverHost, OnlIDL=>$OnlIdls, MakeHeader=>$headerout);

print "******************************\n";


sub Usage() {

print "****  Usage  ****\ndbMakeFiles.pl [-n tableName] -s server -d database [-i schemaID] (-o idloutdir | -c headoutdir) [-g|-h]\n";
print "****  Purpose ****\nFinds schema for table=tableName in database and writes c-struct definition \n";
print "                 -n tableName is optional arg. if not, all will be queried\n";
print "                 -d database required for request\n";
    print "                 -s server has the form 'hostname:portnumber' or simply\n";
    print "                 'hostname' if portnumber=3306\n";
print "                 -i schemaID option chooses a schema different then last update\n";
print "                 -o idloutdir dumps an idl file as definition \n";
print "                 -c headerout dir dumps a header file as definition \n";
print "                 -g for debug output, -h for this message \n\n";

print "****  Requires  **** Read-Access to database\n";

exit;
}
