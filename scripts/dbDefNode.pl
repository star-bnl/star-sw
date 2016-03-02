#!/usr/bin/perl 
#
# $Id: dbDefNode.pl,v 1.5 2003/01/31 02:13:35 porter Exp $
#
# Author: R. Jeff Porter
#***************************************************************************
#
# Description: script to parse an XML version of a Node list 
#              & load all new Nodes into the database
#              Calls subroutine in 
#                  'dbSubs/parseXmlNode.pl'
#
#****************************************************************************
# 
# $Log: dbDefNode.pl,v $
# Revision 1.5  2003/01/31 02:13:35  porter
# got rid of a couple of opsolete files and got rid of environment variable
# STDB_ADMIN
#
# Revision 1.4  2003/01/09 20:30:26  porter
# upgrade of db table structure scripts
#
# Revision 1.2  2001/02/16 22:11:22  porter
# modified for new low-level table structures
#
# Revision 1.1  2000/04/28 14:08:03  porter
# management perl scripts for db-structure accessible from StDbLib
#
#
#####################################

use Getopt::Std;
use DBI;


use FindBin;
use lib "$FindBin::Bin";
require "dbSubs/parseXmlNode.pl";

getopts('f:d:s:hg');

$inputFile=$opt_f;
$inputDbName=$opt_d;
$debug=$opt_g;
$helpout=$opt_h;
$serverHost=$opt_s;

if($helpout or (!$inputFile or !$serverHost)) { Usage(); }

print $inputFile, " ", $inputDbName, " \n";

##########################################
# Some Global Variables
##########################################
$outfile='';
$dbName='';
$tableName='';
@nodeName;
@nodeVersion;
@nodeType;
@nodeStruct;
@nodeElementIDs;
@nodeBaseline;
@nodeIndexed;
@nodeBinary;
@nElements;
@nodeComment;
@nodeIndexName;
@nodeIndexVal;
$nNodeIDs=0;

#########################################
#
# Parse the XML db-Definition on a list of Nodes
# and load information into memory
#
#########################################

parse_Nodes(fileName=>$inputFile,DEBUG=>$debug);

  print "DataBase = ",$dbName,"\n";

if($inputDbName){
print "Overridding DB name \n";
$dbName=$inputDbName;}  # overide that in XML file for testing

if(!$dbName){ die "No database specified in Xml file " ; }

#
#-> connect to DB
#

$dbh = DBI->connect("DBI:mysql:$dbName:$serverHost",$dbuser,$dbpass)
    || die "Cannot connect to server $DBI::errstr\n";

###############################################
#
#  Insert Nodes into the Nodes table
#
#  Will not insert node under 2 conditions
#    1. if nodeName & nodeVersion are already present
#    2. if node is a table and the c-struct is not defined
#       in this database
#
################################################

for($i=0;$i<=$#nodeName;$i++){

 $query = "Select * from Nodes where name='".$nodeName[$i]."' AND versionKey='".$nodeVersion[$i]."'";
 $sth=$dbh->prepare($query);
 if(!$sth->execute){ die "SQL syntax is wrong " ; };


 if(((@row)=$sth->fetchrow_array)) {
   print "Node=",$nodeName[$i]," of versionKey=",$nodeVersion[$i];
   print "Already Exists \n\n";
   $numfields = $sth->{NUM_OF_FIELDS};
   $names=$sth->{NAME};
   print "************* listing ***************\n\n";
   for($k=0;$k<$numfields;$k++){
     print $names->[$k]," = ",$row[$k],"\n";
   }

 } else { # create this node

   $skip=0;
   if($nodeType[$i] eq 'table') {
         $query="Select * from structure where name='".$nodeStruct[$i]."'";
         $sth=$dbh->prepare($query);
         $sth->execute;
     if(!((@row)=$sth->fetchrow_array)) {
       $skip=1;
       print "Cannot create table-node=".$nodeName[$i]." for struct=".$nodeStruct[$i]." as this structure doesn't exist in database=".$dbName;
     } else {
       $tableInfo=", baseline='".$nodeBaseline[$i]."', IsBinary='".$nodeBinary[$i]."', IsIndexed='".$nodeIndexed[$i]."'";
     }

   } else { # not a table...
     $tableInfo=' ';
   }

   if(!$skip){ 

$query="Insert into Nodes set name='".$nodeName[$i]."', versionKey='".$nodeVersion[$i]."', NodeType='".$nodeType[$i]."', structName='".$nodeStruct[$i]."'".$tableInfo.", elementID='".$nodeElementIDs[$i]."', Comment='".$nodeComment[$i]."'".", indexName='".$nodeIndexName[$i]."', indexVal='".$nodeIndexVal[$i]."'";

  if($debug){ print $query,"\n";}

$sth=$dbh->prepare($query);
$sth->execute;

  } # skip check

 } # node exists check

} # for loop




############################################################################
#
#  usage subroutine
#
############################################################################

sub Usage() {

    print "\n";
print "****  Usage   :: dbDefNode.pl -f xmlfile -s server [-d database] [-g|-h]\n";
print "****  Purpose :: Loads Keyed navigation stored in an XML file\n";
    print "                 -f xml-file\n";
    print "                 -s server has the form 'hostname:portnumber' or simply\n";
    print "                 'hostname' if portnumber=3306\n";
print "                 -d database option overides database in the xml-file\n";
print "                 -g for debug output\n";
print "                 -h for this message \n\n";
print "****  Requires  **** Write-Access to database\n";

exit;

}
