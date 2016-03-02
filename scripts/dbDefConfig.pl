#!/usr/bin/perl 
#
# $Id: dbDefConfig.pl,v 1.5 2003/01/31 02:13:35 porter Exp $
#
# Author: R. Jeff Porter
#
#***************************************************************************
#
# Description: script to parse an XML version file containing a tree
#              relation of nodes (Node= name+versionKey)
#              & loads relations into the database
#              Calls subroutines in 
#                  'dbSubs/parseXmlConfig.pl'
#
#****************************************************************************
# 
# $Log: dbDefConfig.pl,v $
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

#-------- check which scripts to use -------#

use FindBin;
use lib "$FindBin::Bin";
require "dbSubs/parseXmlConfig.pl";

#--------------------------------------------------------
getopts('f:d:s:hg');

$inputFile=$opt_f;
$inputDbName=$opt_d;
$debug=$opt_g;
$helpout=$opt_h;
$serverHost=$opt_s;

if($helpout or(!$inputFile or !$serverHost)) { Usage(); }

#---------------------------------------------------------

print $inputFile, " ", $inputDbName, " \n";

##########################################
# Some 'Global' Variables
##########################################

$dbName='';
@nodeName=();
@nodeVersion=();
@nodeID=();
@nodeParent=();
$configID=0;
$thisNodeType;
@actionWord=();
@nodeComment=();

#########################################
#
# Parse the XML db-Definition to load into
# memory
#
#########################################

#--- parse the xml file 
parse_Config(fileName=>$inputFile,DEBUG=>$debug);

#--------- check database name --------------------------
if($inputDbName){$dbName=$inputDbName;}  # overide that in XML file for testing
if(!$dbName){ die "No database specified in Xml file " ; }

print "DataBase = ",$dbName,"\n";

#
#--------> connect to DB -------------------------------
#
$dbh = DBI->connect("DBI:mysql:$dbName:$serverHost",$dbuser,$dbpass)
    || die "Cannot connect to server $DBI::errstr\n";

#################################
#
#  Insert Relations into the NodeRelation table
#
##################################

if($debug){  
       print "num nodes = ",$#nodeName,"\n";
       for($k=0;$k<=$#nodeName;$k++){ 
           print $k," ",$nodeName[$k]," ",$nodeVersion[$k]," ";
           print $nodeParent[$k],"\n"; 
       }
   }

#---------- get nodeID for each node in input configuration ------

 $query=qq{Select ID, nodeType from Nodes where name=? and versionKey=?};
 $sth=$dbh->prepare($query);

for($k=0;$k<=$#nodeName;$k++){

   $sth->execute($nodeName[$k],$nodeVersion[$k]);

#---------- If retrieving of nodeID's fail, quit! -------------

  if( !(($nodeID[$k], $thisNodeType )=$sth->fetchrow_array)) {
  die "Error:: Node => ".$nodeName[$k]." ".$nodeVersion[$k]." Not found in DB";
  }

  if($thisNodeType eq 'Config'){
     if($configID) { die "Error:: multiple configurations in file "; }
     $configID=$nodeID[$k];
  }

}

$sth->finish;

###################################
#
#  Now insert relations
#
#------------------------------------------------------------------------
#
# prepare operations, 'query', 'insert', 'delete', & 'log'
#
#------------------------------------------------------------------------

$query=qq{Select ID from NodeRelation } .
       qq{ where ParentID=? AND NodeID=? AND BranchID=? AND ConfigID=?};
$sthQ=$dbh->prepare($query);

$insert=qq{Insert into NodeRelation } .
        qq{ set ParentID=?, NodeID=?, BranchID=?, ConfigID=?};
$sthI=$dbh->prepare($insert);

$delete=qq{Delete from NodeRelation } .
        qq{ where ID=? };
$sthD=$dbh->prepare($delete);

$log=qq{ Insert into ConfigLog } .
     qq{ set ParentID=?, NodeID=?, ConfigID=?, Action=?, Comment=?};
$sthL=$dbh->prepare($log);  

#-------------------------------------------------------------------------

$rowID=0;
my @branchID;
$#branchID=$#nodeName;
for($k=0;$k<=$#branchID;$k++){$branchID[$k]=0;};
for($k=1;$k<=$#nodeName;$k++){

    if($debug){
        print $k," & ", $nodeName[$k]," & ";
        print $nodeParent[$k], " & ",$actionWord[$nodeParent[$k]], "\n";
    }

#-------- propogate deletes & reasons to all children  ---------
  if($actionWord[$nodeParent[$k]] eq 'delete'){
     $actionWord[$k]="delete";
     if(!$nodeComment[$k]) { $nodeComment[$k]=$nodeComment[$nodeParent[$k]];}
  }

#-------- check this node-Relation ---------
  $sthQ->execute($nodeID[$nodeParent[$k]],$nodeID[$k],$branchID[$nodeParent[$k]],$configID);
  $rowID=$sthQ->fetchrow_array;

#-------- See what to do & do it -----------
  if($actionWord[$k]=~m/add/){ # ------ add nodeRelation --------

        $sthI->execute($nodeID[$nodeParent[$k]],$nodeID[$k],$branchID[$nodeParent[$k]],$configID);
        $sthQ->execute($nodeID[$nodeParent[$k]],$nodeID[$k],$branchID[$nodeParent[$k]],$configID);
        $branchID[$k]=$sthQ->fetchrow_array;
        
  } else { # ---------- delete nodeRelation -------------------

    if( $rowID ){
       $rv=$sthD->execute($rowID);
    } else {
        print "Node=",$nodeName[$k]," of Parent=",$nodeName[$nodeParent[$k]];
        print " Is not part of Configuration, configID=",$configID,"\n";
    }

}


} #---- end of for loop over nodes ------#
        
$sthQ->finish;
$sthI->finish;
$sthD->finish;
$sthL->finish;

$dbh->disconnect;

#####################################################################
#
#####################################################################

sub Usage() {

    print "\n";
print "****  Usage   :: dbDefConfig.pl -f xmlfile -s server [-d database] [-g|-h]\n";
print "****  Purpose :: Node Relations navigation stored in an XML file\n";
    print "                 -f xml-file\n";
    print "                 -s server has the form 'hostname:portnumber' or simply\n";
    print "                 'hostname' if portnumber=3306\n";
print "                 -d database option overides database in the xml-file\n";
print "                 -g for debug output\n";
print "                 -h for this message \n\n";

print "****  Requires  **** Write-Access to database\n";

exit;
}
