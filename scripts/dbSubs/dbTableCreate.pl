#!/usr/bin/perl -w
#
# $Id: dbTableCreate.pl,v 1.6 2007/05/04 03:55:08 deph Exp $
#
# Author: R. Jeff Porter
#
#***************************************************************************
#
# Description: checks table-structure in database to select whether
#              to create a new table or evolve current table
#
#****************************************************************************
# 
# $Log: dbTableCreate.pl,v $
# Revision 1.6  2007/05/04 03:55:08  deph
# New table with bit masks for pmdHotCells
#
# Revision 1.5  2007/03/28 04:18:55  deph
# Added quotes to identifier schema
#
# Revision 1.4  2003/01/31 02:14:42  porter
# got rid of a couple of opsolete files and got rid of environment variable
# STDB_ADMIN
# And fixed bug when adding new schema which is a combination of several old
# sets of schema. It now always updates schema if different then last one
#
# Revision 1.3  2003/01/09 20:30:41  porter
# upgrade of db table structure scripts
#
# Revision 1.1  2000/04/28 14:08:21  porter
# management perl scripts for db-structure accessible from StDbLib
#
#
#####################################

use DBI;

require "dbSubs/dbStoreTables.pl";

sub dbTableCreate(){

  my %args = (
              TableName => '',
              UserName => '',
              dbName => '',
              PassWord => '',
              DEBUG => '',
              dbHostName => '',
              NameRef=>'',
              StoreTable=>'',
              @_
              );

  if($args{DEBUG}){
  print $args{TableName}, "\n";
  print $args{UserName}, "\n";
  print $args{dbName}, "\n";
  print $args{dbHostName}, "\n";
  print $args{NameRef}, "\n";
}

  $structTableName="structure";
  $schemaTableName="schema";
  $relationTableName="relation";
  $namedRef = $args{NameRef};
  $storeTable=$args{StoreTable};

  $dbname = $args{dbName};
  $dbhostname = $args{dbHostName};
  $tableName =  $args{TableName};
  $indexTableName = join("",$args{TableName},"Index");
  $dbuser = $args{UserName};
  $dbpass = $args{PassWord};

  $structID=0;
  @storeType=();

########################################################
#
# 5 tables need to accessed, all in the same DataBase
#   Steps:
#    1. connect to Db
#    2. request Schema that exist for "tableName" from
#       Structures,Schema, & Relations Tables.
#    3.a. If 2=null (tableName is new) then
#        create Table in S,S, & R. 
#        create indexTable (version & timestamp access:
#        create dataTable (id + data)
#
#    3.b. If 2=exists check schema for compatibility:
#          if(identical) check if index & data Tables exists
#          and create if need be.
#          if(!identical) update S,S,&R
#                         update dataTable
#
########################################################

#
#-> connect to DB
#

$dbh = DBI->connect("DBI:mysql:$dbname:$dbhostname",'deph',$dbpass)
    || die "Cannot connect to server $DBI::errstr\n";

#
#-> prepare Query for Schema
#
#
# Check whether this structure "name" exists
#

  my $lschemaID=0;
  my $ssrQuery=qq { SELECT lastSchemaID,ID FROM structure } .
               qq { WHERE name='$tableName' };
  if($debug){ print $ssrQuery, "\n";}

  $sth=$dbh->prepare($ssrQuery);
  $sth->execute;
  if( !(($lschemaID,$structID)=$sth->fetchrow) ){
      $lschemaID=0;
      $structID=addStructure($tableName,$tableComment);
  }
  $lschemaID++;
  for($i=0;$i<=$#elements;$i++){
        if((!($etypes[$i]=~m/char/) || $etypes[$i]=~/uchar/)
             && ($elengths[$i]>60 || $elengths[$i]=~m/\,/) ){ 
# non-string with 60 elements or multi-Dimen.
            print "bin\n";
             $storeType[$i]="bin";
        } else {
            print "ascii\n";
            $storeType[$i]="ascii";
        }
    }
  addSchema($tableName,$lschemaID); # will die in here if schema already exists

  print "StoreTable is ",$storeTable,"\n";
  @tmpTable=split(",",$storeTable);
  for($k=0;$k<=$#tmpTable;$k++){
    my $mstoreTable=$tmpTable[$k];
  print "StoreTable is ",$mstoreTable,"\n";
      if(checkForTable($mstoreTable)){
          print "would update table \n";
         updateStoreTable($mstoreTable);
      } else {
          print "would create table \n";
         createStoreTable($mstoreTable);
      }
  }


}

##########################################
#
#
#
##########################################

sub addStructure {

    my ($tn,$tc) = @_;

    my $sin=qq { insert into structure set } .
            qq { name='$tn', Comment='$tc', lastSchemaID=1 };
    my $addsth=$dbh->prepare($sin);
    $addsth->execute;
    my $retVal;
    $retVal=$addsth->{mysql_insertid};
    print $retVal,"\n";
    return $retVal;
};    

#####################################################
#
# routine to check schema with that in the database
#
# --> now only check for the last schema - if it is not
# the same, create a new schema
#
#####################################################

sub addSchema {

    my ($tn,$sid) = @_;

    my $stest = qq{ select ID from `schema` where } .
                qq{ structName='$tn' and schemaID='$sid' };

    $stesth = $dbh->prepare($stest);

    $stesth->execute;
    my $testID;

    if(($testID)=$stesth->fetchrow){ 
        $stesth->finish;
        die "SchemaID ($sid) for struct=$tn already exists \n";
    }
    $stesth->finish;

    my $addIt=0;
    if($sid==1){$addIt=1;} 
    if($sid!=1){ #check for old schema's

#-> get the list of distinct schemas for this structure
       my $qlist=qq{ select max(schemaID) from `schema` } . 
                 qq{ where structName='$tn'};
       my $scitr=$dbh->prepare($qlist);

#-> get the element-list per distinct schema
       my $scQuery=qq{ select name,type,length,position,storeType from `schema` } .
                   qq{ where structName='$tn' and schemaID=? };
       my $scth= $dbh->prepare($scQuery);           

       my @namecheck;
       my @modifiedElements;
       my @newelements;
       $#namecheck=$#elements;
       $#newelements=$#modifiedElements=$#namecheck=$#elements;
#->some prep here...
       for($i=0;$i<=$#elements;$i++){ 
           $namecheck[$i]=0; 
           $modifiedElements[$i]=0;
           $newelements[$i]=0;

       };

       my $thisID;
       $scitr->execute;
       while( ($thisID)=$scitr->fetchrow){
         my ($sname, $stype, $slength,$sposition,$store);
         $scth->execute($thisID);
         while( ($sname,$stype,$slength,$sposition,$sstore)=$scth->fetchrow_array){
             for($i=0;$i<=$#elements;$i++){
                 if($sname eq $elements[$i]){ 
                     $namecheck[$i]=1;
                     if($stype ne $etypes[$i]) { die "Cannot modify primary type for same element name for $sname\n";}
                     if(($slength eq 1) && ($elengths[$i] > 1)){ die " Cannot evolve from a single element to an array for name=$sname with length=$slength\n";}; 
                     if($sstore ne $storeType[$i]) { 
                         print "Warning:: storetype is changed to $sstore\n";
                         $storeType[$i]=$sstore;
                     }
                     $matches++;
                 }

             }
         }
     }


    for($i=0;$i<=$#elements;$i++){
         if($namecheck[$i] eq 0){
             $newelements[$i]=1;
             $addIt=1;
         }
     }
   }

    if($addIt eq 0){ print "All element found for this c-struct \n"; return;}


    my $ii=0;
    for($i=0;$i<=$#elements;$i++){
        $ii=$i+1;
        my $addS = qq{ insert into `schema` } .
                   qq{ set name='$elements[$i]', type='$etypes[$i]', } .
                   qq{ length='$elengths[$i]', schemaID=$sid, } .
                   qq{ Comment='$ecomments[$i]',storeType='$storeType[$i]', } .
                   qq{ structName='$tableName', structID=$structID, } .
                   qq{ position=$ii, units='$eunits[$i]' };
        print $addS,"\n";
        $dbh->do($addS);
    }

    if(!($ii eq 0)){
        my $updateSID=qq{ update structure set lastSchemaID=$sid }.
            qq{ where ID=$structID };
        print $updateSID,"\m";
        $dbh->do($updateSID);
    }

    return 1;
}

   

1;







