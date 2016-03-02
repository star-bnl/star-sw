#!/usr/bin/perl
#
# $Id: defineTableSQL.pl,v 1.2 2005/11/10 21:10:39 deph Exp $
#
# Author: R. Jeff Porter
#
#***************************************************************************
#
# Description: creates common SQL for new data tables
#
#****************************************************************************
# 
# $Log: defineTableSQL.pl,v $
# Revision 1.2  2005/11/10 21:10:39  deph
# Altered some default values to align with ver 4.1.x
#
# Revision 1.1  2003/01/10 22:56:55  porter
# new scripts for db auto table generation
#
#######################################

sub defineTableSQL {
    
   my ($storeT, $switch, $NODEID) = @_;
   my $retSQL;

   if($switch==0){ # beginning stuff 

      $retSQL= qq{ CREATE table $storeT } .
               qq{ \(dataID int(11) NOT NULL auto_increment, } .
               qq{ entryTime timestamp(14),} .
               qq{ nodeID int(11) NOT NULL,} .
               qq{ elementID smallint(6) DEFAULT '0' NOT NULL,} .
               qq{ beginTime datetime DEFAULT '1970-01-01 00:00:00' NOT NULL,}.
               qq{ endTime datetime DEFAULT '2037-01-01 00:00:00' NOT NULL,} .
               qq{ flavor char(32) DEFAULT 'ofl' NOT NULL,} .
               qq{ schemaID int(11) DEFAULT '1' NOT NULL, } .
               qq{ deactive int(10) unsigned DEFAULT '0' NOT NULL };

      if($NODEID !=0){
          $retSQL=~s/nodeID int\(11\)/nodeID int\(11\) default '$NODEID'/;
      }

   } else {   #ending stuff
       $retSQL= qq{  Key (dataID), KEY entryTime (entryTime), } .
	            qq{  PRIMARY KEY } .
                qq{ (nodeID,beginTime,flavor,elementID,deactive) )} ;
  }

  return $retSQL;

}

1;


