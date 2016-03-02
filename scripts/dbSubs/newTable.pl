#!/opt/star/bin/perl -w
#
# $Id: newTable.pl,v 1.2 2001/02/16 22:11:38 porter Exp $
#
# Author: R. Jeff Porter
#
#***************************************************************************
#
# Description: creates a new table structure in the database
#
#****************************************************************************
# 
# $Log: newTable.pl,v $
# Revision 1.2  2001/02/16 22:11:38  porter
# modified for new low-level table structures
#
# Revision 1.1  2000/04/28 14:08:22  porter
# management perl scripts for db-structure accessible from StDbLib
#
#
## 
#####################################

use DBI;

sub newTable(){

        $statement="INSERT structure SET name='".$tableName."', lastSchemaID=1, Comment='".$tableComment."'";
        $sth=$dbh->prepare($statement);
        $sth->execute;
        $statement="SELECT structure.ID FROM structure WHERE ID IS NULL";
        $sth=$dbh->prepare($statement);
        $sth->execute;
        if(!((@row)=$sth->fetchrow_array)){ die "Couldn't find stucture.ID";}
        $structID=$row[0];

#
# put in alias if called for
#
#        if($namedRef){
#            $statement="INSERT namedRef set name='".$namedRef."', structID=".$structID;
#        } else {
#            $statement="INSERT namedRef set name='".$tableName."', structID=".$structID;
#        }
#            $sth=$dbh->prepare($statement);
#            $sth->execute;
  

for($i=0;$i<=$#elements;$i++){

    if(!($etypes[$i]=~m/char/) && ($elengths[$i]>60 || $elengths[$i]=~m/\,/) ){ # non-string with 60 elements or multi-Dimen.
        $storeType="bin";
    } else {
        $storeType="ascii";
    }
        $ii=$i+1;
        $statement="INSERT schema SET name='".$elements[$i]."', type='".$etypes[$i]."', length='".$elengths[$i]."', schemaID=1, Comment='".$ecomments[$i]."', storeType='".$storeType."', structName='".$tableName."', structID=".$structID.", position=".$ii.", units='".$eunits."'";
    if($debug){print $statement, " \n";}
        $sth=$dbh->prepare($statement);
        $sth->execute;
    }

#####################################################
#
#  Create the new table for data
#  
#  Common elements of all data tables 
#  are dataID & entryTime
#
#####################################################

#    $dbData=qq{ \(dataID int not null auto_increment, 
#                  entryTime timestamp";

my $dbData= qq{ (dataID int(11) DEFAULT '0' NOT NULL auto_increment,} .
 qq{ entryTime timestamp(14),} .
 qq{ nodeID int(11) NOT NULL,} .
 qq{ elementID smallint(6) DEFAULT '0' NOT NULL,} .
 qq{ beginTime datetime DEFAULT '1969-12-31 19:00:00' NOT NULL,} .
 qq{ flavor char(8) DEFAULT 'ofl' NOT NULL,} .
 qq{ numRows smallint(6) DEFAULT '1' NOT NULL,} .
 qq{ schemaID int(11) DEFAULT '1' NOT NULL, } .
 qq{ deactive int(10) unsigned DEFAULT '0' NOT NULL };

##########################################
#
# convert to mysql-types as needed:
#
#  e.g. text or blob ..else.. basic types
#
##########################################


        for($i=0;$i<=$#elements;$i++){

            $dbData=join(", ",$dbData,$elements[$i]);
            $btest=$elengths[$i];
            $ttest=$mysqltypes[$i];
#            if($ttest=~m/char/ && $btest==1){
#               print $ttest," ",$btest,"\n";
#                $ttest="text";                
#               print $ttest," ",$btest,"\n";
#            }

            if($btest=~m/\,/){  # multi-dimensions
                $ttest="longblob";
            } elsif(!($ttest=~m/char/) && ($btest>1) && ($btest<=60)){
                $ttest="mediumtext";
            } elsif($btest==1){
                $ttest=~s/float/float \(16\,8\)/;
                $ttest=~s/double/double \(20\,10\)/;
            } elsif($ttest=~m/char/){
                if($btest<=60){
                    $ttest="char(".$btest.")";
                } elsif($btest<=255){
                    $ttest="varchar(".$btest.")";
                } else {
                    $ttest="mediumtext";
                }
            } elsif($btest>60){
                $ttest="longblob";
            }

           $dbData=join(" ",$dbData,$ttest);
        }

#        $dbData=join(", ",$dbData," Key (dataID), )");

 my    $indexString=qq{ Key (dataID), KEY entryTime (entryTime), } .
	 qq{ PRIMARY KEY (nodeID,beginTime,flavor,elementID,deactive) )};

	$dbData=join(", ",$dbData,$indexString);
##################
#
# print out query
#
##################

        print "Table Definition: ",$dbData,"\n";

$statement=qq(CREATE table $tableName $dbData);

#print "Test:: ", $statement, "\n";

##################
#
# do CREATE query
#
##################

$dbh->do($statement)
    || die "Cannot Create Table $tableName $DBI::errstr\n";

}

1;











