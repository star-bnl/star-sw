#!/opt/star/bin/perl -w
#
# $Id: evolveTable.pl,v 1.3 2007/03/28 04:18:56 deph Exp $
#
# Author: R. Jeff Porter
#
#***************************************************************************
#
# Description: evolves a table based on input schema definition
#
#****************************************************************************
# 
# $Log: evolveTable.pl,v $
# Revision 1.3  2007/03/28 04:18:56  deph
# Added quotes to identifier schema
#
# Revision 1.2  2001/02/16 22:11:38  porter
# modified for new low-level table structures
#
# Revision 1.1  2000/04/28 14:08:22  porter
# management perl scripts for db-structure accessible from StDbLib
#
#
#####################################

use DBI;

sub evolveTable(){

$ssrQuery="SELECT structure.ID, structure.lastSchemaID, schema.name, schema.ID, schema.schemaID, schema.type, schema.length, schema.position, schema.Comment, structure.Comment as structComment, schema.storeType  FROM structure LEFT JOIN `schema` ON structure.ID=schema.structID WHERE structure.name='".$tableName."' ORDER BY schema.position";

if($debug){ print $ssrQuery,"\n";}

#############################################
#
#  row indeces returned from above query
#
#############################################
#
#  0 = structID
#  1 = lastSchemaID
#  2 = member name
#  3 = member id 
#  4 = member schemaID
#  5 = member type
#  6 = member length
#  7 = member position
#  8 = member comment
#  9 = struct comment
# 10 = member storeType ("ascii" or "bin")
#
##############################################

$sth=$dbh->prepare($ssrQuery);
$sth->execute;

######################################
#
# loop over results 
#
######################################

#
# --> initialize some values
#
   $maxSchemaID=0;
   $thisSchemaID=-1;
   $done=0;
    @oldstoreTypes=();
    $#oldstoreTypes=$#elements;
    for($i=0;$i<=$#oldstoreTypes;$i++){
        $oldstoreTypes[$i]="none";
    }

while(((@row)=$sth->fetchrow_array) && !$done ){
#    print " In The Loop: done = ",$done,"\n";

  if($thisSchemaID!=$row[4]){      
      if( ($#elements eq $numFound-1) && ($#elements eq $numQueried-1)){
       $done=1;
      } else {
     if($debug){
       print " Row1= ",$row[1]," row2= ",$row[2]," row4= ",$row[4],"\n";
   }
       $thisSchemaID=$row[4];
       if($thisSchemaID > $maxSchemaID){ $maxSchemaID=$thisSchemaID; }
       $numQueried=$numFound=0;
     if($debug){print "This SchemaID = ",$thisSchemaID,"\n";}
   };
  };

  
  if(!$done){ # if $done then we've a 1-to-1 match for this header in the DB
 
    $numQueried++;

#   $newKeyID=$row[1]<<1;   # next Schema ID number
    $structID=$row[0];
    $found=0;

# for each element in the database:
# loop over all elements in c-struct until it is either
# found in the db or we exhaust all c-struct elements
#
  
   $i=0;
   while(!$found && ($i<=$#elements)){

      if($elements[$i] eq $row[2] ){
         if($etypes[$i] != $row[5]) {
             die "Cannot Modify types for same element name";
         }
         if( ($elengths[$i] > 1 )&& ($row[6] eq 1)){
             die "Cannot evolve from single value to an array";
         }
         if($elengths[$i] eq $row[6]){
             if($debug){ print "EvolveTable:: Found element=",$row[2]," ",$#elements," ",$numFound," ",$numQueried,"\n"; }
            $found=1; # same element is 'name', & 'length'
            $numFound++;      

         }
         $oldstoreTypes[$i]=$row[10];
     }

      $i++;
   } #loop over incoming c-struct elements

 } #continue loop check

} # loop over db elements

if( ($#elements eq $numFound-1) && ($#elements eq $numQueried-1)){$done=1;}

  if($done){ # then match was found in db rather than exhausting the db-list

      die "Schema exist with schemaID=",$thisSchemaID,"\n******************************\n"; }

##########################################
# step 1. update schemaID in structure
##########################################

   $thisSchemaID=$maxSchemaID+1;
   $statement="update structure set lastSchemaID='".$thisSchemaID."' where name='".$tableName."'";
   $sth=$dbh->prepare($statement);
   $sth->execute;

#########################################
# step 2. add new schema to schema table
#
#   note the storeType must stay the same
#  for element names already existing in the
#  database regardless of any changes to the 
#  length of an array ...
#  ... one cannot switch between binary
#  and ascii within a column
#
#########################################

 for($i=0;$i<=$#elements;$i++){

   if($oldstoreTypes[$i]=~m/none/){
    if(!($etypes[$i]=~m/char/) && ($elengths[$i]>60 || $elengths[$i]=~m/\,/) ){        $storeType[$i]="bin";
    } else {
        $storeType[$i]="ascii";
    }
   } else {
     $storeType[$i]=$oldstoreTypes[$i];
   }
       
    $ii=$i+1;
        $statement="INSERT `schema` SET name='".$elements[$i]."', type='".$etypes[$i]."', length='".$elengths[$i]."', schemaID=".$thisSchemaID.", Comment='".$ecomments[$i]."', storeType='".$storeType[$i]."', structName='".$tableName."', structID=".$structID.", position=".$ii.", units='".$eunits."'";
   if($debug){print $statement, " \n";}
        $sth=$dbh->prepare($statement);
        $sth->execute;

}
#########################################
# step 3. update the data table columns
#########################################

$statement="select * from ".$tableName." limit 1";
$sth=$dbh->prepare($statement);
$sth->execute;

   $numfields = $sth->{NUM_OF_FIELDS};
   $names=$sth->{NAME};
   $newElements=0;
   $update="ALTER table ".$tableName;

   for($i=0;$i<=$#elements;$i++){
     $done=0;
     $k=0;
      while( ($k<=$numfields) && !$done ){
        if($elements[$i] eq $names->[$k]){
          $done=1;
        } else {
          $k++;
        }
     }
     if(!$done){ # then add this column

        if($newElements){ #comma separate for more than 1
            $update=join("",$update,",");
        }
        $newElements=1;
        $update=join(" ",$update,"ADD");
        $update=join(" ",$update,$elements[$i]);

        if($elengths[$i] > 1){
          if($storeType[$i]=~m/ascii/){
              $tmpType="text";
              if( ($mysqltypes[$i]=~m/char/) && !($mysqltypes[$i]=~m/uchar/)){
                if($elengths[$i]<=60){
                    $tmpType="char(".$elengths[$i].")";
                } elsif($elengths[$i]<=255){
                    $tmpType="varchar(".$elengths[$i].")";
                } else {
                    $tmpType="mediumtext";
                }
              }
             $update=join(" ",$update,$tmpType);
          } else {
             $update=join(" ",$update,"longblob");
          }
        } else {
            $ttest=$mysqltypes[$i];
                $ttest=~s/float/float \(16\,8\)/;
                $ttest=~s/double/double \(20\,10\)/;
          $update=join(" ",$update,$ttest);
        }
    }
 }

   if($update=~m/ADD/){ #then new column (in case just lengths were mod

       $dbh->do($update);

   }

}
1;









