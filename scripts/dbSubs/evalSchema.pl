#!/opt/star/bin/perl -w
#
# $Id: evalSchema.pl,v 1.3 2007/03/28 04:18:56 deph Exp $
#
# Author: R. Jeff Porter
#
#***************************************************************************
#
# Description: compares against input schema & returns schemaID
#              OR will dump out schema in either idl or *.h format
#              based on name(+schemaID) request.
#
#****************************************************************************
# 
# $Log: evalSchema.pl,v $
# Revision 1.3  2007/03/28 04:18:56  deph
# Added quotes to identifier schema
#
# Revision 1.2  2000/05/03 20:06:26  porter
# header output fix ... continued
#
# Revision 1.1  2000/04/28 14:08:22  porter
# management perl scripts for db-structure accessible from StDbLib
#
#
#########################################################################

use DBI;

sub evalSchema(){
  my %args = (
              CompareHeader => '',
              MakeHeader => '',
              MakeIDL => '',
              OnlIDL => '',
              DEBUG => '',
              SchemaID=>'',
              @_,
              );

  $cmpH        =$args{CompareHeader};
  $mkIdl       =$args{MakeIDL};
  $mkH         =$args{MakeHeader};
  $debug       =$args{DEBUG};
  $thisSchemaID=$args{SchemaID}; 

  if($thisSchemaID){ print "SchemaID input =",$thisSchemaID,"\n"; }

# the hard stuff

  if(!$cmpH){ # need a schemaID to request. 
              # is it input?
    if(!$thisSchemaID){ # get the last schemaID in the database
      $ssrQuery= qq{ select structure.lastSchemaID } .
                 qq{from structure where structure.name='$tableName'};
      $sth=$dbh->prepare($ssrQuery);
      $sth->execute;
      if((@row)=$sth->fetchrow_array){
          if($debug){print $ssrQuery,"\n Yields schemaID = ",$row[0],"\n";}
         $thisSchemaID=$row[0];
        } else {
          die "evalSchema:: Cannot find structure named ",$tableName;
        }
      $sth->finish;
    } 

   } else { #header file input to find schemaID
     $thisSchemaID=-1;
   }

#---> the big query <----

$ssrQuery= qq{ SELECT structure.ID, structure.lastSchemaID, } .
           qq{ schema.name, schema.ID, schema.schemaID, schema.type, } .
           qq{ schema.length, schema.position, schema.Comment, } .
           qq{ structure.Comment as structComment, schema.storeType }.
           qq{ FROM structure } .
           qq{ LEFT JOIN `schema` ON structure.ID=schema.structID }.
           qq{ WHERE structure.name='$tableName' ORDER BY schema.position};

  if($debug){ print $ssrQuery,"\n"; }

#############################################
#
#  row indeces returned from above query
#
#############################################
#
# 0 = structID
# 1 = lastSchemaID
# 2 = member name
# 3 = member id 
# 4 = member schemaID
# 5 = member type
# 6 = member length
# 7 = member position
# 8 = member comment
# 9 = struct comment
#10 = member storeType ("ascii" or "bin")
#
##############################################

$sth=$dbh->prepare($ssrQuery);
$sth->execute;

######################################
#
# loop over results 
#
######################################
   
$done=0; # check if input header's schema is found in DB
$n=0;    # counter of elements in DB

while(((@row)=$sth->fetchrow_array) && !$done){

    $structComment = $row[9];
    if(!$cmpH && !($thisSchemaID==$row[4])){ next; }
  if($cmpH){
   if($thisSchemaID!=$row[4]){      
       if( ($#elements eq $numFound-1) && ($#elements eq $numQueried-1)){
        $done=1;
       } else {
        $thisSchemaID=$row[4];
        $numQueried=$numFound=0;
    };
   };
   
   if( ($#elements eq $numFound-1) && ($#elements eq $numQueried-1)){ $done=1;}

   if(!$done){ # if $done then we've a 1-to-1 match for this header in the DB
 
    $numQueried++;
    $structID=$row[0];
    $found=0;
    $i=0;
    while(!$found && ($i<=$#elements)){

      if($elements[$i] eq $row[2] ){
         if($etypes[$i] != $row[5]) {
             die "Datatype mod not allowed: (".$row[5]." vs ".$etypes[$i].") for element name =".$row[2];
         }
         if( ($elengths[$i] > 1 )&& ($row[6] eq 1)){
           die "1-element to Array mod not allowed for element name =".$row[2];
        }
         if($elengths[$i] eq $row[6]){
            $found=1; # same element is 'name', & 'length'
            $numFound++;      
         }
         $oldstoreTypes[$i]=$row[10];
     }

     $i++;
   } #loop over incoming c-struct elements
  } # $done check

 } else {# producing header or IDL file for given or last schemaID

      $#oelements=$#oposition=$#oschemaID=$#orelationID=$#oID=$n;
      $oelements[$n]=$row[2];
      $oetypes[$n]=$row[5];
      $oelengths[$n]=$row[6];
      $oposition[$n]=$row[7];
      $oecomments[$n]=$row[8];
      $oschemaID[$n]=$row[4];
      $oID[$n]=$row[3];
      if($oschemaID[$n] ne $thisSchemaID){
        die "evalSchema:: schema mis-match in query ";
      }
      $n++;
 }

} #loop over query results

  $sth->finish;

if($cmpH){ # we're through regardless of the results
  if($done){ 
      die "Found Table SchemaID=".$thisSchemaID." for table=".$tableName."\n";
  } else {
      die "Could not find Schema for table= ".$tableName."\n";
  }
}

#############################################################
#
#  now Print out structure in either headers, idls, or both
#
#############################################################

if($mkIdl){
  $outfile=$mkIdl."/".$tableName.".idl";
#  $extend="idl";
  if($args{OnlIDL}){$extend="idlOnl";} else {$extend="idl";}
  writeThisFile(TableName=>$tablename,OutFileName=>$outfile,FileType=>$extend,TableComment=>$structComment);
}

if($mkH) {
  $outfile=$mkH."/".$tableName.".h";
  $extend="h";
  writeThisFile(TableName=>$tablename,OutFileName=>$outfile,FileType=>$extend,TableComment=>$structComment);
}

### that's it  

}

####################### local subroutine ######################

sub writeThisFile(){

  my %args = (
              TableName => '',
              TableComment =>'',
              OutFileName => '',
              FileType => '',
              DEBUG => '',
              @_,
              );

$outfilename=$args{OutFileName};
$filetype=$args{FileType};
$tablename=$args{TableName};
$tablecomment=$args{TableComment};

if(!$outfilename || !$filetype){ die "No file Type Specified";}

  $isHeader=0;
  $isOnlIdl=0;
  if($filetype=~m/h/){ $ending = "H\n"; $isHeader=1; 
     } else{ 
         $ending= "IDL\n"; 
         if($filetype=~m/Onl/){
             $isOnlIdl=1;
             $filetype=~s/Onl//;
         }
     }

#  $testOnl=$ENV{"ONL_ROOT"};
#  if($testOnl) {      $isHeader=1;  }

  open(OUTFILE,"> $outfilename");
  print OUTFILE "/*    ".$tableName.".".$filetype."\n";
  print OUTFILE "*\n*  Table: ".$tableName."\n";
  print OUTFILE "*\n";
  print OUTFILE "*       description:".$tablecomment."\n";
  print OUTFILE "*/ \n\n";

  if($isHeader || $isOnlIdl ){
    print OUTFILE "#ifndef __".$tableName."__".$ending;
    print OUTFILE "#define __".$tableName."__".$ending;
  }

  print OUTFILE "\nstruct ",$tableName," { \n\n";

for($i=0; $i<=$#oelements;$i++){
  if(!$isHeader)                 { $oetypes[$i]=~s/int/long/;             }
  if(!($oetypes[$i]=~m/double/)) { $oetypes[$i]=~s/u/unsigned /;          }
  if(!$isHeader)                 { $oetypes[$i]=~s/unsigned char/octet/;  }

  if($oelengths[$i]>1) {
   $oelengths[$i]=~s/\,/\]\[/;
   print OUTFILE "   ",$oetypes[$i],"  ",$oelements[$i],"[",$oelengths[$i],"];"
  } else {
   print OUTFILE "   ",$oetypes[$i],"  ",$oelements[$i],";"
  }

  print OUTFILE "   /* ".$oecomments[$i]."  */\n";
}

 print OUTFILE "\n };  \n";
 if($isHeader || $isOnlIdl )     { print OUTFILE "#endif \n";   }

  close(OUTFILE);
}


1;









