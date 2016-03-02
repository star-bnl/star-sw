#!/usr/bin/perl
#
###################################

require "dbSubs/defineTableSQL.pl";
require "dbSubs/prepSQLElement.pl";


sub checkForTable(){
    my ($table)=@_;

    my $sth=$dbh->prepare("show tables like '$table'");
    $sth->execute;

    my $testTable;
    my $retVal=0;
    if( (($testTable)=$sth->fetchrow)){ $retVal=1; }
    return $retVal;
}


sub updateStoreTable(){
    my ($table)=@_;
    my $sth=$dbh->prepare("explain $table");

    my $beginSQL=qq{ alter table $table };
    
    $sth->execute;
    my ($fld,$typ,$nl,$key,$def,$extra);
    
    my @fields=();
    $icount=0;
    while (($fld,$typ,$nl,$key,$def,$extra)=$sth->fetchrow_array){
        if($fld eq "dataID"){ next;}
        if($fld eq "nodeID"){ next;}
        if($fld eq "schemaID"){ next;}
        if($fld eq "beginTime"){ next;}
        if($fld eq "endTime"){ next;}
        if($fld eq "flavor"){ next;}
        if($fld eq "deactive"){ next;}
        if($fld eq "numRows"){ next;}
        $fields[$icount]=$fld;
        $icount++;
        print "found field = ",$fld,"\n";
    }

    my $elementSQL='';
        
    my $commaCheck=0;
    for($i=0;$i<=$#elements;$i++){
        my $check=0;
        for($j=0;$j<=$#fields;$j++){
            if($fields[$j] eq $elements[$i]){$check=1;}
        }
        if($check eq 1){ next;}

        my $tmp1= prepSQLElement($elements[$i],$etypes[$i],$storeType[$i],$elengths[$i],$check);
        if($commaCheck > 0){ $elementSQL=$elementSQL.", ";};
        $elementSQL=$elementSQL." ADD ".$tmp1;
        $commaCheck++;
    }

    my $finalSQL = qq{ $beginSQL $elementSQL};
    print $finalSQL,"\n";

    $dbh->do($finalSQL);

};

sub createStoreTable(){
    my ($table)=@_;

    my $beginSQL=defineTableSQL($table,0);
 

    my $elementList='';
    for($i=0;$i<=$#elements;$i++){
        my @edescription=($elements[$i],$etypes[$i],$storeType[$i],$elengths[$i],0);
      my $sqlElement = prepSQLElement(@edescription);
      $elementList = $elementList.",".$sqlElement; 
  };
        
   my $endSQL=defineTableSQL($table,1);

 
  my $createTable=qq { $beginSQL $elementList , $endSQL };
    print $createTable,"\n";
  $dbh->do($createTable);

};


1;

