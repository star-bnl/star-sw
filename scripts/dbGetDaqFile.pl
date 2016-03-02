#!/usr/bin/perl

#############################################
#        STAR Database Script::dbGetDaqFile.pl
#############################################
#             Author: Michael DePhillips
#############################################
#                Date: November 2, 2005
#############################################
#
#  Script to get file name
#  requires RunNumber and EventNumber as input or File contain two columns
#  first column runnumber second column eventnumber          
#  
#   
#############################################
#
#   connects to 1)onlsun1 - port is dynamic
#               
#############################################

use DBI;
use Getopt::Std;


#UNCOMMENT BELOW FOR DEBUGGING DBI
#DBI->trace(1);


getopts('f:r:e:h');

$inputFile=$opt_f;
$runnumber = $opt_r;
$eventNumber=$opt_e;
$helpout=$opt_h;


if($helpout or ((!$inputFile) && (!$runnumber or !$eventNumber)))
{useage();}

if ($inputFile)
{
    if(open(IN,"< $inputFile")) {
	while(my $line = <IN>) {
	    chomp $line;
	    my @data = split(" ", $line);
	    $h_info{$data[0]}->{$data[1]} = 1;
	}
    }else { print "file not found \n"; exit;}
    close IN;

##Create HASH with Run numbers and event numbers
foreach my $runnum (sort numeric keys %h_info) {
     $daqTable = getDaqMap($runnum);
    
     $myQuery = "SELECT fileStream, fileSequence, run, eventNumber FROM $tables[$i] where run = $runnum and eventNumber in (";
    foreach my $eventnum (sort numeric keys %{$h_info{$runnum}}) {
#	print "\t$eventnum\n";
	$myQuery .=" $eventnum ,";
    }

    $myQuery =~ s/,$/\)/;

$dbh2 = DBI->connect("DBI:mysql:RunLog_daq:onlsun1.starp.bnl.gov:$port",$dbUser,"")
   || die "cannot connenct to server $DBI::errstr\n";
 
  $sthGet = $dbh2->prepare($myQuery);
   $sthGet->execute;

    while ( ( $stream, $sequence, $run, $event ) = $sthGet -> fetchrow_array)
    {
    #  print "$myQuery\n";
        if ($event != $eventold)
	{
      	  print "\nFile(s) for RUN=$run and EVENT= $event: \n";
        }
    
	  $eventold=$event;
   
	 # print "\t FileStream= $stream and FileSequnce = $sequence \n";
         $getFileName = "SELECT file from daqFileTag where fileStream = $stream and fileSequence = $sequence and run = $run";
         $sthFile = $dbh2->prepare($getFileName);
	 $sthFile->execute;
	 $fileName = $sthFile->fetchrow;
	 print "\t$fileName\n";
   }#end while
 }#end foreach
} else{ 

    getDaqMap($runnumber);
    $myQuery = "SELECT fileStream, fileSequence, run, eventNumber FROM $tables[$i] where run = $runnumber and eventNumber = $eventNumber";
    $dbh2 = DBI->connect("DBI:mysql:RunLog_daq:onlsun1.starp.bnl.gov:$port",$dbUser,"")
	|| die "cannot connenct to server $DBI::errstr\n";
 
   $sthGet = $dbh2->prepare($myQuery);
    $sthGet->execute;
     $myCountQuery = $myQuery;

     $myCountQuery =~ s/fileStream, fileSequence, run, eventNumber/count\(\*\)/;
      $sthcountGet = $dbh2->prepare($myCountQuery);
       $sthcountGet->execute;  
   
   if(!$sthcountGet -> fetchrow){ print "run or event not found \n";exit;}

     while ( ( $stream, $sequence, $run, $event ) = $sthGet -> fetchrow_array)
    {
    
        if ($event != $eventold)
	{
      	  print "\nFile(s) for RUN=$run and EVENT= $event: \n";
        }
    
	  $eventold=$event;
	 # print "\t FileStream= $stream and FileSequnce = $sequence \n";
       $getFileName = "SELECT file from daqFileTag where fileStream = $stream and fileSequence = $sequence and run = $run";
       $sthFile = $dbh2->prepare($getFileName);
	$sthFile->execute;
	$fileName = $sthFile->fetchrow;
	print "\t$fileName\n";
        #  print "$myQuery\n";
    }
}
$sthFile->finish();
$sthGet->finish();
exit;

sub numeric { $a <=> $b }

sub useage {
    print "\n\t*****USAGE*****\n-f <<filename>> for inputing a 2 column, space delimetted file, containing <<run>> <<event>> \n or -r <<run>> -e <<event>>\n\n";
    exit;
}

sub getDaqMap{
  
  $runNumber = $_[0];
  $dbUser='deph';  #select only queries open to star machines
  $dbPass='';
  
  if ($runNumber> 6000000)
  {
    $port=3404;
    $daqMax  = 20; #number of event tag tables for the given run/port
  }
  elsif ( $runNumber>5000000)
  {
    $port = 3403;
    $daqMax = 4;
  }
  elsif ( $runNumber>4000000)
  {
    $port = 3402;
    $daqMax = 12;
  }
  elsif ( $runNumber>3000000)
  {
    $port = 3401;
    $daqMax = 4;
  }
  elsif ( $runNumber>2000000)
  {
    $port = 3400;
    $daqMax = 1;
}else{print "Not a valid run number\n"; exit;}

$baseTable = "daqEventTag";

 @tables;
 @sub=(1);
 for ($j=1; $j<=$daqMax ; $j++)
 {
    if($j <10) {
	$table = "$baseTable"; $table .= "0$sub[j]";
        
	push(@tables,"$table");
    }else{
	$table = "$baseTable$sub[j]";
        push(@tables,"$table");
    }
  # print "$table\n";
    $sub[j]+=1;
 }

 push(@tables ,"$baseTable");

 $dbh = DBI->connect("DBI:mysql:RunLog_daq:onlsun1.starp.bnl.gov:$port",$dbUser,"")
   || die "cannot connenct to server $DBI::errstr\n";

 for ($i=0 ; $i <=$daqMax ; $i++)
 {
  # print "$tables[$i]\n";    

   $queryFetch = "SELECT min(run), max(run) FROM $tables[$i]";
   $sthFetch = $dbh->prepare($queryFetch);
   $sthFetch->execute;

  while ( ( $min, $max ) = $sthFetch -> fetchrow_array)
  {
      if ($min < $runNumber && $max > $runNumber)
      {  
	#  print "min runNumber is $min :: max runNumber is $max for table $tables[$i] \n";

          return  $table[$i];
      } #end if
  } #end while
 }
  $queryFetch->finish();
  $dbh->diconnect();
}

