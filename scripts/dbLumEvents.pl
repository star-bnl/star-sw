#!/usr/bin/perl

#############################################
#        STAR Database Script::dbLumEvents.pl
#############################################
#             Author: Michael DePhillips
#############################################
#                Date: November 16, 2005
#############################################
#
#  Script to get luminosicity and number of events for
#  a sinlge RunNumber or File containing many runs.          
#  Results are outputted with luminocity ascending
#   
#############################################
#
#   connects to 1)dbx - Calibrations_rich 
#               2)onlsun1 - port is dynamic
#               
#############################################

use DBI;
use Getopt::Std;


#UNCOMMENT BELOW FOR DEBUGGING DBI
#DBI->trace(1);


getopts('f:r:h:t:b:m');

$inputFile=$opt_f;
$runnumber = $opt_r;
$top = $opt_t;
$bottom = $opt_b;
$middle = $opt_m;
$helpout=$opt_h;


if($helpout or (!$inputFile && !$runnumber ))
{useage();}

 $dbh2 = DBI->connect("DBI:mysql:Calibrations_rich:dbx.star.bnl.gov:3316",$dbUser,"")
    || die "cannot connenct to server $DBI::errstr\n";

if ($inputFile)
{
   my $totalCount = 0;
   my $oldPort = 0;
 
   printf("\n%-15s%-25s%-21s%-15s\n",  "Run Number", "LUMINOSITY (zdcX)", "Number of Events", "Running Total of Events");
    print "====================================================================================\n";
    if(open(IN,"< $inputFile")) {
	while(my $line = <IN>) {
	    chomp $line;
	     $runNumber=  $line;
  getPort($runNumber);
	   $h_info{$runNumber}->{PORT} = $port;
       }#end while
    }else { print "file not found \n"; exit;}
    close IN;
  

   foreach  $runNumber    (sort numeric keys %h_info) {
   
     $myQuery = "SELECT runNumber,  max(zdcX)  FROM trigDetSums where runNumber = $runNumber and zdcX > 0 group by runNumber order by zdcX"; 
        if ($port != $oldPort){
	    getConn($port);
            $oldPort = $port;
           }
    
     $sthGet = $dbh2->prepare($myQuery);
     $sthGet->execute;

    while ( ( $runNumber, $lum ) = $sthGet -> fetchrow_array)
     {
         $h_lum{$lum}->{$runNumber} = 1;
     } #end while
 } # end foreach
  
   if($top){ 
      foreach $lum (sort reversenumeric keys %h_lum){
	  foreach $run (keys %{$h_lum{$lum}}) {
	    
	     $queryFetch = "SELECT runNumber, sum(numberofEvents) FROM daqSummary where runNumber = $run group by runNumber";
	     $sthFetch = $dbh->prepare($queryFetch);
	     $sthFetch->execute;
	    
	 COUNTS:    while ( ( $runN, $events ) = $sthFetch -> fetchrow_array)
	     { if ($totalCount <= $top*1000000) {
			 
		     $totalCount += $events;
			 printf("%-15d%-25f%-21d%-15d\n",  $run, $lum , $events , $totalCount)
			 }else{ last COUNTS;}
	   }
	 }
      }
  }else{ 
      foreach  $lum    (sort numeric keys %h_lum) {
       	 foreach $run (keys %{$h_lum{$lum}}) {
	    
	     $queryFetch = "SELECT runNumber, sum(numberofEvents) FROM daqSummary where runNumber = $run group by runNumber";
	     $sthFetch = $dbh->prepare($queryFetch);
	     $sthFetch->execute;
	    
	 COUNTS:    while ( ( $runN, $events ) = $sthFetch -> fetchrow_array)
	     {
		 if($bottom){  
		     if ($totalCount <= $bottom*1000000) {
			 $totalCount += $events;
			 printf("%-15d%-25f%-21d%-15d\n",  $run, $lum , $events , $totalCount)
			 }else{ last COUNTS;}

		  }elsif($middle){ 
		      push(@lums,$lum);
                      push(@runs,$run);
		      push(@eventnum,$events)
			  
                   }else{
			     $totalCount += $events;
			     printf("%-15d%-25f%-21d%-15d\n",  $run, $lum , $events , $totalCount);
			 }
             }#end while
	 }#end inner for each
     }#end outer for each

 } 
   if($middle){
                      my $first = @lums[0];
		      my $last = @lums[$#lums];
		      my $diff = $last - $first;
		      my $mean = $diff/2;
			    
		    MID: for ( $i=0; $i<$last; $i++){
       			if(@lums[$i]>$mean)
			{
			    my $meanV = @lums[$i];
			    $index = $i;
       			    last MID;
			}
		    }
		      $totalCount= @eventnum[$index];
		      printf("%-15d%-25f%-21d%-15d\n",  @runs[$index], @lums[$index] , @eventnum[$index], $totalCount);
		     
			ZIG:  for($j=1;$j<50;$j++)
			   {
			       $totalCount+=@eventnum[$j+$index];
			       printf("%-15d%-25f%-21d%-15d\n",  @runs[$j+$index], @lums[$j+$index] , @eventnum[$j+$index] , $totalCount);

			       $totalCount+=@eventnum[$index-$j];
			       printf("%-15d%-25f%-21d%-15d\n",  @runs[$index-$j], @lums[$index-$j] , @eventnum[$index-$j] , $totalCount);
			       if($totalCount>$middle*1000000){
				   last ZIG;
			       }
			   }
		  }
} elsif ($runnumber) {

     printf("\n%-15s%-25s%-21s%-15s\n",  "Run Number", "LUMINOSITY (zdcX)", "Number of Events");
     print "========================================================\n";

     getPort($runnumber);
     getConn($port);

     $myQuery = "SELECT  max(zdcX)  FROM trigDetSums where runNumber = $runnumber and zdcX > 0 group by runNumber order by zdcX"; 
     
     $sthGet = $dbh2->prepare($myQuery);
     $sthGet->execute;
    
     $lum  = $sthGet -> fetchrow;
               
          $queryFetch = "SELECT sum(numberofEvents) FROM daqSummary where runNumber = $runnumber group by runNumber";
	     $sthFetch = $dbh->prepare($queryFetch);
	     $sthFetch->execute;
     $eventN = $sthFetch->fetchrow;
     
     printf("%-15d%-25f%-21d\n\n",  $runnumber, $lum , $eventN );

}exit;
 sub getConn {
     $port = $_[0];
     $dbh = DBI->connect("DBI:mysql:RunLog:onlsun1.starp.bnl.gov:$port",$dbUser,"")
	 || die "cannot connenct to server $DBI::errstr\n";
}    
 sub numeric { $a <=> $b }
 sub reversenumeric { $b <=> $a }
 sub useage {
     print "\nHELP\n To use a file containing a list of runs :: -f <<filename>>[-b<<num>>|-m<<num>>|-t<<num>>]\n";
     print "\t No additional flag sorts the entire file low lum to high";
     print "\n\t\t -b <<num>> returns lowest lum to high for num*1000000 events";
     print "\n\t\t -t <<num>> returns highest lum to low for num*1000000 events";
     print "\n\t\t -m <<num>> returns middle lum up/down for num*1000000 events";
     print " \n***NOTE:: A file containing 841 runs takes ~2.5 mins to complete*** \n";
     print "\n For a single run::";
     print "\n\t -r <<run>> returns lum and eventcount for <<run>>\n\n";
    exit;
 }

 sub getPort{
  
  $runNumber = $_[0];
  $dbUser='deph'; 
  $dbPass='';
  
  if ($runNumber> 6000000)
  {
    $port=3404;
  }
  elsif ( $runNumber>5000000)
  {
    $port = 3403;
  }
  elsif ( $runNumber>4000000)
  {
    $port = 3402;
  }
  elsif ( $runNumber>3000000)
  {
    $port = 3401;
  }
  elsif ( $runNumber>2000000)
  {
    $port = 3400;
  }else{print "Not a valid run number\n"; exit;}
} #endsub getPort
