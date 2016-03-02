#! /usr/local/bin/perl -w
#
#  $Id: 
# 
# L. Didenko
# $Log: script for production jobs autosubmission to CRS farm with new crs software
#
###############################################################################

use DBI;


my $prodSer = $ARGV[0]; 
my $fileName = $ARGV[1];  
my $CRSDIR  = "/usr/bin";
my $jobdir  = "/home/starreco/" . $prodSer ."/requests/daq/jobfiles";
my $archdir = "/home/starreco/" . $prodSer ."/requests/daq/archive";

my $lockfile = "/star/u/starreco/prodlists/submission.lock";

$dbhost="duvall.star.bnl.gov";
$dbuser="starreco";
$dbpass="";
$dbname="operation";

$JobStatusT = "JobStatus2013";

#$NJOBS  = 1000;  # max jobs in CREATED state
$NJOBS  = 500;   # max jobs in CREATED state
$MAXPRCT= 0.75;  # there will be a loop until at least $NJOBS*$MAXPRCT are in the CREATED state

 if( -f $lockfile) {
     `/bin/rm  $lockfile` ;

     my $listName = "/star/u/starreco/".$fileName;
     my @runSet = ();

     open (RUNLIST, $listName ) or die "cannot open $listName: $!\n";

     @runSet = <RUNLIST>;

     close(RUNLIST);

     my @statlist = ();
     my @jobslist = ();
     my $timestamp ;
     my $lastid = 0;
     my $lastrun = 0;
     my @runs = ();
     my $myrun = 0;
     
     $lastid = scalar(@runSet);

     print "Last id  = ", $lastid, "\n";

     my $nextName = "/star/u/starreco/prodlists/".$lastid."_".$fileName;
     
     print "Next name of file list   ", $nextName, "\n";
     
     `/bin/mv $listName $nextName`;

     if ($lastid > 0 ) {

	 @statlist = `/usr/bin/crs_summary`;

	 my $Ncreate = 0;
	 my $n1 = 0;
	 my $nlast = 0;
	 my $jbfile;
	 my @prt = ();
	 my $year;
	 
	 ($sec,$min,$hour,$mday,$mon,$yr) = localtime();

	 $mon++;
	 if( $mon  < 10) { $mon  = '0'.$mon  };
	 if( $mday < 10) { $mday = '0'.$mday };
	 if( $hour < 10) { $hour = '0'.$hour };
	 if( $min  < 10) { $min  = '0'.$min  };
	 if( $sec  < 10) { $sec  = '0'.$sec  };
	 
	 $year = $yr + 1900;

	 $timestamp = $year."-".$mon."-".$mday." ".$hour.":".$min.":".$sec;

	 print $timestamp, "\n";

	 foreach $line (@statlist) {
	     chop $line ;

    next if($line =~ /---/ or $line =~ /Status/ );
             print "Line  ",  $line, "\n";

	     @prt = ();
	     @prt = split (" ", $line);

	     if ($prt[3] eq "CREATED" ) {
                 $Ncreate = $Ncreate + $prt[1];
		 print "Check  status ", $prt[1],"  %  ",$prt[3], "\n";
	     }elsif ($prt[3] eq "QUEUED" ) {
                 $Ncreate = $Ncreate + $prt[1];
		 print "Check  status ", $prt[1],"  %  ",$prt[3], "\n";

	     }elsif($prt[3] eq "SUBMITTED" ) {
                 $Ncreate = $Ncreate +  $prt[1];  
		 print "Check  status ", $prt[1],"  %  ",$prt[3], "\n";           

	     }elsif($prt[3] eq "STAGING" ) {
                 $Ncreate = $Ncreate +  $prt[1];  
		 print "Check  status ", $prt[1],"  %  ",$prt[3], "\n";           

	     }
	 }

       GO_SUBMIT:
	 if ($Ncreate <= $NJOBS ) {

	     chdir($jobdir) || die "Could not chdir to $jobdir\n";

	     $lastid  = scalar(@runSet) - 1;
	     $lastrun = pop(@runSet);  # $runSet[$lastid]; 

	     chop $lastrun;
 
	     print "Last runnumber\t\t $lastrun\n";

	     if ( $lastrun > 10000000 ) {
		 $jobpat = "*".$prodSer."*".$lastrun."_raw*" ;
 
		 print "Pattern for files\t $jobpat\n";

		 @jobslist = `/bin/ls  $jobpat`;

		 $nlast = scalar(@jobslist);

		 #####  connect to production DB

		 &StDbProdConnect();

		 if ($nlast >= 1) {

		     for ( $kk = $n1; $kk<$nlast; $kk++) {
			 $jbfile = $jobslist[$kk];
			 chop $jbfile ;
			 if ( -f $jbfile) {
			     print  $jbfile, "\n";
			     
                             `/usr/bin/crs_job -insert $jbfile`;

			     `/bin/mv $jbfile $archdir`;
			     $Ncreate++;

	$sql= "update $JobStatusT set submitTime = '$timestamp', submitAttempt = 1 where jobfileName = '$jbfile' ";
	$rv = $dbh->do($sql) || die $rv." ".$dbh->errstr;        

			 } else {
			     print "[?] $jbfile is not a file\n";
			 }
		     }    ##### for
		     
		 } else { ###### if ($nlast >= 1)
		     print "No run number $lastrun in the directory ", "\n";
		 }
		 
		 &StDbProdDisconnect();

	     } else {     ###### if ( $lastrun > 10000000 )
		 print "No new run numbers \n";

	     }

	 } else {         ######  if($Ncreate <= $NJOBS )
	     print "Number of CREATED jobs $Ncreate > $NJOBS \n";

	 }
 
	 
         ##########################
	 if (!open (NEWLIST, ">$listName" )){
	     printf ("Unable to create file %s\n",$listName);
	 } else {
	     if ($lastid >=1 ) {
		 #for ( $kk = 0; $kk<$lastid; $kk++) {
		 # we already pop-ed the last element
		 for ( $kk = 0; $kk <= $#runSet ; $kk++) {
		     $myrun = $runSet[$kk];
		     chop $myrun; 
		     ########
		     # print "Run number to new file:  ",$myrun, "\n";
 
		     print NEWLIST  "$myrun\n";  
		 }

	     } else {

		 print  "No more run numbers in the list", "\n";
		 print NEWLIST "10000000\n";
	     } 
	     close (NEWLIST);
	 }
	 if ($Ncreate < $NJOBS*$MAXPRCT && $lastid > 1){
	     print "We are now at $Ncreate jobs in SUBMITTED state - below ".int($NJOBS*$MAXPRCT)."\n";
	     goto GO_SUBMIT;
	 }
     }

     ################

     if (!open (SUBFILE, ">$lockfile" ))  {printf ("Unable to create file %s\n",$lockfile);}
 
     print SUBFILE "Submission done", "\n";

     close (SUBFILE);

 } else {
     exit;
 }

exit;


######################
sub StDbProdConnect {
    $dbh = DBI->connect("dbi:mysql:$dbname:$dbhost", $dbuser, $dbpass)
        || die "Cannot connect to db server $DBI::errstr\n";
}


######################
sub StDbProdDisconnect {
    $dbh = $dbh->disconnect() || die "Disconnect failure $DBI::errstr\n";
}
