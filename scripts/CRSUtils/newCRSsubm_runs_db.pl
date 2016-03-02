#! /usr/local/bin/perl -w
#
#  $Id: newCRSsubm_runs_db.pl
# 
# L. Didenko
# $Log: script for production jobs autosubmission to CRS farm with new crs software
# using for sibmission operation DB table JobStatus201X and jobfilename and submitting
# 2 production sets with some proportion  
#
###############################################################################

use DBI;

my @prodtag = ();
my @prodset = ("AuAu_200_production_high_2014","AuAu_200_production_mid_2014");
my @jobdir = ();
my @archdir = ();

$prodtag[0] = $ARGV[0]; 

if( defined($ARGV[1]) ) {
 $prodtag[1] = $ARGV[1];
}
if( defined($ARGV[2]) ) {
 $prodtag[2] = $ARGV[2];
}

for($ii = 0; $ii< scalar(@prodtag); $ii++) {


 $jobdir[$ii]  = "/home/starreco/" . $prodtag[$ii] ."/requests/daq/jobfiles";
 $archdir[$ii] = "/home/starreco/" . $prodtag[$ii] ."/requests/daq/archive";

}

my $lockfile = "/star/u/starreco/prodlists/submission.lock";

$dbhost="duvall.star.bnl.gov";
$dbuser="starreco";
$dbpass="";
$dbname="operation";

$JobStatusT = "JobStatus2014";

my $NJOBS  = 2000;  # max jobs in CREATED state
#$NJOBS  = 500;   # max jobs in CREATED state
my $MAXPRCT= 0.75;  # there will be a loop until at least $NJOBS*$MAXPRCT are in the CREATED state

 if( -f $lockfile) {
     `/bin/rm  $lockfile` ;


 my @statlist = ();
 my @jobslist = ();
 my $timestamp ;
 my @nlmt = ();

 $nlmt[0] = 600; # 900;
 $nlmt[1] = 400; # 100; 

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
#		 print "Check  status ", $prt[1],"  %  ",$prt[3], "\n";
	     }elsif ($prt[3] eq "QUEUED" ) {
                 $Ncreate = $Ncreate + $prt[1];
#		 print "Check  status ", $prt[1],"  %  ",$prt[3], "\n";

	     }elsif($prt[3] eq "SUBMITTED" ) {
                 $Ncreate = $Ncreate +  $prt[1];  
#		 print "Check  status ", $prt[1],"  %  ",$prt[3], "\n";           

	     }elsif($prt[3] eq "STAGING" ) {
                 $Ncreate = $Ncreate +  $prt[1];  
#		 print "Check  status ", $prt[1],"  %  ",$prt[3], "\n";           

	     }
	 }

###       GO_SUBMIT:
	 if ($Ncreate <= $NJOBS ) {

###
  &StDbProdConnect();

        
       for($i = 0; $i< scalar(@prodtag); $i++) {

	   @jobslist = ();
           $kk = 0;

 $sql="SELECT jobfileName  FROM $JobStatusT where jobfileName like '$prodset[$i]%$prodtag[$i]%' and jobStatus = 'n/a' and submitTime = '0000-00-00 00:00:00' order by runID  limit $nlmt[$i] ";

      $cursor =$dbh->prepare($sql)
          || die "Cannot prepare statement: $DBI::errstr\n";
       $cursor->execute();

       while( my $jbf = $cursor->fetchrow() ) {

          $jobslist[$kk] = $jbf;
#	  print $jobslist[$kk], "\n";

          $kk++;
       }
    $cursor->finish();

    $nlast = scalar(@jobslist);

    chdir($jobdir[$i]) || die "Could not chdir to $jobdir[$i]\n";
    
     print $jobdir[$i], "\n";

		 if ($nlast >= 1) {

		     for ( $ik = 0; $ik<$nlast; $ik++) {
			 $jbfile = $jobslist[$ik];
#			 chop $jbfile ;

			 if ( -f $jbfile) {
			     print  $jbfile, "\n";
			     
                             `/usr/bin/crs_job -insert $jbfile`;

			     `/bin/mv $jbfile $archdir[$i]`;

	$sql= "update $JobStatusT set submitTime = '$timestamp', submitAttempt = 1 where jobfileName = '$jbfile' ";
	$rv = $dbh->do($sql) || die $rv." ".$dbh->errstr;        

			 } else {
			     print "[?] $jbfile is not a file\n";
			 }
		     }    ##### for
		     
		 } else { ###### if ($nlast >= 1)
		     print "No jobfiles to submit ", "\n";
		 }
            }
		 
		 &StDbProdDisconnect();


	 } else {         ######  if($Ncreate <= $NJOBS )
	     print "Number of CREATED jobs $Ncreate > $NJOBS \n";

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
