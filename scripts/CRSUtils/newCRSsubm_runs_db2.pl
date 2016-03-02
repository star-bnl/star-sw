#! /usr/local/bin/perl -w
#
#  $Id: 
# 
# L. Didenko
# $Log: script for production jobs autosubmission to CRS farm with new crs software
# using different JobStatus20XX table from different years data taken
#
###############################################################################

use DBI;

my @prodtag = ();
my @prodset = ("AuAu_200_production_mid_2014","production_pp200trans_2015");
my @jobdir = ();
my @archdir = ();

$prodtag[0] = $ARGV[0]; 

 $jobdir[0]  = "/home/starreco/" . $prodtag[0] ."/requests/daq/jobfiles";
 $archdir[0] = "/home/starreco/" . $prodtag[0] ."/requests/daq/archive";

if( defined($ARGV[1]) ) {
 $prodtag[1] = $ARGV[1];

 $jobdir[1]  = "/home/starreco/" . $prodtag[1] ."/requests/daq/jobfiles";
 $archdir[1] = "/home/starreco/" . $prodtag[1] ."/requests/daq/archive";

}

my $lockfile = "/star/u/starreco/prodlists/submission.lock";

$dbhost="duvall.star.bnl.gov";
$dbuser="starreco";
$dbpass="";
$dbname="operation";

$JobStatusT = "JobStatus2014";

$JobStatusT2015 = "JobStatus2015";

my $NJOBS  = 2200;  # max jobs in CREATED state
#$NJOBS  = 500;   # max jobs in CREATED state


 if( -f $lockfile) {
     `/bin/rm  $lockfile` ;


 my @statlist = ();
 my @jobslist = ();
 my $timestamp ;
 my @nlmt = ();

 $nlmt[0] = 400;
 $nlmt[1] = 600; 

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

        
	   @jobslist = ();
           $kk = 0;

 $sql="SELECT jobfileName  FROM $JobStatusT where jobfileName like '$prodset[0]%$prodtag[0]%' and jobStatus = 'n/a' and submitTime = '0000-00-00 00:00:00' order by runID  limit $nlmt[0] ";

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

    chdir($jobdir[0]) || die "Could not chdir to $jobdir[0]\n";
    
     print $jobdir[0], "\n";

		 if ($nlast >= 1) {

		     for ( $ik = 0; $ik<$nlast; $ik++) {
			 $jbfile = $jobslist[$ik];
#			 chop $jbfile ;

			 if ( -f $jbfile) {
			     print  $jbfile, "\n";
			     
                             `/usr/bin/crs_job -insert $jbfile`;

			     `/bin/mv $jbfile $archdir[0]`;

	$sql= "update $JobStatusT set submitTime = '$timestamp', submitAttempt = 1 where jobfileName = '$jbfile' ";
	$rv = $dbh->do($sql) || die $rv." ".$dbh->errstr;        

			 } else {
			     print "[?] $jbfile is not a file\n";
			 }
		     }    ##### for
		     
		 } else { ###### if ($nlast >= 1)
		     print "No jobfiles to submit ", "\n";
		 }
	
############# submit run 2015 data


	   @jobslist = ();
           $kk = 0;

 $sql="SELECT jobfileName  FROM $JobStatusT2015 where jobfileName like '$prodset[1]%$prodtag[1]%' and jobStatus = 'n/a' and submitTime = '0000-00-00 00:00:00' order by runID  limit $nlmt[1] ";

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

    chdir($jobdir[1]) || die "Could not chdir to $jobdir[1]\n";
    
     print $jobdir[1], "\n";

		 if ($nlast >= 1) {

		     for ( $ik = 0; $ik<$nlast; $ik++) {
			 $jbfile = $jobslist[$ik];
#			 chop $jbfile ;

			 if ( -f $jbfile) {
			     print  $jbfile, "\n";
			     
                             `/usr/bin/crs_job -insert $jbfile`;

			     `/bin/mv $jbfile $archdir[1]`;

	$sql= "update $JobStatusT2015 set submitTime = '$timestamp', submitAttempt = 1 where jobfileName = '$jbfile' ";
	$rv = $dbh->do($sql) || die $rv." ".$dbh->errstr;        

			 } else {
			     print "[?] $jbfile is not a file\n";
			 }
		     }    ##### for
		     
		 } else { ###### if ($nlast >= 1)
		     print "No jobfiles to submit ", "\n";
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
