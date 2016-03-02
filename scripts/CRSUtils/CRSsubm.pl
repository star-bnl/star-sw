#! /usr/local/bin/perl -w
#
# L. Didenko
#
# CRSsubmit.pl - submit reco jobs to CRS farm
#
###############################################################################

my $prodSer = $ARGV[0];  
my $pflag = $ARGV[1];
 
my $CRSDIR  = "/home/starreco/newcrs/bin";
my $jobdir;
my $archdir;

my @statlist = ();
my @jobslist = ();
my $timestamp ;

use DBI;

$dbhost="duvall.star.bnl.gov";
$dbuser="starreco";
$dbpass="";
$dbname="operation";


if( $pflag eq "reco" ){

 $jobdir  = "/home/starreco/newcrs/" . $prodSer ."/requests/daq/jobfiles";
 $archdir = "/home/starreco/newcrs/" . $prodSer ."/requests/daq/archive";
  
 $JobStatusT = "JobStatus2011";

}elsif( $pflag eq "calib" ) {

 $jobdir  = "/home/starreco/newcrs/" . $prodSer ."/requests/daq/jobs_calib";
 $archdir = "/home/starreco/newcrs/" . $prodSer ."/requests/daq/archive_calib";

$JobStatusT = "CalibJobStatus";

 }

@statlist = `$CRSDIR/farmstat`;

my $Ncreate = 0;

my $n1 = 0;
my $nlast = 0;
my $jbfile;
my @prt = ();
my $year;

($sec,$min,$hour,$mday,$mon,$yr) = localtime();

    $mon++;
if( $mon < 10) { $mon = '0'.$mon };
if( $mday < 10) { $mday = '0'.$mday };
if( $hour < 10) { $hour = '0'.$hour };
if( $min < 10) { $min = '0'.$min };
if( $sec < 10) { $sec = '0'.$sec };

$year = $yr + 1900;

$timestamp = $year."-".$mon."-".$mday." ".$hour.":".$min.":".$sec;

print $timestamp, "\n";

foreach $line (@statlist) {
    chop $line ;
#   print  $line, "\n";
    @prt = ();
    @prt = split (" ", $line);
    if ($prt[0] eq "CREATED") {
	$Ncreate =  $prt[1];
    }
}


if($Ncreate <= 1000) {

#    chdir $jobdir;
  chdir($jobdir) || die "Could not chdir to $jobdir\n";

    @jobslist = `/bin/ls`;

    $nlast = scalar(@jobslist);

    if ($nlast <= 1) {
	print "No more jobs in the directory", "\n";
    }

    if($nlast >= 1001) { 
	$n1 = scalar(@jobslist) - 1000;     
	if ($n1 < 0){ $n1 = 0;}
    } else {
	$n1 = 0;
    }

    for ( $kk = $n1; $kk<$nlast; $kk++) {
	$jbfile = $jobslist[$kk];
	chop($jbfile);
	$lckfile = $jbfile.".lock";

        if ( -f $jbfile && ! -e $lckfile) {
	    open(FO,">$lckfile"); close(FO);  # create lock file before submission
	    print  $jbfile, "\n";
	    `$CRSDIR/crs_job -create $jbfile -q5 -p20 -drop`;
	    `/bin/mv $jbfile $archdir`;
         
      $sql="update $JobStatusT set submitTime = '$timestamp', submitAttempt = 1 where jobfileName = '$jbfile' ";
      $rv = $dbh->do($sql) || die $dbh->errstr;

	    unlink($lckfile);                 # remove lock file

	} else {
	    if ( -e  $lckfile ){
		print "[!] $jbfile has a lock file $lckfile\n"; 
	    } else {
		print "[?] $jbfile was not found (skipping)\n";
	    }
        }
	
    }
} else {
    print "No new jobs submitted", "\n";
}

exit;

