#! /usr/local/bin/perl -w
#
# L. Didenko
###############################################################################

my $prodSer = $ARGV[0]; 
my $fileName = $ARGV[1];  
my $CRSDIR  = "/home/starreco/newcrs/bin";
my $jobdir  = "/home/starreco/newcrs/" . $prodSer ."/requests/daq/jobfiles";
my $archdir = "/home/starreco/newcrs/" . $prodSer ."/requests/daq/archive";

my $lockfile = "/star/u/starreco/prodlists/submission.lock";

use DBI;

$dbhost="duvall.star.bnl.gov";
$dbuser="starreco";
$dbpass="";
$dbname="operation";

$JobStatusT = "JobStatus2011";


 if( -f $lockfile) {
     `rm  $lockfile` ;

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

 `mv $listName $nextName`;

 if (!open (NEWLIST, ">$listName" ))  {printf ("Unable to create file %s\n",$listName);}

 if ($lastid > 0 ) {

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

  chdir($jobdir) || die "Could not chdir to $jobdir\n";

  $lastid = scalar(@runSet) - 1;
  $lastrun = $runSet[$lastid]; 

#############################  
  chop $lastrun;
 
 print "Last runnumber  ", $lastrun, "\n";

  if ( $lastrun > 10000000 ) {

 $jobpat = "*".$prodSer."*".$lastrun."_raw*" ;
 
 print "Patten for files   ", $jobpat, "\n";

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

      `$CRSDIR/crs_job -create $jbfile -q5 -p20 -drop`;

#      `$CRSDIR/crs_job -create $jbfile -q4 -p20 -drop`;

	`/bin/mv $jbfile $archdir`;

      $sql="update $JobStatusT set submitTime = '$timestamp', submitAttempt = 1 where jobfileName = '$jbfile' ";
      $rv = $dbh->do($sql) || die $dbh->errstr;        

       } else {
                print "[?] $jbfile is not a file\n";
        }
      }    ##### for

    }else{
###### if ($nlast >= 1)

	print "No run number $lastrun in the directory ", "\n"
    }

    &StDbProdDisconnect();

    } else {
###### if ( $lastrun > 10000000 )

    print "No new run numbers", "\n";
   }

}else{
######  if($Ncreate <= 1000)

     print "Number of CREATED jobs > 1000", "\n";
 }
 

##########################

if ($lastid >=1 ) {

     for ( $kk = 0; $kk<$lastid; $kk++) {
	 $myrun = $runSet[$kk];
         chop $myrun; 
########
# print "Run number to new file:  ",$myrun, "\n";
 
       print NEWLIST  "$myrun" ,"\n";  
   }

   }else{

    print  "No more run numbers in the list", "\n";
    print NEWLIST "10000000\n";

  } 
   close (NEWLIST);
 }

################

if (!open (SUBFILE, ">$lockfile" ))  {printf ("Unable to create file %s\n",$lockfile);}
 
    print SUBFILE "Submission done", "\n";

 close (SUBFILE);


   }else{
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
