#!/usr/bin/env perl
#
# $Id: scanFastDEV.pl
#
# $Log: L.Didenko
#
# scanFastDEV.pl - script to scan error messages in log files for Fast DEV release test. 
#
##########################################################################################################

use DBI;
use Time::Local;

my $TOPDIR = "/star/rcf/test/devfast/";
my @OUTDIR = ();
my $devflag = "complete";
my @files = ();
my $fullname;
my $output = ();
my $errMessage = "none";
my $logcount = 0;
my $email = "didenko\@bnl.gov,jeromel\@bnl.gov";
my $message = "DEV test failed";
my $subject = "DEV test failed";

my $mTime;
my $ltime;
my $tyear;
my @artime = ();
my $maxtime = "0000-00-00 00:00:00";
my $maxdate = "0000-00-00 00:00:00";
my $ii = 0;

my $yr;
my $mo;
my $dy;
my $hr;
my $min;
my $sec;

my $jobpath = "none";

$dbhost="duvall.star.bnl.gov";
$dbuser="starreco";
$dbpass="";
$dbname="LibraryJobs";

$JobStatusT = "fastJobsStatus";

($sec,$min,$hour,$mday,$mon,$year) = localtime();

$mon++;
if( $mon < 10) { $mon = '0'.$mon };
if( $mday < 10) { $mday = '0'.$mday };
if( $hour < 10) { $hour = '0'.$hour };
if( $min < 10) { $min = '0'.$min };
if( $sec < 10) { $sec = '0'.$sec };


my $todate = ($year+1900)."-".$mon."-".$mday ;

my $dirtree =  $TOPDIR."*/*" ;

 @OUTDIR = `ls -d $dirtree` ;

 foreach my $eachdir (@OUTDIR) {

  chop $eachdir;
  print $eachdir, "\n";

  opendir(DIR, $eachdir) or die "can't open $eachdir\n";

     @files = readdir(DIR);

     foreach my $fname ( @files) {
      next if !$fname;
      next if $fname =~ /^\.\.?$/;
      next if $fname =~ /.root/;  
   
      if ($fname =~ /.log/)  {

    $logcount++;

    $fullname = $eachdir."/".$fname;

    ($mTime) = (stat($fullname))[9];
    ($sec,$min,$hr,$dy,$mo,$yr) = (localtime($mTime))[0,1,2,3,4,5];

    $mo++;
    $tyear = $yr + 2000;

    $ltime = sprintf ("%4.4d-%2.2d-%2.2d %2.2d:%2.2d:%2.2d",
                       $tyear,$mo,$dy,$hr,$min,$sec);

#   print $fullname,"   ", $ltime, "\n";

      $artime[$ii] = $ltime;
      $ii++;

     @output = ();
     @output = `tail -1200 $fullname`;

     foreach my $line (@output) {

    if ($line =~ /Abort/)  {  
         $devflag = "failed";
         $errMessage = "Abort";
         $jobpath = $fullname;
    }elsif ($line =~ /segmentation violation/) {
         $devflag = "failed"; 
         $errMessage = "Segmentation violation";
         $jobpath = $fullname;  
    } elsif ($line =~ /segmentation fault/) {
         $devflag = "failed"; 
         $errMessage = "Segmentation fault";
         $jobpath = $fullname;  
    } elsif ($line =~ /undefined symbol/) {
         $devflag = "failed";  
         $errMessage = "undefined symbol";
         $jobpath = $fullname;
    }elsif ($line =~ /Assertion/ & $line =~ /failed/)  {
         $devflag = "failed";  
         $errMessage = "Assertion failed";
         $jobpath = $fullname;
    }elsif ($line =~ /FATAL/ and $line =~ /floating point exception/) {
         $devflag = "failed"; 
         $errMessage = "FATAL, floating point exception", 
         $jobpath = $fullname;
    }elsif ($line =~ /glibc detected/)  {
         $devflag = "failed"; 
         $errMessage = "glibc detected";
         $jobpath = $fullname;
    }
         }
       }
     }
 }

  for ($ii = 0;$ii<scalar(@artime);$ii++) {
      if ($artime[$ii] gt  $maxtime ) {
	  $maxtime = $artime[$ii];
      }
   }

#   print "Max time ", $maxtime, "\n";

  &StDbConnect(); 

  $sql="select max(entryDate) from $JobStatusT  where entryDate like '$todate%' and autoBuildStatus = 1 and testStatus = 1  ";

      $cursor =$dbh->prepare($sql)
           || die "Cannot prepare statement: $DBI::errstr\n";
      $cursor->execute();

    while( my $mxdt = $cursor->fetchrow) {

       $maxdate = $mxdt ;

        }

   $cursor->finish();


   if($devflag eq "failed") {

 $message = "DEV test failed;  error message:  ".$errMessage."; for more information look at the test path: ".$jobpath ;

   system("echo \"$message\" | mail -s \"$subject\" $email");

    $sql="update $JobStatusT set testStatus = 3, testCompleteTime = '$maxtime', testInfo = '$errMessage' where entryDate = '$maxdate' and testStatus = 1 ";

    $rv = $dbh->do($sql) || die $dbh->errstr;
   
   }elsif($devflag eq "complete" and $logcount == 4 ) {

    $sql="update $JobStatusT set testStatus = 2, testCompleteTime = '$maxtime', testInfo = 'none' where entryDate = '$maxdate' and testStatus = 1 ";

    $rv = $dbh->do($sql) || die $dbh->errstr;

  }

 &StDbDisconnect();

exit;

################################################################################

sub StDbConnect {
    $dbh = DBI->connect("dbi:mysql:$dbname:$dbhost", $dbuser, $dbpass)
        || die "Cannot connect to db server $DBI::errstr\n";
}

######################
sub StDbDisconnect {
    $dbh = $dbh->disconnect() || die "Disconnect failure $DBI::errstr\n";
}
