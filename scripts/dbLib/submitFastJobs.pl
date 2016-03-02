#!/usr/local/bin/perl -w
#
# $Id: submitFastJobs.pl
#
# $Log: L.Didenko
#
# submitFastJobs.pl - script to submit fast nightly test jobs to check if DEV is ready for release. 
#
##########################################################################################################

use DBI;
use Time::Local;

my $maxdate = "0000-00-00 00:00:00";
my $subpath = "/star/u/starreco/ngtest/jobs"; 
my $TESTDIR = "/star/rcf/test/devfast/*/*/*";

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
my $nowtime = ($year+1900)."-".$mon."-".$mday." ".$hour.":".$min.":".$sec;

  &StDbConnect(); 


   $sql="select max(entryDate) from $JobStatusT  where entryDate like '$todate%' and autoBuildStatus = 1 and testStatus = 0  ";
    
      $cursor =$dbh->prepare($sql)
           || die "Cannot prepare statement: $DBI::errstr\n";
      $cursor->execute();

    while( my $mxdt = $cursor->fetchrow) {

       $maxdate = $mxdt ;

        }

   $cursor->finish();


   if ( defined $maxdate ){

       `rm $TESTDIR` ;

       chdir $subpath;
 
       `star-submit -p bnl_condor_high  /star/u/starreco/devfast/test_daq.auau200.y2014.xml`;
       `star-submit -p bnl_condor_high  /star/u/starreco/devfast/test_daq.pp500.y2013.xml`;
       `star-submit -p bnl_condor_high  /star/u/starreco/devfast/test_daq.UU193.y2012.xml`;
       `star-submit -p bnl_condor_high  /star/u/starreco/devfast/test_pp500.trs.y2012.xml`;
  
    $sql="update $JobStatusT set testStatus = 1 , testSubmitTime = '$nowtime' where entryDate = '$maxdate' and autoBuildStatus = 1 and testStatus = 0 ";

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
