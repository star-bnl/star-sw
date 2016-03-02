#!/opt/star/bin/perl -w
#
#  $Id:
#
#  $Log:   readMailfile.pl - script to read email files and update production job status database
#          
#  L.Didenko
#
############################################################################################################

use DBI;
use Time::Local;
use Mysql;

$dbhost="duvall.star.bnl.gov";
$dbuser="starreco";
$dbpass="";
$dbname="Embedding_job_stats";

my $JobStatusT = "jobs_prod_2013";

my $outname;
my $outfile;
my @mailfile = ();
my $mail_line;
my $thisday;
my @wrd = ();
my $jobId = 0;
my $prcId = 0;
my $jbstat;
my $dqsize = 0;
my $mailfdone;

my ($sec,$min,$hour,$mday,$mon,$yr) = localtime();

 $mon++;
 if( $mon  < 10) { $mon  = '0'.$mon  };
 if( $mday < 10) { $mday = '0'.$mday };
 if( $hour < 10) { $hour = '0'.$hour };
 if( $min  < 10) { $min  = '0'.$min  };
 if( $sec  < 10) { $sec  = '0'.$sec  };

my $year = $yr + 1900;

$thisday = $year."-".$mon."-".$mday;

$outname = "mail" . "_" .$thisday . "_" . "out";

$outfile = "/star/u/starreco/" . $outname;
$mailfdone = $outfile."_done";

 print $outfile, "\n";

open (MAILFILE, $outfile ) or die "cannot open $outfile: $!\n";

@mailfile = <MAILFILE>;

close (MAILFILE);

 &StDbConnect();

  foreach $mail_line (@mailfile) {
     chop $mail_line ;
     @wrd = ();
     if ($mail_line =~ /JobInfo/ ) {
      @wrd = split ("%", $mail_line);
      $jobId = $wrd[1];
      $prcId = $wrd[2];
      $jbstat = $wrd[3];
      $dqsize = $wrd[4];

 $sql= "update $JobStatusT set jobState = '$jbstat' where sumsRequestID = '$jobId' and sumsJobIndex = '$prcId' ";

 $rv = $dbh->do($sql) || die $rv." ".$dbh->errstr;

      if($jbstat eq "daq_transferred" ) {

 $sql= "update $JobStatusT set daqSizeOnSite = '$dqsize' where sumsRequestID = '$jobId' and sumsJobIndex = '$prcId' ";

 $rv = $dbh->do($sql) || die $rv." ".$dbh->errstr;

     }
    }
  }

# `mv $outfile $mailfdone`;
  rename($outfile,$mailfdone);

 &StDbDisconnect();


exit;

######################
sub StDbConnect {
    $dbh = DBI->connect("dbi:mysql:$dbname:$dbhost", $dbuser, $dbpass)
        || die "Cannot connect to db server $DBI::errstr\n";
}


######################
sub StDbDisconnect {
    $dbh = $dbh->disconnect() || die "Disconnect failure $DBI::errstr\n";
}
