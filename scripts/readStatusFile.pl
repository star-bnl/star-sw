#!/usr/bin/env perl
#
#  $Id: readStatusFile.pl
#
#  $Log:   readStatusFile.pl - script to read status files on NFS and update production job status database
#          
#  L.Didenko
#
############################################################################################################

use DBI;
use Time::Local;

$dbhost="duvall.star.bnl.gov";
$dbuser="starreco";
$dbpass="";
$dbname="Embedding_job_stats";

my $JobStatusT = "jobs_prod_2016";

my $statusPath = $ARGV[0];

my $outfile;
my @statusfile = ();
my @statfile = ();
my @wrd = ();
my @prt = ();
my $prodtg;
my $daqfile;
my $jbstat;
my $dqsize = 0;
my $nfspath = "/star/data16/GRID/daq/2015/";
my $statSize = 0;
my $fulldname;
my $inpsize = 0;
my $sitedsize = 0;

my ($sec,$min,$hour,$mday,$mon,$year) = localtime(time);

$mon++;
if( $mon < 10) { $mon = '0'.$mon };
if( $mday < 10) { $mday = '0'.$mday };
if( $hour < 10) { $hour = '0'.$hour };
if( $min < 10) { $min = '0'.$min };
if( $sec < 10) { $sec = '0'.$sec };


my $todate = ($year+1900)."-".$mon."-".$mday." ".$hour.":".$min.":".$sec;


 print "############################################", "\n";
 print "UPDATE JOBS STATUS at   ",$todate, "\n";
 print "--------------------------------------------", "\n";

 chdir $statusPath;

 @statusfile = ();
 @statusfile = `ls *daq_transferred`;

 &StDbConnect();

if( scalar(@statusfile) >= 1) {

 foreach my $sline (@statusfile) {
     chop $sline ;
     print $sline, "\n" ;
     $outfile = $statusPath."/".$sline ;
     $statSize = (stat($outfile))[7];

  print "Size of status file  ", $outfile,"   %   ", $statSize, "\n"; 
     next if( $statSize <= 1) ;
     @wrd = ();
     @wrd = split ("-", $sline);
     $prodtg = $wrd[0];
     $daqfile = $wrd[1].".daq";
     $jbstat = $wrd[2];

     @statfile = ();

 open (STATFILE, $outfile ) or die "cannot open $outfile: $!\n";

 @statfile = <STATFILE>;

 close (STATFILE);

 foreach my $line (@statfile) {
    chop $line;
    @prt = ();
    @prt = split ("% ", $line);
    $dqsize = $prt[3];  
    chop $dqsize;

 }
     print "Check name, size of file  ", $daqfile, "  %  ", $dqsize, "\n";

   $sql= "update $JobStatusT set jobProgress = '$jbstat', daqSizeOnSite = '$dqsize' where prodTag = '$prodtg' and inputFileName = '$daqfile' and jobProgress = 'none' ";

  print "$sql\n";
 
   $rv = $dbh->do($sql) || die $rv." ".$dbh->errstr;

##########

 $inpsize = 0;
 $sitedsize = 0;

 $sql="SELECT inputFileSize, daqSizeOnSite  FROM $JobStatusT where prodTag = '$prodtg' and inputFileName = '$daqfile' and jobProgress = 'daq_transferred' and inputFileExists = 'yes'  ";

    $cursor =$dbh->prepare($sql)
    || die "Cannot prepare statement: $DBI::errstr\n";
     $cursor->execute;

   while(@fields = $cursor->fetchrow) {

      $inpsize = $fields[0];
      $sitedsize = $fields[1];
    }

  $cursor->finish();

     $fulldname = $nfspath.$daqfile;
     if($inpsize == $sitedsize ) {

     `rm -f $fulldname`;
     `rm -f $outfile`;

  print "File removed  ", $fulldname, "\n";

     $sql= "update $JobStatusT set inputFileExists = 'removed'  where prodTag = '$prodtg' and inputFileName = '$daqfile' ";

     $rv = $dbh->do($sql) || die $rv." ".$dbh->errstr;

   print "$sql\n";

     }else {
 
	 print "Check files with different sizes ",$daqfile," % ",$inpsize," % ",$sitedsize,"\n";

     $sql= "update $JobStatusT set jobProgress = 'none', jobState = 'none'  where prodTag = '$prodtg' and inputFileName = '$daqfile' ";

     $rv = $dbh->do($sql) || die $rv." ".$dbh->errstr;

   print "$sql\n";

     }
   }
  } 

#######################################################################################

   @statusfile = ();

   @statusfile = `ls *reco_finish`;

  if( scalar(@statusfile) >= 1) {

   foreach my $sline (@statusfile) {
     chop $sline ;
     print $sline, "\n" ;
     $outfile = $statusPath."/".$sline ;
     @wrd = ();
     @wrd = split ("-", $sline);
     $prodtg = $wrd[0];
     $daqfile = $wrd[1].".daq";
     $jbstat = $wrd[2];

   $sql= "update $JobStatusT set jobProgress = '$jbstat' where prodTag = '$prodtg' and inputFileName = '$daqfile' and (jobProgress = 'daq_transferred' or jobProgress = 'none') ";

   $rv = $dbh->do($sql) || die $rv." ".$dbh->errstr;

# print "Check  reco_finish line   ",$outfile, "\n"; 

  `rm -f $outfile`;

  }
}

##########################################################################################


   @statusfile = ();

   @statusfile = `ls *mudst_transferred`;

 if( scalar(@statusfile) >= 1) {

   foreach my $sline (@statusfile) {
     chop $sline ;
     print $sline, "\n" ;
     $outfile = $statusPath."/".$sline ;
     @wrd = ();
     @wrd = split ("-", $sline);
     $prodtg = $wrd[0];
     $daqfile = $wrd[1].".daq";
     $jbstat = $wrd[2];

   $sql= "update $JobStatusT set jobProgress = '$jbstat' where prodTag = '$prodtg' and inputFileName = '$daqfile' and (jobProgress = 'reco_finish' or jobProgress = 'daq_transferred' or jobProgress = 'none' ) ";

   $rv = $dbh->do($sql) || die $rv." ".$dbh->errstr;

# print "Check  mudst status line   ",$outfile, "\n"; 

     `rm -f $outfile`;

   }
 }

##########################################################################

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
