#!/usr/local/bin/perl
#
# $Log:
#
# L.Didenko
#
# ProdTrigSummary.pl - script to fillin ProdTriggerSet table reading triggerID data from RunLog.l0TriggerSet table for
# every runnumber processed in production (processed runnumber are taken from Filecatalog); uses trigger set name
# and production tag as arguments.
# 
###########################################################################################################################


use lib "/afs/rhic.bnl.gov/star/packages/scripts";
use FileCatalog;

use DBI;
use File::Basename;
use Time::Local;

$dbhost="duvall.star.bnl.gov";
$dbuser="starreco";
$dbpass="";
$dbname="operation";

my $TrigDataT = "ProdTriggerSet";

$dbhosto="dbbak.starp.bnl.gov:3411";
$dbusero="starreco";
$dbpasso="";
$dbnameo="RunLog";

my $OnlineTrigT = "l0TriggerSet";

#my $OnlineTrigT = "triggerSet";

my $debugOn=0;

my $trgname = $ARGV[0];
my $prod = $ARGV[1];


($sec,$min,$hour,$mday,$mon,$year) = localtime();

my $mon =  $mon + 1;

if( $mon < 10) { $mon = '0'.$mon };
if( $mday < 10) { $mday = '0'.$mday };
if( $hour < 10) { $hour = '0'.$hour };
if( $min < 10) { $min = '0'.$min };
if( $sec < 10) { $sec = '0'.$sec };


my $todate = ($year+1900)."-".$mon."-".$mday." ".$hour.":".$min.":".$sec;

my $nowdate = ($year+1900).$mon.$mday;

########################

my $SITE         = "BNL";
my $status       = (0==1);


my $fileC = new FileCatalog();

    $fileC->connect_as($SITE."::User","FC_user") || die "Connection failed for FC_user\n";

my @trigset = ();
my @runnum = ();
my @trglable = ();
my @trgevents = ();
my @daqTrig = ();
my @offTrgId = ();
my @presc = ();
my @prodset = ();
my @dbrunnum = () ;


my $nk = 0;
my $ik = 0;
my $tflag = 0;
my @prt = ();
my $nlist = 0;
my $nset = 0;
my $nn = 0;


 $fileC->set_context("trgsetupname=$trgname","production=$prod","filetype=daq_reco_MuDst","storage=hpss","limit=0");

  @prodset = $fileC->run_query("trgsetupname","orda(runnumber)");

  $fileC->clear_context( );

  $fileC->destroy();

    &StDbOprConnect();

   $sql="SELECT distinct runNumber  FROM  $TrigDataT WHERE  trigSetName='$trgname' and prodTag='$prod' "; 

      $cursor =$dbh->prepare($sql)
          || die "Cannot prepare statement: $DBI::errstr\n";
       $cursor->execute();

       while( $runm = $cursor->fetchrow() ) {
          $dbrunnum[$nk] = $runm;
          $nk++; 
  
       }
    $cursor->finish();

    &StDbOprDisconnect();

   foreach my $line (@prodset){

#   print "Check FileCatalog query  ", $line, "\n"; 

    @prt = (); 
    @prt = split("::",$line); 

    $trigset[$nlist] = $prt[0];
    $runnum[$nlist] = $prt[1];  

   $nset = 0;

 $dbh = DBI->connect("dbi:mysql:$dbnameo:$dbhosto", $dbusero, $dbpasso )
        || die "Cannot connect to db server $DBI::errstr\n";


   $sql="SELECT name, HEX(1<<daqTriggerId) AS daqTrgId, offlineTriggerId, TRUNCATE(prescale, 1) prescale, numberOfEvents  FROM $OnlineTrigT  WHERE runNumber = '$runnum[$nlist]' order by offlineTriggerId ";

#  $sql="SELECT name, HEX(1<<daqTriggerId) AS daqTrgId, offlineTriggerId, numberOfEvents  FROM $OnlineTrigT  WHERE runNumber = '$runnum[$nlist]' order by offlineTriggerId ";

    $cursor =$dbh->prepare($sql)
   || die "Cannot prepare statement: $DBI::errstr\n";
           $cursor->execute;
 
     while(@fields = $cursor->fetchrow) {
      my $cols=$cursor->{NUM_OF_FIELDS};

       for($i=0;$i<$cols;$i++) {

        $trglable[$nlist,$nset] = $fields[0] ;
        $daqTrig[$nlist,$nset] = $fields[1];
        $offTrgId[$nlist,$nset] = $fields[2];
        $presc[$nlist,$nset] = $fields[3];
        $trgevents[$nlist,$nset] = $fields[4];
      }

      print  "Check dataset, trigger data  ",$nlist,"  %  ",$nset,"  %  ",$trglable[$nlist,$nset],"  %  ",$offTrgId[$nlist,$nset],"  %  ",$trgevents[$nlist,$nset], "\n";

    for($ik=0; $ik<$nk; $ik++ ) {

    if($runnum[$nlist] == $dbrunnum[$ik] ) {
    $tflag = 1 ;
    print "Runnumber found  ",$runnum[$nlist],"   %   ",$dbrunnum[$ik],  "\n";

goto GO_SKIP;    

     }else{
     $tflag = 0 ;    
    }
   }

     if( $tflag == 0 ) {

    &StDbOprConnect();

    &fillTrigTable();

    &StDbOprDisconnect();
    }

GO_SKIP:

     $nset++; 
   }
      $cursor->finish;

     $nlist++;
  }

#$dbh = $dbh->disconnect("dbi:mysql:$dbnameo:$dbhosto",) || die "Disconnect failure $DBI::errstr\n";

#################################################################################### 
 
 exit;

#######################################################################################

  sub fillTrigTable {

   $sql="insert into $TrigDataT set ";
   $sql.="trigSetName='$trigset[$nlist]',";
   $sql.="prodTag='$prod',";
   $sql.="runNumber='$runnum[$nlist]',";
   $sql.="trigLabel='$trglable[$nlist,$nset]',";
   $sql.="daqTrgId='$daqTrig[$nlist,$nset]',";
   $sql.="offlineTrgId='$offTrgId[$nlist,$nset]',";
   $sql.="Nevents='$trgevents[$nlist,$nset]',";   
   $sql.="prescale='$presc[$nlist,$nset]' ";
   print "$sql\n" if $debugOn;
 print "$sql\n";
   $rv = $dbh->do($sql) || die $dbh->errstr;
   }

#==============================================================================

######################
sub StDbOprConnect {
    $dbh = DBI->connect("dbi:mysql:$dbname:$dbhost",$dbuser, $dbpass )
        || die "Cannot connect to db server $DBI::errstr\n";
}

######################
sub StDbOprDisconnect {
    $dbh = $dbh->disconnect() || die "Disconnect failure $DBI::errstr\n";
}

###################################################################################
