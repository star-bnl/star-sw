#!/usr/local/bin/perl
#
# $Log:
#
# L.Didenko
#
# ProdTrigSummary_stream.pl - script to fillin ProdTriggers table with summary of triggerID for stream data 
# reading triggerID data from RunLog.l0TriggerSet table for every runnumber and stream  processed in production
# (processed runnumber are taken from FileCatalog); uses trigger set name, production tag and stream name as arguments.
# 
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

my $TrigDataT = "ProdTriggers";

#my $TrigDataT = "ProdTriggerSet";

#$dbhosto="dbbak.starp.bnl.gov:3411";
$dbhosto="dbbak.starp.bnl.gov:3413";
#$dbhosto="dbbak.starp.bnl.gov:3501";
#$dbhosto="dbbak.starp.bnl.gov:3412";
#$dbhosto="db04.star.bnl.gov:3412";
$dbusero="starreco";
$dbpasso="";
$dbnameo="RunLog";

my $OnlineTrigT = "l0TriggerSet";

#my $OnlineTrigT = "triggerSet";

my $debugOn=0;

my $trgname = $ARGV[0];
my $prod = $ARGV[1];
my $nmstream = $ARGV[2];

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
my @trgstream = ();
my @trgnm = ();
my @dbstream = ();


my $nk = 0;
my $ik = 0;
my $tflag = 0;
my @prt = ();
my $nlist = 0;
my $nset = 0;
my $nn = 0;


 $fileC->set_context("trgsetupname=$trgname","production=$prod","filetype=daq_reco_MuDst","filename~$nmstream","storage=hpss","limit=0");

  @prodset = $fileC->run_query("trgsetupname","orda(runnumber)");

  $fileC->clear_context( );

  $fileC->destroy();

    &StDbOprConnect();

   $sql="SELECT distinct runNumber, streamName  FROM  $TrigDataT WHERE  trigSetName='$trgname' and prodTag='$prod' and  streamName = '$nmstream' "; 

######## query for filling with streamname if not all streams are processed in production

#   $sql="SELECT distinct runNumber  FROM  $TrigDataT WHERE  trigSetName='$trgname' and prodTag='$prod' "; 

      $cursor =$dbh->prepare($sql)
          || die "Cannot prepare statement: $DBI::errstr\n";
       $cursor->execute();

     while(@fields = $cursor->fetchrow) {
      my $cols=$cursor->{NUM_OF_FIELDS};

       for($i=0;$i<$cols;$i++) {
       $dbrunnum[$nk] = $fields[0];
       $dbstream[$nk] = $fields[1];
       $nk++; 
      }
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

   $sql="SELECT distinct f.name, f.offlineTriggerId, f.numberOfEvents, TRUNCATE(f.prescale, 1) as prescale, HEX(1<<f.daqTriggerId) AS daqTrgId, a.dataStreamNames, b.name  FROM `RunLog`.`l0TriggerSet` f, `Conditions_rts`.`dataStreamNames` a, `Conditions_rts`.`triggers` b, `Conditions_rts`.`run` c  WHERE  c.dataStreamNamesHash=a.hash AND c.idx_rn = b.idx_rn AND a.idxream=b.dataStream AND f.runNumber=b.idx_rn and  b.idx_rn = '$runnum[$nlist]' and  f.name=b.name and a.dataStreamNames = '$nmstream' order by f.offlineTriggerId ";


    $cursor =$dbh->prepare($sql)
   || die "Cannot prepare statement: $DBI::errstr\n";
           $cursor->execute;
 
     while(@fields = $cursor->fetchrow) {
      my $cols=$cursor->{NUM_OF_FIELDS};

       for($i=0;$i<$cols;$i++) {

        $trglable[$nlist,$nset] = $fields[0] ;
        $offTrgId[$nlist,$nset] = $fields[1];
        $trgevents[$nlist,$nset] = $fields[2];
        $presc[$nlist,$nset] = $fields[3];
        $daqTrig[$nlist,$nset] = $fields[4];
        $trgstream[$nlist,$nset] = $fields[5];
        $trgnm[$nlist,$nset] = $fields[6] ;


      }

#      print  "Check dataset, trigger data  ",$nlist,"  %  ",$nset,"  %  ",$trglable[$nlist,$nset],"  %  ",$offTrgId[$nlist,$nset],"  %  ",$trgevents[$nlist,$nset],"  %  ",$trgstream[$nlist,$nset],"  %  ",$trgnm[$nlist,$nset],"  %  ",$presc[$nlist,$nset],"\n";


   for($ik=0; $ik<$nk; $ik++ ) {

    if($runnum[$nlist] == $dbrunnum[$ik] and $nmstream == $dbstream[$ik] ) {

    $tflag = 1 ;
    print "Runnumber and streamname found  ",$runnum[$nlist],"   %   ",$dbrunnum[$ik],"   %   ", $nmstream, "\n";

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
   $sql.="streamName='$trgstream[$nlist,$nset]',";
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
