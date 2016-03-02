#!/usr/local/bin/perl
#
# $Log:
#
# L.Didenko
#
# EmbedSumUpdate.pl - script to fillin EmbedSummary table from FileCatalog; could use trigger set name
# as argument to process only one dataset (triggerset)
# 
################################################################################################


use lib "/afs/rhic.bnl.gov/star/packages/scripts";
use FileCatalog;

use DBI;
use File::Basename;
use Time::Local;

$dbhost="duvall.star.bnl.gov";
$dbuser="starreco";
$dbpass="";
$dbname="operation";

my $EmbedSumT = "EmbedSummary";
my $ProddataT = "ProductionChains";

my $trigname;

  if ( defined($ARGV[0]) ) {
      $trigname = $ARGV[0];
  }else{
      $trigname = "none";
  }


my $debugOn=0;


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

   &StDbOprConnect();
  
########################

my $SITE         = "BNL";
my $status       = (0==1);


my $fileC = new FileCatalog();

    $fileC->connect_as($SITE."::User","FC_user") || die "Connection failed for FC_user\n";

my @coll = ();
my @trig = ();
my @prod = ();
my @libtg = ();
my @coll = ();
my @yearDt = ();
my @sumevt = ();
my @prodset = ();
my @pathname = ();
my @runevents = ();
my @sumsize = ();
my @datasize = ();
my @filelst = ();
my @embpath = ();
my @embpatt = ();
my @partcl = ();
my @reqst = ();
my @nfset = ();
my @prt = ();
my $nline = 0;
my $nlist = 0;
my $nset = 0;
my $nn = 0;
my $ssize = 0;
my $dsize  = 0;
my @numfiles = ();
my @embsubd = ();
my $rsite = "BNL";
my $eflag = 0;


  if( $trigname eq "none") {

 $fileC->set_context("filetype=daq_embedding_MuDst","storage=hpss","limit=0");

  }else{

# $fileC->set_context("trgsetupname=$trigname","path~UU_production_2012/Proton_1","filetype=daq_embedding_MuDst","storage=hpss","limit=0");
  $fileC->set_context("trgsetupname=$trigname","filetype=daq_embedding_MuDst","storage=hpss","limit=0");

 }
  @prodset = $fileC->run_query("trgsetupname","ordd(production)","library");

 $fileC->clear_context( );


    foreach my $line (@prodset){

   print $line, "\n"; 

    @prt = (); 
    @prt = split("::",$line); 

    $trig[$nlist] = $prt[0];
    $prod[$nlist] = $prt[1];  
    $libtg[$nlist] = $prt[2];  

   print $trig[$nlist],"  %  ",$prod[$nlist],"  %  ",$libtg[$nlist], "\n";


   $sql="SELECT distinct collision, yearData FROM $ProddataT  WHERE trgsetName = '$trig[$nlist]' ";

    $cursor =$dbh->prepare($sql)
   || die "Cannot prepare statement: $DBI::errstr\n";
           $cursor->execute;
 
     while(@fields = $cursor->fetchrow) {
      my $cols=$cursor->{NUM_OF_FIELDS};

       for($i=0;$i<$cols;$i++) {

        $coll[$nlist] = $fields[0] ;
        $yearDt[$nlist] = $fields[1];

      }
    }
      $cursor->finish;

  print $trig[$nlist],"  %  ",$coll[$nlist],"  %  ",$yearDt[$nlist],"  %  ",$prod[$nlist],"  %  ",$libtg[$nlist], "\n";

     @pathname = ();

    $fileC->set_context("trgsetupname=$trig[$nlist]","production=$prod[$nlist]","filetype=daq_embedding_MuDst","storage=hpss","sanity=1","limit=0");

    @pathname = $fileC->run_query("path");
 
    $fileC->clear_context();
    $nset = 0;
    $nn = 1;
    $embpatt = "none"; 
    @sumevt  = 0;
    @numfiles = 0;
    @sumsize  = 0;
    @embsubd = ();
    @embpatt = ();
    @embpath = ();
    $eflag = 0;
   $embsubd[0] = "none";

   foreach my $mpath (@pathname){

#       print $mpath, "\n";

    @prt = (); 
    @prt = split("/",$mpath); 

############ NOT RIGHT

# print "Embedding subdir  ",$prt[5] , "\n"; 

    $embsubd[$nn] = $prt[5];

    for ($ii =0; $ii < $nn; $ii++ ) {

#     print "Embed subdir   ", $ii,"  %  ",$nn,"  %  ", $embsubd[$ii], "\n";

    if ($prt[5] eq $embsubd[$ii] ) { 
    $eflag = 0;

    goto GO_NEXT;

    }else{
    $eflag = 1;
    }
   }
    $nn++;


    if($eflag == 1) {

    $embpatt[$nset] = $prt[5];
    print "Embedding unique subdir:  ",$nset,"  %  ",$embpatt[$nset], "\n";
    @prt = (); 
    @prt = split("_",$embpatt[$nset]); 
    $partcl[$nset] = $prt[0];
    $reqst[$nset] = $prt[2];
    $nfset[$nset] = $prt[1];

    
    print "Embedding parameters:  ",$embpatt[$nset],"  %  ", $partcl[$nset],"  %  ",$nfset[$nset],"  %  ",$reqst[$nset],"\n";
 
     @runevents = ();
     $runevents[0] = 0;  
     @datasize = ();
     $datasize[0] = 0; 
     @filelst = ();

    $embpath[$nset] = $trig[$nlist]."/".$embpatt[$nset];

    $fileC->set_context("trgsetupname=$trig[$nlist]","production=$prod[$nlist]","path~$embpath[$nset]","filetype=daq_embedding_MuDst","storage=hpss","limit=0");
 
      @runevents = $fileC->run_query("sum(events)");
      @filelst = $fileC->run_query("path","filename");

      $sumevt[$nset] = $runevents[0];
      $numfiles[$nset] = scalar(@filelst);

      $fileC->clear_context();

    $fileC->set_context("trgsetupname=$trig[$nlist]","production=$prod[$nlist]","path~$embpath[$nset]","filetype~daq_embedding","storage=hpss");

      @datasize = $fileC->run_query("sum(size)"); 
      $sumsize[$nset] = $datasize[0];

      $fileC->clear_context();

#######################
    

    print "Path = ",$embpath[$nset], "  sum events = ",$sumevt[$nset],"  %  ","Total size = ",$sumsize[$nset],"  %  ","Number of MuDst = ",$numfiles[$nset], "\n";

#######################   


 $sql="SELECT distinct id FROM $EmbedSumT  WHERE trigName = '$trig[$nlist]' AND  prodTag = '$prod[$nlist]' AND libTag = '$libtg[$nlist]' AND requestID = '$reqst[$nset]' AND particleID = '$partcl[$nset]' AND fSet = '$nfset[$nset]' ";

    $cursor =$dbh->prepare($sql)
   || die "Cannot prepare statement: $DBI::errstr\n";
           $cursor->execute;

     my $idset = $cursor->fetchrow ;
     $cursor->finish; 

    if( !defined $idset ) {

   &fillEmbedTable();

   }else{
   }

###############
     }
   $nset++;
     GO_NEXT:

 }

  $nlist++; 

  }

#################################################################################### 
 
   &StDbOprDisconnect();


   $fileC->destroy();

#######################################################################################

  sub fillEmbedTable {

   $sql="insert into $EmbedSumT set ";
   $sql.="trigName='$trig[$nlist]',";
   $sql.="collision='$coll[$nlist]',";
   $sql.="yearData='$yearDt[$nlist]',";
   $sql.="prodTag='$prod[$nlist]',";
   $sql.="libTag='$libtg[$nlist]',";
   $sql.="requestID='$reqst[$nset]',";
   $sql.="particleID='$partcl[$nset]',";
   $sql.="fSet='$nfset[$nset]',";
   $sql.="Nevents='$sumevt[$nset]',";   
   $sql.="totsize='$sumsize[$nset]',";
   $sql.="Nmufiles='$numfiles[$nset]',";  
   $sql.="site='$rsite' ";
   print "$sql\n" if $debugOn;
# print "$sql\n";
   $rv = $dbh->do($sql) || die $dbh->errstr;
   }



#==============================================================================

######################
sub StDbOprConnect {
    $dbh = DBI->connect("dbi:mysql:$dbname:$dbhost", $dbuser, $dbpass)
        || die "Cannot connect to db server $DBI::errstr\n";
}

######################
sub StDbOprDisconnect {
    $dbh = $dbh->disconnect() || die "Disconnect failure $DBI::errstr\n";
}

###################################################################################
