#! /usr/local/bin/perl -w
#
# $Id:
# 
# $Log: 
# 
# createJobs.FC_calib.pl  
# L.Didenko
# script to create jobfiles to processed with calibration productions for some sybsystem.
# Script requires 5  arguments: production Series, chain name, subsystem calib tag,
# stream name ('all' if no selection), filename with list of runnumbers
# 
# 
##################################################################################################

use Mysql;
use Class::Struct;
use File::Basename;
use DBI;
use lib "/afs/rhic/star/packages/scripts";
use FileCatalog;


$dbhost="duvall.star.bnl.gov";
$dbuser="starreco";
$dbpass="";
$dbname="operation";

$ProdOptionsT = "ProdOptions";
$JobStatusT = "CalibJobStatus";

my $debugOn=0;
          
my $prodPeriod = $ARGV[0]; 
my $trig; 
my $chName = $ARGV[1];
my $tgcalib = $ARGV[2];
my $ftype = $ARGV[3];
my $fileName = $ARGV[4];
my $datDisk = "/star/data+15-16";

my @runSet = (); 

 my $listName = "/star/u/starreco/".$fileName;

 open (RUNLIST, $listName ) or die "cannot open $listName: $!\n";

 @runSet = <RUNLIST>;

###Set directories to be created for jobfiles

my $TOPHPSS_SINK = "/home/starsink/raw/daq";
my $TOPHPSS_RECO = "/home/starreco/reco";
my $JOB_LOG =  "/star/rcf/prodlog/" . $prodPeriod . "_calib/log/daq";

my $JOB_DIR =  "/star/u/starreco/" . $prodPeriod ."/requests/daq";;

#####  connect to production DB

 &StDbProdConnect();
 
 my $jb_archive;
 my $jb_jobfile;
 my $jb_fstat;

########  declare variables needed to fill the JobStatus table

 my $mflName = "n/a";
 my $mprodSr = "n/a";
 my $mjobFname = "n/a";
 my $mlogFile = "n/a";
 my $mlogDir = "n/a";
 my $mjobSt = "n/a";
 my $mNev  = 0;
 my $mchName = "n/a";
 my $strName;
 my $hpssSt = 'n/a';
 my $outSt = 'n/a';
# my $tgcalib = "TOF/dEdx";


#############################################################################

 $JOB_DIR =  "/star/u/starreco/" . $prodPeriod ."/requests/daq"; 

 my $mchain; 
 my $mlibVer; 
 my $mNikName;

 $sql="SELECT prodSeries, libVersion, chainOpt, chainName  FROM $ProdOptionsT where prodSeries = '$prodPeriod' AND chainName = '$chName'";

     $cursor =$dbh->prepare($sql)
    || die "Cannot prepare statement: $DBI::errstr\n";
     $cursor->execute;
 
   while(@fields = $cursor->fetchrow) {
     my $cols=$cursor->{NUM_OF_FIELDS};

    for($i=0;$i<$cols;$i++) {
     my $fvalue=$fields[$i];
       my $fname=$cursor->{NAME}->[$i];

      print "$fname = $fvalue\n" if $debugOn;
#     print "$fname = $fvalue\n" ;
         $mchain   = $fvalue    if( $fname eq 'chainOpt');          
         $mlibVer  = $fvalue    if( $fname eq 'libVersion');  
         $mNikName = $fvalue    if( $fname eq 'chainName'); 
       }
   } 

    &StDbProdDisconnect();

my $SITE = "BNL";

my $fileC = new FileCatalog();

my ($user,$passwd) = $fileC->get_connection($SITE."::Admin");

if ( ! defined($user) ){  $user  = "";}
if ( ! defined($passwd) ){$passwd= "";}

if ( $user eq ""){
    print "Password : ";
    chomp($passwd = <STDIN>);
    $fileC->connect_as($SITE."::Admin","FC_admin",$passwd) || die "Connection failed for FC_admin\n";
} else {
    if ( $passwd eq "" ){
        print "Password for $user : ";
        chomp($passwd = <STDIN>);
    }
    $fileC->connect_as($SITE."::Admin",$user,$passwd)      || die "Connection failed for $user\n";
}


my $nfiles = 0;
my $mrunId;
my @jobs_set = ();
my $fileSeq;
my $field;
my @prt = ();
my @prts = ();
my $jpath;
my $jfile;
my $flname;
my $chain;
my $jbset;

 for ($ii=0; $ii< scalar(@runSet); $ii++)  {

   chop  $runSet[$ii];
   print $runSet[$ii], "\n";

 @jobs_set = ();
 $nfiles = 0;
 $mrunId = $runSet[$ii];

 if( $ftype eq "all") {

  $fileC->set_context("runnumber=$runSet[$ii]","filetype=online_daq","sanity=1","storage=HPSS");
   }else{

  $fileC->set_context("runnumber=$runSet[$ii]","filetype=online_daq","filename~$ftype","sanity=1","storage=HPSS");
   }

  @jobs_set = $fileC->run_query("trgsetupname","path","filename","runnumber","fileseq","magscale");

    $fileC->clear_context();

 &StDbProdConnect();

 foreach my $jobline (@jobs_set){

    @prt = ();
    @prt = split("::",$jobline);

    $trig = $prt[0];
    $jpath  = $prt[1];
    $flname = $prt[2];
    $fileSeq = $prt[3];
    $mrunId = $prt[4];
    $field = $prt[5];
    $jfile = $flname;
    $jfile =~ s/.daq//g;
    @prts = ();
    @prts = split ("_",$jfile);
    $strName = $prts[1];

    $mprodSr = $prodPeriod; 
    $mflName = $flname;       
    $mlogFile = $jfile . ".log";
    $mlogDir = $JOB_LOG;
    $mjobSt = "n/a";
    $mchName = $mNikName;
    $chain = $mchain;
    $outSt =  "n/a";
    @prts = ();
    @prts =  split ("/", $jpath);

    $jbset = $field . "_" .$prodPeriod . "_" . $prts[5] . "_" . $prts[6]. "_" . $prts[7];
   
    $mjobFname = $trig . "_" .$field . "_" .$prodPeriod ."_".$tgcalib."_". $jfile;

    $jb_fstat = 1;
    $jb_archive = $JOB_DIR . "/archive_calib/" . $mjobFname;
    $jb_jobfile = $JOB_DIR . "/jobs_calib/" . $mjobFname;

     if (-f $jb_archive)  {$jb_fstat = 0};
     if (-f $jb_jobfile)  {$jb_fstat = 0};

      if($jb_fstat == 1)  {
      
     &create_jobs($jfile, $jbset, $chain, $mlibVer, $datDisk); 

      print  $mjobFname,  "\n";
      $nfiles++;
#####  fill  JobStatus table
      print "filling JobStatus table\n";
 
     &fillJSTable();   

     }
   }
}

#### finished with data base
    &StDbProdDisconnect();

   exit;

################################################################################
  sub fillJSTable {

    $sql="insert into $JobStatusT set ";
    $sql.="prodSeries='$mprodSr',";
    $sql.="trigsetName='$trig',";
    $sql.="calibtag='$tgcalib',";
    $sql.="runID='$mrunId',";
    $sql.="jobfileName='$mjobFname',";
    $sql.="streamName='$strName',";
    $sql.="logfileName='$mlogFile',";
    $sql.="logfileDir='$mlogDir',";
    $sql.="inputHpssStatus='$hpssSt',";
    $sql.="outputStatus='$outSt',";
    $sql.="jobStatus='$mjobSt',"; 
    $sql.="NoEvents='$mNev',";     
    $sql.="chainName='$mchName'";

     print "$sql\n" if $debugOn;
    $rv = $dbh->do($sql) || die $dbh->errstr;
    $new_id = $dbh->{'mysql_insertid'};     
  
    }


#############################################################################
##### create jobfiles to get default set of output files

 sub create_jobs {

  my ($gfile, $Jset, $fchain, $jlibVer, $dataDisk ) = @_ ;

 my $Jsetd;
 my $Jsetr;
 my $inFile;
 my @pts = ();

  @pts = split ("_",$Jset);
  $Jsetr = $pts[2] . "/" .$pts[3]. "/" .$pts[4];
  $Jsetd = $trig . "/" .$pts[0] . "/" . $pts[1] . "_".$tgcalib."/" .$Jsetr; 

  $inFile =  $gfile . ".daq";

#  my $exArg = "29,".$jlibVer ."," .$dataDisk . ",4000," . $fchain;

  my $exArg = "29,".$jlibVer ."," .$dataDisk . ",-1," . $fchain;  

##### print $job_set, "\n";
 
  my $jb_jobfile = $JOB_DIR . "/jobs_calib/".$trig."_".$pts[0]. "_".$pts[1]."_".$tgcalib."_".$gfile;  

      my $hpss_raw_dir  = $TOPHPSS_SINK . "/" . $Jsetr;
      my $hpss_raw_file = $inFile;
      my $hpss_dst_dir  = $TOPHPSS_RECO . "/" . $Jsetd;
      my $hpss_dst_file0 = $gfile . ".MuDst.root";
      my $hpss_dst_file1 = $gfile . ".hist.root";
      my $hpss_dst_file2 = $gfile . ".tags.root";
      my $hpss_dst_file3 = $gfile . ".event.root";    
      my $executable     = "/afs/rhic/star/packages/scripts/bfcca";
      my $executableargs = $exArg; 
      my $log_dir       = $JOB_LOG;
      my $log_name      = $gfile . ".log";
      my $err_log       = $gfile . ".err";
      if (!open (JOB_FILE,">$jb_jobfile")) {printf ("Unable to create job submission script %s\n",$jb_jobfile);}
        print JOB_FILE "mergefactor=1\n";


       print JOB_FILE "#input\n";
       print JOB_FILE "      inputnumstreams=1\n";
       print JOB_FILE "      inputstreamtype[0]=HPSS\n";
       print JOB_FILE "      inputdir[0]=$hpss_raw_dir\n";
       print JOB_FILE "      inputfile[0]=$hpss_raw_file\n";
#       print JOB_FILE "      inputstreamtype[1]=UNIX\n";
#       print JOB_FILE "      inputdir[1]=/star/data16/reco/StarDb\n";
#       print JOB_FILE "      inputfile[1]=OneSpaceCharge\n";
#       print JOB_FILE "      inputfile[1]=DUMMY\n";
       print JOB_FILE "#output\n";
       print JOB_FILE "      outputnumstreams=3\n";
       print JOB_FILE "#output stream \n";
       print JOB_FILE "      outputstreamtype[0]=HPSS\n";
       print JOB_FILE "      outputdir[0]=$hpss_dst_dir\n";
       print JOB_FILE "      outputfile[0]=$hpss_dst_file0\n";
       print JOB_FILE "      outputstreamtype[1]= HPSS\n";
       print JOB_FILE "      outputdir[1]=$hpss_dst_dir\n";
       print JOB_FILE "      outputfile[1]=$hpss_dst_file1\n";
       print JOB_FILE "      outputstreamtype[2]=HPSS\n";
       print JOB_FILE "      outputdir[2]=$hpss_dst_dir\n";
       print JOB_FILE "      outputfile[2]=$hpss_dst_file2\n";
#       print JOB_FILE "      outputstreamtype[3]=HPSS\n";
#       print JOB_FILE "      outputdir[3]=$hpss_dst_dir\n";
#       print JOB_FILE "      outputfile[3]=$hpss_dst_file3\n";
       print JOB_FILE "#standard out -- Should be four outputs\n";
       print JOB_FILE "      stdoutdir=$log_dir\n";
       print JOB_FILE "      stdout=$log_name\n";
       print JOB_FILE "#standard error -- Should be five\n";
       print JOB_FILE "      stderrdir=$log_dir\n";
       print JOB_FILE "      stderr=$err_log\n";
       print JOB_FILE "      notify=starreco\@rcrsuser1.rcf.bnl.gov\n";
       print JOB_FILE "#program to run\n";
       print JOB_FILE "      executable=$executable\n";
       print JOB_FILE "      executableargs=$executableargs\n";
 
     close(JOB_FILE);

 }


######################
sub StDbProdConnect {
    $dbh = DBI->connect("dbi:mysql:$dbname:$dbhost", $dbuser, $dbpass)
        || die "Cannot connect to db server $DBI::errstr\n";
}


######################
sub StDbProdDisconnect {
    $dbh = $dbh->disconnect() || die "Disconnect failure $DBI::errstr\n";
}
