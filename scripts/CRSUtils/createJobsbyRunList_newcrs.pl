#! /usr/local/bin/perl -w
#
# $Id:
# 
# $Log: 
# 
# createJobsbyRunList_newcrs.pl - scripts to create CRS jobs. Use 4 arguments: production tag,
# chainName from ProdOptions table, filename with list of run numbers and stream name
# (use "all" if all stream data should be processed)   
# For example:
# createJobsbyRunList_newcrs.pl P11id auau19.run2011.prod1 auau19_goodruns.list st_physics
# 
#
# Author:  L.Didenko
#
##################################################################################################

use Class::Struct;
use File::Basename;
use Net::FTP;
use lib "/afs/rhic/star/packages/scripts";
use FileCatalog;

use DBI;

$dbhost="duvall.star.bnl.gov";
$dbuser="starreco";
$dbpass="";
$dbname="operation";

$JobStatusT = "JobStatus2015";
$ProdOptionsT = "ProdOptions";

my $prodPeriod = $ARGV[0]; 
my $chName = $ARGV[1];
my $fileName = $ARGV[2]; 
my $ftype = $ARGV[3];

my $datDisk = "/star/data+25-98";             
my $trig;

my $listName = "/star/u/starreco/".$fileName;

my @runList = ();

 open (RUNLIST, $listName ) or die "cannot open $listName: $!\n";

 @runList = <RUNLIST>;

###Set directories to be created for jobfiles

my $DISK1        = "/star/rcf/prodlog/";
my $TOPHPSS_SINK = "/home/starsink/raw/daq";
my $TOPHPSS_RECO = "/home/starreco/reco";
my $JOB_LOG =  $DISK1 . $prodPeriod . "/log/daq" ;
my $JOB_DIR =  "/star/u/starreco/" . $prodPeriod ."/requests/daq"; 

my @jobs_set = ();

 my $jb_news;
 my $jb_archive;
 my $jb_jobfile;
 my $jb_fstat;
 my $jb_joblost;
 my $jb_jobrerun;

########  declare variables needed to fill the JobStatus table

 my $mjobID = "n/a";
 my $mprodSr = "n/a";
 my $mjobFname = "n/a";
 my $mjobFdir = "n/a";
 my $mlogFile = "n/a";
 my $mlogDir = "n/a";
 my $mjobSt = "n/a";
 my $startId = "Job_P00h";
 my $startSer = "P00h";
 my $new_id = 0;
 my $mchain; 
 my $mlibVer; 
 my $hpssSt = 'n/a';
 my $outhpss = 'n/a';

 &StDbProdConnect();

##### insert first line to JobStatusT table get last ID 

    $sql="insert into $JobStatusT set ";    
    $sql.="jobID='$startId',"; 
    $sql.="prodSeries='$startSer'";
     print "$sql\n" if $debugOn;
    $rv = $dbh->do($sql) || die $dbh->errstr;
    $new_id = $dbh->{'mysql_insertid'};

############################################################################


 $sql="SELECT libVersion, chainOpt FROM $ProdOptionsT where prodSeries = '$prodPeriod' AND chainName = '$chName'";

     $cursor =$dbh->prepare($sql)
    || die "Cannot prepare statement: $DBI::errstr\n";
     $cursor->execute;
 
   while(@fields = $cursor->fetchrow) {
     my $cols=$cursor->{NUM_OF_FIELDS};

    for($i=0;$i<$cols;$i++) {
     my $fvalue=$fields[$i];
        my $fname=$cursor->{NAME}->[$i];

      print "$fname = $fvalue\n" if $debugOn;

#      print "$fname = $fvalue\n";

         $mchain   = $fvalue    if( $fname eq 'chainOpt');          
         $mlibVer  = $fvalue    if( $fname eq 'libVersion');   
       }
   }

     &StDbProdDisconnect();


my $SITE = "BNL";

my $fileC = new FileCatalog();

$fileC->connect_as($SITE."::User","FC_user") || die "Connection failed for FC_user\n";

my $jbset;
my @prt = ();
my $flname;
my $jpath;
my $jfile;
my $nfiles = 0;
my $fileSeq;
my $field;
my $myID;
my $mrunId;
my $strName; 
my @flsplit = ();

 &StDbProdConnect();

 for ($ii=0; $ii< scalar(@runList); $ii++)  {

 chop $runList[$ii];
 @jobs_set = ();
 $nfiles = 0; 
 $mrunId = $runList[$ii];

  print "Run number to be processed:  ",$runList[$ii], "\n";
 
  if( $ftype eq "all") {

  $fileC->set_context("runnumber=$runList[$ii]","filetype=online_daq","sanity=1","storage=HPSS","limit=0" );
   }else{
  
  $fileC->set_context("runnumber=$runList[$ii]","filetype=online_daq","filename~$ftype","sanity=1","storage=HPSS","limit=0" );
  }

  @jobs_set = $fileC->run_query("trgsetupname","path","filename","fileseq","magscale");

    $fileC->clear_context();

  foreach my $jobline (@jobs_set){
 
    @flsplit = ();
    @prt = ();
    @prt = split("::",$jobline);

    $trig = $prt[0];
    $jpath  = $prt[1];
    $flname = $prt[2];
    $fileSeq = $prt[3];
    $field = $prt[4]; 
    $jfile = $flname;
    $jfile =~ s/.daq//g;
    @flsplit = split ("_",$jfile);   
    $strName = $flsplit[1];

#    print $trig,"   ",$jpath,"   ",$field,"   ",$flname,"\n";

           $mprodSr = $prodPeriod; 
           $myID = 100000000 + $new_id;
           $mjobID = "Job". $myID . "/" . $prodPeriod ."/". $mlibVer;
           $mlogFile = $jfile . ".log";
           $mlogDir = $JOB_LOG;
           $mjobFdir = "new_jobs";
           $mjobSt = "n/a";
          @parts =  split ("/", $jpath);

    $jbset = $field . "_" .$prodPeriod . "_" . $parts[5] . "_" . $parts[6]. "_" . $parts[7];
   
    $mjobFname = $trig . "_" .$field . "_" .$prodPeriod ."_". $jfile;


   $jb_fstat = 1;
    $jb_news = $JOB_DIR . "/new_jobs/" . $mjobFname;
    $jb_archive = $JOB_DIR . "/archive/" . $mjobFname;
    $jb_jobfile = $JOB_DIR . "/jobfiles/" . $mjobFname;
    $jb_joblost = $JOB_DIR . "/jobs_lostfiles/" . $mjobFname;
    $jb_jobrerun = $JOB_DIR . "/jobs_rerun/" . $mjobFname;

     if (-f $jb_news)     {$jb_fstat = 0};
     if (-f $jb_archive)  {$jb_fstat = 0};
     if (-f $jb_jobfile)  {$jb_fstat = 0};
     if (-f $jb_joblost)  {$jb_fstat = 0};
     if (-f $jb_jobrerun) {$jb_fstat = 0};  

      if($jb_fstat == 1)  {
      
      &create_jobs($jfile, $jbset, $mchain, $mlibVer, $JOB_DIR, $datDisk); 

      print  $mjobFname,"  ", $nfiles, "\n";
      $nfiles++;
 
#	  print $mjobID," % ", $mprodSr," % ", $trig ," % ",$mjobFname," % ",$strName," % ",$mrunId," % ",$mjobFdir," % ",$mlogFile," % ",$mlogDir," % ",$hpssSt," % ",$outhpss," % ",$mjobSt," % ",$chName, "\n";

    &fillJSTable();   

     }
 }
}
##### delete from $JobStatusT inserted JobID

     $sql="delete from $JobStatusT WHERE ";    
     $sql.="jobID='$startId' AND "; 
     $sql.="prodSeries='$startSer'";
      print "$sql\n" if $debugOn;
     $rv = $dbh->do($sql) || die $dbh->errstr;

#### finished with data base
    &StDbProdDisconnect();

 $fileC->destroy();

   exit;

################################################################################
  sub fillJSTable {

    $sql="insert into $JobStatusT set ";
    $sql.="jobID='$mjobID',";
    $sql.="prodSeries='$mprodSr',";
    $sql.="trigsetName='$trig',";
    $sql.="jobfileName='$mjobFname',";
    $sql.="streamName='$strName',";
    $sql.="runID='$mrunId',";
    $sql.="jobfileDir='$mjobFdir',";
    $sql.="logfileName='$mlogFile',";
    $sql.="logfileDir='$mlogDir',";
    $sql.="inputHpssStatus='$hpssSt',";
    $sql.="outputHpssStatus='$outhpss',";
    $sql.="jobStatus='$mjobSt',";
    $sql.="chainName='$chName'";

     print "$sql\n" if $debugOn;
    $rv = $dbh->do($sql) || die $dbh->errstr;
    $new_id = $dbh->{'mysql_insertid'};     
  
    }

#############################################################################
##### create jobfiles to get default set of output files

 sub create_jobs {

  my ($gfile, $Jset, $fchain, $jlibVer, $JobDir, $dataDisk ) = @_ ;

 my $Jsetd;
 my $Jsetr;
 my $inFile;
 my @pts = ();

    @pts = split ("_",$Jset);
    $Jsetr = $pts[2]."/".$pts[3]."/".$pts[4];
    $Jsetd = $trig."/".$pts[0]."/".$pts[1]."/".$Jsetr;     

    $inFile = $gfile . ".daq";

   my $exArg = "4,".$jlibVer ."," .$dataDisk . ",-1," . $fchain;
  
    my $jb_new = $JobDir . "/jobfiles/" . $trig."_".$pts[0]."_".$pts[1]."_".$gfile;

      my $hpss_raw_dir  = $TOPHPSS_SINK . "/" . $Jsetr;
      my $hpss_raw_file = $inFile;
      my $hpss_dst_dir  = $TOPHPSS_RECO . "/" . $Jsetd;
      my $hpss_dst_file0 = $gfile . ".MuDst.root";
      my $hpss_dst_file1 = $gfile . ".hist.root";
      my $hpss_dst_file2 = $gfile . ".tags.root";
      my $hpss_dst_file3 = $gfile . ".event.root";
#      my $hpss_dst_file4 = $gfile . ".runco.root";    
      my $executable     = "/afs/rhic.bnl.gov/star/packages/scripts/bfcca";
      my $execargs = $exArg;
         $execargs =~ s/,/ /g;
      my $log_name      = $JOB_LOG."/".$gfile .".log";
      my $err_name      = $JOB_LOG."/".$gfile .".err";

      if (!open (JOB_FILE,">$jb_new")) {printf ("Unable to create job submission script %s\n",$jb_new);}


       print JOB_FILE "                \n";
       print JOB_FILE "[output-0]\n";
       print JOB_FILE "path = $hpss_dst_dir\n";
       print JOB_FILE "type = HPSS\n";
       print JOB_FILE "file = $hpss_dst_file0\n";
       print JOB_FILE "                \n";
       print JOB_FILE "[output-1]\n";
       print JOB_FILE "path = $hpss_dst_dir\n";
       print JOB_FILE "type = HPSS\n";
       print JOB_FILE "file = $hpss_dst_file1\n";
       print JOB_FILE "                \n";
       print JOB_FILE "[output-2]\n";
       print JOB_FILE "path = $hpss_dst_dir\n";
       print JOB_FILE "type = HPSS\n";
       print JOB_FILE "file = $hpss_dst_file2\n";
       print JOB_FILE "               \n"; 
       print JOB_FILE "[exec-0]\n";
       print JOB_FILE "args = $execargs\n";
       print JOB_FILE "gzip_output = True\n";
       print JOB_FILE "stdout = $log_name\n";
       print JOB_FILE "stderr = $err_name\n";
       print JOB_FILE "exec = $executable\n";
       print JOB_FILE "              \n"; 
       print JOB_FILE "[main]\n";
       print JOB_FILE "num_inputs = 1\n";
       print JOB_FILE "num_outputs = 3\n";
       print JOB_FILE "queue = default\n";
       print JOB_FILE "              \n"; 
       print JOB_FILE "[input-0]\n";
       print JOB_FILE "path = $hpss_raw_dir\n";
       print JOB_FILE "type = HPSS\n";
       print JOB_FILE "file = $hpss_raw_file\n";
       print JOB_FILE "              \n"; 
 
 
     close(JOB_FILE);

 }

#==============================================================================

######################
sub StDbProdConnect {
    $dbh = DBI->connect("dbi:mysql:$dbname:$dbhost", $dbuser, $dbpass)
        || die "Cannot connect to db server $DBI::errstr\n";
}

######################
sub StDbProdDisconnect {
    $dbh = $dbh->disconnect() || die "Disconnect failure $DBI::errstr\n";
}

###################################################################################

