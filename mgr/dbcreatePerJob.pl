#! /opt/star/bin/perl -w
#
# 
# 
#  
# 
# dbcreatePerJob.pl  
# L.Didenko
# script to create jobfiles and JobID if input file should be splited
# and fill in JobStatus and jobRelations tables 
##########################################################################################

use Mysql;
use Class::Struct;
use File::Basename;
use File::Find;
use Net::FTP;

require "/afs/rhic/star/packages/DEV00/mgr/dbCpProdSetup.pl";

my $debugOn=0;

my @Sets = (
             "pp200/pythia/default/minibias/year_2a/hadronic_on",

);


my $prodPeriod = "mdc3"; 
my @chName = ("tfs4h", "tdaq4");
my $prodDir = "tfs_6";              
my @chainDir = ("tfs","daq","trs");

###Set directories to be created for jobfiles

my $DISK1        = "/star/rcf/disk00001/star/";
my $TOPHPSS_SINK = "/home/starsink/raw";
my $TOPHPSS_RECO = "/home/starreco/reco";
my @JOB_LOG;
my @JOB_DIR;
my @SUM_DIR;

my @jobIn_set = ();
my $jobIn_no = 0;


struct JFileAttr => {
          setN   => '$', 
         fileN   => '$',
        NjobId   => '$',
          NEvt   => '$', 
		    };

struct OptAttr => {
         prodSer  => '$',
          evType  => '$',  
          chaOpt  => '$',
          libVer  => '$',
         chaName  => '$',
        };

 my @jobOpt;
 my $njobOpt = 0;



### connect to the DB
 &StDbProdConnect();


my $jb_news;
my $jb_archive;
my $jb_jobfile;
my $jb_hold;
my $jb_fstat;

######## declare variables needed to fill the JobStatus table

 my $mchainOp;

 my $mflName = "n\/a";
 my $mjobID = "n\/a";
 my $mprodSr = "n\/a";
 my $mjobFname = "n\/a";
 my $mjobFdir = "n\/a";
 my $msumFile = "n\/a";
 my $msumDir = "n\/a";
 my $mjobSt = "n\/a";
 my $mNev  = 0;
 my $mCPU = 0;
 my $mmemSz = 0;
 my $mNoTrk = 0;
 my $mNoVert = 0;
 my $mchName = "n\/a";
 my $mnodeId = "n\/a";
 my $startId = "Job_mdc3";
 my $startSer = "mdc3";
 my $new_id = 0;

### start loop over input files

my $filename;


### insert first line to JobStatusT table get last ID 

   $sql="insert into $cpJobStatusT set ";    
   $sql.="jobID='$startId',"; 
   $sql.="prodSeries='$startSer'";
    print "$sql\n" if $debugOn;
   $rv = $dbh->do($sql) || die $dbh->errstr;
   $new_id = $dbh->{'mysql_insertid'};

	      for ($ii=0; $ii<2; $ii++)  {
#  $ii = 0;
  $JOB_LOG[$ii] =  $DISK1 . $prodPeriod . "/log/" . $chainDir[$ii];
  $JOB_DIR[$ii] =  "/star/u2e/starreco/" . $prodPeriod ."/requests/". $chainDir[$ii]; 
  $SUM_DIR[$ii] =  $DISK1 . $prodPeriod . "/sum/" . $chainDir[$ii];
 }


 $jobIn_no = 0;
 $njobOpt = 0;


  $sql="SELECT prodSeries, eventType, libVersion, chainOpt, chainName  FROM $ProdOptionsT where prodSeries = '$prodPeriod' AND chainName = '$chName[0]'";

    $cursor =$dbh->prepare($sql)
   || die "Cannot prepare statement: $DBI::errstr\n";
    $cursor->execute;
 
  while(@fields = $cursor->fetchrow) {
    my $cols=$cursor->{NUM_OF_FIELDS};
       $jObAdr = \(OptAttr->new());
 

   for($i=0;$i<$cols;$i++) {
    my $fvalue=$fields[$i];
       my $fname=$cursor->{NAME}->[$i];
     print "$fname = $fvalue\n" if $debugOn;

         ($$jObAdr)->prodSer($fvalue)   if( $fname eq 'prodSeries');
         ($$jObAdr)->evType($fvalue)    if( $fname eq 'eventType');
         ($$jObAdr)->chaOpt($fvalue)    if( $fname eq 'chainOpt');
         ($$jObAdr)->libVer($fvalue)    if( $fname eq 'libVersion');  
         ($$jObAdr)->chaName($fvalue)   if( $fname eq 'chainName'); 
  }

   $jobOpt[$njobOpt] = $jObAdr;
   $njobOpt++;
  }
	    
 $ii = 0;
  $jobIn_no = 0; 
  for ($ii=0; $ii< scalar(@Sets); $ii++)  { 

 $sql="SELECT dataset, fName, Nevents FROM $cpFileCatalogT WHERE fName LIKE '%fzd' AND dataset = '$Sets[$ii]' AND hpss = 'Y'";
   $cursor =$dbh->prepare($sql)
    || die "Cannot prepare statement: $DBI::errstr\n";
          $cursor->execute;
 
  while(@fields = $cursor->fetchrow) {
    my $cols=$cursor->{NUM_OF_FIELDS};
       $fObjAdr = \(JFileAttr->new());
 

   for($i=0;$i<$cols;$i++) {
    my $fvalue=$fields[$i];
      my $fname=$cursor->{NAME}->[$i];
     print "$fname = $fvalue\n" if $debugOn;

        ($$fObjAdr)->setN($fvalue)    if( $fname eq 'dataset');
        ($$fObjAdr)->fileN($fvalue)   if( $fname eq 'fName'); 
        ($$fObjAdr)->NEvt($fvalue)    if( $fname eq 'Nevents'); 
 }

   $jobIn_set[$jobIn_no] = $fObjAdr;
   $jobIn_no++;

 }

}
my $basename;
my $Nevts;
my $fileSeq;
my $Nrun;
my $nf;
my $nfr;
my $NflSeq;
my $nSeq;
my $nEvt = 0;
my $outfile;

###  start loop over input files

 foreach my $jobnm (@jobIn_set){
     my $jset = ($$jobnm)->setN;
     my $flname = ($$jobnm)->fileN;
     my $jfile = $flname;
     if($jfile =~ /fzd/) {
      $jfile =~ s/.fzd//g;
      $basename = $jfile;
      $basename =~ m/(^[a-z0-9]+)_([0-9]+)_([0-9]+)/;
      $fileSeq = $2;
      $Nevts = $3;
      $Nrun = $1;
    }else {
     next;
        }

       foreach my $optchain (@jobOpt) {
        my $mEvt     = ($$optchain)->evType;
        my $mchain   = ($$optchain)->chaOpt;
        my $mlibVer  = ($$optchain)->libVer; 
        my $mNikName = ($$optchain)->chaName;

          $mprodSr = $prodPeriod; 
          $mflName = $flname;
          $msumDir = $SUM_DIR[0];
          $mjobFdir = "new_jobs";
          $mjobSt = "n\/a";
          $mchName = $mNikName;
         my $jbset = $jset;
          $jbset =~ s/\//_/g;
           $nEvt = 5000;
           $nf = int($Nevts / $nEvt) ;
           $nfr = $Nevts % $nEvt ; 
  for ($ii = 1; $ii<= $nf; $ii++) {
           $nSeq = $ii * 10 ;           
           $NflSeq = $nSeq + $fileSeq;  
           $outfile = $Nrun . "_" . $NflSeq . "_" . $nEvt . "evts";          
           $msumFile = $outfile . ".sum"; 
          $mjobFname = $jbset . "_" . $outfile;
          $myID = 100000000 + $new_id;
          $mjobID = "Job". $myID . "_" . $prodPeriod ."_". $mNikName;
print "Jobfilename : ", $mjobFname, " % ", $msumFile, "\n";

   $jb_fstat = 1;
   $jb_news = $JOB_DIR[0] . "/new_jobs/" . $mjobFname;
   $jb_archive = $JOB_DIR[0] . "/archive/" . $mjobFname;
   $jb_jobfile = $JOB_DIR[0] . "/jobfiles/" . $mjobFname;
   $jb_hold = $JOB_DIR[0] . "/jobs_hold/" . $mjobFname;
    if (-f $jb_news)     {$jb_fstat = 0};
    if (-f $jb_archive)  {$jb_fstat = 0};
    if (-f $jb_jobfile)  {$jb_fstat = 0};
    if (-f $jb_hold)     {$jb_fstat = 0};  
     if($jb_fstat eq 1)  {

         &create_jobs($outfile, $jfile, $jset, $mchain, $JOB_DIR[0]); 
  
        print "JOB ID = " ,$mjobID, "\n";

###  fill  JobStatus table
       print "filling JobStatus table\n";
 
      &fillJSTable();   

###  fill  jobRelations table
        print "filling jobRelations table\n";
       &fillJRelTable();

       }
      }
        if ($nfr != 0) {
           $nSeq = ($nf + 1) * 10 ;                 
           $NflSeq = $nSeq + $fileSeq; 
      print "nSeq = " , $nSeq,  " % ", $fileSeq, "%", $NflSeq, "\n";
           $outfile = $Nrun . "_" . $NflSeq . "_" . $nfr . "evts";          
           $msumFile = $outfile . ".sum";           
          $mjobFname = $jbset . "_" . $outfile;
           $myID = 100000000 + $new_id;
          $mjobID = "Job". $myID . "_" . $prodPeriod ."_". $mNikName;
 print "Jobfilename : ", $mjobFname, " % ", $msumFile, "\n";

   $jb_fstat = 1;
   $jb_news = $JOB_DIR[0] . "/new_jobs/" . $mjobFname;
   $jb_archive = $JOB_DIR[0] . "/archive/" . $mjobFname;
   $jb_jobfile = $JOB_DIR[0] . "/jobfiles/" . $mjobFname;
   $jb_hold = $JOB_DIR[0] . "/jobs_hold/" . $mjobFname;
    if (-f $jb_news)     {$jb_fstat = 0};
    if (-f $jb_archive)  {$jb_fstat = 0};
    if (-f $jb_jobfile)  {$jb_fstat = 0};
    if (-f $jb_hold)     {$jb_fstat = 0};  
     if($jb_fstat eq 1)  {

         &create_jobs($outfile, $jfile, $jset, $mchain, $JOB_DIR[0]); 
  
        print "JOB ID = " ,$mjobID, "\n";

###  fill  JobStatus table
       print "filling JobStatus table\n";
 
      &fillJSTable();   

###  fill  jobRelations table
        print "filling jobRelations table\n";
       &fillJRelTable();

       }
      }
 
     }  
 }


###delete from $JobStatusT inserted JobID

    $sql="delete from $cpJobStatusT WHERE ";    
    $sql.="jobID='$startId' AND "; 
    $sql.="prodSeries='$startSer'";
     print "$sql\n" if $debugOn;
    $rv = $dbh->do($sql) || die $dbh->errstr;

# finished with data base
   &StDbProdDisconnect();

  exit;

################################################################################
 sub fillJSTable {

   $sql="insert into $cpJobStatusT set ";
   $sql.="jobID='$mjobID',";
   $sql.="prodSeries='$mprodSr',";
   $sql.="jobfileName='$mjobFname',";
   $sql.="jobfileDir='$mjobFdir',";
   $sql.="sumFileName='$msumFile',";
   $sql.="sumFileDir='$msumDir',";
   $sql.="jobStatus='$mjobSt',"; 
   $sql.="chainName='$mchName'";

    print "$sql\n" if $debugOn;
   $rv = $dbh->do($sql) || die $dbh->errstr;
   $new_id = $dbh->{'mysql_insertid'};     
  
   }

###############################################################################
 sub fillJRelTable {

   $sql="insert into $cpjobRelationsT set ";
   $sql.="JobID='$mjobID',";
   $sql.="prodSeries='$mprodSr',";
   $sql.="inputFile='$mflName'"; 

    print "$sql\n" if $debugOn;
   $rv = $dbh->do($sql) || die $dbh->errstr;

  }

#############################################################################
### create jobfiles to get default set of output files

 sub create_jobs {

  my ($gfile, $fzfile, $Jset, $fchain, $JobDir ) = @_ ;


  my $job_set;
  my @parts;
  my $Jsetd;
  my $Jsetr;
  my $inFile;
  my $logDir;

  $fchain =~ s/_/,/g;
 

  $Jsetr = $Jset . "/gstardata";
  $Jsetd = $Jset . "/" . $prodDir;
  $inFile = $fzfile . ".fzd";
  $logDir = $JOB_LOG[0]; 
   $job_set = $Jset;
  $job_set =~ s/\//_/g;

## print $job_set, "\n";
 
  my $jb_new = $JobDir . "/new_jobs/" .  $job_set . "_" . $gfile;
    print $jb_new, "\n";
  
     my $hpss_raw_dir  = $TOPHPSS_SINK . "/" . $Jsetr;
     my $hpss_raw_file = $inFile;
     my $hpss_dst_dir  = $TOPHPSS_RECO . "/" . $Jsetd;
     my $hpss_dst_file0 = $gfile . ".dst.root";
     my $hpss_dst_file1 = $gfile . ".hist.root";
     my $hpss_dst_file2 = $gfile . ".tags.root";
     my $hpss_dst_file3 = $gfile . ".runco.root";
     my $hpss_dst_file4 = $gfile . ".geant.root";
     my $executable     = "/afs/rhic/star/packages/SL00c_2/mgr/bfc.csh";
     my $executableargs = $fchain;
     my $log_dir       = $logDir;
     my $log_name      = $gfile . ".log";
     my $err_log       = $gfile . ".err";
     if (!open (TOM_SCRIPT,">$jb_new")) {printf ("Unable to create job submission script %s\n",$jb_new);}
       print TOM_SCRIPT "mergefactor=1\n";


      print TOM_SCRIPT "#input\n";
       print TOM_SCRIPT "      inputnumstreams=1\n";
       print TOM_SCRIPT "      inputstreamtype[0]=HPSS\n";
       print TOM_SCRIPT "      inputdir[0]=$hpss_raw_dir\n";
       print TOM_SCRIPT "      inputfile[0]=$hpss_raw_file\n";
       print TOM_SCRIPT "#output\n";
       print TOM_SCRIPT "      outputnumstreams=5\n";
       print TOM_SCRIPT "#output stream \n";
       print TOM_SCRIPT "      outputstreamtype[0]=HPSS\n";
       print TOM_SCRIPT "      outputdir[0]=$hpss_dst_dir\n";
       print TOM_SCRIPT "      outputfile[0]=$hpss_dst_file0\n";
       print TOM_SCRIPT "      outputstreamtype[1]= HPSS\n";
       print TOM_SCRIPT "      outputdir[1]=$hpss_dst_dir\n";
       print TOM_SCRIPT "      outputfile[1]=$hpss_dst_file1\n";
       print TOM_SCRIPT "      outputstreamtype[2]=HPSS\n";
       print TOM_SCRIPT "      outputdir[2]=$hpss_dst_dir\n";
       print TOM_SCRIPT "      outputfile[2]=$hpss_dst_file2\n";
       print TOM_SCRIPT "      outputstreamtype[3]=HPSS\n";
       print TOM_SCRIPT "      outputdir[3]=$hpss_dst_dir\n";
       print TOM_SCRIPT "      outputfile[3]=$hpss_dst_file3\n";
       print TOM_SCRIPT "      outputstreamtype[4]=HPSS\n";
       print TOM_SCRIPT "      outputdir[4]=$hpss_dst_dir\n";
       print TOM_SCRIPT "      outputfile[4]=$hpss_dst_file4\n";
       print TOM_SCRIPT "#standard out -- Should be five outputs\n";
       print TOM_SCRIPT "      stdoutdir=$log_dir\n";
       print TOM_SCRIPT "      stdout=$log_name\n";
       print TOM_SCRIPT "#standard error -- Should be five\n";
       print TOM_SCRIPT "      stderrdir=$log_dir\n";
       print TOM_SCRIPT "      stderr=$err_log\n";
       print TOM_SCRIPT "      notify=starreco\@rcf.rhic.bnl.gov\n";
       print TOM_SCRIPT "#program to run\n";
       print TOM_SCRIPT "      executable=$executable\n";
       print TOM_SCRIPT "      executableargs=$executableargs\n";
 
       close(TOM_SCRIPT);

  }
###==================================================================================
