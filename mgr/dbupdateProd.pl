#! /opt/star/bin/perl -w
#
#  
#
# dbupdateProd.pl - script to update Production FileCatalog and JobStatus
# Requires two arguments: production series, disk data location like '/star/data19/test2001'
# L.Didenko
############################################################################

use Mysql;
use Class::Struct;
use File::Basename;
use Net::FTP;

require "/afs/rhic/star/packages/dev/mgr/dbCpProdSetup.pl";

my $debugOn=0;


my $DISK1 = "/star/rcf/prodlog";
#my $DISK = "/star/data19/test2001"; 
my $prodSr = $ARGV[0]; 
my $DISK = $ARGV[1];
my $jobFDir = "/star/u/starreco/" . $prodSr ."/requests/";
my $logDir = $DISK1 . "/" .$prodSr . "/log_old/daq";

my $topHpssReco  =  "/home/starreco/reco";

my @SetD;
my @SetS;
my @DirD;
my $nSetS = 0;
my $trgDir = "physics";
my $mySet;
my @prtk;
my $lastTime;
my $lastDTime;

  &StDbProdConnect();

   $sql="SELECT DISTINCT path FROM $FileCatalogT WHERE path like '%/daq/2001/%' AND insertTime > 0107180000 ";

     $cursor =$dbh->prepare($sql)
      || die "Cannot prepare statement: $DBI::errstr\n";
     $cursor->execute;
 
      while(@fields = $cursor->fetchrow) {
        my $cols=$cursor->{NUM_OF_FIELDS};

          for($i=0;$i<$cols;$i++) {
             my $fvalue=$fields[$i];
             my $fname=$cursor->{NAME}->[$i];
#          print "$fname = $fvalue\n" ;
         
         $mySet = $fvalue  if ( $fname eq 'path'); 
         }
       next if ($mySet eq "/home/starsink/raw/daq/2001/04");
       next if ($mySet eq "/home/starsink/raw/daq/2001/192");
       next if ($mySet eq "/home/starsink/raw/daq/2001/193");
       next if ($mySet eq "/home/starsink/raw/daq/2001/198");
       next if ($mySet eq "/home/starsink/raw/daq/2001/199");     
        $SetS[$nSetS] = $mySet;
        $nSetS++; 
      }

my $kk = 0;


my $nHpssFiles = 0;
my @hpssDstDirs;
my @hpssDstFiles;
my $nDiskFiles = 0;
my @diskRecoDirs;

 for( $k = 0; $k<scalar(@SetS); $k++) {
  @prtk = split("daq", $SetS[$k]);
  $DirD[$k] = $prtk[1];   
  $SetD[$k] = $prodSr .  $DirD[$k];
  $diskRecoDirs[$k] = $DISK . "/" . $SetD[$k];
  $hpssDstDirs[$k] = $topHpssReco . "/" . $SetD[$k];
print "Production DIR :", $SetD[$k], "\n";
print "diskRecoDir: ", $diskRecoDirs[$k], "\n";
print "hpssDstDir:", $hpssDstDirs[$k], "\n";
}

my $nHpssDirs = scalar(@SetD);


    $sql="SELECT max(createTime) FROM $FileCatalogT WHERE jobID like '%$prodSr%' AND fName like '%.root' AND hpss = 'Y' ";

     $cursor =$dbh->prepare($sql)
      || die "Cannot prepare statement: $DBI::errstr\n";
     $cursor->execute;
 
     my $crHTime = $cursor->fetchrow ;
      $cursor->finish;   


     $sql="SELECT max(createTime) FROM $FileCatalogT WHERE jobID like '%$prodSr%' AND fName like '%.root' AND site = 'disk_rcf' ";

     $cursor =$dbh->prepare($sql)
      || die "Cannot prepare statement: $DBI::errstr\n";
     $cursor->execute;
 
        my $crTime = $cursor->fetchrow;
          $cursor->finish;


 &StDbProdDisconnect();

my $temTime;
my $temDate;
my $temHour;
   $temTime  = $crHTime;
   @prtk = split(" ",$temTime);
   $temDate = $prtk[0];
   $temDate =~ s/-//g;
   $temHour = $prtk[1];
 my @wrd = split(":",$temHour);   
   $lastTime = $temDate . $wrd[0] . $wrd[1];
   $temTime  = $crTime;
   @prtk = split(" ",$temTime);
   $temDate = $prtk[0];
   $temDate =~ s/-//g;
   $temHour = $prtk[1];
 my @wrdk = split(":",$temHour);   
   $lastDTime = $temDate . $wrdk[0] . $wrdk[1];

#    $lastTime = "200107251515";
 print "Last Date: ",  $lastTime, "  %  ",$lastDTime ;


my $recoDir = ("daq");

struct JFileAttr => {
          prSer  => '$',
          job_id => '$', 
          smFile => '$',
          smDir  => '$',
          jbFile => '$',
          NoEvt  => '$',
          NoEvSk => '$',
          jobSt  => '$',  
          FstEvt => '$',
          LstEvt => '$',          
		    };
 
 struct FileAttr => {
    filename  => '$',
    fpath     => '$', 
    dsize     => '$',
    timeS     => '$',
    faccess   => '$',
    fowner    => '$',
                  };

 struct RunAttr => {
        drun   => '$',
        dtSet  => '$',
                  };

 my %monthHash = (
                  "Jan" => 1,
                  "Feb" => 2, 
                  "Mar" => 3, 
                  "Apr" => 4, 
                  "May" => 5, 
                  "Jun" => 6, 
                  "Jul" => 7, 
                  "Aug" => 8, 
                  "Sep" => 9, 
                  "Oct" => 10, 
                  "Nov" => 11, 
                  "Dec" => 12
                  );

my @runSet;
my $nrunSet = 0;
my @jobSum_set;
my $jobSum_no = 0;
my $jbSt = "n/a";
my @runDescr;
my $nrunDescr = 0;

########## Find reco for daq files on HPSS

  $nHpssFiles = 0;

   print "\nFinding DST files in HPSS\n";
   my $ftpRDaq = Net::FTP->new("hpss.rcf.bnl.gov", Port => 2121, Timeout=>600)
     or die "HPSS access failed";
   $ftpRDaq->login("starreco","MockData") or die "HPSS access failed";

   &walkDHpss( $ftpRDaq, \@hpssDstDirs, \@hpssDstFiles, $lastTime );
   print "Total files: ".@hpssDstFiles."\n";
    $ftpRDaq->quit();

  print "\nFinding reco files in disk\n";
 
  my $maccess; 
  my $mdowner; 
  my $flname;

  foreach my $diskDir (@diskRecoDirs) {
    if (-d $diskDir) {
    opendir(DIR, $diskDir) or die "can't open $diskDir\n";
    while( defined($flname = readdir(DIR)) ) {
       next if $flname =~ /^\.\.?$/;
       next if $flname =~ /hold/;

          $maccess = "-rw-r--r--"; 
          $mdowner = "starreco";

       $fullname = $diskDir."/".$flname;
       my @dirF = split(/\//, $diskDir); 
       my $set = sprintf("%s\/%s\/%s\/%s",$dirF[4],$dirF[5],$dirF[6]);
                                                
#      print "Dst Set = ", $set, "\n"; 
      ($size, $mTime) = (stat($fullname))[7, 9];
      ($sec,$min,$hr,$dy,$mo,$yr) = (localtime($mTime))[0,1,2,3,4,5];

      if( $yr > 97 ) {
        $fullyear = 1900 + $yr;
      } else {
        $fullyear = 2000 + $yr;
      }
       
      $mo = sprintf("%2.2d", $mo+1);
      $dy = sprintf("%2.2d", $dy);
#      $timeS = sprintf ("%4.4d%2.2d%2.2d",
#                        $fullyear,$mo,$dy);
      $timeS = sprintf ("%4.4d-%2.2d-%2.2d %2.2d:%2.2d:00",
                         $fullyear,$mo,$dy,$hr,$min);
 my $dkTime = sprintf ("%4.4d%2.2d%2.2d%2.2d%2.2d",
                      $fullyear,$mo,$dy,$hr,$min); 

       if($dkTime > $lastDTime ) {     
#      print "File to be processed from disk: ", $flname, "  %  ",$timeS ,"  %  ", $dkTime,"\n";    
      $fObjAdr = \(FileAttr->new());
      ($$fObjAdr)->filename($flname);
      ($$fObjAdr)->fpath($diskDir);
      ($$fObjAdr)->dsize($size);
      ($$fObjAdr)->timeS($timeS);
      ($$fObjAdr)->faccess($maccess);
      ($$fObjAdr)->fowner($mdowner);
      $hpssDstFiles[$nHpssFiles] = $fObjAdr;
     $nHpssFiles++;
     $nDiskFiles++;
    }
  }
  closedir DIR;
  }
  } 
  print "Total reco files: $nDiskFiles\n";

#####  connect to the DB

   &StDbProdConnect();

 my $mFile;
 my $mEvType = 0;

  for ($ll = 0; $ll<scalar(@SetS); $ll++) {

   $sql="SELECT DISTINCT runID, dataset FROM $FileCatalogT WHERE path = '$SetS[$ll]' AND dataset like 'AuAu%' AND fName like '%daq' ";

     $cursor =$dbh->prepare($sql)
      || die "Cannot prepare statement: $DBI::errstr\n";
     $cursor->execute;
 
      while(@fields = $cursor->fetchrow) {
        my $cols=$cursor->{NUM_OF_FIELDS};
          $fObjAdr = \(RunAttr->new());

          for($i=0;$i<$cols;$i++) {
             my $fvalue=$fields[$i];
             my $fname=$cursor->{NAME}->[$i];
#          print "$fname = $fvalue\n" ;

      ($$fObjAdr)->drun($fvalue)      if( $fname eq 'runID');
      ($$fObjAdr)->dtSet($fvalue)     if( $fname eq 'dataset');   
    }
       $runDescr[$nrunDescr] = $fObjAdr;
       $nrunDescr++;
   } 
  }

#####  select from JobStatus table files which should be updated

 $sql="SELECT prodSeries, JobID, sumFileName, sumFileDir, jobfileName, jobStatus FROM $JobStatusT WHERE prodSeries = '$prodSr' AND jobStatus = 'n/a' ";

    $cursor =$dbh->prepare($sql)
      || die "Cannot prepare statement: $DBI::errstr\n";
     $cursor->execute;
 
      while(@fields = $cursor->fetchrow) {
       my $cols=$cursor->{NUM_OF_FIELDS};
          $fObjAdr = \(JFileAttr->new());
 
     for($i=0;$i<$cols;$i++) {
       my $fvalue=$fields[$i];
         my $fname=$cursor->{NAME}->[$i];
#          print "$fname = $fvalue\n" ;

          ($$fObjAdr)->prSer($fvalue)    if( $fname eq 'prodSeries');
          ($$fObjAdr)->job_id($fvalue)   if( $fname eq 'JobID'); 
          ($$fObjAdr)->smFile($fvalue)   if( $fname eq 'sumFileName'); 
          ($$fObjAdr)->smDir($fvalue)    if( $fname eq 'sumFileDir'); 
          ($$fObjAdr)->jbFile($fvalue)   if( $fname eq 'jobfileName');
          ($$fObjAdr)->jobSt($fvalue)    if( $fname eq 'jobStatus');
     }

         $jobSum_set[$jobSum_no] = $fObjAdr;
         $jobSum_no++; 
 }
 
 my $mchainOp;
 my $pr_chain;
 my $mjobFname;
 my $jb_news;
 my $jb_archive;
 my $jb_jobfile;
 my $jb_hold;
 my $filename;
 my $JOB_DIR;
 my @parts;
 my $mjobDg = "none";
 my $old_logFile;

#########  declare variables needed to fill the JobStatus table

 my $mjobSt = "n/a";
 my $mNev  = 0;
 my $mEvtSk = 0;
 my $mCPU = 0;
 my $mRealT = 0;
 my $mmemSz = 0;
 my $mNoTrk = 0;
 my $mNoVert = 0;
 my $mnodeId = "n/a";
 my $jb_logFile;
 my $mlogFile;
 my $mlogDir;
 my $mproSr;
 my $mjbDir = "new_jobs";
 my $jfile;
 my $mfile;
 my $lgsize;

########  declare variables needed to fill the database table

 my $mJobId = "n/a";
 my $mrunId = 0;
 my $mfileSeq = 0;
 my $mevtType = 0;
 my $mfName = "n/a";
 my $mpath  = "n/a";
 my $mdataSet = "n/a";
 my $msize = 0;
 my $mcTime = 00-00-00;
 my $mNevts = 0;
 my $mNevtLo = 0;
 my $mNevtHi = 0;
 my $mowner = "n/a";
 my $mprotc = "-rw-r-----";
 my $mtype = "n/a";
 my $mcomp = "n/a";
 my $mformat = "n/a";
 my $msite = "n/a";
 my $mhpss = "Y";
 my $mstatus = 0;
 my $mdtStat = "OK";
 my $mcomnt = " ";
 my $mcalib = "n/a";
 my $mtrigger = "physics";
 my $compont;
 my $jLog_err;
 my $efile;
 my $Erfile;
 my $old_erfile;
 my $lgfile;
 my $dbfile = "none";
 my $dbpath = "none";
 my $dbStat = "n/a";
 my $mdone = 0;

#####=======================================================
#####  hpss reco daq file check

 my @flsplit;
 my $mfileS;
 my $extn;
 my $mrun;
 my $Numrun;
 my $mdtSet;

   foreach my $jobnm (@jobSum_set){
        $mproSr   = ($$jobnm)->prSer;
        $mJobId   = ($$jobnm)->job_id;
        $mlogFile = ($$jobnm)->smFile;
        $mlogDir  = ($$jobnm)->smDir;
        $mjobFname = ($$jobnm)->jbFile;
        $dbStat    = ($$jobnm)->jobSt;
        $jfile = $mlogFile;
        $jfile =~ s/.log//g;
      $mfile = $mlogDir ."/". $mlogFile;  
        if (-f $mfile)  {
     my $ltime = `mod_time $mfile`;
           if( $ltime > 7200) {
          $lgsize = (stat($mfile))[7];          
          if($lgsize > 12000) {

       $JOB_DIR = $jobFDir . $recoDir ;
#      print "Job Dir = ", $JOB_DIR, "\n";

      $jb_news = $JOB_DIR . "/new_jobs/" . $mjobFname;
      $jb_archive = $JOB_DIR . "/archive/" . $mjobFname;
      $jb_jobfile = $JOB_DIR . "/jobfiles/" . $mjobFname;
      $jb_hold = $JOB_DIR . "/jobs_hold/" . $mjobFname;
      if (-f $jb_news)     {$mjbDir = "new_jobs"};
      if (-f $jb_archive)  {$mjbDir = "archive"};
      if (-f $jb_jobfile)  {$mjbDir = "jobfiles"};
      if (-f $jb_hold)     {$mjbDir = "jobs_hold"};  

        $first_evts = 0;
        $last_evts = 0;
        $mEvtSk = 0;
        $mjobSt = "n/a";
        $mNev  = 0;
        $mCPU = 0;
        $mRealT = 0; 
        $mmemSz = 0;
        $mNoTrk = 0;
        $mNoVert = 0;
        $mnodeId = "n/a";

        $jb_logFile = $mfile;
         $mjobDg = "none";
         $mjobSt = "n/a"; 

       parse_log($jb_logFile);
       
       $old_logFile = $logDir . "/" .$mlogFile; 
       $efile = $jfile .".err";
       $Erfile = $mlogDir . "/" .$efile;
       $old_erfile = $logDir . "/" .$efile;
       if($mjobSt eq "Done")  {
       rename($jb_logFile,$old_logFile); 
       rename($Erfile,$old_erfile);
#        print $old_logFile, "\n";
#        print $Erfile, " % ", $old_erfile, "\n";
}
       if ($mjobSt ne  $dbStat)  {
        print "JobFile=", $mjobFname," % ",$jb_logFile," % ", "Job Status: ", $mjobSt,"\n";
#        print "Event first, last, Done, Skip, Memory, CPU:", $first_evts," % ",$last_evts," % ",$mNev," % ", $mEvtSk," % ",$mmemSz," % ", $mCPU, " % ", $mNoTrk, " % ",$mNoVert, "\n";

#####  update JobStatus table with info for jobs completed
   print "updating JobStatus table\n";
 
      &updateJSTable(); 
      }else{
       next;
     }
     } else {
       next;
     }
     }else {
      next; 
     } 
     }else {
       next;
     }
   }

      foreach my $eachDstFile (@hpssDstFiles) {

#####   reinitialize variables
 $mJobId = "n/a"; 
 $mrunId = 0;
 $mfileSeq = 0;
 $mevtType = 0;
 $mfName = "n/a";
 $mpath  = "n/a";
 $mdataSet = "n/a";
 $mNevts = 0;
 $mNevtLo = 0;
 $mNevtHi = 0;
 $msize = 0;
 $mcTime = 00-00-00;
 $mowner = "n/a";
 $mprotc = "-rw-r-----";
 $mtype = "n/a";
 $mcomp = "n/a";
 $mformat = "n/a";
 $msite = "n/a";
 $mhpss = "Y";
 $mstatus = 0;
 $mdtStat = "OK";
 $mcomnt = " ";   
 $mcalib = "on-fly";
 $mtrigger = "physics";
 
#####   end of reinitialization

  $mfName = ($$eachDstFile)->filename;
  $mpath  = ($$eachDstFile)->fpath;
  $mcTime  = ($$eachDstFile)->timeS;
  $mprotc = ($$eachDstFile)->faccess;
  $mowner = ($$eachDstFile)->fowner;
  $msize = ($$eachDstFile)->dsize;

   if($mfName =~ /root/) {
      $mformat = "root";
      $basename = basename("$mfName",".root");   
      $compont = $basename;
      if ($compont =~ m/\.([a-z0-9_]{3,})$/) {
      $mcomp = $1;
    }else{
     $mcomp = "unknown";
    } 
    @flsplit = split ("_",$basename);  
    $mfileS = $flsplit[4];
    $mrun =  $flsplit[2];
    $mrunId = $mrun;
    $extn = "." . $mcomp;
    $mfileSeq = basename("$mfileS","$extn"); 
    $jfile = basename("$compont", "$extn"); 
    if ($mpath =~ /starreco/) {  
    $msite = "hpss_rcf";
    $mhpss = "Y";
     }else {
    $msite = "disk_rcf";
    $mhpss = "N";
     }
    $mtype = "daq_reco";

    $mevtType = 3;

    foreach my $runDsc (@runDescr) {

         $Numrun     = ($$runDsc)->drun;
         $mdtSet     = ($$runDsc)->dtSet;
          if ($mrunId == $Numrun) {
           $mdataSet = $mdtSet;
           last;
        }else {
         next;
        }
    }    

  $sql="SELECT JobID, prodSeries, jobfileName, sumFileName, jobStatus, NoEvents FROM $JobStatusT WHERE prodSeries= '$prodSr' AND sumFileName like '$jfile%' AND jobStatus <> 'n/a'";

   $cursor =$dbh->prepare($sql)
    || die "Cannot prepare statement: $DBI::errstr\n";
   $cursor->execute;
 
    while(@fields = $cursor->fetchrow) {
     my $cols=$cursor->{NUM_OF_FIELDS};
 
   for($i=0;$i<$cols;$i++) {
     my $fvalue=$fields[$i];
       my $fname=$cursor->{NAME}->[$i];
#        print "$fname = $fvalue\n" ;

        $mproSr   = $fvalue  if( $fname eq 'prodSeries');
        $mJobId   = $fvalue  if( $fname eq 'JobID');
        $mlogFile = $fvalue  if( $fname eq 'sumFileName');
        $mNevts   = $fvalue  if( $fname eq 'NoEvents');
        $mjobSt   = $fvalue  if( $fname eq 'jobStatus');

      }
   }
        $lgfile = $mlogFile;
        $lgfile =~ s/.log//g;
       if ( $mfName =~ /$lgfile/) {

        if ( $mjobSt eq "Done") {
         $mdtStat = "OK";
         $mcomnt = " ";
      } else{
        $mdtStat = "notOK";
        $mcomnt = $mjobSt;
      }

      $dbfile = "none";

 $sql="SELECT fName, path, redone FROM $FileCatalogT  WHERE fName = '$mfName' AND path = '$mpath' AND jobID LIKE '%$prodSr%' ";

    $cursor =$dbh->prepare($sql)
     || die "Cannot prepare statement: $DBI::errstr\n";
           $cursor->execute;
 
   while(@fields = $cursor->fetchrow) {
     my $cols=$cursor->{NUM_OF_FIELDS};
  
   for($i=0;$i<$cols;$i++) {
     my $fvalue=$fields[$i];
       my $fname=$cursor->{NAME}->[$i];
      print "$fname = $fvalue\n" if $debugOn;
         $dbfile = $fvalue     if( $fname eq 'fName');
         $dbpath = $fvalue     if( $fname eq 'path'); 
         $mdone  = $fvalue     if( $fname eq 'redone');
         }
      }

      if ($dbfile eq "none") {
    print "updating FileCatalog with new files\n";
    print "File to be inserted :", $mJobId, " % ", $mpath, " % ",$mfName, " % ", $mdataSet, " % ", $mtrigger," % ",$mdtStat ," % ",$mcomnt," % ",$mNevts, " % ", $mcTime, "\n"; 
 
     &fillDbTable();   
  }else{
      $mdone++;  
    print "updating files rerun in FileCatalog\n";   
    print "File to be updated :", $mJobId, " % ", $mpath, " % ",$mfName, " % ", $mdataSet, " % ", $mtrigger," % ",$mdtStat ," % ",$mcomnt," % ",$mNevts, " % ", $mcTime, "\n"; 
#     &updateFC();
     }
       } else {
         next;
       }

   }else{
    next;
   }
  }


#####  finished with data base
     &StDbProdDisconnect();

    exit;

################################################################################
   sub updateJSTable {

   $sql="update $JobStatusT set ";
   $sql.="jobfileDir='$mjbDir',";
   $sql.="jobStatus='$mjobSt',";
   $sql.="NoEvents='$mNev',";
   $sql.="mem_size_MB='$mmemSz',";
   $sql.="CPU_per_evt_sec='$mCPU',";
   $sql.="avg_no_tracks='$mNoTrk',";
   $sql.="avg_no_vertex='$mNoVert',";
   $sql.="RealTime_per_evt='$mRealT',";
   $sql.="NoEventSkip='$mEvtSk',";
   $sql.="nodeID='$mnodeId'";
   $sql.=" WHERE sumFileName = '$mlogFile' AND jobfileName = '$mjobFname' AND prodSeries = '$mproSr'";
   print "$sql\n" if $debugOn;
#   print "$sql\n";
   $rv = $dbh->do($sql) || die $dbh->errstr;
    }

##############################################################################

  sub fillDbTable {

   $sql="insert into $FileCatalogT set ";
   $sql.="jobID='$mJobId',";
   $sql.="runID='$mrunId',";
   $sql.="fileSeq='$mfileSeq',";
   $sql.="eventType='$mevtType',";
   $sql.="fName='$mfName',";
   $sql.="path='$mpath',";
   $sql.="dataset='$mdataSet',";
   $sql.="size='$msize',";
   $sql.="createTime='$mcTime',";
   $sql.="Nevents='$mNevts',";
   $sql.="NevLo='$mNevtLo',";
   $sql.="NevHi='$mNevtHi',";
   $sql.="owner='$mowner',";
   $sql.="protection='$mprotc',";
   $sql.="type='$mtype',";
   $sql.="component='$mcomp',";
   $sql.="format='$mformat',";
   $sql.="site='$msite',"; 
   $sql.="hpss='$mhpss',";
   $sql.="status= 0,";
   $sql.="dataStatus='$mdtStat',";
   $sql.="calib='$mcalib',";
   $sql.="trigger='$mtrigger',";
   $sql.="comment='$mcomnt' ";
   print "$sql\n" if $debugOn;
   $rv = $dbh->do($sql) || die $dbh->errstr;
   }

##### ======================================================================
  sub updateFC {

      $sql="update $FileCatalogT set ";
      $sql.="size='$msize',";
      $sql.="createTime='$mcTime',";
      $sql.="Nevents='$mNevts',";
      $sql.="owner='$mowner',";
      $sql.="redone='$mdone',"; 
      $sql.="dataStatus='$mdtStat',";     
      $sql.="comment='$mcomnt' ";  
      $sql.=" WHERE fName = '$mfName' AND path='$mpath' ";
      print "$sql\n" if $debugOn;
      $rv = $dbh->do($sql) || die $dbh->errstr;

    }


##############################################################################
  sub walkDHpss {

    my ( $ftp, $dirs, $files, $lstTime ) = @_;
 
 my @fields;
 my $access;
 my $downer;
 my $size;
 my $month;
 my $day;
 my $year;
 my $name; 
 my @dirF;
 my $fullDir;
 my $set; 
 
    for ($ii=0; $ii<$nHpssDirs; $ii++) {
      my @dird = $ftp->dir($dirs->[$ii]);

       for ($jj=0; $jj<@dird; $jj++) {
           @fields = split(/\s+/, $dird[$jj]);
           $access = $fields[0]; 
           $downer = $fields[2];
           $size   = $fields[4];
           $month  = $fields[5];
           $day    = $fields[6];
           $year   = $fields[7];
           $name   = $fields[8];
           $fullDir = $dirs->[$ii];
          
#          @dirF = split(/\//, $dirs->[$ii]); 

#         $set = sprintf("%s\/%s\/%s\/%s",$dirF[4],$dirF[5],$dirF[6],
#                                                  $dirF[7]);

      my $monthD = $monthHash{$month};
      my $sec = 0;
      my $min = 0;
      my $hr = 0;
      
      if ( $year =~ m/:/ ) {
        ( $hr, $min ) = split(/:/,$year);
        $year = (localtime())[5];
      } else {
        $year = $year - 1900;
      }
      
      if( $year > 97 ) {
        $year = 1900 + $year;
      } else {
        $year = 2000 + $year;
      }
   
  my $hpTime = sprintf ("%4.4d%2.2d%2.2d%2.2d%2.2d",
                        $year,$monthD,$day,$hr,$min);
      $timeS = sprintf ("%4.4d-%2.2d-%2.2d %2.2d:%2.2d:00",
                        $year,$monthD,$day,$hr,$min);
#      $timeS = sprintf ("%4.4d%2.2d%2.2d",
#                      $year,$monthD,$day);
	   if ($hpTime >  $lstTime ) {

#   print "Files to be processed: ", $name,"  %  ", $timeS,"  %  ", $hpTime,"\n";

      $fObjAdr = \(FileAttr->new());
      ($$fObjAdr)->filename($name);
      ($$fObjAdr)->fpath($fullDir);
      ($$fObjAdr)->dsize($size);
      ($$fObjAdr)->timeS($timeS);
      ($$fObjAdr)->faccess($access);
      ($$fObjAdr)->fowner($downer);

      $files->[$nHpssFiles] = $fObjAdr;
      $nHpssFiles++;
      print "File ".$name."\n" if $debugOn;
	 }
      }
    } 
  }

#######################################################################################
 sub mod_time($) { 
my $atime;   # Last access time since the epoch
my $mtime;   # Last modify time since the epoch
my $ctime;   # Inode change time (NOT creation time!) since the epoch
my $now;     # current time 
my $dev;
my $ino;
my $mode;
my $nlink;
my $uid;
my $gid;
my $rdev;
my $blksize;
my $blocks;
my $dtime;
my $file_name =  $ARGV[0];

$now = time;
($dev, $ino, $mode, $nlink, $uid, $gid, $rdev, $size, $atime, $mtime, $ctime, $blksize, $blocks ) = stat $file_name;
printf ($mtime);
$dtime = $now - $mtime;
printf  ($dtime) ;
}

############################################################################################

sub parse_log($) {

  my $logName = $_[0];
  my $line; 
  $mjobSt = "Run not completed";
  my $Err_messg = "none";
  my $exmessg = "none";

#  print $logName, "\n";
 open (LOGFILE, $logName ) or die "cannot open $logName: $!\n";

# dump contents of log file to array for parsing
  my @logfile = <LOGFILE>;
  my $no_event = 0;
  my $num_event = 0;
  my $num_line = 0;  
  $mmemSz = 0; 
  my $maker_size = 0;
  my $mymaker; 
  my $no_tracks; 
  my $no_vertices;
  my $no_prtracks; 
  my $tot_tracks = 0;
  my $tot_vertices = 0;
  my $tot_prtracks = 0;
  my $avr_prtracks;
  my @word_tr;
  $mEvtSk = 0;
  $first_evts = 0;
  $last_evts = 0; 
  my @nparts;
  my @size_line;
   my @myword; 
   my @words;
  $mnodeId = "n/a";
   my $Anflag = 0;
#---------------------------------------------------------

  
# parse beginning of file
 $Anflag = 0;   

  foreach my $line (@logfile) {
     chop $line ;
    
# get memory size at the outputStream
     if ($num_line > 1000){
     if( $line =~ /EndMaker/ and $line =~ /outputStream/){
       @size_line = split(" ",$line); 
       $size_line[6] =~ s/=//g;      
        $maker_size += $size_line[6];
         $mymaker = $size_line[3];
     }
   }
# get  number of events
    if ( $line =~ /QAInfo: Done with Event/ ) {
      @nparts = split ( "/",$line);
      $noEvts = $nparts[2];
      $last_evts = substr($noEvts,4) + 0; 
      if ($no_event == 0) {
      $first_evts = $last_evts;
    }
      $no_event++;
  } 

# get number of tracks, vertices and hits

     if ($line =~ /StMessageManager message summary/) {
      $Anflag = 1;
    }

     if ($line =~ /track nodes:/ && $Anflag == 0 ) {
           my  $string = $line;
             @word_tr = split /:/,$string;            
              $no_tracks = $word_tr[2];
              $tot_tracks += $no_tracks; 
#              print $word_tr[2], $no_tracks, "\n";
    }
        elsif($line =~ /primary tracks:/ && $Anflag == 0 ) {
              $string = $line;
             @word_tr = split /:/,$string;
             $no_prtracks = $word_tr[2]; 
             $tot_prtracks += $no_prtracks;
     }
        elsif($line =~ /V0 vertices:/ && $Anflag == 0 ) {
            $string = $line;
             @word_tr = split /:/,$string;
             $no_vertices = $word_tr[2]; 
             $tot_vertices += $no_vertices;
     }   

# check if job crashed 

      if($line =~ /bus error/) {
         $Err_messg = "bus error";
       }

    elsif ($line =~ /segmentation violation/) {
             $Err_messg = "segmentation violation";
    }
    elsif ($line =~ /eventIsCorrupted/)  {
            $exmessg = "Corrupted event";
    } 
    elsif ($line =~ /Interpreter error recovered/)  {
             $Err_messg = "Interpreter error recovered";
           }

# check how many events have been skiped

      if ( $line =~ /Total events processed/) {

        @word_tr = split /:/,$line;
        $mEvtSk = $word_tr[3];
   }      
        
#check if job is completed
    if ( $line =~ /Run completed/) {          
          $mjobSt = "Done";      
        }
 
   if ($no_event != 0) {
   if ($line =~ /Total: bfc/ and $line =~ /seconds Cpu Time/) {
     @myword = split /:/, $line;    
     @words = split(" ",$myword[3]);
   }  
 }
    if ( $line =~ /Finished job execution/) {
      @word_tr = split(" ",$line);
       $mnodeId = $word_tr[11];          
        } 
  $num_line++;
     }

  $mNev = $no_event;
  $num_event = $no_event - $mEvtSk;

  if( $num_event > 0 )  {

   $mRealT = $words[3] /$num_event;
   $mCPU = $words[8] /$num_event;   
   $mNoTrk  = $tot_tracks/$num_event;
   $mNoVert  = $tot_vertices/$num_event;
   $avr_prtracks  = $tot_prtracks/$num_event;
   $mmemSz = $maker_size/$num_event;
 } 
  close(LOGFILE);

##----------------------------------------------------------------------------
# parse error log file

   my @err_out;
   my $mline;
   my $jLog_err;
   my $err_file;
   my @prt;
   my $jLog = $logName;
   my $basename = basename("$jLog",".log");
  $err_file = $basename .".err";
  $jLog_err = $DISK1 . "/" . $prodSr ."/" ."log/daq" ."/" .$err_file;  
  
# print "Err file ",  $jLog_err, "\n"; 

     @err_out = `tail -100 $jLog_err`;
  foreach $mline (@err_out){
          chop $mline;
       if ( $mline =~ /No space left on device/)  {
        $exmessg = "No space left on device";
     } 
        elsif ($mline =~ /Error calling module/) {
       chop $mline;  
      $exmessg = $mline;
      }
     elsif ($mline =~ /Stale NFS file handle/) {
  
      $exmessg = "Stale NFS file handle";
     }       
     elsif ( $mline =~ /Assertion/ & $mline =~ /failed/)  {
        $Err_messg = "Assertion failed";
     } 
      elsif ($mline =~ /Error calling module/) {
       chop $mline;  
      $exmessg = $mline;
   }
      elsif ($mline =~ /Fatal in <operator delete>/) {
  
       $Err_messg = "Fatal in <operator delete>";   
  }
       elsif ($mline =~ /Fatal in <operator new>/) {
  
       $Err_messg = "Fatal in <operator new>";   
  }
      elsif ($mline =~ /Error: Unexpected EOF/) {
      $Err_messg = "Unexpected EOF";
  } 

  } 
      if ( $Err_messg ne "none") {
     $mjobSt = $Err_messg;
   } 
      if ($mNev == 0 &&  $mjobSt eq "Done" && $exmessg eq "Corrupted event") {
      $mjobSt = $exmessg;
  } 
 }
#==============================================================================
	    
