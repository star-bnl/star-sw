#! /opt/star/bin/perl -w
#
#  
#
# dbupdateProd.pl - script to update Production FileCatalog and JobStatus
#
# L.Didenko
############################################################################

use Mysql;
use Class::Struct;
use File::Basename;
use File::Find;
use Net::FTP;

require "/afs/rhic/star/packages/dev/mgr/dbCpProdSetup.pl";
#require "/afs/rhic/star/packages/dev/mgr/dbOnLineSetup.pl";

my $debugOn=0;


my $DISK1 = "/star/rcf/prodlog";
my $DISK = "/star/data18/reco"; 
my $prodSr = "P01he";
my $jobFDir = "/star/u/starreco/" . $prodSr ."/requests/";

my $topHpssReco  =  "/home/starreco/reco";

my @SetD;
my @SetS;
my @diskRecoDirs;

my @trgDir = ("minbias","central");
my @DirD = (
#            "2000/06",
#            "2000/07",
            "2000/08",
            "2000/09",
);

my $kk = 0;

 for( $k = 0; $k<scalar(@DirD); $k++) {
  $SetS[$k] = "daq" . "/" . $DirD[$k];
 for ( $i = 0; $i<scalar(@trgDir); $i++) {
  $SetD[$kk] =  $trgDir[$i] ."/" . $prodSr . "/" . $DirD[$k]; 
print "Production DIR :", $SetD[$kk], "\n";
  $kk++;
}
print "DAQ files DIR :", $SetS[$k], "\n";
}

for( $ll = 0; $ll<scalar(@SetD); $ll++) { 
  $diskRecoDirs[$ll] = $DISK . "/" . $SetD[$ll];
  print "diskRecoDir: $diskRecoDirs[$ll]\n";
}

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
    iflag     => '$',
                  };

 struct DbAttr =>  {
    gname       => '$', 
    gpath       => '$',
    gsize       => '$',
    gtimeS      => '$',
    gdone       => '$',
                    };
      
 struct RunAttr => {
        drun   => '$',
        dtSet  => '$',
        dtrg   => '$',
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

  my %daqHash = ();
  my %flagHash = ();
  my @runSet;
  my $nrunSet = 0;
  my @jobSum_set;
  my $jobSum_no = 0;
  my @jobFSum_set;
  my $jobFSum_no = 0;
  my $jbSt = "n/a";
  my @runDescr;
  my $nrunDescr = 0;

########## Find reco for daq files on HPSS

  my $nHpssFiles = 0;
  my $nHpssDirs = 4;
  my @hpssDstDirs;
  my @hpssDstFiles;
  my $nDiskFiles = 0;

   $nHpssFiles = 0;


   for( $ll = 0; $ll<scalar(@SetD); $ll++) {
     $hpssDstDirs[$ll] = $topHpssReco . "/" . $SetD[$ll];
     print "hpssDstDir:", $hpssDstDirs[$ll], "\n";
   }
 
   print "\nFinding daq DST files in HPSS\n";
   my $ftpRDaq = Net::FTP->new("hpss.rcf.bnl.gov", Port => 2121, Timeout=>600)
     or die "HPSS access failed";
   $ftpRDaq->login("starreco","MockData") or die "HPSS access failed";

   &walkDHpss( $ftpRDaq, \@hpssDstDirs, \@hpssDstFiles );
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
       my $set = sprintf("%s\/%s\/%s\/%s",$dirF[4],$dirF[5],$dirF[6],$dirF[7]);
                                                
#      print "Dst Set = ", $set, "\n"; 
      ($size, $mTime) = (stat($fullname))[7, 9];
      ($sec,$min,$hr,$dy,$mo,$yr) = (localtime($mTime))[0,1,2,3,4,5];
      $mo = sprintf("%2.2d", $mo+1);
      $dy = sprintf("%2.2d", $dy);
  
      if( $yr > 97 ) {
        $fullyear = 1900 + $yr;
      } else {
        $fullyear = 2000 + $yr;
      }
      $fflag = 1;

#      $timeS = sprintf ("%4.4d%2.2d%2.2d",
#                        $fullyear,$mo,$dy);
      $timeS = sprintf ("%4.4d-%2.2d-%2.2d %2.2d:%2.2d:00",
                         $fullyear,$mo,$dy,$hr,$min);
    
      $fObjAdr = \(FileAttr->new());
      ($$fObjAdr)->filename($flname);
      ($$fObjAdr)->fpath($diskDir);
      ($$fObjAdr)->dsize($size);
      ($$fObjAdr)->timeS($timeS);
      ($$fObjAdr)->faccess($maccess);
      ($$fObjAdr)->fowner($mdowner);
      ($$fObjAdr)->iflag($fflag); 
      $hpssDstFiles[$nHpssFiles] = $fObjAdr;
     $nHpssFiles++;
     $nDiskFiles++;
    }
  closedir DIR;
  }
  } 
  print "Total reco files: $nDiskFiles\n";

#####  connect to the DB

   &StDbProdConnect();

 my $mNevts = 0;
 my $mNevtLo = 0;
 my $mNevtHi = 0;
 my $mFile;
 my $mEvType = 0;

  for ($ll = 0; $ll<scalar(@SetS); $ll++) {

   $sql="SELECT DISTINCT runID, dataset, trigger FROM $FileCatalogT WHERE path like '%$SetS[$ll]%' AND dataset like 'AuAu%' AND fName like '%daq' ";

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
      ($$fObjAdr)->dtrg($fvalue)      if( $fname eq 'trigger');

     }
       $runDescr[$nrunDescr] = $fObjAdr;
       $nrunDescr++;
   } 
  }

#####  select from JobStatus table files which should be updated

   $sql="SELECT prodSeries, JobID, sumFileName, sumFileDir, jobfileName FROM $JobStatusT WHERE prodSeries = '$prodSr' AND jobStatus = 'n/a' ";


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
 my $first_evts = 0;
 my $last_evts = 0;

#########  declare variables needed to fill the JobStatus table

 my $msJobId = "n/a";
 my $mjobSt = "n/a";
 my $mNev  = 0;
 my $mEvtSk = 0;
 my $mCPU = 0;
 my $mRealT = 0;
 my $mmemSz = 0;
 my $mNoTrk = 0;
 my $mNoVert = 0;
 my $mnodeId = "n/a";
 my $jb_sumFile;
 my $msumFile;
 my $msumDir;
 my $mproSr;
 my $mjbDir = "new_jobs";
 my $jfile;

 $jobFSum_no = 0;

   foreach my $jobnm (@jobSum_set){
        $mproSr   = ($$jobnm)->prSer;
        $msJobId   = ($$jobnm)->job_id;
        $msumFile = ($$jobnm)->smFile;
        $msumDir  = ($$jobnm)->smDir;
        $mjobFname = ($$jobnm)->jbFile;
        $jfile = $msumFile;
        $jfile =~ s/.sum//g;
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

        $jb_sumFile = $msumDir . "/" . $msumFile;
        if (-f $jb_sumFile)  {

         $mjobDg = "none";
         $mjobSt = "n/a"; 

             &sumInfo("$jb_sumFile",1);

        print "JobFile=", $mjobFname," % ",$jb_sumFile," % ", "Job Status: ", $mjobSt,"\n";
#       print "Event first, last, Done, Skip :", $first_evts," % ",$last_evts," % ",$mNev," % ", $mEvtSk, "\n";

#####  update JobStatus table with info for jobs completed

       print "updating JobStatus table\n";
 
      &updateJSTable(); 

        $fObjAdr = \(JFileAttr->new());
       
        ($$fObjAdr)->prSer($mproSr);    
        ($$fObjAdr)->job_id($msJobId);  
        ($$fObjAdr)->smFile($msumFile);
        ($$fObjAdr)->jbFile($mjobFname);   
        ($$fObjAdr)->NoEvt($mNev);
        ($$fObjAdr)->NoEvSk($mEvtSk); 
        ($$fObjAdr)->FstEvt($first_evts);
        ($$fObjAdr)->LstEvt($last_evts);               
        ($$fObjAdr)->jobSt($mjobSt);

       $jobFSum_set[$jobFSum_no] = $fObjAdr;
       $jobFSum_no++; 
        
       } else {
          next;
        }
 }

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
 my $mtrigger = "n/a";
 my $compont;

#####=======================================================
#####  hpss reco daq file check

 my @flsplit;
 my $mfileS;
 my $extn;
 my $mrun;
 my $Numrun;
 my $mdtSet;
 my $mtrig;
 my $dfile;

        foreach my $eachDstFile (@hpssDstFiles) {

#####   reinitialize variables

 $mJobId = "n/a"; 
 $mrunId = 0;
 $mfileSeq = 0;
 $mevtType = 0;
 $mfName = "n/a";
 $mpath  = "n/a";
 $mdataSet = "n/a";
 $msize = 0;
 $mcTime = 00-00-00;
 $mNevts = 0;
 $mNevtLo = 0;
 $mNevtHi = 0;
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
 $mtrigger = "n/a";
 
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
    }
     @flsplit = split ("_",$basename);  
    $mfileS = $flsplit[4];
    $mrun =  $flsplit[2];
    $mrunId = $mrun;
    $extn = "." . $mcomp;
    $mfileSeq = basename("$mfileS","$extn"); 

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
         $mtrig      = ($$runDsc)->dtrg;

          if ($mrunId == $Numrun) {
           $mdataSet = $mdtSet;
           $mtrigger = $mtrig;
           last;
        }else {
         next;
        }
    }      

   foreach my $jobm (@jobFSum_set){
         $mproSr   = ($$jobm)->prSer;
         $msumFile = ($$jobm)->smFile;
         $mJobId   = ($$jobm)->job_id;
         $mNevts = ($$jobm)->NoEvt;
         $mNevtLo =($$jobm)->FstEvt;
         $mNevtHi =($$jobm)->LstEvt;
         $mjobSt  =($$jobm)->jobSt;
         $dfile = $msumFile;
         $dfile =~ s/.sum//g;
 
     chop $mjobSt;        
       if ( $mfName =~ /$dfile/) {
       if ( $mjobSt eq "Done") {
         $mdtStat = "OK";
         $mcomnt = " ";
  } else{
        $mdtStat = "notOK";
        $mcomnt = $mjobSt;
  }

   print "File Name :", $mpath, " % ", $mfName, " % ", $mdataSet, " % ", $mtrigger," % ",$mdtStat ," % ",$mcomnt," % ",$mNevts, " % ", $mcTime, "\n";     
      print "updating FileCatalog with new files\n";
 
     &fillDbTable();   

        last;
       } else {
          next;
       }
     }   
   }else{
    next;
   }
  }

#################### update jobs and files which have been redone  #######################

 my @jobSum = ();
 my $njobSum = 0;
 my @jobFSum = ();
 my $njobFSum = 0;
 my @rootFiles = ();
 my $nrootFile = 0;

  $sql="SELECT fName, path, size, createTime, redone FROM $FileCatalogT  WHERE fName LIKE '%root' AND jobID LIKE '%$prodSr%' ";

    $cursor =$dbh->prepare($sql)
     || die "Cannot prepare statement: $DBI::errstr\n";
           $cursor->execute;
 
   while(@fields = $cursor->fetchrow) {
     my $cols=$cursor->{NUM_OF_FIELDS};
        $fObjAdr = \(DbAttr->new());
  
   for($i=0;$i<$cols;$i++) {
     my $fvalue=$fields[$i];
       my $fname=$cursor->{NAME}->[$i];
      print "$fname = $fvalue\n" if $debugOn;
         ($$fObjAdr)->gname($fvalue)     if( $fname eq 'fName');
         ($$fObjAdr)->gpath($fvalue)     if( $fname eq 'path'); 
         ($$fObjAdr)->gsize($fvalue)     if( $fname eq 'size'); 
         ($$fObjAdr)->gtimeS($fvalue)    if( $fname eq 'createTime');
         ($$fObjAdr)->gdone($fvalue)     if( $fname eq 'redone');
  }

    $rootFiles[$nrootFile] = $fObjAdr;
    $nrootFile++;

  }


   $sql="SELECT JobID, prodSeries, jobfileName, sumFileName, sumFileDir, jobStatus, NoEvents FROM $JobStatusT WHERE JobID like '%$prodSr%' AND jobStatus <> 'n/a'";

   $cursor =$dbh->prepare($sql)
    || die "Cannot prepare statement: $DBI::errstr\n";
   $cursor->execute;
 
    while(@fields = $cursor->fetchrow) {
     my $cols=$cursor->{NUM_OF_FIELDS};
         $fObjAdr = \(JFileAttr->new());
 
   for($i=0;$i<$cols;$i++) {
     my $fvalue=$fields[$i];
       my $fname=$cursor->{NAME}->[$i];
#        print "$fname = $fvalue\n" ;

        ($$fObjAdr)->prSer($fvalue)    if( $fname eq 'prodSeries');
        ($$fObjAdr)->job_id($fvalue)   if( $fname eq 'JobID'); 
        ($$fObjAdr)->jbFile($fvalue)   if( $fname eq 'jobfileName');
        ($$fObjAdr)->smFile($fvalue)   if( $fname eq 'sumFileName'); 
        ($$fObjAdr)->smDir($fvalue)    if( $fname eq 'sumFileDir');  
        ($$fObjAdr)->NoEvt($fvalue)    if( $fname eq 'NoEvents');       
        ($$fObjAdr)->jobSt($fvalue)     if( $fname eq 'jobStatus'); 
   }

       $jobSum[$njobSum] = $fObjAdr;
       $njobSum++; 
}

  foreach my $jobn (@jobSum){
       $mproSr    = ($$jobn)->prSer;
       $msJobId   = ($$jobn)->job_id;
       $mjbStat   = ($$jobn)->jobSt;
       $msumFile  = ($$jobn)->smFile;
       $msumDir   = ($$jobn)->smDir;
       $mjobFname = ($$jobn)->jbFile;
       $mNoEvt    = ($$jobn)->NoEvt;
 
      $jfile = $msumFile;
      $jfile =~ s/.sum//g;
      $first_evts = 0;
      $last_evts = 0;
      $mNev  = 0;
      $mEvtSk = 0;
      $mCPU = 0;
      $mRealT = 0; 
      $mmemSz = 0;
      $mNoTrk = 0;
      $mNoVert = 0;
      $mnodeId = "n/a";

      $jb_sumFile = $msumDir . "/" . $msumFile;

      if (-f $jb_sumFile)  {
       $mjbDir = "archive";
       $mjobDg = "none";
       $mjobSt = "n/a"; 

           &sumInfo("$jb_sumFile",1);

        $fObjAdr = \(JFileAttr->new());
       
        ($$fObjAdr)->prSer($mproSr);    
        ($$fObjAdr)->job_id($msJobId);  
        ($$fObjAdr)->smFile($msumFile);
        ($$fObjAdr)->jbFile($mjobFname);   
        ($$fObjAdr)->NoEvt($mNev);
        ($$fObjAdr)->FstEvt($first_evts);
        ($$fObjAdr)->LstEvt($last_evts);               
        ($$fObjAdr)->jobSt($mjobSt);

       $jobFSum[$njobFSum] = $fObjAdr;
       $njobFSum++; 
       chop $mjobSt;
       if ( ($mjobSt ne $mjbStat) or ($mNev != $mNoEvt) ) {
      print  "JobFile=", $mjobFname," % ", $mjobSt, " % ", "Job Status Old: ", $mjbStat, "\n";

##### update JobStatus table with info for jobs completed

       print "updating JobStatus table with jobs redone \n";
 
     &updateJSTable(); 
        
       }else {
        next;
        }
     }else {
     next;
   }
   }

 my $dbfname;
 my $dbfpath;
 my $dbfsize;
 my $dbgtime;
 my $gtime;
 my $dbctime;
 my $fullName;

   foreach my $eachFile (@hpssDstFiles) {

    $mfName = ($$eachFile)->filename;
    $mcTime  = ($$eachFile)->timeS;
    $msize = ($$eachFile)->dsize;
    $mpath = ($$eachFile)->fpath;
    $fullName = $mpath . "/" . $mfName; 
    $flagHash{$fullName} = ($$eachFile)->iflag;
#    $mcTime = substr ($mgTime,-8) ;
#    print "Time = ",  $mcTime, "\n";
     foreach my $gtfile (@rootFiles){
        $dbfname = ($$gtfile)->gname;
        $dbfpath = ($$gtfile)->gpath;
        $dbfsize = ($$gtfile)->gsize;
        $dbgtime = ($$gtfile)->gtimeS;
        @parts = split (" ",$dbgtime);
        $gtime =  $parts[0];
#        $gtime =~ s/-//g;
        $dbctime = $gtime;

      if ( ($mfName eq $dbfname) and ($mpath eq $dbfpath)) { 

            if ( ($msize eq $dbfsize) and ($mcTime =~ /$dbctime/) ) {
#           if ( $msize eq $dbfsize) {  
            $flagHash{$fullName} = 0;
           } else {
             $flagHash{$fullName} = 2;  
           }
         last;
         }else {
          next;
         } 
      }
   }

 my $NumMisFile = 0;
 my $NumUpFile = 0;
 my $newset;
 my @trk;
 my $jbFile;
 my $topHpss = "home/starreco";
 my $topDisk = "star/rcf";
 my $mdone = 0;
 my $mName;
 my $gflag; 

    foreach my $eachFile (@hpssDstFiles) {

#####  reinitialize variables

 $mJobId = "n/a"; 
 $mrunId = 0;
 $mfileSeq = 0;
 $mevtType = 0;
 $mfName = "n/a";
 $mpath  = "n/a";
 $mdataSet = "n/a";
 $msize = 0;
 $mcTime = 0;
 $mNevts = 0;
 $mNevtLo = 0;
 $mNevtHi = 0;
 $mowner = "n/a";
 $mprotc = 0;
 $mtype = "n/a";
 $mcomp = "n/a";
 $mformat = "n/a";
 $msite = "n/a";
 $mhpss = "Y";
 $mstatus = 0;
 $mdtStat = "OK";
 $mcomnt = " ";   
 $mcalib = "on-fly";
 $mtrigger = "n/a";
 $mdone = 0;  
 $gflag = 0;
#####  end of reinitialization

  $mfName = ($$eachFile)->filename;
  $mpath  = ($$eachFile)->fpath;
  $mcTime  = ($$eachFile)->timeS;
  $mprotc = ($$eachFile)->faccess;
  $mowner = ($$eachFile)->fowner;
  $msize = ($$eachFile)->dsize;

  if($mfName =~ /root/) {
      $mformat = "root";
      $basename = basename("$mfName",".root");   
      my $compont = $basename;
      if ($compont =~ m/\.([a-z0-9_]{3,})$/) {
      $mcomp = $1;
    }else {
    }
     
    @flsplit = split ("_",$basename);  
    $mfileS = $flsplit[4];
    $mrun =  $flsplit[2];
    $mrunId = $mrun;
    $extn = "." . $mcomp;
    $mfileSeq = basename("$mfileS","$extn"); 
    $mevtType = 3;

   if($mpath =~ /starreco/)  {
    $msite = "hpss_rcf";
    $mhpss = "Y";
  } else {
    $msite = "disk_rcf";
    $mhpss = "N";
  } 
    $mtype = "daq_reco"; 

    $fullName = $mpath ."/" . $mfName;
    $gflag = $flagHash{$fullName}; 
#  print "File Name :", $fullName," % ", $gflag, "\n";     
       $newset = $fullName;
       @trk = split ("/", $newset);
     $jbFile = $trk[5] ."_" . $trk[4] ."_" .$trk[6] ."_" . $trk[7];    
 
   if( $gflag != 0 ) {
#  print "File name to be inserted :",$fullName, "\n"; 
     foreach my $dbjob (@jobFSum){
       $mproSr   = ($$dbjob)->prSer;
       $mJobId   = ($$dbjob)->job_id;
        $msumFile = ($$dbjob)->smFile;
        $mNevts   = ($$dbjob)->NoEvt;
        $mNevtLo  = ($$dbjob)->FstEvt;
        $mNevtHi  = ($$dbjob)->LstEvt;
        $mjobFname = ($$dbjob)->jbFile;
        $mjobSt   = ($$dbjob)->jobSt;
       $jfile = $msumFile;
       $jfile =~ s/.sum//g;
       chop $mjobSt; 
       if ($mfName =~ /$jfile/ and $mjobFname =~ /$jbFile/) {
#    print "JobFileName :", $jbFile, "\n";
      if ( $mjobSt ne "Done") {

        $mdtStat = "notOK";
        $mcomnt = $mjobSt;
 } else{
   $mdtStat = "OK";
   $mcomnt = " ";
 }
          
      foreach my $runDsc (@runDescr) {

        $Numrun     = ($$runDsc)->drun;
        $mdtSet     = ($$runDsc)->dtSet;
        $mtrig      = ($$runDsc)->dtrg;
       
         if ($mrunId == $Numrun) {
          $mdataSet = $mdtSet;
          $mtrigger = $mtrig;
          last;
       }else {
        next;
       }
   }      

  if ( $gflag == 1) {

    print "Files to be inserted :", "\n"; 
    print "Files: ", $mJobId," % ", $mpath," % ",$mfName, " % ",$mcTime," % ",$mdone, " % ",$mdataSet," % ",$mtrigger," % ",$mdtStat," % ",$mcomnt, "\n"; 
#   print "Number of Events: EvtDone, EvtLo, EvtHi :", $mNevts," % ",$mNevtLo," % ",$mNevtHi, "\n",
      $NumMisFile++;
      $mdone = 0;

#####  fill Files Catalog with missing files
#     print "Filling Files Catalog with missing files\n";
     &fillDbTable();
  }

 elsif ( $gflag == 2) {
   
  print "Files to be updated :", "\n";    

     $NumUpFile++; 

     foreach my $rdfile (@rootFiles){
      $mdone = ($$rdfile)->gdone;
      $mName = ($$rdfile)->gname;
      if($mfName eq $mName) {
          $mdone++;
       
   print "Files: ", $mJobId, " % ",$mpath," % ",$mfName, " % ",$mcTime," % ",$mdone," % ",$mdataSet," % ",$mtrigger," % ",$mdtStat," % ",$mcomnt, "\n";

#####  update RECO DAQ files in Files Catalog if rerun 
#     print "Updating Files Catalog with missing files\n";
    &updateDbTable();  
       last;
       }else{
      next;
      }
     }
    }else{
    } 
   last;
   }else{
     next;
    }
   }
  }else{
  next;
  }
 }
 }

    print "Number of missing files : ", $NumMisFile, "\n";
    print "Number of updated files : ", $NumUpFile, "\n";


#####  finished with data base
     &StDbProdDisconnect();

    exit;

#################################################################################
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
   $sql.=" WHERE sumFileName = '$msumFile' AND jobfileName = '$mjobFname' AND prodSeries = '$mproSr'";
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
  sub updateDbTable {

      $sql="update $FileCatalogT set ";
      $sql.="size='$msize',";
      $sql.="createTime='$mcTime',";
      $sql.="Nevents='$mNevts',";
      $sql.="NevLo='$mNevtLo',";
      $sql.="NevHi='$mNevtHi',";
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

    my ( $ftp, $dirs, $files ) = @_;
 
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
          
         next if ( $name =~ /^delete_me/);
         next if ( $name =~ /^file/);
         next if ( $name =~ /^Star.Test/);
     
#        @dirF = split(/\//, $dirs->[$ii]); 
#       $set = sprintf("%s\/%s\/%s\/%s",$dirF[4],$dirF[5],$dirF[6],
#                                                $dirF[7]);

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
  $fflag = 1;
   
    $timeS = sprintf ("%4.4d-%2.2d-%2.2d %2.2d:%2.2d:00",
                      $year,$monthD,$day,$hr,$min);
   
#    $timeS = sprintf ("%4.4d%2.2d%2.2d",
#                     $year,$monthD,$day);
      
      $fObjAdr = \(FileAttr->new());
      ($$fObjAdr)->filename($name);
      ($$fObjAdr)->fpath($fullDir);
      ($$fObjAdr)->dsize($size);
      ($$fObjAdr)->timeS($timeS);
      ($$fObjAdr)->faccess($access);
      ($$fObjAdr)->fowner($downer);
      ($$fObjAdr)->iflag($fflag); 

      $files->[$nHpssFiles] = $fObjAdr;
      $nHpssFiles++;
      print "File ".$name."\n" if $debugOn;
      }
    } 
  }

##############################################################################

  sub sumInfo {

  my ($jb_sum,$useless) = @_;   
  my $sum_line ;
  my @word_sum;
   my @output = `more $jb_sum`; 
     foreach my $sum_line (@output) {
              chop $sum_line;

#####  get node name
             if ($sum_line =~ /Starting job execution/) {
              @word_sum = split (" ", $sum_line);
              $mnodeId = $word_sum[11];
	    }
#####  get job status

       if ($sum_line =~ /Job status:/) {
          @word_sum = split (":", $sum_line);
           $mjobSt = $word_sum[1];
       } 
        if ($sum_line =~ /Segmentation violation/) {
                $mjobDg = "Segmentation violation";
            }
       elsif ($sum_line =~ /Bus error/) {
               $mjobDg = "bus_error";
          } 
      if($sum_line =~ /Error message/)  {
          @word_sum = split (":", $sum_line); 
             $mjobDg = $word_sum[1];
        }

#####  get number of events done
   
     if ($sum_line =~ /Number of Events Done/ ) {
       @word_sum = split (":", $sum_line);          
         $mNev = $word_sum[1];
     } 
   	     if ( $sum_line =~ /Number of Events Skiped/ ) {
                   @word_sum = split (":", $sum_line);          
         $mEvtSk = $word_sum[1];
 	        }
  	    if ( $sum_line =~ /First event/ ) {
                   @word_sum = split (":", $sum_line);          
         $first_evts = $word_sum[1];
 	        }
              if ( $sum_line =~ /Last event/ ) {
                     @word_sum = split (":", $sum_line);          
         $last_evts = $word_sum[1];

 	 	 }
#####  get chain
           if( $sum_line =~ /QAInfo:Requested chain is/ ) {
             @word_sum = split (":", $sum_line); 
               $pr_chain = $word_sum[2];
               $pr_chain =~ s/ /_/g;
              if ( $pr_chain =~ /^\s*_/ ) {
                my $mIndex = index $pr_chain, "_";
                  $pr_chain = substr( $pr_chain, $mIndex+1);
                 } 
                            }
#####  get max memory size during execution
              if ($sum_line =~ /Package   outputStream/ ) {
                @word_sum = split (" ", $sum_line);
                  $mmemSz = $word_sum[5];
              }
#####  get CPU and REAL Time per event
               next if ($sum_line =~ /Command string/);
              if($sum_line =~ /Total: bfc/ ) {             
             @word_sum = split (" ", $sum_line);
#            print "CPU = ", $sum_line, "\n";   
              if($word_sum[8] =~ /Cpu/) { 
                $mCPU  = $word_sum[11];  
                $mRealT = $word_sum[6];  
             }
            }
#####  get everage number of tracks in the event

        if($sum_line =~ /QAinfo: Average number of tracks/) {
         @word_sum = split (" ", $sum_line) ;  
                $mNoTrk = $word_sum[5];
       }   
#####  get everage number of vertex in the event

       if($sum_line =~ /QAinfo: Average number of vertices/) {     
           @word_sum = split (" ", $sum_line) ;
            $mNoVert = $word_sum[5]; 
       }
     }
  }

