#! /opt/star/bin/perl -w
#
# 
#
# 
#
# dbupdateDAQRun.pl
#
# Update File Catalog with reconstruction DAQ files missing and rerun
#
##############################################################################

use Mysql;
use File::Find;
use Net::FTP;
use Class::Struct;
use File::Basename;

require "/afs/rhic/star/packages/DEV00/mgr/dbCpProdSetup.pl";
require "/afs/rhic/star/packages/DEV00/mgr/dbOnLineSetup.pl";
require "/afs/rhic/star/packages/DEV00/mgr/dbDescriptorSetup.pl";



my $prodSr = "P00hi"; 

my @Sets = (
             "2000/06",
             "2000/07",
             "2000/08",
             "2000/09",
);

my @DirD = (
            "P00hi/2000/06",
            "P00hi/2000/07",
            "P00hi/2000/08",
            "P00hi/2000/09",
	  );

struct FileAttr => {
      filename  => '$',
      fpath     => '$', 
      size      => '$',
      timeS     => '$',
      faccess   => '$',
      fowner    => '$',
      fformat   => '$',
      fcomp     => '$',
      iflag     => '$',    
};

struct JFileAttr => {
    gname       => '$', 
    gpath       => '$',
    gsize       => '$',
    gtimeS      => '$',
    gdone       => '$',
                    };

struct JSFileAttr => {
          prSer  => '$',
          job_id => '$',
          jbFile => '$', 
          smFile => '$',
          smDir  => '$', 
          jbSt   => '$',
          NoEvt  => '$',
          cpuEvt => '$',
          FstEvt => '$',
          LstEvt => '$',
      };


my $debugOn = 0;

my $topHpssReco  =  "/home/starreco/reco";
my $DISK1 = "/star/rcf/disk00001/star";

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
my %collHash = (
                "Gold"    => "Au", 
                "Protons" => "p",
                "Gas"     => "gas"
              );  


struct RunAttr => {
        drun   => '$',
        iname  => '$',
        jname  => '$',
        iMomnt => '$',
        jMomnt => '$',
        magFld => '$',
        rtpc   => '$', 
        rftpc  => '$',
        rsvt   => '$',
        remc   => '$',
        rsmd   => '$',
        rtof   => '$',
        rrich  => '$',
        rgl3   => '$',
        rsl3   => '$', 
}; 

my @runDetector = ("tpc","svt","ftpc","emc","smd","tof","rich","gl3","sl3"); 
my @DetecOn;
my @runDescr;
my $nrunDescr = 0;
my @runSet;
my $nrunSet = 0;

my %flagHash = ();
my $eachHpssFile;
my $mrun;

########## Find Geant input files in HPSS

my @hpssRecoDirs;
my @hpssRecoFiles;
my $eachRecoFile;

my $nHpssDirs = scalar(@Sets);
my $nHpssFiles = 0;

my @jobIn_set;
my $jobIn_no = 0;

my @jobSum_set;
my $jobSum_no = 0;
my @jobFSum_set;
my $jobFSum_no = 0;
my $jbSt = "n\/a";

########## Find reconstruction files in HPSS

for( $ll = 0; $ll<scalar(@Sets); $ll++) {
  $hpssRecoDirs[$ll] = $topHpssReco . "/" . $prodSr . "/" . $Sets[$ll] ;
  }
my $ftpHpss = Net::FTP->new("hpss.rcf.bnl.gov", Port => 2121, Timeout=>600)
  or die "HPSS access failed";
$ftpHpss->login("starsink","MockData") or die "HPSS access failed";

print "\nFinding reco files in HPSS\n"; 

&walkHpss( $ftpHpss, \@hpssRecoDirs, \@hpssRecoFiles );
print "Total files: ".@hpssRecoFiles."\n";
$ftpHpss->quit();

########## Find reco files in disk
my $maccess; 
 my $mdowner;
 my $flname;


###### Find reco files in FileCatalog
 &StDbProdConnect();


 $sql="SELECT fName, path, size, createTime, redone FROM $FileCatalogT  WHERE fName LIKE '%root' AND jobID LIKE '%$prodSr%' ";

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
        ($$fObjAdr)->gname($fvalue)     if( $fname eq 'fName');
        ($$fObjAdr)->gpath($fvalue)     if( $fname eq 'path'); 
        ($$fObjAdr)->gsize($fvalue)     if( $fname eq 'size'); 
        ($$fObjAdr)->gtimeS($fvalue)    if( $fname eq 'createTime');
        ($$fObjAdr)->gdone($fvalue)     if( $fname eq 'redone');
 }

   $jobIn_set[$jobIn_no] = $fObjAdr;
   $jobIn_no++;

 }

### select reco files status from JobStatus

 $sql="SELECT JobID, prodSeries, jobfileName, sumFileName, sumFileDir, jobStatus, NoEvents, CPU_per_evt_sec FROM $JobStatusT WHERE JobID like '%$prodSr%' AND jobfileName like '$prodSr%' AND jobStatus <> 'n/a'";

  $cursor =$dbh->prepare($sql)
   || die "Cannot prepare statement: $DBI::errstr\n";
  $cursor->execute;
 
   while(@fields = $cursor->fetchrow) {
    my $cols=$cursor->{NUM_OF_FIELDS};
        $fObjAdr = \(JSFileAttr->new());
 

   for($i=0;$i<$cols;$i++) {
    my $fvalue=$fields[$i];
      my $fname=$cursor->{NAME}->[$i];
#       print "$fname = $fvalue\n" ;

       ($$fObjAdr)->prSer($fvalue)    if( $fname eq 'prodSeries');
       ($$fObjAdr)->job_id($fvalue)   if( $fname eq 'JobID'); 
       ($$fObjAdr)->jbFile($fvalue)   if( $fname eq 'jobfileName');
       ($$fObjAdr)->smFile($fvalue)   if( $fname eq 'sumFileName'); 
       ($$fObjAdr)->smDir($fvalue)    if( $fname eq 'sumFileDir');  
       ($$fObjAdr)->NoEvt($fvalue)    if( $fname eq 'NoEvents');       
       ($$fObjAdr)->jbSt($fvalue)     if( $fname eq 'jobStatus'); 
       ($$fObjAdr)->cpuEvt($fvalue)   if( $fname eq 'CPU_per_evt_sec');
  }

      $jobSum_set[$jobSum_no] = $fObjAdr;
      $jobSum_no++; 

}

########## declare variables needed to update the JobStatus table

my $mjobSt = "n\/a";;
my $mNev  = 0;
my $mCPU = 0;
my $mRealT = 0;
my $mmemSz = 0;
my $mNoTrk = 0;
my $mNoVert = 0;
my $mnodeId = "n\/a"; 
my $jb_sumFile;
my $msumFile;
my $msumDir;
my $mproSr;
my $mjbDir;
my $mNoEvt;
my $mjbStat;
my $mcpuEvt;
my $mEvtSk = 0;


#########  update JobStatus Table

foreach my $jobnm (@jobSum_set){
      $mproSr    = ($$jobnm)->prSer;
      $msJobId   = ($$jobnm)->job_id;
      $mjbStat   = ($$jobnm)->jbSt;
      $msumFile  = ($$jobnm)->smFile;
      $msumDir   = ($$jobnm)->smDir;
      $mjobFname = ($$jobnm)->jbFile;
      $mNoEvt    = ($$jobnm)->NoEvt;
      $mcpuEvt   = ($$jobnm)->cpuEvt;
 
    my $jfile = $msumFile;
     $jfile =~ s/.sum//g;
     $first_evts = 0;
     $last_evts = 0;
     $mjobSt = "n\/a";
     $mNev  = 0;
     $mEvtSk = 0;
     $mCPU = 0;
     $mRealT = 0; 
     $mmemSz = 0;
     $mNoTrk = 0;
     $mNoVert = 0;
     $mnodeId = "n\/a";

      opendir(DIR, $msumDir) or die "can't open $msumDir\n";
    while( defined($filename = readdir(DIR)) ) {
      next if $filename =~ /^\.\.?$/;
        next if ( !($filename =~ /.sum$/) );
       if ( $filename =~ /$msumFile/ ) {
     $jb_sumFile = $msumDir . "/" . $msumFile;

      $mjobDg = "none";
      $mjobSt = "n\/a"; 

          &sumInfo("$jb_sumFile",1);

       $fObjAdr = \(JSFileAttr->new());
       
       ($$fObjAdr)->prSer($mproSr);    
       ($$fObjAdr)->job_id($msJobId);  
       ($$fObjAdr)->smFile($msumFile);
       ($$fObjAdr)->jbFile($mjobFname);   
       ($$fObjAdr)->NoEvt($mNev);
       ($$fObjAdr)->FstEvt($first_evts);
       ($$fObjAdr)->LstEvt($last_evts);               

      $jobFSum_set[$jobFSum_no] = $fObjAdr;
      $jobFSum_no++; 

       chop $mjobSt;
#      if ($mjobSt ne $mjbStat) { 
# print "Job Status :",$mjobSt," % ",$mjbStat," % ","\n";
      if ( ($mjobSt ne $mjbStat) or ($mNev != $mNoEvt) ) {
     print  "JobFile=", $mjobFname," % ", "Job Status: ", $mjobSt, " % ", "Job Status Old: ", $mjbStat, "\n";

### update JobStatus table with info for jobs completed

      print "updating JobStatus table\n";
 
     &updateJSTable(); 

      }else {
       }
         last;
      }  else {
         next;
       }
    }   
         closedir DIR;

  }
######### declare variables needed to fill the database table
## for database filling

my $mJobId = "n\/a"; 
my $mrunId = 0;
my $mfileSeq = 0;
my $mevtType = 0;
my $mfName = "n\/a";
my $mpath  = "n\/a";
my $msize = 0;
my $mcTime;
my $mNevts = 0;
my $mNevtLo = 0;
my $mNevtHi = 0;
my $mowner = "n\/a";
my $mprotc = 0;
my $mtype = "n\/a";
my $mcomp = "n\/a";
my $mformat = "n\/a";
my $msite = "n\/a";
my $mhpss = "Y";
my $mstatus = 0;
my $mdataSet = "n\/a"; 
my $dbset;
my $dbfname;
my $dbfsize;
my $dbctime;
my @parts;
my $gtime;
my $dbgtime;
my $ztime = "000000";
my $fullName;


   foreach $eachRecoFile (@hpssRecoFiles) {

   $mfName = ($$eachRecoFile)->filename;
   $mcTime  = ($$eachRecoFile)->timeS;
   $msize = ($$eachRecoFile)->size;
   $mpath = ($$eachRecoFile)->fpath;
   $fullName = $mpath . "/" . $mfName; 
   $flagHash{$fullName} = ($$eachRecoFile)->iflag;
#   $mcTime = substr ($mgTime,-8) ;
#   print "Time = ",  $mcTime, "\n";
    foreach my $gtfile (@jobIn_set){
       $dbfname = ($$gtfile)->gname;
       $dbfpath = ($$gtfile)->gpath;
       $dbfsize = ($$gtfile)->gsize;
       $dbgtime = ($$gtfile)->gtimeS;
       @parts = split (" ",$dbgtime);
       $gtime =  $parts[0];
       $gtime =~ s/-//g;
       $dbctime = $gtime;

       if ( ($mfName eq $dbfname) and ($mpath eq $dbfpath)) { 

#           if ( ($msize eq $dbfsize) and ($mcTime eq $dbctime)) {
          if ( $msize eq $dbfsize) {  
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
  
my $compont;
my $NumMisFile = 0;
my $NumUpFile = 0;
my $newset;
my @trk;
my $jbFile;
my $topHpss = "home/starreco";
my $topDisk = "star/rcf";
my $mdone = 0;
my $mName;

   foreach $eachRecoFile (@hpssRecoFiles) {

##### reinitialize variables

  $mJobId = "n\/a"; 
  $mrunId = 0;
  $mfileSeq = 0;
  $mevtType = 0;
  $mfName = "n\/a";
  $mpath  = "n\/a";
  $mdataSet = "n\/a";
  $msize = 0;
  $mcTime = 0;
  $mNevts = 0;
  $mNevtLo = 0;
  $mNevtHi = 0;
  $mowner = "n\/a";
  $mprotc = 0;
  $mtype = "n\/a";
  $mcomp = "n\/a";
  $mformat = "n\/a";
  $msite = "n\/a";
  $mhpss = "Y";
  $mstatus = 0;
    
  
##### end of reinitialization

   $mfName = ($$eachRecoFile)->filename;
   $mpath  = ($$eachRecoFile)->fpath;
   $mcTime  = ($$eachRecoFile)->timeS;
   $mprotc = ($$eachRecoFile)->faccess;
   $mowner = ($$eachRecoFile)->fowner;
   $msize = ($$eachRecoFile)->size;

   if($mfName =~ /root/) {
     $mformat = "root";
     $basename = basename("$mfName",".root");   
     my $compont = $basename;
     if ($compont =~ m/\.([a-z0-9_]{3,})$/) {
     $mcomp = $1;
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
# print "File Name :", $fullName," % ", $gflag, "\n";     
      $newset = $fullName;
      @trk = split ("/", $newset);
   if( $fullName =~ /$topHpss/) {
    $jbFile = $trk[4] ."_" . $trk[5] ."_" . $trk[6];    
   }
  elsif (  $fullName =~ /$topDisk/ && $fullName =~ /data0/ ) { 
     $jbFile = $trk[5] ."_" . $trk[6] ."_" . $trk[7];        
   }     
  elsif ($fullName =~ /$topDisk/ && $fullName =~ /disk00001/ ) {
     $jbFile = $trk[6] ."_" . $trk[7] ."_" . $trk[8];        
   }      

   if( $gflag != 0 ) {
#print "File name to be inserted :",$fullName, "\n"; 
    foreach my $jobnm (@jobFSum_set){
       $mproSr   = ($$jobnm)->prSer;
       $mJobId   = ($$jobnm)->job_id;
       $msumFile = ($$jobnm)->smFile;
       $mNevts   = ($$jobnm)->NoEvt;
       $mNevtLo =($$jobnm)->FstEvt;
       $mNevtHi =($$jobnm)->LstEvt;
       $mjobFname = ($$jobnm)->jbFile;

     my $jfile = $msumFile;
      $jfile =~ s/.sum//g;

       if ($mfName =~ /$jfile/ and $mjobFname =~ /$jbFile/) {
  print "JobFileName :", $jbFile, "\n";
  if ( $gflag == 1) {

   print "Files to be inserted :", "\n"; 
   print "Job ID: ", $mJobId," % ", "Path: ", $mpath," % ", "File: ", $mfName, " % ","Date:", $mcTime,"\n"; 
#  print "Number Event: Evts, EvtLo, EvtHi :", $mNevts," % ",$mNevtLo," % ",$mNevtHi, "\n",
     $NumMisFile++;

##### fill Files Catalog with missing files
   print "Filling Files Catalog\n";
   &fillDbTable();
 }

elsif ( $gflag == 2) {
    
 print "Files to be updated :", "\n";    

    $NumUpFile++; 

    foreach my $rdfile (@jobIn_set){
     $mdone = ($$rdfile)->gdone;
     $mName = ($$rdfile)->gname;
     if($mfName eq $mName) {
        $mdone++;

  print "Job ID: ", $mJobId, " % ","Path: ", $mpath," % ","File:", $mfName, " % ","Date:", $mcTime," % ","Redone :", $mdone, "\n";

##### update RECO DAQ files in Files Catalog if rerun 
   print "Updating Files Catalog\n";
   &updateDbTable();  
       last;
      }else{
       next;
     }
   }
 }
  last;
 }else{
  next;
    }
   }
  }
 }
}

   print "Number of missing files : ", $NumMisFile, "\n";
   print "Number of updated files : ", $NumUpFile, "\n";

##### select files where dataset is not defined

my $myRun;

 for ($ll = 0; $ll<scalar(@DirD); $ll++) {

 $sql="SELECT DISTINCT runID FROM $FileCatalogT WHERE path like '%$DirD[$ll]' AND dataset = 'n/a' ";
  
      $cursor =$dbh->prepare($sql)

    || die "Cannot prepare statement: $DBI::errstr\n";
   $cursor->execute;
 
    while(@fields = $cursor->fetchrow) {
     my $cols=$cursor->{NUM_OF_FIELDS};

        for($i=0;$i<$cols;$i++) {
          my $fvalue=$fields[$i];
          my $fname=$cursor->{NAME}->[$i];
#        print "$fname = $fvalue\n" ;

        $myRun = $fvalue     if( $fname eq 'runID');  
   }
        $runSet[$nrunSet] = $myRun;
        $nrunSet++;
 }
 }


 &StDbProdDisconnect();
##### connect to the DB RunLog

 &StDbDescriptorConnect();

  for ($ii = 0; $ii<scalar(@runSet); $ii++) { 

 $sql="SELECT $runDescriptorT.runNumber as runDNum, cwName, ccwName, cwMomentum, ccwMomentum, magFieldCurrent, $daqDescriptorT.runNumber as runTNum, tpc, svt, ftpc, emc, smd, tof, rich, gl3, sl3 FROM $runDescriptorT, $daqDescriptorT WHERE category = 'physics' AND $runDescriptorT.runNumber = '$runSet[$ii]' AND $daqDescriptorT.runNumber = $runDescriptorT.runNumber ";

   $cursor =$dbh->prepare($sql)
    || die "Cannot prepare statement: $DBI::errstr\n";
   $cursor->execute;
 
    while(@fields = $cursor->fetchrow) {
      my $cols=$cursor->{NUM_OF_FIELDS};
        $fObjAdr = \(RunAttr->new());

        for($i=0;$i<$cols;$i++) {
           my $fvalue=$fields[$i];
           my $fname=$cursor->{NAME}->[$i];
#        print "$fname = $fvalue\n" ;

    ($$fObjAdr)->drun($fvalue)      if( $fname eq 'runDNum');
    ($$fObjAdr)->iname($fvalue)     if( $fname eq 'cwName');   
    ($$fObjAdr)->jname($fvalue)     if( $fname eq 'ccwName');
    ($$fObjAdr)->iMomnt($fvalue)    if( $fname eq 'cwMomentum');
    ($$fObjAdr)->jMomnt($fvalue)    if( $fname eq 'ccwMomentum');
    ($$fObjAdr)->magFld($fvalue)    if( $fname eq 'magFieldCurrent');
    ($$fObjAdr)->rtpc($fvalue)      if( $fname eq 'tpc');
    ($$fObjAdr)->rsvt($fvalue)      if( $fname eq 'svt');
    ($$fObjAdr)->rftpc($fvalue)     if( $fname eq 'ftpc');
    ($$fObjAdr)->remc($fvalue)      if( $fname eq 'emc');
    ($$fObjAdr)->rsmd($fvalue)      if( $fname eq 'smd');
    ($$fObjAdr)->rtof($fvalue)      if( $fname eq 'tof');
    ($$fObjAdr)->rrich($fvalue)     if( $fname eq 'rich');
    ($$fObjAdr)->rgl3($fvalue)      if( $fname eq 'gl3');
    ($$fObjAdr)->rsl3($fvalue)      if( $fname eq 'sl3'); 

   }
     $runDescr[$nrunDescr] = $fObjAdr;
     $nrunDescr++;
 } 
}
 &StDbDescriptorDisconnect();

my $Numrun;
my $cWname;
my $cEname;
my $cWMnt;
my $cEMnt; 
my $ccn;
my $enrg;
my $magF;
my $mdataset = "n\/a";
my $mrunID;

 &StDbProdConnect();

foreach my $runDsc (@runDescr) {

       $Numrun     = ($$runDsc)->drun;
       $cWname     = ($$runDsc)->iname;
       $cEname     = ($$runDsc)->jname;   
       $cWMnt      = ($$runDsc)->iMomnt;
       $cEMnt      = ($$runDsc)->jMomnt;
       $magF       = ($$runDsc)->magFld;
       $DetecOn[0] = ($$runDsc)->rtpc;
       $DetecOn[1] = ($$runDsc)->rsvt;
       $DetecOn[2] = ($$runDsc)->rftpc;
       $DetecOn[3] = ($$runDsc)->remc;
       $DetecOn[4] = ($$runDsc)->rsmd;
       $DetecOn[5] = ($$runDsc)->rtof;
       $DetecOn[6] = ($$runDsc)->rrich;

       $DetecOn[7] = ($$runDsc)->rgl3;
       $DetecOn[8] = ($$runDsc)->rsl3;

       $ccn = $collHash{$cWname}.$collHash{$cEname};
       $engr = int($cWMnt + $cEMnt);
       if( !defined $magF) {$magF = 0};  

       if ($magF < 2000) {
       $daqHash{$Numrun} = $ccn . $engr ."_" ."FieldOff" . "_";
     }
       elsif ( $magF > 2240 && $magF < 2252 ) {
       $daqHash{$Numrun} = $ccn . $engr ."_" ."HalfField" . "_";
    }
       elsif ( $magF > 3000 ) {
       $daqHash{$Numrun} = $ccn . $engr ."_" ."FullField" . "_";
   } else{
       $daqHash{$Numrun} = $ccn . $engr ."_" ."Unknown" . "_";
   }             
       for ($ll = 0; $ll < scalar(@runDetector); $ll++) {

       if($DetecOn[$ll] != 0) { 
       $daqHash{$Numrun} .= $runDetector[$ll]. "."; 
     }
     }
    chop $daqHash{$Numrun};
    print "RunID : ", $Numrun, " % " ,$daqHash{$Numrun}, "\n";

       $mrunID = $Numrun;
       $mdataset = $daqHash{$Numrun}; 
    if ( defined $mdataset) { 
    &updateDataSet();
   }
 } 

#### finished with data base
   &StDbProdDisconnect();



 exit;

###########
 sub updateJSTable {

    $sql="update $JobStatusT set ";
    $sql.="jobStatus='$mjobSt',";
    $sql.="NoEvents='$mNev',";
    $sql.="NoEventSkip='$mEvtSk',";    
    $sql.="mem_size_MB='$mmemSz',";
    $sql.="CPU_per_evt_sec='$mCPU',";
    $sql.="avg_no_tracks='$mNoTrk',";
    $sql.="avg_no_vertex='$mNoVert',";
    $sql.="RealTime_per_evt='$mRealT',";
    $sql.="nodeID='$mnodeId'";
    $sql.=" WHERE sumFileName = '$msumFile' AND jobfileName = '$mjobFname' AND prodSeries = '$mproSr'";
    print "$sql\n" if $debugOn;
#    print "$sql\n";
    $rv = $dbh->do($sql) || die $dbh->errstr;
  
  }

###########
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
     $sql.="comment=''";
     print "$sql\n" if $debugOn;
     $rv = $dbh->do($sql) || die $dbh->errstr;
   }

#####======================================================================
 sub updateDbTable {

     $sql="update $FileCatalogT set ";
     $sql.="size='$msize',";
     $sql.="createTime='$mcTime',";
     $sql.="Nevents='$mNevts',";
     $sql.="NevLo='$mNevtLo',";
     $sql.="NevHi='$mNevtHi',";
     $sql.="owner='$mowner',";
     $sql.="redone='$mdone'"; 
     $sql.=" WHERE fName = '$mfName' AND path='$mpath'";
     print "$sql\n" if $debugOn;
     $rv = $dbh->do($sql) || die $dbh->errstr;

   }

##############################################################################

   sub updateDataSet {
  
    $sql="update $FileCatalogT set ";   
    $sql.="dataset='$mdataset'";
    $sql.=" WHERE runID = '$mrunID' AND fName like '%root' and path like '%$prodSr%' "; 
    print "$sql\n" if $debugOn;
    $rv = $dbh->do($sql) || die $dbh->errstr;
  
   }



#####=========================================================================
 sub sumInfo {

 my ($jb_sum,$useless) = @_;   
 my $sum_line ;

 my @word_sum;
 
 my @output = `more $jb_sum`; 
    foreach my $sum_line (@output) {
             chop $sum_line;

##### get node name
       if ($sum_line =~ /Starting job execution/) {
          @word_sum = split (" ", $sum_line);
          $mnodeId = $word_sum[11];
     }
##### get job status

     if ($sum_line =~ /Job status:/) {
         @word_sum = split (":", $sum_line);
          $mjobSt = $word_sum[1];
    } 

     if($sum_line =~ /Error message/)  {
         @word_sum = split (":", $sum_line); 
            $mjobDg = $word_sum[1];
    }

##### get number of events done
   
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
##### get chain
       if( $sum_line =~ /QAInfo:Requested chain is/ ) {
            @word_sum = split (":", $sum_line); 
              $pr_chain = $word_sum[2];
              $pr_chain =~ s/ /_/g;
         if ( $pr_chain =~ /^\s*_/ ) {
             my $mIndex = index $pr_chain, "_";
               $pr_chain = substr( $pr_chain, $mIndex+1);
              } 
           }
##### get max memory size during execution
          if ($sum_line =~ /Package   tree:/ ) {
            @word_sum = split (" ", $sum_line);
             $mmemSz = $word_sum[5];
         }
##### get CPU and REAL Time per event
       next if ($sum_line =~ /Command string/);
        if($sum_line =~ /Total: bfc/ ) {             
        @word_sum = split (" ", $sum_line);
#       print "CPU = ", $sum_line, "\n";   
        if($word_sum[8] =~ /Cpu/) { 
         $mCPU  = $word_sum[11];  
          $mRealT = $word_sum[6];                     
          }
        }
##### get everage number of tracks in the event

     if($sum_line =~ /QAinfo: Average number of tracks/) {
      @word_sum = split (" ", $sum_line) ;  
             $mNoTrk = $word_sum[5];
    }   
##### get everage number of vertex in the event

    if($sum_line =~ /QAinfo: Average number of vertices/) {     
        @word_sum = split (" ", $sum_line) ;
         $mNoVert = $word_sum[5]; 
    }
  }
 }


#####======================================================================
sub walkHpss {
   my ( $ftp, $dirs, $files ) = @_;

 my $month;
 my $day;
 my $year;
 my $name; 
 my @dirF;
 my $fullDir;
 my @parts;
 my @tk;
 my $fflag = 1;
 my $ppath;


   for ($ii=0; $ii<$nHpssDirs; $ii++) {
     print "Dir ".$dirs->[$ii]."\n" if $debugOn;
     my @dir = $ftp->dir($dirs->[$ii]);
     for ($jj=0; $jj<@dir; $jj++) {
       my @fields = split(/\s+/, $dir[$jj]);
         $access = $fields[0]; 
          $downer = $fields[2];
          $dsize  = $fields[4];
          $month  = $fields[5];
          $day    = $fields[6];
          $year   = $fields[7];
          $name   = $fields[8];
          $ppath  = $dirs->[$ii];
 
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
      
     if( $year > 98 ) {
       $year = 1900 + $year;
     } else {
	$year = 2000 + $year;
      }
      $fflag = 1;   
   
#     $timeS = sprintf ("%4.4d-%2.2d-%2.2d %2.2d:%2.2d:00",
#                        $year,$monthD,$day,$hr,$min);
   
      $timeS = sprintf ("%4.4d%2.2d%2.2d",
			$year,$monthD,$day);
      
      $fObjAdr = \(FileAttr->new());
      ($$fObjAdr)->filename($name);
      ($$fObjAdr)->fpath($ppath);
      ($$fObjAdr)->size($dsize);
      ($$fObjAdr)->timeS($timeS);
      ($$fObjAdr)->faccess($access);
      ($$fObjAdr)->fowner($downer);
      ($$fObjAdr)->iflag($fflag); 
      $files->[$nHpssFiles] = $fObjAdr;
      $nHpssFiles++;
     print "File ".$name."\n" if $debugOn;
#     print "Path:", $ppath, "\n";
      }
     }
   } 


