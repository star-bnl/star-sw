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

require "/afs/rhic/star/packages/DEV00/mgr/dbCpProdSetup.pl";

my $debugOn=0;



###Set directories to be created for jobfiles
my $DISK1 = "/star/rcf/disk00001/star";
my $DISK2 =  "/star/rcf/data03/reco";
my $DISKD = "/star/rcf";

my $prodSr = "mdc3";
my $jobFDir = "/star/u2e/starreco/" . $prodSr ."/requests/";

my $topHpssReco  =  "/home/starreco/reco";

my @SetG = (
             "auau200/nexus/default/b0_3/year_1h/hadronic_on",
             "auau200/mevsim/vanilla/central/year_1h/hadronic_on",
             "auau200/mevsim/vanilla/flow/year_1h/hadronic_on", 
             "auau200/mevsim/vanilla/resonance/year_1h/hadronic_on", 
             "auau200/mevsim/vanilla/trigger/year_1h/hadronic_on", 
             "auau200/vni/default/b0_3/year_1h/hadronic_on",
             "auau200/vni/default/b0_3/year_1h1/hadronic_on", 
             "auau200/hijing/b0_3_jetq_off/jet05/year_1h/hadronic_on",
             "auau200/hijing/b0_3_jetq_on/jet05/year_1h/hadronic_on",
             "auau200/hijing/b8_15_jetq_off/jet05/year_1h/hadronic_on",
             "auau200/hijing/b8_15_jetq_on/jet05/year_1h/hadronic_on", 
             "auau200/hbt/default/peripheral/year_1h/hadronic_on",
             "auau200/hbt/default/midperipheral/year_1h/hadronic_on",
             "auau200/hbt/default/middle/year_1h/hadronic_on",
             "auau200/hbt/default/central/year_1h/hadronic_on",
             "auau200/hbt/default/midcentral/year_1h/hadronic_on",
             "auau200/starlight/2gamma/halffield/year_1h/hadronic_on",
             "auau200/starlight/2gamma/none/year_1h/hadronic_on",
             "auau200/starlight/vmeson/halffield/year_1h/hadronic_on",
             "auau200/starlight/vmeson/none/year_1h/hadronic_on",
             "auau200/dtunuc/two_photon/halffield/year_1h/hadronic_on",
             "auau200/dtunuc/two_photon/none/year_1h/hadronic_on",
             "auau200/hemicosm/default/none/year_1h/hadronic_on",
             "auau200/hijing/beamgas/hydrogen/year_1h/hadronic_on",
             "auau200/hijing/beamgas/nitrogen/year_1h/hadronic_on", 
             "pau200/hijing/b0_7/gam15/year_1h/hadronic_on",
             "pau200/hijing/b0_7/jet15/year_1h/hadronic_on", 
             "pau200/hijing/b0_7/gam15/year_2a/hadronic_on",
             "pau200/hijing/b0_7/jet15/year_2a/hadronic_on",
             "auau200/mevsim/vcascade/central/year_1h/hadronic_on",       
             "auau200/mevsim/vcascade/flow/year_1h/hadronic_on", 
             "auau200/mevsim/vcascade/fluct/year_1h/hadronic_on",
             "auau200/mevsim/vcascade/resonance/year_1h/hadronic_on", 
             "auau200/mevsim/vcascade/trigger/year_1h/hadronic_on",
             "auau200/mevsim/cascade/central/year_1h/hadronic_on",
             "auau200/mevsim/vanilla/fluct/year_1h/hadronic_on",
             "auau200/hijing_quark/b0_3_jetq_off/jet05/year_1h/hadronic_on",
             "auau200/hijing_quark/b0_3_jetq_on/jet05/year_1h/hadronic_on",
             "auau200/hijing_antinuc/b0_3_jetq_off/jet05/year_1h/hadronic_on",
             "auau200/hijing_antinuc/b0_3_jetq_on/jet05/year_1h/hadronic_on",
             "pp200/pythia/default/minibias/year_2a/hadronic_on",
);

my $SetD = "dst/prod5/1999/12";

my @recoDir = ("tfs_6", "trs_6");


struct JFileAttr => {
          prSer  => '$',
          job_id => '$', 
          smFile => '$',
          smDir  => '$',
          jbFile => '$',
          NoEvt  => '$',
          jobSt  => '$',  
          FstEvt => '$',
          LstEvt => '$',
		    };

 my $jbSt = "n\/a";
 my $eachRecoFile;
 my $eachDstFile;

 struct FileAttr => {
    filename  => '$',
    dset      => '$',
    fpath     => '$', 
    dsize     => '$',
    timeS     => '$',
    faccess   => '$',
    fowner    => '$',
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


my @jobSum_set;
my $jobSum_no = 0;
my @jobFSum_set;
my $jobFSum_no = 0;


############################################################################
###   Find reco files in HPSS

$nHpssFiles = 0;
my @hpssRecoFiles;
my @hpssRecoDirs;
my $kk = 0;
my $kl = 0;


for( $ll = 0; $ll<scalar(@SetG); $ll++) {
#  for ($kl = 0; $kl<scalar(@recoDir); $kl++) { 
  $hpssRecoDirs[$kk] = $topHpssReco ."/" .$SetG[$ll]. "/" . $recoDir[$kl];
    $kk++;

 print "hpssRecoDir: $hpssRecoDirs[$ll]\n" if $debugOn;
#print "hpssRecoDir: ", $hpssRecoDirs[$ll], "\n";
 }
#}

$nHpssDirs = scalar(@hpssRecoDirs);

$ftpReco = Net::FTP->new("hpss.rcf.bnl.gov", Port => 2121, Timeout=>200)
  or die "HPSS access failed";
$ftpReco->login("starreco","MockData") or die "HPSS access failed";

print "\nFinding reco files in HPSS\n"; 
 &walkHpss($ftpReco, \@hpssRecoDirs, \@hpssRecoFiles );
    print "Total files: ".@hpssRecoFiles."\n";
 $ftpReco->quit();


########## Find reco for daq files on HPSS

my $nDHpssFiles = 0;
my $nDHpssDirs = 1;
my @hpssDstDirs;
my @hpssDstFiles;

# $nHpssDaqDirs = scalar(@SetD);
 $nDHpssFiles = 0;

#for( $ll = 0; $ll<scalar(@SetD); $ll++) {
  $hpssDstDirs[0] = $topHpssReco . "/" . $SetD;
  print "hpssDstDir:", $hpssDstDirs[0], "\n";
#}

my $ftpRDaq = Net::FTP->new("hpss.rcf.bnl.gov", Port => 2121, Timeout=>100)
  or die "HPSS access failed";
$ftpRDaq->login("starreco","MockData") or die "HPSS access failed";

print "\nFinding daq DST files in HPSS\n"; 
&walkDHpss( $ftpRDaq, \@hpssDstDirs, \@hpssDstFiles );
print "Total files: ".@hpssDstFiles."\n";
 $ftpRDaq->quit();


########## Find reco files in disk
my $maccess; 
 my $mdowner;
 my $flname;
my $nDiskFiles = 0;
my @diskRecoDirs;

my $inext =scalar(@SetG); 

#for( $ll = 0; $ll<scalar(@SetG); $ll++) {
#  $diskRecoDirs[$ll] = $DISK1 . "/" . $SetG[$ll] . "/tfs_6";
#  print "diskRecoDir: $diskRecoDirs[$ll]\n" if $debugOn;
#}
for( $ll = 0; $ll< 34; $ll++) { 
  $diskRecoDirs[$ll] = $DISK2 . "/" . $SetG[$ll] . "/tfs_6";
  print "diskRecoDir: $diskRecoDirs[$ll]\n";
}
for( $ii = 34; $ii< 40; $ii++) { 
$diskRecoDirs[$ii] = $DISK1 . "/" . $SetG[$ii] . "/tfs_6";
  print "diskRecoDir: $diskRecoDirs[$ii]\n";
}


print "\nFinding reco files in disk\n";
 
foreach $diskDir (@diskRecoDirs) {
#  if (-d $diskDir) {
  opendir(DIR, $diskDir) or die "can't open $diskDir\n";
  while( defined($flname = readdir(DIR)) ) {
     next if $flname =~ /^\.\.?$/;
     next if $flname =~ /geant.root/;

#        @fields = split(/\s+/, $diskDir);
        $maccess = "-rw-r--r--"; 
        $mdowner = "starreco";

     $fullname = $diskDir."/".$flname;
   
     my @dirF = split(/\//, $diskDir); 
     my $set = sprintf("%s\/%s\/%s\/%s\/%s\/%s",$dirF[5],$dirF[6],$dirF[7],
                                                $dirF[8],$dirF[9],$dirF[10]);
    ($size, $mTime) = (stat($fullname))[7, 9];
    ($sec,$min,$hr,$dy,$mo,$yr) = (localtime($mTime))[0,1,2,3,4,5];
    $mo = sprintf("%2.2d", $mo+1);
    $dy = sprintf("%2.2d", $dy);
  
    if( $yr > 98 ) {
      $fullyear = 1900 + $yr;
    } else {
      $fullyear = 2000 + $yr;
    }


    $timeS = sprintf ("%4.4d%2.2d%2.2d",
                      $fullyear,$mo,$dy);
#  print "Set = ", $set, "File Name = ", $flname, "\n";
    
    $fObjAdr = \(FileAttr->new());
    ($$fObjAdr)->filename($flname);
    ($$fObjAdr)->fpath($diskDir);
    ($$fObjAdr)->dset($set);
    ($$fObjAdr)->dsize($size);
    ($$fObjAdr)->timeS($timeS);
    ($$fObjAdr)->faccess($maccess);
    ($$fObjAdr)->fowner($mdowner);
    $hpssRecoFiles[$nHpssFiles] = $fObjAdr;
   $nHpssFiles++;
   $nDiskFiles++;
  }
closedir DIR;
#}
}
print "Total reco files: $nDiskFiles\n";

###  find daq reco files on disk

my @diskDstDirs;
$nDiskFiles = 0;
#for( $ll = 0; $ll<scalar(@SetG); $ll++) {
  $diskDstDirs[0] = $DISKD . "/" . $SetD;
  print "diskDstDir: $diskDstDirs[0]\n" if $debugOn;
#}
print "\nFinding daq reco files in disk\n";
 
foreach $diskDir (@diskDstDirs) {
  opendir(DIR, $diskDir) or die "can't open $diskDir\n";
  while( defined($flname = readdir(DIR)) ) {
     next if $flname =~ /^\.\.?$/;

        $maccess = "-rw-r--r--"; 
        $mdowner = "starreco";

     $fullname = $diskDir."/".$flname;
   
     my @dirF = split(/\//, $diskDir); 
     my $set = sprintf("%s\/%s\/%s\/%s",$dirF[3],$dirF[4],$dirF[5],$dirF[6]);
 
#    print "Dst Set = ", $set, "\n";                                       
    ($size, $mTime) = (stat($fullname))[7, 9];

   ($sec,$min,$hr,$dy,$mo,$yr) = (localtime($mTime))[0,1,2,3,4,5];
    $mo = sprintf("%2.2d", $mo+1);
    $dy = sprintf("%2.2d", $dy);
  
    if( $yr > 98 ) {
      $fullyear = 1900 + $yr;
    } else {
      $fullyear = 2000 + $yr;
    }

    $timeS = sprintf ("%4.4d%2.2d%2.2d",
                      $fullyear,$mo,$dy);
#  print "Set = ", $set, "File Name = ", $flname, "\n";
    
    $fObjAdr = \(FileAttr->new());
    ($$fObjAdr)->filename($flname);
    ($$fObjAdr)->fpath($diskDir);
    ($$fObjAdr)->dset($set);
    ($$fObjAdr)->dsize($size);
    ($$fObjAdr)->timeS($timeS);
    ($$fObjAdr)->faccess($maccess);
    ($$fObjAdr)->fowner($mdowner);
     $hpssDstFiles[$nDHpssFiles] = $fObjAdr;
    $nDHpssFiles++;
    $nDiskFiles++;
  }
closedir DIR;
}

print "Total daq reco files: $nDiskFiles\n";

### connect to the DB

&StDbProdConnect();

### select from JobStatus table files which should be updated

 $sql="SELECT prodSeries, JobID,sumFileName, sumFileDir, jobfileName FROM $cpJobStatusT WHERE prodSeries = '$prodSr' AND jobStatus = 'n/a'";

  $cursor =$dbh->prepare($sql)
   || die "Cannot prepare statement: $DBI::errstr\n";
  $cursor->execute;
 
   while(@fields = $cursor->fetchrow) {
    my $cols=$cursor->{NUM_OF_FIELDS};
       $fObjAdr = \(JFileAttr->new());
 

   for($i=0;$i<$cols;$i++) {
    my $fvalue=$fields[$i];
      my $fname=$cursor->{NAME}->[$i];
#       print "$fname = $fvalue\n" ;

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
######### declare variables needed to fill the JobStatus table

my $msJobId = "n\/a";
my $mjobSt = "n\/a";
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
my $mjbDir = "new_jobs";

$jobFSum_no = 0;

foreach my $jobnm (@jobSum_set){
      $mproSr   = ($$jobnm)->prSer;
      $msJobId   = ($$jobnm)->job_id;
      $msumFile = ($$jobnm)->smFile;
      $msumDir  = ($$jobnm)->smDir;
      $mjobFname = ($$jobnm)->jbFile;
    my $jfile = $msumFile;
     $jfile =~ s/.sum//g;
     $first_evts = 0;
     $last_evts = 0;
     $mjobSt = "n\/a";
     $mNev  = 0;
     $mCPU = 0;
     $mRealT = 0; 
     $mmemSz = 0;
     $mNoTrk = 0;
     $mNoVert = 0;
     $mnodeId = "n\/a";

     @parts = split ("/",$msumDir);
 
    $JOB_DIR = $jobFDir . $parts[7];
#   print "Job Dir = ", $JOB_DIR, "\n";
 
   $jb_news = $JOB_DIR . "/new_jobs/" . $mjobFname;
   $jb_archive = $JOB_DIR . "/archive/" . $mjobFname;
   $jb_jobfile = $JOB_DIR . "/jobfiles/" . $mjobFname;
   $jb_hold = $JOB_DIR . "/jobs_hold/" . $mjobFname;
   if (-f $jb_news)     {$mjbDir = "new_jobs"};
   if (-f $jb_archive)  {$mjbDir = "archive"};
   if (-f $jb_jobfile)  {$mjbDir = "jobfiles"};
   if (-f $jb_hold)     {$mjbDir = "jobs_hold"};  

#    print "Sum Dir = ", $msumDir, "  SUM File = ", $msumFile, "\n";
   
      opendir(DIR, $msumDir) or die "can't open $msumDir\n";
    while( defined($filename = readdir(DIR)) ) {
      next if $filename =~ /^\.\.?$/;
        next if ( !($filename =~ /.sum$/) );
       if ( $filename =~ /$msumFile/ ) {
     $jb_sumFile = $msumDir . "/" . $msumFile;
#      print "Sum Dir = ", $jb_sumFile, "\n";
      $mjobDg = "none";
      $mjobSt = "n\/a"; 

          &sumInfo("$jb_sumFile",1);
     print "File name: ",$filename, "Sum dir: ", $msumDir, "JobFile=", $mjobFname, "Job Status: ", $mjobSt, "\n";

### update JobStatus table with info for jobs completed

      print "updating JobStatus table\n";
 
     &updateJSTable(); 

       $fObjAdr = \(JFileAttr->new());
       
       ($$fObjAdr)->prSer($mproSr);    
       ($$fObjAdr)->job_id($msJobId);  
       ($$fObjAdr)->smFile($msumFile);
       ($$fObjAdr)->jbFile($mjobFname);   
       ($$fObjAdr)->NoEvt($mNev);
       ($$fObjAdr)->FstEvt($first_evts);
       ($$fObjAdr)->LstEvt($last_evts);               

      $jobFSum_set[$jobFSum_no] = $fObjAdr;
      $jobFSum_no++; 
    
         last;
      }  else {
         next;
       }
     }   
         closedir DIR;

  }

######## declare variables needed to fill the database table
## for database filling

my $mJobId = "n\/a";
my $mrunId = 0;
my $mfileSeq = 0;
my $mevtType = 0;
my $mfName = "n\/a";
my $mpath  = "n\/a";
my $mdataSet = "n\/a";
my $msize = 0;
my $mcTime = 00-00-00;
my $mNevts = 0;
my $mNevtLo = 0;
my $mNevtHi = 0;
my $mowner = "n\/a";
my $mprotc = "-rw-r-----";
my $mtype = "n\/a";
my $mcomp = "n\/a";
my $mformat = "n\/a";
my $msite = "n\/a";
my $mhpss = "Y";
my $mstatus = 0;

# need to clear the FileCatalog table first here
# $sql="delete from $FilesCatalogTestT";
# $cursor =$dbh->prepare($sql)
#    || die "Cannot prepare statement: $DBI::errstr\n";
# $cursor->execute;

###=======================================================
## hpss reco file check

     foreach $eachRecoFile (@hpssRecoFiles) {

## reinitialize variables

  $mJobId = "n\/a"; 
  $mrunId = 0;
  $mfileSeq = 0;
  $mevtType = 0;
  $mfName = "n\/a";
  $mpath  = "n\/a";
  $mdataSet = "n\/a";
  $msize = 0;
  $mcTime = 00-00-00;
  $mNevts = 0;
  $mNevtLo = 0;
  $mNevtHi = 0;
  $mowner = "n\/a";
  $mprotc = "-rw-r-----";
  $mtype = "n\/a";
  $mcomp = "n\/a";
  $mformat = "n\/a";
  $msite = "n\/a";
  $mhpss = "Y";
  $mstatus = 0;
    
## end of reinitialization

my $mName;
my $jfile;
my $newset;

   $mdataSet  = ($$eachRecoFile)->dset;
   $mfName = ($$eachRecoFile)->filename;
   $mpath  = ($$eachRecoFile)->fpath;
   $mcTime  = ($$eachRecoFile)->timeS;
   $mprotc = ($$eachRecoFile)->faccess;
   $mowner = ($$eachRecoFile)->fowner;
   $msize = ($$eachRecoFile)->dsize;
   $mName = $mfName;
   $mName =~ m/(^[a-z0-9]+)_([0-9]+)_([0-9]+)/;  
   $mfileSeq = $2 ; 
   $mrun = $1;
   $mrunId = substr($1,3) + 0;    

   if($mfName =~ /dst.xdf/ ) {
     $mformat = "xdf";
     $mcomp = "dst";
   }
  elsif($mfName =~  /root/) {
     $mformat = "root";
     my $compont = basename("$mfName",".root");
     if ($compont =~ m/\.([a-z0-9_]{3,})$/) {
     $mcomp = $1;
   } 
   }
  if($mpath =~ /starreco/)  {
   $msite = "hpss_rcf";
   $mhpss = "Y";
 } else {
   $msite = "disk_rcf";
   $mhpss = "N";
 } 
   $mtype = "MC_reco";
foreach my $jobnm (@jobFSum_set){
      $mproSr   = ($$jobnm)->prSer;
      $msumFile = ($$jobnm)->smFile;
      $mJobId   = ($$jobnm)->job_id;
      $mjobFname = ($$jobnm)->jbFile;
      $mNevts = ($$jobnm)->NoEvt; 
      $mNevtLo = 1;
      $mNevtHi = $mNevts;
      $jfile = $msumFile;
      $jfile =~ s/.sum//g;
      $newset = $mdataSet;
      $newset =~ s/\//_/g;
    
     if ( $mfName =~ /$jfile/ and $mjobFname =~ /$newset/) {

#    print "File = ",$mfName, "Path = ", $mpath, "Job ID = ", $mJobId , "\n";
      print "updating cpFileCatalogT table\n";
 
     &fillDbTable();
         last;
      }  else {
         next;
       }
     }   
    
  }
###=======================================================
## hpss reco daq file check

my @flsplit;
my $mfileS;
my $extn;
my $mrun;

     foreach $eachDstFile (@hpssDstFiles) {

## reinitialize variables

  $mJobId = "n\/a"; 
  $mrunId = 0;
  $mfileSeq = 0;
  $mevtType = 999;
  $mfName = "n\/a";
  $mpath  = "n\/a";
  $mdataSet = "n\/a";
  $msize = 0;
  $mcTime = 00-00-00;
  $mNevts = 0;
  $mNevtLo = 0;
  $mNevtHi = 0;
  $mowner = "n\/a";
  $mprotc = "-rw-r-----";
  $mtype = "n\/a";
  $mcomp = "n\/a";
  $mformat = "n\/a";
  $msite = "n\/a";
  $mhpss = "Y";
  $mstatus = 0;
    
## end of reinitialization

my $dfile;

   $mdataSet  = ($$eachDstFile)->dset;
   $mfName = ($$eachDstFile)->filename;
   $mpath  = ($$eachDstFile)->fpath;
   $mcTime  = ($$eachDstFile)->timeS;
   $mprotc = ($$eachDstFile)->faccess;
   $mowner = ($$eachDstFile)->fowner;
   $msize = ($$eachDstFile)->dsize;

#  print "Path:  ", $mpath,  "File name:", $mfName, "\n";
   if($mfName =~ /dst.xdf/ ) {
     $mformat = "xdf";
     $mcomp = "dst";
   $basename = basename("$mfName",".xdf");
   }
  elsif($mfName =~  /root/) {
     $mformat = "root";
     $basename = basename("$mfName",".root");   
     my $compont = $basename;
     if ($compont =~ m/\.([a-z0-9_]{3,})$/) {
     $mcomp = $1;
   } 
   }
    if($basename =~ /^st_/ ) {
   @flsplit = split ("_",$basename);  
   $mfileS = $flsplit[4];
   $mrun =  $flsplit[2];
   $mrunId = $mrun;
   $extn = "." . $mcomp;
   $mfileSeq = basename("$mfileS","$extn"); 
 }
    else {
     $mfileSeq = 0;
     $mrunId = 0;
   }

  if ($mpath =~ /starreco/) {  
   $msite = "hpss_rcf";
   $mhpss = "Y";
 }else {
   $msite = "disk_rcf";
   $mhpss = "N";
 }
   $mtype = "daq_reco";
# print "Dst name = ",$mfName, " Nrun = ", $mrunId, "  FileSeq = ", $mfileSeq, "  component =", $mcomp, "\n";
foreach my $jobnm (@jobFSum_set){
      $mproSr   = ($$jobnm)->prSer;
      $msumFile = ($$jobnm)->smFile;
      $mJobId   = ($$jobnm)->job_id;
      $mNevts = ($$jobnm)->NoEvt;
      $mNevtLo =($$jobnm)->FstEvt;
      $mNevtHi =($$jobnm)->LstEvt;
      $mevtType = 999;
      $dfile = $msumFile;
      $dfile =~ s/.sum//g;
        
     if ( $mfName =~ /$dfile/) {
      print "updating cpFileCatalogT table\n";
 
#      &updateDbTable();
#      &fillDbTestTable();
#      &fillDbTable();   

       last;
      }  else {
         next;
       }
     }   
    
  }

# finished with data base
  &StDbProdDisconnect();

 exit;

################################################################################
sub updateJSTable {

    $sql="update $cpJobStatusT set ";
    $sql.="jobfileDir='$mjbDir',";
    $sql.="jobStatus='$mjobSt',";
    $sql.="NoEvents='$mNev',";
    $sql.="mem_size_MB='$mmemSz',";
    $sql.="CPU_per_evt_sec='$mCPU',";
    $sql.="avg_no_tracks='$mNoTrk',";
    $sql.="avg_no_vertex='$mNoVert',";
    $sql.="RealTime_per_evt='$mRealT',";
    $sql.="nodeID='$mnodeId'";
    $sql.=" WHERE sumFileName = '$msumFile' AND sumFileDir = '$msumDir' AND prodSeries = '$mproSr'";
    print "$sql\n" if $debugOn;
#    print "$sql\n";
    $rv = $dbh->do($sql) || die $dbh->errstr;
  
  }

#############################################################################
sub fillDbTable {

    $sql="insert into $cpFileCatalogT set ";
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
###============================================================
 sub updateDbTable {

       $sql="update $cpFileCatalogT set ";
       $sql.="NevLo='$mNevtLo',";
       $sql.="NevHi='$mNevtHi'";
       $sql.=" WHERE fName='$mfName' "; 
    print "$sql\n" if $debugOn;
    $rv = $dbh->do($sql) || die $dbh->errstr;

  }
###=======================================================

sub walkHpss {

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
                   
        @dirF = split(/\//, $dirs->[$ii]); 
         $set = sprintf("%s\/%s\/%s\/%s\/%s\/%s",$dirF[4],$dirF[5],$dirF[6],
                                                 $dirF[7],$dirF[8],$dirF[9]);
#   print "Set = ", $set, "\n";

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
  
       $timeS = sprintf ("%4.4d-%2.2d-%2.2d %2.2d:%2.2d:00",
                        $year,$monthD,$day,$hr,$min);
      
      $fObjAdr = \(FileAttr->new());
      ($$fObjAdr)->filename($name);
      ($$fObjAdr)->fpath($fullDir);
      ($$fObjAdr)->dset($set);
      ($$fObjAdr)->dsize($size);
      ($$fObjAdr)->timeS($timeS);
      ($$fObjAdr)->faccess($access);
      ($$fObjAdr)->fowner($downer);

      $files->[$nHpssFiles] = $fObjAdr;
      $nHpssFiles++;
      print "File ".$name."\n" if $debugOn;
#         print "File = ", $name,"\n";     
     }
    } 
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
 
  for ($ii=0; $ii<$nDHpssDirs; $ii++) {
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
         next if ( $name =~ /^ Star.Test/);
      
         @dirF = split(/\//, $dirs->[$ii]); 

        $set = sprintf("%s\/%s\/%s\/%s",$dirF[4],$dirF[5],$dirF[6],
                                                 $dirF[7]);
#      print "Set = ", $set, "\n";

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
   
   
      $timeS = sprintf ("%4.4d-%2.2d-%2.2d %2.2d:%2.2d:00",
                        $year,$monthD,$day,$hr,$min);
   
#      $timeS = sprintf ("%4.4d%2.2d%2.2d",
#                       $year,$monthD,$day);
      
      $fObjAdr = \(FileAttr->new());
      ($$fObjAdr)->filename($name);
      ($$fObjAdr)->fpath($fullDir);
      ($$fObjAdr)->dset($set);
      ($$fObjAdr)->dsize($size);
      ($$fObjAdr)->timeS($timeS);
      ($$fObjAdr)->faccess($access);
      ($$fObjAdr)->fowner($downer);

      $files->[$nDHpssFiles] = $fObjAdr;
      $nDHpssFiles++;
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

##get node name
             if ($sum_line =~ /Starting job execution/) {
              @word_sum = split (" ", $sum_line);
              $mnodeId = $word_sum[11];
	    }
##get job status

     if ($sum_line =~ /Job status:/) {
        @word_sum = split (":", $sum_line);
         $mjobSt = $word_sum[1];
     } 
      if ($sum_line =~ /Segmentation violation/) {
              $mjobDg = "Segmentation violation";
          }
     elsif ($sum_line =~ /buss error/) {
             $mjobDg = "bus_error";
        }  
    if($sum_line =~ /Error message/)  {
        @word_sum = split (":", $sum_line); 
           $mjobDg = $word_sum[1];
      }

## get number of events done
   
   if ($sum_line =~ /Number of Events Done/ ) {
     @word_sum = split (":", $sum_line);          
       $mNev = $word_sum[1];
   } 
	    if ( $sum_line =~ /First event/ ) {
                 @word_sum = split (":", $sum_line);          
       $first_evts = $word_sum[1];
	       }
            if ( $sum_line =~ /Last event/ ) {
                   @word_sum = split (":", $sum_line);          
       $last_evts = $word_sum[1];

		 }
##get chain
         if( $sum_line =~ /QAInfo:Requested chain is/ ) {
           @word_sum = split (":", $sum_line); 
             $pr_chain = $word_sum[2];
             $pr_chain =~ s/ /_/g;
            if ( $pr_chain =~ /^\s*_/ ) {
              my $mIndex = index $pr_chain, "_";
                $pr_chain = substr( $pr_chain, $mIndex+1);
               } 
              
            }
##get max memory size during execution
            if ($sum_line =~ /Package   tree:/ ) {
              @word_sum = split (" ", $sum_line);
                $mmemSz = $word_sum[5];
            }
## get CPU and REAL Time per event
             next if ($sum_line =~ /Command string/);
            if($sum_line =~ /Total: bfc/ ) {             
           @word_sum = split (" ", $sum_line);
#          print "CPU = ", $sum_line, "\n";   
            if($word_sum[8] =~ /Cpu/) { 
              $mCPU  = $word_sum[11];  
              $mRealT = $word_sum[6];  
           }
          }
## get everage number of tracks in the event

      if($sum_line =~ /QAinfo: Average number of tracks/) {
       @word_sum = split (" ", $sum_line) ;  
              $mNoTrk = $word_sum[5];
     }   
## get everage number of vertex in the event

     if($sum_line =~ /QAinfo: Average number of vertices/) {     
         @word_sum = split (" ", $sum_line) ;
          $mNoVert = $word_sum[5]; 
     }
   }
}

