#! /opt/star/bin/perl -w
#
# 
#
# 
#
# dbupdateRECO.pl
#
# Update File Catalog with reconstruction  files missing and rerun
#
##############################################################################

use Mysql;
use File::Find;
use Net::FTP;
use Class::Struct;
use File::Basename;

require "/afs/rhic/star/packages/DEV00/mgr/dbCpProdSetup.pl";

my $prodSr = "mdc3"; 

my @Sets = (
            "auau200/nexus/default/b0_3/year_1h/hadronic_on",
             "auau200/mevsim/vanilla/central/year_1h/hadronic_on",
             "auau200/mevsim/vanilla/flow/year_1h/hadronic_on", 
             "auau200/mevsim/vanilla/resonance/year_1h/hadronic_on", 
             "auau200/mevsim/vanilla/trigger/year_1h/hadronic_on", 
#             "auau200/vni/default/b0_3/year_1h/hadronic_on",
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
             "pp200/pythia/default/minibias/year_2a/hadronic_on",
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
             "auau200/venus412/default/b0_3/year_1h/hadronic_on",
);


struct FileAttr => {
      filename  => '$',
      dset      => '$',
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
    gset        => '$',
    gname       => '$', 
    gpath       => '$',
    gsize       => '$',
    gtimeS      => '$',
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
my $DISK2 =  "/star/rcf/data03/reco";
my $DISKD = "/star/rcf";

my $prod_ext = "tfs_6";

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

########## Find reconstruction files in HPSS

for( $ll = 0; $ll<scalar(@Sets); $ll++) {
  $hpssRecoDirs[$ll] = $topHpssReco . "/" . $Sets[$ll] . "/" . $prod_ext;
  }
my $ftpHpss = Net::FTP->new("hpss.rcf.bnl.gov", Port => 2121, Timeout=>100)
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
my $nDiskFiles = 0;
my @diskRecoDirs;

my $inext =scalar(@Sets); 

for( $ll = 0; $ll<34; $ll++) { 
  $diskRecoDirs[$ll] = $DISK2 . "/" . $Sets[$ll] . "/tfs_6";
  print "diskRecoDir: $diskRecoDirs[$ll]\n";
}
for( $ii = 34; $ii< 41; $ii++) { 
$diskRecoDirs[$ii] = $DISK1 . "/" . $Sets[$ii] . "/tfs_6";
  print "diskRecoDir: $diskRecoDirs[$ii]\n";
}

print "\nFinding reco files in disk\n";
 
my $dflag;

foreach $diskDir (@diskRecoDirs) {
#  print "diskRecoDir: ", $diskDir, "\n";
  if (-d $diskDir) {
#  print "diskRecoDir_after: ", $diskDir, "\n";
  opendir(DIR, $diskDir) or die "can't open $diskDir\n";
  while( defined($flname = readdir(DIR)) ) {
     next if $flname =~ /^\.\.?$/;
     next if $flname =~ /geant.root/;

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
    $dflag = 1;    

    $fObjAdr = \(FileAttr->new());
    ($$fObjAdr)->filename($flname);
    ($$fObjAdr)->fpath($diskDir);
    ($$fObjAdr)->dset($set);
    ($$fObjAdr)->size($size);
    ($$fObjAdr)->timeS($timeS);
    ($$fObjAdr)->faccess($maccess);
    ($$fObjAdr)->fowner($mdowner);
    ($$fObjAdr)->iflag($dflag); 
    $hpssRecoFiles[$nHpssFiles] = $fObjAdr;
   $nHpssFiles++;
   $nDiskFiles++;
  }
closedir DIR;
}
}
print "Total reco files: $nDiskFiles\n";


###### Find reco files in Files Catalog
 &StDbProdConnect();


 $sql="SELECT dataset, fName, path, size, createTime FROM $FileCatalogT  WHERE fName LIKE '%root' AND jobID LIKE '%mdc3%' ";
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
        ($$fObjAdr)->gset($fvalue)      if( $fname eq 'dataset');
        ($$fObjAdr)->gname($fvalue)     if( $fname eq 'fName');
        ($$fObjAdr)->gpath($fvalue)     if( $fname eq 'path'); 
        ($$fObjAdr)->gsize($fvalue)     if( $fname eq 'size'); 
        ($$fObjAdr)->gtimeS($fvalue)    if( $fname eq 'createTime');
 }

   $jobIn_set[$jobIn_no] = $fObjAdr;
   $jobIn_no++;

 }

### select reco files status from JobStatus

 $sql="SELECT JobID, prodSeries, jobfileName, sumFileName, sumFileDir, jobStatus, NoEvents, CPU_per_evt_sec FROM $JobStatusT WHERE prodSeries = '$prodSr' AND jobStatus <> 'n/a'";

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


my @jobFSum_set;
my $jobFSum_no = 0;

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
#      print "Sum Dir = ", $jb_sumFile, "\n";
      $mjobDg = "none";
      $mjobSt = "n\/a"; 

          &sumInfo("$jb_sumFile",1);

#        if( $mCPU != $mcpuEvt)  {
       if ( $mNev != $mNoEvt )   {    
#      if ( $mjobSt ne $mjbStat) { 
     print  "JobFile=", $mjobFname, "Job Status: ", $mjobSt,  "Job Status Old: ", $mjbStat, "\n";

### update JobStatus table with info for jobs completed

      print "updating JobStatus table\n";
 
     &updateJSTable(); 

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
my $mdataSet = "n\/a";
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

my $dbset;
my $dbfname;
my $dbfsize;
my $dbctime;
my @parts;
my $gtime;
my $dbgtime;
my $ztime = "000000";
my $mpath;
my $fullName;


   foreach $eachRecoFile (@hpssRecoFiles) {

   $mdataSet  = ($$eachRecoFile)->dset;
   $mfName = ($$eachRecoFile)->filename;
   $mcTime  = ($$eachRecoFile)->timeS;
   $msize = ($$eachRecoFile)->size;
   $mpath = ($$eachRecoFile)->fpath;
   $fullName = $mpath . "/" . $mfName; 
   $flagHash{$fullName} = ($$eachRecoFile)->iflag;
#    print "Path = ", $fullName, "\n";
#   $mcTime = substr ($mgTime,-8) ;
#   print "Time = ",  $mcTime, "\n";
    foreach my $gtfile (@jobIn_set){
       $dbset = ($$gtfile)->gset;
       $dbfname = ($$gtfile)->gname;
       $dbfpath = ($$gtfile)->gpath;
       $dbfsize = ($$gtfile)->gsize;
       $dbgtime = ($$gtfile)->gtimeS;
       @parts = split (" ",$dbgtime);
       $gtime =  $parts[0];
       $gtime =~ s/-//g;
       $dbctime = $gtime;

       if ( ($mfName eq $dbfname) and ($mpath eq $dbfpath)) { 
# print "Name of dst on HPSS: ", $mpath, "Name of dst onDB:", $dbfpath,"\n";
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

   foreach $eachRecoFile (@hpssRecoFiles) {

## reinitialize variables

#  $gflag;
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
    
## end of reinitialization

   $mdataSet  = ($$eachRecoFile)->dset;
   $mfName = ($$eachRecoFile)->filename;
   $mpath  = ($$eachRecoFile)->fpath;
   $mcTime  = ($$eachRecoFile)->timeS;
   $mprotc = ($$eachRecoFile)->faccess;
   $mowner = ($$eachRecoFile)->fowner;
   $msize = ($$eachRecoFile)->size;
   $mName = $mfName; 
   $mName =~ m/(^[a-z0-9]+)_([0-9]+)_([0-9]+)/;
   $mfileSeq = $2 +0;
   $mrun = $1;
   $mrunId = substr($1,3) + 0;  
   if($mfName =~  /root/) {
     $mformat = "root";
     $compont = basename("$mfName",".root");
     if ($compont =~ m/\.([a-z0-9_]{3,})$/) {
     $mcomp = $1;
   }

  if($mpath =~ /starreco/)  {
   $msite = "hpss_rcf";
   $mhpss = "Y";
 } else {
   $msite = "disk_rcf";
   $mhpss = "N";
 } 
   $mtype = "MC_reco";
   $fullName = $mpath ."/" . $mfName;
   $gflag = $flagHash{$fullName}; 
     if( $gflag != 0 ) {
    foreach my $jobnm (@jobSum_set){
       $mproSr   = ($$jobnm)->prSer;
       $mJobId   = ($$jobnm)->job_id;
       $msumFile = ($$jobnm)->smFile;
       $mNevts   = ($$jobnm)->NoEvt;
       $mjobFname = ($$jobnm)->jbFile;
     my $jfile = $msumFile;
      $jfile =~ s/.sum//g;
      $mNevtLo = 1;
      $mNevtHi = $mNevts;
      $newset = $mdataSet;
      $newset =~ s/\//_/g;

       if ($mfName =~ /$jfile/ and $mjobFname =~ /$newset/) {

#  print "Set: ", $mdataSet, "File: ", $mfName, "Flag = ", $gflag, "\n";
if ( $gflag eq 1) {

   print "Files to be inserted :", "\n"; 
   print "Job ID: ", $mJobId, "Path: ", $mpath, "File: ", $mfName, "Date:", $mcTime,"\n"; 
     $NumMisFile++;
## fill Files Catalog with new RECO files
   print "Filling Files Catalog\n";
   &fillDbTable();
 }

elsif ( $gflag eq 2) {
    
 print "Files to be updated :", "\n";    
 print "Job ID: ", $mJobId, "Path: ", $mpath,"File:", $mfName, "Date:", $mcTime, "\n";
    $NumUpFile++; 
## update RECO files in Files Catalog 
   print "Updating Files Catalog\n";
   &updateDbTable();  

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

 &StDbProdDisconnect();

 exit;

###########
sub updateJSTable {

    $sql="update $JobStatusT set ";
    $sql.="jobStatus='$mjobSt',";
    $sql.="NoEvents='$mNev',";
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

###======================================================================
sub updateDbTable {

    $sql="update $FileCatalogT set ";
    $sql.="size='$msize',";
    $sql.="createTime='$mcTime',";
    $sql.="Nevents='$mNevts',";
    $sql.="NevLo='$mNevtLo',";
    $sql.="NevHi='$mNevtHi',";
    $sql.="owner='$mowner'";
    $sql.=" WHERE fName = '$mfName' AND dataset= '$mdataSet' AND path='$mpath'";
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


###======================================================================
sub walkHpss {
  my ( $ftp, $dirs, $files ) = @_;

 my $month;
 my $day;
 my $year;
 my $name; 
 my @dirF;
 my $fullDir;
 my $set; 
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
 
    my @dirF = split(/\//, $dirs->[$ii]); 

   my $set = sprintf("%s\/%s\/%s\/%s\/%s\/%s",$dirF[4],$dirF[5],$dirF[6],
                                                 $dirF[7],$dirF[8],$dirF[9]);


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
      ($$fObjAdr)->dset($set);
      ($$fObjAdr)->size($dsize);
      ($$fObjAdr)->timeS($timeS);
      ($$fObjAdr)->faccess($access);
      ($$fObjAdr)->fowner($downer);
      ($$fObjAdr)->iflag($fflag); 
      $files->[$nHpssFiles] = $fObjAdr;
      $nHpssFiles++;
      print "File ".$name."\n" if $debugOn;
#      print "Path:", $ppath, "Set:", $set,  "\n";
     }
    }
  } 


