#! /opt/star/bin/perl -w
#
# 
#
# 
#
#
######################################################################
#
# dbDaqOpera.pl
#
# Create db for DAQ production operation
#
#
use Net::FTP;
use Class::Struct;
use File::Basename;
use Class::Struct;

require "/afs/rhic/star/packages/SL99h/mgr/dbheader.pl";
require "/afs/rhic/star/packages/SL99h/mgr/dbsetup.pl";

require "dbDaqOperaSetup.pl";

my $debugOn = 0;


 my $prodPeriod   = "prod4";
 my $disk0        =  "/star/rcf/dst";
 my $topHpssSink  =  "/home/starsink/raw";
 my $topHpssReco  =  "/home/starreco/reco/dst";

struct DaqAttr => {
    daqName   => '$', 
    daqSize   => '$',
    daqDate   => '$',  
    daqEvts   => '$',
    daqfield  => '$',
    daqtrig   => '$', 
    daqbeam   => '$',
};

struct DstAttr => {
    dstName   => '$', 
    dstDir     => '$', 
    dstDSize   => '$',
    dstDDate   => '$',

};
  
my @daqInfo;
my $ndaqInfo = 0;

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


my @diskFiles;
my @hpssRecoFiles;
my @hpssRecoDirs;
my @diskRecoDirs;

my $nDiskFiles = 0;
my $nhpssFiles = 0;

########## Set variables to find jobs running
my @j_name;
my @j_status;
my $jline = 0;


&StDbConnect();


      $sql="SELECT $SystemData.$RunFileT.name as name,  $SystemData.$RunFileT.size as size,  $SystemData.$RunFileT.ctime as ctime,  $SystemData.$RunFileT.nrun as nrun,  $SystemData.$RunT.name as namer, $SystemData.$RunT.events as events, $SystemData.$RunT.field as dfield, $SystemData.$RunT.trig as trig, $SystemData.$RunT.beam as beam from $SystemData.$RunFileT, $SystemData.$RunT WHERE $SystemData.$RunFileT.hpss='Y' AND $SystemData.$RunFileT.nrun = $SystemData.$RunT.name AND $SystemData.$RunT.trig='cosmic' AND $SystemData.$RunT.events > 0";

      $cursor =$dbh->prepare($sql)
        || die "Cannot prepare statement: $DBI::errstr\n";
      $cursor->execute;

      my $nDbfiles = 0;
      while(@fields = $cursor->fetchrow) {
          my $cols=$cursor->{NUM_OF_FIELDS};
          $daqAddr = \(DaqAttr->new());

          for($i=0;$i<$cols;$i++) {
              my $fvalue=$fields[$i];
              my $fname=$cursor->{NAME}->[$i];
              ($$daqAddr)->daqName($fvalue) if( $fname eq 'name');
              ($$daqAddr)->daqSize($fvalue) if( $fname eq 'size');
              ($$daqAddr)->daqDate($fvalue) if( $fname eq 'ctime');
              ($$daqAddr)->daqEvts($fvalue) if( $fname eq 'events');
              ($$daqAddr)->daqfield($fvalue) if( $fname eq 'dfield');
              ($$daqAddr)->daqtrig($fvalue) if( $fname eq 'trig'); 
              ($$daqAddr)->daqbeam($fvalue) if( $fname eq 'beam');  

          }
           $daqInfo[$ndaqInfo] =$daqAddr; 
           $ndaqInfo++;
      }

#    }

&StDbDisconnect();

#===========================================================
# get list of dst files on disk
#

my $ii = 0;
my @daqf_list;
my @daq_dir;
my @words;
my $daq_name;
my $diskDir;
my $flname;
my $fullname;
my $ik;
my $dfile;


foreach $daqAddr (@daqInfo) {
        $dfile = ($$daqAddr)->daqName;
#       print $dfile, "\n";
         @words = split ("/", $dfile);
         $daq_dir[$ii] = $words[5] ."/".$words[6];
#        print $daq_dir[$ii], "\n";
          $ii++;
   }

  $ik = 1;
     $diskRecoDirs[0] = $disk0 . "/" . $prodPeriod . "/" . $daq_dir[0];
     $hpssRecoDirs[0] = $topHpssReco ."/" . $prodPeriod . "/" . $daq_dir[0];
 for( $ll = 1; $ll<scalar(@daq_dir); $ll++) {
       if(!($daq_dir[$ll] eq $daq_dir[$ll-1])) {
     $diskRecoDirs[$ik] = $disk0 . "/" . $prodPeriod . "/" . $daq_dir[$ll];
     $hpssRecoDirs[$ik] = $topHpssReco . "/" . $prodPeriod . "/" . $daq_dir[$ll];
      $ik++; 
    }
}
print "\nFind dst files on disk\n";

foreach $diskDir (@diskRecoDirs) {
  opendir(DIR, $diskDir) or die "can't open $diskDir\n";
  while( defined($filename = readdir(DIR)) ) {
    next if $filename =~ /^\.\.?$/;
    next if $filename =~ /.dst.xdf$/;
    next if $filename =~ /.event.root$/;
    next if $filename =~ /.hist.root$/;
      $fullname = $diskDir . "/" . $filename;
   
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
 
    my $dname = $filename; 
     $dname =~ s/.dst.root//g;
     @words = split ("/", $diskDir);
     $ddir = $words[2] . "/" . $words[3] . "/" . $words[4] . "/" . $words[5]; 

     $fObjAdr = \(DstAttr->new());
     ($$fObjAdr)->dstDir($ddir);
     ($$fObjAdr)->dstName($dname); 
     ($$fObjAdr)->dstDSize($size);
     ($$fObjAdr)->dstDDate($timeS);    

   $diskFiles[$nDiskFiles] = $fObjAdr;
    $nDiskFiles++;
  }
closedir DIR;
}
print "Total files: $nDiskFiles\n";

#=======================================================================
# get list of dst files on HPSS

my $nHpssDirs;

$nHpssDirs = scalar(@hpssRecoDirs);
$nhpssFiles = 0;

$ftpHpss = Net::FTP->new("rmds01.rhic.bnl.gov", Port => 2121, Timeout=>100)
  or die "HPSS access failed";
$ftpHpss->login("starreco","MockData") or die "HPSS access failed";

print "\nFind dst files in HPSS\n"; 
&walkHpss( $ftpHpss, \@hpssRecoDirs, \@hpssRecoFiles );
print "Total files: ".@hpssRecoFiles."\n";
$ftpHpss->quit();



## connect to the DB

&StDbDaqOperaConnect();

# need to clear the operation table first here
 $sql="delete from $DaqOperationT";
 $cursor =$dbh->prepare($sql)
    || die "Cannot prepare statement: $DBI::errstr\n";
$cursor->execute;

# declaration of variables needed to fill in DbTable

my $mdir = "unknown";
my $mdaqName = "n\/a";
my $mdaqSize = 0;
my $mdaqDate = "0000-00-00";   
my $mdaqEvts = 0; 
my $mdaqfield = 0; 
my $mdaqtrig = "n\/a"; 
my $mdaqbeam = "n\/a"; 
my $dst_disk_date = "0000-00-00";
my $dst_disk_size = 0;
my $dst_hpss_date = "0000-00-00";
my $dst_hpss_size = 0;
my $mchain = "n\/a";
my $mlibVer = "n\/a";
my $mjobStatus = "n\/a";
my $mdstEvts = 0;
my $msumFile = "no";
my $mjobFile = "no";
my $mmemSize = 0;
my $mcpu = 0;
my $mTrk = 0;
my $mVtx = 0;


## start loop over daq files on HPSS
my $dqfiles;

  foreach $dqfiles (@daqInfo) {

## reinitialize variables

 $mdir = "unknown";
 $mdaqName = "n\/a";
 $mdaqSize = 0;
 $mdaqDate = "0000-00-00";   
 $mdaqEvts = 0; 
 $mdaqfield = 0; 
 $mdaqtrig = "n\/a"; 
 $mdaqbeam = "n\/a"; 
 $dst_disk_date = "0000-00-00";
 $dst_disk_size = 0;
 $dst_hpss_date = "0000-00-00";
 $dst_hpss_size = 0;
 $mchain = "n\/a";
 $mlibVer = "n\/a";
 $mjobStatus = "n\/a";
 $mdstEvts = 0;
 $msumFile = "no";
 $mjobFile = "no";
 $mmemSize = 0;
 $mcpu = 0;
 $mTrk = 0;
 $mVtx = 0;

 my $dir_set;

       $dfile = ($$dqfiles)->daqName; 
        @words = split ("/", $dfile);
        $daq_name = $words[7];
        $dir_set =  "dst/" . $prodPeriod . "/" . $words[5] ."/".$words[6];
        $basename = basename("$daq_name",".daq");
        $mdaqName = $daq_name;
        $mdir = $dir_set;  
        $mdaqSize = ($$dqfiles)->daqSize;
        $mdaqDate = ($$dqfiles)->daqDate;
        $mdaqEvts = ($$dqfiles)->daqEvts;
        $mdaqfield = ($$dqfiles)->daqfield;
        $mdaqtrig = ($$dqfiles)->daqtrig;
        $mdaqbeam = ($$dqfiles)->daqbeam;

       if(!defined($mdaqfield)) {
         $mdaqfield = 0;
 } 
## check if jobfile is created
 my $run_chain = "daq";
 my $jSet;

        $jSet = $dir_set;
        $jSet =~ s/\//_/g;
        $jobfile_nm = $jSet . "_" . $basename;
        job_file($run_chain,$jobfile_nm);

## find attributes for dst files on disk

   foreach $mdiskFile (@diskFiles) {
       
      $mdstDiskDir = ($$mdiskFile)->dstDir; 
      next if ( $mdstDiskDir ne $mdir);
      $mdstDiskFile = ($$mdiskFile) -> dstName; 
       next if !($mdstDiskFile =~ /$basename/);
       $dst_disk_date = ($$mdiskFile)->dstDDate;
       $dst_disk_size = ($$mdiskFile)->dstDSize;
   }

##find attributes for dst files on HPSS

   foreach $mhpssfile (@hpssRecoFiles) {
             
      $mdstHpssDir = ($$mhpssfile)->dstDir; 
      next if ($mdstHpssDir ne $mdir);
      $mdstHpssFile = ($$mhpssfile) -> dstName; 
       next if !($mdstHpssFile =~ /$basename/);
       $dst_hpss_date = ($$mhpssfile)->dstDDate;
       $dst_hpss_size = ($$mhpssfile)->dstDSize;
   }

   print "filling operation table\n";
    &fillTable();
 }

&StDbDaqOperaDisconnect();

exit;     

#=========================================================================
sub fillTable {

     $sql="insert into $DaqOperationT set ";
     $sql.="fileName='$mdaqName',";
     $sql.="DstDir='$mdir',";
     $sql.="daq_size='$mdaqSize',";
     $sql.="daq_date='$mdaqDate',";
     $sql.="mfield='$mdaqfield',";
     $sql.="trig='$mdaqtrig',";
     $sql.="beam='$mdaqbeam',";
     $sql.="daqEvents='$mdaqEvts',";
     $sql.="HPSS_date='$dst_hpss_date',";
     $sql.="HPSS_size='$dst_hpss_size',";
     $sql.="disk_date='$dst_disk_date',";
     $sql.="disk_size='$dst_disk_size',";
     $sql.="chain='$mchain',";
     $sql.="LibVer='$mlibVer',";
     $sql.="jobStatus='$mjobStatus',";
     $sql.="dstEvents='$mdstEvts',";
     $sql.="summaryfile='$msumFile',";
     $sql.="jobfile='$mjobFile',";
     $sql.="mem_size_MB='$mmemSize',";
     $sql.="CPU_per_evt_sec='$mcpu',";
     $sql.="avg_no_tracks='$mTrk',";
     $sql.="avg_no_vertex='$mVtx'";  
     print "$sql\n" if $debugOn;
     $rv = $dbh->do($sql) || die $dbh->errstr;

  }

#==================================================================
sub job_file($$) {

my $run_ch = $_[0];
my $jfile_nm = $_[1];

my $prod_dir   = "/star/u2e/starreco/prod4/requests/";
my $jobf_dir   = $prod_dir . $run_ch . "/" . "jobfiles";
my $jarch_dir  = $prod_dir . $run_ch . "/" . "archive";
my $jnew_dir   = $prod_dir . $run_ch . "/" . "new_jobs"; 
my $jhold_dir  = $prod_dir . $run_ch . "/" . "jobs_hold";  
 
chdir $jhold_dir;
if (-f $jfile_nm) {$mjobFile = "jobs_hold"};  
chdir $jobf_dir;
if (-f $jfile_nm) {$mjobFile = "jobfiles"};
chdir $jarch_dir;
if (-f $jfile_nm) {$mjobFile = "archive"};
chdir $jnew_dir;
if (-f $jfile_nm) {$mjobFile = "new_jobs"};
 
}

#==================================================================
sub walkHpss {
  my ( $ftp, $dirs, $files ) = @_;

  for ($ii=0; $ii<$nHpssDirs; $ii++) {
#      print "Dir ".$dirs->[$ii]."\n";
    my @dird = $ftp->dir($dirs->[$ii]);
    for ($jj=0; $jj<@dird; $jj++) {
      my @fields = split(/\s+/, $dir[$jj]);
      my $size   = $fields[4];
      my $month  = $fields[5];
      my $day    = $fields[6];
      my $year   = $fields[7];
      my $dsname = $fields[8];

       next if $dsname =~ /.dst.xdf$/;
      next if $dsname =~ /.event.root$/; 
      next if $dsname =~ /.hist.root$/;

      my @dirF = split(/\//, $dirs->[$ii]); 

      my $dset = sprintf("%s\/%s\/%s\/%s",$dirF[3],$dirF[4],$dirF[5],
                                                 $dirF[6]);
      print "Dset ".$dset. "\n";
      my $monthD = $monthHash{$month};

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
   
   
#      $timeS = sprintf ("%4.4d%2.2d%2.2d%2.2d%2.2d%2.2d",
#                       $year,$monthD,$day,$hr,$min,$sec);
   
      $timeS = sprintf ("%4.4d%2.2d%2.2d",
                        $year,$monthD,$day);
      
      $fObjAdr = \(DstAttr->new());
      ($$fObjAdr)->dstName($dsname);
      ($$fObjAdr)->dstDir($dset);
      ($$fObjAdr)->dstDSize($size);
      ($$fObjAdr)->dstDdate($timeS);
      $files->[$nHpssFiles] = $fObjAdr;
      $nHpssFiles++;
#      print "File ".$dsname."\n" ;
    }
  }
}


