#! /opt/star/bin/perl -w
#
# 
#
# 
#
# L. Didenko 
######################################################################
#
# dbDstProd.pl     
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

require "/afs/rhic/star/packages/SL99i/mgr/dbDstProdSetup.pl";

my $debugOn = 0;


 my @prodPeriod   = ("prod4", "prod5");
 my $disk0        =  "/star/rcf/dst";
 my $topHpssSink  =  "/home/starsink/raw/daq";
 my $topHpssReco  =  "/home/starreco/reco/dst";
 my @sumDir ;
    $sumDir[0]   =  "/star/rcf/disk00001/star/" . $prodPeriod[0] . "/sum";

struct DaqAttr => {
    daqName   => '$',
    daqRun    => '$',   
    daqSeq    => '$',  
    daqEvts   => '$',
    daqfield  => '$',
    daqType   => '$', 
};

struct FilAttr => {
    filName   => '$', 
    filDir     => '$', 
    filSize   => '$',
    filDate   => '$',

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
my @hpssDaqDirs;
my @hpssDaqFiles;

my $nDiskFiles = 0;
my $nhpssFiles = 0;

########## Set variables to find jobs running
my @j_name;
my @j_status;
my $jline = 0;


&StDbConnect();


       $sql="SELECT $SystemData.$RunFileT.name as name, $SystemData.$RunFileT.nrun as nrun, $SystemData.$RunFileT.nseq as nseq, $SystemData.$RunT.name as namer, $SystemData.$RunT.events as events, $SystemData.$RunT.field as dfield, $SystemData.$RunT.trig as trig from $SystemData.$RunFileT, $SystemData.$RunT WHERE $SystemData.$RunFileT.hpss='Y' AND $SystemData.$RunFileT.nrun = $SystemData.$RunT.name AND $SystemData.$RunT.trig='cosmic' AND $SystemData.$RunT.events > 0";

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
              ($$daqAddr)->daqRun($fvalue) if( $fname eq 'nrun');    
              ($$daqAddr)->daqSeq($fvalue) if( $fname eq 'nseq');
              ($$daqAddr)->daqEvts($fvalue) if( $fname eq 'events');
              ($$daqAddr)->daqfield($fvalue) if( $fname eq 'dfield');
              ($$daqAddr)->daqType($fvalue) if( $fname eq 'trig');  

          }
           $daqInfo[$ndaqInfo] =$daqAddr; 
           $ndaqInfo++;
      }

#    }

 &StDbDisconnect();

#===========================================================
# get list of dst files on disk
#

my  $ii = 0;
my  @daqf_list;
my  @daq_dir;
my  @words;
my  $daq_name;
my  $diskDir;
my  $flname;
my  $fullname;
my  $ik;
my  $dfile;


foreach $daqAddr (@daqInfo) {
        $dfile = ($$daqAddr)->daqName;
#       print $dfile, "\n";
         @words = split ("/", $dfile);
         $daq_dir[$ii] = $words[5] ."/".$words[6];
#        print $daq_dir[$ii], "\n";
          $ii++;
   }

@daq_dir = ("1999/06", "1999/07", "1999/08");
  $ik = 1;
     $hpssDaqDirs[0] = $topHpssSink ."/" . $daq_dir[0];
     $diskRecoDirs[0] = $disk0 . "/" . $prodPeriod[0] . "/" . $daq_dir[0];
     $hpssRecoDirs[0] = $topHpssReco ."/" . $prodPeriod[0] . "/" . $daq_dir[0];
 for( $ll = 1; $ll<scalar(@daq_dir); $ll++) {
       if(!($daq_dir[$ll] eq $daq_dir[$ll-1])) {
     $hpssDaqDirs[$ik] = $topHpssSink ."/" . $daq_dir[$ll];
     $diskRecoDirs[$ik] = $disk0 . "/" . $prodPeriod[0] . "/" . $daq_dir[$ll];
     $hpssRecoDirs[$ik] = $topHpssReco . "/" . $prodPeriod[0] . "/" . $daq_dir[$ll];
      $ik++; 
    }
}
#### Find Daq files in HPSS

print "\nFind daq files on Hpss\n";

my $nHpssDirs;

 $nHpssDirs = scalar(@hpssDaqDirs);
 $nhpssFiles = 0;

 $ftpHpss = Net::FTP->new("rmds01.rhic.bnl.gov", Port => 2121, Timeout=>100)
   or die "HPSS access failed";
 $ftpHpss->login("starreco","MockData") or die "HPSS access failed";

 &walkHpss( $ftpHpss, \@hpssDaqDirs, \@hpssDaqFiles );
 print "Total files: ".@hpssDaqFiles."\n";
 $ftpHpss->quit();


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
      $ddir = $words[3] . "/" . $words[4] . "/" . $words[5] . "/" . $words[6]; 

     $fObjAdr = \(FilAttr->new());
     ($$fObjAdr)->filDir($ddir);
     ($$fObjAdr)->filName($dname); 
     ($$fObjAdr)->filSize($size);
     ($$fObjAdr)->filDate($timeS);    

    $diskFiles[$nDiskFiles] = $fObjAdr;
     $nDiskFiles++;
   }
 closedir DIR;
 }
print "Total files: $nDiskFiles\n";

#=======================================================================
#  get list of dst files on HPSS


 $nHpssDirs = scalar(@hpssRecoDirs);
 $nhpssFiles = 0;

 $ftpHpss = Net::FTP->new("rmds01.rhic.bnl.gov", Port => 2121, Timeout=>100)
   or die "HPSS access failed";
 $ftpHpss->login("starreco","MockData") or die "HPSS access failed";

 print "\nFind dst files in HPSS\n"; 
 &walkHpss( $ftpHpss, \@hpssRecoDirs, \@hpssRecoFiles );
 print "Total files: ".@hpssRecoFiles."\n";
 $ftpHpss->quit();

##  find running jobs


   my @CRS_JOB = `ssh rcf.rhic.bnl.gov crs_node_status.pl -c`;
   foreach my $job_line (@CRS_JOB) {
      chop $job_line; 
     my @job_word = split ("_", $job_line);
     my @word_part = split ("%", $job_word[10]);
     $j_name[$jline] = $job_word[6]."_". $job_word[7]."_". $job_word[8]."_". $job_word[8]."_". $word_part[0];
     $j_status[$jline] = $word_part[1];
      $jline++; 
  } 



##  connect to the DB

&StDbDstProdConnect();

# need to clear the operation table first here
  $sql="delete from $DstProductionT";
  $cursor =$dbh->prepare($sql)
     || die "Cannot prepare statement: $DBI::errstr\n";
 $cursor->execute;

# declaration of variables needed to fill in DbTable

 my $mnrun = 0;
 my $mfileSeq = 0;
 my $meventType = 0;
 my $mprodSer = "n\/a";
 my $mdaqName = "n\/a";
 my $mdir = "unknown";
 my $mdaqEvts = 0;
 my $mdaqDate = "0000-00-00";
 my $mdaqSize = 0; 
 my $mdaqfield = 0;   
 my $dst_disk_date = "0000-00-00";
 my $dst_disk_size = 0;
 my $dst_hpss_date = "0000-00-00";
 my $dst_hpss_size = 0;
 my $mjobStatus = "n\/a";
 my $mdstEvts = 0;
 my $msumFile = "no";
 my $mjobFile = "no";
 my $mjobName = "n\/a";
 my $mmemSize = 0;
 my $mcpu = 0;
 my $mTrk = 0;
 my $mVtx = 0;
 my $mnode = "n\/a";


##  start loop over daq files on HPSS
my $dqfiles;
my $mdaqhpss;
my $dir_set;
my $dir_dset;
my $mdaqhpssF;
my $mdaqhpssDr;
my $mdaqhpssFl; 

   foreach $dqfiles (@daqInfo) {

##  reinitialize variables

  $mnrun = 0;
  $mfileSeq = 0;
  $meventType = 0;
  $mprodSer = $prodPeriod[0];
  $mdaqName = "n\/a";
  $mdir = "unknown";
  $mdaqEvts = 0;
  $mdaqSize = 0;
  $mdaqDate = "0000-00-00";   
  $mdaqfield = 0;  
  $dst_disk_date = "0000-00-00";
  $dst_disk_size = 0;
  $dst_hpss_date = "0000-00-00";
  $dst_hpss_size = 0;
  $mjobStatus = "n\/a";
  $mdstEvts = 0;
  $msumFile = "no";
  $mjobFile = "no";
  $mjobName = "n\/a"; 
  $mmemSize = 0;
  $mcpu = 0;
  $mTrk = 0;
  $mVtx = 0;
  $mnode = "n\/a";

my $dir_set;
my $dir_dset;

      $dfile = ($$dqfiles)->daqName; 
       @words = split ("/", $dfile);
       $daq_name = $words[7];
       $dir_dset = "daq/" . $words[5] ."/".$words[6] ."/";
       $basename = basename("$daq_name",".daq");
       $mdaqName = $daq_name;
       $mnrun = ($$dqfiles)->daqRun;
       $mfileSeq = ($$dqfiles)->daqSeq; 
       $mdaqEvts = ($$dqfiles)->daqEvts;
       $mdaqfield = ($$dqfiles)->daqfield;
       $meventType = 999;                    # ($$dqfiles)->daqType;  
#      print $mdaqName, "\n";
      if(!defined($mdaqfield)) {
        $mdaqfield = 0;
  } 

## get date and size for daq files
    foreach $mdaqhpssF (@hpssDaqFiles) {
          $mdaqhpssDr = ($$mdaqhpssF)->filDir; 
#     print "dir set: ", $dir_dset, "\n";
	  if ($mdaqhpssDr eq $dir_dset) {
       $mdaqhpssFl = ($$mdaqhpssF) -> filName; 
       if ($mdaqhpssFl eq $mdaqName) {
#       print "file name: ",$mdaqhpssFl, "\n";   
          $mdaqDate = ($$mdaqhpssF)->filDate;
          $mdaqSize = ($$mdaqhpssF)->filSize; 
       }
      }
    }
## loop over production series

    $dir_set =  "dst/" . $prodPeriod[0] . "/" . $words[5] ."/".$words[6];
    $mdir = $dir_set; 

##  find attributes for dst files on disk

    foreach $mdiskFile (@diskFiles) {
       
       $mdstDiskDir = ($$mdiskFile)->filDir; 
       next if ( $mdstDiskDir ne $mdir);
#     print $mdstDiskDir, "\n";
       $mdstDiskFile = ($$mdiskFile) -> filName; 
        next if !($mdstDiskFile =~ /$basename/);
#     print $mdstDiskFile, "\n";  
        $dst_disk_date = ($$mdiskFile)->filDate;
        $dst_disk_size = ($$mdiskFile)->filSize;
    }
## find attributes for dst files on HPSS

    foreach $mhpssfile (@hpssRecoFiles) {
             
       $mdstHpssDir = ($$mhpssfile)->filDir; 
       next if ($mdstHpssDir ne $mdir);
       $mdstHpssFile = ($$mhpssfile) -> filName; 
        next if !($mdstHpssFile =~ /$basename/);
#     print $mdstHpssFile, "\n";

        $dst_hpss_date = ($$mhpssfile)->filDate;
        $dst_hpss_size = ($$mhpssfile)->filSize;
    }

##  check if jobfile is created
  my $run_chain = "daq";
  my $jSet;

         $jSet = $dir_set;
         $jSet =~ s/\//_/g;
         $jobfile_nm = $jSet . "_" . $basename;
         $mjobName = $jobfile_nm;  
#      print "Jobfile name: ",$jobfile_nm, "\n"; 
         job_file($prodPeriod[0], $run_chain,$jobfile_nm);

##  summary info check
 my $sumDirDaq;

     $sumDirDaq = $sumDir[0] . "/daq";
     opendir(DIR, $sumDirDaq) or die "can't open $sumDirDaq\n";
     while( defined($filename = readdir(DIR)) ) {
       next if $filename =~ /^\.\.?$/;
       next if ( !($filename =~ /.sum$/) );

       if ( $filename =~ /$basename/ ) {
         $msumFile = "yes";
         my $summ_File = "$sumDirDaq/$basename\.sum";
         &sum_info("$summ_File",1);
         last;
   }
      else {
        next;
       }
    } 
     closedir DIR;

#  check if job is running

    if($mjobStatus eq "n\/a") {
       my $jj = 0;
      foreach my $job_fname (@j_name) {
     if( $job_fname =~ /$basename/ ) {
       $mjobStatus = $j_status[$jj];
#    print $job_fname, "\n";
#    print $basename, "\n";
#    print $mjobStatus, "\n";
   last;
 }
    else {   
     next; 
   } 
       $jj++;
  }
 }
    print "filling operation table\n";
     &fillTable();
 
## should be end of loop
 }

 &StDbDstProdDisconnect();

 exit;     

#=========================================================================
 sub fillTable {


      $sql="insert into $DstProductionT set ";
      $sql.="nrun='$mnrun',";
      $sql.="fileSeq='$mfileSeq',";
      $sql.="eventType='$meventType',";
      $sql.="prodSeries='$mprodSer',";
      $sql.="fileName='$mdaqName',"; 
      $sql.="DstDir='$mdir',";
      $sql.="daqEvents='$mdaqEvts',";
      $sql.="daq_date='$mdaqDate',";
      $sql.="daq_size='$mdaqSize',";
      $sql.="disk_date='$dst_disk_date',";
      $sql.="disk_size='$dst_disk_size',";
      $sql.="HPSS_date='$dst_hpss_date',";
      $sql.="HPSS_size='$dst_hpss_size',";
      $sql.="mfield='$mdaqfield',";
      $sql.="dstEvents='$mdstEvts',";
      $sql.="jobStatus='$mjobStatus',";
      $sql.="summaryfile='$msumFile',";
      $sql.="jobfile_dir='$mjobFile',";
      $sql.="mem_size_MB='$mmemSize',";
      $sql.="CPU_per_evt_sec='$mcpu',";
      $sql.="avg_no_tracks='$mTrk',";
      $sql.="avg_no_vertex='$mVtx'";
      $sql.="jobfile_name='$mjobName'"; 
      $sql.="node='$mnode'";  
      print "$sql\n" if $debugOn;
      $rv = $dbh->do($sql) || die $dbh->errstr;

   }

#================================================================== 
 sub job_file($$$) {

 my $prodPer = $_[0];
 my $run_ch = $_[1];
 my $jfile_nm = $_[2];

 my $prod_dir = "/star/u2e/starreco/" .$prodPer .  "/requests/";
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
 sub sum_info  {

 my ($jb_sum,$useless) = @_;   
 my $sum_line ;
 my @word_sum;
 my $mlibTag;
 
 my @output = `more $jb_sum`; 
   foreach my $sum_line (@output) {
            chop $sum_line;

# get node name
	    if ($sum_line =~ /Starting job execution/) {
             @word_sum = split (" ", $sum_line);
             $mnode = $word_sum[11];
	   }

#  get job status
      if ($sum_line =~ /Segmentation violation/) {
              $mjobStatus = "crash";
          }
     elsif ($sum_line =~ /bus error/) {
             $mjobStatus = "bus_err";
        }  
     elsif ($sum_line =~ /Job status:/) {
        @word_sum = split (":", $sum_line);
          $mjobStatus = $word_sum[1];
        } 

#   get number of events done
   
     if ($sum_line =~ /Number of Events Done/ ) {
       @word_sum = split (":", $sum_line);          
         $mdstEvts = $word_sum[1];
     } 
       
#  get library tag
          if ($sum_line =~ /Library version/)  {
           @word_sum = split (":", $sum_line); 
            $mlibVer = $word_sum[1];
         }
        if ($sum_line =~ /from Tag/) {
          @word_sum = split (" ", $sum_line) ;       
            $mlibTag = $word_sum[9];    
        }
#  get chain
           if( $sum_line =~ /QAInfo:Requested chain is/ ) {
             @word_sum = split (":", $sum_line); 
               $mchain = $word_sum[2];
               $mchain =~ s/ /_/g;
              if ( $mchain =~ /^\s*_/ ) {
                   my $mIndex = index $mchain, "_";
                  $mchain = substr( $mchain, $mIndex+1);
               } 

            }
#  get max memory size during execution
            if ($sum_line =~ /Package   tree:/ ) {
              @word_sum = split (" ", $sum_line);
                $mmemSize = $word_sum[5];
            }
#  get CPU per event
            if($sum_line =~ /bfc/ ) {
             @word_sum = split (" ", $sum_line);
            if($word_sum[7] =~ /Cpu/) {
               $mcpu = $word_sum[10];  
           }
         }
         
#   get everage number of tracks in the event

      if($sum_line =~ /QAinfo: number of tracks/) {
       @word_sum = split (" ", $sum_line) ;  
        $mTrk = $word_sum[4];
     }   
#   get everage number of vertex in the event

      if($sum_line =~ /QAinfo: number of vertices/) {     
          @word_sum = split (" ", $sum_line) ;
           $mVtx = $word_sum[4]; 
      }

    }
#   define if not defined

         if (!defined $job_status)  {  
          $job_status = "Not defined"; 
        }

            if(!defined $lb_tag)  {
           $lb_tag = $lb_ver;
         }
 
 }

#==================================================================
 sub walkHpss {
   my ( $ftp, $dirs, $files ) = @_;

   for ($ii=0; $ii<$nHpssDirs; $ii++) {
#       print "Dir ".$dirs->[$ii]."\n";
     my @dird = $ftp->dir($dirs->[$ii]);
     for ($jj=0; $jj<@dird; $jj++) {
       my @fields = split(/\s+/, $dird[$jj]);
       my $size   = $fields[4];
       my $month  = $fields[5];
       my $day    = $fields[6];
       my $year   = $fields[7];
       my $dsname = $fields[8];

        next if $dsname =~ /.dst.xdf$/;
       next if $dsname =~ /.event.root$/; 
       next if $dsname =~ /.hist.root$/;

       my @dirF = split(/\//, $dirs->[$ii]); 

 my $dset;
       if($dirF[4] eq "daq")  {
        $dset = sprintf("%s\/%s\/%s\/%s",$dirF[4],$dirF[5],$dirF[6]);
      }
        elsif ($dirF[4] eq "dst") {
        $dset = sprintf("%s\/%s\/%s\/%s",$dirF[4],$dirF[5],$dirF[6],
                                                 $dirF[7]);
      }
#       print "Dset ".$dset. "\n";
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
   
   
#       $timeS = sprintf ("%4.4d%2.2d%2.2d%2.2d%2.2d%2.2d",
#                        $year,$monthD,$day,$hr,$min,$sec);
   
       $timeS = sprintf ("%4.4d%2.2d%2.2d",
                         $year,$monthD,$day);
      
       $fObjAdr = \(FilAttr->new());
       ($$fObjAdr)->filName($dsname);
       ($$fObjAdr)->filDir($dset);
       ($$fObjAdr)->filSize($size);
       ($$fObjAdr)->filDate($timeS);
       $files->[$nhpssFiles] = $fObjAdr;
       $nhpssFiles++;
#       print "File ".$dsname."\n" ;
#       print "Size ".$size."\n" ;
     }
   }
 }


