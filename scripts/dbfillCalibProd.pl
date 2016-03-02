#!/usr/bin/env perl
#
#  $Id:
#
# dbfillCalibProd.pl - script to update CalibJobStatus table
# Requires 4 arguments: production series, name of trigset,  calibration tag, last createtime (optionl).
# Example of usage: dbfillCalibProd2011.pl P11id AuAu19_production TOF 2011-08-09 
#
# L.Didenko
############################################################################

use Class::Struct;
use File::Basename;
use Compress::Zlib;
use Time::Local;
use DBI;

$dbhost="duvall.star.bnl.gov";
$dbuser="starreco";
$dbpass="";
$dbname="operation";

$FileCatalogT = "FileCatalog2013";
$JobStatusT = "CalibJobStatus";

my $debugOn=0;

my $DISK1 = "/star/rcf/prodlog";
my $prodSr = $ARGV[0]; 
my $trig = $ARGV[1];
my $comcalib = $ARGV[2];
my $bldisk = "data16";

my $lastDTime;

my $jobFDir = "/star/u/starreco/" . $prodSr ."/requests/";

my $topHpssReco  =  "/home/starreco/reco";

my $Startfile = $trig ."_calib.log";

# if( -f $Startfile) { `rm  $Startfile`};

my $recoDir = ("daq");

struct JFileAttr => {
          prSer  => '$', 
          smFile => '$',
          smDir  => '$',
          jbFile => '$',
          NoEvt  => '$',
          sbtime => '$',
          jobSt  => '$',  
		    };
 
 struct FileAttr => {
    filename  => '$',
    fpath     => '$',
    dsize     => '$',
    timeS     => '$',
    mdisk     => '$'
                  };

 struct RunAttr => {
        drun   => '$',
        dtSet  => '$',
        dtTrg  => '$',
        dpath  => '$',
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

my @DISK = (
            "/star/data96/reco",
            "/star/data97/reco",
            "/star/data98/reco",

  );


my @jobSum_set;
my $jobSum_no = 0;
my $jbSt = "n/a";

 my @dirSet;
 my $ndirSet = 0;

my $temTime;
my $temDate;
my $temHour;
my $crtime;
my @prtk = ();
my @wrdk = ();

   &StDbProdConnect();


     $sql="SELECT max(createTime) FROM $JobStatusT WHERE jobfileName like '$trig%$prodSr%' AND prodSeries = '$prodSr'  and calibTag = '$comcalib' ";

     $cursor =$dbh->prepare($sql)
      || die "Cannot prepare statement: $DBI::errstr\n";
     $cursor->execute;

        my $crTime = $cursor->fetchrow;
          $cursor->finish;

 if( !defined $crTime) {

   $crTime = $ARGV[3]." 00:00:00";
  };

 print "Max time:  ",$crTime, "\n";

   @prtk = ();
   $temTime  = $crTime;
   @prtk = split(" ",$temTime);
   $temDate = $prtk[0];
   $temDate =~ s/-//g;
   $temHour = $prtk[1];
   @wrdk = ();
   @wrdk = split(":",$temHour);

#  $lastDTime = $temDate . $wrdk[0].$wrdk[1];

   $lastDTime = $temDate . $wrdk[0]."00";

 $lastDTime = "201308100000";

 print "Last Date: ", $lastDTime, "\n" ;

  $sql="SELECT DISTINCT path, dataset, trigset FROM $FileCatalogT WHERE trigset = '$trig' AND fName like '%daq' ";

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

      ($$fObjAdr)->dpath($fvalue)     if( $fname eq 'path');
      ($$fObjAdr)->dtSet($fvalue)     if( $fname eq 'dataset');
      ($$fObjAdr)->dtTrg($fvalue)     if( $fname eq 'trigset');

         }
       $dirSet[$ndirSet] = $fObjAdr;
        $ndirSet++;
      }

 my $ndstFile = 0;
 my @dstDirs = ();
 my @dstFiles = ();

my $SetI = "/home/starreco/reco/MinBias/FullField/P01gk/2001/180";
my $SetH;
my @SetD = ();
my $fieldD;
my $trigD;
my $pathD;
my $dataS;
my $k = 0;
my $ik = 0;
my $kk = 0;
my %dirflag = ();
my $flname;
my $fullname;
my $size = 0;
my $rdisk = 'n/a';
my $mTime = "0000-00-00 00:00:00";
my $timeS = "0000-00-00 00:00:00";
my @wrd = ();
my $fullyear = 0;
my $mfile;


  foreach my $dSet (@dirSet) {
   $trigD = ($$dSet)->dtTrg;
   $dataS = ($$dSet)->dtSet;
   $pathD = ($$dSet)->dpath;
   @prtk = ();
   @prtk = split("daq", $pathD);
   $DirD = $prtk[1];
   @wrd = ();
   @wrd = split("_",$dataS);
   $fieldD = $wrd[1];
   $SetH = $trigD . "/" . $fieldD . "/" . $prodSr ."_".$comcalib.$DirD;
#  print $SetH, "\n";
 next if($SetH eq $SetI);
   $SetI = $SetH;
   $SetD[$kk] = $SetH;
   $kk++;
   $dirflag{$SetH} = 0;
   for ($l = 0; $l < $kk; $l++) {
     if ($SetH eq $SetD[$l]) {
   $dirflag{$SetH}++;
     }
   }
   next if ($dirflag{$SetH} >= 2 );
  $dstDirs[$k] = $DISK[0] . "/" . $SetD[$kk-1];

#  print "Disk: ", $dstDirs[$k], "\n";
  $k++;
}

 my $ndisk1 = $k ;
 my $nnd = 0;

 for (my $nk = 1; $nk < scalar(@DISK); $nk++) {
    $nnd = $ndisk1*$nk;
 for ($ik = 0; $ik < $ndisk1; $ik++)  {
    $dstDirs[$nnd + $ik] = $dstDirs[$ik];
    $dstDirs[$nnd + $ik] =~ s|$DISK[0]|$DISK[$nk]|g;
#   print "Check disk dirs ", $dstDirs[$nnd + $ik], "\n";
   }
  }

$ndstFile = 0;


   foreach my $diskDir (@dstDirs) {
   if (-d $diskDir) {
#       print $diskDir, "\n";
    opendir(DIR, $diskDir) or die "can't open $diskDir\n";
    while( defined($flname = readdir(DIR)) ) {
       next if $flname =~ /^\.\.?$/;

       $fullname = $diskDir."/".$flname;
       my @dirF = split(/\//, $diskDir);
#       my $set = sprintf("%s\/%s\/%s\/%s",$dirF[4],$dirF[5],$dirF[6],$dirF[7]);

#      print "Path name = ",$fullname,"   ",$dirF[2],"\n";
      ($size, $mTime) = (stat($fullname))[7, 9];
      ($sec,$min,$hr,$dy,$mo,$yr) = (localtime($mTime))[0,1,2,3,4,5];

      if( $yr > 96 ) {
        $fullyear = 1900 + $yr;
      } else {
        $fullyear = 2000 + $yr;
      }

      $mo = sprintf("%2.2d", $mo+1);
      $dy = sprintf("%2.2d", $dy);

      $timeS = sprintf ("%4.4d-%2.2d-%2.2d %2.2d:%2.2d:00",
                         $fullyear,$mo,$dy,$hr,$min);

      my $dkTime = sprintf ("%4.4d%2.2d%2.2d%2.2d%2.2d",
                      $fullyear,$mo,$dy,$hr,$min);

      $rdisk = $dirF[2]; 

      if($flname =~ /MuDst.root/ or $flname =~ /event.root/ ) {

       if($dkTime >= $lastDTime ) {
     print "File to be processed from disk: ", $fullname, "  %  ",$timeS ,"  %  ", $dkTime,"\n";

      $fObjAdr = \(FileAttr->new());
      ($$fObjAdr)->filename($flname);
      ($$fObjAdr)->fpath($diskDir);
      ($$fObjAdr)->dsize($size);
      ($$fObjAdr)->mdisk($rdisk);
      $dstFiles[$ndstFile] = $fObjAdr;
     $ndstFile++;
         }
       }
     }
  closedir DIR;
    }
   }
 
  print "Total reco files: $ndstFile\n";


#####  select from JobStatus table files which should be updated

 $sql="SELECT prodSeries, logfileName, logfileDir, jobfileName, jobStatus, NoEvents, submitTime FROM $JobStatusT WHERE prodSeries = '$prodSr' AND jobfileName like '$trig%$prodSr_$comcalib%' AND calibTag = '$comcalib' AND jobStatus <> 'Done'" ;


    $cursor =$dbh->prepare($sql)
      || die "Cannot prepare statement: $DBI::errstr\n";
     $cursor->execute;
 
      while(@fields = $cursor->fetchrow) {
       my $cols=$cursor->{NUM_OF_FIELDS};
          $fObjAdr = \(JFileAttr->new());
 
     for($i=0;$i<$cols;$i++) {
       my $fvalue=$fields[$i];
         my $fname=$cursor->{NAME}->[$i];
#         print "$fname = $fvalue\n" ;


          ($$fObjAdr)->prSer($fvalue)    if( $fname eq 'prodSeries');
          ($$fObjAdr)->smFile($fvalue)   if( $fname eq 'logfileName'); 
          ($$fObjAdr)->smDir($fvalue)    if( $fname eq 'logfileDir'); 
          ($$fObjAdr)->jbFile($fvalue)   if( $fname eq 'jobfileName');
          ($$fObjAdr)->jobSt($fvalue)    if( $fname eq 'jobStatus');
          ($$fObjAdr)->NoEvt($fvalue)    if( $fname eq 'NoEvents');
          ($$fObjAdr)->sbtime($fvalue)   if( $fname eq 'submitTime');
     }

         $jobSum_set[$jobSum_no] = $fObjAdr;
         $jobSum_no++; 
 }
 
 my $jb_news;
 my $jb_archive;
 my $jb_jobfile;
 my $jb_loop;
 my $jb_lost;
 my $mjobFname;
 my $JOB_DIR;
 my $mfName;

 my @prt = (); 
 my $daqFile;

#########  declare variables needed to fill the JobStatus table

 my $mjobSt = "n/a";
 my $mNev  = 0;
 my $mCPU = 0;
 my $mRealT = 0;
 my $mNoTrk = 0;
 my $mNoVert = 0;
 my $mnodeId = "n/a";
 my $jb_logFile;
 my $mlogFile;
 my $mlogDir;
 my $mproSr;
 my $jfile;
 my $lgsize;
 my $outSt = 'n/a'; 
 my $rcfdisk;
 my $nfsize;
 my $lgname;
 my $musize = 0;
 my %evsize = {};

 my $nevt = 0;
 my $nevent_vtx = 0;
 my $no_prvertx = 0;
 my $avr_prvertx = 0;
 my $Rvtx = 0;
 my $first_evts = 0;
 my $last_evts = 0; 
 my $mEvtSk = 0;


 my $mNevPrimVtx = 0;

 my $jLog_err;
 my $lgfile;
 my $dbStat = "n/a";
 my $mdone = 0;
 my $exterr = ".err.gz";

 my $subtime;
 my $epsubtime;
 my $epcrtime;
 my $difmin;
 my $difhour;
 my $rnhours; 
 my $sec1;
 my $min1;
 my $hr1;
 my $dy1;
 my $mn1;
 my $yr1;

 my $dbNev = 0;
 my $ltime;
 my $mfilegz;
 my $flag1 = 0;
 my $flag2 = 0;
 my $bfcflag = 0;
 my $mlogDir2;
 my $cretime;
 my $mfTime;
 my $rday = "0000-00-00";
 my @sprt = ();
 my $prstat = "on_disk"; 
 my $muname;

   foreach my $jobnm (@jobSum_set){
        $mproSr   = ($$jobnm)->prSer;
        $mlogFile = ($$jobnm)->smFile;
        $mlogDir  = ($$jobnm)->smDir;
        $mjobFname = ($$jobnm)->jbFile;
        $dbStat    = ($$jobnm)->jobSt;
        $dbNev     = ($$jobnm)->NoEvt;
        $subtime   = ($$jobnm)->sbtime;
    
        $jfile = $mlogFile;
        $jfile =~ s/.log//g;
#        $mlogDir =~ s/daq/daq.1/g; 
        $daqFile = $jfile . ".daq";
      $mfilegz = $mlogDir ."/". $mlogFile .".gz";  
#     print $mfilegz, "\n";
      $mfile = $mlogDir ."/". $mlogFile;  
      $flag1 = 0;
      $flag2 = 0;  
      $ltime = 10; 
        if (-f $mfilegz)  {
      $ltime = `mod_time $mfilegz`;

     ($mfTime) = (stat($mfilegz))[9];
     ($sec,$min,$hr,$dy,$mo,$yr) = (localtime($mfTime))[0,1,2,3,4,5];

     if( $yr > 97 ) {
        $fullyear = 1900 + $yr;
      } else {
        $fullyear = 2000 + $yr;
      };

      $mo = sprintf("%2.2d", $mo+1);
      $dy = sprintf("%2.2d", $dy);

     $cretime = sprintf ("%4.4d-%2.2d-%2.2d %2.2d:%2.2d:00",
                       $fullyear,$mo,$dy,$hr,$min );
     $rday = sprintf ("%4.4d-%2.2d-%2.2d", $fullyear,$mo,$dy); 

  $epcrtime = timelocal($sec, $min, $hr, $dy, $mo-1, $fullyear );  

      $jb_logFile = $mfilegz;
      $flag1 = 1;
   }       

        $exterr = ".err.gz"; 
        if( $flag1 == 1 ) {
          $lgsize = (stat($jb_logFile))[7];          
          if($lgsize > 2000) {

  if($subtime ne "0000-00-00 00:00:00") {

#  print $jb_logFile, "\n";

  @prt = ();
  @sprt = ();
  @prt = split (" ",$subtime);
  @sprt = split ("-",$prt[0]);
  $yr1 = $sprt[0];
  $mn1 = $sprt[1];
  $dy1 = $sprt[2];
  @sprt = ();
  @sprt = split (":",$prt[1]);
  $hr1 = $sprt[0];
  $min1 = $sprt[1];
  $sec1 = $sprt[2];

  $epsubtime = timelocal($sec1, $min1, $hr1, $dy1, $mn1-1, $yr1);

  $difmin = ($epcrtime - $epsubtime)/60.;
  $difhour = $difmin/60.;

  $rnhours = sprintf("%.2f", $difhour);

# print "Jobs total time:  ", $cretime,"   ",$subtime,"   ",$epcrtime,"   ",$epsubtime,"   ",$difmin,"   ",$rnhours, "\n";

  }else{

  $rnhours = 0;
  
  }

       $JOB_DIR = $jobFDir . $recoDir ;

      $jb_archive = $JOB_DIR . "/archive_calib/" . $mjobFname;
      $jb_jobfile = $JOB_DIR . "/jobs_calib/" . $mjobFname;
      $jb_loop = $JOB_DIR . "/jobs_looping/" . $mjobFname;
      $jb_lost = $JOB_DIR . "/jobs_lostfiles/" . $mjobFname;
      if (-f $jb_archive)  {$mjbDir = "archive_calib"};
      if (-f $jb_jobfile)  {$mjbDir = "jobs_calib"};
      if (-f $jb_loop)     {$mjbDir = "jobs_looping"};  
      if (-f $jb_lost)     {$mjbDir = "jobs_lostfiles"};  


        $mEvtSk = 0;
        $first_evts = 0;
        $last_evts = 0;
        $mjobSt = "n/a";
        $mNev  = 0;
        $mCPU = 0;
        $mRealT = 0; 
        $mNoTrk = 0;
        $mNoVert = 0;
        $mnodeId = "n/a";
        $mjobSt = "n/a"; 

        $mNevPrimVtx = 0;

       parse_log($jb_logFile,$exterr);

       if (($mjobSt ne  $dbStat) or ( $mNev != $dbNev ) )  {
        print "JobFile=", $mjobFname," % ", "Job Status: ", $mjobSt, "\n";
#      print "Event,CPU,Trk", $mNev," % ",  $mCPU, " % ", $mNoTrk, "\n";

#####  update JobStatus table with info for jobs completed
#   print "updating JobStatus table\n";
 
     &updateJSTable(); 

  if($comcalib eq "BL" ) {
    $outSt = "yes";
    $rcfdisk = "$bldisk";
    $prstat = "on_disk";
    $lgname = $mlogFile;
    $nfsize = 0;

     &updateClTable(); 
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

  foreach my $mufile (@dstFiles) {
  $mfName  = ($$mufile)->filename;
  if($mfName =~ /event.root/) {
  $muname = $mfName;
  $muname =~ s/event/MuDst/g;  
  $evsize{$muname}= ($$mufile)->dsize;  

    } 
  }

 foreach my $mufile (@dstFiles) {

  $mfName  = ($$mufile)->filename;
  $rcfdisk = ($$mufile)->mdisk;
  if($mfName =~ /MuDst.root/) {
  $musize  = ($$mufile)->dsize;
  $nfsize = $musize + $evsize{$mfName};
print "Size of MuDst, event.root and sum:  ",$mfName," % ",$musize," % ", $evsize{$mfName}," % ",$nfsize, "\n";
  $lgname = $mfName;
  $lgname =~ s/MuDst.root/log/g;
  $outSt = "yes";
  $prstat = "on_disk";

########################
 print "Updating JobStatus table with file ", $mfName, "\n";

     &updateClTable(); 
    }
  }


     &StDbProdDisconnect();

my $nmin;
my $nhour;
my $nday;
my $nmon;
my $nyear;
my $wday;
my $yday;
my $isdst;
my $thistime;

   ($nsec,$nmin,$nhour,$nday,$nmon,$nyear,$wday,$yday,$isdst) = localtime(time);
   $nmon++;
   $thistime = $nmon . "." .$nday . "." .$nhour . "." . $nmin . "." . $nsec;

  open (STDOUT, ">$Startfile");
    print "Update finished at $thistime", "\n";

 close (STDOUT); 

  `sleep 3600`;
    exit;

##############################################################################
  sub updateJSTable {

   $sql="update $JobStatusT set ";
   $sql.="jobStatus='$mjobSt',";
   $sql.="inputHpssStatus='OK',";
   $sql.="outputStatus='$outSt',";
   $sql.="createTime='$cretime',";
   $sql.="runDay='$rday',";
   $sql.="NoEvents='$mNev',";
   $sql.="CPU_per_evt_sec='$mCPU',";
   $sql.="avg_no_tracks='$mNoTrk',";
   $sql.="avg_no_prvertx='$avr_prvertx',"; 
   $sql.="RealTime_per_evt='$mRealT',";
   $sql.="nodeID='$mnodeId' "; 
   $sql.=" WHERE logfileName = '$mlogFile' AND jobfileName = '$mjobFname' AND calibTag = '$comcalib' ";
   print "$sql\n" if $debugOn;
#   print "$sql\n";
   $rv = $dbh->do($sql) || die $dbh->errstr;
    }

#############################################################################
  sub updateClTable {

   $sql="update $JobStatusT set ";
   $sql.="outputStatus='$outSt',";
   $sql.="mudstsize='$nfsize',";
   $sql.="status='$prstat',";      
   $sql.="diskname='$rcfdisk'";
   $sql.=" WHERE trigsetName like '$trig%' and logfileName = '$lgname' and prodSeries = '$prodSr' and calibtag = '$comcalib' ";
   print "$sql\n" if $debugOn;
#   print "$sql\n";
   $rv = $dbh->do($sql) || die $dbh->errstr;
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

sub parse_log($$) {

  my $logName = $_[0];
  my $exErr = $_[1];
  my $line; 
  $mjobSt = "Run not completed";
  my $Err_messg = "none";
  my $gz;
  my $status;
  my $comp; 

  my $Anflag = 0;
  my $no_event = 0;
  my $num_event = 0;
  my $num_line = 0;  
  my $no_tracks; 
  my $no_vertices;
  my $no_prtracks; 
  my $tot_tracks = 0;
  my $tot_vertices = 0;
  my $tot_prtracks = 0;
  my $avr_prtracks;
  my @word_tr = ();
  my @nparts = (); 
  my @words = ();
  my @prtk = ();
  my $cpuflag = 0;
  my $NprtTrk0 = 0;
  my $mRealTbfc = 0;
  my $mCPUbfc = 0;
  my $bfcflag = 0;
  my @vrank = ();
  my $npr = 0;
  my @nmb = ();
  my @nmbx = ();


  $mEvtSk = 0;
  $first_evts = 0;
  $last_evts = 0; 
  $mnodeId = "n/a";
  $mRealT = 0;
  $mCPU = 0;
  $nevent_vtx = 0;
  $nevt = 0;
  $no_prvertx = 0;
  $avr_prvertx = 0;
  $Rvtx = 0;

 #---------------------------------------------------------
#  print $logName, "\n";

#   $nevent_vtx = `zgrep 'primary vertex(0):' $logName | wc -l ` ;

 $nevent_vtx = 0;

    if($logName =~ /\.gz/){
        $gz     = gzopen($logName , "r");
        $status = defined($gz);
        $comp   = 1;
    } else {
        $comp   = 0;
        $status = open(FI,"logName");
    }

 $Anflag = 0;   

        if( $comp){
        $status = ($gz->gzreadline($line) > 0);
        } else {
        $status = defined($line = <FI>);
        }

        while ( $status ){
            chomp($line);

     if ( $line =~ /You are using STAR_LEVEL/ ) {
       @word_tr = split(":",$line);
       $mnodeId = $word_tr[4];  
#  print $mnodeId, "\n";
     }

         if ($line =~ /Processing bfc.C/) {
         $bfcflag++;
       }
              if( $bfcflag == 1) { 

# get  number of events

   if ( $line =~ /QAInfo: Done with Event/ ) {
      @nparts = (); 
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

  @nmb = ();
  @nmbx = ();
  @word_tr = ();

     if ($line =~ /track nodes:/ && $Anflag == 0 ) {
           my  $string = $line;
             @word_tr = split /:/,$string;            
              $no_tracks = $word_tr[2];
              $tot_tracks += $no_tracks; 
#              print $word_tr[2], $no_tracks, "\n";

               if($no_tracks >= 1) {
              $nevt++;
	  }

          $npr = 0;
          @no_prtracks = ();
          @no_prtrck_nfit15 = ();

        }elsif(  $line =~ /QA :INFO/ and $line =~ /Rank/ and $line =~ /#V/ ) {
              @word_tr = ();
              @nmbx = ();
              @word_tr = split (":",$line);
              @nmbx = split (" ",$word_tr[4]);
#         print "Check splitting   ",$word_tr[3]," %  ", $word_tr[4]," %  ", $word_tr[5]," % ", $word_tr[6], "\n";
             $vrank[$npr] = $nmbx[0];
             @nmb = ();
             @nmb = split (",",$word_tr[5]);
             $no_prvertx++;
             $no_prtracks[$npr] = $nmb[1];
             $no_prtrck_nfit15[$npr]  = $nmb[2];

             $tot_prtracks += $no_prtracks[$npr];

             $npr++;

           if ($npr == 1 ) {
              $nevent_vtx++;
	  }
####	  
       }
	     

# check if job crashed 

      if($line =~ /Bus error/) {
         $Err_messg = "Bus error";
     }

    elsif ($line =~ /Segmentation violation/) {
             $Err_messg = "segmentation violation";
    }
    elsif ($line =~ /Segmentation fault/) {
	    $Err_messg = "segmentation fault";
     }

    elsif ($line =~ /eventIsCorrupted/)  {
            $Err_messg = "Corrupted event";
    } 
    elsif ($line =~ /Interpreter error recovered/)  {
             $Err_messg = "Interpreter error recovered";
           }
    
   elsif ($line =~ /Killed/)  {
             $Err_messg = "Killed";
           }

   elsif ($line =~ /Abort/)  {
             $Err_messg = "Abort";
           }

  elsif ($line =~ /glibc detected/)  {
             $Err_messg = "glibc detected";
         }

   elsif ($line =~ /Tried to find a host for 500 times, will abort now/)  {
             $Err_messg = "DB connection failed";
           }

  elsif ($line =~ /FATAL/ and $line =~ /floating point exception/ )  {
             $Err_messg = "FPE";
         }


# check how many events have been skiped
my $temEvt = 0;
# @prtk = (); 
 
      if ( $line =~ /QAInfo:Run/ and $line =~ /Total events processed/) {
#  print $line, "\n";
	@word_tr = ();
        @word_tr = split /:/,$line;
        $temEvt = $word_tr[3];
        $mEvtSk =  $word_tr[4];
#       @prtk = split(" ", $temEvt);
#        $mNev = $prtk[0];
#   }      
  }

 $mNev =  $no_event;

#check if job is completed
    if ( $line =~ /Run completed/) {          
          $mjobSt = "Done";      
        }

   if ($no_event != 0) {
   if ($line =~ /StBFChain::bfc/) {

      @words= ();
      @words= split (" ", $line); 

     if($words[8] eq "=" ){

      $mCPUbfc = $words[9];
      }elsif($words[8] eq "Cpu" ){
       $mCPUbfc = $words[10];
      }else{
 
      $mCPUbfc = $words[8];
      $mCPUbfc = substr($mCPUbfc,1) + 0;
    }

      if($words[6] eq "=" ){
      $mRealTbfc = $words[7];
      }else{
      $mRealTbfc = $words[6];
      $mRealTbfc = substr($mRealTbfc,1) + 0;
    }
#      print "CHeck CPU, RealTime   : ",  $mCPUbfc,"    ", $mRealTbfc,"\n";

     $cpuflag = 1;
    }  
   }
  }

  $num_line++;

      if( $comp){
                $status = ($gz->gzreadline($line) > 0);
           } else {
                $status = defined($line = <FI>);
           }
     }

       if($comp){
        $gz->gzclose();
        } else {
        close(FI);
        }

# print "Number no_event, status ", $mNev,"  %  ", $mjobSt,"\n";

  $num_event = $mNev - $mEvtSk;
  $mNevPrimVtx =  $mNev - $NprtTrk0 ;

 
  if( $num_event > 0 )  {

    if($cpuflag == 1) {
    $mCPU = $mCPUbfc/$num_event;
    $mRealT = $mRealTbfc/$num_event;
#  print "CPU2 ", $mCPU,"   %   ", $mRealT, "\n";
 }


   $mNoVert  = $tot_vertices/$num_event;
#   $avr_prtracks  = $tot_prtracks/$num_event;
   $Rvtx = $nevent_vtx/$num_event;

 } 
 
   if($nevt >=1 )  {
  $mNoTrk  = $tot_tracks/$nevt;
  $avr_prtracks  = $tot_prtracks/$nevt;
 }

      if($nevent_vtx >= 1 ) {
   $avr_prvertx      = $no_prvertx/$nevent_vtx;
    }else{
   $avr_prvertx      = 0;
   } 
   

##----------------------------------------------------------------------------
# parse error log file

   my @err_out = ();
   my $mline;
   my $jLog_err;
   my $err_file;
   

  $err_file = $jfile . $exErr ;
 
  $jLog_err = $DISK1 . "/" . $prodSr ."_calib/log/daq" ."/" .$err_file;  
# print "Err file ",  $jLog_err, "\n"; 

   if($exErr eq ".err.gz") {
      @err_out = `zcat $jLog_err | tail -100 `;
   }else{
      @err_out = `tail -100 $jLog_err`; 
  } 

  foreach $mline (@err_out){
          chop $mline;
       if ( $mline =~ /No space left on device/)  {
        $Err_messg = "No space left on device";
     } 
        elsif ($mline =~ /Error calling module/) {
       chop $mline;  
      $Err_messg = $mline;
      }
     elsif ($mline =~ /Stale NFS file handle/) {
  
      $Err_messg = "Stale NFS file handle";
     }       
     elsif ( $mline =~ /Assertion/ & $mline =~ /failed/)  {
        $Err_messg = "Assertion failed";
     } 
      elsif ($mline =~ /Error calling module/) {
       chop $mline;  
      $Err_messg = $mline;
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
      elsif ($mline =~ /Error: Symbol G__exception is not defined/) {
      $Err_messg = "G_exception not defined";
     }   
  } 
      if ( $Err_messg ne "none") {
     $mjobSt = $Err_messg;
   } 
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
