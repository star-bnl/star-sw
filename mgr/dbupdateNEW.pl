#! /opt/star/bin/perl -w
#
# 
#
#  L.Didenko
#
# dbupdateNEW.pl
#
# Update DB for nightly test jobs in NEW library 
#
##############################################################################

use Mysql;
use File::Find;
use Net::FTP;
use Class::Struct;
use File::Basename;

require "/afs/rhic/star/packages/DEV00/mgr/dbTJobsSetup.pl";


my $TOP_DIRD = "/star/rcf/test/new/";
my @dir_year = ("year_1h", "year_2a");
my @node_dir = ("tfs_redhat61", "tfs_Solaris_CC5"), 
my @hc_dir = ("hc_lowdensity", "hc_standard", "hc_highdensity", "peripheral");

my @OUT_DIR;
my @OUTD_DIR;


my %dayHash = (
		 "Mon" => 1,
		 "Tue" => 2, 
		 "Wed" => 3, 
		 "Thu" => 4, 
		 "Fri" => 5, 
		 );

my $min;
my $hour;
my $mday;
my $mon;
my $year;
my $wday;
my $yday;
my $isdst;
my $thisday;
my $thistime;


 my $ii = 0;

##### setup output directories for NEW 

for ($i = 0; $i < 2; $i++) {
     for ($j = 0; $j < 2; $j++) {
      for ($ll = 0; $ll < scalar(@hc_dir); $ll++) {
   $OUT_DIR[$ii] = $TOP_DIRD . $node_dir[$i] . "/". $dir_year[$j] . "/" . $hc_dir[$ll];
    print "Output Dir for NEW :", $OUT_DIR[$ii], "\n";
        $ii++;
      }
  }
}

struct FileAttr => {
      fjbID     => '$',
      flibL     => '$',
      flibV     => '$',
      fplatf    => '$', 
      filename  => '$',
      evtType   => '$',
      fpath     => '$', 
      fgeom     => '$',
      evtGen    => '$',
      ftime     => '$',
      evtReq    => '$',
      evtDone   => '$',
      evtSkip   => '$',
      fsize     => '$',
      fformat   => '$',
      fcomp     => '$',          
};

struct JFileAttr => {
       oldjbId   => '$',
       oldpath   => '$',
       oldfile   => '$',
       oldvail   => '$',
};

 struct LFileAttr => {
        jbId     => '$',
        pth      => '$',
        lbT      => '$', 
        lgName   => '$',
        evDn     => '$',
        evSkp    => '$',
};        

 my $fullyear;
 my $mo;
 my $dy;
 my $size;
 my $mTime; 
 my @dirF;
 my $hr;
 my $yr;
 my $timeS;
 my $now;

 my $debugOn = 0;

 my @old_jobs;
 my $nold_jobs = 0;
 my @testOutNFiles;
 my $nOutNFiles = 0;
 my $eachOutNDir;
 my $fullname;
 my $flname;
 my @prt;
 my $libL = "n\/a";
 my $libV = "n\/a";
 my $platf;
 my $EvTp;
 my $lgFile;
 my $EvDone = 0;
 my $EvReq = 0;
 my $EvSkip = 0;
 my $form;
 my $comp;
 my $geom;
 my $EvType;
 my $EvGen;
 my $evR;
 my $comname;

 my $newAvail; 
 my $no_event = 0; 
 my @maker_size = ();
 my $jrun = "Run not completed";
 my $tot_tracks = 0;
 my $tot_vertices = 0;
 my $tot_prtracks = 0;
 my $tot_knvertices = 0;
 my $tot_xivertices = 0;
 my $avr_tracks = 0;
 my $avr_vertices = 0;
 my $avr_prtracks = 0;
 my $avr_knvertices = 0;
 my $avr_xivertices = 0;
 my $node_name = "n/\a";
 my $rootL = "n/\a"; 
 my $Err_messg = "none";
 my $mCPU = 0;
 my $mRealT = 0;
 my $mchain = "n/\a";
 my $fname;
 my $mpath;
 my $memFst = 0;
 my $memLst = 0;
 my $jobTime;
 my $mavail = "n\/a";
 my $lgTest = "test.log";
 my $startId = "Job00000_test";
 my $myID;
 my $mjID;
 my @testJobStFiles;
 my $nJobStFiles = 0;
 my $jpath;
 my $jEvDone;
 my $jlibV;
 my $jfile;
 my $jEvSkip;
 my $logName; 
 my $crCode = "n\/a"; 
 my $pvpath;
 my $pvpath;
 my $pvjbId;
 my $pvavail;

  $now = time;
##### connect to DB TestJobs

  &StDbTJobsConnect(); 

$nold_jobs = 0;
$newAvail = "n\/a";

#####  select files from JobStatusT with avail = 'Y' 

 $sql="SELECT jobID, path, logFile, avail FROM $JobStatusT WHERE path LIKE '%/test/new%' and avail = 'Y'";
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

        ($$fObjAdr)->oldjbId($fvalue)   if( $fname eq 'jobID');
        ($$fObjAdr)->oldpath($fvalue)   if( $fname eq 'path');
        ($$fObjAdr)->oldfile($fvalue)   if( $fname eq 'logFile');  
        ($$fObjAdr)->oldvail($fvalue)   if( $fname eq 'avail'); 
   }

       $old_jobs[$nold_jobs] = $fObjAdr;
       $nold_jobs++; 

 }


##### insert first line to JobStatusT table get last ID 

   $sql="insert into $JobStatusT set ";    
   $sql.="jobID='$startId',"; 
   $sql.="logFile='$lgTest'";
    print "$sql\n" if $debugOn;
   $rv = $dbh->do($sql) || die $dbh->errstr;
   $new_id = $dbh->{'mysql_insertid'};


##### mark previous files as unavailable
      
        foreach my $eachOldJob (@old_jobs) {
          $pvjbId = ($$eachOldJob)->oldjbId;
          $pvpath = ($$eachOldJob)->oldpath;
          $pvfile = ($$eachOldJob)->oldfile;
          $pvavail = ($$eachOldJob)->oldvail;
          
          $newAvail = "N";
  print  "Changing availability for test files", "\n";
  print  "files to be updated:", $pvjbId, " % ", $pvpath, " % ", $newAvail, "\n"; 

         &updateJSTable();
	}

####### read log files and fill in JobStatus

 foreach  my $eachOutLDir (@OUT_DIR) {
          if (-d $eachOutLDir) {
     opendir(DIR, $eachOutLDir) or die "can't open $eachOutLDir\n";

    while( defined($fname = readdir(DIR)) ) {
      next if $fname =~ /^\.\.?$/;    
     
 $jrun = "Run not completed";
 $EvDone = 0;
 $avr_tracks = 0;
 $avr_vertices = 0;
 $avr_prtracks = 0;
 $avr_knvertices = 0;
 $avr_xivertices = 0;
 $tot_tracks = 0;
 $tot_vertices = 0;
 $tot_prtracks = 0;
 $tot_knvertices = 0;
 $tot_xivertices = 0;
 $node_name = "n/\a";
 $libL = "n/\a";
 $libV = "n/\a";  
 $rootL = "n/\a"; 
 $Err_messg = "none";
 $mchain = "n/\a";
 $no_event = 0;
 $mCPU = 0;
 $mRealT = 0;
 $mavail = 'Y'; 
 $memFst = 0;
 $memLst = 0; 
 $EvSkip = 0;
 $jobTime = 0; 

       if ($fname =~ /evts.log/)  {
#    print "File Name:",$fname, "\n";       
       $fullname = $eachOutLDir."/".$fname;
      $mpath = $eachOutLDir;
      @dirF = split(/\//, $eachOutLDir);
       $libL = $dirF[4];
       $platf = $dirF[5]; 
       $logName = $fname; 
 
      ($size, $mTime) = (stat($fullname))[7, 9];
    
      $ltime = $now - $mTime; 
     ($sec,$min,$hr,$dy,$mo,$yr) = (localtime($mTime))[0,1,2,3,4,5];
      $mo = sprintf("%2.2d", $mo+1);
      $dy = sprintf("%2.2d", $dy);
  
      if( $yr > 98 ) {
        $fullyear = 1900 + $yr;
      } else {
        $fullyear = 2000 + $yr;
      };

       $timeS = sprintf ("%4.4d-%2.2d-%2.2d %2.2d:%2.2d:00",
                       $fullyear,$mo,$dy,$hr,$min);    

           if( $ltime > 3600 ){         
#   print "Log time: ", $ltime, "\n";
      
        &logInfo("$fullname", "$platf");

      $jobTime = $timeS;  
      $mavail = 'Y';
      $myID = 100000000 + $new_id;
      $mjID = "Job". $myID ;
      $crCode = "n\/a"; 

  print  "Filling JobStatus with NEW log files \n";
  print  "files to be inserted:", $mjID, " % ",$mpath, " % ",$logName, " % ", $mavail, "\n";  
         &fillJSTable();

       $fObjAdr = \(LFileAttr->new());
      ($$fObjAdr)->jbId($mjID);
      ($$fObjAdr)->pth($mpath);
      ($$fObjAdr)->lbT($libV);
      ($$fObjAdr)->lgName($logName);
      ($$fObjAdr)->evDn($EvDone);
      ($$fObjAdr)->evSkp($EvSkip);
      $testJobStFiles[$nJobStFiles] = $fObjAdr;
      $nJobStFiles++;
    }
    }else {
      next;
    }
   }
 closedir DIR;
   }
 }

 
##### delete from $JobStatusT inserted JobID

    $sql="delete from $JobStatusT WHERE ";    
    $sql.="jobID='$startId' AND "; 
    $sql.="logFile='$lgTest'";
     print "$sql\n" if $debugOn;
    $rv = $dbh->do($sql) || die $dbh->errstr;

######## read output files for NEW test

foreach  $eachOutNDir (@OUT_DIR) {
         if (-d $eachOutNDir) {
    opendir(DIR, $eachOutNDir) or die "can't open $eachOutNDir\n";

    while( defined($flname = readdir(DIR)) ) {
      next if $flname =~ /^\.\.?$/;    
      if ($flname =~ /.root/)   {
      $fullname = $eachOutNDir."/".$flname;
      @dirF = split(/\//, $eachOutNDir);
       $libL = $dirF[4];
       $platf = $dirF[5]; 
       $geom = $dirF[6];
       $EvTp = $dirF[7]; 
 
       if ($EvTp =~ /hc_/) {
       $EvGen = "hadronic_cocktail";
       $EvType = substr($EvTp,3); 
    }
       elsif ($EvTp =~ /peripheral/) {
       $EvGen = "hadronic_cocktail";
        $EvType = $EvTp;
    }
       $form = "root";
    my $comname = basename("$flname",".root");
      if ($comname =~ m/\.([a-z0-9_]{3,})$/) {
      $comp = $1;
    }     
      my $bsname = basename("$comname","$comp");
       if($flname =~ /2gamma/) {
       @prt = split("_",$bsname);
        $evR = $prt[2];
        $EvReq = $EvReq = substr($evR,0,-5);
     }else {
      @prt = split(/\./,$bsname);
      $evR = $prt[1];
      $EvReq = substr($evR,0,-5);
   };
       $lgFile = $eachOutNDir."/" . $bsname . "log";

       if ( -f $lgFile) {
          ($size, $mTime) = (stat($lgFile))[7, 9];
            $ltime = $now - $mTime;
#  print "Log time: ", $ltime, "\n"; 
           if( $ltime > 3600 ){         
	     foreach my $eachLogFile (@testJobStFiles) {

               $mjID    = ($$eachLogFile)->jbId;
               $jpath   = ($$eachLogFile)->pth; 
               $EvDone  = ($$eachLogFile)->evDn;
               $EvSkip  = ($$eachLogFile)->evSkp;
               $jfile   = ($$eachLogFile)->lgName;
               $libV    = ($$eachLogFile)->lbT;
               $jfpath  = $jpath . "/" .$jfile;
 
	       if ( $jfpath eq $lgFile ) {
 
#       print "File info: ", $libL," % ", $platf, " % ", $fullname, " % ", $geom, " % ", $EvType," % ", $EvGen, " % ", $EvReq," % ", $comp," % ", $EvDone," % ", $libV,  "\n";
   
     ($size, $mTime) = (stat($fullname))[7, 9];
     ($sec,$min,$hr,$dy,$mo,$yr) = (localtime($mTime))[0,1,2,3,4,5];
     $mo = sprintf("%2.2d", $mo+1);
     $dy = sprintf("%2.2d", $dy);
  
     if( $yr > 98 ) {
       $fullyear = 1900 + $yr;
     } else {
       $fullyear = 2000 + $yr;
     };

      $timeS = sprintf ("%4.4d-%2.2d-%2.2d %2.2d:%2.2d:00",
                      $fullyear,$mo,$dy,$hr,$min);


      $fObjAdr = \(FileAttr->new());
     ($$fObjAdr)->fjbID($mjID);
     ($$fObjAdr)->filename($flname);
     ($$fObjAdr)->fpath($jpath);
     ($$fObjAdr)->flibL($libL);
     ($$fObjAdr)->flibV($libV);
     ($$fObjAdr)->fplatf($platf);
     ($$fObjAdr)->evtType($EvType); 
     ($$fObjAdr)->fgeom($geom); 
     ($$fObjAdr)->evtGen($EvGen);
     ($$fObjAdr)->evtReq($EvReq);    
     ($$fObjAdr)->evtDone($EvDone);
     ($$fObjAdr)->evtSkip($EvSkip);  
     ($$fObjAdr)->fformat($form); 
     ($$fObjAdr)->fsize($size);
     ($$fObjAdr)->ftime($timeS);
     ($$fObjAdr)->fcomp($comp); 
     $testOutNFiles[$nOutNFiles] = $fObjAdr;
     $nOutNFiles++;
         }else {
          next;
         } 
        }
       }
      }
    }else {
     next;
   }
   }
 closedir DIR;
       }
 }

 print "Total output files for NEW test: $nOutNFiles\n";


##### initialize variables


 my @old_set;
 my $nold_set = 0;
 my $eachOldFile;
 my $pfullName;

$nold_set= 0;
##### make files from previous test in $thisday directories in DB unavailable 

#####  select all files from FilesCatalog from NEW direcroties

 $sql="SELECT jobID, path, fName, avail FROM $FilesCatalogT WHERE path LIKE '%test/new%' AND avail = 'Y'";
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
        
        ($$fObjAdr)->oldjbId($fvalue)   if( $fname eq 'jobID');
        ($$fObjAdr)->oldpath($fvalue)   if( $fname eq 'path');
        ($$fObjAdr)->oldfile($fvalue)   if( $fname eq 'fName'); 
        ($$fObjAdr)->oldvail($fvalue)   if( $fname eq 'avail'); 
   }

       $old_set[$nold_set] = $fObjAdr;
       $nold_set++; 

 }


######### declare variables needed to fill the database table
##  for database filling

 my $mLibL= "n\/a"; 
 my $mLibT = "n\/a";
 my $mplform = "n\/a";
 my $mevtType = "n\/a";
 my $mflName = "n\/a";
 my $mgeom = "n\/a";
 my $msize = 0;
 my $mevtGen = "n\/a";
 my $mcTime = "0000-00-00";
 my $mNevtR = 0;
 my $mNevtD = 0;
 my $mNevtS = 0;
 my $mcomp = "n\/a";
 my $mformat = "n\/a";
 my $mstatus = 0;

#####  change avail for old files in NEW
#####  in FilesCatalogT if new files available and fill in new files


   foreach my $eachOldFile (@old_set) {
          $pvjbId = ($$eachOldFile)->oldjbId;
          $pvpath = ($$eachOldFile)->oldpath;
          $pvfile = ($$eachOldFile)->oldfile;
          $pvavail = ($$eachOldFile)->oldvail;
          $pfullName = $pvpath . "/" . $pvfile;
              $newAvail = "N";
   print "Changing availability for test files", "\n";
   print "file to be updated:", $pvjbId, " % ", $pfullName, " % ", $newAvail, "\n"; 
   &updateDbTable();

        }

        foreach my $eachTestFile (@testOutNFiles) {

### reinitialize variables
  $mjID = "n\/a";
  $mLibL= "n\/a"; 
  $mLibT = "n\/a";
  $mplform = "n\/a";
  $mevtType = "n\/a";
  $mflName = "n\/a";
  $mpath  = "n\/a";
  $mgeom = "n\/a";
  $msize = 0;
  $mevtGen = "n\/a";
  $mcTime = "0000-00-00";
  $mNevtR = 0;
  $mNevtD = 0;
  $mNevtS = 0; 
  $mcomp = "n\/a";
  $mformat = "n\/a";
  $mavail = "n\/a";
  $mstatus = 0;
             
            $mjID  = ($$eachTestFile)->fjbID;
            $mpath = ($$eachTestFile)->fpath;
            $mflName = ($$eachTestFile)->filename;
            $thfullName = $mpath ."/" . $mflName;
            $mevtType = ($$eachTestFile)->evtType;
            $mLibL    = ($$eachTestFile)->flibL;
            $mLibT    = ($$eachTestFile)->flibV; 
            $mplform  = ($$eachTestFile)->fplatf; 
            $mpath    = ($$eachTestFile)->fpath;
            $mgeom    = ($$eachTestFile)->fgeom;
            $msize    = ($$eachTestFile)->fsize;
            $mevtGen  = ($$eachTestFile)->evtGen;
            $mcTime   = ($$eachTestFile)->ftime;
            $mNevtR   = ($$eachTestFile)->evtReq;
            $mNevtD   = ($$eachTestFile)->evtDone; 
            $mNevtS   = ($$eachTestFile)->evtSkip;
            $mformat  = ($$eachTestFile)->fformat;
            $mcomp    = ($$eachTestFile)->fcomp;
            $mavail   = "Y";

    print "Filling Files Catalog with NEW output files\n";
    print "file to be inserted:", $mjID, " % ",$thfullName, " % ", $mavail, "\n";
   &fillDbTable();

      }

##### DB disconnect

  &StDbTJobsDisconnect();

  exit;

###########
sub fillDbTable {

    $sql="insert into $FilesCatalogT set ";
    $sql.="jobID='$mjID',";
    $sql.="LibLevel='$mLibL',";
    $sql.="LibTag='$mLibT',";
    $sql.="platform='$mplform',";
    $sql.="eventType='$mevtType',";
    $sql.="fName='$mflName',";
    $sql.="path='$mpath',";
    $sql.="geometry='$mgeom',";
    $sql.="eventGen='$mevtGen',";
    $sql.="createTime='$mcTime',";
    $sql.="NoEventReq='$mNevtR',";
    $sql.="NoEventDone='$mNevtD',";
    $sql.="NoEventSkip='$mNevtS',";
    $sql.="size='$msize',";
    $sql.="component='$mcomp',";
    $sql.="format='$mformat',";
    $sql.="avail='$mavail',"; 
    $sql.="status= 0,";
    $sql.="comment=''";
    print "$sql\n" if $debugOn;
    $rv = $dbh->do($sql) || die $dbh->errstr;

  }

###########
sub  updateDbTable {

     $sql="update $FilesCatalogT set ";
     $sql.="avail='$newAvail',";
     $sql.="status= 1";
     $sql.=" WHERE jobID = '$pvjbId' AND path = '$pvpath' AND fName = '$pvfile'";   
     print "$sql\n" if $debugOn;
     $rv = $dbh->do($sql) || die $dbh->errstr;

   }

########### fill in JobStatus

sub fillJSTable {

    $sql="insert into $JobStatusT set ";
    $sql.="jobID='$mjID',";
    $sql.="LibLevel='$libL',";
    $sql.="LibTag='$libV',";
    $sql.="rootLevel='$rootL',";
    $sql.="path='$mpath',";
    $sql.="logFile='$logName',";
    $sql.="createTime='$jobTime',";
    $sql.="chainOpt='$mchain',";
    $sql.="jobStatus='$jrun',";
    $sql.="crashedCode='$crCode',";
    $sql.="errMessage='$Err_messg',";
    $sql.="NoEventDone='$EvDone',";
    $sql.="NoEventSkip='$EvSkip',";
    $sql.="memUsageF='$memFst',";
    $sql.="memUsageL='$memLst',";
    $sql.="CPU_per_evt_sec='$mCPU',";
    $sql.="RealTime_per_evt='$mRealT',";
    $sql.="avg_no_tracks='$avr_tracks',";
    $sql.="avg_no_V0Vrt='$avr_vertices',";
    $sql.="avg_no_primaryT='$avr_prtracks',";
    $sql.="avg_no_XiVrt='$avr_xivertices',";
    $sql.="avg_no_KinkVrt='$avr_knvertices',";
    $sql.="nodeID='$node_name',"; 
    $sql.="avail='$mavail'"; 

    print "$sql\n" if $debugOn;
    $rv = $dbh->do($sql) || die $dbh->errstr;
    $new_id = $dbh->{'mysql_insertid'};  

  }

###########
sub  updateJSTable {

     $sql="update $JobStatusT set ";
     $sql.="avail='$newAvail'";
     $sql.=" WHERE path = '$pvpath' AND logFile = '$pvfile'";   
     print "$sql\n" if $debugOn;
     $rv = $dbh->do($sql) || die $dbh->errstr;

   }

#####=======================================================================
   sub logInfo {

 my ($fl_log,$plt_form) = @_;
 
 my $num_line = 0;
 my @mem_words;
 my $mymaker; 
 my $no_tracks; 
 my $no_vertices;
 my $no_xivertices;
 my $no_knvertices;
 my $no_prtracks;
 my @word_tr;
 my $i;
 my @part;
 my @size_line;
 my @cpu_output;
 my $ij = 0;
 my $end_line; 
    $tot_tracks = 0;
    $tot_vertices = 0;
    $tot_prtracks = 0;
    $tot_knvertices = 0;
    $tot_xivertices = 0;
    $no_event = 0;
    $mCPU = 0;
    $mRealT = 0;
    $memFst = 0;
    $memLst = 0; 
    $EvSkip = 0;
    $EvCom = 0;
#---------------------------------------------------------

  open (LOGFILE, $fl_log ) or die "cannot open $fl_log: $!\n";

   my @logfile = <LOGFILE>;

   foreach my $line (@logfile) {
       chop $line ;
        $num_line++; 
#   get ROOT_LEVEL and node

       if ($line =~ /QAInfo:You are using STAR_LEVEL/) {
         @part = split (" ", $line);
         $rootL = $part[8];
         $node_name = $part[12];   
       }
#   get library version
      if ( $line =~ /={3} You are in (\w+)/ ) {
        $libV = $1;
      }
#   get chain option

       if ( $line =~ /QAInfo: Requested chain bfc is/)  {
         @part = split /:/, $line ;
         $mchain = $part[2]; 
         $mchain =~ s/ /_/g;  
       }
#   get  number of events
      if ( $line =~ /QAInfo: Done with Event/ ) {
        $no_event++;
     } 

#  get memory size
      if ($num_line > 200){
      if( $line =~ /EndMaker/ and $line =~ /root4sta/){
        @size_line = split(" ",$line); 

          $mymaker = $size_line[3];
        if( $mymaker eq "tree:"){
	  if( $plt_form eq "tfs_redhat61") {
         $maker_size[$no_event + 1] = $size_line[9]/1000;
      }elsif( $plt_form eq "tfs_Solaris_CC5")  {
         $maker_size[$no_event + 1] = $size_line[12]/1000; 
        }
       }
      }
    }
# get number of tracks and vertices

      if ($line =~ /QAInfo: StAnalysisMaker/ ) {

            my  $string = $logfile[$num_line];
              @word_tr = split /:/,$string;
              $no_tracks = $word_tr[2];
              $tot_tracks += $no_tracks; 
#               print $word_tr[2], $no_tracks, "\n";
              $string = $logfile[$num_line + 1];
              @word_tr = split /:/,$string;
              $no_prtracks = $word_tr[2]; 
              $tot_prtracks += $no_prtracks;
              $string = $logfile[$num_line + 2];
              @word_tr = split /:/,$string;
              $no_vertices = $word_tr[2]; 
              $tot_vertices += $no_vertices;
              $string = $logfile[$num_line + 3];
              @word_tr = split /:/,$string;
              $no_xivertices = $word_tr[2]; 
              $tot_xivertices += $no_xivertices;
              $string = $logfile[$num_line + 4];
              @word_tr = split /:/,$string;
              $no_knvertices = $word_tr[2]; 
              $tot_knvertices += $no_knvertices;
           
  }   
#  check if job crashed due to break_buss_error
      if($line =~ /bus error/) {
          $Err_messg = "Break bus error";
        }

#  check if job crashed due to segmentation violation
     elsif ($line =~ /segmentation violation/) {
           $Err_messg = "segmentation violation";
  }
      elsif ($line =~ /Stale NFS file handle/) {
  
       $Err_messg = "Stale NFS file handle";
  } 
       elsif ( $line =~ /Assertion/ & $line =~ /failed/)  {
         $Err_messg = "Assertion failed";
  } 
       elsif ($line =~ /Fatal in <operator delete>/) {
  
       $Err_messg = "Fatal in <operator delete>";   
  }
       elsif ($line =~ /Fatal in <operator new>/) {
  
       $Err_messg = "Fatal in <operator new>";   
  }

       if ( $line =~ /Total events processed/) {

        @part = split /:/,$line;
        $EvSkip = $part[3];
    }      
# check if job is completed
     if ( $line =~ /Run completed/) {
          
           $jrun = "Done";      
         }

###### 
     
    }
      $EvDone = $no_event;
      $EvCom = $EvDone - $EvSkip;
 
##### get CPU and Real Time per event
      
 if ($EvCom != 0) {
    @cpu_output = `tail -250 $fl_log`;
     
  foreach $end_line (@cpu_output){
          chop $end_line;
   if ($end_line =~ /QAInfo: Total: bfc/ and $end_line =~ /Cpu Time/) {
     @part = split (" ", $end_line); 
     $mCPU = $part[12]/$EvCom;
     $mRealT = $part[7]/$EvCom;
   }else{
    next;
      }
   }
    $avr_tracks     = $tot_tracks/$EvCom;
    $avr_vertices   = $tot_vertices/$EvCom;
    $avr_prtracks   = $tot_prtracks/$EvCom;
    $avr_knvertices = $tot_knvertices/$EvCom;
    $avr_xivertices = $tot_xivertices/$EvCom;
 
    if ( defined $maker_size[1]) { 
    $memFst = $maker_size[1];
    }else {
    $memFst = 0;
  }
    if ( defined $maker_size[$no_event]) {
    $memLst = $maker_size[$no_event];
    } else {
    $memLst = 0;
  }
 }      
   close (LOGFILE);
  }
