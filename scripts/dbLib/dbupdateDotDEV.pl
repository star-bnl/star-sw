#!/usr/bin/env perl
#
# $Id:
#
# $Log: L.Didenko
#
# dbupdateDotDEV.pl
#
# update JobStatus and FileCatalog for .DEV test jobs
# 
##############################################################################

use Mysql;
use File::Find;
use Net::FTP;
use Class::Struct;
use File::Basename;

require "/afs/rhic.bnl.gov/star/packages/scripts/dbLib/dbTJobsSetup.pl";


my $TOP_DIRD = "/star/rcf/test/dotdev/";
my @dir_year = ("year_2004", "year_2005", "year_2006", "year_2007", "year_2008");
my @node_dir = ("trs_sl302.ittf");
my @node_daq = ("daq_sl302.ittf");
my @hc_dir = ("auau_minbias","cucu200_minbias","auau200_central");
my @daq_dir = ("AuAuMinBias","CuCu200_MinBias","ppProdTrans","2007ProductionMinBias","production_dAu2008");

my @OUT_DIR;
my @OUTD_DIR;
my @Nday = ("Sun","Mon","Tue","Wed","Thu","Fri","Sat");


my %dayHash = (
                 "Sun" => 1,
		 "Mon" => 2,
		 "Tue" => 3, 
		 "Wed" => 4, 
		 "Thu" => 5, 
		 "Fri" => 6,
                 "Sat" => 7, 
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


#
# Set name of week day 
#
  ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
    $thisday = (Sun,Mon,Tue,Wed,Thu,Fri,Sat)[(localtime)[6]];
    $thistime = $hour . "." . $min . "." . $sec;
    print ("time is : ",  $thistime, "\n"); 

   my $ii = 0;
 my $iday;
 my $testDay;
 my $beforeDay;
 $iday = $dayHash{$thisday}; 
 $testDay = $Nday[$iday - 1];
 $beforeDay = $Nday[$iday - 2];


  print "Day Name: ",$thisday, " % ", "Index", $iday, "\n";
 
##### setup output directories for DEV with thisDay
 
 $ii = 0;

   $OUT_DIR[$ii] = $TOP_DIRD . $node_dir[0] . "/" . $testDay . "/". $dir_year[0] . "/" . $hc_dir[0];
    print "Output Dir for DEV :", $OUT_DIR[$ii], "\n";
        $ii++;
  
  $OUT_DIR[$ii] = $TOP_DIRD . $node_dir[0] . "/" . $testDay . "/". $dir_year[1] . "/" . $hc_dir[1]; 
  print "Output Dir for DEV :", $OUT_DIR[$ii], "\n";
    $ii++; 

  $OUT_DIR[$ii] = $TOP_DIRD . $node_dir[0] . "/" . $testDay . "/". $dir_year[3] . "/" . $hc_dir[2];
  print "Output Dir for DEV :", $OUT_DIR[$ii], "\n";
   $ii++;

   for ($ik = 0; $ik < 5; $ik++) { 

   $OUT_DIR[$ii] = $TOP_DIRD . $node_daq[0] . "/" . $testDay . "/". $dir_year[$ik] . "/" . $daq_dir[$ik];
   print "Output Dir for DEV :", $OUT_DIR[$ii], "\n";
        $ii++;
 }


   $OUT_DIR[$ii] = $TOP_DIRD . $node_dir[0] . "/" . $beforeDay . "/". $dir_year[0] . "/" . $hc_dir[0];
    print "Output Dir for DEV :", $OUT_DIR[$ii], "\n";
        $ii++;
  
  $OUT_DIR[$ii] = $TOP_DIRD . $node_dir[0] . "/" . $beforeDay . "/". $dir_year[1] . "/" . $hc_dir[1]; 
  print "Output Dir for DEV :", $OUT_DIR[$ii], "\n";
    $ii++; 

  $OUT_DIR[$ii] = $TOP_DIRD . $node_dir[0] . "/" . $beforeDay . "/". $dir_year[3] . "/" . $hc_dir[2];
  print "Output Dir for DEV :", $OUT_DIR[$ii], "\n";
   $ii++;

   for ($ik = 0; $ik < 5; $ik++) { 

   $OUT_DIR[$ii] = $TOP_DIRD . $node_daq[0] . "/" . $beforeDay . "/". $dir_year[$ik] . "/" . $daq_dir[$ik];
   print "Output Dir for DEV :", $OUT_DIR[$ii], "\n";
        $ii++;
 } 


struct JFileAttr => {
       oldjbId   => '$',
       oldpath   => '$',
       oldfile   => '$',
       oldTime   => '$',
       oldcomp   => '$',
       oldvail   => '$',
};

 struct LFileAttr => {
        jbId      => '$',
        pth       => '$',
        lbT       => '$', 
        lbL       => '$',
        rtL       => '$',
        lgName    => '$',
        crTm      => '$',
        chOpt     => '$',
        errMs     => '$',
        jbSt      => '$',      
        evDn      => '$',
        evSkp     => '$',
        memF      => '$',
        memL      => '$',
        jCPU      => '$',
        jRT       => '$',
        avTr      => '$',
        avPrTr    => '$',
        avTrGd    => '$',
        avPrTrGd  => '$',
        avPrv1    => '$',
        avPrfitv1 => '$',       
        avVrt     => '$',
        avXi      => '$',
        avKn      => '$',
        prctus    => '$',
        avTrus    => '$',
        avTrfitus => '$',
        avPrv1us  => '$',
        avPrfitv1us => '$',
        avV0us    => '$',
        avXius    => '$',
        avKnus    => '$',
        ndID      => '$', 
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

 my %flagHash = (); 
 my %idHash = ();
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
 my $lgFile;
 my $platf;
 my $EvDone = 0;
 my $EvSkip = 0;
 
 my $newAvail; 
 my $no_event = 0; 
 my @maker_size = ();
 my $jrun = "Run not completed";
 my $tot_tracks = 0;
 my $tot_vertices = 0;
 my $tot_prtracks = 0;
 my $tot_trck_nfit15 = 0;
 my $tot_prtrck_hfit15 = 0;
 my $tot_prtracks_1vtx = 0;
 my $tot_prtrck_nfit15_1vtx = 0; 
 my $tot_knvertices = 0;
 my $tot_xivertices = 0;
 my $avr_tracks = 0;
 my $avr_vertices = 0;
 my $avr_prtracks = 0;
 my $avr_trck_nfit15 = 0; 
 my $avr_prtrck_nfit15 = 0;  
 my $avr_prtracks_1vtx = 0;
 my $avr_prtrck_nfit15_1vtx = 0; 
 my $avr_knvertices = 0;
 my $avr_xivertices = 0;
 my $perct_usb = 0;
 my $avr_trk_usb = 0;
 my $avr_prtrk_usb= 0;
 my $avr_trkfit15_usb = 0;
 my $avr_prtrkfit15_usb = 0;
 my $avr_v0_usb = 0;
 my $avr_kink_usb = 0;
 my $avr_xi_usb = 0;
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
 
  $now = time;
##### connect to DB TestJobs

  &StDbTJobsConnect(); 

#####  select all files from JobStatusT from testDay direcroties

 $sql="SELECT jobID, path, logFile, createTime, avail FROM $JobStatusT WHERE path LIKE '%dotdev%$testDay%' AND avail = 'Y'";
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
        ($$fObjAdr)->oldTime($fvalue)   if( $fname eq 'createTime');
        ($$fObjAdr)->oldvail($fvalue)   if( $fname eq 'avail'); 
   }

       $old_jobs[$nold_jobs] = $fObjAdr;
       $nold_jobs++; 

 }

#####  select all files from JobStatusT from beforeDay direcroties

 $sql="SELECT jobID, path, logFile, createTime, avail FROM $JobStatusT WHERE path LIKE '%dotdev%$beforeDay%' AND avail = 'Y'";
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
        ($$fObjAdr)->oldTime($fvalue)   if( $fname eq 'createTime'); 
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


####### read log files and fill in JobStatus

my $pvfile;
my $pvpath = "n/a";
my $pvTime = 0000;
my $pvjbId;
my $pvavail;
my $pvcomp;
my $pfullName;
my $fflag;
my $Fname;
my @files;


 foreach  my $eachOutLDir (@OUT_DIR) {
          if (-d $eachOutLDir) {
     opendir(DIR, $eachOutLDir) or die "can't open $eachOutLDir\n";
      @files = readdir(DIR);
#    while( defined($fname = readdir(DIR)) ) {
     foreach $fname ( @files) {
      next if !$fname;
      next if $fname =~ /^\.\.?$/;    
     
 $jrun = "Run not completed";
 $EvDone = 0;
 $perct_usb = 0;
 $avr_tracks = 0;
 $avr_vertices = 0;
 $avr_prtracks = 0;
 $avr_knvertices = 0;
 $avr_xivertices = 0;
 $avr_trck_nfit15 = 0; 
 $avr_prtrck_nfit15 = 0; 
 $avr_prtracks_1vtx = 0;
 $avr_prtrck_nfit15_1vtx = 0;
 $avr_trk_usb = 0;
 $avr_prtrk_usb= 0;
 $avr_trkfit15_usb = 0;
 $avr_prtrkfit15_usb = 0;
 $avr_v0_usb = 0;
 $avr_kink_usb = 0;
 $avr_xi_usb = 0; 
 $node_name = "n/a";
 $libL = "n/a";
 $libV = "n/a";  
 $rootL = "n/a"; 
 $Err_messg = "none";
 $mchain = "n/a";
 $no_event = 0;
 $mCPU = 0;
 $mRealT = 0;
 $mavail = 'Y'; 
 $memFst = 0;
 $memLst = 0; 
 $EvSkip = 0;
 $jobTime = 0; 

       if ($fname =~ /.log/)  {
#    print "File Name:",$fname, "\n";       
       $fullname = $eachOutLDir."/".$fname;
      $mpath = $eachOutLDir;
      @dirF = split(/\//, $eachOutLDir);
       $libL = $dirF[4];
       $platf = $dirF[5]; 
       $logName = $fname; 
#      $Fname =  $mpath . "/" . $fname;
       $flagHash{$fullname} = 1;
      ($size, $mTime) = (stat($fullname))[7, 9];
    
      $ltime = $now - $mTime; 
     ($sec,$min,$hr,$dy,$mo,$yr) = (localtime($mTime))[0,1,2,3,4,5];
      $mo = sprintf("%2.2d", $mo+1);
      $dy = sprintf("%2.2d", $dy);
  
      if( $yr > 97 ) {
        $fullyear = 1900 + $yr;
      } else {
        $fullyear = 2000 + $yr;
      };

       $timeS = sprintf ("%4.4d-%2.2d-%2.2d %2.2d:%2.2d:00",
                       $fullyear,$mo,$dy,$hr,$min);    

#           if( $ltime > 600 && $ltime < 518400 ){         
          if( $ltime > 600 ) { 
#   print $fullname, "\n";
        &logInfo("$fullname", "$platf");
     $jobTime = $timeS;  

      $fObjAdr = \(LFileAttr->new());
      ($$fObjAdr)->pth($mpath);
      ($$fObjAdr)->lbT($libV);
      ($$fObjAdr)->lbL($libL);
      ($$fObjAdr)->rtL($rootL);
      ($$fObjAdr)->crTm($jobTime); 
      ($$fObjAdr)->chOpt($mchain);
      ($$fObjAdr)->jbSt($jrun);
      ($$fObjAdr)->errMs($Err_messg);
      ($$fObjAdr)->lgName($logName);
      ($$fObjAdr)->evDn($EvDone);
      ($$fObjAdr)->evSkp($EvSkip);
      ($$fObjAdr)->memF($memFst);
      ($$fObjAdr)->memL($memLst);
      ($$fObjAdr)->jCPU($mCPU);
      ($$fObjAdr)->jRT($mRealT);
      ($$fObjAdr)->avTr($avr_tracks);
      ($$fObjAdr)->avPrTr($avr_prtracks);
      ($$fObjAdr)->avVrt($avr_vertices);
      ($$fObjAdr)->avXi($avr_xivertices);
      ($$fObjAdr)->avKn($avr_knvertices);
      ($$fObjAdr)->avTrGd($avr_trck_nfit15);
      ($$fObjAdr)->avPrTr($avr_prtrck_nfit15);
      ($$fObjAdr)->avPrv1($avr_prtracks_1vtx);
      ($$fObjAdr)->avPrfitv1($avr_prtrck_nfit15_1vtx);
      ($$fObjAdr)->prctus($perct_usb);
      ($$fObjAdr)->avTrus($avr_trk_usb);
      ($$fObjAdr)->avPrv1us($avr_prtrk_usb);
      ($$fObjAdr)->avTrfitus($avr_trkfit15_usb);
      ($$fObjAdr)->avPrfitv1us($avr_prtrkfit15_usb);
      ($$fObjAdr)->avV0us($avr_v0_usb);
      ($$fObjAdr)->avKnus($avr_kink_usb);
      ($$fObjAdr)->avXius($avr_xi_usb);
      ($$fObjAdr)->ndID($node_name); 
      $testJobStFiles[$nJobStFiles] = $fObjAdr;
      $nJobStFiles++;


      foreach my $eachOldJob (@old_jobs) {
          $pvjbId = ($$eachOldJob)->oldjbId;
          $pvpath = ($$eachOldJob)->oldpath;
          $pvfile = ($$eachOldJob)->oldfile;
          $pvTime = ($$eachOldJob)->oldTime;
          $pvavail = ($$eachOldJob)->oldvail;
          $pfullName = $pvpath . "/" . $pvfile;
        
#       if( ($fullname eq $pfullName) and ($pvavail eq "Y") ) {

        if( $pfullName eq $fullname ) {
        $flagHash{$fullname} = 0;

	 if( $timeS ne $pvTime) {

          $newAvail = "N";
  print  "Changing availability for test files", "\n";
  print  "files to be updated:", $pvjbId, " % ",$pvpath, " % ",$pvTime, " % ",$newAvail, "\n"; 
    &updateJSTable();

      $mavail = 'Y';
      $myID = 100000000 + $new_id;
      $mjID = "Job". $myID ;
      $crCode = "n/a"; 
      $idHash{$fullname} = $mjID;

  print  "Filling JobStatus with DEV log files for testDay and beforeDay\n";
  print  "files to be inserted:", $mjID, " % ",$mpath, " % ",$timeS , " % ", $memFst," % ",$memLst," % ", $mavail, "\n";  
     &fillJSTable();

   }else{
   }  
   }else{
     next;
   }
    last;
	}
    }
    }else {
      next;
    }
   }
 closedir DIR;
   }
 }


             foreach my $newjobFile (@testJobStFiles) {

 $jrun = "Run not completed";
 $EvDone = 0;
 $perct_usb = 0;
 $avr_tracks = 0;
 $avr_vertices = 0;
 $avr_prtracks = 0;
 $avr_knvertices = 0;
 $avr_xivertices = 0;
 $avr_trck_nfit15 = 0; 
 $avr_prtrck_nfit15 = 0; 
 $avr_prtracks_1vtx = 0;
 $avr_prtrck_nfit15_1vtx = 0; 
 $avr_trk_usb = 0;
 $avr_prtrk_usb= 0;
 $avr_trkfit15_usb = 0;
 $avr_prtrkfit15_usb = 0;
 $avr_v0_usb = 0;
 $avr_kink_usb = 0;
 $avr_xi_usb = 0;
 $node_name = "n/a";
 $libL = "n/a";
 $libV = "n/a";  
 $rootL = "n/a"; 
 $Err_messg = "none";
 $mchain = "n/a";
 $no_event = 0;
 $mCPU = 0;
 $mRealT = 0;
 $mavail = 'Y'; 
 $memFst = 0;
 $memLst = 0; 
 $EvSkip = 0;
 $jobTime = 0;

    $mpath =   ($$newjobFile)->pth;
    $libV=     ($$newjobFile)->lbT;
    $libL =    ($$newjobFile)->lbL;
    $rootL =   ($$newjobFile)->rtL;
    $jobTime =  ($$newjobFile)->crTm; 
    $mchain =  ($$newjobFile)->chOpt;
    $jrun =    ($$newjobFile)->jbSt;
    $Err_messg = ($$newjobFile)->errMs;
    $logName = ($$newjobFile)->lgName;
    $EvDone =  ($$newjobFile)->evDn;
    $EvSkip =  ($$newjobFile)->evSkp;
    $memFst=   ($$newjobFile)->memF;
    $memLst=   ($$newjobFile)->memL;
    $mCPU =    ($$newjobFile)->jCPU;
    $mRealT =  ($$newjobFile)->jRT;
    $avr_tracks=  ($$newjobFile)->avTr;
    $avr_prtracks = ($$newjobFile)->avPrTr;
    $avr_vertices = ($$newjobFile)->avVrt;
    $avr_xivertices = ($$newjobFile)->avXi;
    $avr_knvertices = ($$newjobFile)->avKn;
    $avr_trck_nfit15 =  ($$newjobFile)->avTrGd;
    $avr_prtrck_nfit15 =  ($$newjobFile)->avPrfitv1;
    $avr_prtracks_1vtx = ($$newjobFile)->avPrv1; 
    $perct_usb = ($$newjobFile)->prctus;
    $avr_trk_usb = ($$newjobFile)->avTrus;
    $avr_prtrk_usb= ($$newjobFile)->avPrv1us;
    $avr_trkfit15_usb = ($$newjobFile)->avTrfitus;
    $avr_prtrkfit15_usb = ($$newjobFile)->avPrfitv1us;
    $avr_v0_usb = ($$newjobFile)->avV0us;
    $avr_kink_usb = ($$newjobFile)->avKnus;
    $avr_xi_usb = ($$newjobFile)->avXius;
    $node_name = ($$newjobFile)->ndID; 

    $fullName = $mpath ."/" .$logName;

    if($flagHash{$fullName} == 1) {

#      $new_id = $dbh->{'mysql_insertid'};
      $mavail = 'Y';
      $myID = 100000000 + $new_id;
      $mjID = "Job". $myID ;
      $crCode = "n/a"; 
 print "Insert new files: ", $mjID, " % ",$fullName, "\n";
      $idHash{$fullName} = $mjID;

###########################

    &fillJSTable();

        foreach my $nOldJob (@old_jobs) {
          $pvjbId = ($$nOldJob)->oldjbId;
          $pvpath = ($$nOldJob)->oldpath;
          $pvfile = ($$nOldJob)->oldfile;

	  if($mpath eq  $pvpath) {
            $newAvail = "N";
   print  "Changing avalability for files have been replaced  :", $pvjbId, " % ",$pvpath," % ",$pvfile, "\n";
     &updateJSTable();

    }
	}
    }
}

##### delete from $JobStatusT inserted JobID


    $sql="delete from $JobStatusT WHERE ";    
    $sql.="jobID='$startId' AND "; 
    $sql.="logFile='$lgTest'";
     print "$sql\n" if $debugOn;
    $rv = $dbh->do($sql) || die $dbh->errstr;



  &StDbTJobsDisconnect();

  exit;

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
    $sql.="avg_no_tracksnfit15='$avr_trck_nfit15',";
    $sql.="avg_no_primaryTnfit15='$avr_prtrck_nfit15',";
    $sql.="avg_no_primaryT_1vtx='$avr_prtracks_1vtx',";
    $sql.="avg_no_primaryTnfit15_1vtx='$avr_prtrck_nfit15_1vtx',";
    $sql.="avg_no_XiVrt='$avr_xivertices',";
    $sql.="avg_no_KinkVrt='$avr_knvertices',";
    $sql.="percent_of_usable_evt='$perct_usb',";
    $sql.="avgNoTrack_usbevt='$avr_trk_usb',";
    $sql.="avgNoTrackNfit15_usbevt='$avr_trkfit15_usb',";
    $sql.="avgNoPrTrack_1vtx_usbevt='$avr_prtrk_usb',";
    $sql.="avgNoPrTrackNfit15_1vtx_usbevt='$avr_prtrkfit15_usb',";
    $sql.="avgNoV0_usbevt='$avr_v0_usb',";
    $sql.="avgNoXi_usbevt='$avr_xi_usb',";
    $sql.="avgNoKink_usbevt='$avr_kink_usb',";
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
 my $no_tracks = 0; 
 my $no_vertices = 0 ;
 my $no_xivertices = 0;
 my $no_knvertices = 0;
 my @no_prtracks = () ;
 my $no_trck_nfit15 = 0; 
 my @no_prtrck_nfit15 = ();
 my $no_prtracks_1vtx = 0 ;
 my $no_prtrck_nfit15_1vtx = 0;
 my @word_tr;
 my @nmb = ();
 my @nmbx = ();
 my $i;
 my @part;
 my @size_line;
 my @memSize;
 my @cpu_output;
 my $ij = 0;
 my $end_line; 
 my $npr = 0;
 my $nevt = 0;
 my $max_npr = 0;
 my $max_npr_nfit15 = 0;
    $tot_tracks = 0;
    $tot_vertices = 0;
    $tot_prtracks = 0;
    $tot_trck_nfit15 = 0;
    $tot_prtrck_nfit15 = 0;
    $tot_prtracks_1vtx = 0;
    $tot_prtrck_nfit15_1vtx = 0;
    $tot_knvertices = 0;
    $tot_xivertices = 0;
    $no_event = 0;
    $mCPU = 0;
    $mRealT = 0;
    $memFst = 0;
    $memLst = 0; 
    $EvSkip = 0;
    $EvCom = 0;
@maker_size = ();

#---------------------------------------------------------

  print $fl_log, "\n";

  open (LOGFILE, $fl_log ) or die "cannot open $fl_log", "\n";

   my @logfile = <LOGFILE>;

my $Anflag = 0;
my $runflag = 0;
my $mCPUbfc = 0;
my $mRealTbfc = 0;
my $embflag = 0;
my @tmm = ();
my $mrlt = 0;
my $mcpu = 0;
my $rlt = 0;
my $cput = 0;
my $mixline = "$STAR/StRoot/macros/embedding";

$Err_messg = "none";
$jrun = "n/a";

  if($fl_log =~ /embed/) {

   $embflag = 1;
 }


   foreach my $line (@logfile) {
       chop $line ;
        $num_line++; 
#   get ROOT_LEVEL and node

       if($line =~ /Processing bfc.C/) {
          $runflag++;
	}
   if ($line =~ /StMessageManager message summary/) {
      $Anflag = 1;
    }

  
       if ($line =~ /QAInfo:You are using STAR_LEVEL/) {
         @part = split (" ", $line);
         $rootL = $part[8];
         $node_name = $part[12];   
       }
#   get library version
      if ( $line =~ /={3} You are in (\w+)/ ) {
        if( $Anflag == 0 or $embflag == 1 ) {
        $libV = $1;
      }else{
       next;
       }
    }

#   get chain option
	  if($runflag == 1 or $embflag == 1 ) {
	      if ( $line =~ /Processing bfc.C/)   {
         if( $Anflag == 0 ) {
         @part = ();
         @part = split /"/, $line ;
         $mchain = $part[1]; 
         $mchain =~ s/ /,/g;  
#   print  $mchain, "\n";

    }else{
       next;
        }

    }elsif ( $line =~ /$mixline/ ) {
     @part = ();
     @part = split( "/", $line) ;
       $mchain = $part[4];
# print $line, "\n";

         }

#   get  number of events
#     if ( $line =~ /QAInfo: Done with Event/ ) {
      if ( $line =~ /Done with Event/ ) {
        $no_event++;

#############################################
    if($embflag == 1)  {
    @part = ();
    @part = split( "=", $line) ;
    $mrlt = $part[1];
    $mcpu = $part[2];
     @tmm = ();
    @tmm = split(" ", $part[1]) ; 
    $rlt = $tmm[0];
     @tmm = ();
    @tmm = split(" ", $part[2]) ;      
    $cput = $tmm[0];

    }
#############################################
     } 

#  get memory size
      if ($num_line > 200){
	if( $line =~ /EndMaker/ and $line =~ /total/ ) {
        @size_line = split(" ",$line); 
        
          $mymaker = $size_line[3];
        @memSize = split("=",$size_line[6]);
        if( $mymaker eq "outputStream:"){

          $maker_size[$no_event] = $memSize[1];

       }
      }
    }
# get number of tracks and vertices

      if ($line =~ /QA :INFO  - StAnalysisMaker/ && $Anflag == 0 ) {
 
  @nmb = ();
  @nmbx = ();
  @word_tr = ();
  $npr = 0;
  $max_npr = 0;
  $max_npr_nfit15 = 0;
   
           my  $string = $logfile[$num_line];
              chop $string; 
#              print $string, "\n";

              if($string =~  /track nodes:/) {
              @word_tr = split /:/,$string;
              @nmb =  split /</,$word_tr[2];
              $no_tracks = $nmb[0];
              $tot_tracks += $no_tracks;
              if($no_tracks >= 1) {
              $nevt++;
               }
              @nmbx =  split /</,$word_tr[4];
              $no_trck_nfit15 = $nmbx[0];
              $tot_trck_nfit15 += $no_trck_nfit15;              
              @no_prtrck_nfit15 = ();
              @no_prtracks = ();
              $no_prtrck_nfit15[0] = 0;
              $no_prtracks[0] = 0; 
              $no_vertices = 0;
              $no_xivertices  = 0;
              $no_knvertices = 0;
              $npr = 0;
              $no_prtracks_1vtx = 0;
              $no_prtrck_nfit15_1vtx = 0;
               
            for ($ik = 2; $ik< 100; $ik++)  { 
              $string = $logfile[$num_line + $ik];
              chop $string;

           if( $string =~ /primary tracks/) {
              @word_tr = split /:/,$string;
              @nmb =  split /</,$word_tr[2];
              $no_prtracks[$npr] = $nmb[0];
              @nmbx =  split /</,$word_tr[4];
              $no_prtrck_nfit15[$npr]  = $nmbx[0];
 
#              if( $no_prtracks[$npr] >= $max_npr) {
               if( $no_prtrck_nfit15[$npr] >= $max_npr_nfit15) {
               $max_npr_nfit15 = $no_prtrck_nfit15[$npr];
               $max_npr = $no_prtracks[$npr];
              } 
              $npr++;
 
             }elsif( $string =~ /V0 vertices/) { 
              @word_tr = split /:/,$string;
              @nmb =  split /</,$word_tr[2];
              $no_vertices = $nmb[0];              
              $tot_vertices += $no_vertices;
            } elsif( $string =~ /Xi vertices/) { 
              @word_tr = split /:/,$string;
              @nmb =  split /</,$word_tr[2];
              $no_xivertices = $nmb[0];
              $tot_xivertices += $no_xivertices;
            } elsif( $string =~ /Kink vertices/) {
              @word_tr = split /:/,$string;
              @nmb =  split /</,$word_tr[2];
              $no_knvertices = $nmb[0];
              $tot_knvertices += $no_knvertices;
        } 
      }  
 
   }
              $no_prtracks_1vtx = $no_prtracks[0];
              $no_prtrck_nfit15_1vtx  = $no_prtrck_nfit15[0]; 

               $tot_prtracks += $max_npr;
              $tot_prtrck_nfit15 += $max_npr_nfit15;
              $tot_prtracks_1vtx += $no_prtracks_1vtx;
              $tot_prtrck_nfit15_1vtx += $no_prtrck_nfit15_1vtx;                  
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

       if ( $line =~ /INFO  - QAInfo:Run/ and $line =~ /Total events processed/) {
         @part = split /:/,$line;
        $EvSkip = $part[4];
     }      
# check if job is completed
     if ( $line =~ /Run completed/) {
          
           $jrun = "Done";      
         }
###### 
     
       }
 
#     }
#### check here
   } 
      $EvDone = $no_event;
      $EvCom = $EvDone - $EvSkip;

   if($embflag == 1) {
      $mCPU = $cput/$EvDone;
      $mRealT = $rlt/$EvDone;

 }


##### get CPU and Real Time per event

 if ($EvCom != 0) {
    @cpu_output = `tail -5000 $fl_log`;
 
  foreach $end_line (@cpu_output){
          chop $end_line;
   if ($end_line =~ /QAInfo:Chain/ and $end_line =~ /StBFChain::bfc/) {

#    if ( $end_line =~ /StBFChain::bfc/) {  
#  print $end_line, "\n";
     @part = split (" ", $end_line); 
      $mCPUbfc = $part[8];
      $mRealTbfc = $part[6];
      $mCPUbfc = substr($mCPUbfc,1) + 0;
      $mRealTbfc = substr($mRealTbfc,1) + 0;
#     print "CPU ", $mCPUbfc,"   %   ", $mRealTbfc, "\n";
     $mCPU = $mCPUbfc/$EvCom;
     $mRealT = $mRealTbfc/$EvCom;
    
   }else{
    next;
      }
   }
    $perct_usb        = ($nevt/$EvCom)*100;
    $avr_tracks       = $tot_tracks/$EvCom;
    $avr_vertices     = $tot_vertices/$EvCom;
    $avr_prtracks     = $tot_prtracks/$EvCom;
    $avr_trck_nfit15  = $tot_trck_nfit15/$EvCom;   
    $avr_prtrck_nfit15  = $tot_prtrck_nfit15/$EvCom; 
    $avr_prtracks_1vtx = $tot_prtracks_1vtx/$EvCom;
    $avr_prtrck_nfit15_1vtx = $tot_prtrck_nfit15_1vtx/$EvCom;  
    $avr_knvertices = $tot_knvertices/$EvCom;
    $avr_xivertices = $tot_xivertices/$EvCom;
    if($nevt >= 1 ) {
    $avr_trk_usb = $tot_tracks/$nevt;
    $avr_prtrk_usb= $tot_prtracks_1vtx/$nevt; 
    $avr_trkfit15_usb = $tot_trck_nfit15/$nevt;
    $avr_prtrkfit15_usb = $tot_prtrck_nfit15_1vtx/$nevt;
    $avr_v0_usb = $tot_vertices/$nevt;
    $avr_kink_usb = $tot_knvertices/$nevt;
    $avr_xi_usb =$tot_xivertices/$nevt ;
}

# print "Size of executable:  ", $EvDone, "  ", $no_event,"  ",$EvCom,"  ",$maker_size[$EvCom -1], "\n";                               

    if ( defined $maker_size[0]) { 
    $memFst = $maker_size[0];
    }else {
    $memFst = 0;
  }
    if ( defined $maker_size[$EvCom -1]) {
    $memLst = $maker_size[$EvCom -1];
    } else {
    $memLst = 0;
  }
 }      
# print "Memory size:   ",$memFst, "   ", $memLst, "\n";
    
   close (LOGFILE);

}
