#!/usr/bin/env perl
#
# $Id:
#
# $Log: L.Didenko
#
# dbupdateDEV.pl
#
# update JobStatus and FileCatalog for DEV test jobs
# Run this script next day after jobs have been submitted
##############################################################################


use DBI;
use Class::Struct;
use File::Basename;

$dbhost="duvall.star.bnl.gov";
$dbuser="starreco";
$dbpass="";
$dbname="LibraryJobs";

$JobStatusT = "JobStatus";
$FilesCatalogT = "FilesCatalog";
$JobQAT = "newJobsQA";


my $TOP_DIRD = "/star/rcf/test/dev/";

my @dir_year = ("year_2000", "year_2001", "year_2003", "year_2004", "year_2005", "year_2006", "year_2007", "year_2008","year_2009", "year_2010", "year_2011", "year_2012", "year_2013", "year_2014", "year2015", "2016");
my @node_dir = ("daq_sl302.ittf", "daq_sl302.ittf_opt" ,"trs_sl302.ittf", "trs_sl302.ittf_opt","simu");


my @OUT_DIR0 = ();
my @OUT_DIR1 = ();
my @OUT_DIR2 = ();
my @OUT_DIR3 = ();
my @OUT_DIR4 = ();
my @OUT_DIRB0 = ();
my @OUT_DIRB1 = ();
my @OUT_DIRB2 = ();
my @OUT_DIRB3 = ();
my @OUT_DIRB4 = ();
my @OUT_DIR;
my @TDIR = ();
my @BDIR = ();

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
 my $ik = 0;
 my $iday;
 my $testDay;
 my $beforeDay;
 $iday = $dayHash{$thisday}; 
 $testDay = $Nday[$iday - 1];
 $beforeDay = $Nday[$iday - 2];

  print "Day Name: ",$thisday, " % ", "Index", $iday, "\n";

 ##### setup output directories for thisday
 

 for ($i = 0; $i < scalar(@node_dir); $i++) {

    $TDIR[$i] = $TOP_DIRD . $node_dir[$i]."/".$testDay."/*/*";
}
    @OUT_DIR0 = `ls -d $TDIR[0]`;
    @OUT_DIR1 = `ls -d $TDIR[1]`;
    @OUT_DIR2 = `ls -d $TDIR[2]`;
    @OUT_DIR3 = `ls -d $TDIR[3]`;
    @OUT_DIR4 = `ls -d $TDIR[4]`;

 $ii = 0;

  for ($i = 0; $i < scalar(@OUT_DIR0); $i++) {
      $OUT_DIR[$ii] = $OUT_DIR0[$i];
      chop $OUT_DIR[$ii];
  print "Output Dir for $testDay :", $OUT_DIR[$ii],"\n";
    $ii++;
}
  for ($i = 0; $i < scalar(@OUT_DIR1); $i++) {
     $OUT_DIR[$ii] = $OUT_DIR1[$i];
     chop $OUT_DIR[$ii];
  print "Output Dir for $testDay :", $OUT_DIR[$ii],"\n";
     $ii++;
}

  for ($i = 0; $i < scalar(@OUT_DIR2); $i++) {
     $OUT_DIR[$ii] = $OUT_DIR2[$i];
     chop $OUT_DIR[$ii];
  print "Output Dir for $testDay :", $OUT_DIR[$ii],"\n";
   $ii++;
}
  for ($i = 0; $i < scalar(@OUT_DIR3); $i++) {
     $OUT_DIR[$ii] = $OUT_DIR3[$i];
     chop $OUT_DIR[$ii];
  print "Output Dir for $testDay :", $OUT_DIR[$ii],"\n";
      $ii++;
  }

  for ($i = 0; $i < scalar(@OUT_DIR4); $i++) {
     $OUT_DIR[$ii] = $OUT_DIR4[$i];
     chop $OUT_DIR[$ii];
  print "Output Dir for $testDay :", $OUT_DIR[$ii],"\n";
      $ii++;
  }


 @BDIR = ();  

 ##### setup output directories for beforday

 for ($i = 0; $i < scalar(@node_dir); $i++) {

    $BDIR[$i] = $TOP_DIRD . $node_dir[$i]."/".$beforeDay."/*/*";
}
    @OUT_DIRB0 = `ls -d $BDIR[0]`;
    @OUT_DIRB1 = `ls -d $BDIR[1]`;
    @OUT_DIRB2 = `ls -d $BDIR[2]`;
    @OUT_DIRB3 = `ls -d $BDIR[3]`;
    @OUT_DIRB4 = `ls -d $BDIR[4]`;

$ik = $ii;

  for ($i = 0; $i < scalar(@OUT_DIRB0); $i++) {
      $OUT_DIR[$ik] = $OUT_DIRB0[$i];
      chop $OUT_DIR[$ik];
     
  print "Output Dir for $beforeDay :", $OUT_DIR[$ik],"\n";
    $ik++;
}
  for ($i = 0; $i < scalar(@OUT_DIRB1); $i++) {
     $OUT_DIR[$ik] = $OUT_DIRB1[$i];
     chop $OUT_DIR[$ik];
  print "Output Dir for $beforeDay :", $OUT_DIR[$ik],"\n";
     $ik++;
}

  for ($i = 0; $i < scalar(@OUT_DIRB2); $i++) {
     $OUT_DIR[$ik] = $OUT_DIRB2[$i];
     chop $OUT_DIR[$ik];
  print "Output Dir for $beforeDay :", $OUT_DIR[$ik],"\n";
   $ik++;
}
  for ($i = 0; $i < scalar(@OUT_DIRB3); $i++) {
     $OUT_DIR[$ik] = $OUT_DIRB3[$i];
     chop $OUT_DIR[$ik];
  print "Output Dir for $beforeDay :", $OUT_DIR[$ik],"\n";
      $ik++;
  }

  for ($i = 0; $i < scalar(@OUT_DIRB4); $i++) {
     $OUT_DIR[$ik] = $OUT_DIRB4[$i];
     chop $OUT_DIR[$ik];
  print "Output Dir for $beforeDay :", $OUT_DIR[$ik],"\n";
      $ik++;
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
       oldTime   => '$',
       oldcomp   => '$',
       oldvail   => '$',
};

 struct LFileAttr => {
        jbId      => '$',
        pth       => '$',
        pyr       => '$',
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
        avPrVtx   => '$',
        nEvtVtx   => '$',
        avPrTr    => '$',
        avTrGd    => '$',
        avPrfit   => '$',  
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
 my $nevent_vtx = 0;
 my $numevt_vtx = 0;
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
 my $avr_prvertx = 0;
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
 my $nMtdHits = 0;
 my $nPxlHits = 0;
 my $totMtdHits = 0;
 my $totPxlHits = 0;
 my $avgMtdHits = 0;
 my $avgPxlHits = 0;
 my $nIstHits = 0;
 my $totIstHits = 0;
 my $avgIstHits = 0;
 my $nSstHits = 0;
 my $totSstHits = 0;
 my $avgSstHits = 0;
 my $nAssPxlHits = 0;
 my $nAssIstHits = 0;
 my $nAssSstHits = 0;
 my $avgAssPxlHits = 0;
 my $avgAssIstHits = 0;
 my $avgAssSstHits = 0;
 my $totAssPxlHits = 0;
 my $totAssIstHits = 0;
 my $totAssSstHits = 0;

 my @nmatchMtdHits = ();
 my $totmatchMtdHits = 0;
 my $avgmatchMtdHits = 0;
 my $NevtMtdHits = 0;


  $now = time;
##### connect to DB TestJobs

  &StDbTJobsConnect(); 

#####  select all files from JobStatusT from testDay direcroties

 $sql="SELECT jobID, path, logFile, createTime, avail FROM $JobStatusT WHERE path LIKE '%/dev/%$testDay%' AND avail = 'Y' ";

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

 $sql="SELECT jobID, path, logFile, createTime, avail FROM $JobStatusT WHERE path LIKE '%/dev/%$beforeDay%' AND avail = 'Y'";
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
my $pyear = 0;
 @prt = ();


 foreach  my $eachOutLDir (@OUT_DIR) {
          if (-d $eachOutLDir) {
     opendir(DIR, $eachOutLDir) or die "can't open $eachOutLDir\n";
      @files = readdir(DIR);
#    while( defined($fname = readdir(DIR)) ) {
     foreach $fname ( @files) {
      next if !$fname;
      next if $fname =~ /^\.\.?$/;    
      next if $fname =~ /hijing.log/;
      next if $fname =~ /starsim.log/;
      next if $fname =~ /simu.log/;
     
 $jrun = "Run not completed";
 $EvDone = 0;
 $nevent_vtx = 0;
 $numevt_vtx = 0;
 $perct_usb = 0;
 $avr_tracks = 0;
 $avr_vertices = 0;
 $avr_prvertx = 0;
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
 $nMtdHits = 0;
 $nPxlHits = 0;
 $avgMtdHits = 0;
 $avgPxlHits = 0;
 @nmatchMtdHits = ();
 $avgmatchMtdHits = 0;
 $NevtMtdHits = 0;
 $nIstHits = 0;
 $totIstHits = 0;
 $avgIstHits = 0;
 $nAssPxlHits = 0;
 $nAssIstHits = 0;
 $avgAssPxlHits = 0;
 $avgAssIstHits = 0;
 $totAssPxlHits = 0;
 $totAssIstHits = 0;

 $nSstHits = 0;
 $totSstHits = 0;
 $avgSstHits = 0;
 $nAssSstHits = 0;
 $avgAssSstHits = 0;
 $totAssSstHits = 0;

 @prt = ();
 
      if ($fname =~ /.log/)  {
#    print "File Name:",$fname, "\n";       
       $fullname = $eachOutLDir."/".$fname;
      $mpath = $eachOutLDir;
#  print "Path Name: ",$mpath, "\n";   
      @dirF = split(/\//, $eachOutLDir);
       $libL = $dirF[4];
       $platf = $dirF[5];
       @prt =split("_", $dirF[7]);  
       $pyear = $prt[1];
      next if ($mpath =~ /ppl_minbias/);
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

#           if( $ltime > 2400 && $ltime < 518400 ){         
          if( $ltime > 1200 and $size > 1000 ) { 
#   print "Log time: ", $ltime, "\n";
   print $fullname, "\n";
      
   next if($fullname =~ /hold/);

       &logInfo("$fullname", "$platf");
     $jobTime = $timeS;  

      $fObjAdr = \(LFileAttr->new());
      ($$fObjAdr)->pth($mpath);
      ($$fObjAdr)->pyr($pyear);
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
      ($$fObjAdr)->avPrVtx($avr_prvertx);
      ($$fObjAdr)->nEvtVtx($nevent_vtx); 
      ($$fObjAdr)->avPrTr($avr_prtracks);
      ($$fObjAdr)->avVrt($avr_vertices);
      ($$fObjAdr)->avXi($avr_xivertices);
      ($$fObjAdr)->avKn($avr_knvertices);
      ($$fObjAdr)->avTrGd($avr_trck_nfit15);
      ($$fObjAdr)->avPrfit($avr_prtrck_nfit15);
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

  print  "files to be inserted:", $mjID, " % ",$mpath, " % ",$timeS , " % ", $memFst," % ",$memLst," % ", $mavail, "\n";  
    &fillJSTable();
##############

   if($mpath =~ /AuAu200_production_low_2014/ or $mpath =~ /AuAu200_production_mid_2014/ or $mpath =~ /production_pp200long_2015/ or $mpath =~ /production_pAu200_2015/  or $mpath =~ /AuAu200_production_2016/) {

       print "Fillin QA table  ", $mpath, "\n";
    next if ($mpath =~ /nohft/);
    &updateQATable();
    &fillQATable();
   }

############

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
 $nevent_vtx = 0; 
 $numevt_vtx = 0;
 $avr_tracks = 0;
 $avr_prvertx = 0;
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
    $pyear =   ($$newjobFile)->pyr;
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
    $nevent_vtx=  ($$newjobFile)->nEvtVtx;
    $avr_prvertx= ($$newjobFile)->avPrVtx;
    $avr_prtracks = ($$newjobFile)->avPrTr;
    $avr_vertices = ($$newjobFile)->avVrt;
    $avr_xivertices = ($$newjobFile)->avXi;
    $avr_knvertices = ($$newjobFile)->avKn;
    $avr_trck_nfit15 =  ($$newjobFile)->avTrGd;
    $avr_prtrck_nfit15 =  ($$newjobFile)->avPrfit;
    $avr_prtracks_1vtx = ($$newjobFile)->avPrv1; 
    $avr_prtrck_nfit15_1vtx = ($$newjobFile)->avPrfitv1; 
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

      $new_id = $dbh->{'mysql_insertid'};
      $mavail = 'Y';
      $myID = 100000000 + $new_id;
      $mjID = "Job". $myID ;
      $crCode = "n/a"; 
 print "Insert new files: ", $mjID, " % ",$fullName, "\n";
      $idHash{$fullName} = $mjID;
###########################

    &fillJSTable();

     if($mpath =~ /AuAu200_production_2016/) {

       print "Fillin QA table  ", $mpath, "\n";
#    next if ($mpath =~ /nohft/);

    &fillQATable();
   }


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


my @fileR;
######## read output files for DEV test at testDay

@prt = ();


foreach  $eachOutNDir (@OUT_DIR) {
         if (-d $eachOutNDir) {
    opendir(DIR, $eachOutNDir) or die "can't open $eachOutNDir\n";

#    while( defined($flname = readdir(DIR)) ) {
           @fileR = readdir(DIR);  
   foreach $flname ( @fileR) {
       next if !$flname;
      next if $flname =~ /^\.\.?$/;    
      if ($flname =~ /.root/)   {
      $fullname = $eachOutNDir."/".$flname;
      @dirF = split(/\//, $eachOutNDir);
       $libL = $dirF[4];
       $platf = $dirF[5]; 
       $geom = $dirF[7];
       $EvTp = $dirF[8];
       if ($EvTp =~ /hc_/) {
       $EvGen = "hadronic_cocktail";
       $EvType = substr($EvTp,3); 
    }
       elsif ($EvTp =~ /pp_/ ) {
       $EvGen = "pythia";
       $EvType = "pp_minbias";
     }
       elsif ($EvTp =~ /ppl_/) {
       $EvGen = "pythia";
       $EvType = "ppl_minbias";
     }
       elsif ($EvTp =~ /peripheral/) {
       $EvGen = "hadronic_cocktail";
       $EvType = $EvTp;
    }
      elsif ($EvTp eq "dau_minbias") {
       $EvGen = "hijing";
       $EvType = "dau_minbias";

    } elsif ($EvTp eq "auau_minbias") {
       $EvGen = "hijing";
       $EvType = "auau_minbias";
 
   } elsif ($EvTp eq "cucu200_minbias") {
       $EvGen = "hijing";
       $EvType = "cucu200_minbias";

  } elsif ($EvTp eq "cucu62_minbias") {
       $EvGen = "hijing";
       $EvType = "cucu62_minbias";

      } else {
       $EvGen = "daq";
       $EvType = $EvTp;
  }
       $form = "root";
    my $comname = basename("$flname",".root");
      if ($comname =~ m/\.([A-Za-z0-9_]{3,})$/) {
      $comp = $1;
    }     
      my $bsname = basename("$comname","$comp");
       if($flname =~ /2gamma/) {
       @prt = split("_",$bsname);
        $evR = $prt[2];
        $EvReq = $EvReq = substr($evR,0,-5);
   }
      elsif($EvTp eq "minbias") {
        $EvReq = 192; 
     } 
     elsif($EvTp eq "central") {
        $EvReq = 85;
      }
     elsif($EvTp eq "ppMinBias" and $geom = "year_2001") {
        $EvReq = 700;
      }
     elsif($EvTp eq "ppMinBias" and $geom = "year_2003") {
        $EvReq = 900;
      }
     elsif($EvTp eq "embedding") {
        $EvReq = 78;
      }
     elsif($EvTp eq "dAuMinBias") {
        $EvReq = 497;
      }

     elsif($EvTp eq "AuAuMinBias") {
        $EvReq = 100;
      }
     elsif($EvTp eq "AuAu_prodHigh") {
        $EvReq = 100;
      }
     elsif($EvTp eq "AuAu_prodLow") {
        $EvReq = 100;
      }
    elsif($EvTp eq "prodPP") {
        $EvReq = 500;
      }

     elsif($EvTp eq "CuCu200_MinBias") {
        $EvReq = 200;
      }
     elsif($EvTp eq "CuCu200_HighTower") {
        $EvReq = 200;
      }
     elsif($EvTp eq "CuCu62_MinBias") {
        $EvReq = 200;
      }
     elsif($EvTp eq "CuCu22_MinBias") {
        $EvReq = 200;
      }
     elsif($EvTp eq "ppProduction") {
        $EvReq = 500;
      }
     elsif($EvTp eq "ppProdLong") {
        $EvReq = 1000;
      }
     elsif($EvTp eq "ppProdTrans") {
        $EvReq = 1000;
      }
     elsif($EvTp eq "2007ProductionMinBias") {
        $EvReq = 100;
      }
     elsif($EvTp eq "ppProduction2008") {
        $EvReq = 1000;
      }
     elsif($EvTp eq "production_dAu2008") {
        $EvReq = 400;
      }

     elsif($EvTp eq "production2009_500GeV") {
        $EvReq = 500;
      }

#       else {
#      @prt = split(/\./,$bsname);
#      $evR = $prt[1];
#      $EvReq = substr($evR,0,-5);
    elsif($EvTp eq "hc_lowdensity") {          
         $EvReq = 100;
   }  
    elsif($EvTp eq "hc_highdensity") {          
         $EvReq = 16;
   }  
    elsif($EvTp eq "hc_standard") {          
         $EvReq = 20;
 }
    elsif($EvTp eq "peripheral") {          
         $EvReq = 500;
 }
    elsif($EvTp eq "ppl_minbias") {          
         $EvReq = 100;
 }
    elsif($EvTp eq "pp_minbias") {          
         $EvReq = 1000;
 }
     elsif($EvTp eq "dau_minbias") {          
         $EvReq = 500;
 }
     elsif($EvTp eq "auau_minbias") {          
         $EvReq = 100;
 }
    elsif($EvTp eq "auau_central") {          
         $EvReq = 50;
 }
     elsif($EvTp eq "cucu200_minbias") {          
         $EvReq = 200;
 }
    elsif($EvTp eq "cucu62_minbias") {          
         $EvReq = 200;
 }
    elsif($EvTp eq "auau200_central") {          
         $EvReq = 50;
 }     
      if( $bsname =~ /hc_highdensity/) {
      $lgFile = $eachOutNDir ."/" . $bsname ."16_evts.log" ;
      }elsif( $bsname =~ /hc_lowdensity/) {
      $lgFile = $eachOutNDir ."/" . $bsname ."400_evts.log" ;
      }elsif( $bsname =~ /hc_standard/) {
      $lgFile = $eachOutNDir ."/" . $bsname ."40_evts.log" ;
     }else{
       $lgFile = $eachOutNDir ."/" . $bsname ."log" ;
     }

     if( $EvGen eq "hadronic_cocktail" and $flname =~ /tags.root/)  {
       $lgFile = $eachOutNDir ."/" . $bsname ."log" ;
     }

#      print $lgFile, "\n"; 
       if ( -f $lgFile) {
          ($size, $mTime) = (stat($lgFile))[7, 9];
            $ltime = $now - $mTime;
#           if( $ltime > 1200 && $ltime < 518000 ){         
            if( $ltime > 1200) { 
	     foreach my $eachLogFile (@testJobStFiles) {

               $jpath   = ($$eachLogFile)->pth; 
               $EvDone  = ($$eachLogFile)->evDn;
               $EvSkip  = ($$eachLogFile)->evSkp;
               $jfile   = ($$eachLogFile)->lgName;
               $libV    = ($$eachLogFile)->lbT;
               $jfpath  = $jpath . "/" .$jfile;
  
               if ( $jfpath eq $lgFile ) {

            next if( !defined $idHash{$jfpath} );
              $idHash{$fullname} = $idHash{$jfpath};

     print "jobs ID:  ", $idHash{$fullname}," % ",$fullname," % ",$jfpath," % ",$idHash{$jfpath}, "\n";
#       print "File info: ",$idHash{$fullname}," % ", $jpath ," % ", $platf, " % ", $fullname, " % ", $geom, " % ", $EvType," % ", $EvGen, " % ", $EvReq," % ", $comp," % ", $EvDone," % ", $libV,  "\n";
   
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

     $Fname = $eachOutNDir . "/" .$flname;
     $flagHash{$Fname} = 1;
 
     $fObjAdr = \(FileAttr->new());

     ($$fObjAdr)->filename($flname);
     ($$fObjAdr)->fpath($eachOutNDir);
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

 print "Total output files for DEV test at testDay: $nOutNFiles\n";


##### initialize variables


 my @old_set;
 my $nold_set = 0;
 my $eachOldFile;

##### make files from previous test in $thisday directories in DB unavailable 

#####  select all files from FilesCatalog from testDay directories

 $sql="SELECT jobID, path, fName, createTime, component, avail FROM $FilesCatalogT WHERE path LIKE '%$testDay%' AND avail = 'Y' ";
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
        ($$fObjAdr)->oldTime($fvalue)   if( $fname eq 'createTime');
        ($$fObjAdr)->oldcomp($fvalue)   if( $fname eq 'component');
        ($$fObjAdr)->oldvail($fvalue)   if( $fname eq 'avail'); 
   }

       $old_set[$nold_set] = $fObjAdr;
       $nold_set++; 

 }

#####  select all files from FilesCatalog from beforeDay direcroties

  $sql="SELECT jobID, path, fName, createTime, component, avail FROM $FilesCatalogT WHERE path LIKE '%$beforeDay%' AND avail = 'Y'";
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
        ($$fObjAdr)->oldTime($fvalue)   if( $fname eq 'createTime');
        ($$fObjAdr)->oldcomp($fvalue)   if( $fname eq 'component');  
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

#####  change avail for files from testDay and beforeDay directories
#####  in FilesCatalogT if new files available and fill in new files

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
       

   foreach my $eachOldFile (@old_set) {
          $pvjbId = ($$eachOldFile)->oldjbId;
          $pvpath = ($$eachOldFile)->oldpath;
          $pvfile = ($$eachOldFile)->oldfile;
          $pvTime = ($$eachOldFile)->oldTime;
          $pvcomp = ($$eachOldFile)->oldcomp;
          $pvavail = ($$eachOldFile)->oldvail;
          $pfullName = $pvpath . "/" . $pvfile;

#	   if ($pfullName eq $thfullName) {
            if ( ($pvpath eq $mpath) and ( $mcomp eq $pvcomp ) ) {
               $flagHash{$thfullName} = 0;      
 	     if ( $mcTime ne $pvTime ) {
               $mjID = $idHash{$thfullName};
      print   $thfullName, " % ",$mjID, "\n";   
              $newAvail = "N";
   print "Changing availability for test files", "\n";
   print "file to be updated:", $pvjbId, " % ", $pfullName, " % ",$pvTime, " % ", $newAvail, "\n"; 
   &updateDbTable();

           
    print "Filling Files Catalog with DEV output files for testDay and beforeDay\n";
    print "file to be inserted:", $mjID, " % ",$thfullName, " % ", $mcTime," % ", $mavail, "\n";
   &fillDbTable();

	    }else{
	    }
	}else{          
         next; 
       }
       }
      }


### insert new test files

        foreach my $newTestFile (@testOutNFiles) {

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

            $mpath = ($$newTestFile)->fpath;
            $mflName = ($$newTestFile)->filename;
            $thfullName = $mpath ."/" . $mflName;
            $mevtType = ($$newTestFile)->evtType;
            $mLibL    = ($$newTestFile)->flibL;
            $mLibT    = ($$newTestFile)->flibV; 
            $mplform  = ($$newTestFile)->fplatf; 
            $mpath    = ($$newTestFile)->fpath;
            $mgeom    = ($$newTestFile)->fgeom;
            $msize    = ($$newTestFile)->fsize;
            $mevtGen  = ($$newTestFile)->evtGen;
            $mcTime   = ($$newTestFile)->ftime;
            $mNevtR   = ($$newTestFile)->evtReq;
            $mNevtD   = ($$newTestFile)->evtDone; 
            $mNevtS   = ($$newTestFile)->evtSkip;
            $mformat  = ($$newTestFile)->fformat;
            $mcomp    = ($$newTestFile)->fcomp;
            $mavail   = "Y";

       if($flagHash{$thfullName} == 1) {
         
           $mjID = $idHash{$thfullName};

    print "Filling Files Catalog with new DEV output files for testDay and beforeDay\n";
    print "Insert new files:", $mjID, " % ",$thfullName, " % ", $mcTime," % ",$mNevtR, " % ",$mNevtD, "\n";
   &fillDbTable();

	 }
}

##### fill in FilesCatalog with new test files             

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
     $sql.=" WHERE path = '$pvpath' AND fName = '$pvfile' AND avail = 'Y'";
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
    $sql.="prodyear='$pyear',";
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
    $sql.="NoEventVtx='$nevent_vtx',";
    $sql.="avgNoVtx_evt='$avr_prvertx',";
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
############
sub fillQATable {

    $sql="insert into $JobQAT set ";
    $sql.="jobID='$mjID',";
    $sql.="LibTag='$libV',";
    $sql.="path='$mpath',";
    $sql.="logFile='$logName',";
    $sql.="createTime='$jobTime',";
    $sql.="jobStatus='$jrun',";
    $sql.="NoEventDone='$EvDone',";
    $sql.="PxlHits='$avgPxlHits',";
    $sql.="AsstPxlHits='$avgAssPxlHits',";
    $sql.="IstHits='$avgIstHits',";
    $sql.="AsstIstHits='$avgAssIstHits',";
    $sql.="SstHits='$avgSstHits',";
    $sql.="AsstSstHits='$avgAssSstHits',";
    $sql.="MtdMatchHits='$avgmatchMtdHits',";
    $sql.="MtdHits='$avgMtdHits',";
    $sql.="avail='$mavail'";  
#    print "$sql\n";
    print "$sql\n" if $debugOn;
    $rv = $dbh->do($sql) || die $dbh->errstr;
    $new_id = $dbh->{'mysql_insertid'}; 
}

###########
sub  updateJSTable {

     $sql="update $JobStatusT set ";
     $sql.="avail='$newAvail'";
     $sql.=" WHERE path = '$pvpath' AND logFile = '$pvfile' AND avail = 'Y'";   
     print "$sql\n" if $debugOn;
     $rv = $dbh->do($sql) || die $dbh->errstr;

   }

###########
sub  updateQATable {

     $sql="update $JobQAT set ";
     $sql.="avail='$newAvail'";
     $sql.=" WHERE path = '$pvpath' AND jobID = '$pvjbId' AND avail = 'Y'";   
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
 my @prt = ();
 my @prtt = ();
 my $mixer;
 my @size_line;
 my @memSize;
 my @cpu_output;
 my $ij = 0;
 my $end_line; 
 my $npr = 0;
 my $nevt = 0;
 my $no_prvertx = 0;
 my @vrank = ();

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

 $nMtdHits = 0;
 $nPxlHits = 0;
 $totMtdHits = 0;
 $totPxlHits = 0;
 $avgMtdHits = 0;
 $avgPxlHits = 0;
 $nIstHits = 0;
 $totIstHits = 0;
 $avgIstHits = 0;
 $nSstHits = 0;
 $totSstHits = 0;
 $avgSstHits = 0;

 $nAssIstHits = 0;
 $totAssIstHits = 0;
 $avgAssIstHits = 0;

 $nAssPxlHits = 0;
 $totAssPxlHits = 0;
 $avgAssPxlHits = 0;

 $nAssSstHits = 0;
 $totAssSstHits = 0;
 $avgAssSstHits = 0;

 @nmatchMtdHits = ();
 $totmatchMtdHits = 0;
 $avgmatchMtdHits = 0;
 $NevtMtdHits = 0;
 
 $nmatchMtdHits[0] = 0; 

 my $nnline = 0;

#---------------------------------------------------------

#  print $fl_log, "\n";

 $nevent_vtx = 0;

# $nevent_vtx = `grep '#V\[  0\]' $fl_log | wc -l` ;

# print "Number of events with primary vertex  ",$nevent_vtx, "\n";

  open (LOGFILE, $fl_log ) or die "cannot open $fl_log", "\n";

   my @logfile = <LOGFILE>;

my $Anflag = 0;
my $runflag = 0;
my $mCPUbfc = 0;
my $mRealTbfc = 0;
my $embflag = 0;
my @tmm = ();
my $mixline = "StRoot/macros/embedding";
my $evtcomp = 0;
my $Err_messg = "none";
my $ntemp = 0;


$jrun = "Run not completed";

  if($fl_log =~ /embed/) {

   $embflag = 1;
 }

   foreach my $line (@logfile) {
       chop $line ;
        $num_line++; 

       if($line =~ /Processing bfc.C/) {
          $runflag++;
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
#     @part = split( "/", $line) ;
#     $mchain = $part[9];     
      @part = split( "embedding/", $line) ;
      $mixer = $part[1];
      @prt = ();
      @prt = split( ".C", $mixer) ;
      $mchain = $prt[0].".C";

# print $line, "\n";
# print $mchain, "\n";

         }
      
   if($fl_log =~ /AuAu200_production_low_2014/  or $fl_log =~ /AuAu200_production_mid_2014/ or $fl_log =~ /production_pp200long_2015/ or $fl_log =~ /production_pAu200_2015/ or $fl_log =~ /AuAu200_production_2016/) {

#       print "Check path   ",$fl_log,"\n";

      if ( $line =~ /StMtdHitMaker:INFO/ and  $line =~ /MTD hits in event/) {
      @prt = ();
      @prt = split( " ", $line) ;
      $nMtdHits = $prt[2];
      $totMtdHits += $nMtdHits;
      }
      if ( $line =~ /PixelHitLoader loaded/) {
      @prt = ();
      @prt = split( " ", $line) ;
      $nPxlHits = $prt[6];
      $totPxlHits += $nPxlHits;
     }elsif($line =~ /IstHitLoader loaded/) {
      @prt = ();
      @prt = split( " ", $line) ;
      $nIstHits = $prt[6];
      $totIstHits += $nIstHits;

     }elsif($line =~ /SstHitLoader loaded/) {

      @prt = ();
      @prt = split( " ", $line) ;
      $nSstHits = $prt[6];
      $totSstHits += $nSstHits;

     }

     if ( $line =~ /Number of used hits:PxlId/) {

      @prt = ();
      @prt = split( ":", $line) ;
      $ntemp = $prt[4];
      @prtt = ();
      @prtt = split( " ", $ntemp) ;      
      $nAssPxlHits = $prtt[0];
      $totAssPxlHits += $nAssPxlHits;

#      $nnline++;
#      print "PXL associate hits  =  ", $nAssPxlHits,"  ","Sum  = ",$totAssPxlHits,"   ",$nnline,   "\n";

     }elsif($line =~ /Number of used hits:IstId/) {


      @prt = ();
      @prt = split( ":", $line) ;
      $ntemp = $prt[4];
      @prtt = ();
      @prtt = split( " ", $ntemp) ;  

      $nAssIstHits = $prtt[0];
      $totAssIstHits += $nAssIstHits;

     }elsif($line =~ /Number of used hits:SstId/) {


      @prt = ();
      @prt = split( ":", $line) ;
      $ntemp = $prt[4];
      @prtt = ();
      @prtt = split( " ", $ntemp) ;  

      $nAssSstHits = $prtt[0];
      $totAssSstHits += $nAssSstHits;

     }

     if ( $line =~ /mtd hit matched with track successfully/ ) {

	 $nmatchMtdHits[$no_event]++;
#     print "Number of matched MTD hits  ", $nmatchMtdHits[$no_event], "  ", "in the event #",$no_event, "\n";

     }
   }

#   get  number of events
#     if ( $line =~ /QAInfo: Done with Event/ ) {
      if ( $line =~ /Done with Event/ ) {
        $no_event++;
        $nmatchMtdHits[$no_event] = 0;  
     } 

       if ( $line =~ /Reading Event:/ ) {
        $evtcomp++;
     }   

#  get memory size
      if ($num_line > 500){
	if( $line =~ /EndMaker/ and $line =~ /total/ ) {

        @size_line = split(" ",$line); 
        $mymaker = $size_line[3];
        @memSize = split("=",$size_line[6]);
        if( $mymaker eq "outputStream:"){

          $maker_size[$evtcomp] = $memSize[1];

       }
      }
    }


# get number of tracks and vertices

      if ($line =~ /QA :INFO  - StAnalysisMaker/ && $Anflag == 0 ) {

  @nmb = ();
  @nmbx = ();
  @word_tr = ();
  $npr = 0;
   
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
              @vrank = ();
              $vrank[0] = -0.01 ;              

            for ($ik = 1; $ik< 120; $ik++)  { 
              $string = $logfile[$num_line + $ik];
              chop $string;

           if(  $string =~ /QA :INFO/ and $string =~ /Rank/ and $string =~ /#V/ ) {

              @word_tr = ();
              @nmbx = ();
              @word_tr = split (":",$string);
              @nmbx = split (" ",$word_tr[4]);
#         print "Check splitting   ",$word_tr[3]," %  ", $word_tr[4]," %  ", $word_tr[5]," % ", $word_tr[6], "\n"; 
             $vrank[$npr] = $nmbx[0];

             my $string2 = $logfile[$num_line + $ik+1];
             chop $string2;            
             my  $string3 = $logfile[$num_line + $ik+2];
             chop $string3;

          if( $string2 =~ /MessageKey/ and $string2 =~ /primary all/ ) {
             $no_prvertx++;
              @word_tr = split /=/,$string2;
              @nmb =  split ("'",$word_tr[3]);
              $no_prtracks[$npr] = $nmb[1];   

          }

            if( $string3 =~ /MessageKey/ and $string3 =~ /primary good/ ) {
              @word_tr = ();
              @nmbx = ();
              @word_tr = split /=/,$string3;
              @nmbx =  split ("'",$word_tr[3]);
              $no_prtrck_nfit15[$npr]  = $nmbx[1];

           }
#          print "Vertex rank ", $npr,"   ",$vrank[$npr],"   ", $no_prtracks[$npr], "   ", $no_prtrck_nfit15[$npr],"\n"; 

              $npr++;
          }

#######
             if( $string =~ /V0 vertices/) { 
              @word_tr = split /:/,$string;
              @nmb =  split /</,$word_tr[2];
              $no_vertices = $nmb[0];              
              $tot_vertices += $no_vertices;
            } elsif( $string =~ /Xi vertices/) { 
              @word_tr = split /:/,$string;
              @nmb =  split /</,$word_tr[2];
              $no_xivertices = $nmb[0];
              $tot_xivertices += $no_xivertices;
        } 
       }

             if ($vrank[0] > 0.00000001 and $embflag == 0 ) {
              $numevt_vtx++;
              $no_prtracks_1vtx = $no_prtracks[0];
              $no_prtrck_nfit15_1vtx  = $no_prtrck_nfit15[0]; 

              $tot_prtracks += $no_prtracks[0];
              $tot_prtrck_nfit15 += $no_prtrck_nfit15[0];
              $tot_prtracks_1vtx += $no_prtracks_1vtx;
              $tot_prtrck_nfit15_1vtx += $no_prtrck_nfit15_1vtx;                  

           }elsif($embflag == 1 and $vrank[0] == -5 ) {

              $numevt_vtx++;
              $no_prtracks_1vtx = $no_prtracks[0];
              $no_prtrck_nfit15_1vtx  = $no_prtrck_nfit15[0]; 

              $tot_prtracks += $no_prtracks[0];
              $tot_prtrck_nfit15 += $no_prtrck_nfit15[0];
              $tot_prtracks_1vtx += $no_prtracks_1vtx;
              $tot_prtrck_nfit15_1vtx += $no_prtrck_nfit15_1vtx;      

           }
             if ($npr >= 1 ) {
              $nevent_vtx++;
          }

      }
   }


#  check if job crashed due to break_buss_error
      if($line =~ /bus error/) {
          $Err_messg = "Break bus error";

#  check if job crashed due to segmentation violation
   }elsif ($line =~ /segmentation violation/ or $line =~ /Segmentation violation/ ) {
          $Err_messg = "segmentation violation";

   }elsif ($line =~ /segmentation fault/ or $line =~ /Segmentation fault/ ) {
          $Err_messg = "segmentation fault";

   } elsif ($line =~ /Stale NFS file handle/) {
           $Err_messg = "Stale NFS file handle";

   } elsif ( $line =~ /Assertion/ & $line =~ /failed/)  {
         $Err_messg = "Assertion failed";

  }elsif ($line =~ /Tried to find a host for 500 times, will abort now/)  {
             $Err_messg = "DB connection failed";

  }elsif ($line =~ /Killed/)  {
             $Err_messg = "Killed";

   }elsif ($line =~ /Abort/)  {
             $Err_messg = "Abort";

   }elsif ($line =~ /StFATAL/)  {
             $Err_messg = "StFATAL";

#   }elsif ($line =~ /glibc detected/)  {
#             $Err_messg = "glibc detected";

   } elsif ($line =~ /Catch exception FATAL/) {
  
         $Err_messg = "FATAL";
   
   }elsif ($line =~ /FATAL/ and $line =~ /floating point exception/) {

         $Err_messg = "Floating point exception";

   } elsif ($line =~ /Fatal in <operator delete>/) {
  
         $Err_messg = "Fatal in <operator delete>";   
   }
       elsif ($line =~ /Fatal in <operator new>/) {
  
       $Err_messg = "Fatal in <operator new>";   
  }
       elsif ($line =~ /std::runtime_error/) {
  
       $Err_messg = "runtime_error";   
  }

       if ( $line =~ /INFO  - QAInfo:Run/ and $line =~ /Total events processed/) {
         @part = split /:/,$line;
        $EvSkip = $part[4];
     }      
# check if job is completed
     if ( $line =~ /Run completed/ and $Err_messg eq "none") {
 
           $jrun = "Done";      

      }elsif($Err_messg ne "none" ){

      $jrun = "$Err_messg";

         }else{

      }

###### 
     
       } 
#     }       # $fl_log
#### check here
   } 
      $EvDone = $no_event;
      $EvCom = $EvDone - $EvSkip;

##### get CPU and Real Time per event

 if ($EvCom != 0) {
    @cpu_output = `tail -2000 $fl_log`;
 
  foreach $end_line (@cpu_output){
          chop $end_line;

  if($embflag == 0 ) { 
    
  if ($end_line =~ /QAInfo:Chain/ and $end_line =~ /StBFChain::bfc/) {

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
 
  }elsif($embflag == 1 ) {  


  if ($end_line =~ /QAInfo:Chain/ and $end_line =~ /StChain::Embedding/) {

      @part = split (" ", $end_line); 
      $mCPUbfc = $part[8];
      $mRealTbfc = $part[6];
      $mCPUbfc = substr($mCPUbfc,1) + 0;
      $mRealTbfc = substr($mRealTbfc,1) + 0;
#     print "CPU ", $mCPUbfc,"   %   ", $mRealTbfc, "\n";
     $mCPU = $mCPUbfc/$EvCom;
     $mRealT = $mRealTbfc/$EvCom;
#   print "CPU and RealTime  ",$EvCom,"   ",$mCPU, "    ",$mRealT, "\n";
   
   }else{
    next;
      }
    }
   }
    $perct_usb        = ($nevt/$EvCom)*100;
    $avr_tracks       = $tot_tracks/$EvCom;
    $avr_vertices     = $tot_vertices/$EvCom;
    $avr_trck_nfit15  = $tot_trck_nfit15/$EvCom;   

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

       if($numevt_vtx >= 1 ) {
    $avr_prtracks     = $tot_prtracks/$numevt_vtx;
    $avr_prtrck_nfit15  = $tot_prtrck_nfit15/$numevt_vtx; 
    $avr_prtracks_1vtx = $tot_prtracks_1vtx/$numevt_vtx;
    $avr_prtrck_nfit15_1vtx = $tot_prtrck_nfit15_1vtx/$numevt_vtx;  
 
    }else{
    $avr_prtracks = 0;
    $avr_prtrck_nfit15  = 0; 
    $avr_prtracks_1vtx = 0 ;
    $avr_prtrck_nfit15_1vtx = 0 ;
   } 

       if($nevent_vtx >= 1 ) {
   $avr_prvertx      = $no_prvertx/$nevent_vtx;
    }else{
   $avr_prvertx      = 0;
   } 


#  print "Number of vertices = ", $no_prvertx,"   ", "Number of events ", $no_event,"  ",$EvCom,"  ",$nevent_vtx,"  ",$numevt_vtx, "  Average No vtx = ", $avr_prvertx,"   ","Avg no primary tracks   ", $avr_prtracks,"   ",$avr_prtrck_nfit15, "\n"; 

  if($fl_log =~ /AuAu200_production_low_2014/ or $fl_log =~ /AuAu200_production_mid_2014/ or $fl_log =~ /production_pp200long_2015/ or $fl_log =~ /production_pAu200_2015/ or $fl_log =~ /AuAu200_production_2016/) {

     if($EvDone >= 1) {
     $avgMtdHits = $totMtdHits/$EvDone;
     $avgPxlHits = $totPxlHits/$EvDone;
     $avgIstHits = $totIstHits/$EvDone;
     $avgSstHits = $totSstHits/$EvDone;
     $avgAssPxlHits = $totAssPxlHits/$EvDone;
     $avgAssIstHits = $totAssIstHits/$EvDone;
     $avgAssSstHits = $totAssSstHits/$EvDone;


     }
  print "Avg #MtdHits = ",  $avgMtdHits,"   ", "Avg #PxlHits = ", $avgPxlHits,"   ", "Avg #IstHits = ", $avgIstHits,"   ", "Avg #SsdHits = ", $avgSstHits,"\n";

  print " Events = ", $EvDone, "  Avg #AssPxlHits = ", $avgAssPxlHits,"   ", "Avg #AssIstHits = ", $avgAssIstHits,"   ", "Avg #AssSstHits = ", $avgAssSstHits,"\n";


     for ($jj = 0; $jj < $no_event; $jj++ ) {
	 if ( $nmatchMtdHits[$jj] >= 1 ) {
	     $totmatchMtdHits += $nmatchMtdHits[$jj];
             $NevtMtdHits++;
         }
     }
        if($NevtMtdHits >= 1) {
          $avgmatchMtdHits = $totmatchMtdHits/$NevtMtdHits;      
        }
  print "Avg number of matched Mtd Hits = ",  $avgmatchMtdHits,"   ", "in number of events =  ", $NevtMtdHits, "\n"; 

  }

    if ( defined $maker_size[0]) { 
    $memFst = $maker_size[0];
    }else {
    $memFst = 0;
  }
    if ( defined $maker_size[$evtcomp -1]) {
    $memLst = $maker_size[$evtcomp -1];
    } else {
    $memLst = 0;
  }
 }      
# print "Memory size:   ",$memFst, "   ", $memLst, "\n";
   
   close (LOGFILE);

#  } # close log file

 }


######################
sub StDbTJobsConnect {
    $dbh = DBI->connect("dbi:mysql:$dbname:$dbhost", $dbuser, $dbpass)
        || die "Cannot connect to db server $DBI::errstr\n";
}

######################
sub StDbTJobsDisconnect {
    $dbh = $dbh->disconnect() || die "Disconnect failure $DBI::errstr\n";
}


##########################
