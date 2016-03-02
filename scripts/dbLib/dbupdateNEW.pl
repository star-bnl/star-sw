#! /opt/star/bin/perl -w
#
# $Id:
#
# $Log: L.Didenko
#
# dbupdateNEW.pl
#
# update JobStatus and FileCatalog with test jobs for NEW& NEW_embed library
# Author: L.Didenko
#
##############################################################################

use Class::Struct;
use File::Basename;

require "/afs/rhic.bnl.gov/star/packages/scripts/dbLib/dbTJobsSetup.pl";

my $TOP_DIRD = "/star/rcf/test/new/";

my @dir_year = ("year_2000", "year_2001", "year_2003", "year_2004", "year_2005", "year_2006", "year_2007", "year_2008","year_2009","year_2010","year_2011","year_2012","year_2013","year_2014");
my @node_dir = ("daq_sl302.ittf", "daq_sl302.ittf_opt" ,"trs_sl302.ittf", "trs_sl302.ittf_opt","simu"); 

my @OUT_DIR0 = ();
my @OUT_DIR1 = ();
my @OUT_DIR2 = ();
my @OUT_DIR3 = ();
my @OUT_DIR4 = ();
my @OUT_DIR = ();
my @OUTD_DIR = ();
my @TDIR = ();
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

##### setup output directories 

 for ($i = 0; $i < scalar(@node_dir); $i++) {

    $TDIR[$i] = $TOP_DIRD . $node_dir[$i]."/*/*";
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
  print "Output Dir for NEW :", $OUT_DIR[$ii],"\n";
    $ii++;
}
  for ($i = 0; $i < scalar(@OUT_DIR1); $i++) {
     $OUT_DIR[$ii] = $OUT_DIR1[$i]; 
     chop $OUT_DIR[$ii];
  print "Output Dir for NEW :", $OUT_DIR[$ii],"\n";
     $ii++;
}

  for ($i = 0; $i < scalar(@OUT_DIR2); $i++) {
     $OUT_DIR[$ii] = $OUT_DIR2[$i];
     chop $OUT_DIR[$ii];  
  print "Output Dir for NEW :", $OUT_DIR[$ii],"\n";
   $ii++; 
}
  for ($i = 0; $i < scalar(@OUT_DIR3); $i++) {
     $OUT_DIR[$ii] = $OUT_DIR3[$i];
     chop $OUT_DIR[$ii]; 
  print "Output Dir for NEW :", $OUT_DIR[$ii],"\n";
      $ii++;  
  }

  for ($i = 0; $i < scalar(@OUT_DIR4); $i++) {
     $OUT_DIR[$ii] = $OUT_DIR4[$i];
     chop $OUT_DIR[$ii]; 
  print "Output Dir for NEW :", $OUT_DIR[$ii],"\n";
      $ii++;  
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
        avVrt     => '$',
        avTrGd    => '$',
        avPrfit   => '$',
        avPrv1    => '$',
        avPrfitv1 => '$',
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
 my $avr_knvertices = 0;
 my $avr_xivertices = 0;
 my $avr_trck_nfit15 = 0; 
 my $avr_prtrck_nfit15 = 0; 
 my $avr_prtracks_1vtx = 0;
 my $avr_prtrck_nfit15_1vtx = 0;
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

 $sql="SELECT jobID, path, logFile, createTime, avail FROM $JobStatusT WHERE path LIKE '%test/new%' AND avail = 'Y'";


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
	      next if ($eachOutLDir =~ /temp/) ;

     opendir(DIR, $eachOutLDir) or die "can't open $eachOutLDir\n";
      @files = readdir(DIR);
#    while( defined($fname = readdir(DIR)) ) {
     foreach $fname ( @files) {
      next if !$fname;
      next if $fname =~ /^\.\.?$/;    
      next if $fname =~ /hijing.log/;
      next if $fname =~ /starsim.log/;
     
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
 @prt = ();

      next if ($fname =~ /.simu.log/);
       if ($fname =~ /.log/)  {
#    print "File Name:",$fname, "\n";       
       $fullname = $eachOutLDir."/".$fname;
      $mpath = $eachOutLDir;
      @dirF = split(/\//, $eachOutLDir);
       $libL = $dirF[4];
       $platf = $dirF[5];
       @prt =split("_", $dirF[6]);
       $pyear = $prt[1]; 
       $logName = $fname; 
       next if ($mpath =~ /ppl_minbias/);
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
#   print "Log time: ", $ltime, "\n";
#   print $fullname, "\n";
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

  print  "Filling JobStatus with NEW log files \n";
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
###

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


my @fileR = ();
######## read output files for NEW test

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
       $geom = $dirF[6];
       $EvTp = $dirF[7];
 
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
    } elsif ($EvTp eq "auau200_central") {
       $EvGen = "hijing";
       $EvType = "auau200_central";

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
        $EvReq = 500;
      }
     elsif($EvTp eq "AuAu_prodHigh") {
        $EvReq = 335;
      }
     elsif($EvTp eq "AuAu_prodLow") {
        $EvReq = 332;
      }
     elsif($EvTp eq "prodPP") {
        $EvReq = 500;
      }   

     elsif($EvTp eq "CuCu200_MinBias") {
        $EvReq = 7120;
      }
     elsif($EvTp eq "CuCu200_HighTower") {
        $EvReq = 606;
      }
     elsif($EvTp eq "CuCu62_MinBias") {
        $EvReq = 800;
      }
     elsif($EvTp eq "CuCu22_MinBias") {
        $EvReq = 800;
      }
     elsif($EvTp eq "ppProduction") {
        $EvReq = 800;
      }   
     elsif($EvTp eq "ppProdLong") {
        $EvReq = 2000;
      }
     elsif($EvTp eq "ppProdTrans") {
        $EvReq = 2000;
      }      
     elsif($EvTp eq "2007ProductionMinBias") {
        $EvReq = 600;
      }
     elsif($EvTp eq "2007Production") {
        $EvReq = 600;
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
         $EvReq = 400;
   }  
    elsif($EvTp eq "hc_highdensity") {          
         $EvReq = 16;
   }  
    elsif($EvTp eq "hc_standard") {          
         $EvReq = 40;
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
    elsif($EvTp eq "auau_minbias") {          
         $EvReq = 100;
 }
     elsif($EvTp eq "cucu200_minbias") {          
         $EvReq = 200;
 }     
     elsif($EvTp eq "cucu62_minbias") {          
         $EvReq = 200;
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

 print "Total output files for NEW test: $nOutNFiles\n";


##### initialize variables


 my @old_set;
 my $nold_set = 0;
 my $eachOldFile;

##### make files from previous test in $thisday directories in DB unavailable 

#####  select all files from FilesCatalog from testDay directories

 $sql="SELECT jobID, path, fName, createTime, component, avail FROM $FilesCatalogT WHERE path LIKE '%test/new%' AND avail = 'Y'";
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

           
    print "Filling Files Catalog with NEW output files\n";
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

    print "Filling Files Catalog with new output files\n";
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
     $sql.=" WHERE path = '$pvpath' AND fName = '$pvfile' AND avail = 'Y' ";
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
    $sql.="avg_no_primaryTnfit15='$avr_prtrck_nfit15',";
    $sql.="avg_no_tracksnfit15='$avr_trck_nfit15',";
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
 my $no_tracks; 
 my $no_vertices;
 my $no_xivertices;
 my $no_knvertices;
 my @no_prtracks = ();
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

#---------------------------------------------------------

# print $fl_log, "\n";

 $nevent_vtx = 0;

# $nevent_vtx = `grep '# primary vertex(  0)' $fl_log | wc -l ` ;  

  open (LOGFILE, $fl_log ) or die "cannot open $fl_log: $!\n";

 my @logfile = ();

   @logfile = <LOGFILE>;

my $Anflag = 0;
my $runflag = 0;
my $mCPUbfc = 0;
my $mRealTbfc = 0;
my $embflag = 0;
my $mixline = "/StRoot/macros/embedding";
my @tmm = ();
my $evtcomp = 0;
my $Err_messg = "none";
$jrun = "Run not completed";


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
	  if($runflag == 1 or $embflag == 1) {
	      if ( $line =~ /Processing bfc.C/)   {
         if( $Anflag == 0 ) {
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
#       $mchain = $part[4];
      @part = split( "embedding/", $line) ;
      $mixer = $part[1];
      @part = ();
      @part = split( ".C", $mixer) ;
      $mchain = $part[0].".C";

       }
 
#   get  number of events
      if ( $line =~ /Done with Event/ ) {
        $no_event++;
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
### may change to $evtcomp
#          $maker_size[$evtcomp -1] = $memSize[1];
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
  $max_npr = 0;
  $max_npr_nfit15 = 0;


            my  $string = $logfile[$num_line];
            chop $string;

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
            
            for ($ik = 2; $ik< 100; $ik++)  { 
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
#           print "Vertex rank ", $npr,"   ",$vrank[$npr],"   ", $no_prtracks[$npr], "   ", $no_prtrck_nfit15[$npr],"\n";

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
#            } elsif( $string =~ /Kink vertices/) {
#              @word_tr = split /:/,$string;
#              @nmb =  split /</,$word_tr[2];
#              $no_knvertices = $nmb[0];
#              $tot_knvertices += $no_knvertices;

         }
      }

             if ($embflag == 0  and $vrank[0] > 0.00000001) {
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
        }

#  check if job crashed due to segmentation violation
     elsif ($line =~ /segmentation violation/ or $line =~ /Segmentation violation/) {
           $Err_messg = "segmentation violation";

   }elsif ($line =~ /segmentation fault/ or $line =~ /Segmentation fault/) {
          $Err_messg = "segmentation fault";

   }elsif ($line =~ /Stale NFS file handle/) {
         $Err_messg = "Stale NFS file handle";

   }elsif ( $line =~ /Assertion/ & $line =~ /failed/)  {
         $Err_messg = "Assertion failed";

  }elsif ($line =~ /Tried to find a host for 500 times, will abort now/)  {
             $Err_messg = "DB connection failed";

  }elsif ($line =~ /Killed/)  {
             $Err_messg = "Killed";

   }elsif ($line =~ /Abort/)  {
             $Err_messg = "Abort";

   }elsif ($line =~ /FATAL/ and $line =~ /floating point exception/) {
         $Err_messg = "Floating point exception";

#    }elsif ($line =~ /glibc detected/)  {
#             $Err_messg = "glibc detected";

   }elsif ($line =~ /Catch exception FATAL/) {

         $Err_messg = "FATAL";

  }elsif ($line =~ /Fatal in <operator delete>/) {
  
       $Err_messg = "Fatal in <operator delete>";   
   }elsif ($line =~ /Fatal in <operator new>/) {
  
       $Err_messg = "Fatal in <operator new>";   
   }

       if ( $line =~ /INFO  - QAInfo:Run/ and $line =~ /Total events processed/) {

        @part = split /:/,$line;
        $EvSkip = $part[4];
    }      
# check if job is completed

    if ( $line =~ /Run completed/ and $Err_messg eq "none") {
 
#     print "ERROR messages  ",   $Err_messg, "\n";
        
           $jrun = "Done"; 

      }elsif($Err_messg ne "none" ){

      $jrun = "$Err_messg";

         }else{

         }
#############
       } 
 }

       $EvDone = $no_event;
      $EvCom = $EvDone - $EvSkip;

##### get CPU and Real Time per event

 if ($EvCom != 0) {

   @cpu_output = ();

    @cpu_output = `tail -2000 $fl_log`;
 
  foreach $end_line (@cpu_output){
          chop $end_line;

   if($embflag == 0 ) {
   if ($end_line =~ /QAInfo:Chain/ and $end_line =~ /StBFChain::bfc/) {
     @part = split (" ", $end_line); 
      $mCPUbfc = $part[8];
      $mRealTbfc = $part[6];
      $mCPUbfc = substr($mCPUbfc,1) + 0;
      $mRealTbfc = substr($mRealTbfc,1) + 0;

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
    $avr_tracks     = $tot_tracks/$EvCom;
    $avr_vertices   = $tot_vertices/$EvCom;
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


    if ( defined $maker_size[0]) { 
    $memFst = $maker_size[0];
    }else {
    $memFst = 0;
  }
    if ( defined $maker_size[$evtcomp -1]) {
    $memLst = $maker_size[$evtcomp -1];
    } elsif( defined $maker_size[$evtcomp -2]) {
    $memLst = $maker_size[$evtcomp -2];
   }else{
    $memLst = $maker_size[$evtcomp -3];
  }
 }      

   close (LOGFILE);
 }
