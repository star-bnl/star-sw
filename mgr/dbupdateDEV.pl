#! /opt/star/bin/perl -w
#
# 
#
#  L.Didenko
#
# dbupdateDEV.pl
#
# Create File Catalog for nightly test files
# Run this script next day after jobs have been submitted
##############################################################################

use Mysql;
use File::Find;
use Net::FTP;
use Class::Struct;
use File::Basename;

require "/afs/rhic/star/packages/DEV00/mgr/dbTJobsSetup.pl";

#require "dbTJobsSetup.pl";

my $TOP_DIRD = "/star/rcf/test/dev/";
my @dir_year = ("year_1h", "year_2a");
my @node_dir = ("tfs_redhat61", "tfs_Solaris_CC5"), 
my @hc_dir = ("hc_lowdensity", "hc_standard", "hc_highdensity", "peripheral");

my @OUT_DIR;
my @OUTD_DIR;
my @Nday = ("Mon","Tue","Wed","Thu","Fri");


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
if($thisday eq "Mon") {
  $testDay = "Fri";
  $beforeDay = "Thu";

}else{
 $testDay = $Nday[$iday - 2];
  if($testDay eq "Mon") {
    $beforeDay = "Fri";
  }else{
 $beforeDay = $Nday[$iday - 3];
}
}
  print "Day Name: ",$thisday, " % ", "Index", $iday, "\n";

##### setup output directories for DEV with thisDay

for ($i = 0; $i < 2; $i++) {
     for ($j = 0; $j < 2; $j++) {
      for ($ll = 0; $ll < scalar(@hc_dir); $ll++) {
   $OUT_DIR[$ii] = $TOP_DIRD . $node_dir[$i] . "/" . $testDay . "/". $dir_year[$j] . "/" . $hc_dir[$ll];
    print "Output Dir for DEV :", $OUT_DIR[$ii], "\n";
        $ii++;
      }
  }
}

##### setup output directories for DEV with beforeDay

for ($i = 0; $i < 2; $i++) {
     for ($j = 0; $j < 2; $j++) {
      for ($ll = 0; $ll < scalar(@hc_dir); $ll++) {
   $OUT_DIR[$ii] = $TOP_DIRD . $node_dir[$i] . "/" . $beforeDay . "/". $dir_year[$j] . "/" . $hc_dir[$ll];
    print "Output Dir for DEV :", $OUT_DIR[$ii], "\n";
        $ii++;
      }
  }
}

struct FileAttr => {
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
      fsize     => '$',
      fformat   => '$',
      fcomp     => '$',          
};

struct JFileAttr => {
       oldpath   => '$',
       oldfile   => '$',
       oldvail   => '$',
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

my $debugOn = 0;

my @testInFiles;
my $nInFiles = 0;
my $eachInDir;
my @testOutNFiles;
my $nOutNFiles = 0;
my $eachOutNDir;
my $fullname;
my $flname;
my @prt;
my $libL;
my $libV = "n\/a";
my $platf;
my $EvTp;
my $lgFile;
my $EvDone = 0;
my $EvReq = 0;
my $form;
my $comp;
my $geom;
my $EvType;
my $EvGen;
my $evR;
my $comname;

######## read output files for DEV test at testDay

foreach $eachOutNDir (@OUT_DIR) {
        if (-d $eachOutNDir) {
   opendir(DIR, $eachOutNDir) or die "can't open $eachOutNDir\n";

   while( defined($flname = readdir(DIR)) ) {
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
        my $ltime = `mod_time $lgFile`;
# print "Log time: ", $ltime, "\n"; 
          if( $ltime > 3600 && $ltime < 345600 ){         
      &logInfo("$lgFile", 1);

#      print "File info: ", $libL," % ", $platf, " % ", $fullname, " % ", $geom, " % ", $EvType," % ", $EvGen, " % ", $EvReq," % ", $comp," % ", $EvDone," % ", $libV,  "\n";
   
    ($size, $mTime) = (stat($fullname))[7, 9];
    ($sec,$min,$hr,$dy,$mo,$yr) = (localtime($mTime))[0,1,2,3,4,5];
    $mo = sprintf("%2.2d", $mo+1);
    $dy = sprintf("%2.2d", $dy);
  
    if( $yr > 98 ) {
      $fullyear = 1900 + $yr;
    } else {
      $fullyear = 2000 + $yr;
    };


#    $timeS = sprintf ("%4.4d%2.2d%2.2d",
#                      $fullyear,$mo,$dy);
     $timeS = sprintf ("%4.4d-%2.2d-%2.2d %2.2d:%2.2d:00",
                     $fullyear,$mo,$dy,$hr,$min);


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
    ($$fObjAdr)->fformat($form); 
    ($$fObjAdr)->fsize($size);
    ($$fObjAdr)->ftime($timeS);
    ($$fObjAdr)->fcomp($comp); 
    $testOutNFiles[$nOutNFiles] = $fObjAdr;
   $nOutNFiles++; 
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


my @old_set;
my $nold_set = 0;
my $eachOldFile;
my $nbf_set = 0;
my @bf_set;
my $pvpath;
my $pvfile;
my $pvavail;
my $newAvail = "n\/a";
my $thpath;
my $thfile;
my $bfullName; 
my $bfavail;
my $thfullName;
my $pfullName;

##### make files from previous test in $thisday directories in DB unavailable 

 &StDbTJobsConnect();

##### select all files from FilesCatalog from testDay direcroties

 $sql="SELECT path, fileName, avail FROM $FilesCatalogT WHERE path LIKE '%$testDay%'";
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

       ($$fObjAdr)->oldpath($fvalue)   if( $fname eq 'path');
       ($$fObjAdr)->oldfile($fvalue)   if( $fname eq 'fileName'); 
       ($$fObjAdr)->oldvail($fvalue)   if( $fname eq 'avail'); 
  }

      $old_set[$nold_set] = $fObjAdr;
      $nold_set++; 

}

##### select all files from FilesCatalog from beforeDay direcroties

 $sql="SELECT path, fileName, avail FROM $FilesCatalogT WHERE path LIKE '%$beforeDay%'";
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

       ($$fObjAdr)->oldpath($fvalue)   if( $fname eq 'path');
       ($$fObjAdr)->oldfile($fvalue)   if( $fname eq 'fileName'); 
       ($$fObjAdr)->oldvail($fvalue)   if( $fname eq 'avail'); 
  }

      $bf_set[$nbf_set] = $fObjAdr;
      $nbf_set++; 

}

##### change avail for files from testDay directories in FC if new files produced

  foreach my $eachOldFile (@old_set) {
  
         $pvpath = ($$eachOldFile)->oldpath;
         $pvfile = ($$eachOldFile)->oldfile;
         $pvavail = ($$eachOldFile)->oldvail;
         $pfullName = $pvpath . "/" . $pvfile;

       foreach my $eachTestFile (@testOutNFiles) {
           $thpath = ($$eachTestFile)->fpath;
           $thfile = ($$eachTestFile)->filename;
            $thfullName = $thpath ."/" . $thfile;

 	  if (($pfullName eq $thfullName) and  ($pvavail eq "Y")) {
             $newAvail = "N";
  print "Changing availability for test files", "\n";
  print "file to be updated:",$pfullName, " % ", $newAvail, "\n"; 
        &updateDbTable();
	}else{
 	next;
       }
      }
     }
######### declare variables needed to fill the database table
## for database filling

my $mLibL= "n\/a"; 
my $mLibT = "n\/a";
my $mplform = "n\/a";
my $mevtType = "n\/a";
my $mflName = "n\/a";
my $mpath  = "n\/a";
my $mgeom = "n\/a";
my $msize = 0;
my $mevtGen = "n\/a";
my $mcTime = "0000-00-00";
my $mNevtR = 0;
my $mNevtD = 0;
my $mcomp = "n\/a";
my $mformat = "n\/a";
my $mavail = "n\/a";
my $mstatus = 0;


#######
 
# need to clear the FilesCatalog table first here
# $sql="delete from $FilesCatalogT";
# $cursor =$dbh->prepare($sql)
#    || die "Cannot prepare statement: $DBI::errstr\n";
# $cursor->execute;
  
####### fill in FilesCatalog with Output files from DEV for testDay

   foreach my $eachTestFile (@testOutNFiles) {

## reinitialize variables

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
 $mcomp = "n\/a";
 $mformat = "n\/a";
 $mavail = "n\/a";
 $mstatus = 0;

## end of reinitialization

        $mfName  = ($$eachTestFile)->filename;        
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
        $mformat  = ($$eachTestFile)->fformat;
        $mcomp    = ($$eachTestFile)->fcomp;
        $mavail   = "Y";


   print "Filling Files Catalog with DEV output files for testDay\n";

  &fillDbTable();
}

##### update FC with test files from beforeDay directories if they are missing


  foreach my $eachbfFile (@bf_set) {
  
         $pvpath = ($$eachbfFile)->oldpath;
         $pvfile = ($$eachbfFile)->oldfile;
         $bfavail = ($$eachbfFile)->oldvail;
         $bfullName = $pvpath . "/" . $pvfile;

      foreach my $eachTestFile (@testOutNFiles) {

## reinitialize variables

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
 $mcomp = "n\/a";
 $mformat = "n\/a";
 $mavail = "n\/a";
 $mstatus = 0;


        $mfName  = ($$eachTestFile)->filename;        
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
        $mformat  = ($$eachTestFile)->fformat;
        $mcomp    = ($$eachTestFile)->fcomp;
        $mavail   = "Y";

            $thfullName = $mpath ."/" . $mfName;

          if (($bfullName eq $thfullName) and  ($bfavail eq "Y")) {
             $newAvail = "N";
  print "Changing availability for test files", "\n";
  print "file to be updated:",$bfullName, " % ", $newAvail, "\n"; 
        &updateDbTable();
  print "Filling Files Catalog with DEV output files for beforDay\n";
        &fillDbTable();
        }else{
        next;
       }
      }
     }


 &StDbTJobsDisconnect();

 exit;

###########
sub fillDbTable {

    $sql="insert into $FilesCatalogT set ";
    $sql.="LibLevel='$mLibL',";
    $sql.="LibTag='$mLibT',";
    $sql.="platform='$mplform',";
    $sql.="eventType='$mevtType',";
    $sql.="fileName='$mfName',";
    $sql.="path='$mpath',";
    $sql.="geometry='$mgeom',";
    $sql.="eventGen='$mevtGen',";
    $sql.="createTime='$mcTime',";
    $sql.="NoEventReq='$mNevtR',";
    $sql.="NoEventDone='$mNevtD',";
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
sub updateDbTable {

    $sql="update $FilesCatalogT set ";
    $sql.="avail='$newAvail',";
    $sql.="status= 1";
    $sql.=" WHERE path = '$pvpath' AND fileName = '$pvfile'";   
    print "$sql\n" if $debugOn;
    $rv = $dbh->do($sql) || die $dbh->errstr;

  }

#####======================================================================
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
my $size;
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

#####========================================================================
 sub logInfo {

 my ($fl_log,$useless) = @_;

 my $line;
my $no_event = 0;

open (LOGFILE, $fl_log ) or die "cannot open $fl_log: $!\n";

 my @logfile = <LOGFILE>;

  foreach $line (@logfile) {
     chop $line ;

# get library version
    if ( $line =~ /={3} You are in (\w+)/ ) {
      $libV = $1;
    }

# get  number of events
    if ( $line =~ /QAInfo: Done with Event/ ) {
      $no_event++;
   } 
   }
    $EvDone = $no_event;
 close (LOGFILE);
}
