#! /opt/star/bin/perl -w
#
# 
#
#  L.Didenko
#
# dbfillTJobs.pl
#
# Create File Catalog for nightly test files
#
##############################################################################

use Mysql;
use File::Find;
use Net::FTP;
use Class::Struct;
use File::Basename;

require "/afs/rhic/star/packages/DEV00/mgr/dbTJobsSetup.pl";

#require "dbTJobsSetup.pl";

my $TOP_DIRD = "/star/rcf/test/dev/";
my $TOP_DIRN = "/star/rcf/test/new/";
my $TOP_DIRIN = "/star/rcf/simu/cocktail/hadronic/default/";
my @dir_year = ("year_1h", "year_2a");
my @node_dir = ("tfs_redhat61", "tfs_Solaris_CC5"), 
my @hc_dir = ("hc_lowdensity", "hc_standard", "hc_highdensity", "peripheral");
my @IN_DIR = (
                "/star/rcf/simu/cocktail/hadronic/default/lowdensity/year_1h/hadronic_on/Gstardata/rcf0078",
                "/star/rcf/simu/cocktail/hadronic/default/standard/year_1h/hadronic_on/Gstardata/rcf0076",
                "/star/rcf/simu/cocktail/hadronic/default/highdensity/year_1h/hadronic_on/Gstardata/rcf0074",
                "/star/rcf/simu/cocktail/hadronic/default/peripheral/year_1h/hadronic_on/Gstardata",
                "/star/rcf/simu/cocktail/hadronic/default/lowdensity/year_2a/hadronic_on/Gstardata/rcf0079",
                "/star/rcf/simu/cocktail/hadronic/default/standard/year_2a/hadronic_on/Gstardata/rcf0077",
                "/star/rcf/simu/cocktail/hadronic/default/highdensity/year_2a/hadronic_on/Gstardata/rcf0075", 
 );

my @OUT_DIR;

my @Nday = ("Mon","Tue","Wed","Thu","Fri");


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

##### setup output directoies for NEW

for ($i = 0; $i < 2; $i++) {
  for ($j = 0; $j < 2; $j++) {
      for ($ll = 0; $ll < scalar(@hc_dir); $ll++) {
   $OUT_DIR[$ii] = $TOP_DIRN . $node_dir[$i] . "/" . $dir_year[$j] . "/" . $hc_dir[$ll];
#   print "Output Dir for NEW :", $OUT_DIR[$ii], "\n";
       $ii++;
   }
  }
}

##### setup output directories for DEV

for ($i = 0; $i < 2; $i++) {
    for ($k = 0; $k < scalar(@Nday); $k++) {
     for ($j = 0; $j < 2; $j++) {
      for ($ll = 0; $ll < scalar(@hc_dir); $ll++) {
   $OUT_DIR[$ii] = $TOP_DIRD . $node_dir[$i] . "/" . $Nday[$k] . "/". $dir_year[$j] . "/" . $hc_dir[$ll];
#    print "Output Dir for DEV :", $OUT_DIR[$ii], "\n";
        $ii++;
      }
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

#####  read input files
 
foreach $eachInDir (@IN_DIR) {
        if (-d $eachInDir) {
   opendir(DIR, $eachInDir) or die "can't open $eachInDir\n";

   while( defined($flname = readdir(DIR)) ) {
     next if $flname =~ /^\.\.?$/;
     if ($flname =~ /.fz/)   {
     $fullname = $eachInDir."/".$flname;
     @dirF = split(/\//, $eachInDir);
      $geom = $dirF[8];
      $EvType = $dirF[7];  
      $EvGen = $dirF[5] . "_" . $dirF[4]; 
     $form = "fz";
     $comp = "n\/a";    

     my $bsname = basename("$flname",".fz");
      if($flname =~ /2gamma/) {
      @prt = split("_",$bsname);
       $evR = $prt[2];
       $EvReq = $EvReq = substr($evR,0,-4);
    }else {
     @prt = split(/\./,$bsname);
     $evR = $prt[1];
     $EvReq = substr($evR,0,-5);
  };
#     print "Split dir on parts: ", $fullname, " % ", $geom, " % ", $EvType," % ", $EvGen, " % ", $EvReq, "\n";
     $EvDone = $EvReq;
   
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
    ($$fObjAdr)->fpath($eachInDir);
    ($$fObjAdr)->evtType($EvType); 
    ($$fObjAdr)->fgeom($geom); 
    ($$fObjAdr)->evtGen($EvGen);
    ($$fObjAdr)->evtReq($EvReq);  
    ($$fObjAdr)->evtDone($EvDone); 
    ($$fObjAdr)->fformat($form); 
    ($$fObjAdr)->fsize($size);
    ($$fObjAdr)->ftime($timeS);
    $testInFiles[$nInFiles] = $fObjAdr;
   $nInFiles++;
   }
  }
closedir DIR;
      }
}
print "Total input files: $nInFiles\n";


######## read output files for NEW and DEV test

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
     if ($libL =~ /new/) {
      $geom = $dirF[6];
      $EvTp = $dirF[7];  
    }
     elsif($libL =~ /dev/) {
      $geom = $dirF[7];
      $EvTp = $dirF[8];
   }   
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
      &logInfo("$lgFile", 1);
    }
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
   }else {
    next;
  }
  }
closedir DIR;
      }
}
print "Total output files for NEW and DEV test: $nOutNFiles\n";



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

#my $eachTestFile;

#######
 
 &StDbTJobsConnect();

# need to clear the FilesCatalog table first here
 $sql="delete from $FilesCatalogT";
 $cursor =$dbh->prepare($sql)
    || die "Cannot prepare statement: $DBI::errstr\n";
 $cursor->execute;
  
   foreach my $eachTestFile (@testInFiles) {

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
        $mpath    = ($$eachTestFile)->fpath;
        $mgeom    = ($$eachTestFile)->fgeom;
        $msize    = ($$eachTestFile)->fsize;
        $mevtGen  = ($$eachTestFile)->evtGen;
        $mcTime   = ($$eachTestFile)->ftime;
        $mNevtR   = ($$eachTestFile)->evtReq;
        $mNevtD   = ($$eachTestFile)->evtDone;
        $mformat  = ($$eachTestFile)->fformat;
        $mavail   = "Y";


   print "Filling Files Catalog with Input files\n";

  &fillDbTable();
}

####### fill in Files Catalod with Output files from NEW

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


   print "Filling Files Catalog with NEW output files\n";

  &fillDbTable();
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

###======================================================================

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
