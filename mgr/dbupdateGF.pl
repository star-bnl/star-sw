#! /opt/star/bin/perl -w
#
# 
#
# 
#
# dbupdateGF.pl
#
# Update File Catalog for GEANT file
#
##############################################################################

use Mysql;
use File::Find;
use Net::FTP;
use Class::Struct;
use File::Basename;

require "/afs/rhic/star/packages/DEV00/mgr/dbCpProdSetup.pl";

#require "dbProdSetup.pl";

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
    gsize       => '$',
    gtimeS      => '$',
		    };

my $debugOn = 0;

  my @dirSet = ("auau100", "auau200", "augas100", "pau200", "pp200", "pp400", "sisi200");

my $topHpssSink  =  "/home/starsink/raw";
my $topHpssReco  =  "/home/starreco/reco";

my @hpssRawFiles;

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

my @hpssGeantDirs;
my @hpssGeantFiles;
my @checkedSimuDirs;

my $nHpssDirs = 1;
my $nHpssFiles = 0;

my @jobIn_set;
my $jobIn_no = 0;


########## Find GEANT files in HPSS

 $hpssGeantDirs[0] = $topHpssSink . "/" . "auau200";
# $hpssGeantDirs[1] = $topHpssSink . "/" . "auau100";
 $checkedSimuDirs[0] = 0; 
# $checkedSimuDirs[1] = 0; 
my $ftpSimu = Net::FTP->new("hpss.rcf.bnl.gov", Port => 2121, Timeout=>100)
  or die "HPSS access failed";
$ftpSimu->login("starsink","MockData") or die "HPSS access failed";

print "\nFinding Geant files in HPSS\n"; 
&walkHpss( $ftpSimu, $topHpssSink, \@checkedSimuDirs,\@hpssGeantDirs, \@hpssGeantFiles );
print "Total files: ".@hpssGeantFiles."\n";
 $ftpSimu->quit();


###### Find GEANT files in Files Catalog
 &StDbProdConnect();

# $sql="delete from $cpFileCatalogT WHERE dataset = 'auau200/hijing/b8_15_jetq_on/jet05/year_1h/hadronic_on' ";
# $cursor =$dbh->prepare($sql)
#    || die "Cannot prepare statement: $DBI::errstr\n";
# $cursor->execute;


 $sql="SELECT dataset, fName, size, createTime FROM $cpFileCatalogT  WHERE fName LIKE '%fzd' OR fName LIKE '%fz' AND hpss = 'Y' ";
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
        ($$fObjAdr)->gsize($fvalue)     if( $fname eq 'size'); 
        ($$fObjAdr)->gtimeS($fvalue)    if( $fname eq 'createTime');
 }

   $jobIn_set[$jobIn_no] = $fObjAdr;
   $jobIn_no++;

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

## 

   foreach $eachGeantFile (@hpssGeantFiles) {

   $mdataSet  = ($$eachGeantFile)->dset;
   $mfName = ($$eachGeantFile)->filename;
   $mcTime  = ($$eachGeantFile)->timeS;
   $msize = ($$eachGeantFile)->size;
   $flagHash{$mfName} = ($$eachGeantFile)->iflag;
#   $mcTime = substr ($mgTime,-8) ;
#   print "Time = ",  $mcTime, "\n";
    foreach my $gtfile (@jobIn_set){
       $dbset = ($$gtfile)->gset;
       $dbfname = ($$gtfile)->gname;
       $dbfsize = ($$gtfile)->gsize;
       $dbgtime = ($$gtfile)->gtimeS;
       @parts = split (" ",$dbgtime);
       $gtime =  $parts[0];
       $gtime =~ s/-//g;
       $dbctime = $gtime;

        if ( ($mfName eq $dbfname) and ($mdataSet eq $dbset)) { 
          if ( ($msize eq $dbfsize) and ($mcTime eq $dbctime)) {
#         if ( $msize eq $dbfsize) {  
           $flagHash{$mfName} = 0;
         } else {
           $flagHash{$mfName} = 2;  
	 }
        last;
	}else {
         next;
        } 
     }
 }
  
   foreach $eachGeantFile (@hpssGeantFiles) {

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

   $mdataSet  = ($$eachGeantFile)->dset;
   $mfName = ($$eachGeantFile)->filename;
   $mpath  = ($$eachGeantFile)->fpath;
   $mcTime  = ($$eachGeantFile)->timeS;
   $mprotc = ($$eachGeantFile)->faccess;
   $mowner = ($$eachGeantFile)->fowner;
   $msize = ($$eachGeantFile)->size;
   $basename = basename("$mfName",".fzd");
   $basename =~ m/(^[a-z0-9]+)_([0-9]+)_([0-9]+)/;
   $mfileSeq = $2 +0;
   $mNevts = $3;
   $mrun = $1;
   $mrunId = substr($1,3) + 0;  
   $mNevtLo = 1;
   $mNevtHi = $mNevts;
   $mformat = "fzd";
   $mcomp = "fzd";
   $msite = "hpss_rcf";
   $mhpss = "Y";
   $mtype = "MC"; 
   $gflag = $flagHash{$mfName}; 
    next if ($mfName =~ /fzd.hold/);
#  print "Set: ", $mdataSet, "File: ", $mfName, "Flag = ", $gflag, "\n";
if ( $gflag eq 1) {

   print "Files to be inserted :", "\n"; 
   print "Set: ", $mdataSet, "Path: ", $mpath, "File: ", $mfName, "Date:", $mcTime,"\n"; 

## fill Files Catalog with new GEANT files
   print "Filling Files Catalog\n";
   &fillDbTable();
 }

elsif ( $gflag eq 2) {
  
 print "Files to be updated :", "\n";    
 print "Set: ", $mdataSet, "Path: ", $mpath,"File:", $mfName, "Date:", $mcTime, "\n";

## update GEANT files in Files Catalog 
   print "Updating Files Catalog\n";
   &updateDbTable();  

}
}
 &StDbProdDisconnect();

 exit;

###########
sub fillDbTable {

    $sql="insert into $cpFileCatalogT set ";
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
    $sql.="hpss='Y',";
    $sql.="status= 0,";
    $sql.="comment=''";
    print "$sql\n" if $debugOn;
    $rv = $dbh->do($sql) || die $dbh->errstr;


  }

###======================================================================
sub updateDbTable {

    $sql="update $cpFileCatalogT set ";
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


###======================================================================

sub walkHpss {
# Go down the list of directories and for each one that
# has not been checked for subdirectories ($checked[i]=0), check
# it and mark it. Add new directories found to the
# list of directories.
    my ( $ftp, $home, $checked, $dirs, $files ) = @_;
 
 my $myDSet;
 my $ext_file;
 my @fields;
 my $access;
 my $downer;
 my $dsize;
 my $month;
 my $day;
 my $year;
 my $name; 
 my @dirF;
 my $fullDir;
 my $set; 
 my @parts;
 my @tk;
 my $Nlevel;
 my $fflag = 1;
 my $ppath;

  for ($ii=0; $ii<$nHpssDirs; $ii++) {
    print "Dir ".$dirs->[$ii]." check: ".$checked->[$ii]."\n" ;
     if ( ! $checked->[$ii] ) {
    my @dird = $ftp->dir($dirs->[$ii]);
       $checked->[$ii] = 1;

     for ($jj=0; $jj<@dird; $jj++) {
         @fields = split(/\s+/, $dird[$jj]);
         $access = $fields[0]; 
         $downer = $fields[2];
         $dsize  = $fields[4];
         $month  = $fields[5];
         $day    = $fields[6];
         $year   = $fields[7];
         $name   = $fields[8];
         if (&isDirectory($access)) {
         $fullDir = $dirs->[$ii]."/".$name;
         $dirs->[$nHpssDirs] = $fullDir;
  
         print "Dir  ".$fullDir."\n";
          $dpath = $fullDir;
          $dpath =~ s/$home\///;
          @tk = split(/\//,$dpath);
      print "Nlevels ".@tk." Path ".$dpath."\n" if $debugOn;
          $nLevels = @tk;
          if ( $nLevels < 8 ) {
           $checked->[$nHpssDirs] = 0;
         } else {
        $checked->[$nHpssDirs] = 1;
      }
        $nHpssDirs++;
       print "Dir  ".$fullDir."\n" if $debugOn;
       }else{
         $fname = $dirs->[$ii]."/".$name;
          $ppath = $dirs->[$ii];
          @dirF = split(/\//, $dirs->[$ii]);
         if( $ppath =~ /gstardata/) {         
	  if($dirF[9] eq 'gstardata') {
         $set = sprintf("%s\/%s\/%s\/%s\/%s",$dirF[4],$dirF[5],$dirF[6],
			                         $dirF[7],$dirF[8]);

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
#      print "Set: ", $set, "Path: ", $ppath, "\n";  
     }
	 elsif ( $dirF[10] eq 'gstardata' ) {
 
         $set = sprintf("%s\/%s\/%s\/%s\/%s\/%s",$dirF[4],$dirF[5],$dirF[6],
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
       }else {
       next;
      }
       }
     } 
   }
 }
}
}
######################
sub isDirectory {
    my ( $char ) = @_;
    if ( $char =~ m/^d/ ) {
        return 1;
    } else {
        return 0;
    }
}
