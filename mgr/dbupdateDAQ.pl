#! /opt/star/bin/perl -w
#
# 
#
# 
#
# dbupdateDAQ.pl
#
# Update File Catalog with new DAQ files
#
##############################################################################

use Mysql;
use File::Find;
use Net::FTP;
use Class::Struct;
use File::Basename;

require "/afs/rhic/star/packages/DEV00/mgr/dbCpProdSetup.pl";


struct FileAttr => {
      filename  => '$',
      fpath     => '$', 
      size      => '$',
      timeS     => '$',
      faccess   => '$',
      fowner    => '$',
      fformat   => '$',
      iflag     => '$',    
};

struct JFileAttr => {
    gpath       => '$',
    gname       => '$', 
    gsize       => '$',
		    };

my $debugOn = 0;

  my $dirSet = "daq";

my $topHpssSink  =  "/home/starsink/raw";

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

my %evtTHash = (
               "physics"  => 3,
               "pedestal" => 1,
               "cluster2" => 2,
               "notdefined" => 100 
             );

my %flagHash = ();


########## Find Geant input files in HPSS

my @hpssRawDirs;
my @hpssRawFiles;
my @checkedRawDirs;

my $nRawDirs = 1;
my $nRawFiles = 0;

my @jobIn_set;
my $jobIn_no = 0;


########## Find GEANT files in HPSS

 $hpssRawDirs[0] = $topHpssSink . "/" . "daq";
 $checkedRawDirs[0] = 0; 

my $ftpRaw = Net::FTP->new("hpss.rcf.bnl.gov", Port => 2121, Timeout=>100)
  or die "HPSS access failed";
$ftpRaw->login("starsink","MockData") or die "HPSS access failed";

print "\nFinding DAQ files in HPSS\n"; 
&walkDHpss( $ftpRaw, $topHpssSink, \@checkedRawDirs,\@hpssRawDirs, \@hpssRawFiles );
print "Total files: ".@hpssRawFiles."\n";
 $ftpRaw->quit();


###### Find DAQ files in Files Catalog
 &StDbProdConnect();

 $sql="SELECT path, fName, size, createTime FROM $FileCatalogT  WHERE fName LIKE '%daq' AND hpss = 'Y' ";
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
        ($$fObjAdr)->gpath($fvalue)      if( $fname eq 'path');
        ($$fObjAdr)->gname($fvalue)     if( $fname eq 'fName'); 
        ($$fObjAdr)->gsize($fvalue)     if( $fname eq 'size'); 
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
my $dbpath;
my $dbfname;
my $dbfsize;
my $dbctime;
my @parts;
my $gtime;
my $dbgtime;
my  @flsplit;

######################################################################## 

   foreach my $eachRawFile (@hpssRawFiles) {

   $mpath  = ($$eachRawFile)->fpath;
   $mfName = ($$eachRawFile)->filename;
   $msize = ($$eachRawFile)->size;
   $flagHash{$mfName} = ($$eachRawFile)->iflag;

    foreach my $gtfile (@jobIn_set){
       $dbpath = ($$gtfile)->gpath;
       $dbfname = ($$gtfile)->gname;
       $dbfsize = ($$gtfile)->gsize;

        if ( ($mfName eq $dbfname) and ($mpath eq $dbpath)) { 
           $flagHash{$mfName} = 0;
        last;
	}else {
         next;
        } 
     }
 }
  
   foreach my $eachRawFile (@hpssRawFiles) {

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
  $mcTime = 00-00-00;
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

   $mfName = ($$eachRawFile)->filename;
   $mpath  = ($$eachRawFile)->fpath;
   $mcTime  = ($$eachRawFile)->timeS;
   $mprotc = ($$eachRawFile)->faccess;
   $mowner = ($$eachRawFile)->fowner;
   $msize = ($$eachRawFile)->size;
   $basename = basename("$mfName",".daq");
    if($basename =~ /^st_/ ) {
   @flsplit = split ("_",$basename);  
   $mfileSeq = $flsplit[4];
   $mrunId =  $flsplit[2];
   $mName = $flsplit[1]; 
}
    else {
     $mfileSeq = 0;
     $mrunId = 0;
     $mName = "notdefined";  
   }

   $mevtType = $evtTHash{$mName};
   $mformat = "daq";
   $mcomp = "daq";
   $msite = "hpss_rcf";
   $mhpss = "Y";
   $mtype = "online"; 
   $gflag = $flagHash{$mfName}; 

if ( $gflag eq 1) {

   print "Files to be inserted :", "\n"; 
   print "Path: ", $mpath, " % ", "File: ", $mfName, "Date:", $mcTime,"\n";
#   print "RunID & FileSeq : ", $mrunId, " % ", $mfileSeq," % ", "EvtType : ", $mevtType," % ", "Size :", $msize,  "\n"; 

##### fill in Files Catalog with new DAQ files
   print "Filling Files Catalog\n";
   &fillDbTable();
 }

}
 &StDbProdDisconnect();

 exit;

###########
sub fillDbTable {

    $sql="insert into $FileCatalogT set ";
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

sub walkDHpss {
# Go down the list of directories and for each one that
# has not been checked for subdirectories ($checked[i]=0), check
# it and mark it. Add new directories found to the
# list of directories.
    my ( $ftp, $home, $checked, $dirs, $files ) = @_;
 
# my $myDSet;
# my $ext_file;
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
# my $set; 
 my @parts;
 my @tk;
 my $Nlevel;
 my $fflag = 1;
 my $ppath;

  for ($ii=0; $ii<$nRawDirs; $ii++) {
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
          next if ( $name =~ /^Star.Test/);
          next if ( $name =~ /^st_config/);
          next if ( $name =~ /^rcftst/); 
          next if ( $name =~ /^filea/);
          next if ( $name =~ /^fileb/); 
         if (&isDirectory($access)) {
         $fullDir = $dirs->[$ii]."/".$name;
         next if ( $fullDir =~ /test/);
         $dirs->[$nRawDirs] = $fullDir;
         
         print "Dir  ".$fullDir."\n";
          $dpath = $fullDir;
           $dpath =~ s/$home\///;
          @tk = split(/\//,$dpath);
      print "Nlevels ".@tk." Path ".$dpath."\n" if $debugOn;
          $nLevels = @tk;
          if ( $nLevels < 6 ) {
           $checked->[$nRawDirs] = 0;
         } else {
        $checked->[$nRawDirs] = 1;
      }
        $nRawDirs++;
       print "Dir  ".$fullDir."\n" if $debugOn;
       }else{
         $fname = $dirs->[$ii]."/".$name;
          $ppath = $dirs->[$ii];
#          @dirF = split(/\//, $dirs->[$ii]);

#         $set = sprintf("%s\/%s\/%s\/%s",$dirF[4],$dirF[5],$dirF[6]);

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
      ($$fObjAdr)->size($dsize);
      ($$fObjAdr)->timeS($timeS);
      ($$fObjAdr)->faccess($access);
      ($$fObjAdr)->fowner($downer);
      ($$fObjAdr)->iflag($fflag); 
      $files->[$nRawFiles] = $fObjAdr;
      $nRawFiles++;
      print "File ".$name."\n" if $debugOn;
#     print "FileName: ",$name , "Path: ", $ppath, "\n";  

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
