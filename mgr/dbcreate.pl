#!/opt/star/bin/perl
#
# $Id: dbcreate.pl,v 1.4 1999/09/21 12:24:00 wenaus Exp $
#
######################################################################
#
# create.pl
#
# T. Wenaus 3/99
#
# Manages (re)creation of production/file DB and populating it
# with disk and HPSS resident data files
#
# Usage:    create.pl <pwd>
#
# $Log: dbcreate.pl,v $
# Revision 1.4  1999/09/21 12:24:00  wenaus
# Update to run on Solaris, and run quieter
#
# Revision 1.3  1999/07/25 16:23:09  wenaus
# Modify DB access to allow remote use
#
# Revision 1.2  1999/07/07 13:22:10  wenaus
# incorporate run log
#
# Revision 1.1  1999/06/25 15:16:59  wenaus
# Add scripts for managing prod DB and SW guide
#
package StDbCreate;

#use strict;
use File::Find;
use File::Basename;
use Net::FTP;

use lib "/star/u2d/wenaus/datadb";
require "dbsetup.pl";

my $debugOn=0;

for ($ii=0; $ii<@ARGV; $ii++) {
    if ( $ARGV[$ii] eq "droprun" ) {
        print "Run tables will be dropped\n";
        $dropRunTables = 1;
    }
}

my @dataDisks=(
            "/disk00000/star",
            "/disk00001/star",
            "/disk0/star/starreco",
            "/disk1/star"
            );
# Ignore some directories
my %ignoreDirs = (
               "test" => 0,
               "kathy" => 0,
               "GC" => 0,
               "MDC1" => 0
               );
# Extensions corresponding to known formats
my %okExtension = (
                ".root" => 0,
                ".xdf" => 0,
                ".fz" => 0,
                ".fzd" => 0,
                ".rz" => 0
                );
# Ignore some filenames
my %ignoreFile = (
               "N" => 0,
               "NN" => 0,
               "dummy" => 0,
               "paw" => 0,
               "logfile" => 0,
               "last" => 0,
               "core" => 0,
               "tmpredo" => 0,
               "tmplist" => 0,
               "filter" => 0,
               "detm" => 0,
               "detmsys" => 0,
               "hpss" => 0,
               );
# Ignore some extensions
my %ignoreExt = (
              ".txt" => 0,
              ".log" => 0,
              ".out" => 0,
              ".tmp" => 0,
              ".del" => 0,
              ".def" => 0,
              ".g" => 0,
              ".F" => 0,
              ".C" => 0,
              ".idl" => 0,
              ".job" => 0,
              ".kumac" => 0,
              ".kumacold" => 0,
              ".99" => 0,
              ".ps" => 0,
              ".metafile" => 0,
              ".ftp_job" => 0,
              ".ftp_list" => 0,
              ".com" => 0,
              ".need_to_fix" => 0,
              ".awk" => 0,
              ".sh" => 0,
              "" => 0
               );

# Initialize some of the key values. If they are represented
# on disk they are picked up dynamically in the discovery
# step below.
%beamTypes = (
              "auau50" => 1
              );

my $recoDir="/star/u2e/starreco/MDC3";
my $recoRequests="$recoDir/requests";
my $recoJobs="$recoRequests/jobfiles";
my $recoSummary="$recoDir/summary";
my $jobLog="/disk1/star/MDC2";

#### Walk HPSS directory trees
my $nHpssDirs = 0;
my $nHpssFiles = 0;

## Find all the Reco dirs, files in HPSS
my $ftpRecoHome = "/home/starreco/reco";
my $ftpReco = Net::FTP->new("rmds02.rhic.bnl.gov", Port => 2121)
    or die "rmds02 connect failed";
$ftpReco->login("starreco") or die "Login failed";
my $nRecoFiles = 0;
my @theRecoDirs;
my @checkedRecoDirs;
my @theRecoFiles;
my @theRecoFileSpecs;

$nHpssDirs = 1;
$theRecoDirs[0] = $ftpRecoHome;
$checkedRecoDirs[0] = 0;
$nHpssFiles = 0;
&walkHpss( $ftpReco, $ftpRecoHome, \@theRecoDirs, \@checkedRecoDirs,
           \@theRecoFiles, \@theRecoFileSpecs );
if ( @theRecoFiles == 0 ) {
  print "dbcreate error: Total reco files in HPSS: ".@theRecoFiles."\n";
}
print "Total reco files in HPSS: ".@theRecoFiles."\n" if $debugOn;

$ftpReco->quit();

## Find all the Simu dirs, files in HPSS
my $ftpSimuHome = "/home/starsink/raw";
my $ftpSimu = Net::FTP->new("rmds02.rhic.bnl.gov", Port => 2121)
    or die "rmds01 connect failed";
$ftpSimu->login("starsink","MockData") or die "Login failed";
my $nSimuFiles = 0;
my @theSimuDirs;
my @checkedSimuDirs;
my @theSimuFiles;
my @theSimuFileSpecs;

$nHpssDirs = 1;
$theSimuDirs[0] = $ftpSimuHome;
$checkedSimuDirs[0] = 0;
$nHpssFiles = 0;
&walkHpss( $ftpSimu, $ftpSimuHome, \@theSimuDirs,\@checkedSimuDirs,
           \@theSimuFiles, \@theSimuFileSpecs );
if ( @theSimuFiles == 0 ) {
  print "dbcreate.pl error: Total simu files in HPSS: ".@theSimuFiles."\n";
}
print "Total simu files in HPSS: ".@theSimuFiles."\n" if $debugOn;

$ftpSimu->quit();

#### Discover datasets in the production data areas
print "Finding data on disk\n" if $debugOn;
foreach $dataDir (@dataDisks) {
    open(DIRS, "gfind $dataDir -mindepth 6 -maxdepth 10 -type d |");
    while (<DIRS>) {
        chomp;
        s/$dataDir\///;
        my $hiddenDir = m/(\/\.)/;
        my $cvsDir = m/(\/CVS\/)/;
        my $excludeDir = $hiddenDir || $cvsDir;
        print "Excluding $_\n" if ($excludeDir && $debugOn);
        my @tk = split(/\//);
        if ((! $excludeDir) && (! exists($ignoreDirs{$tk[0]}))) {
            ## Account for flaky directory levels
            my $nHad = 5;
            if ( ! ($tk[5] =~ m/hadronic/) ) {
                for ($kk=6; $kk<@tk; $kk++) {
                    if ( $tk[$kk] =~ m/hadronic/ ) { $nHad = $kk; last; }
                }
                if ( $tk[$nHad] =~ m/year/ ) { $nHad = $nHad +1; }
            }
            $beamTypes{$tk[0]}++;
            $genTypes{$tk[1]}++;
            $parTypes{$tk[2]}++;
            $bTypes{$tk[$nHad-2]}++;
            $geomt = $tk[$nHad-1];
            $geomt =~ s/_//g;
            if ($geomt eq "compplete") { $geomt = "complete"; } # fix typo
            $geomTypes{$geomt}++;
            if ( $tk[$nHad-1] eq 'rho' ) { print $_." $nHad\n"; }
            if (@tk > $nHad) { $hadTypes{$tk[$nHad]}++; }
            if (@tk > $nHad+1) { $bfcTypes{substr($tk[$nHad+1],0,3)}++; }
        }
    }
}

if ( $debugOn ) {
    foreach $bm (sort keys %beamTypes) { print "Beam $bm\n"; }
    foreach $gn (sort keys %genTypes) { print "Gen $gn\n"; }
    foreach $pr (sort keys %parTypes) { print "Param $pr\n"; }
    foreach $bt (sort keys %bTypes) { print "bImp $bt\n"; }
    foreach $gm (sort keys %geomTypes) { print "Geom $gm\n"; }
    foreach $hd (sort keys %hadTypes) { print "Had $hd\n"; }
    foreach $bf (sort keys %bfcTypes) { print "Bfc $bf\n"; }
}

# Build enums
$beamEnum = "'unknown'";
foreach $bm (sort keys %beamTypes) { $beamEnum .= ",'$bm'"; }
$genEnum = "'unknown'";
foreach $gn (sort keys %genTypes) { $genEnum .= ",'$gn'"; }
$paramEnum = "'unknown'";
foreach $vl (sort keys %parTypes) { $paramEnum .= ",'$vl'"; }
$bimpEnum = "'unknown'";
foreach $vl (sort keys %bTypes) { $bimpEnum .= ",'$vl'"; }
$geomEnum = "'unknown'";
foreach $vl (sort keys %geomTypes) { $geomEnum .= ",'$vl'"; }
$hadEnum = "'unknown'";
foreach $vl (sort keys %hadTypes) { $hadEnum .= ",'$vl'"; }
$bfcEnum = "'unknown'";
foreach $bf (sort keys %bfcTypes) { $bfcEnum .= ",'$bf'"; }


print "Create and fill tables\n" if $debugOn;
# drop any existing tables
&StDbDeleteTables();

# create tables
&StDbCreateTables();

# connect to the DB
&StDbConnect();

### populate tables

# disk resident
$nstored=0;
$oldDir="";
foreach $dataArea (@dataDisks) {
    open(FILES, "gfind $dataArea -mindepth 6 -maxdepth 10 -type f |");
    while (<FILES>) {
        chomp;
        &addFile($_,"");
    }
}

# HPSS reco
$nstored=0;
$oldDir="";
$dataArea = $ftpRecoHome;
for ( $ih=0; $ih<@theRecoFiles; $ih++) {
    &addFile($theRecoFiles[$ih],$theRecoFileSpecs[$ih]);
}

# HPSS simu
$nstored=0;
$oldDir="";
$dataArea = $ftpSimuHome;
for ( $ih=0; $ih<@theSimuFiles; $ih++) {
    &addFile($theSimuFiles[$ih],$theSimuFileSpecs[$ih]);
}

# finished

&StDbDisconnect();
exit;

#=======================================================================

######################
sub addFile {
    my ( $fullname, $filespecs ) = @_;
    $volume = "";
    $location = "";
    if ( $fullname =~ m/\/([a-z0-9]+)\/([a-z0-9]+)\/([a-z0-9]+)/ ) {
        $volume="/$1/$2/$3";
        $location=$1;
        if ( $location eq "home" ) {
            $location = "hpss-rcf";
        } else {
            $location = "disk-rcf";
        }
    }
    $truncname = $fullname;
    $truncname =~ s/$dataArea\///;
    $isHidden = $fullname =~ m/(\/\.)/;
    $isCVS = $fullname =~ m/(\/CVS\/)/;
    $excludeFile = $isHidden || $isCVS;
    print "Excluding $fullname\n" if ($excludeFile && $debugOn);
    @tk = split(/\//,$truncname);
    if ( (@tk >= 6) && (! $excludeFile) && (! exists($ignoreDirs{$tk[0]})) ) {
        if ($debugOn) { print "$fullname\n"; }
        ($filename, $dirname, $extension) = fileparse($fullname,'\.[a-zA-z]*');
        $dirname =~ s/\/$//;
        if (exists($ignoreFile{$filename}) ) {
            $ignoreFile{$filename}++;
        } elsif (exists($ignoreExt{$extension})) {
            $ignoreExt{$extension}++;
        } else {
            my $inHpss = "'U'";
            ## If not an HPSS file, get specs from filesystem
            if ( ! ($fullname =~ m/^\/home/) ) {
                $inHpss = "'N'";
                $addfilesize = 0;
                ($fmode, $uid, $gid, $filesize, 
                 $readTime, $writeTime, $cTime) =
                     (stat($fullname))[2,4,5,7,8,9,10];
                $grnam = (getgrgid($gid))[0];
                $unam = (getpwuid($uid))[0];
                ($sec,$min,$hr,$dy,$mo,$yr,$wkd,$ydy,$isdst) =
                    localtime($readTime);
                $readTimeS = sprintf ("%4.4d%2.2d%2.2d%2.2d%2.2d%2.2d",
                                      $yr+1900,$mo+1,$dy,$hr,$min,$sec);
                ($sec,$min,$hr,$dy,$mo,$yr,$wkd,$ydy,$isdst) =
                    localtime($writeTime);
                $writeTimeS = sprintf ("%4.4d%2.2d%2.2d%2.2d%2.2d%2.2d",
                                       $yr+1900,$mo+1,$dy,$hr,$min,$sec);
                ($sec,$min,$hr,$dy,$mo,$yr,$wkd,$ydy,$isdst) =
                    localtime($cTime);
                $cTimeS = sprintf ("%4.4d%2.2d%2.2d%2.2d%2.2d%2.2d",
                                   $yr+1900,$mo+1,$dy,$hr,$min,$sec);
            } else {
                $inHpss = "'Y'";
                my @fields = split(/\s+/, $filespecs);
#                $fmode = $fields[0];
                $fmode = 0;
                $filesize = $fields[4];
                $addfilesize = $filesize;
                %cmo = (
                        'Jan' => 1,
                        'Feb' => 2,
                        'Mar' => 3,
                        'Apr' => 4,
                        'May' => 5,
                        'Jun' => 6,
                        'Jul' => 7,
                        'Aug' => 8,
                        'Sep' => 9,
                        'Oct' => 10,
                        'Nov' => 11,
                        'Dec' =>12
                        );
                $mo = $cmo{$fields[5]};
                $sec = 0;
                $min = 0;
                $hr = 0;
                $dy = $fields[6];
                $yr = $fields[7];
                if ( $yr =~ m/:/ ) {
                    ( $hr, $min ) = split(/:/,$yr);
                    ($curdy,$curmo,$yr) = (localtime())[3,4,5];
                    # Directory listing shows hour rather than year
                    # if date > current date
                    $curmo = $curmo +1;
                    if ($curmo <= $mo) {
                        if ($curdy <= $dy) { $yr = $yr -1; }
                    }
                } else {
                    $yr = $yr - 1900;
                }
                $timeS = sprintf ("%4.4d%2.2d%2.2d%2.2d%2.2d%2.2d",
                                      $yr+1900,$mo,$dy,$hr,$min,$sec);
                $readTimeS = $timeS;
                $writeTimeS = $timeS;
                $cTimeS = $timeS;
                $grnam = $fields[3];
                $unam = $fields[2];
            }
            if (exists($okExtension{$extension})) {
                $format = $extension;
                $format =~ s/\.//;
            } else {
                $format='unknown';
            }
            
            ## Account for flaky directory levels
            $nHad = 5;
            if ( ! ($tk[5] =~ m/hadronic/) ) {
                for ($kk=6; $kk<@tk; $kk++) {
                    if ( $tk[$kk] =~ m/hadronic/ ) { $nHad = $kk; last; }
                }
                if ( $tk[$nHad] =~ m/year/ ) { $nHad = $nHad +1; }
            }
            $beamType=$tk[0];
            $genType=$tk[1];
            $parType=$tk[2];
            $bType=$tk[$nHad-2];

            $geomt = $tk[$nHad-1];
            $geomt =~ s/_//g;
            if ($geomt eq "compplete") { $geomt = "complete"; } # fix typo
            $geomType=$geomt;

            $hadType=$tk[$nHad];
            if ( @tk > $nHad+1 ) {
                $bfcType=substr($tk[$nHad+1],0,3);
                $prodtag=substr($tk[$nHad+1],3,99);
            } else {
                $bfcType="";
                $prodtag="";
            }
            
            $dataSet=$filename;
            $subset=$filename;
            $nevFirst=$filename;
            if ( $nevFirst =~ m/(^[a-z0-9]+)_([0-9]+)_([0-9]+)/ ) {
                $nevFirst = $2;
                $nevLast = $3;
                $nevents = $nevLast - $nevFirst +1;
            } else {
                $nevents = 0;
            }
            if ( $dataSet =~ m/^([a-z]{3,}[0-9]{3,})/ ) {
                $dataSet = $1;
                $subset =~ m/^([a-z0-9_]+evts)/;
                $subset = $1;
                $site = $dataSet;
                $site =~ m/^([a-z]{3,})/;
                $site = $1;
                print "Dataset $dataSet\n" if $debugOn;
            } else {
                $dataSet = "";
                $subset = "";
                $site = "";
            }
            $component=$filename;
            if ( ($component =~ m/\.([a-z0-9_]{3,})$/) 
                 && ($format eq "root") ) {
                $component = $1;
                print "File $filename Component $component\n" if $debugOn;
            } else {
                $component = "";
            }
            if ($format eq "root" && $component eq "") {
                ### distinguish old-format ROOT
                $format = "root-MDC2";
            }
            
            ### add DataFile table to database
            $sql="insert into $DataFileT  set ";
            $sql.="name='$fullname',";
            $sql.="location='$location',";
            $sql.="volume='$volume',";
            $sql.="ext='$extension',";
            $sql.="dataset='$dataSet',";
            $sql.="subset='$subset',";
            $sql.="site='$site',";
            $sql.="events='$nevents',";
            $sql.="component='$component',";
            $sql.="cTime=$cTimeS,";
            $sql.="readTime=$readTimeS,";
            $sql.="format='$format',";
            $sql.="owner='$unam',";
            $sql.="grp='$grnam',";
            $sql.="access=0$fmode,";
            $sql.="hpss=$inHpss,";
            $sql.="size=$filesize,";
            $sql.="beam='$beamType',";
            $sql.="mcgen='$genType',";
            $sql.="param='$parType',";
            $sql.="bimp='$bType',";
            $sql.="geom='$geomType',";
            $sql.="had='$hadType',";
            $sql.="bfc='$bfcType',";
            $sql.="prodtag='$prodtag',";
            $sql.="status=0,";
            $sql.="comment=''";
            $nstored++;
            print "$sql\n" if $debugOn;
            $rv = $dbh->do($sql);
            
            ### add DataSet entry
            if ( $dataSet ne "" ) {
                $dataSets{$dataSet}++;
                if ($dataSets{$dataSet}==1) {
                    $sql="insert into $DataSetT  set ";
                    $sql.="name='$dataSet',";
                    $sql.="site='$site',";
                    $sql.="owner='$unam',";
                    $sql.="grp='$grnam',";
                    $sql.="cTime=$cTimeS,";
                    $sql.="modTime=$writeTimeS,";
                    $sql.="nFiles=1,";
                    $sql.="size=$addfilesize,";
                    $sql.="beam='$beamType',";
                    $sql.="mcgen='$genType',";
                    $sql.="param='$parType',";
                    $sql.="bimp='$bType',";
                    $sql.="geom='$geomType',";
                    $sql.="had='$hadType',";
                    $sql.="comment=''";
                    $nstored++;
                    print "$sql\n" if $debugOn;
                    $rv = $dbh->do($sql) || die $dbh->errstr;
                } else {
                    $sql="update $DataSetT set ";
                    $sql.="nFiles=nFiles+1,size=size+$addfilesize";
                    $sql.=" where name='$dataSet'";
                    print "$sql\n" if $debugOn;
                    $rv = $dbh->do($sql) || die $dbh->errstr;
                }
            }
            
            ### add Subset entry
            if ( $subset ne "" ) {
                $subsets{$subset}++;
                if ($subsets{$subset}==1) {
                    $sql="insert into $SubsetT  set ";
                    $sql.="name='$subset',";
                    $sql.="dataset='$dataSet',";
                    $sql.="site='$site',";
                    $sql.="owner='$unam',";
                    $sql.="grp='$grnam',";
                    $sql.="cTime=$cTimeS,";
                    $sql.="modTime=$writeTimeS,";
                    $sql.="nFiles=1,";
                    $sql.="size=$addfilesize,";
                    $sql.="beam='$beamType',";
                    $sql.="mcgen='$genType',";
                    $sql.="param='$parType',";
                    $sql.="bimp='$bType',";
                    $sql.="geom='$geomType',";
                    $sql.="had='$hadType',";
                    $sql.="comment=''";
                    $nstored++;
                    print "$sql\n" if $debugOn;
                    $rv = $dbh->do($sql) || die $dbh->errstr;
                } else {
                    $sql="update $SubsetT set ";
                    $sql.="nFiles=nFiles+1,size=size+$addfilesize";
                    $sql.=" where name='$subset'";
                    print "$sql\n" if $debugOn;
                    $rv = $dbh->do($sql) || die $dbh->errstr;
                }
            }

            ### add DataDir entry
            if ($dirname ne $oldDir) {
                $oldDir = $dirname;
                $sql="insert into $DataDirT  set ";
                $sql.="dir='$dirname',";
                $sql.="modTime=$writeTimeS,";
                $sql.="beam='$beamType',";
                $sql.="mcgen='$genType',";
                $sql.="param='$parType',";
                $sql.="bimp='$bType',";
                $sql.="geom='$geomType',";
                $sql.="had='$hadType',";
                $sql.="bfc='$bfcType',";
                $sql.="comment=''";
                $nstored++;
                print "$sql\n" if $debugOn;
                $rv = $dbh->do($sql) || die $dbh->errstr;
            }
            
            if ( $debugOn ) {
              if ($nstored%200 == 0) { print "$nstored\n"; }
            }
        }
    }
}

######################
sub StDbDeleteTables {
    open ( DELTABLES, "| /opt/star/bin/mysql --user=$dbuser --host=$dbhost --password=$dbpass") or die "Open failure: $!";
    print DELTABLES <<END;
    use $dbname;
    drop table if exists $DataFileT;
    drop table if exists $FileLocationT;
    drop table if exists $DataSetT;
    drop table if exists $SubsetT;
    drop table if exists $DataDirT;
END
    if ( $dropRunTables ) {
    print DELTABLES <<END;
#    drop table if exists $RunT;
#    drop table if exists $RunFileT;
#    drop table if exists $EventT;
END
    }
    close DELTABLES;
}

######################
sub StDbCreateTables {
    open ( MAKETABLES, "| /opt/star/bin/mysql --user=$dbuser --host=$dbhost --password=$dbpass ") or die "Open failure: $!";
    $dbCmd =<<END_BLOCK;
# --------- Table building -----------
    use $dbname;
    
    create table $DataFileT
        (
         id mediumint not null auto_increment primary key,
         ctime datetime not null,
         size bigint not null,
         name varchar(255) not null,
         ext varchar(10) not null,
         dataset varchar(20) not null,
         component varchar(20) not null,
         format enum(
                     'unknown',
                     'daq',
                     'xdf',
                     'fz',
                     'fzd',
                     'rz',
                     'ps',
                     'root',
                     'root-MDC2',
                     'objy'
                     ) not null,
         owner varchar(20) not null,
         grp varchar(20) not null,
         # access control (protection)
         access mediumint,
         hpss enum('U','Y','N') not null,
         beam enum( $beamEnum ) not null,
         mcgen enum( $genEnum ) not null,
         param enum( $paramEnum ) not null,
         bimp enum( $bimpEnum ) not null,
         geom enum( $geomEnum ) not null,
         had enum( $hadEnum ) not null,
         bfc enum( $bfcEnum ) not null,
         prodtag varchar(10) not null,
         subset varchar(40) not null,
         events smallint,
         readtime datetime not null,
         inserttime timestamp(10) not null,
         location varchar(10) not null,
         site varchar(10) not null,
         volume varchar(30) not null,
         status smallint,
         comment blob,

         unique(name),
         index(location),
         index(ctime),
         index(format),
         index(hpss),
         index(beam),
         index(mcgen),
         index(bimp),
         index(geom),
         index(had),
         index(bfc),
         index(prodtag),
         index(site),
         index(subset)
         );

#    show columns from $DataFileT;
#    show index from $DataFileT;

    create table $FileLocationT
        (
         id mediumint not null auto_increment primary key,
         fileid mediumint not null,
         path varchar(255) not null,
         site varchar(32) not null,
# is this the primary instance of the file
         prime enum('U', 'Y','N') not null,
         modtime datetime not null,
         owner varchar(20) not null,
         grp varchar(20) not null,
         access mediumint,
         size mediumint not null,
         comment blob,

         index(fileid),
         index(site)
         );

#    show columns from $FileLocationT;
#    show index from $FileLocationT;

    create table $DataSetT
        (
         id mediumint not null auto_increment primary key,
         ctime datetime not null,
         size bigint,
         name varchar(128) not null,
         site varchar(10) not null,
         owner varchar(20) not null,
         grp varchar(20) not null,
         modtime datetime not null,
         nfiles mediumint,
         beam enum( $beamEnum ) not null,
         mcgen enum( $genEnum ) not null,
         param enum( $paramEnum ) not null,
         bimp enum( $bimpEnum ) not null,
         geom enum( $geomEnum ) not null,
         had enum( $hadEnum ) not null,
         comment blob,
         
         unique(name),
         index(name),
         index(site),
         index(ctime),
         index(modtime)
         );

    create table $SubsetT
        (
         id mediumint not null auto_increment primary key,
         ctime datetime not null,
         size bigint,
         name varchar(128) not null,
         site varchar(10) not null,
         owner varchar(20) not null,
         grp varchar(20) not null,
         modtime datetime not null,
         nfiles mediumint,
         beam enum( $beamEnum ) not null,
         mcgen enum( $genEnum ) not null,
         param enum( $paramEnum ) not null,
         bimp enum( $bimpEnum ) not null,
         geom enum( $geomEnum ) not null,
         had enum( $hadEnum ) not null,
         dataset varchar(20) not null,
         comment blob,
         
         unique(name),
         index(name),
         index(site),
         index(ctime),
         index(modtime)
         );

#    show columns from $SubsetT;
#    show index from $SubsetT;

    create table $DataDirT
        (
         id mediumint not null auto_increment primary key,
         dir varchar(128) not null,
         modtime datetime not null,
         beam enum( $beamEnum ) not null,
         mcgen enum( $genEnum ) not null,
         param enum( $paramEnum ) not null,
         bimp enum( $bimpEnum ) not null,
         geom enum( $geomEnum ) not null,
         had enum( $hadEnum ) not null,
         bfc enum( $bfcEnum ) not null,
         comment blob,

         index(beam),
         index(mcgen),
         index(param),
         index(bimp),
         index(geom),
         index(had),
         index(bfc)
         );

#    show columns from $DataDirT;
#    show index from $DataDirT;
END_BLOCK
    print MAKETABLES $dbCmd;

    if ( $dropRunTables ) {
    $dbCmd =<<END_BLOCK;
    use $dbname;
    create table $RunFileT
        (
         id mediumint not null auto_increment primary key,
         ctime datetime not null,
         size bigint not null,
         fileId1 int unsigned not null,
         fileId2 int unsigned not null,
         owner varchar(20) not null,
         grp varchar(20) not null,
         access mediumint,
         readtime datetime not null,
         inserttime timestamp(10) not null,
         runname varchar(80) not null,
         name varchar(255) not null,
         type enum ( $runTypeList ) not null,
         nrun int unsigned not null,
         stage enum ( $procStages ) not null,
         nseq int not null,
         events mediumint unsigned,
         format varchar(30) not null,
         hpss enum('U','Y','N') not null,
         location varchar(10) not null,
         site varchar(10) not null,
         volume varchar(30) not null,
         status smallint,
         comment blob,

         index(type),
         index(nrun),
         index(stage),
         unique(name)
         );
#    show columns from $RunFileT;
#    show index from $RunFileT;

    create table $RunT
        (
         id mediumint not null auto_increment primary key,
         name varchar(80) not null,
         # person responsible for this run or entry
         user varchar(60),
         # log info only, not a run spec entry?
         logonly enum ( 'U','Y','N' ) not null,
         type enum ( $runTypeList ) not null,
         format int unsigned,
         nrun int unsigned not null,
         gainmode enum ( $gainModeList ) not null,
         pedmode enum ( $pedModeList ) not null,
         zerosup enum ( 'U','Y','N' ) not null,
         rawformat enum ( 'U','Y','N' ),
         trig set ( $trigTypeList ) not null,
         trigwdin smallint unsigned,
         beam enum( $beamEnum ) not null,
         # Percent of full field; -ve for reversed polarity
         field smallint,
         thrlo smallint,
         thrhi smallint,
         seqlo smallint,
         seqhi smallint,
         ctime datetime not null,
         starttime datetime not null,
         endtime datetime not null,
         period varchar(20) not null,
         status smallint,
         banks set ( $bankTypeList ),
         sectors set ( $sectorList ),
         events mediumint unsigned,
         ngood mediumint unsigned,
         nfiles mediumint unsigned,
         stage set ( $procStages ) not null,
         title blob,
         comment blob,

         index(type),
         index(logonly),
         index(nrun),
         index(gainmode),
         index(pedmode),
         index(zerosup),
         index(beam),
         index(starttime),
         index(period)
         );

#    show columns from $RunT;
#    show index from $RunT;

    create table $EventT
        (
         id mediumint not null auto_increment primary key,
         type enum ( $eventTypeList ) not null,
#          the trigger mask
         trig set ( $trigTypeList ) not null,
         trigwd smallint unsigned,
         trigwdin smallint unsigned,
         format smallint unsigned,
         nrun mediumint unsigned not null,
         nevent mediumint unsigned not null,
         time datetime not null,
         nwords int unsigned,
         npads int unsigned,
#          number of banks
         nbanks smallint unsigned,
#          the set of banks in this event
         banks set ( $bankTypeList ),
         sectors set ( $sectorList ),
         comment blob,

         index(type),
         index(trig),
         index(nrun),
         index(nevent),
         unique(nrun,nevent)
         );

#    show columns from $EventT;
#    show index from $EventT;

# --------- End of table building ----------
END_BLOCK
    print MAKETABLES $dbCmd;
  }
    close MAKETABLES;
    print "----------------- Tables built -----------------\n" if $debugOn;
}

######################
sub walkHpss {
# Go down the list of directories and for each one that
# has not been checked for subdirectories ($checked[i]=0), check
# it and mark it. Add new directories found to the
# list of directories.
    my ( $ftp, $home, $dirs, $checked, $files, $filespecs ) = @_;

## Loop over the directories, check those not already checked,
##   add directories inside checked directories to the list
    for ($ii=0; $ii<$nHpssDirs; $ii++) {
        print "Dir ".$dirs->[$ii]." check: ".$checked->[$ii]."\n" if $debugOn;
        if ( ! $checked->[$ii] ) {
            my @dir = $ftp->dir($dirs->[$ii]);
            $checked->[$ii] = 1;
            for ($jj=0; $jj<@dir; $jj++) {
                my @fields = split(/\s+/, $dir[$jj]);
                my $access = $fields[0];
                my $size = $fields[4];
                my $name = $fields[8];
                if (&isDirectory($access)) {
                    $fullDir = $dirs->[$ii]."/".$name;
                    $dirs->[$nHpssDirs] = $fullDir;
# Check the depth of the directory. If at the bottom, don't
# try to descend any further (checked=1). Fragile against changes in
# directory scheme.
                    $dpath = $fullDir;
                    $dpath =~ s/$home\///;
                    @tk = split(/\//,$dpath);
                    print "Nlevels ".@tk." Path ".$dpath."\n" if $debugOn;
                    $nLevels = @tk;
                    if ( $nLevels < 7 ) {
                        $checked->[$nHpssDirs] = 0;
                    } else {
#                        $checked->[$nHpssDirs] = 1;
                        $checked->[$nHpssDirs] = 0;
                        ( $beamType, $genType, $parType, $bType,
                          $geomType, $hadType, $bfcType ) = @tk;
                    }
                    $nHpssDirs++;
                    print "Dir  ".$fullDir."\n" if $debugOn;
                } else {
                    $fname = $dirs->[$ii]."/".$name;
                    $files->[$nHpssFiles] = $fname;
                    $filespecs->[$nHpssFiles] = $dir[$jj];
                    $nHpssFiles++;
                    print "File ".$fname."\n" if $debugOn;
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
