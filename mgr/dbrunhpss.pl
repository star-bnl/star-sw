#! /opt/star/bin/perl
#
# $Id: dbrunhpss.pl,v 1.4 2000/01/26 15:58:25 wenaus Exp $
#
# $Log: dbrunhpss.pl,v $
# Revision 1.4  2000/01/26 15:58:25  wenaus
# rhic.bnl.gov -> rcf.bnl.gov
#
# Revision 1.3  1999/10/30 15:09:12  wenaus
# Eliminate password
#
# Revision 1.2  1999/08/08 18:56:33  wenaus
# Include handling of new name format - WD
#
# Revision 1.1  1999/07/23 15:08:58  wenaus
# Wensheng's script to update DAQ HPSS files in DB
#
#
######################################################################
#
# dbrunhpss.pl
#
# Wensheng Deng 7/99
#
# Update DAQ HPSS file list in database
#
# Usage: dbrunhpss.pl
#

use Net::FTP;
use Class::Struct;

use lib "/star/u2d/wenaus/datadb";
require "dbsetup.pl";

struct FileAttri => {
    filename      => '$',
    nameformat    => '$',
# (1). st_unknown_0002807_dst_0011.daq (2). 990719.3.daq  (3). 990723.2607.01.daq
    ctime         => '$',
    size          => '$',
};

my $debugOn=0;

## connect to the DB
&StDbConnect();

## Find all the Daq dirs, files in HPSS
my $hpssHost = 'rmds01.rcf.bnl.gov';
my $netrc = $ENV{HOME}.'/.netrc';
open(NETRC,"<$netrc");
my $pass = '';
while (<NETRC>) {
    if ( m/.*$hpssHost\s.*password\s([\S]*)\s/ ) {$pass = $1}
}
if ( $pass eq '' ) {
    print "Password not found in .netrc\n";
    exit;
}
my $ftpDaqHome = "/home/starsink/raw/daq";
my $ftpDaq = Net::FTP->new($hpssHost, Port => 2121, Timeout=>10)
    or die "$hpssHost connect failed";
$ftpDaq->login("starsink",$pass) or die "Login failed";
my $nDaqFiles = 0;
my @theDaqDirs;
my @checkedDaqDirs;
my @theDaqFiles;
my @theDaqFileSpecs;

$nHpssDirs = 1;
$theDaqDirs[0] = $ftpDaqHome;
$checkedDaqDirs[0] = 0;
$nHpssFiles = 0;
&walkHpss( $ftpDaq, $ftpDaqHome, \@theDaqDirs,\@checkedDaqDirs,
           \@theDaqFiles, \@theDaqFileSpecs );
print "Total Daq files in HPSS: ".@theDaqFiles."\n" if $debugOn;
$ftpDaq->quit();

## Find the whole RunFile table, files in DB
my @theDbFiles;
$nDbfiles = 0;

$sql="select name from $RunFileT";
$cursor =$dbh->prepare($sql)
  || die "Cannot prepare statement: $DBI::errstr\n";
$cursor->execute;

while(@fields = $cursor->fetchrow) {
  my $cols=$cursor->{NUM_OF_FIELDS};
  for($i=0;$i<$cols;$i++) {
    my $fvalue=$fields[$i];
    my $fname=$cursor->{NAME}->[$i];
    $theDbFiles[$nDbfiles] = $fvalue;
    printf "$fname ".$theDbFiles[$nDbfiles]."\n" if $debugOn; 
    $nDbfiles++;
  }
}
print "Total files in RunFile table: ".@theDbFiles."\n" if $debugOn;

## @theDaqFiles---HPSS files list  @theDbFiles---DB files list 
 HPSSLOOP: foreach $daqE (@theDaqFiles) {
     foreach $dbEach (@theDbFiles) {
         if( ($$daqE)->filename eq $dbEach ) {
             next HPSSLOOP;
         }
     }
     $daqEName     = ($$daqE)->filename;
     $daqENameForm = ($$daqE)->nameformat;
     $daqECtime    = ($$daqE)->ctime;
     $daqESize     = ($$daqE)->size;

     $onlyName =  (split(/\//,$daqEName))[7];

     if ( $daqENameForm == 1 ) {
       $onlyName =~ m/st_([a-z0-9]+)_([0-9]+)_([a-z0-9]+)_([0-9]+)\.([\.a-z0-9]+)/;
       $type = $1;
       if ( $type eq 'pedestal' ) {
	 $type = substr($type,0,3);
       }
       $nrun = $2;
       $stage = $3;
       $nseq = $4;
       $format = $5;
     } elsif ( $daqENameForm == 2 ) {
       @onlyNameE = split(/\./, $onlyName);
       $type = 'unknown';
       $nrun = $onlyNameE[1];
       $stage = 'daq';
       $nseq = 0;
       $format = $onlyNameE[2];
     } else {
       @onlyNameE = split(/\./, $onlyName);
       $type = 'unknown';     
       $nrun = $onlyNameE[1];
       $stage = 'daq';
       $nseq = $onlyNameE[2];
       $format = $onlyNameE[3];
     }

     $sql="insert into $RunFileT set ";
     $sql.="name='".$daqEName."',";
     $sql.="type='$type',";
     $sql.="nrun=$nrun,";
     $sql.="stage='$stage',";
     $sql.="nseq=$nseq,";
     $sql.="format='$format',";
     $sql.="ctime=$daqECtime,";
     $sql.="size=$daqESize,";
     $sql.="hpss='Y'";
     print "$sql\n" if $debugOn;
     $rv = $dbh->do($sql) || die $dbh->errstr;
 }

&StDbDisconnect();
print "DB disconnected<br>\n" if $debugOn;

exit;


######################
sub walkHpss {
    my ( $ftp, $home, $dirs, $checked, $files, $filespecs ) = @_;
    
    for ($ii=0; $ii<$nHpssDirs; $ii++) {
        print "Dir ".$dirs->[$ii]." check: ".$checked->[$ii]."\n" if $debugOn;
        if ( ! $checked->[$ii] ) {
            my @dir = $ftp->dir($dirs->[$ii]);
            $checked->[$ii] = 1;
            for ($jj=0; $jj<@dir; $jj++) {
                my @fields = split(/\s+/, $dir[$jj]);
                my $access = $fields[0];
                my $size = $fields[4];
                my $day = $fields[6];                
                my $time = $fields[7];                
                my $name = $fields[8];
                if (&isDirectory($access)) {
                    $fullDir = $dirs->[$ii]."/".$name;
                    $dirs->[$nHpssDirs] = $fullDir;
                    $dpath = $fullDir;
                    $dpath =~ s/$home\///;
                    
                    $checked->[$nHpssDirs] = 0;
                    $nHpssDirs++;
                    print "Dir  ".$fullDir."\n" if $debugOn;
                } else {
		    $filter1 = ( $name =~ /^st_[a-z0-9]+_[0-9]+_[a-z0-9]+_[0-9]+[\.a-z0-9]+/ );
		    $filter2 = ( $name =~ /^[0-9]{6}\.[0-9]+\.[a-z]+$/ );
		    $filter3 = ( $name =~ /^[0-9]{6}\.[0-9]+\.[0-9]+\.[a-z]+$/ );
                    next if ( !($filter1 || $filter2 || $filter3) );

		    $nameformat = 1 if ( $filter1 );
		    $nameformat = 2 if ( $filter2 );
		    $nameformat = 3 if ( $filter3 );
		  
                    $fname = $dirs->[$ii]."/".$name;
		                      
                    @nameE = split(/\./, $name);
                    
                    @fnameE = split(/\//,$fname);
                    @timeE = split(/:/,$time);
                    $secDummy = 00;
                    $cTimeS = sprintf ("%4.4d%2.2d%2.2d%2.2d%2.2d%2.2d", $fnameE[5],
                                       $fnameE[6],$day,$timeE[0],$timeE[1],$secDummy);
                    
                    $fObjAdr = \(FileAttri->new());
                    ($$fObjAdr)->filename($fname);
                    ($$fObjAdr)->nameformat($nameformat);
                    ($$fObjAdr)->ctime($cTimeS);
                    ($$fObjAdr)->size($size);
                
                    $files->[$nHpssFiles] = $fObjAdr;
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
