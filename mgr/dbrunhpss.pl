#! /opt/star/bin/perl
#
# $Id: dbrunhpss.pl,v 1.1 1999/07/23 15:08:58 wenaus Exp $
#
# $Log: dbrunhpss.pl,v $
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
# Present run log submission form and content display
#
# Usage: dbrunhpss.pl
#

use Net::FTP;
use Class::Struct;

require "dbsetup.pl";

struct FileAttri => {
    filename  => '$',
    nrun      => '$',
    ctime     => '$',
    size      => '$',
};

my $debugOn=0;

## connect to the DB
&StDbConnect();

## Find all the Daq dirs, files in HPSS
my $ftpDaqHome = "/home/starsink/raw/daq";
my $ftpDaq = Net::FTP->new("rmds02.rhic.bnl.gov", Port => 2121, Timeout=>10)
    or die "rmds01 connect failed";
$ftpDaq->login("starsink","MockData") or die "Login failed";
my $nDaqFiles = 0;
my @theDaqDirs;
my @checkedDaqDirs;
my @theDaqFiles;
my @theDaqFileSpecs;

$nHpssDirs = 1;
$theDaqDirs[0] = $ftpDaqHome;
$checkedDaqDirs[0] = 0;
$nHpssFiles = 0;
print "Finding HPSS Daq files\n";
&walkHpss( $ftpDaq, $ftpDaqHome, \@theDaqDirs,\@checkedDaqDirs,
           \@theDaqFiles, \@theDaqFileSpecs );
print "Total Daq files in HPSS: ".@theDaqFiles."\n";
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
        if ( $fname eq "name") {
            $theDbFiles[$nDbfiles] = $fvalue;
            printf "$fname ".$theDbFiles[$nDbfiles]."\n" if $debugOn; 
            $nDbfiles++;
        }
    }
}
print "Total files in RunFile table: ".@theDbFiles."\n";

## @theDaqFiles---HPSS files list  @theDbFiles---DB files list 
 HPSSLOOP: foreach $daqEach (@theDaqFiles) {
     foreach $dbEach (@theDbFiles) {
         if( ($$daqEach)->filename eq $dbEach ) {
             next HPSSLOOP;
         }
     }
     $daqEachName = ($$daqEach)->filename;
     $daqEachNrun = ($$daqEach)->nrun;
     $daqEachCtime = ($$daqEach)->ctime;
     $daqEachSize = ($$daqEach)->size;
     
     $sql="insert into $RunFileT set ";
     $sql.="name='".$daqEachName."',";
     $sql.="nrun=$daqEachNrun,";
     $sql.="stage='daq',";
     $sql.="format='daq',";
     $sql.="ctime=$daqEachCtime,";
     $sql.="size=$daqEachSize,";
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
                    $fname = $dirs->[$ii]."/".$name;		  
                    
                    @nameE = split(/\./, $name);
                    $numRun = $nameE[1];
                    
                    @fnameE = split(/\//,$fname);
                    @timeE = split(/:/,$time);
                    $secDummy = 00;
                    $cTimeS = sprintf ("%4.4d%2.2d%2.2d%2.2d%2.2d%2.2d", $fnameE[5],
                                       $fnameE[6],$day,$timeE[0],$timeE[1],$secDummy);
                    
                    $fObjAdr = \(FileAttri->new());
                    ($$fObjAdr)->filename($fname);
                    ($$fObjAdr)->nrun($numRun);
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
