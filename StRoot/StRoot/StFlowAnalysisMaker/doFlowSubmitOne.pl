#!/usr/bin/perl -w
# doFlowSubmitOne.pl is a perl script to submit doFlowEvents.C
# It is meant for StFlowDirectCumulantMaker because all centralities are done at once in this Maker
# The $homeDir directory should contain a link to your inDir, and to your outDir
# The $macDir directory should contain doFlowEvents.C
# The $rootDir directory should contain the library dot file
# $inDir should be a link to a directory containing subdirectories of files
# $outDir is a link to where you want your output
# If the 1st pass has completed there is no submission
# rm outDir/*/doFlowEvents.C if it has changed
# by Art Poskanzer

use strict;
use File::Basename;
use File::Copy;
use Getopt::Std;

my $events = 200000;
#my $events = 100;
my $queTime = "25:00:00"; # always bombs after 1 day
#my $queTime = "01:00:00";

# options
# -t :test, no submission
my %opt;
getopts('t',\%opt);

my $usage = "usage: -opts  inDir runNo \n";

# set directories
my $inDir = shift or die $usage;
my $outDir = "./outDir"; # directory for output
my $runNo = shift or die $usage;
-e $inDir or die "$inDir :$!\n";

###### User defined:
my $homeDir = "~posk/directCumulant"; # directory containing the links
my $macDir = "$homeDir/mac"; # directory containing the macro
my $rootDir = "$homeDir"; # directory containing the library
print "homeDir = $homeDir\n";
print "inDir = $inDir\n";
print "outDir = $outDir\n";
print "macDir = $macDir\n";
print "rootDir = $rootDir\n";
my $fileExt = "*MuDst.root";

my $starVer = basename $ENV{STAR};
my $io = 1;
my $resource = '';

# which machine are we at?
if ($ENV{HOST} =~ /pdsf/) {
  if ($inDir eq "in4") {
    $resource = "-l eliza9io=$io"; # in4
  } elsif ($inDir eq "in7" || $inDir eq "in73") {
    $resource = "-l eliza12io=$io"; # in7
  } else {
    $resource = "-l eliza3io=$io"; # eliza3 year7 data
  }
} else {
  die "wrong host $ENV{HOST}\n";
}

print "host = $ENV{HOST}\n";
print "resource = $resource\n";
print "star ver.= $starVer\n";
print "max time = $queTime\n";
print "events = $events\n";

my $command;
my $firstPass;
my $workDir;
my $exec = "qsub -V -m e"; # for mail at the end
#my $exec = "qsub -V"; # for no mail
my $subDirNo;

# subdirectory loop
my @subDirs;
if ( `ls -l $inDir/ | grep ^d` ) { # subdirectories?
  @subDirs = split(/\n/,`ls $inDir`); # yes
} else {
  @subDirs = "."; # no subdirectories
}
print "subdirectories: @subDirs\n";
foreach my $subDir (@subDirs) {
  if ( -d "$inDir/$subDir" && `ls $inDir/$subDir/` ) { # dir exists and contains a file
    $firstPass = "kFALSE";
    #$subDirNo = substr($subDir, index($subDir, ".")+1, 4); # 4 chars after the dot
    $subDirNo = substr($subDir,-3, 3); # last 3 chars

      # make work directory
      $workDir = "$outDir/$inDir-$subDir-$runNo";
      -e $workDir or do {
	print "making $workDir directory\n";
	mkdir $workDir,0755 or die "$!\n";
      };
      # copy doFlowEvents.C
      -e "$workDir/doFlowEvents.C" or do {
	$command = "cp $macDir/doFlowEvents.C $workDir/doFlowEvents.C"; # don't use copy
	print $command,"\n";
	`$command`;
      };
      # make links
      -e "$workDir/.sl44_gcc346" or do {
	$command = "ln -s $rootDir/.sl44_gcc346 $workDir/.sl44_gcc346";
	print $command,"\n";
	`$command`;
      };
      -e "$workDir/M_WeightsY7.root" or do {
	$command = "ln -s $rootDir/M_WeightsY7.root $workDir/M_WeightsY7.root";
	print $command,"\n";
	`$command`;
      };
      -e "$workDir/$inDir" or do {
	$command = "ln -s $homeDir/$inDir $workDir/$inDir";
	print $command,"\n";
	`$command`;
      };

      my $firstPassOutput = "$workDir/flow.dirCumulant.root";
      my $firstPassDone = "kFALSE";
      if (-e $firstPassOutput) {
	$firstPassDone = "kTRUE";
	print "$subDir-$runNo firstPassDone\n";
      }

      # make the shell script
      my $scriptName = "$workDir/flow.csh";
      open(FH,">$scriptName") or die "$scriptName : $!\n";
      print FH <<EOF;
#!/bin/csh
#\$ -o /dev/null
#\$ -e /dev/null
#\$ -M AMPoskanzer\@LBL.gov
#\$ -m a
cd $workDir
source \$GROUP_DIR/.starver $starVer
set logDir = \$SCRATCH
set log = \$logDir/ana.log
root4star -b << FINIS >&! \$log
.L doFlowEvents.C
doFlowEvents($events,"$inDir/$subDir","$fileExt")
.q
FINIS
set outLog = ana.log
head -130 \$log > \$outLog
echo " " >> \$outLog
echo "########################  head -> tail ##########################" >> \$outLog
echo " " >> \$outLog
tail -300 \$log >> \$outLog
exit
EOF
      # execute the shell script, option N should have only 10 characters
      close FH;
      chmod 0755, $scriptName;
      $command = "$exec -hard $resource -l h_cpu=$queTime -N $inDir$subDirNo-$runNo $scriptName";
      if ($firstPassDone eq "kFALSE") {      # skip if 1st pass completed
	print "##### ",$command,"\n";
	`$command` unless $opt{t};
      };
    }
}

#///////////////////////////////////////////////////////////////////////////////
#//
#// $Log: doFlowSubmitOne.pl,v $
#// Revision 1.1  2010/03/08 16:54:51  posk
#// Added StFlowDirectCumulantMaker written by Dhevan Gangadharan.
#//
#//
#///////////////////////////////////////////////////////////////////////////////
