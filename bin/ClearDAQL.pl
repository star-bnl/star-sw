#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = cwd();
my $sql = '\'select run,file from RunLog.daqFileTag  where hpss = 1 and file like "%daq" and ';
$sql .= 'run in (select runNumber  from RunLog.magField where abs(scaleFactor) < 0.1) and  ';
$sql .=  'run in (select runNumber  from RunLog.runDescriptor where glbSetupName like "laser%") and ';
$sql .=  'run in (select runNumber  from RunLog.detectorSet where detectorID = 0) and '; # tpc
$sql .=  'run in (select runNumber  from RunLog.runStatus where shiftLeaderStatus = 0) \'';
my @list = `mysql RunLog -h onldb.starp.bnl.gov -P 3501 -e $sql`;

foreach my $item (@list) {
  next if $item !~ /daq/;
  chomp($item); #print "$item\n";
  my ($run,$file) = split ' ', $item;
  my @chars = split '', $run;
  my $day  = $chars[1] . $chars[2] . $chars[3];
#  print "$item $file $day $run \n";
  my $dir = $day . "/" . $run;
  my $topdir = "/home/starsink/raw/daq/2007/";
  if (! -d $dir ) {`mkdir -p $dir`;}
  my $FullPath =  $topdir  . $dir . "/" . $file;
  my $Path = $dir . "/" . $file;
#  print "get $FullPath => $Path\n";
  if (-r $Path) { 
    print "$Path has been read\n";
  }
}

