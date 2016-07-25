#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = cwd();
#my $sql = '\'select run,file from RunLog.daqFileTag  where hpss = 1 and file like "%laser%daq"';
##$sql .= ' and run in (select runNumber  from RunLog.magField where abs(scaleFactor) < 0.1)'; 
#$sql .=  ' and run in (select runNumber  from RunLog.daqSummary where numberOfEvents > 2000)'; 
#$sql .=  ' and run in (select runNumber  from RunLog.runDescriptor where glbSetupName like "laser%")'; 
#$sql .=  ' and run in (select runNumber  from RunLog.detectorSet where detectorID = 0)'; # tpc 
#$sql .=  ' and run in (select runNumber  from RunLog.detectorSet where detectorID = 20)'; # tpx 
#$sql .=  ' and run in (select runNumber  from RunLog.runStatus where shiftLeaderStatus = 0) \'';
my $sql = '\'select entryTime,beginTime,elementID,runNumber,frequency  from RunLog.clockFrequency  where deactive = 0';
$sql .=  ' and runNumber in (select runNumber  from RunLog.daqSummary where numberOfEvents > 2000)'; 
$sql .=  ' and runNumber in (select runNumber  from RunLog.runDescriptor where glbSetupName like "laser%")'; 
$sql .=  ' and runNumber in (select runNumber  from RunLog.detectorSet where detectorID = 0)'; # tpc 
$sql .=  ' and runNumber in (select runNumber  from RunLog.detectorSet where detectorID = 20)'; # tpx 
$sql .=  ' and runNumber in (select runNumber  from RunLog.runStatus where shiftLeaderStatus = 0) \'';
print "$sql\n";
my @list = `mysql RunLog -h onldb.starp.bnl.gov -P 3501 -e $sql`;
#my @list = `mysql RunLog -h onldb.starp.bnl.gov -P 3503 -e $sql`;
print "list : @list\n";
die;
foreach my $item (@list) {
  next if $item !~ /daq/;
#  chomp($item); #print "$item\n";
#  my ($run,$file) = split ' ', $item;
#  my @chars = split '', $run;
#  my $day  = $chars[1] . $chars[2] . $chars[3];
#  print "$item $file $day $run \n";
#  my $dir = $day . "/" . $run;
#  my $topdir = "/home/starsink/raw/daq/2007/";
#  if (! -d $dir ) {`mkdir -p $dir`;}
#  my $FullPath =  $topdir  . $dir . "/" . $file;
#  my $Path = $dir . "/" . $file;
#  if (! -r $Path) { 
#    print "get $FullPath => $Path\n";
#    `hsi get $Path : $FullPath `;
#    $no++;
#  } else {
#    print "$FullPath has been read\n";
#  }
  #    if ($no > 50) {last;}
  print "$item\n";
}
#close (In);
