#! /usr/bin/env perl
# get_file_list.pl -delim '/' -keys 'path,filename,events' -cond 'filetype=online_daq,filename~st_phy,tpx=1,runnumber[]14074001-14128999,events>1000' -limit 10
use File::Basename;
use Cwd;
my $pwd = cwd();
#my @list = glob "/star/data03/daq/2013/15[4-9]/*/st_physics*.daq";
my @list = glob "/star/data03/daq/2013/*/*/st_physics_14*_raw_*.daq  /star/data15/TPC/daq/2013/*/*/st_physics_14*_raw_*.daq /star/data16/TPC/daq/2013/*/*/st_physics_14*_raw_*.daq";
my $j = 0;
my $Total = 1000;
my $step  =  500;
foreach my $file (@list) {
  chomp($file);
#  my $bla  = File::Basename::basename($file,"daq") . "bla.root";
#  if (-e $bla) {next;}
  my ($dev, $ino, $mode, $nlink, $uid, $gid, $rdev, $size, $atime, $mtime, $ctime, $blksize, $blocks ) = stat $file;
#  print "$file $uid\n";
  if ($uid != 2764) {next;}
  my $fileb = File::Basename::basename($file,".daq"); 
  for (my $i = 1 - $step; $i < $Total; $i += $step) {
    if ($i < 0) {
      my $rootf = $fileb . ".event.root";
      if (-e $rootf) {last;}
      next;
    }
    my $e = $i + $step - 1;
    my $rootf = $fileb . "_" . $i . "_" . $e  .".event.root"; 
    if (-e $rootf) {next;}
#    print "$rootf\n";
    print "string:$file:$i:$e\n"; # => $file\n;
  }
#  print "$file\n";
  $j++;
#  if ($j >= 50) {last;}
}
