#!/usr/bin/env perl 
use File::Basename;
use File::Copy;
#use File::Copy 'move';
use Sys::Hostname;
use Cwd;
my $pwd = Cwd::cwd(); #print "pwd = $pwd\n";
my $OLD = ".";
#my $NEW = "..";
my $NEW = "/net/l401/data/scratch1/daq/2020";
#my $glob = $OLD . "/*.daq"; 
my $glob = $OLD . "/st*.daq";
#my @daqs = `ls -1d  "/*.daq`; # 
my @daqs  = glob $glob; # print "glob = $glob, daqs  @daqs\n";
foreach my $file (@daqs) {
  chomp($file);
  if (-l $file) {next;}
  my $bfile = File::Basename::basename($file);
  my $dfile = $bfile;
  $dfile =~ s/st_physics_adc_//;
  $dfile =~ s/st_physics_//;
  $dfile =~ s/hlt_//;
  my ($run) = split '_', $dfile;
  my $day = int ($run/1000)%1000; # - 20000;
  if ($day < 0) {die "illegal day";}
  if    ($day < 10) {$day = "00" . $day;}
  elsif ($day < 100) {$day = "0" . $day;}
  my $dir = $NEW . "/" . $day . "/" . $run; #print "dir = $dir\n";
  my $newfile = $dir . "/" . $bfile;
  print "$file => $newfile => $run => $day\n";
  if (! -d $dir) {`mkdir -p $dir`;}
  if (-r $newfile) {
    print "newfile = $newfile already exists. Skip\n";
    next;
  }
  move ($file, $newfile) or die "move $file, $newfile failed: $!";
#  symlink($newfile,$file) or die print "$!\n";
#  my $cmd = "mv " . $file . " . $newfile;
#  print "cmd = $cmd\n";
#  my $flag = system($cmd);
#  if ($flag) {die "Can't move $bfile";}
#  die;
}
