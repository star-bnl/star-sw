#! /usr/bin/env perl
use File::Basename;
use Cwd;
#my @list = glob "/gpfs01/star/scratch/fisyak/daq/2017/*/*/st_laser*.daq";
my @list = ();
if (-d "/net/l401/data/scratch1/fisyak/") {
  @list = glob "/net/l401/data/scratch1/daq/2018/*/*/*laser*.daq";
} else {
  @list = glob "/star/data03/daq/2018/*/*/st_laser*.daq";
}
foreach my $fullpath (@list) {
  my $file = File::Basename::basename($fullpath,".daq");
  my $rootfile = "./" . $file . ".laser.root";
  if (-r $rootfile) {next;}
#   my $glob = "./*/" . $file . ".laser.root";
#   my @rootfiles = glob $glob;# print "glob = $glob rootfiles = @rootfiles\n";
#   if ($#rootfiles >= 0) {next;}
  $rootfile = $file;
  $rootfile =~ s/_adc//;
  $rootfile .= "_adc.laser.root";
  if (-r $rootfile) {next;}
  print "string:$fullpath\n";
}
