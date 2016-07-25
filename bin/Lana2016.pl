#! /usr/bin/env perl
use File::Basename;
use Cwd;
#my @list = glob "/gpfs01/star/scratch/fisyak/daq/2014/*/*/st_laser*.daq";
my @list = glob "/star/data03/daq/2016/*/*/st_laser*.daq";
foreach my $fullpath (@list) {
  my $file = File::Basename::basename($fullpath,".daq");
  my $rootfile = $file . ".laser.root";
  if (-r $rootfile) {next;}
  $rootfile = $file;
  $rootfile =~ s/_adc//;
  $rootfile .= "_adc.laser.root";
  if (-r $rootfile) {next;}
  print "string:$fullpath\n";
}
