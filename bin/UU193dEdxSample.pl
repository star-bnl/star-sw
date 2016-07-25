#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = cwd();
#my @list = glob "/star/data03/daq/2013/15[4-9]/*/st_physics*.daq";
my @list = glob "/star/data03/daq/2012/1[1-3]*/*/st_physics_*_raw_101000[1|5].daq /star/simu/fisyak/daq/2012/1[1-3]*/*/st_physics_*_raw_101000[1|5].daq /star/data15/TPC/daq/2012/1[1-3]*/*/st_physics_*_raw_101000[1|5].daq";
my $i = 0;
foreach my $line (@list) {
#  chop($line);
  my $file = File::Basename::basename($line,"daq") . "event.root";
  my $bla  = File::Basename::basename($line,"daq") . "bla.root";
  if (-e $file || -e $bla) {next;}
  print "string:$line\n"; # => $file\n;
  $i++;
  if ($i >= 50) {last;}
}
