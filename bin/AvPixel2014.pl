#! /usr/bin/env perl
use File::Basename;
use Cwd;
#my @list = glob "/star/data03/daq/2014/040/15040047/st_laser_adc_*.daq  /star/data03/daq/2014/040/15040049/st_laser_adc_*.daq /star/data03/daq/2014/050/15050192/st_laser_adc_*.daq /star/data03/daq/2014/050/15050194/st_laser_adc_*.daq /star/data03/daq/2014/051/15051026/st_laser_adc_*.daq /star/data03/daq/2014/051/15051088/st_laser_adc_*.daq";
#my @list = glob "/star/data03/daq/2014/064/15064050/st_laser_adc_15064050_*.daq";
my @list = glob "/gpfs01/star/scratch/fisyak/daq/2014/070/15070022/*.daq";
foreach my $fullpath (@list) {
  my $file = File::Basename::basename($fullpath,".daq");
  my $rootfile = $file . ".ClnoW.root";
  if (-r $rootfile) {next;}
  print "string:$fullpath\n";
}
