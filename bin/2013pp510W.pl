#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = cwd();
my $debug = 0;
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'filetype=online_daq,filename~st_physics_adc_15,tpx=1,pxl=1,ist=1,sanity=1,magscale=ReversedFullField,trgsetupname=AuAu_200_LowLuminosity_2014' -limit 0`;
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'filetype=online_daq,filename~st_physics_15,tpx=1,sanity=1,magscale=ReversedFullField,trgsetupname=AuAu_200_LowLuminosity_2014' -limit 0`;
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'filetype=online_daq,filename~st_physics_15,tpx=1,sanity=1,magscale=ReversedFullField,trgsetupname=AuAu_200_LowLuminosity_2014' -limit 0`;
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'filetype=online_daq,filename~st_physics_adc_15,tpx=1,sanity=1,magscale=ReversedFullField,trgsetupname=AuAu_200_LowLuminosity_2014' -limit 0`;
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'filetype=online_daq,filename~st_WB_14,runnumber>14076004,runnumber<14125061,tpx=1,tof=1,sanity=1,trgsetupname=pp500_production_2013' -limit 100`;
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'filetype=online_daq,filename~st_WE_14,runnumber>14076004,runnumber<14125061,tpx=1,tof=1,sanity=1,trgsetupname=pp500_production_2013' -limit 100`;
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'filetype=online_daq,filename~st_W,runnumber>14076004,tpx=1,sanity=1,trgsetupname=pp500_production_2013' -limit 0`;
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'filetype=online_daq,filename~st_W,runnumber>14076004,tpx=1,sanity=1,trgsetupname=pp500_production_2013' -limit 0`;
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'filetype=online_daq,filename~st_physics_15,tpx=1,sanity=1,trgsetupname=AuAu_200_LowLuminosity_2014' -limit 100`;
#my @list = `get_file_list.pl -delim ':' -keys 'path,filename,events' -cond 'filetype=online_daq,filename~st_W,runnumber>14076004,tpx=1,sanity=1,trgsetupname=pp500_production_2013,events>1' -limit 0`;
my @list = `cat /star/subsys/tpc/fisyak/daq/2013W/2013W.events1`;
my ($run,$dummy);
foreach my $l (@list) {
  chomp($l);
  my ($dir,$daqf,$N) = split(":",$l); print "dir = $dir, daqf = $daqf, N = $N\n" if ($debug);
  my $file = $dir . "/" . $daqf;
#  chomp($file);
  my $daqf = File::Basename::basename($file,".daq");# print "$daqf\n";
#  my $cmd = "get_file_list.pl -keys 'events' -cond 'filetype=online_daq,filename=" . $daqf . ".daq' -limit 1"; print "$cmd\n" if ($debug);
#  my $N = `$cmd`; chomp($N); print "N = $N\n" if ($debug);
  if ($daqf =~ /adc/) {
    ($dummy,$dummy,$dummy,$run) = split('_',$daqf);#  print "$daqf => $run\n";
  } else {
    ($dummy,$dummy,$run) = split('_',$daqf);#  print "$daqf => $run\n";
  }
#  if ($run < 14077000) {next;}
  if ($run <= 14145000) {next;}
#  if ($run > 14155000) {last;}
  my $basename = $file;
  $basename =~ s#/home/starsink/raw/daq/2013##;
  my $found = 0;
  my $dir;
  my $logf = $basename; 
  $logf =~ s#.daq##; print "logf = $logf\n" if ($debug);
  $logf .= "*" . $N . ".log*";   print "logf = $logf\n" if ($debug);
  my @glob = glob "/gpfs01/star/pwg/fisyak/reco/2013W" . $logf;
  my $done = 0;
  if ($#glob >= 0) {$done = 1;}
  my @dirs = qw(/star/data03/daq/2013 /gpfs01/star/scratch/fisyak/daq/2013);
  foreach $dir (@dirs) {
    my $fullpath = $dir . $basename;
    if (-r $fullpath) {
      $found = 1; 
      last;
    }
  }
  if ($found) {
    if ($done) {
      print "$file $dirs[1]$basename Done\n";
    }
  } else {
    if (! $done) {
      print "$file $dirs[1]$basename\n";
    }
  }
}
