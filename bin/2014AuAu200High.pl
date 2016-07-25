#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = cwd();
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'filetype=online_daq,filename~st_physics_adc_15,tpx=1,pxl=1,ist=1,sanity=1,magscale=ReversedFullField,trgsetupname=AuAu_200_LowLuminosity_2014' -limit 0`;
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'filetype=online_daq,filename~st_physics_15,tpx=1,sanity=1,magscale=ReversedFullField,trgsetupname=AuAu_200_LowLuminosity_2014' -limit 0`;
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'filetype=online_daq,filename~st_physics_15,tpx=1,sanity=1,magscale=ReversedFullField,trgsetupname=AuAu_200_LowLuminosity_2014' -limit 0`;
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'filetype=online_daq,filename~st_physics_adc_15,tpx=1,sanity=1,magscale=ReversedFullField,trgsetupname=AuAu_200_LowLuminosity_2014' -limit 0`;
my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'filetype=online_daq,filename~st_physics_15,runnumber>1508922,runnumber<15089055,tpx=1,tof=1,sanity=1,trgsetupname=AuAu_200_Production_high_2014' -limit 100`;
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'filetype=online_daq,filename~st_physics_15,tpx=1,sanity=1,trgsetupname=AuAu_200_LowLuminosity_2014' -limit 100`;
my ($run,$dummy);
foreach my $file (@list) {
  chomp($file);
  my $daqf = File::Basename::basename($file,".daq");# print "$daqf\n";
  if ($daqf =~ /adc/) {
    ($dummy,$dummy,$dummy,$run) = split('_',$daqf);#  print "$daqf => $run\n";
  } else {
    ($dummy,$dummy,$run) = split('_',$daqf);#  print "$daqf => $run\n";
  }
# if ($run < 15100084 || $run > 15100090) {next;}
  if ($run > 15163000) {next;}
  my $basename = $file;
  $basename =~ s#/home/starsink/raw##;
#  print "$file = > $basename\n";
  my $found = 0;
  my $dir;
  my @dirs = qw(/star/data03); # /gpfs01/star/scratch/fisyak);
  foreach $dir (@dirs) {
#    my $fullpath = $dir . $basename;
#    print "$fullpath ===========";
    if (-r $fullpath) {
      $found = 1; 
      print "found\n"; 
      last;
    }
#    print "not found\n";
  }
  if (! $found) {
    my $rootfile = $file;
    $rootfile =~ s/daq$/MuDst\.root/;
    if (-r $rootfile) {next;}
    print "$file $dirs[0]$basename\n";
  }
  last;
}
