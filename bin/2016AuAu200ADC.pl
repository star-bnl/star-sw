#! /usr/bin/env perl
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename,events' -cond 'filetype=online_daq,filename~st_laser,tpx=1,sanity=1,runnumber>15000000,events>10' -limit 0`;
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'filetype=online_daq,filename~st_laser,tpx=1,sanity=1,runnumber>16161045,events>10' -limit 0`;
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'magscale=FullField,filetype=online_daq,trgsetupname=CosmicLocalClock,tpx=1,pxl=1,ist=1,gmt=1,sanity=1,events>10000' -limit 50`;
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'magscale=ReversedFullField,filetype=online_daq,trgsetupname=CosmicLocalClock,tpx=1,pxl=1,ist=1,gmt=1,sanity=1,events>10000' -limit 50`;
#if ($#ARGV < 0) {
#  print "Usage $0 ZF, FF or RF\n";
#  exit 0;
#}
my $field = "ReversedFullField";
if ($#ARGV >= 0) {
  my $Field = $ARGV[0];
  print "$Field\n";
  if    ($Field eq "FF") {$field = "FullField";}
  elsif ($Field eq "RF") {$field = "ReversedFullField";}
  #elsif ($Field eq "ZF") {$field = "FieldOff"; }
  #print "$Field => $field\n";
}
#if ($field eq "") {die "$Field has not defined";}
#my
# $cmd = "get_file_list.pl -delim '/' -keys 'path,filename' -cond 'magscale=" . $field . ",filetype=online_daq,filename~st,trgsetupname=CosmicLocalClock,tpx=1,gmt=1,sanity=1,runnumber>17000000' -limit 0"; 
# $cmd = "get_file_list.pl -delim '/' -keys 'path,filename' -cond 'magscale=" . $field . ",filetype=online_daq,filename~adc,trgsetupname=AuAu_200_production_2016,tpx=1,gmt=1,sanity=1,runnumber>17000000' -limit 0"; 
# $cmd = "get_file_list.pl -delim '/' -keys 'path,filename' -cond 'magscale=" . $field . ",filetype=online_daq,filename~adc,trgsetupname=low_lumi_AuAu2016,tpx=1,gmt=1,sanity=1,runnumber>17000000' -limit 0"; 
 $cmd = "get_file_list.pl -delim '/' -keys 'path,filename' -cond 'magscale=" . $field . ",filetype=online_daq,filename~st_physics_adc,tpx=1,gmt=1,sanity=1,runnumber=17126051' -limit 0"; 
#print "$cmd\n";
my @list = `$cmd`;
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'magscale=ReversedFullField,filetype=online_daq,filename~st_gmt,trgsetupname=CosmicLocalClock,tpx=1,gmt=1,sanity=1,events>10000' -limit 50`;

foreach my $file (@list) {
  chomp($file);
  my $basename = $file;
  $basename =~ s#/home/starsink/raw##;
#  print "$file = > $basename\n";
  my $found = 0;
  my $dir;
  my @dirs = qw(/star/data03 /gpfs01/star/scratch/fisyak);
  foreach $dir (@dirs) {
    my $fullpath = $dir . $basename;
#    print "$fullpath ===========";
    if (-r $fullpath) {
      $found = 1; 
#      print "found\n"; 
      last;
    }
#    print "not found\n";
  }
  if (! $found) {
    print "$file $dirs[0]$basename\n";
  }
}
	
