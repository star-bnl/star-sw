#! /usr/bin/env perl
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename,events' -cond 'filetype=online_daq,filename~st_laser,tpx=1,sanity=1,runnumber>15000000,events>10' -limit 0`;
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'filetype=online_daq,filename~st_physics_15,tpx=1,sanity=1,runnumber>15000000,events>10' -limit 0`;
#my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'filetype=online_daq,filename~st_physics_adc,tpx=1,sanity=1,events>10,trgsetupname=pp_production_2012,runnumber>=13099056,runnumber<=13099058' -limit 0`; 
my @list = `get_file_list.pl -delim '/' -keys 'path,filename' -cond 'filetype=online_daq,filename~st_zerobias_adc,tpx=1,sanity=1,events>10,runnumber>=13099056' -limit 0`; # print "@list\n";
my $i = 1;   
my $OldRun = 0;
foreach my $file (@list) { 
  chomp($file);
  my @words = split('/',$file);
  my $run = $words[7];
#  print "run = $run\n";
  if ($run != $OldRun) {$i = 1; $OldRun = $run;}
  else                 {$i++;}
  if ($i%20 != 1) {next;}
  my $basename = $file;
  $basename =~ s#/home/starsink/raw##;
#  print "$file = > $basename\n";
  my $found = 0;
  my $dir;
  my @dirs = qw(/star/data03);# /gpfs01/star/scratch/fisyak);
  foreach $dir (@dirs) {
    my $fullpath = $dir . $basename;
#    print "$i : $fullpath ===========";
    if (-r $fullpath) {
      $found = 1; 
      #print "found\n"; 
      last;
    }
#    print "not found\n";
  }
  if (! $found) {
    print "$file $dirs[0]$basename\n";
  }
}
