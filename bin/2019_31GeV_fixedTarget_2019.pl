#! /usr/bin/env perl
#my $cmd = "get_file_list.pl -delim '/' -keys 'path,filename' -cond 'filetype=online_daq,filename~st,trgsetupname=production_31GeV_fixedTarget_2019,tpx=1,itpc=1,sanity=1,runnumber>20000000' -limit 0";  
my $cmd = "get_file_list.pl -delim '/' -keys 'path,filename' -cond 'filetype=online_daq,filename~st_physics_20,trgsetupname=production_31GeV_fixedTarget_2019,tpx=1,itpc=1,sanity=1,runnumber>201890000,runnumber<20191000' -limit 0";  

print "$cmd\n";
my @list = `$cmd`;

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
	
