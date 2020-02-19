#!/usr/bin/env perl
use File::Basename;
use Cwd;
my $daq = "";
my @List = glob "*picoDst.root";
my $debug = 1;
foreach my $file (@List) {
#  my $Dir = File::Basename::dirname($file);
  my $fileB = File::Basename::basename($file,".picoDst.root");
#  print "file = $file, Dir = $Dir, fileB = $fileB  \n" if ($debug);
  $fileB =~ s/st_physics_//;
  $fileB =~ s/adc_//;
  $fileB =~ s/hlt_//;
  my ($run) = split('_',$fileB);
  my $day = int ($run/1000);
  $day    =     ($day%1000);
  my $sday = sprintf("%03i",$day);
  print "$file => day = $day, sday = $sday,  run = $run\n" if ($debug);
  my $dir = $sday . "/" . $run;
  if (! -d $dir) {`mkdir -p $dir`;}
  my $cmd = "cd $dir; ln -sf ../../$file .";
  print "cmd = $cmd\n";
  my $flag = system($cmd);
#  if (! $flag) {die;}
}
