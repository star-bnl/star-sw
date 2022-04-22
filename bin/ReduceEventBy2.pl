#!/usr/bin/env perl
use File::Basename;
use Cwd;
my $daq = "";
my $reduction = 0;
my $pwd = cwd();
for (my $run = 100; $run < 106; $run++) {
  my $rundir = $run;
  if ($run <  10) {$rundir = "00" . $run;}
  elsif ($run < 100) {$rundir =  "0" . $run;}
  my $cmd = "find " . $rundir ." -name \"*event.root\" -cmin +60";
  print "# cmd = $cmd\n";
  my @list = `find $rundir -name "*event.root" -cmin +60`;
  #print "ist = @list\n";
  my $count = 0;
  foreach my $file (@list) { 
    $count++;
    my $keep = $count%2;
    if ($keep == 0) {
      print "rm     $file";
    } else {
      print "# keep $file";
    }
  }
}
