#!/usr/bin/env perl
use File::Basename;
use Cwd;
my $daq = "";
my $reduction = 0;
my $pwd = cwd();
for (my $run = 1; $run < 108; $run++) {
  my $rundir = $run;
  if ($run <  10) {$rundir = "00" . $run;}
  elsif ($run < 100) {$rundir =  "0" . $run;}
  my $cmd = "find " . $rundir ." -name \"*picoDst.root\" -cmin +60";
  print "# cmd = $cmd\n";
  my @list = `$cmd`;
  #print "ist = @list\n";
  my $count = 0;
  foreach my $file (@list) { 
    $count++;
    my $keep = $count%10;
    if ($keep != 1) {
      print "rm     $file";
    } else {
      print "# keep $file";
    }
  }
}
