#!/usr/bin/env perl
use File::Basename;
use Cwd;
my $daq = "";
my $reduction = 0;
my $pwd = cwd();
for (my $day = 107; $day < 122; $day++) {
  my $daydir = $day;
  if ($day <  10) {$daydir = "00" . $day;}
  elsif ($day < 100) {$daydir =  "0" . $day;}
  if (! -d $daydir) {next;}
  my $cmd = "find " . $daydir ." -name \"*event.root\" -cmin +60";
  print "# cmd = $cmd\n";
  my @list = `find $daydir -name "*event.root" -cmin +60`;
#  my $list = `$cmd`;
  #print "ist = @list\n";
  my $count = 0;
  foreach my $file (@list) { 
    $count++;
    my $keep = $count%4;
    if ($keep != 1) {
      print "rm     $file";
    } else {
      print "# keep $file";
    }
  }
}
