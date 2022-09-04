#!/usr/bin/env perl
use File::Basename;
use Cwd;
my $daq = "";
my $reduction = 0;
my $pwd = cwd();
my $glob = "*/*/*event.root";
my @files = glob $glob; print "#total files = $#files\n";
my @count = 0;
foreach my $file (@files) { 
  $count++;
  my $keep = $count%2;
  if ($keep == 0) {
    print "rm     $file";
  } else {
    print "# keep $file";
  }
  print "\n";
}
