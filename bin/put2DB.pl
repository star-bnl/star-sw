#!/usr/bin/env perl
foreach my $file (@ARGV) {
  if (! -r $file) {
    print "Can't read $file --- skipped\n";
    next;
  }
  my $cmd = "root.exe -q -b  lDb.C put2DB.C\\\(\\\"". $file . "\\\"\\\) | tee " . $file . ".log";
#  my $cmd = "root4star -q -b  put2DB.C\\\(\\\"". $file . "\\\"\\\) | tee " . $file . ".log";
  print "$cmd\n";
  my $status = system($cmd);
  if ($status) {last;}
}
