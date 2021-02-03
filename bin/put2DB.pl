#!/usr/bin/env perl
foreach my $file (@ARGV) {
  if ($file !~ /\*/ && ! -r $file) {
    print "Can't read $file --- skipped\n";
    next;
  }
  my $log = $file;
  $log =~ s/\*//;
  $log =~ s/\.root//;
  $log .= "_" . '`date +%m%d%y:%H%M`.log';
#  my $cmd = "root.exe -q -b  lDb.C put2DB.C\\\(\\\"". $file . "\\\"\\\) >& " . $log;
#  my $cmd = "root4star -q -b  put2DB.C\\\(\\\"". $file . "\\\"\\\) | tee " . $file . ".log";
#  my $cmd = "root.exe -q -b  lDb.C 'put2DB.C(" . '"'  . $file . '","TFG")' . "' >& " . $log;
  my $cmd = "root.exe -q -b  lDb.C 'put2DB.C(" . '"'  . $file . '","ofl")' . "' >& " . $log;
  print "$cmd\n";
  my $status = system($cmd);
  if ($status) {last;}
}
