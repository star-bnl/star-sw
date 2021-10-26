#!/usr/bin/env perl
my %ARG = (flavor => 'ofl',
	  );
foreach my $opt (@ARGV) {
  if ($opt =~ /=/) { 
#    print "$opt\n";
    $_ = $opt;
    my($key, $val) = /([^=]*)=(.*)/; $ARG{$key} = $val;
#    my $line =  "ARG{" . $key . "} = " . $ARG{$key};
#    print "$line\n";
  }
}
foreach my $file (@ARGV) {
  if ($file =~ /=/) {next;}
  print "file = $file\n";
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
#  my $cmd = "root.exe -q -b  lDb.C 'put2DB.C(" . '"'  . $file . '","ofl")' . "' >& " . $log;
#  my $cmd = "root.exe -q -b  lDb.C 'put2DB.C(" . '"'  . $file . '","FXT")' . "' >& " . $log;
  my $cmd = "root.exe -q -b  lDb.C 'put2DB.C(" . '"'  . $file . '","' . $ARG{flavor}  .'")' . "' >& " . $log;
  print "$cmd\n";
  my $status = system($cmd);
  if ($status) {last;}
}
