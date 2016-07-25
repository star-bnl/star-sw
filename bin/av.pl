#! /usr/bin/env perl
use File::Basename;
use Cwd;
my @files = glob "*ClnoW.s*.Fit8.root";
#print "files = @files\n";
my %Hash = ();
foreach my $file (@files) {
  my @words = split '\.', $file;
  my $key = $words[0];
  $Hash{$key} .= " " . $file;
#  print "$key => $Hash{$key}\n";
}
foreach my $key ( sort keys %Hash ) {
  print "$key => $Hash{$key}\n";
  my $fin  = $key . ".ClnoW.s*.Fit8.root";
  my $fout = $key . ".ClnoW.event.root";
  if (-r $fout) {next;}
  my $cmd = "root.exe -q -b 'makeAvLaserHits.C(\"" . $fin ."\")' >& " . $key . ".log";
  print "$cmd\n";
  my $flag = system($cmd);
}
