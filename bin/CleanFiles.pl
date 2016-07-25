#!/usr/bin/env perl
my $logf = "Reco.list";
my $files = "*.root";
if ($#ARGV >= 0) {$files = $ARGV[0];}
if (! -r $logf) {
 my $cmd = "root.exe -q -b $files" . ' >& ' . $logf;
 `$cmd`;
}
if (! -d hold) {mkdir hold;}
open(IN,"<$logf") or die "Cannot open $logf";
my $line;
while ($line = <IN>) {
  next if $line !~ /not closed, trying to recover/;
  my ($dum,$dum,$dum,$dum,$file) = split ' ',$line;
  print "Recovered $file\n";
  $file =~ s/Event//;
  $file =~ s/\.root//;
  my $cmd = "mv *" . $file . "*.* hold/";
  print "cmd: $cmd\n";
  `$cmd`;
#  qq($cmd);
}
