#!/usr/bin/env perl
use File::Basename;
use Cwd;
use Env;
my @list = glob "/star/institutions/bnl/fisyak/daq/2003/*/st_laser*.daq";
print "@list\n";
foreach my $f (@list) {
  my $file = File::Basename::basename($f);
  my $dir  = File::Basename::dirname($f);
  print "file = $file\n";
  $file =~ s/_raw.*//;
  print "file = $file\n";
  my $SCRIPT = $file . ".csh";
  if (-r $SCRIPT) {next;}
  my $root = $file . ".root";
  my $log  = $file . ".log";
  print "Create $SCRIPT\n";
  open (OUT,">$SCRIPT") or die "Can't open $SCRIPT";
  print OUT "#! /usr/local/bin/tcsh -f\n";
  my $macro = "'bfc.C(100000,\"in,TpxAvLaser\",\"" . $dir . "/" . $file . "*.daq" . "\",0,\"" . $root . "\")' >& $log";
  my $cmd = "test ! -r " . $root . " && root.exe -q -b " . $macro;
  print "$cmd\n";
  print OUT "$cmd\n";
  close (OUT);
}
