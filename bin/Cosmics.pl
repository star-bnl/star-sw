#!/usr/bin/env perl
use File::Basename;
use Cwd;
my $macro = "Cosmics.C";
my $debug = 1;
foreach my $d (glob "???") {
  print "$d\n";
  my $SCRIPT = $d . ".csh";
  print "Create $SCRIPT\n";
  open (OUT,">$SCRIPT") or die "Can't open $SCRIPT";
  print OUT "#! /usr/local/bin/tcsh -f\n";
  my $RootFile = "Cosmics_" . $d . ".root";
  my $LOG = $d . ".log";
  my $cmd = "";
  $cmd .= "test ! -r " . $RootFile  . " && root -l -q -b lMuDst.C '" . $macro;
  $cmd .= "+(\"" . $d . "/*MuDst.root\",\"" . $RootFile .  "\")\' >& $LOG";
  print OUT "$cmd\n";
  close (OUT);
}
