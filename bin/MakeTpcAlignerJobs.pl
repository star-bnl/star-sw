#!/usr/bin/env perl
use File::Basename;
use Cwd;
my $macro = "TpcAligner";
my $debug = 0;
my @FilesNew = glob "../*.event.root"; print "FilesNew @FilesNew\n" if $debug;
foreach my $f (@FilesNew) {
  my $bf = File::Basename::basename($f);
  $bf =~ s/\.event\.root//;
  $bf =~ s/st_physics_//;
  $bf =~ s/adc_//;
  my $key = $bf;
  my $SCRIPT = $key;
  my $LOG = $SCRIPT . ".log";
  my $root = $SCRIPT;
  my $RootFile = $root . ".root";
  next if -r $RootFile;
  $SCRIPT .= ".csh";
  print "Create $SCRIPT\n";
  open (OUT,">$SCRIPT") or die "Can't open $SCRIPT";
  print OUT "#! /usr/local/bin/tcsh -f\n";
  #    print OUT "source /afs/.rhic.bnl.gov/star/group/.starver .DEV2;\n";
  my $cmd = "";
  $cmd .= "test ! -r " . $RootFile  . " && root -l -q -b  '" . $macro;
  $cmd .= ".C(1000000,\"" . $f . "\")\' >& $LOG";
  print OUT "$cmd\n";
  close (OUT);
}


