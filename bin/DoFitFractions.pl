#!/usr/bin/env perl
foreach my $name (qw(sZdEdx sZdEdxFit)) {
  for (my $j = 1; $j <=24; $j++) {
#    my $cmd = "root.exe -q -b ../RunXII_W_dEdxNew3.root 'doFractionFit.C(\"" . $name . "\",-1,-1,1,-1," . $j . "," . $j . ",1,kFALSE,kFALSE,3)' >& " . $name . "_" . $j . ".log";
    my $cmd = "root.exe -q -b ../RunXII_W_dEdxNew3.root 'doFractionFit.C(\"" . $name . "\",-1,-1,1,-1," . $j . "," . $j . ",0,kFALSE,kFALSE,3)' >& " . $name . "_" . $j . ".log";
    my $file = $name . "_" . $j . ".csh";
    open(Out, ">$file") or die "Can't open $file";
    print Out "$cmd\n";
    close(Out);
  }
}
