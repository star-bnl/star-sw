#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $file = "file:/star/institutions/bnl/fisyak/MuDst/2011/AuAu200/MudEdx_Sparse_pT100_eta24.root";
my $NRefMult = 10;
my $NpT = 100;
my $NpT2 = $NpT/2;
my $Neta = 24;
#print "NRefMult = $NRefMult NpT = $NpT Neta = $Neta\n";
for (my $r = 0; $r <= $NRefMult; $r++) {
  my $r1 = $r;
  my $r2 = $r;
  for (my $i = 1; $i < $NpT; $i += $NpT2) {
    my $i1 = $i;
    my $i2 = $i1 + $NpT2 - 1;
    if ($i2 >= $NpT) {$i2 = -1;}
    my $line = $file . ":" . $r1 . ":" .  $r2 . ":1:-1:" . $i1 . ":" .  $i2;
    print "string:$line\n";
  }
}

