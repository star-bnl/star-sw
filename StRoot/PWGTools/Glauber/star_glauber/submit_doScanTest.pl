#! /usr/bin/perl

my $nppbin = 1;
my $nppmin = 2.01;
my $nppmax = 2.01;

my $kbin = 1;
my $kmin = 2.0;
my $kmax = 2.0;

my $xbin = 1;
my $xmin = 0.10;
my $xmax = 0.10;

my $effbin = 1;
my $effmin = 0.11;
my $effmax = 0.11;

my $npp_step = ($nppbin==1) ? 0 : ($nppmax-$nppmin)/($nppbin-1);
my $k_step = ($kbin==1) ? 0 : ($kmax-$kmin)/($kbin-1);
my $x_step = ($xbin==1) ? 0 : ($xmax-$xmin)/($xbin-1);
my $eff_step = ($effbin==1) ? 0 : ($effmax-$effmin)/($effbin-1);
my $macro = "doScanX_my.csh";
for (my $i=0; $i<$nppbin; $i++) {
  for(my $j=0; $j<$kbin; $j++) {
    for (my $k=0; $k<$xbin; $k++) {
      for(my $ki=0; $ki<$effbin; $ki++) {
        my $npp_value = $nppmin + $npp_step * $i;
        my $k_value   = $kmin   + $k_step   * $j;
        my $x_value   = $xmin   + $x_step   * $k;
        my $eff_value = $effmin + $eff_step * $ki;
		my $submit = "submit_condor.pl -s $macro $npp_value $k_value $x_value $eff_value";
		system("$submit");
        print "$submit\n";
      }
    }
  }
}
