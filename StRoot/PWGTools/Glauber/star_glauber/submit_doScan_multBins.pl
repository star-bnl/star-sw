#! /usr/bin/perl

my $nppbin = 81;
my $nppmin = 1.8;
my $nppmax = 2.6;

my $kbin = 1;
my $kmin = 2.0;
my $kmax = 2.0;

my $xbin = 21;
my $xmin = 0.05;
my $xmax = 0.25;

my $effbin = 21;
my $effmin = 0.05;
my $effmax = 0.25;

my $npp_step = ($nppbin==1) ? 0 : ($nppmax-$nppmin)/($nppbin-1);
my $eff_step = ($effbin==1) ? 0 : ($effmax-$effmin)/($effbin-1);
my $macro = "doScanX_multBins.csh";
for (my $i=0; $i<$nppbin; $i++) {
  for(my $ki=0; $ki<$effbin; $ki++) {
        my $npp_value = $nppmin + $npp_step * $i;
        my $eff_value = $effmin + $eff_step * $ki;
		my $submit = "submit_condor.pl -s $macro $npp_value $kbin $kmin $kmax $xbin $xmin $xmax $eff_value";
		system("$submit");
        print "$submit\n";
  }
}
