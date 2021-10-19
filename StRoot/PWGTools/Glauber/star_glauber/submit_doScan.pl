#! /usr/bin/perl

#############################################################################
#the follow are the parameters obtained in Run11 27GeV AuAu mb data
#	const Double_t npp = 1.385 ; // default npp
#	//  const Double_t npp = 1.390 ; // default npp
#	mNpp = npp + GetNppError(npp) ;
#	mK   = 1.65 ;
#	mX   = 0.12 + xError ;
#	mEfficiency  = 0.14 ;
#	mTriggerBias = 1.00 ;
#############################################################################


##############################################################################
# to scan for the best parameters, we can set the #bins and range for each parameter
#npp1.270-1.270_k1.700-1.700_x0.150_0.150_eff0.090.
my $nppbin = 5;
my $nppmin = 2.0;
my $nppmax = 3.0;

my $kbin = 5;
my $kmin = 1.5;
my $kmax = 5.0;

my $xbin = 5;
my $xmin = 0.10;
my $xmax = 0.25;

my $effbin = 5;
my $effmin = 0.08;
my $effmax = 0.20;
#############################################################################


#############################################################################
#after the scan, find the best parameters, and fix them as follows:
#and please change the input RefmultCor to be the one after the reweight (Refmult_LumVzCor/hRatio)
#npp1.270-1.270_k1.700-1.700_x0.150_0.150_eff0.090.
#
#my $nppbin = 1;
#my $nppmin = 1.27;
#my $nppmax = 1.27;
#
#my $kbin = 1;
#my $kmin = 1.80;
#my $kmax = 1.80;
#
#my $xbin = 1;
#my $xmin = 0.16;
#my $xmax = 0.16;
#
#my $effbin = 1;
#my $effmin = 0.11;
#my $effmax = 0.11;
#############################################################################

my $nTotJobs = $nppbin * $kbin * $xbin * $effbin;

print "number of jobs will be submitted: $nTotJobs\n";

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
