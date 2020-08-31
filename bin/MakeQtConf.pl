#!/usr/bin/env perl 
use File::Basename;
use Sys::Hostname;
use Cwd;
my @qtdirs = qw( /cvmfs/star.sdcc.bnl.gov/TFG/opt/star/sl73_gcc485/qt4
		 /cvmfs/star.sdcc.bnl.gov/TFG/opt/star/sl73_x8664_gcc1010/qt4
		 /cvmfs/star.sdcc.bnl.gov/TFG/opt/star/sl73_x8664_gcc485/qt4
		 /cvmfs/star.sdcc.bnl.gov/TFG/opt/star/sl73_x8664_gcc531/qt4
		 /cvmfs/star.sdcc.bnl.gov/TFG/opt/star/sl73_x8664_gcc630/qt4
		 /cvmfs/star.sdcc.bnl.gov/TFG/opt/star/sl73_x8664_gcc631/qt4
		 /cvmfs/star.sdcc.bnl.gov/TFG/opt/star/sl73_x8664_gcc7/qt4
		 /cvmfs/star.sdcc.bnl.gov/TFG/opt/star/sl73_x8664_gcc8/qt4
		 /cvmfs/star.sdcc.bnl.gov/TFG/opt/star/sl73_x8664_gcc920/qt4
		 /cvmfs/star.sdcc.bnl.gov/TFG/opt/star/sl73_x8664_gcc930/qt4
		 /cvmfs/star.sdcc.bnl.gov/TFG/opt/star/sl73_x8664_gcc9/qt4
	      );
foreach my $qtdir (@qtdirs) {
  my $qtconf = $qtdir . "/bin/qt.conf";
  print "Create $qtconf\n";
  open (OUT,">$qtconf") or die "Can't open $qtconf";
  print OUT "[Paths]\n";
  print OUT" Prefix = $qtdir\n";
  print OUT "Translations = i18n\n";
  close(OUT);
}
