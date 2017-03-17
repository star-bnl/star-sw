#! /usr/bin/env perl
use File::Basename;
my %ARG = (file => 'MudEdx1.root',
           Hist => 'pTPhiPiDz',
           mode => 'GP',
	   nx   => '100',
          );
while (@ARGV) {
  $_ = shift @ARGV;
  if ($_ =~ /=/) { my($key, $val) = /([^=]*)=(.*)/; $ARG{$key} = $val;}
}
while (my ($key,$value) = each %ARG) {
  print  "$key=$value\n";
}
if (! -r $ARG{file}) {exit 1;}
for (my $ix = 1; $ix <= $ARG{nx}; $ix++) {
  my $base = File::Basename::basename($ARG{file},".root");
  $base .= $ARG{Hist} . "_" . $ix;
  my $Script = $base; $Script .= ".csh";# print "Script = $Script\n";
  open(Script,">$Script") or die "Can't open $Script";
  my $cmd =  "setenv STARFPE NO; setenv NODEBUG yes; setup 64b; root.exe -q -b -x lBichsel.C $ARG{file} 'dEdxFit.C+(\"$ARG{Hist}\",\"GP\",\"R\",$ix)' >& $base.log";
#  print "$cmd\n";
  print Script "$cmd\n";
  close(Script);
}
