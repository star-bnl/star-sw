#!/usr/bin/env perl 
use File::Basename;
use Sys::Hostname;
use Cwd;
my $maxEvts = 10;
my $energy  = 200.;
my @beamAZ  = qw( 197 79 ); #gold
my @targAZ  = qw( 197 79 ); #gold
my $inp = "hijev.inp";
open(Out, ">$inp") or die "Can't open $inp";
print Out "
'  ====================================================== '
'  =====         Hijing Control file                ===== '
'  ====================================================== '
' Events                          '  $maxEvts
' Frame/Energy                    '  'CMS'  $energy
' Projectile  type/A/Z            '  'A'  $beamAZ[0] $beamAZ[1]
' Target      type/Z/Z            '  'A'  $targAZ[0] $targAZ[1]
' Impact parameter min/max (fm)   '  0.   999.
' Jet quenching (1=yes/0=no)      '  0
' Hard scattering/pt jet          '  0   2.0
' Max # jets per nucleon (D=10)   '  10
' Set ihpr2(11) and ihpr2(12)     '  1   0
' Set ihpr2(21) and ihpr2(18)     '  1   1
' set B production                '  1.5
' istat=1 old istat>1 new         '  2
";
close(Out);
for (my $iset = 3; $iset <= 100; $iset++) {
  my $cmd = "hijing_382 -run $iset -inp $inp";
  print "$cmd\n";
  my $flag = system($cmd);
  $cmd = "mv evgen.$iset.nt evgen_$iset.nt";
  print "$cmd\n";
  $flag = system($cmd);
#  if (! $flag) {last;}
}
