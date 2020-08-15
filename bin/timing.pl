#! /usr/bin/env perl
use File::Basename;
use Cwd;
# my $file = "/afs/rhic.bnl.gov/star/users/fisyak/bin/timing.list";
#  my $file = "/star/u/fisyak/bin/timing.list";
#  open(In, $file) or die "Can't open $file";
#  while ( my $it = <In>) {
#    my @words = split(":",$it);
#    my $log = $words[0] . "_" . $words[1] . "_" . $words[2] . "_" . $words[3] . "/st_physics_11029020_raw_1030002.log";
#    if (-r $log) {next;}
#    print "string:$it";
#  }
 # # Setup directory structure for timing
  my @gccs = qw( .sl73_gcc485
  	       .sl73_x8664_gcc485
  	       .sl73_x8664_gcc531
  	       .sl73_x8664_gcc631
  	       .sl73_x8664_gcc7
  	       .sl73_x8664_gcc8
  	       .sl73_x8664_gcc9
  	       .sl73_x8664_gcc1010);
#my @gccs = qw( .sl73_x8664_gcc1010);
my $debug = 0;
foreach my $gver (@gccs) {
  my @items = split '_', $gver;
  my $N = $#items; 
  if ($debug) {
    print "N = $N\n";
    foreach my $t (@items) {
      print "t = $t\n";
    }
  } 
  my $gccv = $items[$N];
  if ($gccv eq 'gcc1010') {$gccv = "gcc/10.1.0";}
  my $bits = "32b";
  if ($gver =~ /x8664/) {$bits = "64b";}
#  my @vers = qw(.DEV2 DEV6);
  my @vers = qw(.DEV2);
  if ($gccv ne 'gcc631') {@vers = qw(.DEV2);}
  foreach my $ver (@vers) {
    print "ver = $ver, gccv = $gccv\n" if ($debug);
    foreach my $deb (qw(debug opt)) {
      my $dir = "rc" . $ver . $gver . "_" . $deb;
       print "string:$dir:$ver:$gccv:$bits:$deb\n";
    }
  }
}

