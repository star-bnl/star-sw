#!/usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = cwd();
my @dirs = glob "/star/data06/embed/andrewar/hiroshi/outputs/P08ic/D*/*";
foreach my $dir (@dirs) {
  my $b1 = File::Basename::basename($dir);# print "b1 = $b1\n";
  my $d1 = File::Basename::dirname($dir);# print "d1 = $d1\n";
  my $b2 = File::Basename::basename($d1);# print "b2 => $b2\n";
  my $tag = $b2 . "_" . $b1;
  print "$dir => $tag\n";
  my $script = $tag . ".csh";
  open (Out,">$script") or die "Can't open $script";
  print Out "starver .DEV2\n";
  print Out "cd $pwd\n";
  print Out "root.exe -q -b Load.C 'MuKpi.C+(\"" . $dir . "/*.MuDst.root\",\"" . $tag . ".root\")'\n";
  close (Out);
}
