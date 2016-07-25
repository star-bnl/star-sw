#! /usr/bin/env perl
use File::Basename;
use Cwd;
my @files = glob "*Fit7.root";
foreach $file (@files) {
  my $root = $file;
  $root =~ s/\.Fit7\./\./;
  my $cmd = "root.exe -q -b lMuDst.C ../$root AddHeader.C";
  print "$cmd\n";
  $status = system($cmd);
#  last;
}
