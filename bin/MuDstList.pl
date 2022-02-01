#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = Cwd::cwd();
my @fileList = glob "./*MuDst.root";
foreach my $file (@fileList) {
#  my $string = "string:" . $pwd . "/" . $file;
  my $string = $pwd . "/" . $file;
  print "$string\n";
}
