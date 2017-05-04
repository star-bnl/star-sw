#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $glob = "../*.event.root";
my $pwd = cwd();
if ($#ARGV >= 0) {$glob = $ARGV[0];}
my @Files = glob $glob;
foreach my $file (@Files) {
  my $string = "string:" . $pwd . "/" . $file;
  print "$string\n";
}
