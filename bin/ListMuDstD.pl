#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = cwd();
if ($#ARGV >= 0) {
  $pwd = $ARGV[0];
}
my $dir = File::Basename::basename($pwd);# print "dir = $dir\n";
my @list = glob $pwd . "/*.MuDst.root";
my @dirs = ();
foreach my $f (@list) {
  my $d = File::Basename::dirname($f);
  my $found = 0;
  foreach my $dd (@dirs) {
    if ($d eq $dd) {$found = 1; last;}
  }
  if ($found) {next;}
  push @dirs, $d;
}
foreach my $d (@dirs) {
  my $b = File::Basename::basename($d);
  my $file = "Mu" . $b. ".root";
  if (-r $file) {next;}
  print "string:$d\n";
}
