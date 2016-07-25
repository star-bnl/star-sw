#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = cwd();
my $dir = File::Basename::basename($pwd);# print "dir = $dir\n";
my @list = glob "*.tree.root";
foreach my $f (@list) {
  my $b = File::Basename::basename($f,".tree.root");
  my $file = $b. ".plot.root";
  if (-r $file) {next;}
  print "string:$pwd/$f\n";
}
