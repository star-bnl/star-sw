#!/usr/bin/env perl
use File::Basename;
if ($#ARGV < 0) {
  print "Usage:  $0 run_number1 [run_number2]\n"; 
  exit;
}
my $debug = 0;
my $run1 = $ARGV[0]; $run1 =~ s/,//;
my $run2 = $run1; 
if ($#ARGV  == 1) {$run2 = $ARGV[1];}
if ($#ARGV  == 2) {$run2 = $ARGV[2];}
$run2 =~ s/,//;
my @files = glob "*/2*.root";
my $List = "";
foreach my $file (@files) {
  my $run = $file;
  $run =~ s/.*\///;
  $run =~ s/_.*//; #print "$file => $run\n";
  if ($run < $run1 || $run > $run2) {next;}
#  $List .= " *GeV*/" . $run . "*.root";
  $List .= " */" . $run . "*.root";
}
print "root.exe $List 'CheckPads.C+(1)'\n";
