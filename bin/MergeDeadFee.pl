#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $file = "";
my $debug = 0;
if ($#ARGV < 0) {
  print "Usage $0 DeadFEE.listSorted\n";
  exit 0;
} else {
  $file = $ARGV[0];
}
my $flag = "Dead";
$flag = "Alive" if ($file =~ /Alive/);
print "file = $file, flag = $flag\n" if ($debug);
open(In, $file) or die "Can't open $file";
my $line;
my $tagOld = "";
my $run1 = 0;
my $run2 = 0;
while ($line = <In>) {
  print $line if ($debug);
  my ($tag,$run,$dummy1,$dummy2) = split('/',$line); print "tag = $tag,run = $run\n" if ($debug);
  $run =~ s#\*##g;
  if ($run < 2000000) {next;}
#  my $tag = $line; $tag =~ s/\/\*.*$//;
#  my $run = $line; $run =~ s/$tag//;
  print "tag = $tag run = $run\n" if ($debug);
  if ($tagOld eq "") {
    $run1 = $run;
    $run2 = $run;
    $tagOld = $tag;
    print "run1 = $run1, run2 = $run2, tagOld = $tagOld\n" if ($debug);
  } elsif ($tag ne $tagOld) {
    if ($run1 != 0) {
#      print "$tagOld /* $run1 - $run2 */\n"; 
      print " /* $run1 - $run2 */ $tagOld /* $flag */\n"; 
      $run1 = $run;
      $run2 = $run;
      $tagOld = $tag;
    }      
  } else {
    $run2 = $run;
    $tagOld = $tag;
#    print "run1 = $run1, run2 = $run2, tagOld = $tagOld\n"
  }
}
#print "$tagOld /* $run1 - $run2 */\n"; 
#      print " /* $run1 - $run2 */ $tagOld\n"; 
print " /* $run1 - $run2 */ $tagOld /* $flag */\n"; 

