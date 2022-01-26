#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $file = "DeadFEE.listSorted";
open(In, $file) or die "Can't open $file";
my $line;
my $tagOld = "";
my $run1 = 0;
my $run2 = 0;
while ($line = <In>) {
#  print $line;
  my ($tag,$run,$dummy1,$dummy2) = split('/',$line);
  $run =~ s#\*##g;
#  my $tag = $line; $tag =~ s/\/\*.*$//;
#  my $run = $line; $run =~ s/$tag//;
#  print "tag = $tag run\n";
  if ($tagOld eq "") {
    $run1 = $run;
    $run2 = $run;
    $tagOld = $tag;
#    print "run1 = $run1, run2 = $run2, tagOld = $tagOld\n"
  } elsif ($tag ne $tagOld) {
    if ($run1 != 0) {
#      print "$tagOld /* $run1 - $run2 */\n"; 
      print " /* $run1 - $run2 */ $tagOld\n"; 
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
      print " /* $run1 - $run2 */ $tagOld\n"; 

