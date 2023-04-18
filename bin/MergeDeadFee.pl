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
my $iold = 1;
my $nruns = 0;
#my @Runs = glob "2*.list"; print "Found $#Runs files\n";
my @Runs = `ls -1d 2*.list | sed -e 's/\.list//'`; if ($debug) { print "Found $#Runs files\n";}
my %RunIndex = ();
my $index = 0;
foreach my $run (@Runs) {
  chop($run);
  $RunIndex{$run} = $index;
#  if ($debug) {print "RunIndex[$run] = $index\n";}
  $index++;
}
while ($line = <In>) {
  print $line if ($debug);
  #  my ($tag,$run,$dummy1,$dummy2) = split('/',$line); print "tag = $tag,run = $run\n" if ($debug);
  chop($line);
  my @words = split ",", $line; 
  #   if ($debug) {
  #     for (my $i = 0; $i <= $#words; $i++) {print "$i: |$words[$i]|\t";}
  #     print "\n";
  # }
  my $tag = $words[0] . ", -1, -1, -1," . $words[4] . "," .  $words[5];
  if ($words[5] > 0) {$tag = $words[0] . ", -1, -1, -1,-1," .  $words[5];}
  my ($dum,$run,$dum)  = split(" ",$words[6]);
  print "tag = $tag run = $run\n" if ($debug);
  if ($run < 2000000) {$nruns = 0; next;}
  if ($tagOld eq "") {
    $run1 = $run;
    $run2 = $run;
    $tagOld = $tag;
    $iold = $RunIndex{$run}; if ($debug) {print "run = $run => iold = $iold\n";}
    $nruns = 1;
    print "run1 = $run1, run2 = $run2, tagOld = $tagOld, iold = $iold\n" if ($debug);
  } elsif ($tag ne $tagOld) {
    if ($run1 != 0) {
#      print "$tagOld /* $run1 - $run2 */\n"; 
      print " /* $run1 - $run2 */ $tagOld /* $flag nruns = $nruns iold = $iold */\n"; 
    }
    $run1 = $run;
    $run2 = $run;
    $tagOld = $tag;
    $iold = $RunIndex{$run}; if ($debug) {print "run = $run => iold = $iold\n";}
    $nruns = 1;
  } else {
    my $inew = $RunIndex{$run}; if ($debug)       {print "run = $run => iold = $iold inew = $inew nruns = $nruns\n";}
    if ($inew - $iold == 1) {
      $nruns++;
      $iold = $inew;
      $run2 = $run;
      $tagOld = $tag;
      #    print "run1 = $run1, run2 = $run2, tagOld = $tagOld\n"
    } else {
      print " /* $run1 - $run2 */ $tagOld /* $flag nruns = $nruns*/\n"; 
      $run1 = $run;
      $run2 = $run;
      $tagOld = $tag;
      $iold = $RunIndex{$run}; if ($debug) {print "run = $run => iold = $iold\n";}
      $nruns = 1;
    }
  }
}
#print "$tagOld /* $run1 - $run2 */\n"; 
#      print " /* $run1 - $run2 */ $tagOld\n"; 
if ($nruns > 1) {
print " /* $run1 - $run2 */ $tagOld /* $flag */\n"; 
}
