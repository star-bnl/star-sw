#!/usr/bin/env perl
use strict;
my $line;
foreach my $file (@ARGV) {
  if ($file !~ /^st_physics_/) {next;}
  my @w = split('_',$file);
  print "$file";
  for (my $i = 0; $i <= $#w; $i++) {
#    print "\t$i:$w[$i]";
    if ($w[$i] =~ m/\./) {
#      print " w[i] = $w[$i] ";
      my @u = split('\.',$w[$i]);
      $u[0] = sprintf("%04i",$u[0]);
      $w[$i] = join(".",@u);
#      print "-> w[$i] = $w[$i] ";
      $w[$i-1] = sprintf("%04i",$w[$i-1]);
#      print "\t=====> :$w[$i-1] $w[$i]";
    }
  }
  my $newfile = join("_",@w);
  if ($file eq $newfile) {print "\n"; next;}
  print "\t => $newfile\n";
  my $status = rename $file, $newfile;
  if (! $status) {last;}
#  my $cmd = "cp -p $file $newfile";
#  my $flag = system($cmd);
#  if ($flag) {die "$cmd failed";}
}
