#!/usr/bin/env perl
use File::Basename;
use Cwd;
use warnings;
use strict;
my $SourceDir = "/gpfs01/star/subsysg/TFG/Pico/2021/RF/TFG21g/3p85GeV_fixedTarget_2021/";
my $TargetDir = "/gpfs01/star/pwg_tasks/tfg02/2021/RF/TFG21g.B/3p85GeV_fixedTarget_2021/";
my $glob =  $SourceDir . "*/*/*.picoDst.root"; print "glob = $glob\n";
my @Picos = glob $glob; print "Found $#Picos picoDst.root files\n";
my $count = 0;
foreach my $pico (@Picos) {
  my $dst = $pico;
  my $holdst = $dst . ".HOLD";
  $dst =~ s/$SourceDir//; 
  my $target = $TargetDir . $dst;
  print "$pico => $dst => |$target|\n";
  if (-r $target) {
    print "target = $target\n";
    my $cmd = "mv $pico $holdst";
    my $flag = system($cmd);
    if ($flag) {die "$cmd failed";}
    else {print "$cmd is done\n";}
  }
  $count++;
#  if ($count > 10) {die;}
} 
