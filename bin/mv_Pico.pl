#!/usr/bin/env perl 
use File::Basename;
use File::Copy;
use Sys::Hostname;
use Cwd;
my $pwd = Cwd::cwd(); print "pwd = $pwd\n";
my @picos = `ls -1d *.picoDst.root`; # print "picos = @picos\n";
foreach my $file (@picos) {
  chomp($file);
  my $bfile = File::Basename::basename($file,".picoDst.root");
  my ($dummy,$run) = split '_', $bfile;
  my $day = int ($run/1000) - 20000;
  print "$file => $bfile => $run => $day\n";
  if (! -d $day) {mkdir $day;}
  my $dir = $day . "/" . $run;
  if (! -d $dir) {mkdir $dir;}
  my $cmd = "mv " . $bfile . "* " . $dir . "/";
  print "cmd = $cmd\n";
  my $flag = system($cmd);
  if ($flag) {die "Can't move $bfile";}
#  die;
}
