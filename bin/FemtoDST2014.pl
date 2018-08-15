#! /usr/bin/env perl
use File::Basename;
use Cwd;
use File::stat;
my $debug = 0;
my $pwd = cwd();
my $trig = File::Basename::basename(File::Basename::dirname(File::Basename::dirname($pwd)),new); print "trig = $trig\n" if ($debug);
my $day = File::Basename::basename(File::Basename::dirname($pwd)); print "day = $day\n" if ($debug);
my $run =  File::Basename::basename($pwd);                         print "run = $run\n" if ($debug);
#my $dir = "/gpfs01/star/subsys-tpc/fisyak/reco/2014/50M/SL15StiCAKFV/" . $day . "/" . $run;
#my $dir = "/net/l401/data/scratch2/fisyak/MuDst/2016/" . $day . "/" . $run;
#my $dir = "/gpfs01/star/pwg/fisyak/MuDst/2016/" . $day . "/" . $run; print "dir = $dir\n" if ($debug);
#$dir = "./";
#my $dir = "/gpfs02/eic/ayk/STAR/reco/MuDst/2010/" . $day . "/" . $run; print "dir = $dir\n" if ($debug);
#my $dir = "/gpfs02/eic/ayk/STAR/reco/MuDst/2016/" . $day . "/" . $run; print "dir = $dir\n" if ($debug);
my $dir = "/star/u/fisyak/Pico/2014/" . $trig . "/" . $day . "/" . $run; print "dir = $dir\n" if ($debug);
my %Hash = ();
my @list = glob "$dir" . "/*.picoDst.root";
my $NJobs = 0;
my @femto = glob "*.femtoDst.root"; print "no. of femto $#femto\n" if ($debug);
if ($#femto >= 0) { die "No.jobs";}
foreach my $line (@list) { 
  print "$line\n" if ($debug);
  my $key = File::Basename::basename($line,".picoDst.root");
  print "string:$dir/$key.picoDst.root\n";
  $NJobs++;
}
if (! $NJobs) {die "No.jobs";}
