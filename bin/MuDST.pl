#! /usr/bin/env perl
use File::Basename;
use Cwd;
use File::stat;
my $pwd = cwd();
my $day = File::Basename::basename(File::Basename::dirname($pwd));
my $run =  File::Basename::basename($pwd);
my $dir = "/star/subsys/tpc/fisyak/reco/2014/50M/SL15StiCAKFV/" . $day . "/" . $run;
my $debug = 0;
my %Hash = ();
my @list = glob "$dir" . "/*.MuDst.root";
my $NJobs = 0;
my ($st,$physcs,$adc,$r,$raw,$p,$f1,$l1);
my $file;
foreach my $line (@list) {
  print "string:$line\n";
}
