#! /usr/bin/env perl
use File::Basename;
use Cwd;
use Env;
my $production =  File::Basename::basename(cwd());
my $glob = "/hlt/cephfs/daq/2020/" . $production . "_2020/*adc*.daq";
my @files = glob $glob;
foreach my $file (@files) {
  print "string:$file\n";
}
