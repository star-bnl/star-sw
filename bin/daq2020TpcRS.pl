#! /usr/bin/env perl
use File::Basename;
use Cwd;
use Env;
my $debug = 0;
my $pwd = cwd();
#my $Trigger =  File::Basename::basename($pwd); $Trigger =~ s/daq_//;
#my $Trigger =  File::Basename::basename($pwd); $Trigger =~ s/TpcRS_/daq_/; $Trigger =~ s/\..*//; print "Trigger = $Trigger\n" if ($debug);
#my $glob = "/net/l401/data/scratch1/daq/2020/" . $Trigger . "/st_physics_adc*.daq";  print "glob = $glob\n" if ($debug);
#my $glob = "/net/l401/data/scratch1/fisyak/Tpc/TpcRS/" . $Trigger . "/st_physics_adc*.MuDst.root";  print "glob = $glob\n" if ($debug);
my $Trigger =  File::Basename::basename($pwd); $Trigger =~ s/TpcRS_//; $Trigger =~ s/\..*$//;print "Trigger = $Trigger\n" if ($debug);
my $fNo = 0;
my $glob = "../" . $Trigger . "/*.MuDst.root"; print "glob = $glob\n" if ($debug);
my @globs = glob $glob; print "globs = @globs\n" if ($debug);
foreach my $file (@globs) {
  my $b = File::Basename::basename($file,".daq"); 
#  my $b = File::Basename::basename($file,".MuDst.root"); 
  print "$b\n" if ($debug);
  my $mufile = $b . ".MuDst.root";
  if (-r $mufile) {next;}
  my $pifile = $b . ".picoDst.root";
  if (-r $pifile) {next;}
  my $blafile = $b . ".event.root";
  if (-r $blafile) {next;}
  print "string:$file\n";
  $fNo++;
}
if (! $fNo) {die "Don't have input files\n";}
