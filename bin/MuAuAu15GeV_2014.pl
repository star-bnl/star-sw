#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = cwd();
my $sample = File::Basename::basename($pwd);
my $glob = "";
if ($sample =~ /AuAu15P14idpT/) {$glob = "/star/data*/reco/production_15GeV_2014/ReversedFullField/P14id/2014/*/*/st_physics_15*.MuDst.root";}
if ($sample =~ /AuAu15BpT/)     {$glob = "/gpfs01/star/scratch/fisyak/reco/2014/AuAu15B/st_physics_15*.MuDst.root";}
if ($sample =~ /AuAu15CpT/)     {$glob = "/gpfs01/star/scratch/fisyak/reco/2014/AuAu15C/st_physics_15*.MuDst.root";}
if ($glob == "") {print "No selection done. Abort\n"; exit 0;}
my @list = glob $glob;
foreach my $line (@list) {
#  my $file = File::Basename::basename($line,".MuDst.root");
#  my $rootf = $file . ".root";
#  if (-r $rootf) {next;}
#  my $logfile = $file . "B.log";
#  if (-r $logfile) {next;}
  print "string:$line\n";
}
