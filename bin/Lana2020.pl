#! /usr/bin/env perl
use File::Basename;
use Cwd;
#my @list = glob "/gpfs01/star/scratch/fisyak/daq/2017/*/*/st_laser*.daq";
my @list = ();
if (-d "/hlt/cephfs/daq/2019/" or -d  "/hlt/cephfs/daq/2020/") {
  @list = glob "/hlt/cephfs/daq/2019/*/*/*laser*.daq /hlt/cephfs/daq/2020/*/*/*laser*.daq";
} else {
  @list = glob "/star/data03/daq/2019/*/*/st_laser*.daq";
}
#print "list = @list\n";
my $NoJobs = 0;
foreach my $fullpath (@list) {
  my $file = File::Basename::basename($fullpath,".daq");
  my $rootfile = "./" . $file . ".laser.root";
  if (-r $rootfile) {next;}
#  my $glob = "./*/" . $file . ".laser.root";
#  my @rootfiles = glob $glob;# print "glob = $glob rootfiles = @rootfiles\n";
#  if ($#rootfiles >= 0) {next;}
  $rootfile = $file;
  $rootfile =~ s/_adc//;
  $rootfile .= "_adc.laser.root";
  if (-r $rootfile) {next;}
  print "string:$fullpath\n";
  $NoJobs++;
}
if (! $NoJobs) {die "No jobs";}
