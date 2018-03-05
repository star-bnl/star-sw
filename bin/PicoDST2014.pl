#! /usr/bin/env perl
use File::Basename;
use Cwd;
use File::stat;
my $pwd = cwd();
my $debug = 0;
open(In,"MuDst.list") or die "Can't open MuDst.list";
my $line;
while ($line = <In>) {
  chomp($line);
  my $dir = File::Basename::dirname($line);
  my $key = File::Basename::basename($line,".MuDst.root");
  my $rootf = $key . ".picoDst.root"; print "dir = $dir, key = $key,rootf = $rootf\n" if ($debug);
  if (-r $rootf) {next;}
  $NJobs++;
  print "string:$dir;$key\n";
}
close(In);
if (! $NJobs) {die "No.jobs";}
