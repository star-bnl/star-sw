#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $glob = "/net/l404/data/fisyak/daq/2016/1\*\*/\*/\*adc\*\.daq";# $ARGV[0];
my @list = glob $glob;# print "$glob => @list";
foreach my $fullpath (@list) {
   my $base = File::Basename::basename($fullpath,".daq");
#   print "$fullpath => $base\n";
 #  if ($rootfile =~ m/$BadFiles/) {next;}
   my $rootfile = $base . ".event.root";
   if (-r $rootfile) {next;}
   my $mufile   = $base . ".MuDst.root";
   if (-r $mufile) {next;}
   my $logfile  = $base . "B.log";
   if (-r $logfile) {next;}
  print "string:$fullpath\n";
#  last;
}
