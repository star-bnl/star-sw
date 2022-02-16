#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $debug = 0;
#my $glob = "/net/l404/data/fisyak/daq/2016/1\*\*/\*/\*adc\*\.daq";# $ARGV[0];
my $glob = $ARGV[0]; print "glob = $glob\n" if ($debug);
my @list = glob $glob; print "$glob => @list\n" if ($debug);
foreach my $fullpath (@list) {
   my $base = File::Basename::basename($fullpath,".daq");
   print "$fullpath => $base => " if ($debug);
 #  if ($rootfile =~ m/$BadFiles/) {next;}
   my $rootfile = $base . ".event.root";
   if (-r $rootfile) {
     print "$rootfile exists\n" if ($debug);
     next;
   }
   my $mufile   = $base . ".MuDst.root";
   if (-r $mufile) {
     print "$mufile exists\n" if ($debug);
     next;
   }
   my $logfile  = $base . "B.log";
   if (-r $logfile) {
     print "$logfile exists\n" if ($debug);
     next;
   }
   print "string:$fullpath\n";
#  last;
}
