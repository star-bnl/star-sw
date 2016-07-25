#! /usr/bin/env perl
use File::Basename;
use Cwd;
if ($#ARGV < 0) {
  print "Usage: $0 path_to_dir_with_event_root_files'\n";
  exit 0;
} 

#		/star/subsys/tpc/fisyak/reco/2013/MC.SL15.ExB/*.event.root

my @globs = @ARGV;
foreach my $glob (@globs) {
  my @list = glob $glob . "/*event.root";
  my $fNo = 0;
  foreach my $line (@list) {
    my $file = File::Basename::basename($line,".event.root");
    my $rootf = $file . ".MuDst.root";
    if (-r $rootf) {next;}
    print "string:$line\n";
#    last;
  }
}
