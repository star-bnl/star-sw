#!/opt/star/bin/perl
use File::Basename;
my $debug = 0;# 1;
my @files;
my $glob = "/star/simu/simu/gstardata/Simu2012/rcf12010/out/*_10evts_W*_enu.fzd";
@files = glob $glob;
foreach my $file (@files) {
  my $rootf = File::Basename::basename($file,"fzd");
  $rootf .= "MuDst.root";
#  print "$file => $rootf\n";
  if (-r $rootf) {next;}
  print "string:$file\n";
#  last;
}
