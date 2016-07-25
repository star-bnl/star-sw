#!/opt/star/bin/perl
use File::Basename;
my $debug = 0;# 1;
my @files;
my $glob = "/star/simu/simu/gstardata/Simu2010/JWRequest/output/rcf10100_*_200evts_Wplus_enu.fzd";
@files = glob $glob;
foreach my $file (@files) {
  print "string:$file\n";
#  last;
}
