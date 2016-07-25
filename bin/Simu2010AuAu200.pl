#!/opt/star/bin/perl
use File::Basename;
my $debug = 0;# 1;
my @files;
my $glob = "/star/simu/simu/gstardata/Simu2014/rcf14010/out/*.fzd";
@files = glob $glob;
my $step = 25;
my $nevents = 100;
foreach my $file (@files) {
  my $b = File::Basename::basename($file,".fzd");
  for (my $ev1 = 1; $ev1 < $nevents; $ev1 += $step) {
    my $ev2 = $ev1 + $step - 1;
    my $mudst = $b . "_" . $ev1 . "_" . $ev2 . ".MuDst.root";
    if (-r $mudst) {next;}
    print "string:$file:$ev1:$ev2\n";
    #  last;
  }
}
