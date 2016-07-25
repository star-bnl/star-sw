#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = cwd();
#		/star/subsys/tpc/fisyak/reco/2013/MC.SL15.ExB/*.event.root
my @globs = qw(
		/star/subsys/tpc/fisyak/reco/2013/MC.DEV2.ExB/*.event.root
	     );
foreach my $glob (@globs) {
  my @list = glob $glob;
  my $fNo = 0;
  foreach my $line (@list) {
    my $file = File::Basename::basename($line,".event.root");
    my $rootf = $file . ".MuDst.root";
    if (-r $rootf) {next;}
    print "string:$line\n";
#    last;
  }
}
