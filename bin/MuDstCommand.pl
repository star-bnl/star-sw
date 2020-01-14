#! /usr/bin/env perl
use File::Basename;
#print "ARGV[0] = $ARGV[0]\n";
#my @list = glob $ARGV[0]; 
#my @list = @ARGV; 
my @list = ();
my $debug = 0;
#my $MUDST = "/net/l404/data/fisyak/reco/2016/AuAu200_adc";
my $MUDST = "/gpfs01/star/subsys-tpc/fisyak/reco/2019/19GeV_TFG19e5X";
if (-d $MUDST) {
  @list = glob $MUDST . "/*.MuDst.root";
  print "list = @list\n" if ($debug);
} else {
#  $MUDST = "/gpfs02/eic/ayk/STAR/reco/MuDst/AuAu_200_production_2016/ReversedFullField/P16ij/2016";
#  if (-d $MUDST) {
#    @list = glob $MUDST . "/*/*/*.MuDst.root";
#  }
  print "MUDST = $MUDST does not exist\n";
  exit 1;
}
my $no = 0;
foreach my $line (@list) {
  my $mudst = File::Basename::basename($line);
  if (-r $mudst) {next;}
  print "string:$line\n";
  $no++;
#  if ($no >= 10) {last;}
}
