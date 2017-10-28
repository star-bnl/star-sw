#! /usr/bin/env perl
use File::Basename;
#print "ARGV[0] = $ARGV[0]\n";
#my @list = glob $ARGV[0]; 
#my @list = @ARGV; 
my @list = glob "/net/l404/data/fisyak/reco/2016/AuAu200_adc/*.MuDst.root";
my $no = 0;
foreach my $line (@list) {
  my $mudst = File::Basename::basename($line);
  if (-r $mudst) {next;}
  print "string:$line\n";
  $no++;
  if ($no >= 400) {last;}
}
