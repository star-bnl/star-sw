#!/usr/bin/env perl
use File::Basename;
use Cwd;
my $daq = "";
my $reduction = 0;
my $pwd = cwd();
if ($#ARGV < 0) {
  print "Usage: cd /hlt/cephfs/reco/2021/RF/TFG21e/OO_200GeV_2021/ $0 4\n";
  exit;
} else {
  $reduction = $ARGV[0];
}
# if ($reduction <= 1 || $reduction > 10) {die "Illegal reduction factor = $reduction";}
# my @List = glob "*/*/hlt*.event.root";
# if ($#List < 100) {die "Only $#List hlt*event.root files found in #pwd"; exit;}
# print "# Reduce no. $#List files by a factor of $reduction\n";
# my $n = 0;
# foreach my $file (@List) {
#   $n++;
#   $i = $n%$reduction;
#   if ($i == 1) {
#     print "#keep $file\n";
#     next;
#   }
#   print "rm $file\n";
# }
my @dirList = glob "*/*"; print "dirlist = @dirlist\n";
foreach my $dir (@dirlist) {
  my @list = glob $dir . "/*event.root"; print "list = @list\n";
}
