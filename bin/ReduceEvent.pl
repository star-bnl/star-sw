#!/usr/bin/env perl
use File::Basename;
use Cwd;
my $daq = "";
my $reduction = 0;
my $pwd = cwd();
# if ($#ARGV < 0) {
#   print "Usage: cd /hlt/cephfs/reco/2021/RF/TFG21e/OO_200GeV_2021/ $0 4\n";
#   exit;
# } else {
#   $reduction = $ARGV[0];
# }
#  if ($reduction <= 1 || $reduction > 50) {die "Illegal reduction factor = $reduction";}
#  my @List = glob "*/*/hlt*.event.root";
#  if ($#List < 100) {die "Only $#List hlt*event.root files found in #pwd"; exit;}
#  print "# Reduce no. $#List files by a factor of $reduction\n";
#  my $n = 0;
#  foreach my $file (@List) {
#    $n++;
#    $i = $n%$reduction;
#    if ($i == 1) {
#      print "#keep $file\n";
#      next;
#    }
#    print "rm $file\n";
#  }

#  my @dirList = glob "???/2*"; print "dirlist = @dirlist\n";
#  foreach my $dir (@dirlist) {
#    my @list = glob $dir . "/*event.root"; print "list = @list\n";
#  }
my $debug = 1;
my $dir = ".";
opendir( DIR, $dir ) or next; # die "Can't open $dir\n";
my @subdirS = readdir DIR; # print "subdirS = @subdirS\n" if ($debug);
closedir DIR;
foreach my $sub_dir (@subdirS) { 
#  print "$sub_dir\n";
  next if $sub_dir !~ /\d\d\d/;
  opendir( DIR, $sub_dir ) or next; # die "Can't open $sub_dir\n";
  my @subdirS = readdir DIR; # print "subdirS = @subdirS\n" if ($debug);
  closedir DIR;
  foreach my $ssdir (@subdirS) {
    next if  $ssdir !~ /^2.*/;
    my $glob = $dir . "/" .  $sub_dir . "/" . $ssdir . "/*event.root";
    my @flist = glob $glob; 
    next if ($#flist) <= 0;
#    print "$glob => $#flist => @flist\n";
    my $keep = 0;
    foreach my $file (@flist) {
      if ($keep > 0) {
	print "rm $file\# $keep from $#flist\n";
      } else {
	print "# keep $file\n";
	$keep++;
      }
    }
  }
}
