#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $debug = 0;
my $glob = "../*.event.root";
my $pwd = cwd();
if ($#ARGV >= 0) {$glob = $ARGV[0];}
my @Files = glob $glob;
my $No = 0;
foreach my $file (@Files) {
  my $bf = File::Basename::basename($file,".event.root");# print "bf = $bf\n";
  my $glob = $bf . "*.root";
  my @list = glob "$glob"; print "list = $#list => @list\n" if ($debug);
  if ($#list > -1) {next;}
  $No++;
#  if ($No%25 != 1) {next;}
#  my ($dev,$ino,$mode,$nlink,$uid,$gid,$dev, $size, $atime, $mtim, $ctime, $blksize,$blocks) = stat($file);
#  my $now = time(); #print "now = $now\n";
#  my $dt = $now - $ctime;# print "now = $now, ctime = $ctime, dt = $dt\n";
#  if ($dt < 3600) {next;}
  my $string = "string:" . $pwd . "/" . $file;
  print "$string\n";
}
