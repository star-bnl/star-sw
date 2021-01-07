#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = cwd(); #print "pwd = $pwd\n";
my $FIELD = "";
my $year  = "";
if ($pwd =~ /2019/)    {$year = "2019";}
elsif ($pwd =~ /2020/) {$year = "2020"};
if ($pwd =~ /FF/)    {$FIELD = "FF";}
elsif ($pwd =~ /RF/) {$FIELD = "RF"};
#print "FIELD = $FIELD\n";
if (! $FIELD) {die "Field is not defined";}
#my $glob = "/hlt/cephfs/reco/2019/" . $FIELD . "/*.event.root"; #print "glob = $glob\n";
#my $glob = "/net/l401/data/scratch1/reco/2019/" . $FIELD . "/*.event.root"; #print "glob = $glob\n";
my $glob = "/hlt/cephfs/reco/" . $year . "/.DEV2/" . $FIELD . "/Cosmic/*.event.root"; #print "glob = $glob\n";
my @Files = glob $glob; #print "Files = @Files\n";
my $n = 0;
foreach my $file (@Files) {
  my $bf = File::Basename::basename($file,".event.root");# print "bf = $bf\n";
  my $glob = $bf . "*";
  my @list = glob "$glob"; #print "list = $#list => @list\n";
  if ($#list > -1) {next;}
#  my ($dev,$ino,$mode,$nlink,$uid,$gid,$dev, $size, $atime, $mtim, $ctime, $blksize,$blocks) = stat($file);
#  my $now = time(); #print "now = $now\n";
#  my $dt = $now - $ctime;# print "now = $now, ctime = $ctime, dt = $dt\n";
#  if ($dt < 3600) {next;}
  my $string = "string:" .  $file;
  print "$string\n";
  $n++;
  if ($n >= 500) {last;}
}
