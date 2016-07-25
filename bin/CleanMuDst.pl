#!/usr/bin/env perl
# find inconsistencies with MuDsts
use File::Basename;
use Cwd;
# print "CleanMuDst\n";
#  CleanMuDst.pl => inconsistent st_physics_15131041_raw_1000014 : 0001 1990 => 1797 7103
#  dir st_physics_15131041_raw_1000014*
#  dir -ltr `grep -l st_physics_15131041_raw_1000014 sched*`
#  grep sched2DD109765836DB465EE874BD00C5B493_2.csh ../../condq.log
#  condor_rm 8160769
#  rm `grep -l st_physics_15131041_raw_1000014 sched*`
#  rm st_physics_15131041_raw_1000014_1797_7103*

my @MuDsts = glob "*.MuDst.root";# print "MuDsts : @MuDsts\n";
my $tag = "";
my $f = 0;
my $l = 0;
my $i = 0;
foreach my $mu (@MuDsts) {
  my $b = $mu;
  $b =~ s/\.MuDst\.root//;#  print "$mu => $b\n";
  my @words = split("_",$b); # print "words: $#words : @words\n";
  my $fC = $words[$#words-1];
  my $lC = $words[$#words];
  my $tagC = $b;
  $tagC =~ s/\_$fC//; 
  $tagC =~ s/\_$lC//; #print "$tagC fC= $fC lC = $lC\n";
  if ($tagC ne $tag) {
    $tag = $tagC;
    $f   = $fC;
    $l   = $lC;
  } else {
    if ($f != $fC and $fC > $l) {
      $l = $lC;
    } else {
#      print "inconsistent $tag : $f $l => $fC $lC\n";
##      my @sched = `grep -l $tag sched*.csh`;# print "@sched\n";
#      my $sched = "";
#      foreach my $s (@sched) {
#	chop($s);
#	if ($sched) {$sched .= "|";}
#	$sched .= $s;
#      }
##      my $sched = join("|", @sched); 
#      print "$sched\n";
#      my $cmd = "egrep '(" . $sched . ")' ../../condq.log";
#      my @jobs = `$cmd`; print "@jobs \n";
##      my @files = glob "$tag" . "*"; print "@files\n";
#      my @list = ();
#      foreach my $job (@jobs) {
#	my ($id) = split $job; 
#	push @list, $id;
#      }
#      my $list = join(" ",@list); print "$list\n";
      $cmd  = "rm " . $b . "*"; print "$cmd\n";
#      die;
    }
  }
  $i++;
#  if ($i > 20) {last;}
}
