#!/usr/bin/env perl
if ($#ARGV < 0) {exit 0;}
my $tag = $ARGV[0];
my $cmd = "ls -1 " . $tag . "*==*";
my @list = `$cmd`;
my $NoVers = $#list + 1;
print "NoVers  = $NoVers\n";
print " @list\n";
my $base = $tag . "Config==1";
if (! -r $base) {die "There is no $base\n";}
for (my $i=2; $i<=$NoVers; $i++) {
  my $cur = $tag . "Config==" . $i;
  if (! -r $cur) {die "There is no $cur\n";}
  my $out = $base ."+". $i;
#  my $cmd = "diff -I '^#' -D__" . $cur . "__ " . $base . " " . $cur . " > " .$out . ";";
#  my $cmd = "diff -bBd --exclude=pattern='^#' -D__" . $cur . "__ " . $base . " " . $cur . " > " .$out . ";";
#  my $cmd = "diff -bBd -I '^#' -D__" . $cur . "__ " . $base . " " . $cur . " > " .$out . ";";
#  my $cmd = "diff -bBd --ifdef=__" . $cur . "__ " . $base . " " . $cur . " > " .$out . ";";
#  my $cmd = "diff -bBd -I '^#' --ifdef=" . $cur . " " . $base . " " . $cur . " > " .$out . ";";
  my $cmd = "diff -bBd --ifdef=" . $cur . " " . $base . " " . $cur . " > " .$out . ";";
# my $cmd = "diff -bBd  --ifdef=" . $cur . " " . $base . " " . $cur . " > " .$out . ";";
  print "$cmd\n"; 
  system($cmd);
  $base = $out;
}
