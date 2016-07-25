#!/usr/bin/env perl 
use File::Basename;
use Sys::Hostname;
use Cwd;
my $dir = "Pass217/TpcSsd/RFC";
if ($#ARGV >= 0) {
  $dir = $ARGV[0];
  print "watch $dir\n";
}
#my $checkjobs = "bjobs -l -q star_cas_prod | grep $bname";
my $checkjobs = "bjobs -q star_cas_prod | wc -l";
#my $checkjobs = "bjobs -q star_cas_short | wc -l";
#my $checkjobs = "bjobs -q star_cas_mem | wc -l";
my $cmd;
for (;;) {
  my $log = `$checkjobs`; print "$log";
  if ($log <= 0) {last;}
  sleep 60;
}
print "Done! with StEvent files\n";
$cmd = "hsumT.pl '" . $dir . "/Event*.root'"; print "$cmd\n";
$flag = system($cmd); 
exit 0; 

