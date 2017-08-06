#!/usr/bin/env perl
use File::Basename;
my $mount;
my $disk = "";
my $DISK = "";
if ($#ARGV != 0) {
  print "Usage $0 $mount,  valid options: u, tpc, bnl[D]\n";
  $mount = "bnl";
} else {
  $mount = $ARGV[0];
}
if    ($mount eq 'u'  ) { $DISK = "/direct/star+u"; $disk = "/direct/star/u";}
elsif ($mount eq 'tpc') { $DISK = "/direct/star+subsys+tpc"; $disk = "/direct/star/subsys/tpc";}
elsif ($mount eq 'bnl') { $DISK = "/gpfs01/star/i_bnl"; $disk = "/direct/star/institutions/bnl";}
elsif ($mount eq 'gpfs01') { $DISK = "/direct/gpfs01"; $disk = "/direct/gpfs01";}
elsif ($mount eq 'gpfs02') { $DISK = "/direct/gpfs02"; $disk = "/direct/gpfs02";}
elsif ($mount eq 'gpfs03') { $DISK = "/direct/gpfs03"; $disk = "/direct/gpfs03";}
elsif ($mount eq 'gpfs04') { $DISK = "/direct/gpfs04"; $disk = "/direct/gpfs04";}
else {
  die "Non valid option";
}
if (! $DISK) {die "mount DISK is not defined";}
if (! $disk) {die "mount point disk is not defined";}
if (! -r $disk ) {
  my $cmd = "mkdir -p $disk";
  my $flag = system($cmd);
  print "$cmd\n";
  if ( $flag) {print "failed \n";}
  print "\n";
}
my $myVolName = $mount; #File::Basename::basename($mount);
# fusermount -u /direct/gpfs01
$cmd = "sshfs fisyak\@rftpexp02.rhic.bnl.gov:$DISK $disk";
print "$cmd";
$flag = system($cmd);
if ( $flag) {print " === failed";}
print "\n";


