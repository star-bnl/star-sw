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
if    ($mount eq 'u'  ) { $DISK = "/direct/star+u"; $disk = "/tmp/fisyak/star/u";}
elsif ($mount eq 'tpc') { $DISK = "/direct/star+subsys+tpc"; $disk = "/tmp/fisyak/star/subsys/tpc";}
elsif ($mount eq 'bnl') { $DISK = "/gpfs01/star/i_bnl"; $disk = "/tmp/fisyak/star/institutions/bnl";}
elsif ($mount eq 'gpfs01') { $DISK = "/direct/gpfs01"; $disk = "/tmp/fisyak/gpfs01";}
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
# sshfs fisyak@rftpexp02.rhic.bnl.gov:/direct/star+u /tmp/fisyak/star/direct/star+u  -oauto_cache,reconnect,defer_permissions,noappledouble,negative_vncache,volname=u
# sshfs fisyak@rftpexp02.rhic.bnl.gov:/gpfs01/star/i_bnl /tmp/fisyak/star/institutions/bnl -oauto_cache,reconnect,defer_permissions,noappledouble,negative_vncache,volname=bnl
# sshfs fisyak@rftpexp02.rhic.bnl.gov:/direct/star+subsys+tpc /tmp/fisyak/star/subsys/tpc -oauto_cache,reconnect,defer_permissions,noappledouble,negative_vncache,volname=tpc
#$cmd = "sshfs fisyak\@rftpexp02.rhic.bnl.gov:$DISK $disk\n";
# sshfs -p 22 username@remoteserver:/webapps/ ~/mountpoint -oauto_cache,reconnect,defer_permissions,noappledouble,negative_vncache,volname=myVolName
# umount -f /tmp/fisyak/star/subsys/tpc
#$cmd = "sshfs fisyak\@rftpexp02.rhic.bnl.gov:$DISK $disk  -oauto_cache,reconnect,defer_permissions,noappledouble,negative_vncache,volname=$myVolName\n";
#$cmd = "sshfs fisyak\@rftpexp02.rhic.bnl.gov:$DISK $disk  -oauto_cache,reconnect,defer_permissions,noappledouble,negative_vncache,volname=$myVolName\n";
$cmd = "sshfs fisyak\@rftpexp02.rhic.bnl.gov:$DISK $disk";
print "$cmd";
$flag = system($cmd);
if ( $flag) {print " === failed";}
print "\n";


