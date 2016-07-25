#! /usr/bin/env perl
use File::Basename;
use File::Find;
use Cwd;
@ARGV = ('.') unless @ARGV;
#sub process_file {
#  my ($dev,$ino,$mode,$nlink,$uid,$gid,$dev, $size, $atime, $mtim, $ctime, $blksize,$blocks) = stat($_);
#  printf("%10i %s\n",$size, $_);
#}
#find(\&process_file, @ARGV);
foreach my $dir (@ARGV) {
  opendir( DIR, $dir ) or die "Can't open $dir\n";
  my @fileS = readdir DIR;
  closedir DIR;
  foreach my $sub_dir (@fileS) {#
    my ($dev,$ino,$mode,$nlink,$uid,$gid,$dev, $size, $atime, $mtim, $ctime, $blksize,$blocks) = stat($file);
    printf("%10i %s\n",$size, $file);
  }
}
