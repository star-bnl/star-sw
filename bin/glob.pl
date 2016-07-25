#! /usr/bin/env perl
use File::Basename;
use Cwd;

my @Files = glob "$ARGV[0]";
foreach my $file (@Files) {
    my ($dev, $ino, $mode, $nlink, $uid, $gid, $rdev, $size, $atime, $mtime, $ctime, $blksize, $blocks ) = stat $file;
    print "$file : dev,  ino,  mode,  nlink,  uid,  gid,  rdev,  size,  atime,  mtime,  ctime,  blksize,  blocks\n";
}
