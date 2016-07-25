#!/usr/bin/env perl
use Env;
use File::Basename;
my $glob = shift;
my @files  = glob "$glob"; print "files = @files\n";
#die;
my $i = 0;
foreach my $oldfile (@files) {
  my ($dev, $ino, $mode, $nlink, $uid, $gid, $rdev, $size, $atime, $mtime, $ctime, $blksize, $blocks ) = stat $oldfile;
  my $nosubfiles = int ($size/1.e8 + 0.5); # a subfile per 10e3 events
  print "$oldfile => $size => $nosubfiles\n";
  for (my $j = 0; $j < $nosubfiles; $j++) {
    my $first   = 5000*$j + 1;
    my $last    = 5000*($j + 1);
    my $newfile = File::Basename::basename($oldfile,(".daq"));
    $newfile .= "X" . $first . "X" . $last . "X.daq";
    print "$oldfile => $newfile\n";
    symlink($oldfile,$newfile);
  }
}
#________________________________________________________________________________
__END__;





