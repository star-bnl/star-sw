#!/usr/bin/env perl
my $Production = "P04ii";
if ($ARGV[0]) {$Production = $ARGV[0];}
my $glb =  "/star/data*/reco/*/*/" . $Production  . "/*/*/*0.event.root";
my @Files = glob "$glb"; 
print "Files $glb:  $#Files\n";
foreach my $file (@Files) {
  my ($dev, $ino, $mode, $nlink, $uid, $gid, $rdev, $size, $atime, $mtime, $ctime, $blksize, $blocks ) = stat $file;
  my ($sec,$min,$hour,$mdat,$mon,$year,$wday,$yday,$isdst) = localtime($ctime);
  my $date = scalar localtime($ctime);
  print "$file $size $date \n";
}
