#!/usr/bin/env perl
use File::Basename;
use Sys::Hostname;
use Cwd;
my @Files = ();
if ($#ARGV == 0) {
  @Files = glob $ARGV[0];
} else {
  foreach my $arg (@ARGV) {
    push @Files, $arg;
  }
}
print "Found $#Files files\n";
my $now = time();
foreach my $file (@Files) {
  my ($dev,$ino,$mode,$nlink,$uid,$gid,$dev, $size, $atime, $mtim, $ctime, $blksize,$blocks) = stat($file);
#  print "file $file size $size ctime $ctime now $now\n";
  if ($size < 10000000) {print "$file is too small $size --- skipped\n"; next;}
  my $dt = $now - $ctime;
  if ($dt < 3600) {print "$file is too young $dt --- skipped\n"; next};
  my $MuDst = File::Basename::basename($file);
  $MuDst =~ s/\.event/\.MuDst/;
  my $log = $MuDst;
  $log =~ s/\.root//;
  $log =~ s/st_physics_//;
  $log =~ s/\.MuDst//;
  my $name = $log;
  $log .= ".log";
# my $cmd = "bsub -J $name -o $log -q star_cas_short  root.exe -q -b runSvtTree.C\\(\\\"";
  if (-r $MuDst) {
#    print "$log has been created. Skip it\n";
    print "$MuDst has been created. Skip it\n";
  } else {
    my $cmd = "bsub -J $name -o $log -q star_cas_prod  root.exe -q -b bfc.C\\(9999,\\\"";
    $cmd .= "MakeMuDst\\\",\\\"";
    $cmd .= $file;
    $cmd .= "\\\"\\)";
    print "$cmd\n";
    $flag = system($cmd);
    sleep 5;
#    my $cmd = "root.exe -q -b 'runSvtTree.C(\"" .  $file . "\")' >& " .  $log;
#    print "$cmd\n";
  }
}
