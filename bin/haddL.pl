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
my %hash = ();
my $now = time();
foreach my $file (@Files) {
  my ($dev,$ino,$mode,$nlink,$uid,$gid,$dev, $size, $atime, $mtim, $ctime, $blksize,$blocks) = stat($file);
#  print "file $file size $size ctime $ctime now $now\n";
# if ($size < 10000000) {print "$file is too small $size --- skipped\n"; next;}
  my $dt = $now - $ctime;
#  if ($dt < 3600) {print "$file is too young $dt --- skipped\n"; next};
  my $logf = File::Basename::basename($file);
  my $dir  = File::Basename::dirname($file);
  my @logf = split '_', $logf;# print "logf = @logf\n";
  my $log = "";
  if ($logf[0] eq 'st') {
    $log = $logf[1] . "_" . $logf[2];# . "_" . $logf[3] . "_" . $logf[4]; #print "$logf => $log\n";
    $log =~ s/\.root//;
  } else {
    $log = $logf[0] . "_" . $logf[1];
  }
  if (! $hash{$log} ) {$hash{$log} = $dir;}
}
foreach my $key ( sort keys %hash ) {
#  print "$key => $hash{$key}\n";
  my $dir = $hash{$key};
  my $log = $key . ".log";
  my $root = $key . ".root";
  if (-r $root) {
    print "$root has been created. Skip it\n";
  } else {
#    my $cmd = "bsub -J $key -o $log -q star_cas_prod  root.exe -q -b bfc.C\\(9999,\\\"lana,mysql,nodefault\\\",\\\"";
#    my $cmd = "bsub -J $key -o $log -q star_cas_prod  root4star -q -b bfc.C\\(9999,\\\"lana,mysql,nodefault\\\",\\\"";
#    $cmd .= $dir . "/st_" . $key . "*.event.root" . "\\\",0,\\\"" . $root;
#    $cmd .= "\\\"\\)";
    my @files = glob $dir . "/st*" . $key . "*tags.root";
    if ($#files == 0) {
      $cmd = "ln -s $files[0] $root";
    } else {
      $cmd = "hadd " . $root . " " . $dir . "/st*" . $key . "*tags.root";
    }
    print "$cmd\n";
    $flag = system($cmd);
#    sleep 5;
  }
}
