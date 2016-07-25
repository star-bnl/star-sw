#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $glob = "y*/*/*.log";
my @list = glob $glob;
my $today = (Sun,Mon,Tue,Wed,Thu,Fri,Sat)[(localtime)[6]];
my $glob_ref = "/star/rcf/test/dev/*ittf/" .$today. "/y*/*/*.log";
my @listref = glob $glob_ref;
my $DIR = Cwd::cwd();
foreach my $log (@list) {
  my $dir = File::Basename::dirname($log);
  my $loc = File::Basename::basename($log);
#  print "$log => $fileref\n";
  foreach my $ref (@listref) {
    my $locref = File::Basename::basename($ref);
#    print "$log and $locref\n";
    if ($loc eq $locref) {
      my $fileref = $locref . ".ref";
      chdir($dir); my $cwd = Cwd::cwd(); print "Current directory $cwd\n";
      if (-f $fileref) {`rm $fileref`;}
#      my $sym = $cwd . "/" . $fileref;
#      my $lnk = eval (symlink($ref, $sym));
      my $cmd = "ln -s $ref $fileref";
      my $flag = system($cmd);
      if ($flag) {die "Can't make link $ref => $fileref\n";}
      else       {print     "make link $ref => $fileref\n";}
      chdir($DIR);
    }
  }
}
