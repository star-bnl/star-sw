#! /usr/bin/env perl
use File::Basename;
use Cwd;
# my $file = "/afs/rhic.bnl.gov/star/users/fisyak/bin/timing.list";
# open(In, $file) or die "Can't open $file";
# while ( my $it = <In>) {
#   my @words = split(":",$it);
#   my $log = $words[0] . "_" . $words[1] . "_" . $words[2] . "_" . $words[3] . "/st_physics_11029020_raw_1030002.log";
#   if (-r $log) {next;}
#   print "string:$it";
# }
# Setup directory structure for timing

foreach my $vers (qw(dev .DEV2 TFG19m)) {
  print "vers = $vers\n";
  foreach my $bits (qw(32b 64b)) {
    print "bits = $bits\n";
    foreach my $deb (qw(debug opt)) {
      print "deb = $deb\n";
      my @RC_list = qw(StiCA);
      my @GCC_list = qw(485);
      if ($vers != "dev") {
        @RC_list = qw(StiCA Stx);
        @GCC_list = qw(485 631 731 830);
      }
      foreach my $rc (@RC_list) {
        print "rc = $rc\n";
        foreach my $comp (@GCC_list) {
	  print "comp = $comp\n";
          my $dir = "RC_" . $comp . "_" . $bits . "_" . $rc . "_" . $vers . "_" . $deb;
	  print "dir = $dir\n";
	  my $cmd = "test -d $dir || mkdir $dir; cd $dir; source /star/u/fisyak/bin/SetVersion.csh;";
	  print "cmd = $cmd\n";
	  my $flag = system($cmd);
	  if ($flag) {die;}
	  $flag = system("cd -");
#          if (! -d $dir) mkdir $dir;
#	  cd $dir
#          SetVersion
#	  cd -
	}
      }
    }
  }
}

