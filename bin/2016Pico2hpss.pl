#! /usr/bin/env perl
# cd 076; 2016Pico2hpss.pl | hsi
    use File::Basename;
    use Cwd;
use strict;
use File::Find ();

# Set the variable $File::Find::dont_use_nlink if you're using AFS,
# since AFS cheats.

# for the convenience of &wanted calls, including -eval statements:
use vars qw/*name *dir *prune/;
*name   = *File::Find::name;
*dir    = *File::Find::dir;
*prune  = *File::Find::prune;
my $debug = 0;
my $head  = 0;
my $count = 0;
sub wanted;

sub wanted;
my @logs = glob "hpss*.log"; print "logs: @logs\n" if ($debug);
my $AGE_OFhpss_log = 9999999;
foreach my $log (@logs) {
  my $age = -M $log;
  if ($age < $AGE_OFhpss_log) {
    $AGE_OFhpss_log = $age;
    print "AGE_OFhpss_log = $AGE_OFhpss_log for $log\n" if ($debug);
  }
}

# Traverse desired filesystems
#File::Find::find({wanted => \&wanted}, 'find', '.');
File::Find::find({wanted => \&wanted}, '.');
#exit;
#________________________________________________________________________________
sub print_head() {
  if (! $head) {
    $head = 1;
    my $pwd = cwd(); print "pwd = $pwd\n" if ($debug);
    my $dir = File::Basename::dirname($pwd);
    my $day = File::Basename::basename($dir); print "day = $day\n" if ($debug);
    print "cd reco/2016Pico\n";
    print "mkdir $day\n";
    print "cd $day\n";  
  } 
}
#________________________________________________________________________________
sub wanted {
  my $file = $_;
  if ($file =~ /^.*\.root\z/s )  {
    #  my ($dev,$ino,$mode,$nlink,$uid,$gid) = lstat($file);
    my $AGE = -M $file;
    print "AGE = $AGE for $file\n" if ($debug);
    if ($AGE < $AGE_OFhpss_log) {
      print "AGE_OFhpss_log = $AGE_OFhpss_log, AGE = $AGE for $file\n" if ($debug);
      print("$name\n") if ($debug);
      print_head();
      my $run = File::Basename::dirname($name);
      my $f = File::Basename::basename($name);
      #  if ($run !~ /17128002|17128003|17128006|17128007|17128008|17128009|17128010/) {next;}
      print "mkdir $run\n";
      print "cd $run\n";
      print "lcd $run\n";
      print "cput $f\n";
      print "cd ..\n";
      print "lcd ..\n";
      $count++;
      if ($debug && $count > 2) {die;}
    }
  }
}
#________________________________________________________________________________

