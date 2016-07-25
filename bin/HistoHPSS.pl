#! /usr/bin/env perl
use File::Basename;
use Cwd;
if ($#ARGV < 0) {
  print "Usage: $0 RunXV'\n";
  exit 0;
} 
my $pat = $ARGV[0];
my $pwd = cwd();
my @lines = glob $pat . "*/*.root";# print "lines @lines\n";
print "cd Histograms\n";
print "mkdir $pat\n";
print "cd $pat\n";  
foreach my $file (@lines) {
  my $d = File::Basename::dirname($file);
  print "mkdir $d\n";
  print "cd $d\n";
  print "lcd $d\n";
  my $f = File::Basename::basename($file);
  print "cput $f\n";
  print "cd ..\n";
  print "lcd ..\n";
}
