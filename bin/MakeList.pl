#!/usr/bin/env perl
use File::Basename;
my $dir1 = $ARGV[0]; print "dir1: $dir1\n";
my $dir2 = $ARGV[1]; print "dir2: $dir2\n";
my @list1 = glob $dir1 . "/*.tags.*"; print "list1: @list1\n";
my @list2 = glob $dir2 . "/*.tags.*"; print "list2: @list2\n";
my @common = ();
foreach my $file1 (@list1) {
  my $f1 = File::Basename::basename($file1);
  foreach my $file2 (@list2) {
    my $f2 = File::Basename::basename($file2);
    if ($f1 eq $f2) {
      push @common, $f2;
      last;
    }
  }
}
foreach my $c (@common) {
  print "\"$c\",\n";
}
