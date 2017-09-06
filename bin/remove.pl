#!/usr/bin/env perl
#my @Files = glob "*.csh";
my @Files = glob "sched*";
my $i = 0;
foreach my $file (@Files) {
  print "$file\t";
#  if ($i%4 == 0) {
#    print " keep\n";
#  } else {
    print " remove\n";
    unlink $file;
#  }
  $i++;
}
