#! /usr/bin/env perl
my $dir = ".";
my $match = "";
#print "no. of args = $#ARGV\n";
#for (my $i = 0; $i <= $#ARGV; $i++) {
#  print "ARGV[$i] = $ARGV[$i]\n";
#}
#die;
if ($#ARGV >= 0) {$dir = $ARGV[0];}

opendir(DIR, "$dir") || die "Cannot open $dir";
while(my $file = readdir(DIR)) {
  if ($file =~ /sched/) {
    print "$dir/$file\n";
  }
}
closedir(DIR);
