#!/usr/bin/env perl
if ($#ARGV < 0) {
  print "Usage:  $0 run_number\n"; 
  exit;
}
my $run = $ARGV[0];
my $cmd = "mysql -h robinson.star.bnl.gov --port=3306 -u \"fisyak\" RunLog_onl  -e 'select runNumber,DATE(beginTime)+0,TIME(beginTime)+0 from starMagOnl where runNumber >= $run limit 2';";
#print "cmd = $cmd\n";
#my $flag = system($cmd);
my @list = `$cmd`; # print "list = @list\n";
foreach my $line (@list) {
  next if ($line =~ /run/);
  my @words = split " ",$line;
#  print "$words[0], $words[1],$words[2]\n";
  printf("run = %8i => date =  %08i.%06i", $words[0], $words[1],$words[2]);
  print "\t";
}
print "\n";
