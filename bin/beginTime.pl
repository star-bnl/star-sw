#!/usr/bin/env perl
if ($#ARGV < 0) {
  print "Usage:  $0 run_number1 [run_number2]\n"; 
  exit;
}
my $debug = 0;
my $run1 = $ARGV[0]; $run1 =~ s/,//;
my $run2 = $run1; 
if ($#ARGV  == 1) {$run2 = $ARGV[1];}
if ($#ARGV  == 2) {$run2 = $ARGV[2];}
$run2 =~ s/,//;
my @cmds = ("mysql -h robinson.star.bnl.gov --port=3306 -u \"fisyak\" RunLog_onl  -e 'select runNumber,DATE(beginTime)+0,TIME(beginTime)+0 from starMagOnl where runNumber >= $run1 limit 2';",
             "mysql -h robinson.star.bnl.gov --port=3306 -u \"fisyak\" RunLog_onl  -e 'select runNumber,DATE(beginTime)+0,TIME(beginTime)+0 from starMagOnl where runNumber >= $run2 limit 2';");
#my $flag = system($cmd);
print "runs = $run1 - $run2 begin and end Time:\t";
my $beginTime = "";
my $endTime = "";
for (my $i = 0; $i < 2; $i++) {
  my $cmd = $cmds[$i];
  #print "cmd = $cmd\n";
  my @list = `$cmd`;  print "list = @list\n" if ($debug);
  my $l = 0;
  foreach my $line (@list) {
    next if ($line =~ /run/);
    my @words = split " ",$line;
     print "$words[0], $words[1],$words[2]\n" if ($debug);
#    printf("run = %8i => date =  %08i.%06i", $words[0], $words[1],$words[2]);
#    print "\t";
    if (! $beginTime) {$beginTime = sprintf "%08i.%06i", $words[1],$words[2];}
    else              {$endTime   = sprintf "%08i.%06i", $words[1],$words[2];}
  }
}
#print "\n";
print " $beginTime\t$endTime\n";
