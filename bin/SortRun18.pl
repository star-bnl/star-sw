#!/usr/bin/env perl 

#my $input = "/star/u/fisyak/DB/Run19.list";
#open(In,$input) or die "Can't open $input";
my $line;
my $runOld = 0;
my $trigOld = "";
my $dateOld = 0;
my $timeOld = 0;
my $runMin = 0;
my $runMax = 0;
my $N = 0;
my $run,$trig,$date,$time;
#while ($line = <In>) {
while ($line = <>) {
  ($run,$trig,$date,$time) = split ' ', $line;
  if ($run eq 'cmd' or $run eq 'runNumber') {next;}
  if ($trig !~  /production/) {next;}
#   if ($trig =~ /^ped/) {next;}
#   if ($trig =~ /^las/) {next;}
#   if ($trig =~ /^jml/) {next;}
#   if ($trig =~ /^chris/) {next;}
#   if ($trig =~ /^tune/) {next;}
#   if ($trig =~ /^cal/) {next;}
#   if ($trig =~ /^Jack/) {next;}
#   if ($trig =~ /^Cos/) {next;}
#   if ($trig =~ /^straw/) {next;}
#   if ($trig =~ /^Vern/) {next;}
#   if ($trig =~ /^test/) {next;}
#   if ($trig =~ /^straw/) {next;}
#   $trig =~ s/_bbcveto//;
#   $trig =~ s/_lzr//;
#   $trig =~ s/_opentac//;
#  print "run = $run, trig = $trig, date = $date, time = $time\n";
#  print "trigOld = $trigOld\n";
  if ($trig eq $trigOld) {
    $runMax = $run;
    $dateMax = $date;
    $timeMax = $time;
  } else {
#    print "trigOld2 = $trigOld\n";
    if ($trigOld ne '') {
    $dateMax = $date;
    $timeMax = $time;
      printf("%-40s",$trigOld);
      print "\t$runMin\t$runMax\t$dateMin\t$timeMin\t$dateMax\t$timeMax\n";
#    die;
    }
    $trigOld = $trig;# print "trig = $trig. trigOld = $trigOld\n";
    $runMin = $run; $runMax = $run;
    $dateMin = $date; $dateMax = $date;
    $timeMin = $time; $timeMax = $time;
#    print "$trigOld\t$runMin\t$runMax\t$dateMin\t$timeMin\t$dateMax\t$timeMax\n";
  }
  $N++;
#  if ($N > 20) {last;}
}
if ($trigOld != 0) {
    $dateMax = $date;
    $timeMax = $time;
      printf("%-40s",$trigOld);
  print "\t$runMin\t$runMax\t$dateMin\t$timeMin\t$dateMax\t$timeMax\n";
}
#close(In);
