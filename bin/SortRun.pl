#!/usr/bin/env perl 

#my $input = "/star/u/fisyak/DB/Run19.list";
#open(In,$input) or die "Can't open $input";
my $line;
my $runOld = 0;
my $trigOldF = "";
my $trigOld = "";
my $dateOld = 0;
my $timeOld = 0;
my $runMin = 0;
my $runMax = 0;
my $fieldOld = "";
my $N = 0;
#while ($line = <In>) {
sub SPrint ($$$$$$$$) {
  my ($trigOld,$runMin,$runMax,$dateMin,$timeMin,$dateMax,$timeMax,$field) =  @_;
#  print "\t\'$N\' => {tag =>";
  my $t = "'" . $trigOld . "',";
#  printf("\'%i\' \t=> %s=> ",$N,$t);
  printf("'%3i' => {trig=>%-22s",$N,$t);
  my $dd = $dateMin; $dd =~ s/\-//g;
  my $tt = $timeMin; $tt =~ s/://g;
  print "\tfield => \'$field\',\tfirst=> \'$runMin\',\t last => \'$runMax\',\t list => \'\',  beginTime => \'$dd.$tt\'}, \# \t$dateMin\t$timeMin\t$dateMax\t$timeMax\n";
  $N++;
}
print "\@Runs = ( # onl CURRENT | SortRun.pl \n";
while ($line = <>) {
  my ($run,$trig,$date,$time,$scaleFactor,$rtsStatus,$shiftLeaderStatus,$destinationID) = split ' ', $line;
  my $field = "UF";
  if     ($scaleFactor >-1.2 && $scaleFactor < -0.8) {$field = "RF";}
  elsif ($scaleFactor > -0.8 && $scaleFactor < -0.2) {$field = "RHF";}
  elsif ($scaleFactor > -0.2 && $scaleFactor <  0.2) {$field = "ZF";}
  elsif ($scaleFactor >  0.2 && $scaleFactor <  0.8) {$field = "FHF";}
  elsif ($scaleFactor >  0.8 && $scaleFactor <  1.2) {$field = "FF";}
  if ($run eq 'cmd' or $run eq 'runNumber') {next;}
#  if ($rtsStatus != 0 || $shiftLeaderStatus != 0) {next;}
  if ($trig !~  /production/ and $trig !~  /^tune/ and $trig !~ /^Cosmic/) {next;}
   if ($trig =~ /^ped/) {next;}
   if ($trig =~ /^las/) {next;}
   if ($trig =~ /^jml/) {next;}
   if ($trig =~ /^chris/) {next;}
   if ($trig =~ /^tune/) {next;}
   if ($trig =~ /^cal/) {next;}
   if ($trig =~ /^Jack/) {next;}
 # if ($trig =~ /^Cos/) {next;}
   if ($trig =~ /^straw/) {next;}
   if ($trig =~ /^Vern/) {next;}
   if ($trig =~ /^test/) {next;}
   if ($trig =~ /^straw/) {next;}
  $trig =~ s/_bbcveto//;
  $trig =~ s/_lzr//;
  $trig =~ s/_opentac//;
  $trig =~ s/_EPDtest//;
  $trig =~ s/_GMT//;
  $trig =~ s/LocalClock//;
  $trig =~ s/production_//;
  $trig =~ s/_2020//;
#  print "run = $run, trig = $trig, date = $date, time = $time\n";
#  print "trigOld = $trigOld\n";
  $timeMax = $time;
  $dateMax = $date;
  my $trigF = $trig . $field;
  if ($trigF eq $trigOldF && ($run - $runOld) <= 1) {
#    print "runMax = $runMax, runOld = $runOld, run = $run\n";
    $runOld = $run;
    $runMax = $run;
  } else {
#    print "trigOld2 = $trigOld\n";
    if ($trigOld ne '') {
      SPrint($trigOld,$runMin,$runMax,$dateMin,$timeMin,$dateMax,$timeMax,$fieldOld);
#    die;
    }
    $trigOld = $trig;# print "trig = $trig. trigOld = $trigOld\n";
    $trigOldF = $trigF;
    $fieldOld = $field;
    $runMin = $run; $runMax = $run;
    $runOld = $run;
    $dateMin = $date; 
    $timeMin = $time; 
#    print "$trigOld\t$runMin\t$runMax\t$dateMin\t$timeMin\t$dateMax\t$timeMax\n";
  }
#  if ($N > 20) {last;}
}
if ($trigOld ne '') {
  SPrint($trigOld,$runMin,$runMax,$dateMin,$timeMin,$dateMax,$timeMax,$fieldOld);
}
#close(In);
print ");\n1;\n";
