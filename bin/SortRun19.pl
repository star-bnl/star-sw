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
#while ($line = <In>) {
sub SPrint ($$$$$$$) {
  my ($trigOld,$runMin,$runMax,$dateMin,$timeMin,$dateMax,$timeMax) =  @_;
  print "\t\'$N\' => {tag =>\'$trigOld',\tfirst => \'$runMin\',\t second => \'$runMax\',\t list => \'\'\}, \#  \t$dateMin\t$timeMin\t$dateMax\t$timeMax\n";
  $N++;
}
while ($line = <>) {
  my ($run,$trig,$date,$time) = split ' ', $line;
  if ($run eq 'cmd' or $run eq 'runNumber') {next;}
  if ($trig !~  /^prod/ and $trig !~  /^tune/ ) {next;}
  if ($trig =~ /^ped/) {next;}
  if ($trig =~ /^las/) {next;}
  if ($trig =~ /^jml/) {next;}
  if ($trig =~ /^chris/) {next;}
#  if ($trig =~ /^tune/) {next;}
  if ($trig =~ /^cal/) {next;}
  if ($trig =~ /^Jack/) {next;}
  if ($trig =~ /^Cos/) {next;}
  if ($trig =~ /^straw/) {next;}
  if ($trig =~ /^Vern/) {next;}
  if ($trig =~ /^test/) {next;}
  if ($trig =~ /^straw/) {next;}
  $trig =~ s/_bbcveto//;
  $trig =~ s/_lzr//;
  $trig =~ s/_opentac//;
#  print "run = $run, trig = $trig, date = $date, time = $time\n";
#  print "trigOld = $trigOld\n";
  if ($trig eq $trigOld) {
    $runMax = $run;
    $dateMax = $date;
    $timeMax = $time;
  } else {
#    print "trigOld2 = $trigOld\n";
    if ($trigOld ne '') {
      SPrint($trigOld,$runMin,$runMax,$dateMin,$timeMin,$dateMax,$timeMax);
#    die;
    }
    $trigOld = $trig;# print "trig = $trig. trigOld = $trigOld\n";
    $runMin = $run; $runMax = $run;
    $dateMin = $date; $dateMax = $date;
    $timeMin = $time; $timeMax = $time;
#    print "$trigOld\t$runMin\t$runMax\t$dateMin\t$timeMin\t$dateMax\t$timeMax\n";
  }
#  if ($N > 20) {last;}
}
if ($trigOld ne '') {
  SPrint($trigOld,$runMin,$runMax,$dateMin,$timeMin,$dateMax,$timeMax);
}
#close(In);
