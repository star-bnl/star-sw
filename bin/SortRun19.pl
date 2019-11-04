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
#  print "\t\'$N\' => {tag =>";
  my $t = "'" . $trigOld . "'";
  printf("\'%i\' \t= > %-50s \t=> ",$N,$t);
  my $dd = $dateMin; $dd =~ s/\-//g;
  my $tt = $timeMin; $tt =~ s/://g;
  print "\'$runMin\',\t second => \'$runMax\',\t list => \'\'\}, \# $dd.$tt \t$dateMin\t$timeMin\t$dateMax\t$timeMax\n";
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
  if ($trig =~ /^tune/) {next;}
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
  $timeMax = $time;
  $dateMax = $date;
  if ($trig eq $trigOld) {
    $runMax = $run;
  } else {
#    print "trigOld2 = $trigOld\n";
    if ($trigOld ne '') {
      SPrint($trigOld,$runMin,$runMax,$dateMin,$timeMin,$dateMax,$timeMax);
#    die;
    }
    $trigOld = $trig;# print "trig = $trig. trigOld = $trigOld\n";
    $runMin = $run; $runMax = $run;
    $dateMin = $date; 
    $timeMin = $time; 
#    print "$trigOld\t$runMin\t$runMax\t$dateMin\t$timeMin\t$dateMax\t$timeMax\n";
  }
#  if ($N > 20) {last;}
}
if ($trigOld ne '') {
  SPrint($trigOld,$runMin,$runMax,$dateMin,$timeMin,$dateMax,$timeMax);
}
#close(In);
