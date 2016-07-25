#! /usr/bin/env perl
    use Env;
    use File::Basename;
my @list = glob "$STAR/StarDb/Calibrations/tpc/tpcPadGainT0B*.root";
foreach my $f (@list) {
  my $b = File::Basename::basename($f);
  my @words = split('\.',$b);
  my $date = int $words[1];
  my $time = int $words[2];
  print "$f => $date $time\n";
  my $cmd = "root.exe -q -b 'tpcPadGainT02B.C(" . $date . "," . $time . ")' >& " . $date . "." . $time . ".log";
  print "$cmd \n";
  my $flag = system($cmd); print "$flag\n";
  if ($flag) {exit 1;}
} 
