#!/usr/bin/env perl
# Sort 4 list for tpcPadGainT0
use Env;
my $debug = 0;
my @List3 = glob "$STAR/StarDb/Calibrations/tpc/Path2tpxGain*.*.C";
my @dt3 = ();
foreach my $line (@List3) {
  $dt = $line;
  $dt =~ s/.*tpxGain\.//;
  $dt =~ s/\.C//;
  print "$line :  dt = $dt\n" if ($debug);
  push @dt3, $dt;
}
#die;
my @List2 = glob "$STAR/hold/StarDb/Calibrations/tpc/tpcPadGainT0.*.root*";
my @dt2 = ();
foreach my $line (@List2) {
  $dt = $line;
  $dt =~ s/.*tpcPadGainT0\.//;
  $dt =~ s/\.root//;
  $dt =~ s/.HOLD.ofl//;
  print "$line :  dt = $dt\n" if ($debug);
  push @dt2, $dt;
}
my @List1 = glob "db/tpcPadGainT0*.root";
my @dt1 = ();
foreach my $line (@List1) {
  $dt = $line;
  $dt =~ s/.*tpcPadGainT0\.//;
  $dt =~ s/\.root//;
  print "$line :  dt = $dt\n" if ($debug);
   push @dt1, $dt;
}
my @List0 = `mysql -h robinson.star.bnl.gov --port=3306 -u "fisyak" Calibrations_tpc -e "select beginTime,entryTime,deactive from tpcPadGainT0  order by beginTime;" | grep -v deactive`;
my @dt0 = ();
foreach my $line (@List0) {
  chop($line);
  my @word = split /\s+/, $line;
  print "$line : $word[0] $word[1]" if ($debug);
  $word[0] =~ s/-//g;
  $word[1] =~ s/://g;
  my $dt = $word[0] . "." .  $word[1];
  print "\tdt = $dt\n" if ($debug);
  push @dt0, $dt;
}
sub mindt($$$$) {
  my $k = $_[0];
  for (my $j = 1; $j < 4; $j++) {
    if ($_[$j] > $k) {next;}
    $k = $_[$j];
  }
#  print "$_[0] $_[1] $_[2] $_[3] => $k\n";
  return $k;
}
#print "dt0 = $#dt0, dt1 = $#dt1, dt2 = $#dt2, dt3 = $#dt3\n" if ($debug); 
my @i = (0, 0, 0, 0);
my @max = ($#dt0, $#dt1, $#dt2, $#dt3); print "max = @max\n";
while ($i[0] < $max[0] && $i[1] < $max[1] && $i[2] < $max[2] && $i[3] < $max[3]) {
  my @d = (9999999,9999999,9999999,9999999);
  if ($i[0] < $max[0]) {$d[0] = $dt0[$i[0]];}
  if ($i[1] < $max[1]) {$d[1] = $dt1[$i[1]];}
  if ($i[2] < $max[2]) {$d[2] = $dt2[$i[2]];}
  if ($i[3] < $max[3]) {$d[3] = $dt3[$i[3]];}
  my $dmin = mindt($d[0],$d[1],$d[2],$d[3]);
  my $line = "";
#   if ($d[0] == $dmin) {$line .= sprintf("%-60s",$List0[$i[0]]); $i[0]++;} else {$line .= "1                                                   ";}
#   if ($d[1] == $dmin) {$line .= sprintf("%16.6f",$d[1]);        $i[1]++;} else {$line .= "2               ";}
#   if ($d[2] == $dmin) {$line .= sprintf("%16.6f",$d[2]); 	$i[2]++;} else {$line .= "3               ";}
#   if ($d[3] == $dmin) {$line .= sprintf("%16.6f",$d[3]); 	$i[3]++;} else {$line .= "4               ";}
  if ($d[0] == $dmin) {$line .= sprintf("%-60s",$List0[$i[0]]); $i[0]++;} else {$line .= sprintf("1 %66s"," ");}
  if ($d[1] == $dmin) {$line .= sprintf("%16.6f",$d[1]);        $i[1]++;} else {$line .= sprintf(" 2%14s"," ");}
  if ($d[2] == $dmin) {$line .= sprintf("%16.6f",$d[2]); 	$i[2]++;} else {$line .= sprintf(" 3%14s"," ");}
  if ($d[3] == $dmin) {$line .= sprintf("%16.6f",$d[3]); 	$i[3]++;} else {$line .= sprintf(" 4%14s"," ");}
  print "$line\n";
#   for (my $j = 0; $j < 4; $j++) {
#     print "\t$i[$j]";
#   }
#   print "\n";
} 
