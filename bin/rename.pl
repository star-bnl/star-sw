#!/usr/bin/env perl
my $line;
foreach my $file (@ARGV) {
  open (In,"$file") or die "Can't open $file";
  $line = <In>;
  print $line;
  close(Int);
  if ($line !~ /time start/) {die "line does not contain time start";}
  #  $newfile =~ s/ //g;
  my @words = split ' ', $line;
  my $timestart = $words[3];
  my $timeend   = $words[6];
  my ($ms,$ds,$ys) = split '/', $timestart;
  my ($me,$de,$ye) = split '/', $timeend;
  my $newfile = $ys . "-" . $ms . "-" . $ds . "_" . $ye . "-" . $me . "-" . $de;
#  print "$file : $timestart - $timeend    => $newfile\n";
  print "$file  => $newfile\n";
  if (-r $newfile) {die "$newfile already existed";}
#  my $status = rename $file, $newfile;
#  if (! $status) {last;}
  my $cmd = "cp -p $file $newfile";
  my $flag = system($cmd);
  if ($flag) {die "$cmd failed";}
}
