#!/usr/bin/env perl
#while (my $line = <>) {
#  if ($line =~ /[^a-zA-Z_0-9 \t\n\r\f\(\)\{\}\]/\[\]\'\"]/) {print $line;}
#}
while (my $line = <>) {
  chop($line);
  my ($daq,$data3) = split ' ', $line;
#  print "$line => daq = $daq, data3 = $data3\n";
  if (-r $data3) {next;}
  print "$line\n";
}
