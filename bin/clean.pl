#!/usr/bin/env perl
while (my $line = <>) {
  if ($line =~ /[^a-zA-Z_0-9 \t\n\r\f\(\)\{\}\]/\[\]\'\"]/) {print $line;}
}
