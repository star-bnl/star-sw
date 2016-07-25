#! /usr/bin/env perl
foreach my $IO (qw(Inner Outer)) {
  my @Voltage = ();
  if ($IO eq 'Inner') {@Voltage = qw(1170 1135 1100);}
  else                {@Voltage = qw(1390 1345);}
  foreach my $V (@Voltage) {
    print "string:$IO$V:1000\n";
  }
}
