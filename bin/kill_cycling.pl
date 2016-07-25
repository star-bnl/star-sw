#!/usr/bin/env perl
use File::Basename;
open(In,"cycling.list") or die "Can't open cycling.list";
my $line;
open(COND,"cond.log") or die "Can't open cond.log";
my @cond = `condq`; print "@cond\n";
my %Hash = ();
while (my $cond = <COND>) {
  if ($cond !~ /sched/) {next;}
  print "$cond";
  my ($ID,$dum,$dum,$dum,$dum,$dum,$dum,$dum,$shed) = split(" ",$cond);
  print "$ID $shed\n";
  $Hash{$shed} = $ID;
}
close(COND);
foreach my $key ( sort keys %Hash ) {
  print"{$key} = \t$Hash{$key}\n";
}
while ($line = <In>) {
  print "$line";
  chop($line);
  open(Log,$line) or die "Can't open $line\n";
  my $script = <Log>;
  print "$script";
  close(Log);
  my $sched = File::Basename::basename($script); print "sched = $sched\n";
  my $ID = $Hash{$sched};
  print "$ID $shed\n";
}
close(In);
