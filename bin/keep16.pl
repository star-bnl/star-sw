#!/usr/bin/env perl 
use File::Basename;
use Sys::Hostname;
use Cwd;
my @list = glob "hlt*.daq";
my $NF = $#list + 1; print "$NF\n";#  from @list\n";
if ($NF <= 40) {exit 0;}
my $i = 0;
foreach my $f (@list) {
  $i += 1;
  if ($i%16 == 1) {print "keep $i: $f\n"}
  else            {print "rm   $i: $f\n"; `rm $f`;}
}
