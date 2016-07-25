#!/usr/bin/env perl
# merge #if nad #endif
open(In,"$ARGV[0]") or die "Can't open $ARGV[0]";
my $line = "";
my $def = "";
my $endif = "";
while ($line = <In>) {
  print "line: $line";
  my @words = split ' ', $line;
#  for (my $i = 0; $i <= $#words; $i++) {print "$i : $words[$i]\n";}
  if      ($words[0] eq  '#ifndef') {
    if (! $def) {
      $def = "#if ! defined(" . $words[1] . ")";
    } else {
      $def .= " || ! defined(" . $words[1] . ")";
    }
#    print  "def: $def\n";
    next;
  } elsif ($words[0] eq  '#ifdef') {
    if (! $def) {
      $def = "#if defined(" . $words[1] . ")";
    } else {
      $def .= " || defined(" . $words[1] . ")";
    }
    next; 
  } elsif ($words[0] eq  '#endif') {
    if (! $endif) {
      $endif = "#if defined(" . $words[1] . ")";
    } else {
      $endif .= " || defined(" . $words[1] . ")";
    }
#    print "endif: $endif\n";
    next;
  } elsif($words[0] eq  '#else') {
    if (! $elsif) {
      $endif = "#elsif defined(" . $words[2] . ")";
    } else {
      $endif .= " || defined(" . $words[1] . ")";
    }
    print "endif: $endif\n";
   next;
   } 
  if ($def) {
    print "$def\n";
    $def = "";
  }
  if ($endif) {
    print "$endif\n";
    $endif = "";
  }
  print "$line";
}
close(In);
