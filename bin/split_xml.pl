#!/usr/bin/env perl
if ($#ARGV < 1) {
  print "Usage: split_xml.pl Template.xml N_copies\n";
}
my $Template = $ARGV[0];
my $Ncopies  = $ARGV[1];
for (my $copy = 0; $copy < $Ncopies; $copy++) {
  open (In, $Template) or die "Can't open $Template";
  my $Output = $copy ."_". $Template;
  open (Out, ">$Output") or die "Can't open $Output";
  my $line;
  while ($line = <In>) {
    $line =~ s/\[T\]/$copy/g;
    print Out $line;
  }
  close (Out);
  close (In);
}
