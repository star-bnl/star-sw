#! /usr/bin/env perl

#print "ARGV = @ARGV\n";
my $glob = shift; #print "glob = $glob\n";
my @files = glob $glob . "*";
if ($#files < 0) {exit 0;}
foreach my $file (@files) {
  my $digit = $file;
  $digit =~ s/$glob//;
  my $newfile = $glob . "0" . $digit;
  print "$file => $newfile\n";
  `mv $file $newfile`;
}
