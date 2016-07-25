#! /usr/bin/env perl
use strict;
use File::Basename;
if ($#ARGV < 0) {
  print "Usage: $0 depends_file\n";
  exit 0;
}
my $DepsFile = $ARGV[0];
open (IN,"$DepsFile") or die "Can't open $DepsFile";
my $line;
my %HoL = ();
while ($line = <IN>) {
  next if $line !~ /\:/;
  my ($lib,$deps) = split ':', $line;
  my $Lib = File::Basename::basename($lib);
  $Lib =~ s/^lib//;
  $Lib =~ s/\.so//;
  my @dlibs = split ' ',$deps;
  my @Dlibs = ();
  next if $#dlibs < 0;
  foreach my $d (@dlibs) {
    my $dep = File::Basename::basename($d);
    $dep =~ s/^lib//;
    $dep =~ s/\.so//;
    if (! $HoL{$dep}) {$HoL{$dep} = "";}
    push @Dlibs, $dep;
  }
  $HoL{$Lib} = join '|', @Dlibs;
}
close (IN);
my $gdl = "star_deps.gdl";
open (OUT,">$gdl") or die "Can't open $gdl";
print OUT '
graph: {
';
foreach my $key (sort keys %HoL) {
  print OUT "node: { title: \"$key\"  label: \"$key\"}\n";
}
foreach my $key (sort keys %HoL) {
  my @Dlibs = split '\|', $HoL{$key};
  foreach my $d (@Dlibs) {
    print OUT "edge: { source: \"$key\"  target: \"$d\"}\n";
  }
}
print OUT "}\n";
close (OUT);
