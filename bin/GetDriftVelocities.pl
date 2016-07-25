#! /usr/bin/env perl
use File::Basename;
use Cwd;
my @files = glob "tpcDriftVelocity.2007*.C";
my @words;
foreach my $file (@files) {
  open(IN,"$file") or die "Can't open file $file";
  my ($DV,$dDV,$DVWest,$dDVWest,$DVEast,$dDVEast);
  my $line;
  while ($line = <IN>) {
#    print "$line";
    if ($line =~ /AddAt/) {
      @words = split(" ",$line);
      $DV = $words[4]; $dDV = $words[6];
#      print "DV = $DV +/- $dDV\n";
    } elsif ($line =~ /return \(TD/) {
      @words = split(" ",$line);
      $DVWest = $words[5]; $dDVWest = $words[7];
      $DVEast = $words[10]; $dDVEast = $words[12];
#      print "DVWest = $DVWest +/- $dDVWest ;   DVEast = $DVEast +/- $dDVEast  \n";
    }
  }
  close(IN);
  print "All = $DV +/- $dDV; West = $DVWest +/- $dDVWest; DVEast = $DVEast +/- $dDVEast;\n";
#  die;
}
