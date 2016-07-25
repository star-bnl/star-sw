#!/usr/bin/env perl
# find inconsistencies with MuDsts
use File::Basename;
use Cwd;

my @Chains = glob "*Chain*";# print "Chains : @Chains\n";
my $tag = "";
my $f = 0;
my $l = 0;
my $i = 0;
foreach my $file (@Chains) {
  open(In,"$file") or die "Can't open $file";
  while (my $line = <In>) {
    chop($line);
    if ($line !~ /^#/) {next;}
    my @w = split (" ",$line);
    my $file = $w[2];
    my $N   = $w[3];
    my $tag = $file;
    $tag =~ s/\.MuDst\.root//;
    $tag =~ s/_adc//;
    my @t = split('_',$tag);
    my $f = $t[$#t-1];
    my $l = $t[$#t];
    if ($l - $f + 1 != $N) {
      print "$file $f $l $N\n";
    }
  }
  close(In);
}
