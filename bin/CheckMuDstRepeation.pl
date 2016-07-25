#! /usr/bin/env perl
use File::Basename;
use Cwd;
my @list = glob "*MuDst.root";
my $tagOld = "";
my $dstOld = "";
my $fail = 0;
my $dir = "";
foreach my $dst (@list) {
#  print "dst = $dst\n";
  my $tag = $dst;
  $tag =~ s/_\d\d\d\d\.(MuDst|bla)\.root//;# print "$dst => $tag\n";
  if ($tag eq $tagOld) {
    $fail++;
#    print "$dstOld\n";
#    print "$dst\n";
    $dir = $dst;
    $dir =~ s/_\d\d\d\d\_\d\d\d\d\.(MuDst|bla)\.root//; # print "$dst => $dir\n";
  }
  $tagOld = $tag;
  $dstOld = $dst;
}
if ($fail) {my $cmd = "ls -alFh " .  $dir .  "*.root"; print `$cmd`; die "Repetions"} 
