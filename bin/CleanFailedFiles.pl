#!/usr/bin/env perl
my $FailedFiles = "FailedFiles";
my @FailedFiles = ();
open (In,"<$FailedFiles") or die "Can't open $FailedFiles";
my $line;
while ($line = <In>) {
  chomp($line);
  push @FailedFiles, $line;
}
close (In);
my $failed = join '|', @FailedFiles;
print "Failed Files \n @FailedFiles = $failed\n";
my $dir = ".";
opendir( DIR, $dir ) or die "Can't open $dir\n";
my @lists = readdir DIR;
closedir DIR;
foreach my $list (@lists) {
  next if $list !~ /\.list$/;
  print "$list\n";
  my $found = 0;
  open(In, "<$list") or die "Can't open $list";
  my $out = "temp.tmp";
  open (Out, ">$out") or die "Can't open $out";
  while ($line = <In>) {
    if ($line =~ $failed) {
      $found++;
      print "Found => $line";
      next;
    }
    print Out "$line";
  }
  close (Out);
  close(In);
  if ($found) {
    `mv $list $list.HOLD`;
    `mv $out $list`;
    `diff -u $list.HOLD $list`;
  }
}
