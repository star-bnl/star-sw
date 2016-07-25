#!/usr/bin/env perl
my $input = "starMagnet.txt";#"MagFactor.list";
open(IN,$input) or die "Can't open $input";
my ($datime, $factor, $oldatime, $oldfactor) = ("","","","");
my $line;
while ($line = <IN>) {
  my ($date,$time,$factor) = split ' ', $line;
  $datime = $date . " " . $time;
  $datime =~ s/ /\./;
  $datime =~ s/-//g;
  $datime =~ s/://g;
  if ((int  $factor/100) == (int  $oldfactor/100)) {
    $oldatime = $datime;
  } else {
    if ($oldatime) {printf("%12s %7.2f\n",$oldatime,$oldfactor);}
    printf("%12s %7.2f\n",$datime,$factor);
    $oldfactor = $factor;
    $oldatime = "";
  }
}
close (IN);
