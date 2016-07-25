#!/usr/bin/env perl
use File::Basename;
use FileHandle;
my @vers = qw(year2000 year_2b year2001 year2002 year_2a
	      year2003 y2003x y2003a y2003b y2003c
	      y2004 y2004x y2004y y2004a y2004b y2004c
	      y2004d y2005x y2005 y2005b y2005c y2005d y2005e y2005f y2005g
	      y2006 y2006a y2006b y2006c y2006g
	      y2007 y2007a y2007g y2008
	      dev2005 complete upgr01 upgr02 upgr03 upgr04 upgr05 upgr06 upgr07
	      upgr08 upgr09 upgr10 upgr11 upgr12 upgr13 upgr14 upgr15 upgr20 upgr21);
my $fout = "GeometryConfiguration.h";
open (Out, ">$fout") or die "Can't opend $fout";
foreach my $v (@vers) {
  print Out "#ifdef $v\n";
  my $cmd = "ls -l *" . $v . ".h";
  my @lines = `$cmd`;
  foreach my $line (@lines) {
    chomp($line);
    if ($line !~ /->/) {next;}
    if ($line =~ /geometry/) {next;}
#    print "$line\n";
    my @words = split ' ', $line;
#    print "$words[8] => $words[10]\n";
    my $tag = $words[10];
    $tag =~ s/==/   /;
    print Out "  #define $tag\n";
  }
  print Out "#endif /* $v */\n";
}
close (Out);
