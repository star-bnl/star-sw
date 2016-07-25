#!/usr/bin/env perl
if ($#ARGV < 0) {exit 0;}
my $tag = $ARGV[0];
my $cmd = "ls -1 " . $tag . "*.h";
my @list = `$cmd`;
my $NoVers = $#list + 1;
print "NoVers  = $NoVers\n";
print " @list\n";
if ($NoVers <= 0) {die "Can't find $tag *.h files";}
my @vers = qw(year2000 year_2b year2001 year2002 year_2a
	      year2003 y2003x y2003a y2003b y2003c
	      y2004 y2004x y2004y y2004a y2004b y2004c y2004d 
	      y2005x y2005 y2005b y2005c y2005d y2005e y2005f y2005g
	      y2006 y2006a y2006b y2006c y2006g
	      y2007 y2007a y2007g y2008
	      dev2005 complete upgr01 upgr02 upgr03 upgr04 upgr05 upgr06 upgr07
	      upgr08 upgr09 upgr10 upgr11 upgr12 upgr13 upgr14 upgr15 upgr20 upgr21);
my @ordlist = ();
foreach my $V (@vers) {
  for (my $i = 0; $i <$NoVers; $i++) {
    my $f = $list[$i]; 
    chomp($f);
    if (! $f) {next;}
#    print "$i => $f\n";
    my $VV1 = $V; $VV1 .= "\+";#  print "VV1= $VV1\n";
    my $VV2 = $V; $VV2 .= "\.h";# print "VV2= $VV2\n";
    my $file = "";
    if ($f =~ /$VV1/ or $f =~ /$VV2/) {
      $file = $f;
#      print "Found $file for $VV1 or $VV2\n";
      push @ordlist, $file; 
      $list[$i] = "";
      last;
    }
  }
}
print "ordlist $#ordlist: @ordlist\n";
my $base = $ordlist[0];
if (! -r $base) {die "There is no $base\n";}
for (my $i=1; $i<=$NoVers; $i++) {
  my $cur = $ordlist[$i]; #print "$i: $cur\n";
  if (! $cur) {next;}
  if (! -r $cur) {die "There is no $cur\n";}
  my $D = $cur;
  $D =~ s/$tag\.//;
  $D =~ s/\.h//;
  my $out = $base;
  $out =~ s/\.h//;
  $out .= "_" . $D;
  $out =~ s/year/Y/g;
  $out =~ s/complete/C/g;
  $out =~ s/200//g;
  $out =~ s/upgr/u/g;
#  my $cmd = "diff -I '^#' -D__" . $cur . "__ " . $base . " " . $cur . " > " .$out . ";";
#  my $cmd = "diff -bBd --exclude=pattern='^#' -D__" . $cur . "__ " . $base . " " . $cur . " > " .$out . ";";
#  my $cmd = "diff -bBd -I '^#' -D__" . $cur . "__ " . $base . " " . $cur . " > " .$out . ";";
#  my $cmd = "diff -bBd --ifdef=__" . $cur . "__ " . $base . " " . $cur . " > " .$out . ";";
  my $cmd = "diff -bBd -I '^#' --ifdef=" . $D . " " . $base . " " . $cur . " > " .$out . ";";
# my $cmd = "diff -bBd  --ifdef=" . $cur . " " . $base . " " . $cur . " > " .$out . ";";
  print "$cmd\n"; 
  system($cmd);
  $base = $out;
}
