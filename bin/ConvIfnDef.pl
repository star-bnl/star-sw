#! /usr/bin/env perl
if ($#ARGV < 0) {
  print "Usage : $0 input_file\n";
}
my $def = 0;
my $line;
open(In,"$ARGV[0]") or die "Can't open $ARGV[0]";
while ($line = <In>) {
  my $nextline = "";
  my $nextline2 = "";
 START:
  if ($line =~ /^#ifndef/) {
    print "line:\t$line";
    $nextline = <In>;
    print "nextline:\t$nextline";
    if ($nextline and $nextline =~ /^#endif/) {
      $nextline2 = <In>;
      print "nextline2:\t$nextline2";
      if ($nextline2 and $nextline =~ /^#endif/) {
	my ($ifnd,$tag) = split / /, $line;
	print "split into $ifnd,$tag";
	chomp($tag);
	if ($nextline2 =~ /$tag/) {
	  print " // ========> skip \n $line and $nextline2\n";
	  print $nextline;
	  next;
	}
      }
      
    }
  }
  print $line;
  if ($nextline) {print $nextline;}
  if ($nextline2) {print $nextline2;}
}
close(In);
