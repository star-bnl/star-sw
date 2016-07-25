#! /usr/local/bin/perl
#
my $file = @ARGV[0];
print "input file :", $file, "\n";
my $line;
my $count = 0;
my $line_no = 0;
open (INPUT, $file) or die "Cannot open $file";
while ($line = <INPUT>) {
  $line_no++;
  if ($line !~ /\#/) {
#    next;
#    }
      if ($line =~ /endif/) {
	$count--; 
      }
      else {
	if ($line =~ /if/) {
	  $count++; 
	}
	else {next;}
      }
      printf ("%i -- %i -- %s", $line_no,$count, $line);
    }
  else {next;}
}
if ($count) {printf ("=================================\n");}
exit(0);
# last line
