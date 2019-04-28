#! /usr/bin/env perl
#
#print "no. of ARGV = $#ARGV\n";
if ($#ARGV < 0) {
  print "Usage: $0 file_list\n";
  exit 1;
}
my @files = @ARGV;
#print "files = @files\n";
foreach my $file (@files) {
  my $found = 0;
  my $l = 0;
  open (INPUT, $file) || die "Cannot open $file";
  while ($line = <INPUT>) {
    $l++;
    if ($line !~ /\s*<job/) {next;}
    if ($line =~ /copyInputLocally/) {next;}
    $found++;
    last;
  }
  close (INPUT);
  if ($found) {
    my $newfile = $file . ".NEW";
    my $oldfile = $file . ".BAK";
    open (INPUT, "$file") or die "Can't open $file";
    open (OUTPUT, ">$newfile") or die "Can't overwrite $newfile";
    print "--- $file\n+++ $newfile\n";
    while ($lineo = <INPUT>) {
      my $line = $lineo;
      if ($line =~ /\s*<job/ and $line !~ /copyInputLocally/) {
#	print "old line:$line";
	$line =~ s/>/ copyInputLocally="false">/;
#	print "new line:$line";
      }
      print OUTPUT $line;  #print $line;
      if ($lineo ne $line) {
	print "- ", $lineo;
	print "+ ", $line;
      }
    }
    close (INPUT);
    close (OUTPUT);
    rename $file, $oldfile or die "Can't rename $file to $oldfile";
    rename $newfile, $file or die "Can't rename $newfile to $file";
  }
}
