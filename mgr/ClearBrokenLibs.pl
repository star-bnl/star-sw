#!/usr/bin/env perl
use File::Basename;
my $ListOfBrokenLibraries = "";
foreach my $logfile (glob "b*.log") {
  open(In, $logfile) or die "Can't open $logfile";
#  print "logfile = $logfile\n";
  while (my $line = <In>) {
#    if ($line !~ /unrecognized relocation (0x2a) in section/) {next;}
    if ($line !~ /unrecognized relocation/ &&
	$line !~ /access beyond end of merged section/) {next;}
							 #    print "$line\n";
    my ($dummy,$lib) = split(":",$line);
#    print "=> $lib\n";
    my $pkg;
    if ($lib =~ /\.a\(/) {$pkg = $lib; $pkg =~ s/a\(.*/a/;}
    else {    $pkg = File::Basename::dirname($lib);}
    $ListOfBrokenLibraries .= " " . $pkg . "\n";
#    last;
  }
  close(In);
}
print "$ListOfBrokenLibraries";

