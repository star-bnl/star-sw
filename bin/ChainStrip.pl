#! /usr/bin/env perl
# root.exe -q -b bfc.C |  ChainStrip.pl > Chain.pm
use File::Basename;
use Cwd;
my $line;
print "{\n%Chain = ();\n";
while ($line = <>) {
  if ($line !~ /MC\./ && $line !~ /RC\./) {next;}
#  print $line;
  $line =~ s/\s//g;
#  print "$line\n";
  my @words = split(":",$line);
  if ($words[2] =~ /^test/) {next;}
  if ($words[2] =~ /^Test/) {next;}
  if ($words[2] =~ /^eval/) {next;}
  my $l = '$Chain{';
  $l .= "'" . $words[2] . "'}";
  printf("%-45s = \"%s\";\n",$l, $words[5]);
} 
print "}\n1;\n";
