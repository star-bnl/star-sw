#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = cwd();
my $Field = "Undefined";
my $dir = File::Basename::basename($pwd);# print "dir = $dir\n";
if    ($dir eq 'RFF') {$Field = 'ReversedFullField';}
elsif ($dir eq 'FF' ) {$Field = 'FullField';}
if ($Field eq "Undefined") {die "Field is not defined";}
my @list = glob "/star/institutions/bnl/fisyak/Tpc/Alignment/2011/daq/" . $dir . "/*.daq";
my $events = 100000;
foreach my $line (@list) {
#  chop($line);
  print "string:$line:1:$events\n";
}
