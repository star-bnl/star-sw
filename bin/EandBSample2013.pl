#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = cwd();
my $sample = "2DFF";
if ($pwd =~ /RF/) {$sample = "2DRFF";}
my @list = glob "/star/subsys/tpc/2013/Cosmics/" . $sample . "/*event.root";
foreach my $line (@list) {
  print "string:$line\n";
}
