#!/usr/bin/env perl
use Env;
if (defined($AFS)) {$File::Find::dont_use_nlink;}
require "find.pl";
my $dir = ".";
if ($#ARGV > -1) {$dir = $ARGV[0];}
print "Find dead link in $dir\n";
&find (\&wanted,$dir);
sub wanted {
  -l and not -e and print "bigus link: $File::Find::name\n";
}
