#!/usr/local/bin/perl
use Env;
if (defined($AFS)) {$File::Find::dont_use_nlink;}
require "find.pl";
&find (\&wanted,'.');
sub wanted {
  -l and not -e and print "bigus link: $File::Find::name\n";
}
