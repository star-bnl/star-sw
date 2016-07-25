#!/usr/bin/env perl
use Env;
if (defined($AFS)) {$File::Find::dont_use_nlink;}
require "find.pl";
my $dir = ".";
if ($#ARGV > -1) {$dir = $ARGV[0];}
print "Find empty dir in $dir:\n";
&find (\&wanted,$dir);
print "\n";
sub wanted {
  if (-d $File::Find::name) {
#    print "$File::Find::name\n";
    my $svn = $File::Find::name . ".svn";
    if ($File::Find::name !~ /\.svn/ && ! -r $svn) {
      printf(" %s",$File::Find::name);
    }
  }
}
