#! /usr/bin/env perl
use File::Basename;
use Cwd;
use Env;
my @list = qw(
	       gcc482.DEV2
	       gcc482.DEV2.NODEBUG
	       gcc482.NODEBUG.x8664
	       gcc482.x8664
	       gcc492.DEV2
	       gcc492.DEV2.NODEBUG
	       gcc492.DEV2.NODEBUG.x8664
	       gcc492.DEV2.x8664
	       gcc521.DEV2.NODEBUG.x8664
	       gcc521.DEV2.x8664
	       gcc620.DEV2.NODEBUG.x8664
	       gcc620.DEV2.x8664
	    );
foreach my $item (@list) {
   print "string:$item\n";
}
