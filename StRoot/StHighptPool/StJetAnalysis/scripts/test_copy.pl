#!/usr/bin/perl
use File::Copy;

print STDOUT "executing test_copy.pl \n";
copy("file_blah_dummy.txt","file_copy.txt");
copy("*.txt","../.");
