#! /usr/local/bin/perl
#use File::Basename;
#print "rootcint @ARGV\n";
my $dir = shift;
my $com = "cd $dir && rootcint @ARGV";# print "======================\n$com\n";
exit `$com`;
