#!/usr/bin/perl

$name = "dummy";
my $is = sprintf qq(blah_%s),$name;
print STDOUT "is= $is \n";

$dir = "./";
opendir DIR,$dir or die;
my @files = grep {/.txt/} readdir DIR;
closedir DIR;
for my $file (@files) {
  print STDOUT "Found file $file in dir \n";
  symlink "${file}","temp.txt";
}
