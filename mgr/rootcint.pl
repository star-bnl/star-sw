#!/usr/bin/env perl
#use File::Basename;
#print "rootcint @ARGV\n";
my $DirName = shift; #print "DirName = $DirName ";
my $dir = `pwd`; chomp $dir;  print "dir = $dir\n";

my @argv = ();
foreach my $a (@ARGV) {
  foreach my $c (split / /, $a) {
    if ($c  !~ /^-I/ || $c =~ /^-I\//) {push @argv, $c;}
    else {(my $cc = $c) =~ s/-I//; $cc = "-I" . $dir . "/" . $cc; push @argv, $cc;}
  }
}
my $com = "cd $DirName && rootcint @argv"; print "$com\n";
exit `$com`;
