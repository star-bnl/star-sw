#!/usr/bin/env perl
use Env;
use File::Basename;
my $glob = shift;
my @files  = glob "$glob"; print "files = @files\n";
#die;
my $i = 0;
foreach my $file (@files) {
  $i++;
#  if ($file =~ /\.jpg$/) {
    print "unlink $i $file\n";
    unlink $file;
#  }
}
#________________________________________________________________________________
__END__;





