#!/usr/bin/env perl
while (<>) {
  $_ =~ s|/afs/usatlas.bnl.gov/software/cvs|:kserver:atlas-sw.cern.ch:/atlascvs|g;
  print "$_";
}
