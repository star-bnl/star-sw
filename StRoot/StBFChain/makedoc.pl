#!/bin/env perl
# merge StBFChain.cxx with BFC.h  and BFC2.h into StBFChain.cxx_doc
open (CXX, "StBFChain.cxx")      or die "Can't open StBFChain.cxx";
open (DOC, ">StBFChain.cxx_doc") or die "Can't openStBFChain.cxx_doc";
my $line;
while ($line = <CXX>) {
  if ($line =~ "^#include") {
    if ($line =~ "BFC\.h" or $line =~ "BFC2\.h") {
      print DOC "// $line";
      if ($line =~ "BFC\.h") {
	open (BFC, "BFC.h")              or die "Can't open BFC.h";
      } else {
	open (BFC, "BFC2.h")            or die "Can't open BFC2.h";
      }
      while ($line = <BFC>) {
	print DOC $line;
      }
      close (BFC);
      next;
    }
  }
  print DOC $line;
}
close (CXX);
close (DOC);
