#!/usr/bin/env perl
print "#include \"CreateGeometry.h\"\n";
print "TDataSet *CreateTable() {return CreateGeometry(\"@ARGV[0]\");}\n";


