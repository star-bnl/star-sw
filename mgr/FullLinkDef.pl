#!/usr/bin/env perl
use File::Basename;
my $dst = shift; print "dst = $dst\n";
open (OUT,">$dst") or die "Can't open $dst\n";
print OUT '
#if defined( __CINT__) || defined(__CLING__)                        
#pragma link off all globals;         
#pragma link off all classes;         
#pragma link off all functions;       
';
foreach my $file (@ARGV) {
  next if ($file !~ /_Table\.h/);
  my $stem = File::Basename::basename($file,"_Table.h");  
  $stem =~ s/^St_//;
#  print "cons::TableLinkDef : $dst => $stem\n";
#  print "file = $file; stem = $stem\n";
  print OUT '
#pragma link C++ class St_' . $stem . '-;
#pragma link C++ class ' . $stem . '_st+;
';
}
print OUT '
#endif
';
close (OUT);
