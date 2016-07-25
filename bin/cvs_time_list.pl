#!/usr/bin/env perl
use Env;
if (defined($AFS)) {$File::Find::dont_use_nlink;}
require "find.pl";
%LoH = ();
if ($#ARGV > -1) {$dir = $ARGV[0];}
&find (\&wanted,$ENV{CVSROOT});
my $output = "cvs_time.table";
open (Out, "> $output") or die "Can't open $output";
foreach my $key (sort keys %LoH) {
  print "$LoH{$key} = $key\n";
  print Out "$LoH{$key} = $key\n";
}
close(Out);
#________________________________________________________________________________
sub wanted {
#  print "$_  ========\n";
  if (-d $_ && 
      ($_ eq 'Attic' || 
       $_ =~ '\.backup'
      ) #&& $_ ne 'CVSROOT'
     ) {$File::Find::prune = 1; return;}
  if (-f $_ && $_ =~ /,v$/) {
#    print "file $File::Find::name / $_\n"; 
#    unlink($File::Find::name) or die "Can't remove $File::Find::name";
    open(In, $_) or die "Can't open $_";
    my $line;
    while ($line = <In>) {
      next if $line !~ /^date/;
      my ($d,$date,$dd,$author,$ddd,$state) = split ' ', $line;
#      print "$d,$date,$dd,$author,$ddd,$state\n";
      next  if $author eq 'perev;' or $author eq 'fisyak;' or $author eq 'fine';
      my ($ye,$mo,$da,$ho,$mi,$se) = split /\./, $date;
#      print "$date  ==> $ye|$mo|$da|$ho|$mi|$se\n";
      if ($ye < 2000) {$ye = $ye + 1900;}
      chop($line);
      my $datime = $da + 100*($mo + 100*$ye);
      my $name =  $File::Find::name;
      $name =~ s/$ENV{CVSROOT}//;
      $name =~ s/^\///;
#      print "$datime $line $File::Find::name  => $name\n";
      my @dirs = split '\/', $name;
      my $d = $name;
      for (my $i = 0; $i < $#dirs; $i++)  {
	$d = File::Basename::dirname($d);# print "$d\n";
	if (! $LoH{$d} or $LoH{$d} < $datime) {
	  $LoH{$d} = $datime;
	}
	print "$d => $LoH{$d}\n";
      }
      last;
    }
  }
}
