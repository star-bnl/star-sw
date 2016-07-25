#!/usr/bin/env perl
my @files = glob "/star/subsys/tpc/2013/pp500/Filter/*.event.root";
my @runs = ();
foreach my $file (@files) {
  my @words = split("_",$file);
  my $run = $words[2];# print "$file => $run\n";
  push @runs, $run;
}
my $runs = join("|",@runs);# print "runs = $runs\n";
my $l  = 0;
my $ListOfFiles = "/star/subsys/tpc/2013/Run13pp500P1.list";
open (In, $ListOfFiles) or die "Can't open $ListOfFiles";
while (my $line = <In>) {
  my @words = split("_",$line);
  my $run = $words[2]; ###   print "$file => $run\n";
  if ($run =~ /$runs/) {next;}
  print "$line";
#  print "============== accepted\n";
  $runs .= "|" . $run;
}
#  my @words = split "\"",$line;
#  my $file = $words[2]; #print "$file\n";
#  if ($file !~ /st_physics.*1010001$/ and $file !~ /st_physics.*1010005$/) {next;}
#  my $evf = "/star/data16/TPC/2012/UU193/" . $file . ".event.root";
##  print "$file => $evf\n";
#  if (-r $evf) {next};
##  print "=========== $line";
#  my $dir = $words[0]; 
#  my $f   = $words[2] . ".daq";
#  my $odir = $dir;
#  $odir =~ s#/home/starsink/raw#/star/data15/TPC#;
#  print "$dir/$f $odir/$f\n";
#  $l++;
#  if ($l >= 200) {last;}
#}
