#!/usr/bin/env perl
if ($#ARGV < 0) {
  print "Usage: $0 diff_of_two_log_files\n";
  exit 0;
}
my $file = $ARGV[0];
open(IN,"$file") or die "Can't open $file";
my ($newl,$oldl,$diff,$odlfiff) = ("","",0,0);
my ($dum1,$dum2,$dum3,$oldmk,$oldproc,$dum6,$oldmem,$dum8,$oldheap,$dum10,$oldstack,$oldd);
my (                  $newmk,$newproc,      $newmem,      $newheap,       $newstack,$newd);
my $olddiff = 0;
while (my $line = <IN>) {
  next if $line !~ /QAInfo: doPs for/;
#  print $line;
  chomp($line);
  $line =~ s/=//g;
  $line =~ s/\(//g;
  if ($line =~ /^\-/) {
    $oldl = $line; #print "oldl $oldl\n"; 
    next;
  }
  elsif ($line =~ /^\+/) {
    $newl = $line; #print "newl $newl\n";
  }
  else {next;}
#  print "$oldl\n$newl\n";
#  my @wold = split ' ', $oldl;# for (my $i = 0; $i <= $#wold; $i++) {print "$i $wold[$i]\n";}
#  my $oldmk = $wold[3]; 
#  my $oldproc = $wold[4]; 
#  my $oldmem = $wold[6]; $oldmem =~ s/=//;
#  print "old:@wold   $oldmk,$oldproc,$oldmem\n";
#  my @wnew = split ' ', $newl;
#  my $newmk = $wnew[3]; 
#  my $newproc = $wnew[4]; 
#  my $newmem = $wnew[6]; $newmem =~ s/=//;
  ($dum1,$dum2,$dum3,$oldmk,$oldproc,$dum6,$oldmem,$dum8,$oldheap,$dum10,$oldstack,$oldd) = 
    split ' ', $oldl; 
#  print "old:@wold   $oldmk,$oldproc,$oldmem,$oldstack,$oldd\n";
  ($dum1,$dum2,$dum3,$newmk,$newproc,$dum6,$newmem,$dum8,$newheap,$dum10,$newstack,$newd) = 
    split ' ', $newl; 
#  print "new:@wnew   $newmk,$newproc,$newmem,$newstack,$newd\n";
  if ($oldmk ne $newmk or $oldproc ne $newproc) {
    print "mismatch mk $oldmk :: $newmk or proc $oldproc :: $newproc\n";
    next;
  }
  my $dd = $newd - $oldd;
  my $diff = $newmem - $oldmem;#  print "$newmk, $newproc, $diff, dd = $dd\n";
  my $difdif = $diff - $olddiff;
#  if ($difdif > 0 or abs($dd) > 0.1) {
  if (abs($dd) > 0.1) {
    if ($difdif > 0.01) {
      print "$newmk $newproc  difference = $diff / $difdif / $dd\n";
    }
    $olddiff = $diff;
#    print "$oldl\n$newl difference = $diff\n";
  }
}
