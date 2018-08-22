#! /opt/star/bin/perl
#######! /usr/bin/env perl
use File::Basename;

#print "$#ARGV\n";
if ($#ARGV < 0) {
  print "Usage : $0 trigger\n";
  exit 0;
}
my $debug = 0;
my $trigger = $ARGV[0];
my $TriggerFile = $trigger . ".list"; print "Trigger file = $TriggerFile\n" if ($debug);
if (! -r $TriggerFile) {
#  my $cmd = "get_file_list.pl -delim '/' -keys 'path,filename' -cond 'production=P10ih,trgsetupname=" . $trigger . ",filetype=daq_reco_MuDst,filename~st_physics,tpx=1,sanity=1,events>10' -limit 0"; 
  my $cmd = "get_file_list.pl -delim '/' -keys 'path,filename' -cond 'production=P10ih,trgsetupname=" . $trigger . ",filetype=daq_reco_MuDst,filename~st_physics,sanity=1,tpx=1' -limit 0";
  print "$cmd\n" if ($debug);
  my  @list = `$cmd`;
  open(Out, ">$TrigerFile") or die "Can't open $TriggerFile";
  foreach my $file (@list) {
    print Out "$file";
  }
  close (Out);
}
my $j = 1;   
open(In, "$TriggerFile") or die "Can't open $TriggerFile";
while (my $file = <In>) { 
  chomp($file);
  print "$file\n" if ($debug);
  if ($file !~ /starlib/) {next;}
  my @words = split('/',$file); 
  my $nw = $#words;
#  print "words = $nw\n"; for (my $i = 1; $i <= $nw; $i++) {print "\twords[$i] = $words[$i]";} print "\n";
  my $year = int $words[$nw-3];
  my $day = int $words[$nw-2]; 
  my $run = int $words[$nw-1];
  my $basename = $words[$nw];
  print "run = $run year = $year day = $day file = $file basename = $basename\n" if ($debug);
  if ($day < 10) {$day = "00" . $day;}
  elsif ($day < 100) {$day = "0" . $day;}
  my $dir = $trigger . "/" . $year . "/" . $day . "/" . $run; print "dir = $dir\n" if ($debug);
  if (! -d $dir) {`mkdir -p $dir`;}
  my $Out = $dir . "/MuDst.list";
  open(Out, ">>$Out") or die "Cannot open $Out";
  print Out "$file\n"; 
  close(Out); 
  $j++;
#  if ($j > 10) {  die; }
}
close (In);
