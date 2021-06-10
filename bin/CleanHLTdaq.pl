#!/usr/bin/env perl
use File::Basename;
use Cwd;
my $daq = "";
my @List = glob "*/*/Done";
my $debug = 0;
if ($#ARGV >= 0) {
  $debug = $ARGV[0];
}
foreach my $file (@List) {
  my $Dir = File::Basename::dirname($file);
  print "Dir = $Dir\n" if ($debug);
#  my $glob = $Dir . "/*picoDst.root";
  my $glob = $Dir . "/*B.log";
  my @PicoList = glob $glob;  print "glob = $glob, PicoList = @PicoList\n" if ($debug);
  my $nPicos = $#PicoList; print "nPicos = $nPicos\n" if ($debug);
  my $globEv = $Dir . "/*event.root"; 
  my @EventList = glob $globEv;
  my $count = $#EventList;  print "$globEv = > EventList = @EventList, count = $count\n" if ($debug);
  my @daqs = ();
  my $kept = 0;
  foreach my $pico (@PicoList) {
#    my $tag = File::Basename::basename($pico,".picoDst.root");
    my $tag = File::Basename::basename($pico,"B.log");
    my $daq = "/hlt/cephfs/daq/2021/" . $Dir . "/" . $tag . ".daq";
#     if (! -r $daq) {
#       $daq = "/hlt/cephfs/daq/2020/" . $Dir . "/" . $tag . ".daq";
#       if (! -r $daq) {next;}
#     }
    if (! -r $daq) {next;}
    my $event = $Dir . "/" . $tag . ".event.root";
    if (-r $event)  {
      print "# keep $daq because of $event\n";# if ($debug);
      $kept++;
      next;
    }
    push @daqs, $daq;
  }
  my $Ndaqs = $#daqs; print "Ndaqs = $Ndaqs, @daqs\n" if ($debug);
#  if ($Ndaqs <= 0.5*$nPicos) {next;}
  if ($Ndaqs <= 1) {next;}
  my $j = 0;
  foreach my $daq (@daqs) {
#     $j++;
#     if ($j%50 == 2 && $kept < 2 ) {
#       print "# keep $daq\n";# if ($debug);
#       $kept++;
#       next;
#     }
    print "rm $daq\n";
  }
}
