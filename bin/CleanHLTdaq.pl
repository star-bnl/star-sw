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
  my @PicoList = ();
  foreach my $globP ("picoDst.root") {# B.log B.log.gz)) {
    my $glob = $Dir . "/*" . $globP;
    @PicoList = glob $glob;  
    if ($#PicoList >= 0) {
      print "glob = $glob, PicoList = @PicoList\n" if ($debug);
      last;
    }
  }
  my $nPicos = $#PicoList; print "nPicos = $nPicos\n" if ($debug);
#  my $globEv = $Dir . "/*event.root"; 
#  my @EventList = glob $globEv;
#  my $count = $#EventList;  print "$globEv = > EventList = @EventList, count = $count\n" if ($debug);
#  if ($count < 0) {next;}
  my @daqs = ();
  my $kept = 0;
  foreach my $pico (@PicoList) {
    my $tag  = File::Basename::basename($pico,".picoDst.root");
#     my $tag2 = File::Basename::basename($tag1,"B.log.gz");
#     my $tag  = File::Basename::basename($tag2,"B.log");
    my $daq = "/hlt/cephfs/daq/2021/" . $Dir . "/" . $tag . ".daq"; print "daq = $daq\n" if ($debug);
#     if (! -r $daq) {
#       $daq = "/hlt/cephfs/daq/2020/" . $Dir . "/" . $tag . ".daq";
#       if (! -r $daq) {next;}
#     }
    if (! -r $daq) {next;}
 #    my $event = $Dir . "/" . $tag . ".event.root";
#     if (-r $event)  {
#       print "# keep $daq because of $event\n";# if ($debug);
#       $kept++;
#       next;
#     }
#     push @daqs, $daq;
#   }
    push @daqs, $daq;
  }
  my $Ndaqs = $#daqs; print "Ndaqs = $Ndaqs, @daqs\n" if ($debug);
  if ($Ndaqs <= 0.1*$nPicos) {next;}
  if ($Ndaqs <= 10) {next;}
  my $j = 0;
  foreach my $daq (@daqs) {
    $j++;
    if ($j%100 == 2 && $kept < 5 ) {
      $kept++;
      print "# keep = $kept from $Ndaqs ;  $daq\n";# if ($debug);
      next;
    }
    print "rm $daq\n";
  }
}
