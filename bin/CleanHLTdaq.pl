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
  my $glob = $Dir . "/*picoDst.root";
  my @PicoList = glob $glob;  print "glob = $glob, PicoList = @PicoList\n" if ($debug);
  my $nPicos = $#PicoList; print "nPicos = $nPicos\n" if ($debug);
  my $globEv = $Dir . "/*event.root"; 
  my @EventList = glob $globEv;
  my $count = $#EventList;  print "$globEv = > EventList = @EventList, count = $count\n" if ($debug);
  my @daqs = ();
  foreach my $pico (@PicoList) {
    my $tag = File::Basename::basename($pico,".picoDst.root");
    my $daq = "/hlt/cephfs/daq/2019/" . $Dir . "/" . $tag . ".daq";
    if (! -r $daq) {
      $daq = "/hlt/cephfs/daq/2020/" . $Dir . "/" . $tag . ".daq";
      if (! -r $daq) {next;}
    }
    push @daqs, $daq;
  }
  my $Ndaqs = $#daqs; print "Ndaqs = $Ndaqs, @daqs\n" if ($debug);
  if ($Ndaqs <= 0.5*$nPicos) {next;}
  foreach my $pico (@PicoList) {
    my $tag = File::Basename::basename($pico,".picoDst.root");
    my $event = $Dir . "/" . $tag . ".event.root";
    print "tag = $tag; count = $count\n" if ($debug);
    if (-r $event and $debug) {
      print "$pico => $event\n";
    }
    if (-r $event) {
      if ($debug) {
	if (-r $event) { print "$count : $pico => $event\n";}
	else           { print "$count : $pico\n";}
      }
      $count++;
      print "keep $daq" if ($debug);
      next;
    }
    my $daq = "/hlt/cephfs/daq/2019/" . $Dir . "/" . $tag . ".daq";
    if (! -r $daq) {
      $daq = "/hlt/cephfs/daq/2020/" . $Dir . "/" . $tag . ".daq";
    }
    if (! -r $daq) {
      print "daq = $daq is not found \n" if ($debug);
      next;
    }
    $count++;
    if ($count%50 == 2) {
      print "# keep $daq\n";# if ($debug);
      next;
    }
    print "rm $daq\n";
  }
}
