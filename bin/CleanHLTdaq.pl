#!/usr/bin/env perl
use File::Basename;
use Cwd;
my $daq = "";
my @List = glob "*/*/Done";
my $debug = 0;
foreach my $file (@List) {
  my $Dir = File::Basename::dirname($file);
  print "Dir = $Dir\n" if ($debug);
  my $glob = $Dir . "/*picoDst.root";
  my @PicoList = glob $glob;  print "glob = $glob, PicoList = @PicoList\n" if ($debug);
  my $globEv = $Dir . "/*event.root"; 
  my @EventList = glob $globEv;
  my $count = $#EventList;  print "$globEv = > EventList = @EventList, count = $count\n" if ($debug);
  foreach my $pico (@PicoList) {
    my $tag = File::Basename::basename($pico,".picoDst.root");
    my $event = $Dir . "/" . $tag . ".event.root";
    print "tag = $tag\n" if ($debug);
    if (-r $event and $debug) {
      print "$pico => $event\n";
    }
    if (-r $event or $count == -1) {
      if ($debug) {
	if (-r $event) { print "$count : $pico => $event\n";}
	else           { print "$count : $pico\n";}
      }
      $count++;
      next;
    }
    my $daq = "/hlt/cephfs/daq/2019/" . $Dir . "/" . $tag . ".daq";
    if (! -r $daq) {
      $daq = "/hlt/cephfs/daq/2020/" . $Dir . "/" . $tag . ".daq";
      if (! -r $daq) {
	next;
      }
    }
    print "rm $daq\n";
  }
}
