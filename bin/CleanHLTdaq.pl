#!/usr/bin/env perl
use File::Basename;
use Cwd;
my $daq = "";
my @List = glob "*/*/Done";
foreach my $file (@List) {
  my $Dir = File::Basename::dirname($file);
#  print "Dir = $Dir\n";
  my $glob = $Dir . "/*picoDst.root";
  my @PicoList = glob $glob; # print "glob = $glob, PicoList = @PicoList\n";
  my $globEv = $Dir . "/*event.root"; 
  my @EventList = glob $globEv;
  my $count = $#EventList; # print "$globEv = > EventList = @EventList\n";
  foreach my $pico (@PicoList) {
    my $tag = File::Basename::basename($pico,".picoDst.root");
    my $event = $Dir . "/" . $tag . ".event.root";
#    print "tag = $tag, event = $event\n";
#    if (-r $event) {
#      print "$pico => $event\n";
#    }
    if (-r $event or $count == 0) {
#      if (-r $event) { print "$count : $pico => $event\n";}
#      else           { print "$count : $pico\n";}
      $count++;
      next;
    }
    my $daq = "/hlt/cephfs/daq/2019/" . $Dir . "/" . $tag . ".daq";
    if (! -r $daq) {next;}
    print "rm $daq\n";
  }
}
