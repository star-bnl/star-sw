#!/usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = cwd();
my $debug = 0;
if ($#ARGV < 1) {
#  ReduceDaqByEvent.pl /hlt/cephfs/reco/2022/RF/pp500_2022/ /hlt/cephfs/daq/2022/
  print "Usage:  $0 /hlt/cephfs/reco/2022/RF/pp500_2022/  /hlt/cephfs/daq/2022/\n";
  exit;
}
my $EVT = $ARGV[0]; print "EVT = $EVT\n" if ($debug);
my $DAQ = $ARGV[1]; print "DAQ = $DAQ\n" if ($debug);
my @events = glob $EVT . "*/*/*event.root"; print "events == @events\n" if ($debug);
my %Dirhash = ();
foreach $ev (@events) {
  my $e = $ev; 
  $e =~ s/$EVT//; print "$ev => $e \n" if ($debug);
  my $dir = File::Basename::dirname($e);
  my $fdaq = File::Basename::basename($e,".event.root"); $fdaq .= ".daq"; 
  print "$dir $fdaq\n" if ($debug);
  if ($Dirhash{$dir}) {
    $Dirhash{$dir}  .= "|" . $fdaq;
  } else {
    $Dirhash{$dir}  = $fdaq;
  }
  print "$dir => $Dirhash{$dir}\n" if ($debug);
}
foreach my $dir ( sort keys %Dirhash ) {
  my $glob = $DAQ . $dir . "/hlt*.daq";
  my @daqf = glob $glob; print "glob = $glob, dir = $dir => @daqf\n" if ($debug);
  foreach my $daq (@daqf) {
    my $d = File::Basename::basename($daq);
    if ($d =~ $Dirhash{$dir}) {
      print "#keep $daq\n";
    } else {
      print "rm $daq\n";
    }
  }
}
