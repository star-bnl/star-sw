#! /usr/bin/env perl
use File::Basename;
use Cwd;
use Env;
my $debug = 0;
my $pwd = cwd();
#my $Trigger =  File::Basename::basename($pwd); $Trigger =~ s/daq_//;
#my $Trigger =  File::Basename::basename($pwd); $Trigger =~ s/TpcRS_/daq_/; $Trigger =~ s/\..*//; print "Trigger = $Trigger\n" if ($debug);
#my $glob = "/net/l401/data/scratch1/daq/2020/" . $Trigger . "/st_physics_adc*.daq";  print "glob = $glob\n" if ($debug);
#my $glob = "/net/l401/data/scratch1/fisyak/Tpc/TpcRS/" . $Trigger . "/st_physics_adc*.MuDst.root";  print "glob = $glob\n" if ($debug);
my $dirName =  File::Basename::basename($pwd);  #$Trigger =~ s/TpcRS_//; print "Trigger = $Trigger\n" if ($debug);
my ($dummy,$Trigger) = split('_',$dirName);
my $fNo = 0;
#my $glob = "../" . $Trigger . "/*.MuDst.root"; print "glob = $glob\n" if ($debug);
my $glob = "../daq_" . $Trigger . "/*.MuDst.root"; print "glob = $glob\n" if ($debug);
my @globs = glob $glob; print "globs = @globs\n" if ($debug);
my $N = 0;
my $step = 0;
my $i1 = 0;
if ($#ARGV > 1) {
  $i1 = $ARGV[0];
  $N = $ARGV[1];
  $step = $ARGV[2];
}
foreach my $file (@globs) {
#  my $b = File::Basename::basename($file,".daq"); 
  my $b = File::Basename::basename($file,".MuDst.root"); 
  print "$b\n" if ($debug);
#  my $cmd = "root.exe -q -b 'MuDstNoEntries.C(\"" . $file . "\")' >& /dev/null";
#  print "cmd $cmd\n" if ($debug);
#  $N = system($cmd); print "N = $N\n" if ($debug);
  if ($step > 0) {
    for (my $i = $i1; $i <= $N; $i += $step) {
      my $j1 = $i;
      my $j2 = $i + $step - 1;
      
      #   my $mufile = $b . ".MuDst.root";
      #   if (-r $mufile) {next;}
      #   my $pifile = $b . ".picoDst.root";
      #   if (-r $pifile) {next;}
      #   my $blafile = $b . ".event.root";
      #   if (-r $blafile) {next;}
      print "string:$file:$j1:$j2\n";
      $fNo++;
    }
  } else {
    print "string:$file\n";
    $fNo++;
  }
}
if (! $fNo) {die "Don't have input files\n";}
