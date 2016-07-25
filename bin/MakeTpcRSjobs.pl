#!/usr/bin/env perl
use File::Basename;
use Cwd;
use Env;
my $DIR = Cwd::cwd();
my $macro = "TpcRS";
for (my $i = 1; $i < 1000; $i += 25) {
  my $i1 = $i;
  my $I1 = $i;
  if    ($i1 <  10) {$i1 = "00" . $i1;}
  elsif ($i1 < 100) {$i1 = "0"  . $i1;}
  my $i2 = $i + 24;
  my $I2 = $i2;
  if    ($i2 <  10) {$i2 = "00" . $i2;}
  elsif ($i2 < 100) {$i2 = "0"  . $i2;}
  my $file = $macro . "." . $i1 . "," . $i2;
  my $script = $file . ".csh";
  open (Out, ">$script") or die "Caon't open $script\n";
  print Out '
starver .DEV2;
cd ' . $DIR . ';
root4star -q -b \''. $macro .'.C(' . $I1 . ',25,"TpcRS,dEdxY2,sdt20080129","","Bichsel,50muons0.5GeV")\' >& ' . $file . '.B.log
';
#root4star -q -b \'TpcRS.C(' . $I1 . ',' .  $I2 . ',"TpcRS,dEdxY2,sdt20080129","/star/rcf/simu/rcf1207_01_225evts.fzd","Bichsel")\' >& ' . $file . '.B.log
  close (Out);
}
my $xml = "jobs.xml";
open (Out, ">$xml") or die "Can't open $xml";
print Out 
'<?xml version="1.0" encoding="utf-8" ?> 
<job name="' . $macro . '" maxFilesPerProcess="1" simulateSubmission="false">
<command>      csh -x $INPUTFILE0 </command>
        <stdout URL="file:' . $DIR . '/shed$JOBID.log" />
<input URL="file:' . $DIR . '/' . $macro .'*.csh" />
</job> 
';
