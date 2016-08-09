#!/usr/bin/env perl
use File::Basename;
use Cwd;
use Env;
my $DIR = Cwd::cwd(); 
@Files = glob "st*.csh"; 
if  ($#Files < 0) {exit 0;} 
my $CONDOR = "jobs.condor";  
open (CONDOR,">$CONDOR") or die "Can't open $CONDOR"; 
foreach my $file (@Files) { 
  my $name = File::Basename::basename($file,".csh");
  my $log = $name . "B.log"; 
  if (-r $log) {next;}
  my $event = $name . ".event.root";
  if (-r $event) {next;}
  my $MuDst = $name . ".MuDst.root";
  if (-r $MuDst) {next;}
  my $cmd = "test ! -r " . $log . " && ";
  print CONDOR `
Universe       = vanilla
Notification   = never
Executable     = /star/u/users/fisyak/bin/trapguard.csh
Arguments      = . ' . $file . '
Output         = ' . $log . '
Error          = ' . $log . '
Requirements   = (CPU_Experiment == \"star\")
Log            = ' . $log . '
Initialdir     = ' . $DIR . '
+Experiment     = \"star\"
+Job_Type       = \"cas\"
kill_sig        = SIGINT
PeriodicRemove  = (NumJobStarts >=1 && JobStatus==1) || (JobStatus == 2 && (CurrentTime - JobCurrentStartDate > (54000)) && ((RemoteUserCpu+RemoteSysCpu)/(CurrentTime-JobCurrentStartDate) < 0.10)) || (((CurrentTime - EnteredCurrentStatus) > (2*24*3600)) && JobStatus == 5) || (JobRunCount >= 1 && JobStatus == 1)
Priority        = +10
Queue
';
}
</job>
';
close (CONDOR);
