#!/opt/star/sl45_gcc346/bin/perl
# Script to record the memeopry the OnlinePresneter occupied.
# Usage: perl mempstat.pl -
# ------  It discovered the PID of the root4starN process
#         creates the log file "PID".log and record there
#         the memory usage each second if it is growing.
#
# ------------ checkMemory -------------
my $averageMem=0;
my $diffAverageMem=0;
my $lastMem=0;
my $diffLastMem=0;
my $testCmd = "ps -aux | grep operator | grep root4starN | grep Present";
# ------------stat -------------
sub statMem
{
  my $currentMem  = @_[0];
  $averageMem     = ($currentMem + $averageMem)/2;
  $diffAverageMem = $currentMem - $averageMem;
  $diffLastMem    = $currentMem - $lastMem ;
  if ($diffLastMem > 0) {$lastMem = $currentMem;}
  return $diffLastMem;
}
# ------------ checkMemory-------------
sub checkMemory
{
  my $pid = @_[0];
  my $procStat = "/proc/" . $pid . "/status";
  my $counter = 0;
  open(fileToTest,$procStat) or die;

  my $line;
  while(($line = <fileToTest>) && ($line !~ /^VmSize/) ){}
  if (length($line)){
     my @vmsize = split(" ",$line);
    ($sec,$min,$hour,$mday,$mon,$year,$wday,
     $yday,$isdst)=localtime(time);
     if ( statMem($vmsize[1]) >0) {
        open(SPYFILE, ">>$pid.log") or die;
        printf SPYFILE  "%4d-%02d-%02d %02d:%02d:%02d memory = %d %s\n",
        $year+1900,$mon+1,$mday,$hour,$min,$sec,$vmsize[1],$vmsize[2];
        close(SPYFILE);
     }
  }
}

# -----------main --------------
while(1) {
  # find the server application
  open (PID, "$testCmd|" ) or die;
  my @line= split(" ",<PID>);
  close(PID);
  if (length($line[1])) {
    open(SPYFILE, ">>$line[1].log") or die;
    print SPYFILE join(" : ", @line),"\n";
    close(SPYFILE);
    while(1) {
       # is it still alive?
      open (PID, "$testCmd|" ) or die;
      my @testline= split(" ",<PID>);
      close(PID);
      if ($line[1] != $testline[1]) { print "It dies. wait the next one\n"; last;}
      checkMemory($line[1]);
      sleep 2;
    }
  } else {
     # wait presented
     print "Waiting for the presenter to start\n";
     sleep 10;
  }
}
