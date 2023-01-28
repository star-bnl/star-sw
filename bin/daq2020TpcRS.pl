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
my $locdir  = File::Basename::basename($pwd);
# File::Basename::basename($pwd); $Trigger =~ s/TpcRS_//; $Trigger =~ s/\..*$//;print "Trigger = $Trigger\n" if ($debug);
my ($dum,$Trigger) =  split '_', $locdir; print "Trigger = $Trigger\n" if ($debug);
my $fNo = 0;
my $glob = "../" . $Trigger . "/*.MuDst.root"; print "glob = $glob\n" if ($debug);
#my @globs = glob $glob; print "globs = @globs\n" if ($debug);
my $GlobL = "Glob.log";
if (! -r $GlobL) {
  my $cmd = "root.exe -q -b 'Chain.C+(\"" . $glob . "\")' >&" . $GlobL; print "$cmd\n" if ($debug);
  my @Glob = `$cmd`;
}
#print "================================================================================\n";
#print "@Glob\n";
my %Hash = ();
open(In, $GlobL) or die "Can't open $GlobL";
while ( my $line = <In>) {
  if ($line !~ /No\.Events/) {next;}
  my @words = split ' ', $line;
  my $file = $words[2];
  my $N    = $words[8];
#  print "$line : file = $file, N = $N\n";
  if ($N <= 0) {next;}
  $Hash{$file} = $N;
}
close(In);
my $Split = 10;
foreach my $file (sort keys %Hash) {
  if (! $file) {next;}
  my $N = $Hash{$file};
  print "key = $file; N = $N\n" if ($debug);
  my $B = File::Basename::basename($file,".MuDst.root"); 
  print "$B\n" if ($debug);
  for (my $i = 1; $i < $N; $i++) {
    my $i1 = $i;
    my $i2 = $N;
    if ($Split > 0) {
      $i2 = $i1 + $Split - 1;
      if ($i2 > $N) {$i2 = $N;}
    } else {
      $i2 = $N;
    }
    $i  = $i2;
    my $b = $B . "_" . $i1 . "_" . $i2;
    my $mufile = $b . ".MuDst.root";
    if (-r $mufile) {next;}
    my $pifile = $b . ".picoDst.root";
    if (-r $pifile) {next;}
    my $blafile = $b . ".event.root";
    if (-r $blafile) {next;}
    print "string:$file:$i1:$i2:$b\n";
    $fNo++;
  }
}
if (! $fNo) {die "Don't have input files\n";}
