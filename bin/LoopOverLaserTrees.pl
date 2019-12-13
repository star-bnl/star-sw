#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $debug = 0;
#my @list = glob "*.laser.root";
#my @list = glob "/star/data9*/reco/AuAu_200_production_2014/*/P15ib_TPC/2014/*/*/*.laser.root";
#my @list = glob "~/work/Tpc/Laser/LANA.2014/021315/*.laser.root";
#my @list = glob "/gpfs01/star/subsys-tpc/fisyak/Tpc/Laser/LANA.2014/laser/*.laser.root";
my @list = glob "./*.laser.root"; print "list = @list\n" if $debug;
my %runs = ();
foreach my $file (@list) {
  my $run = File::Basename::basename($file,".laser.root");
  my $dir = File::Basename::dirname($file);
  $run =~ s/st_laser_//;
  $run =~ s/st_physics_//;
  $run =~ s/adc_//;
  my @words = split('_',$run);
  my $r = $words[0];
  print "$file => $r\n" if $debug;
  my $outfile = "LaserPlots." . $r . ".root";
  print "$outfile\n" if $debug;
  if (-r $outfile) {next;}
  print "does not exist\n" if $debug;
  if ($runs{$r}) {next;}
  $runs{$r} = $dir;
}
my $Njobs = 0;
my $now = time();
foreach my $key ( sort keys %runs ) {
  print "$key => $runs{$key}\n" if $debug;
  my @files = glob "st_*" . $key . "*.root";# print "@files\n";
  my $finished = 1;
  foreach my $file (@files) {
     my ($dev,$ino,$mode,$nlink,$uid,$gid,$dev, $size, $atime, $mtim, $ctime, $blksize,$blocks) = stat($file);
     my $dt = $now - $ctime;
#     print "$file dt = $dt\n";
     if ($dt < 600) {$finished = 0; last;}
  }
  if (! $finished) {next;}
  print "string:$key:$runs{$key}\n";
  $Njobs++;
}
if (! $Njobs) {die "No jobs";}
