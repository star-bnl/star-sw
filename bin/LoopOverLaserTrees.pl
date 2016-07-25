#! /usr/bin/env perl
use File::Basename;
use Cwd;
#my @list = glob "*.laser.root";
#my @list = glob "/star/data9*/reco/AuAu_200_production_2014/*/P15ib_TPC/2014/*/*/*.laser.root";
#my @list = glob "~/work/Tpc/Laser/LANA.2014/021315/*.laser.root";
#my @list = glob "/star/subsys/tpc/fisyak/Tpc/Laser/LANA.2014/laser/*.laser.root";
my @list = glob "./*.laser.root";
my %runs = ();
foreach my $file (@list) {
  my $run = File::Basename::basename($file,".laser.root");
  my $dir = File::Basename::dirname($file);
  $run =~ s/st_laser_//;
  $run =~ s/st_physics_//;
  $run =~ s/adc_//;
  my @words = split('_',$run);
  my $r = $words[0];
#  print "$file => $r\n";
  my $outfile = "LaserPlots." . $r . ".root";
  if (-r $outfile) {next;}
  if ($runs{$r}) {next;}
  $runs{$r} = $dir;
}
foreach my $key ( sort keys %runs ) {
#  print "$key => $runs{$key}\n";
  print "string:$key:$runs{$key}\n";
}
