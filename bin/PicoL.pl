#! /usr/bin/env perl
use File::Basename;
use Cwd;
#my $glob =  "/net/l404/data/fisyak/Pico/BES-I/AuAu19_production/2011/???/*";
#my $glob =  "/net/l401/data/scratch2/kehw/reco/2019/???/*";
#my $glob =  "/net/l401/data/scratch2/kehw/reco/2019/TFG19d/???/*";
#my $glob =  "/net/l401/data/scratch2/kehw/reco/2019/14GeV_2019_StiCA/0??/*";
#my $glob =  "./Pico*/???/*/*.picoDst.root";
my $PICOPATH = "";
my $debug = 0;
my $pwd = Cwd::cwd(); print "pwd = $pwd\n" if ($debug);
my $glob;
if ($pwd =~ /dev/) {
  $PICOPATH = "/gpfs01/star/data*";
  $glob = $PICOPATH;
  if    ($pwd =~ /5p75GeV_fixedTarget/) {$glob .= "/reco/production_5p75GeV_fixedTarget_2020/ReversedFullField/dev/20*";}
  elsif ($pwd =~ /11p5GeV/)             {$glob .= "/reco/production_11p5GeV_2020/ReversedFullField/dev/20*";}
  
} else {# TFG
  $PICOPATH = "/gpfs01/star/pwg_tasks/tfg02";
  if (! -r  $PICOPATH) {$PICOPATH = "/net/l401/data/scratch1/reco";}
  if (! -r $PICOPATH) {exit 1;}
  $glob = $PICOPATH;
  if    ($pwd =~ /5p75GeV_fixedTarget/) {$glob .= "/2020/TFG19m/RF/5p75GeV_fixedTarget.B";}
  elsif ($pwd =~ /11p5GeV/)             {$glob .= "/2020/TFG19m/RF/11p5GeV.B";}
}
$glob .= "/*/*";
print "glob = $glob\n" if ($debug);
my %Runs= ();
foreach my $run (glob $glob) {
  my $f = File::Basename::basename($run);
  $Runs{$f}++;
  my $pico = $f . "_" . $Runs{$f} . ".root";
  if ( -r $pico) {next};
  print "string:$run:$pico\n";
#  last;
}
