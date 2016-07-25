#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = cwd();
#my $sample = "FFB";
#if ($pwd =~ /RF/) {$sample = "RFB";}
#my @list = glob "/star/subsys/tpc/fisyak/Cosmics/2014/" . $sample . "/*event.root";
#my %Runs = ();
# /star/data8*/reco/CosmicLocalClock/FullField/P15ii_MTD/2015/*/*
# /star/data8*/reco/CosmicLocalClock/ReversedFullField/P15ii_MTD/2015/*/*
my $sample = "FullField";
if ($pwd =~ /RF/) {$sample = "ReversedFullField";}
#my @list = glob "/star/subsys/tpc/fisyak/reco/2014/Cosmics/901/" . $sample . "/*event.root";
my @list = glob "/star/data8*/reco/CosmicLocalClock/" . $sample . "/P15ii_MTD/2015/*/*/*.event.root";
foreach my $line (@list) {
  my $file = File::Basename::basename($line,".event.root");
#  $file =~ s/_raw_.*//;
#  $Runs{$file} = File::Basename::dirname($line);
##  print "$file\n";
#}
#foreach my $key ( sort keys %Runs ) {
#  my $rootf = $key . ".root";
  my $rootf = $file . ".root";
#  print "$line => $file => $rootf\n";
  if (-r $rootf) {next;}
  my $logfile = $file . "B.log";
  if (-r $logfile) {next;}
#  my $line = $Runs{$key} . "/" . $key . "\*.event.root";
#  my $line = $Runs{$key} . "/" . $key;
#  my $line 
  print "string:$line\n";
#  last;
}
