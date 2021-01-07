#! /usr/bin/env perl
use File::Basename;
use Cwd;
use Env;
use lib "/net/l402/data/fisyak/STAR/packages/.DEV2/bin";#$ENV{ConstructLocation}; 
use RunXIXDefs;
my $debug = 1;
if ($#ARGV >= 0) {
  $debug = $ARGV[0]; print "debug $debug\n" if ($debug);
}
my $TriggerF = "";
my $beginTime = "";
my $endTime = "";
#____________________________________________________________
sub PrintHash($$) {
  my $env = shift; # print "Call PrintHash\n";
  my $prefix = shift;
  foreach my $key (sort keys %$env ) {
    my $t = $env->{$key}->{trig} . "/" . $env->{$key}->{field};
    if ($t =~ /Cosmic/) {next;}
    $endTime = $env->{$key}->{beginTime};
    if ($TriggerF ne $t) {
      if ($TriggerF) {
	printf("%f : %f %s\n",$beginTime,$endTime, $TriggerF);
      }
      $TriggerF = $t;
      $beginTime = $env->{$key}->{beginTime};
    }
  }
	printf("%f : %f %s\n",$beginTime,$endTime, $TriggerF);
}
my $def = {@Runs};# print "Runs = @Runs\n";
PrintHash($def,"Runs") if ($debug);
#die;
