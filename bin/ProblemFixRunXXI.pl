#! /usr/bin/env perl
use File::Basename;
use Cwd;
use Env;
use lib "/net/l402/data/fisyak/STAR/packages/.DEV2/bin";#$ENV{ConstructLocation}; 
#use RunXXIDefs;
use RunXXIDefs;
sub PrintHash($) {
  my $env = shift; # print "Call PrintHash\n";
  my $prefix = shift;
  foreach my $key (sort keys %$env ) {
    print "{ $key }\t=> {'$env->{$key}->{trig}', \tfield=>`$env->{$key}->{field}',\tfirst=>'$env->{$key}->{first}', \tlast=>'$env->{$key}->{last}', \tbeginTime=>'$env->{$key}->{beginTime}'\n";
  }
}
my $env = {@Runs};# print "Runs = @Runs\n";
#PrintHash($env);
foreach my $key (sort keys %$env ) {
#  print "$pwd, trig = $env->{$key}->{trig}, field = $env->{$key}->{field}; first = $env->{$key}->{first}, last = $env->{$key}->{last}\n";
#  if ($env->{$key}->{trig} !~ /4p59GeV_fixedTarget/) {next;}
  if ($env->{$key}->{trig} !~ /fixedTarget/) {next;}
  print "$env->{$key}->{trig} => first = $env->{$key}->{first}, last =  $env->{$key}->{last}\n";
  my $cmd = "mysql -h robinson.star.bnl.gov --port=3306 -u \"\" --password=\"\" RunLog_onl -e 'select beginTime,flavor,deactive,runNumber,yellowEnergy,yellowIntensity,yellowFillNumber  from beamInfo where yellowIntensity < 1 and  runNumber >= " . $env->{$key}->{first} . "  and runNumber <= " . $env->{$key}->{last} . ";'";
  print "$cmd\n"; 
  system($cmd);
}


