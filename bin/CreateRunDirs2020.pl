#! /usr/bin/env perl
use File::Basename;
use Cwd;
use Env;
use lib "/net/l402/data/fisyak/STAR/packages/.DEV2/bin";#$ENV{ConstructLocation}; 
#use RunXIXDefs;
use RunXXDefs;
#use RunXXIDefs;
#use RunXXIIDefs;
my $pwd = cwd();
#my $day = File::Basename::basename(File::Basename::dirname($pwd));
#my $run =  File::Basename::basename($pwd);
#my @globs = ("/hlt/cephfs/daq/2020/" . $day . "/" . $run . "*/hlt*.daq");#  print "globs = @globs\n";
my $debug = 0;
my $fNo = 0;
# foreach my $glob (@globs) {
#   my @files = glob $glob;
#   foreach my $file (@files) {# print "file = $file\n";
#     my $b = File::Basename::basename($file,".daq");
#     print "$b\n" if ($debug);
#     my $mufile = $b . ".MuDst.root";
#     if (-r $mufile) {next;}
#     print "string:$file\n";
#     $fNo++;
#   }
# }
#____________________________________________________________
sub PrintHash($$) {
  my $env = shift; # print "Call PrintHash\n";
  my $prefix = shift;
  foreach my $key (sort keys %$env ) {
    print "{ $key }\t=> {'$env->{$key}->{trig}', \tfield=>`$env->{$key}->{field}',\tfirst=>'$env->{$key}->{first}', \tlast=>'$env->{$key}->{last}', \tbeginTime=>'$env->{$key}->{beginTime}'\n";
  }
}
sub GoodRun($$) {
  my $env = shift;
  my $run = shift;
  print "GoodRun:: run = $run" if $debug;
  foreach my $key (sort keys %$env ) {
#    if ( $env->{$key}->{first} < 23001000) {next;}
    my $trig = $env->{$key}->{trig};
    if ($trig =~ /Cosmic_/) {$trig = "Cosmic";}
    print "$pwd, trig = $trig, field = $env->{$key}->{field}; first = $env->{$key}->{first}, last = $env->{$key}->{last}" if ($debug);
    if ($pwd =~ /tune/ and $trig !~ /tune/) {print ", $run rejected by trig\n"  if ($debug); next;}
    if ($pwd !~ /$trig/)  {print ", $run rejected by trig\n"  if ($debug); next;}
    if ($pwd !~ /$env->{$key}->{field}/) {print ", $run rejected by field\n" if ($debug); next;}
    if ($run < $env->{$key}->{first})    {print ", $run rejected by first\n" if ($debug); next;}
    if ($run > $env->{$key}->{last})     {print ", $run rejected by last\n"  if ($debug); next;}
    print " accepted\n" if ($debug);
    return $run;
  }
  print "\t$run  rejected\n" if ($debug);
  return -1;
}
my $def = {@Runs};# print "Runs = @Runs\n";
#PrintHash($def,"Runs") if ($debug);
#die;
my  @runs  = glob "/hlt/cephfs/daq/2019/???/* /hlt/cephfs/daq/2020/???/*";  print "runs = @runs\n" if ($debug);
#my  @runs  = glob "/hlt/cephfs/daq/2020/2??/*";  print "runs = @runs\n" if ($debug);
#my  @runs  = glob "/hlt/cephfs/daq/2019/350/*";  print "runs = @runs\n" if ($debug);
#my  @runs  = glob "/hlt/cephfs/daq/2020/012/2101202?";  print "runs = @runs\n" if ($debug);
#my  @runs  = glob "/hlt/cephfs/daq/2021/???/*";  print "runs = @runs\n" if ($debug);
#my  @runs  = glob "/hlt/cephfs/daq/2021/3??/* /hlt/cephfs/daq/2022/???/*";  print "runs = @runs\n" if ($debug);
#my  @runs  = glob "/hlt/cephfs/daq/2019B/???/*";  print "runs = @runs\n" if ($debug);
foreach my $run (@runs) {
  my $r = File::Basename::basename($run); print "run = $run, r = $r\n" if ($debug);
  if (GoodRun($def,$r) < 0) {next;}
#  if ($r < 21040001) {next;}
#  if ($r < 21042001) {next;} # exclude 9p2GeV
  my $glob = $run . "/*.daq";
  my @daqfiles = glob $glob;
  if ($#daqfiles < 0) {next;}
  my $day = sprintf("%03i",(int ($r/1000))%1000); print "ru = $r => day = $day\n" if ($debug);
  my $dir = $day . "/" . $r;
  if (-d $dir) {next;}
  my $cmd = "mkdir -p $dir"; print "cmd = $cmd\n";
  my $flag = system($cmd);
  if ($flag) {last;}
}



