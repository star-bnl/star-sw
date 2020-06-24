#! /usr/bin/env perl
use File::Basename;
use Cwd;
use Env;
use lib "/net/l402/data/fisyak/STAR/packages/.DEV2/bin";#$ENV{ConstructLocation}; 
use RunXXDefs;
my $debug = 0;
my $pwd = cwd();
#my $Day = File::Basename::basename(File::Basename::dirname($pwd));
#my $Run =  File::Basename::basename($pwd);
my $Day =  File::Basename::basename($pwd);
print "Day = $Day\n" if ($debug);
#my @globs = ("/hlt/cephfs/daq/2020/" . $Day . "/" . $Run . "*/hlt*.daq");#  print "globs = @globs\n";
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
  print "GoodRun:: run = $run\n" if $debug;
  foreach my $key (sort keys %$env ) {
    print "$pwd, trig = $env->{$key}->{trig}, field = $env->{$key}->{field}; first = $env->{$key}->{first}, last = $env->{$key}->{last}" if ($debug);
    if ($pwd !~ /$env->{$key}->{trig}/)  {print ", rejected by trig $env->{$key}->{trig}\n"  if ($debug); next;}
    if ($pwd !~ /$env->{$key}->{field}/) {print ", rejected by field\n" if ($debug); next;}
    if ($run < $env->{$key}->{first})     {print ", rejected run $run by first\n" if ($debug); next;}
    if ($run > $env->{$key}->{last})      {print ", rejected run $run by last\n"  if ($debug); next;}
    print " accepted\n" if ($debug);
    return $run;
  }
  print " rejected\n" if ($debug);
  return -1;
}
my $def = {@Runs};# print "Runs = @Runs\n";
#PrintHash($def,"Runs") if ($debug);
#die;
my $glob = "/hlt/cephfs/daq/2019/".$Day."/* /hlt/cephfs/daq/2020/" . $Day ."/*";
my  @runs  = glob $glob;  print "runs = @runs\n" if ($debug);
#my @runs = ("/hlt/cephfs/daq/2019/" . $Day . "/*",
#	    "/hlt/cephfs/daq/2020/" . $Day . "/*");
print "runs = @runs\n" if ($debug);
foreach my $run (@runs) {
  my $r = File::Basename::basename($run);
  if (GoodRun($def,$r) < 0) {next;}
  foreach my $tag (qw(st_physics_2 hlt)) {
    my @files = glob $run . "/" . $tag . "*.daq";#print "files = @files\n" if ($debug);
    if ($#files < 0) {next;}
    #  print "files = @files\n";
#    my $day = int ($r/1000 - 20000); #print "ru = $r => day = $day\n";
#    if ($day != / 107 and $day != 113 and $day != 169) {next;}
#    if ($day !~ m/158|160|179|180|181|182|183/) {next;}
    #  if ($r >= 20100000) {next;}
    my $NF = $#files + 1;
    my $step = 1; #(int $NF);
    print "run = $run. files = $NF, step = $step\n" if ($debug);
    if ($step < 1) {$step = 1;}
    print "run = $run. files = $NF, step = $step\n" if ($debug);
    for (my $i = 0; $i < $NF; $i = $i +  $step) {
      print "i = $i, step = $step \n" if ($debug);
      my $file = $files[$i];
      my $b = File::Basename::basename($file,".daq");
      #    print "$b\n" if ($debug);
      my $evfile = $b . ".event.root";
      if (-r $evfile) {next;}
      my $mufile = $b . ".MuDst.root";
      if (-r $mufile) {next;}
      my $pifile = $b . ".picoDst.root";
      if (-r $pifile) {next;}
      my $blafile = $b . ".bla.root";
      if (-r $blafile) {next;}
      print "string:$file\n";
      $fNo++;
    }
  }
}
if (! $fNo) {die "Don't have input files\n";}

