#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = cwd();
#my $day = File::Basename::basename(File::Basename::dirname($pwd));
#my $run =  File::Basename::basename($pwd);
#my @globs = ("/hlt/cephfs/daq/2019/" . $day . "/" . $run . "*/hlt*.daq");#  print "globs = @globs\n";
my $debug = 1;
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
#my  @runs  = glob "/hlt/cephfs/daq/2019/???/* /net/l401/data/scratch1/daq/2019/???/*";  print "runs = @runs\n" if ($debug);
#my  @runs  = glob "/hlt/cephfs/daq/2019/???/*";  print "runs = @runs\n" if ($debug);
my  @runs  = glob "/gpfs01/star/daq/2019/???/*";  print "runs = @runs\n" if ($debug);
foreach my $run (@runs) {
#  my @files = glob $run . "/hlt*.daq";#
  my @files = glob $run . "st_physics_20*.daq";#
  my $r = File::Basename::basename($run);
#  if ($r >= 20100000) {next;}
  if ($#files < 0) {next;}
  print "run = $run. files = $#files\n" if ($debug);
  my $NF = $#files;
  my $step = $NF/2;
  if ($step < 1) {$step = 1;}
  for (my $i = 0; $i < $NF; $i+= $step) {
    my $file = $files[$i];
    my $b = File::Basename::basename($file,".daq");
    print "$b\n" if ($debug);
    my $mufile = $b . ".MuDst.root";
    if (-r $mufile) {next;}
    my $blafile = $b . ".bla.root";
    if (-r $blafile) {next;}
    print "string:$file\n";
    $fNo++;
  }
}
if (! $fNo) {die "Don't have input files\n";}

