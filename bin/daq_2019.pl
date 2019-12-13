#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = cwd();
#my $day = File::Basename::basename(File::Basename::dirname($pwd));
#my $run =  File::Basename::basename($pwd);
#my @globs = ("/hlt/cephfs/daq/2019/" . $day . "/" . $run . "*/hlt*.daq");#  print "globs = @globs\n";
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
#my  @runs  = glob "/hlt/cephfs/daq/2019/???/* /net/l401/data/scratch1/daq/2019/???/*";#  print "runs = @runs\n" if ($debug);
my  @runs  = glob "/net/l401/data/scratch1/daq/2019/???/*";#  print "runs = @runs\n" if ($debug);
#my  @runs  = glob "/hlt/cephfs/daq/2019/???/*";  print "runs = @runs\n" if ($debug);
#my  @runs  = glob "/gpfs01/star/daq/2019/???/*";  print "runs = @runs\n" if ($debug);
foreach my $run (@runs) {
  foreach my $tag (qw(st_physics_20 hlt)) {
    my @files = glob $run . "/" . $tag . "*.daq";  print "files = @files\n" if ($debug);
    my $NF = $#files; 
    if ($NF     < 0) {next;}
    #  print "files = @files\n";
    my $r = File::Basename::basename($run);
#    my $day = int ($r/1000 - 20000); #print "ru = $r => day = $day\n";
#    if ($day != / 107 and $day != 113 and $day != 169) {next;}
#    if ($day !~ m/158|160|179|180|181|182|183/) {next;}
    #  if ($r >= 20100000) {next;}
    print "run = $run. NF= $NF, @files\n" if ($debug);
    my $step = 1; # $NF/8;
    if ($step < 1) {$step = 1;}
    for (my $i = 0; $i <=$NF; $i+= $step) {
      my $file = $files[$i];
      my $b = File::Basename::basename($file,".daq");
      #    print "$b\n" if ($debug);
      my $mufile = $b . ".MuDst.root";
      if (-r $mufile) {next;}
      my $blafile = $b . ".bla.root";
      if (-r $blafile) {next;}
      print "string:$file\n";
      $fNo++;
    }
  }
}
if (! $fNo) {die "Don't have input files\n";}

