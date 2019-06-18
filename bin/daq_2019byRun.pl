#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $debug = 0;
my $pwd = cwd();
my $day = File::Basename::basename(File::Basename::dirname($pwd));
my $run =  File::Basename::basename($pwd);
#my @globs = ("/hlt/cephfs/daq/2019/" . $day . "/" . $run . "*/hlt*.daq");#  print "globs = @globs\n"
my @globs = ("/gpfs01/star/daq/2019/" . $day . "/" . $run . "*/st_physics*.daq");#  print "globs = @globs\n";
my $fNo = 0;
 foreach my $glob (@globs) {
   my @files = glob $glob;
   foreach my $file (@files) {# print "file = $file\n";
     my $b = File::Basename::basename($file,".daq");
     print "$b\n" if ($debug);
     my $MuDst = $b . ".MuDst.root";
     if (-r $MuDst) {next;}
     my $bla = $b . ".bla.root";
     if (-r $bla) {next;}
     print "string:$file\n";
     $fNo++;
   }
 }
if (! $fNo) {die "Don't have input files\n";}

