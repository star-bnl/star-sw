#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = cwd();
my $day = File::Basename::basename(File::Basename::dirname($pwd));
my $run =  File::Basename::basename($pwd);
my @globs = ("/hlt/cephfs/daq/2019/" . $day . "/" . $run . "*/hlt*.daq");#  print "globs = @globs\n";
my $debug = 0;
my $fNo = 0;
foreach my $glob (@globs) {
  my @files = glob $glob;
  foreach my $file (@files) {# print "file = $file\n";
    my $b = File::Basename::basename($file,".daq");
    print "$b\n" if ($debug);
    my $mufile = $b . ".MuDst.root";
    if (-r $mufile) {next;}
    print "string:$file\n";
    $fNo++;
  }
}
if (! $fNo) {die "Don't have input files\n";}
