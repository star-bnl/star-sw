#! /usr/bin/env perl
use File::Basename;
my %ARG = (dir => '/net/l404/data/fisyak/Tpc/TpcRS/daq_2016_AuAu200D2',
	   extension => '.MuDst.root',
	   lock      => '.MuDst.root',
          );
while (@ARGV) {
  $_ = shift @ARGV;
  if ($_ =~ /=/) { my($key, $val) = /([^=]*)=(.*)/; $ARG{$key} = $val;}
}
my $glob = $ARG{dir} . "/*" . $ARG{extension};# print "glob = $glob\n";
my @Files = glob $glob;
if ($#Files < 0) {die "Empty list";}
foreach my $file (@Files) {
  chomp($file);
  my $basename =  File::Basename::basename($file,$ARG{extension});
  my $out = $basename . "*" .  $ARG{lock};
  my @f= glob "$out";# print "$out => $#f : @f\n";
  if ("$#f" > -1) {next;}
  print "string:$file\n";
}
	
