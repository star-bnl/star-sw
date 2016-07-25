#!/opt/star/bin/perl
use File::Basename;
my %ARG = (files => '/star/data03/daq/2013/*/*/st_cosmic_14*.daq',
	   step =>  500
);
while (@ARGV) {
  $_ = shift @ARGV;
  if ($_ =~ /=/) { my($key, $val) = /([^=]*)=(.*)/; $ARG{$key} = $val;}
}
my $debug = 0;# 1;
if ($debug) {
  while (my ($key,$value) = each %ARG) {
    print  "$key=$value\n";
  }
}
my $glob = $ARG{files}; print "glob = $glob\n" if ($debug);
my $step = $ARG{step};  print "step = $step\n" if ($debug);
my @files = glob $glob;
my $seed = 0;
foreach my $file (@files) {
  my $bfile = File::Basename::basename($file);
  print "$file => $bfile\n" if ($debug);
  my $cmd = "get_file_list.pl -keys 'events' -cond 'filetype=online_daq,filename=" . $bfile . "' -limit 1";
  print "$cmd\n" if ($debug);
  my $N = `$cmd`; chomp($N); print "N = $N and step = $step\n" if ($debug);
  $bfile =~ s/\.daq//;
  my $mudstb = $bfile . ".MuDst.root"; 
  if (-r $mudst) {next;}
  for (my $i = 1; $i < $N; $i += $step) {
    $seed++;
    my $i1 = $i;
    my $i2 = $i + $step - 1;
    if ($i2 > $N) {$i2 = $N;}
    my $fout = $bfile . "_" . $i1 . "_" . $i2;
    my $mudst = $fout . ".MuDst.root";
    if (-r $mudst) {next;}
    print "string:$file:$i1:$i2:$fout:$seed\n";
  }
}
