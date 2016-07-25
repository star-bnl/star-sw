#!/opt/star/bin/perl
use File::Basename;
my $debug = 0;# 1;
my @files;
my $glob = "/star/data03/daq/2011/172/121720*/st_physics_adc_*.daq";# /star/data10/VFtest/daq/2012/09*/*/st_zerobias_adc*.daq";
@files = glob $glob;
my $seed = 0;
foreach my $file (@files) {
  my $bfile = File::Basename::basename($file);
  print "$file => $bfile\n" if ($debug);
  my $cmd = "get_file_list.pl -keys 'events' -cond 'filetype=online_daq,filename=" . $bfile . "' -limit 1"; 
  print "$cmd\n" if ($debug);
  my $N = `$cmd`; chomp($N); print "N = $N\n" if ($debug);
  my $step = 50; 
  for (my $i = 1; $i < $N; $i += $step) {
    $seed++;
    my $i1 = $i;
    my $i2 = $i + $step - 1;
    if ($i2 > $N) {$i2 = $N;}
    my $fout = $bfile . "_" . $i1 . "_" . $i2;
    $fout =~ s/\.daq//;
    my $mudst = $fout . ".MuDst.root";
    if (-r $mudst) {
#      print "$mudst already exist\n" if ($debug);
    } else {
      print "string:$file:$i1:$i2:$fout:$seed\n";
    }
  }
}
