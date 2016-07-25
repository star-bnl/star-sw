#!/opt/star/bin/perl
# get_file_list.pl -delim '/' -keys 'path,filename,events' -cond 'filetype=online_daq,filename~st_zerobias_adc_,tpx=1,runnumber[]13077066-13099025,events>500' -limit 100
use File::Basename;
my $debug = 0;# 1;
my @files;
#my $glob = "/star/data03/daq/2012/*/*/st_zerobias_adc*.daq";# /star/data10/VFtest/daq/2012/09*/*/st_zerobias_adc*.daq";
my $glob = "/star/data03/daq/2012/076/*/st_phys*.daq /star/data03/daq/2012/091/*/st_phys*.daq /star/data03/daq/2012/108/*/st_phys*.daq";
@files = glob $glob;
my $seed = 0;
my $Njobs = 0;
foreach my $file (@files) {
  my $bfile = File::Basename::basename($file);
  print "$file => $bfile\n" if ($debug);
  my $cmd = "get_file_list.pl -keys 'events' -cond 'filetype=online_daq,filename=" . $bfile . "' -limit 1"; 
  print "$cmd\n" if ($debug);
  my $N = `$cmd`; chomp($N); print "N = $N\n" if ($debug);
  my $step = 1000;
  for (my $i = 1; $i < $N; $i += $step) {
    $seed++;
    my $i1 = $i;
    my $i2 = $i + $step - 1;
    if ($i2 > $N) {$i2 = $N;}
    my $fout = $bfile;
    $fout =~ s/\.daq//;
    $fout .= "_" . $i1 . "_" . $i2;
    my $mudst = $fout . ".MuDst.root";
    if (-r $mudst) {
#      print "$mudst already exist\n" if ($debug);
    } else {
      print "string:$file:$i1:$i2:$fout:$seed\n";
      $Njobs++;
    }
  }
#  last;
 #  if ($Njobs > 100) {last;}
}
#13078028 
#13078035/*zerobias_adc*.daq 13078039/*zerobias_adc*.daq 13078043/*zerobias_adc*.daq 13078048/*zerobias_adc*.daq 13078051/*zerobias_adc*.daq 13078058/*zerobias_adc*.daq 13078070/*zerobias_adc*.daq 13099025/*zerobias_adc*.daq
