#! /usr/bin/env perl
use File::Basename;
use Cwd;
# JB 11/08/12
#Low-lumi run list, average zdc rate=130kHz, 20 runs
my @low = qw( 13077081
	      13078001
	      13078002
	      13078003
	      13078004
	      13078006
	      13078007
	      13078009
	      13078011
	      13078012
	      13078014
	      13078057
	      13078058
	      13078060
	      13078063
	      13078070
	      13080015
	      13091012
	      13091011
	      13091009);
#High lumi run list, average zdc rate=330kHz, 20 runs
my @high = qw(
	       13087009
	       13094081
	       13096060
	       13097021
	       13098062
	       13099025
	       13099026
	       13099056
	       13099058
	       13100025
	       13100026
	       13100049
	       13100051
	       13101058
	       13105038
	       13105061
	       13107013
	       13108071
	       13108072
	       13108073);
my $low = join('|', @low);
my $high = join('|',@high);
my $glob = "/star/data03/daq/2012/*/*/st_W*.daq";
my @files = glob $glob;
foreach my $file (@files) {
#  my ($dev,$ino,$mode,$nlink,$uid,$gid,$dev, $size, $atime, 
#      $mtim, $ctime, $blksize,$blocks) = stat($file );
#  my $N = int ($size/1000000 + 1); print "size = $size N = $N\n";
  my $bfile = File::Basename::basename($file);
  my $dir = "any";
  my ($dummy,$dummy,$run) = split '_', $bfile;# print "$bfile => $run\n";
  if ($run =~ /$low/) {$dir = "low";}
  elsif ($run =~ /$high/) {$dir = "high";}
  if ($dir ne "low" and $dir ne "high") {next;}
  $dir .= "/" . $run;
  if (! -d $dir) {`mkdir -p $dir`;}
  my $cmd = "get_file_list.pl -keys 'events' -cond 'filetype=online_daq,filename=" . $bfile . "' -limit 1"; 
  my $N = `$cmd`; chomp($N);# print "N = $N\n";
  my $step = 50; 
  for (my $i = 1; $i < $N; $i += $step) {
    my $i1 = $i;
    my $i2 = $i + $step - 1;
    if ($i2 > $N) {$i2 = $N;}
    my $fout = $dir . "/" . $bfile . "_" . $i1 . "_" . $i2;
    $fout =~ s/\.daq//;
    my $mudst = $fout . ".MuDst.root";
    if (-r $mudst) {
#      print "$mudst already exist\n";
    } else {
      print "string:$file:$i1:$i2:$fout\n";
    }
  }
}
