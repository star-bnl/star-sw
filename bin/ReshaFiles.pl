#!/opt/star/bin/perl
use File::Basename;
use File::Copy;
my @files = glob "*MuDst.root";# print "@files\n";
foreach my $file (@files) { 
#  print "$file\n";
  my $b = File::Basename::basename($file,".MuDst.root");#  print "$b\n";
  my $dn = $b;
  $dn =~ s/_adc//g;
  my @w = split("_",$dn);
  my $run = $w[2];
  my $year = int ($run/1000000);
  my $day = int (($run - $year*1000000)/1000);
  if (!-r $day && ! mkdir($day,0755)) {die "$0: can't create directory $day\n";}
#  print "$file => $b => $dn => $run => $year => $day\n";
  my $dir = $day . "/" . $run; print "$dir\n";
  if (!-r $dir && ! mkdir($dir,0755)) {die "$0: can't create directory $dir\n";}
#  if (! -d $dir) `mkdir -p $dir`;
  my $cmd = "mv " . $b . "*" . " " . $dir; 
  print "$cmd\n"; 
  my $flag = system($cmd); if ($flag) {die "$cmd failed";}

}
