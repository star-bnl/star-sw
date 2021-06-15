#! /usr/bin/env perl
use File::Basename;
use Cwd;
use Env;
use lib "/net/l402/data/fisyak/STAR/packages/.DEV2/bin";#$ENV{ConstructLocation}; 
#use RunXXDefs;
use RunXXIDefs;
my $pwd = cwd();
#my $day = File::Basename::basename(File::Basename::dirname($pwd));
#my $run =  File::Basename::basename($pwd);
#my @globs = ("/hlt/cephfs/daq/2020/" . $day . "/" . $run . "*/hlt*.daq");#  print "globs = @globs\n";
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
#____________________________________________________________
sub PrintHash($$) {
  my $env = shift; # print "Call PrintHash\n";
  my $prefix = shift;
  foreach my $key (sort keys %$env ) {
    print "{ $key }\t=> {'$env->{$key}->{trig}', \tfield=>`$env->{$key}->{field}',\tfirst=>'$env->{$key}->{first}', \tlast=>'$env->{$key}->{last}', \tbeginTime=>'$env->{$key}->{beginTime}'\n";
  }
}
my $def = {@Runs};# print "Runs = @Runs\n";
#PrintHash($def,"Runs") if ($debug);
#die;
#my  @runs  = glob "/hlt/cephfs/daq/2019/???/* /hlt/cephfs/daq/2020/???/*";  print "runs = @runs\n" if ($debug);
#my  @runs  = glob "/hlt/cephfs/daq/2020/2??/*";  print "runs = @runs\n" if ($debug);
#my  @runs  = glob "/hlt/cephfs/daq/2019/350/*";  print "runs = @runs\n" if ($debug);
#my  @runs  = glob "/hlt/cephfs/daq/2020/012/2101202?";  print "runs = @runs\n" if ($debug);
my  @runs  = glob "/hlt/cephfs/daq/2021/???/*"; #  print "runs = @runs\n" if ($debug);
my %CosmicList = ();
foreach my $run (@runs) {
  my $r = File::Basename::basename($run);
#  print "$run => $r\n";
  foreach my $key (sort keys %$def ) {
    print "trig = $def->{$key}->{trig}, field = $def->{$key}->{field}; first = $def->{$key}->{first}, last = $def->{$key}->{last}" if ($debug);
    if ($r < $def->{$key}->{first})    {print ", rejected by first\n" if ($debug); next;}
    if ($r > $def->{$key}->{last})     {print ", rejected by last\n"  if ($debug); next;}
#    print "trig = $def->{$key}->{trig}, field = $def->{$key}->{field}; first = $def->{$key}->{first}, last = $def->{$key}->{last}\n"; # if ($debug);
    if ($def->{$key}->{trig} !~ /Cosmic/)  {print ", rejected by trig\n"  if ($debug); next;}
    my $trig = $def->{$key}->{trig} . $def->{$key}->{field};
    $CosmicList{$trig} .= " " . $run;
 #   print "$trig => $CosmicList{$trig}\n";
    print " accepted\n" if ($debug);
  }
}
foreach my $trig (sort keys %CosmicList ) {
  my @runs = split(' ',$CosmicList{$trig});
  my $nodaq = 0;
  my $TotalSize = 0;
  my $keep = 0;
  if ($trig =~ /RF/) {$keep = 10;}
  elsif ($trig =~ /FF/) {$keep = 2;}
  foreach my $r (@runs) {
    my @daqfs = glob $r . "/hlt*.daq";
    my $n = 0;
    foreach my $file (@daqfs) {
      $nodaq++;
      $n++;
      my ($dev,$ino,$mode,$nlink,$uid,$gid,$dev, $size, $atime, $mtim, $ctime, $blksize,$blocks) = stat($file);
      $TotalSize += $size;
      my $i = 0;
      if ($keep > 0) {$i = $n%$keep;}
      if ($i != 1) {print "rm    $file\n";}
      else         {print "\#keep $file\n";}
    }
  }
  my $TB = $TotalSize/1024/1024/1024/1024;
  print "\# $trig total runs $#runs";# => $CosmicList{$trig}\n";
  print " $trig total files = $nodaq, total size = ";
  printf ("%8.3f TB, keep each %3i file\n", $TB,$keep);
}
# rm ^TB
# CosmicFF total runs 45 CosmicFF total files = 2044, total size =    1.101 TB, keep each   2 file
# CosmicRF total runs 229 CosmicRF total files = 8084, total size =    5.321 TB, keep each  10 file
# CosmicZF total runs 11 CosmicZF total files = 240, total size =    0.158 TB, keep each   0 file
# Cosmic_gmt_aRF total runs 12 Cosmic_gmt_aRF total files = 420, total size =    0.243 TB, keep each  10 file
# Cosmic_gmt_bRF total runs 21 Cosmic_gmt_bRF total files = 540, total size =    0.263 TB, keep each  10 file
# Cosmic_gmt_cRF total runs 20 Cosmic_gmt_cRF total files = 620, total size =    0.319 TB, keep each  10 file
# Cosmic_gmt_dRF total runs 8 Cosmic_gmt_dRF total files = 280, total size =    0.185 TB, keep each  10 file
# Cosmic_gmt_eRF total runs 3 Cosmic_gmt_eRF total files = 120, total size =    0.092 TB, keep each  10 file
# Cosmic_gmt_fRF total runs 1 Cosmic_gmt_fRF total files = 80, total size =    0.072 TB, keep each  10 file

