#!/usr/bin/env perl
use File::Basename;
my @hpss_files = ();
my @disk_files = ();
my %files = {};
my $HPPS_list = "/afs/rhic/star/users/fisyak/public/HPPS_statistics/P00hi.list";
my $DISK_lust = "/afs/rhic/star/users/fisyak/public/HPPS_statistics/P00hi_disk.list";
open (HPSS,"$HPPS_list") or die "Can't open $HPPS_list";
my $SET = "/" . $ARGV[0] . "/";# print "SET = |$SET|\n";
my $HPSS_DIR = "";
my $i = 0;
my $line;
while ($line = <HPSS>) {
  next if !$line eq "\n";
  chop($line);
  if ($line =~ 'home')  {
    ($HPSS_DIR = $line) =~ s/://; 
#    print "HPSS_DIR = $HPSS_DIR\n"; 
    $HPSS_DIR .= "/";
    next;
  } 
  next if !$HPSS_DIR;
  my @words = split ' ',$line;
  my $f = $words[8];
  next if !$f;
  if ($SET ne '//') {next if $HPSS_DIR !~ $SET;} 
#  print "File = $words[8]\n";
  push @hpss_files, $HPSS_DIR . $words[8];
  $i++;
#  goto END if $i > 100;
} 
END: 
close (HPSS);
my @dirs = glob "/star/data*/reco";# print "dirs = @dirs\n";
my $DISK_DIR = "";
my $old = "";
my $o = 0;
while (my $fin = pop @hpss_files) {
#  print "$fin \n";
  my $fout = $fin;
  $fout =~ s|/home/starreco/reco||;# print "fout = $fout\n";
  (my $tag = $fout) =~ s|\..*\.root||;# print "tag= $tag\n";
  if ($old ne $tag) {
    $DISK_DIR = "";
    $old = $tag;
  }
  my $Fout; 
  if (!$DISK_DIR) {
    foreach my $dir (@dirs) {
      $Fout = $dir .  $fout;# print "Fout = $Fout\n"; 
      if (-r $Fout) {
	$DISK_DIR = $dir; 
	#	print "DISK_DIR = $DISK_DIR\n"; 
	goto enddirl;
      }
    }
  enddirl:
  }
  my $disk_dir = "";
  if ($SET ne '//') {
    next if $DISK_DIR;
    if ($fin =~ "/08/") {$disk_dir = "/star/data07/reco";}
    if ($fin =~ "/09/") {$disk_dir = "/star/data09/reco";}
  }
  else {next if !$DISK_DIR; $disk_dir = $DISK_DIR;}
  next if !$disk_dir;
  $Fout = $disk_dir .  $fout;
  next if -r $Fout;
  next if $Fout =~ "\.dst.root" && $Fout !~ "raw_0001" ;
  print "pget $fin $Fout\n";
#  print "dst_get.pl $fin $Fout&\n";
#  $o++;
#  if ($o>=10) {$o = 0; print "sleep 600\n";}
}
