#!/usr/bin/env perl
use File::Basename;
use Cwd;
my @list = `ls -1 V*.root`;
my %List = ();
my $Dir = File::Basename::basename(cwd());
foreach my $f (@list) {
  chomp($f);
  my ($v1,$v2) = split '_', $f;
  my $tag = $v1 . "_" . $v2;
#  print "$f => $tag\n";
  if ($List{$tag}) {$List{$tag} .= " " . $f;}
  else             {$List{$tag}  =       $f;}
}
foreach my $tag ( sort keys %List) {
#  print "$tag => $List{$tag}\n";
  my @files = split ' ', $List{$tag};
  my $cmd;
  if ($#files == 0) {$cmd = "mv $files[0] $Dir" . "_" . $tag . ".root";}
  else              {$cmd = "hadd $Dir" . "_" . $tag . ".root " .  $List{$tag};}
#  else              {$cmd = "hadd $Dir" . "_" . $tag . ".root " .  $List{$tag} . "; if (\$? == 0) rm " . $List{$tag};}
  print "$cmd\n";
  my $flag = system($cmd);
}
