#! /usr/bin/env perl
use Cwd;
use lib "/afs/rhic/star/users/fisyak/public/.dev";
use JobSubmit;
my $dir =  File::Basename::basename(Cwd::cwd());
print "dir = $dir\n";
#print "ARGV[0] = $ARGV[0]\n";
my $jobs = 0;
my @Files = glob "$ARGV[0]";
my $noFiles = $#Files;
my $glb = $ARGV[0]; #print "$glb\n";
$glb =~ s|\/|_|g; #print "$glb\n";
$glb =~ s/_Event\*.*//g; #print "$glb\n";
print "no of files $glb : $noFiles\n";
my $FilesPerJob = 50;
my $fno = 0;
my $NJB = ($#Files+1)/$FilesPerJob+1;
my $j = 0;
for (my $jb = 1; $jb <= $NJB; $jb++) {
  my $i = 0;
  my @List = ();
  for (; $i< $FilesPerJob && $j <= $noFiles; $i++, $j++) {
    my ($dev,$ino,$mode,$nlink,$uid,$gid,$dev, $size, $atime, 
	$mtim, $ctime, $blksize,$blocks) = stat($Files[$j] );
    next if $size < 500000; # 0.5 MB limit
    push @List, $Files[$j];
  }
  next if  $#List == -1;
  my $list = join ' ', @List;
  my $jj = "p";
  if    ($j <  10) {$jj .= "00" . $j;}
  elsif ($j < 100) {$jj .= "0"  . $j;}
  else             {$jj .=        $j;}
  my $log = $glb . $jj . ".log";
#  my $cmd = " bsub -o $log -N -q star_cas_short";
  my $cmd = " bsub -o $log -N -q star_cas_prod";
#  my $cmd = " bsub -q star_cas_prod";
  $cmd .= " root.exe -q -b " . $list;
  #  $cmd .= " '/afs/rhic/star/users/fisyak/.dev/Hadd.C(\"" . $rootfile . "\")'";
  $cmd .= " 'makeTPlots.C(\"" . $jj . "\")'";
  print "job:$jb files: $i => $cmd \n";
  my $flag = system($cmd);
}

