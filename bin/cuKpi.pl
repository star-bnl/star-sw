#!/usr/bin/env perl 
use File::Basename;
use Sys::Hostname;
use Cwd;
#print "---------\n";
#my @List = `get_file_list.pl -keys path,filename -cond 'production=P07ic,trgsetupname=cuProductionMinBias,filetype=daq_reco_MuDst,storage=NFS' -limit 20`;
my $glb = "/star/data*/reco/*/*/P07ic/2005/*/st_phys*.MuDst.root";
#my $glb = "/star/data12/reco/cu62productionMinBias/FullField/P07ic/2005/*/st_phys*.MuDst.root";
my @List = glob $glb;
#print "$glb => @List\n";
my %DirHash = ();
foreach my $file (@List) {
  my $dir = File::Basename::dirname($file);
#  my $dst = File::Basename::basename($file);
#  if (! $DirHash{$dir}) {$DirHash{$dir} = $dst;}
#  else                  {$DirHash{$dir} .= "_" . $dst;}
#  chomp($dir);
#  $dir =~ s/\:\:.*$//;
  next if $DirHash{$dir};
  my @words = split '/', $dir;
  next if ($#words < 2);
  my $b = $words[2];
  $b .= "_" . $words[$#words] . ":";
  for (my $i = 4; $i < $#words; $i++) {
    if ($i != 4) {$ .= "_";}
    $b .= $words[$i];
  }
  $DirHash{$dir} = $b;
#  print "$file => $dir =>  $DirHash{$dir}\n";

}
foreach my $dir ( sort keys %DirHash ) {
#  print "$dir -> $DirHash{$dir}\n";
  my ($name,$DIR) = split (':',$DirHash{$dir});
  $root = $name  . ".root";
  if (-r $root) { 
    print "$root already exists, skip.....\n"; 
    next;
  }
#  my $cmd = "bsub -J $b -o $b.log -q star_cas_prod  root.exe -q -b 'MuKpi.C+(\"" . $dir . "/st_phys*.MuDst.root\",\"" . $root . "\")'";
  # system($cmd);
  my $cmd = "test ! -r " . $root ." && root.exe -q -b 'MuKpiC.C+(\"" . $dir . "/st_phys*.MuDst.root\",\"" . $root . "\")'";
  print "$cmd\n"; 
  if (! -d $DIR) {mkdir $DIR;}
  $SCRIPT = $DIR . '/' . $name . ".csh";
  next if -r $SCRIPT;
  open (OUT,">$SCRIPT") or die "Can't open $SCRIPT";
  print "Create $SCRIPT\n";
  print OUT "#! /usr/local/bin/tcsh -f\n";
  print OUT "$cmd\n";
  close (OUT);

}
