#!/usr/bin/env perl
use File::Basename;
use Cwd;
my $macro = "TpcT.C+";
my $debug = 1;
my @Runs = ();
my @Files = glob "*tags.root"; # print "Files = @Files\n"; #`ls -1d *tags.root | awk -F_ '{print $4}' | sort -u`; print "Runs = @Runs\n";
foreach my $file (@Files) {
  my $f = File::Basename::basename($file,".tags.root");# print "$f\n";
  my @r = split '_',$f;
#   my $i = 0;
#   my $nr = $#r; print "nr = $n\n";
#   for (my $i = 0; $i <= $nr; $i++) {
#     print "$i\t $r[$i]\n";
#   }
  my $run = $r[3];
#  print "$file = >$ run\n";
  my $Runs = join '|', @Runs; #print "Runs = $Runs\n";
  if ($Runs and $run =~ $Runs) {next;}
  push @Runs, $run;
}
print "Runs = @Runs\n";
foreach my $run (@Runs) { 
  print "run = $run\n";
  my $SCRIPT = "R" . $run;
  my $LOG = $SCRIPT . ".log";
  my $root = $SCRIPT;
  my $RootFile = $root . ".root";
  next if -r $RootFile;
  $SCRIPT .= ".csh";
  print "Create $SCRIPT\n";
  open (OUT,">$SCRIPT") or die "Can't open $SCRIPT";
  #    print OUT "#! /usr/local/bin/tcsh -f\n";
  #    print OUT "source /afs/.rhic.bnl.gov/star/group/.starver .DEV2;\n";
  my $cmd = "";
  $cmd .= "test ! -r " . $RootFile  . " && root -l -q -b  '" . $macro;
  $cmd .= "(\"*" . $run . "*.tags.root\",\"R\",\"" . $RootFile  . "\")\' >& $LOG";
  print "$cmd\n";
  print OUT "$cmd\n";
  close (OUT);
}

