#!/usr/bin/env perl
use File::Basename;
my @datadisks = qw(rcf);
my $hist = "FlowP01gl"; #3 use library version with scale 560/335 + flow pico 
$TOPSW = "FlowP01gl";
my $script = "";
$script = $TOPSW;# . "O";
#  my $SCR = "/star/rcf/disk1/star/fisyak/";
my $SCR = "/star/data05/calib/fisyak/";
$scr = $SCR . $hist . "/";
my $glb = "/star/*/reco/*/*/P01gl/2001/*/*.event.root";
my @Files = glob "$glb"; 
print "Files $glb:  noFiles = $#Files\n";
my $count = 0;
my $opened = 0;
my $fileno = 0;
foreach my $file (@Files) {  
  next if $file =~ /FieldOff/; 
  my $dir = File::Basename::dirname($file);
  #    my $f = File::Basename::basename($file);
  my $f = $file;            #print "$f";
  $f =~ s|/star/data\d*/reco/||; #print "==> $f";
  $f =~ s|_raw_|_|;         #print " ==> $f\n";
  $f =~ s|P01gl/2001/\d*/||;print " ==> $f\n"; 
  $f =~ s|\.root||;
  foreach my $ff (@files) {goto ENDL if $ff eq $f;}
  push @files, $f;
  (my $ff = $f) =~ s|st_physics_||;
  #    $ff =~ s|_raw_0001||;
  $ff =~ s|\.dst||g;
  $ff =~ s|\.event||g;
  $ff =~ s/rcf//g; $ff =~ s/evts//;
  
  my $log = $scr . $ff . ".log";
  my $root = $scr . $ff . ".root";
  next if -r $root;
  # switch between all and 1-st one
  if ($opened) {# and $count == 50) {
    close (OUT); $opened = 0;  $count = 0;
  }
  if (! $opened) {
    my $ss = File::Basename::basename($ff);
    my $SCRIPT = $script . $ss . ".csh";
    print "Create $SCRIPT\n";
    open (OUT,">$SCRIPT") or die "Can't open $SCRIPT";
    $opened = 1;
    print OUT "#! /usr/local/bin/tcsh -f\n";
    print OUT "source /afs/rhic/star/group/star.dev; cd ~/.dev/Flow\n"; 
  }
  my $dd = File::Basename::dirname($root);
  print OUT "if (! -d $dd) mkdir -p $dd\n";
  $fileno++;
  my $cmd = "root4star -q -b 'Flow.C(10000,\"" . $file . "\",\"" . $root . "\")\' >& $log";
  
  print OUT "$cmd\n";
  $count++;
 ENDL:
}
if ($opened ) {
  close (OUT); $opened = 0;
}
