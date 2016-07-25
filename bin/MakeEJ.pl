#!/usr/bin/env perl
use File::Basename;
use Cwd;
my $macro = "ErrorAnalysis";
my $debug = 1;
my @trees = qw(SpX SpZ);
foreach my $tree (@trees) {
  my $NN = 7;
  for (my $sec = 1; $sec <= 2; $sec++) {
    for (my $row = 1; $row <= 2; $row++) {
      for (my $p = 1; $p <= $NN; $p++) {
	for (my $t = 1; $t < $NN; $t++) {
	    my $tag = $tree . "_s_" . $sec . "_r_" . $row . "_p_" . $p . "_t_" . $t;
	    my $LOG = $tag . ".log";
	    my $SCRIPT = $tag . ".csh";
	    my $rootf = $tag . "_B_1.root";
	    my $log =  $tag . "_B_1.log";
	    if (! -r $log) {
	      print "Create $SCRIPT\n";
	      open (OUT,">$SCRIPT") or die "Can't open $SCRIPT";
	      print OUT "#! /usr/local/bin/tcsh -f\n";
#	      print OUT "onintr  eviction\n";
	      #	    print OUT "source /afs/.rhic.bnl.gov/star/group/.starver .DEV2;\n";
	      my $cmd = "";
	      $cmd .= "root.exe -q -b " .  $tree . ".root '" . $macro;
	      $cmd .= ".C(\"$tree\",$sec,$row,$p,$t,1,\"$rootf\")' >& $log";
	      print "$cmd\n";
	      print OUT "$cmd\n";
#print OUT "
#eviction:     
#set PIDS=`ps  -o \"%p\" --no-headers --ppid \$\$`
#foreach  process (\$PIDS)
#  set killit=`ps -o \"%p\" --no-headers -p \$process`
#  if ( \"x\$killit\" !~ \"x\" ) then
#     echo \"evicting `ps  --no-headers -p\$process`\"
#     kill -15 \$process
#  endif
#end
#";	      
	      close (OUT);
	    }
	  }
      }
    }
  }
}

