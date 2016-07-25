#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = cwd();
my $day = File::Basename::basename(File::Basename::dirname($pwd));
my $run =  File::Basename::basename($pwd);
#print "$pwd => $day => $run\n";
#my @list = glob "/star/data03/daq/2013/15[4-9]/*/st_physics*.daq";
#my @list = glob "/gpfs01/star/scratch/fisyak/daq/2013/*/*/st_physics*.daq";
#
#		
my $debug = 0;
if ($#ARGV >= 0) {
  $debug = $ARGV[0]; print "debug $debug\n";
}
my @globs = ("/gpfs01/star/daq/2013/" . $day . "/" . $run . "*/st_W*.daq"); 
#my @globs = qw(
#		/gpfs01/star/daq/2013/131/*/st*.daq
#		/gpfs01/star/daq/2013/130/*/st*.daq
#	     ); 
#my @globs = qw(/gpfs01/star/daq/2013/089/15089024/st_physics_15089024_raw_1000025.daq);
my $t = 2000000;
my $step  =  100000;
#for (my $first =  1; $first < 200; $first += $step) {
my $first =  1;
my $last  =  $first + $step - 1;
#print "first = $first, last = $last\n" if ($debug);
my $count = 0;
sub SPrint ($$$$) {
  my ($line,$file,$f,$l) = @_;
  if ($l - $f >= 0) {# Allow to have error in no. of events in FileCatalog
    my $rootf = $file . "_" . $f . "_" . $l . ".MuDst.root"; 
    if (-r $rootf) {print "rootf = $rootf\n" if (debug); next;}
    my $blaf  = $file . "_" . $f . "_" . $l . ".bla.root";
    if (-r $blaf) { print "$blaf\n" if (debug); next;}
    if (! $debug) {`touch $blaf`;}
    my $chain = "";
    if ($day >= 76 && $day <= 126) {
      #             DbV20140222 pp2013a mtd btof fmsDat fgt fgtPoint VFPPVnoCTB beamline BEmcChkStat Corr4 OSpaceZ2 OGridLeak3D -hitfilt
      $chain = "DbV20140222,pp2013a,mtd,btof,fmsDat,fgt,fgtPoint,VFPPVnoCTB,beamline,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D";#,-hitfilt"; #for days 76-126 .
    }
    if ($day >= 129 && $day <= 161) {
      #             DbV20140905 pp2013b mtd btof fmsDat fgt fgtPoint VFPPVnoCTB beamline StiHftP BEmcChkStat Corr4 OSpaceZ2 OGridLeak3D -hitfilt
      #   $chain = "DbV20140905,pp2013b,mtd,btof,fmsDat,fgt,fgtPoint,VFPPVnoCTB,beamline,StiHftP,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D";#,-hitfilt"; #days129-161 (part 2) 
      $chain = "DbV20141125,pp2013b,mtd,btof,fmsDat,fgt,fgtPoint,VFPPVnoCTB,beamline,StiHftP,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D";#,-hitfilt"; #days129-161 (part 2) 
    }
    if ($chain eq "") {die "chain has not defined";}
    $chain .= ",StiCA";
    $chain .= ",noRunco,noHistos,noTags,-evout";
    #  if ($pwd =~ /KF/) {$chain .= ",KFVertex";}
    #  print "chain = $chain\n" if (debug);
    #  if (! $debug) {
    print "string:$line:$f:$l:$chain\n";
    #  }
    $count++;
  }
}
foreach my $glob (@globs) {
  my @list = glob $glob;
  my $fNo = 0;
  foreach my $line (@list) {
    $fNo++;
#    chop($line);
    my $file = File::Basename::basename($line,".daq");
    my $cmd = "get_file_list.pl -keys 'events' -cond 'filetype=online_daq,filename=" . $file . ".daq' -limit 1"; print "$cmd\n" if ($debug);
    my $N = `$cmd`; chomp($N); 
    print "$file N = $N" if ($debug);
    if (! $N) {$N = `strings $line | grep EventSummary | wc -l`; chomp($N); print "\tfrom strings N = $N" if ($debug);}
    print "\n"  if ($debug);
    $last = $N;
    my $f = $first;
    my $l = $last;
    my $lC = $f;
    my $fC = $l;
    my @files = glob $file . "*.MuDst.root"; 
    if ($debug) {
      foreach my $file (@files) {print "$file\n";}
    }
#    die;
    # check repeats
    my $f0 = 0;
    my @F = ();
    my @L = ();# print "initialization : $#F\n";
    foreach my $mu (@files) {
      my $b = File::Basename::basename($mu,".MuDst.root");
      $b =~ s/_adc//;
#      print "$b\n" if ($debug);
      my ($dummy,$dummy,$dummy,$dummy,$dummy,$f1,$l1) = split('_',$b); 
      push @F, $f1;
      push @L, $l1;
    }
    my $No = $#F + 1;
    if ($debug) {
#      print "$No, @F, @L\n";
      for (my $i = 0; $i < $No; $i++) {
#	print "$F[$i]  $L[$i]\n";
	for (my $j = $i+1; $j < $No; $j++) {
	  if ($F[$i] >= $F[$j] && $F[$i] <= $L[$j] ||
	      $L[$i] >= $F[$j] && $L[$i] <= $L[$j]) {
	    print "[$F[$i],$L[$i]] overlap with $F[$j],$L[$j]]\n";
	    die "died at $day/$run";
	  }
	}
      }
    }
    if ($No <= 0) {
      $f = $first;
      $l = $last;
      SPrint($line,$file,$f,$l); $count++;
    } else {
      if ($L[$No-1] < $N) {
	$f = $L[$No-1] + 1;
	$l = $N;
	SPrint($line,$file,$f,$l); $count++;
      } 
      if ($F[0] > $f) {
	$f = 1;
	$l = $F[0] - 1;
	SPrint($line,$file,$f,$l); $count++;
      } 
      for (my $i = 1; $i < $No; $i++) {
#	print "$i  : $F[$i]  $L[$i]\n" if ($debug);
	my $f = $L[$i-1]+1;
	my $l = $F[$i]-1;
	# print "f = $f l = $l\n" if ($debug);
	if ($f <= $l) {
	  SPrint($line,$file,$f,$l); $count++;
	}
      }
    }
    #    #      [f                 l]
#      #      [   f1      l1      ]
#      foreach my $mu (@files) {
#      my $b = File::Basename::basename($mu,".MuDst.root");
      
#      $b =~ s/_adc//;
##      print "$b\n" if ($debug);
#      my ($dummy,$dummy,$dummy,$dummy,$dummy,$f1,$l1) = split('_',$b); 
#      $l1 =~ s/\.MuDst.root//;  
#      print "$mu: f1 = $f1, l1 = $l1\n" if ($debug);
#      if ($fC - $f1 > 0) {$fC = $f1;}
#      if ($lC - $l1 < 0) {$lC = $l1;}
#      print "f1 = $f1, l1 = $l1, fC = $fC, lC = $lC\n" if ($debug);
##      if ($fC <= $lC and $lC != $N and $f1 != $lC+1) {
#      if ($f1 > $fC) {
#	my $fG = $lC+1;
#	my $lG = $f1-1;
#	print "============ gap : f1 = $f1 l1 = $l1 and fC = $fC lC = $lC => [$fG,$lG]\n" if ($debug);
#	if ($fG >= $lG) {next;}
#	for (my $i = $fG; $i < $lG; $i += $t) {
#	  my $j = $i + $t - 1;
#	  if ($j > $lG) {$j = $lG;}
#	  print "$line i = $i j = $j\n" if ($debug);
#	  SPrint($line,$file,$i,$j);
#	}
##	die;
#      }
#      print "$mu => $f1 $l1\n" if ($debug);
#      if ($f >= $f1 && $last <= $l1) {$f = $l1 + 1; next;}
#      if ($f1 >= $f) {
#	$f = $l1 + 1;
#      }
#    }
#    print "f = $f l = $l\n" if ($debug);
#    if ($f > $l) {next;}
#    for (my $i = $f; $i <= $l; $i += $t) {
#      my $j = $i + $t - 1;
#      if ($j > $l) {$j = $l;}
#      SPrint($line,$file,$i,$j);
##      last;
#    }
##    last;
#  }
  }
}
exit $count;
