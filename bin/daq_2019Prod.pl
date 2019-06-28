#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = cwd();
my $day = File::Basename::basename(File::Basename::dirname($pwd));
my $run =  File::Basename::basename($pwd);
#print "$pwd => $day => $run\n";
#my @list = glob "/star/data03/daq/2019/15[4-9]/*/st_physics*.daq";
#my @list = glob "/gpfs01/star/scratch/fisyak/daq/2019/*/*/st_physics*.daq";
#
#		
my $debug = 0;
if ($#ARGV >= 0) {
  $debug = $ARGV[0]; print "debug $debug\n" if ($debug);
}
my @globs = ("/gpfs01/star/daq/2019/" . $day . "/" . $run . "*/st_physics*.daq"); 
#my @globs = qw(
#		/gpfs01/star/daq/2019/131/*/st*.daq
#		/gpfs01/star/daq/2019/130/*/st*.daq
#	     ); 
#my @globs = qw(/gpfs01/star/daq/2019/089/15089024/st_physics_15089024_raw_1000025.daq);
my $t = 2000000;
my $step  =  5000;
#for (my $first =  1; $first < 200; $first += $step) {
my $first =  1;
my $last  =  $first + $step - 1;
#print "first = $first, last = $last\n" if ($debug);
my $count = 0;
#________________________________________________________________________________
sub SPrint ($$$$) {
  my ($line,$file,$f,$l) = @_;
  if ($l - $f >= 2) {# Allow to have error in no. of events in FileCatalog
    my $rootf = $file . "_" . $f . "_" . $l . ".picoDst.root"; 
    if (-r $rootf) {
#      print "rootf = $rootf\n" if (debug);
      next;
    }
#    my $blaf  = $file . "_" . $f . "_" . $l . ".bla.root";
#    if (-r $blaf) {
#      print "$blaf\n" if (debug); 
#      next;
#    }
#    if (! $debug) {`touch $blaf`;}
    my                       $chain = "P2019a,-hitfilt,mtd,btof,BEmcChkStat,CorrY,OSpaceZ2,OGridLeak3D,-evout,NoHistos,noTags,noRunco,Stx,KFVertex,VFMinuitX,picoWrite,PicoVtxVpdOrDefault";
    if ($run == "20093001") {$chain = "P2019a,-hitfilt,mtd,btof,BEmcChkStat,CorrY,OSpaceZ2,OGridLeak3D,evout,NoHistos,noTags,noRunco,Stx,KFVertex,VFMinuitX,picoWrite,PicoVtxVpdOrDefault";}
    print "string:$line:$f:$l:$chain\n";
    #  }
    $count++;
  }
}
# sort
#________________________________________________________________________________
sub FirstEvent($){
  my $file = @_[0];# print "@_[0] => file = $file\n";
  $file =~ s/adc_//;
  my @words = split('_',$file);# print "file = $file; words = @words\n";
  my $fa = $words[5];
  #      my ($dummy,$dummy,$dummy,$dummy,$dummy,$fa,$dummy) = split('_',$_);
#  print "FirstEvent: $file = $fa\n";
  return $fa;
}
#________________________________________________________________________________

sub sortByFirstEvent {
  #      my ($dummy,$dummy,$dummy,$dummy,$dummy,$fa,$dummy) = split('_',$a);
  #      my ($dummy,$dummy,$dummy,$dummy,$dummy,$fb,$dummy) = split('_',$b);
  #      print "sortByFirstEvent: $a, $b => $fa => $fb\n";
  FirstEvent($a) <=> FirstEvent($b);
}
#________________________________________________________________________________
foreach my $glob (@globs) {
  my @list = glob $glob;
  my $fNo = 0;
  foreach my $line (@list) {
    $fNo++;
#    chop($line);
    my $file = File::Basename::basename($line,".daq");
    print "----------------------------------------\n" if ($debug);
    my $cmd = "get_file_list.pl -keys 'events' -cond 'filetype=online_daq,filename=" . $file . ".daq' -limit 1";# print "$cmd\n" if ($debug);
    my $N = `$cmd`; chomp($N); 
    print "$file N = $N" if ($debug);
#    if (! $N) {$N = `strings $line | grep EventSummary | wc -l`; chomp($N); print "\tfrom strings N = $N" if ($debug);}
    print "\n"  if ($debug);
    $first = 1;
    $last = $N;
    my $f = $first;
    my $l = $last;
    my $lC = $f;
    my $fC = $l;
    my @filesU = glob $file . "*.picoDst.root";# print "filesU  = @filesU\n" if ($debug);
    my $NF = $#filesU + 1; #print "NF = $NF\n";
    my @files = @filesU;# print "files = @files\n" if (debug);
    for (my $i = 0; $i < $NF; $i++) {
      for (my $j; $j < $i; $j++) {
	my $fj = FirstEvent($files[$j]);# print "fj = $fj\n" if (debug);
	my $fi = FirstEvent($files[$i]);# print "fi = $fi\n" if (debug);
	if ($fi < $fj) {
#	  print "before swap j = $j $files[$j] i = $i $files[$i]\n";
	  my $temp = $files[$i];
	  $files[$i] = $files[$j];
	  $files[$j] = $temp;
#	  print "after swap j = $j $files[$j] i = $i $files[$i]\n";
	} 
      }
    }
#    my $files = sort sortByFirstEvent @filesU; 
#    print "files  = @files\n" if ($debug);
    if ($debug) {
#       print "Unsorted\n";
#       foreach my $file (@filesU) {
# 	my $fa = FirstEvent($file);
# 	print "$file => fa = $fa\n";
#       }
#       print "Sorted\n";
       foreach my $file (@files) {
# 	my $fa = FirstEvent($file);
 	print "$file\n"; # => fa = $fa\n";
       }
    }
#    die;
    # check repeats
    my $f0 = 0;
    my @F = ();
    my @L = ();# print "initialization : $#F\n";
    foreach my $mu (@files) {
      my $base = File::Basename::basename($mu,".picoDst.root");
      $base =~ s/_adc//;
#      print "$base\n" if ($debug);
      my ($dummy,$dummy,$dummy,$dummy,$dummy,$f1,$l1) = split('_',$base); #print "f1 = $f1, l1 = $l1\n" if ($debug);
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
      if ($F[0] > 2) {
	$f = 1;
	$l = $F[0] - 2; # reject last event
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
  }
}
exit $count;
