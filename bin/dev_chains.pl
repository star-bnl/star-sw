#! /bin/env perl 
use File::Basename;
use Cwd;
my $vers = "";#new";
my $glob = "";
if (! $vers) {
  ##--------
  my $today = (Sun,Mon,Tue,Wed,Thu,Fri,Sat)[(localtime)[6]];
  my ($thissec, $thismin, $thishour, $thismday, $thismon, $thisyear, $thiswday, $thisyday, $thisisdst) = localtime();
  #  print "this year = $thisyear, mon = $thismon, day=$thismday\n";
  #print "$today\n";
#  $today = "Sun";
 # $today = "Sat";
  $glob = "/star/rcf/test/dev/*.ittf/" . $today . "/*/*/*.log";
} else {
##--------
#my $glob = "/star/data06/ITTF/RefBFCComparision/*/*/*/*/*.log";
  $glob = "/star/rcf/test/new/*/*/*/*.log";
}
my $CWD = cwd(); print "CWD = $CWD\n";
my $star_level = File::Basename::basename($CWD); print "star_level = $star_level\n";
my $gcc_vers = "gcc451";
my @Files = glob $glob;# print "Files = @Files\n";
#my @skip = qw( year_1h_central_daq_sl302 year_1h_minbias_daq_sl302 );
my $ChainDef = "ChainDef.h";
open(OUTC, ">$ChainDef") or die "Can't open $ChainDef";

foreach my $file (@Files) { 
  print "$file\n";
  #  next if $file !~ /pds0200_04_12812evts\.log/;
#  next if $file !~ /sl302/;
#  next if $file =~ /icc80/ or $file =~ /_opt/;
#  next if $file =~ /Insure/;
#  next if $file =~ /ppl/;
#  next if $file =~ /(year_1h_central_daq_sl302|year_1h_minbias_daq_sl302)/;
  print "================> $file\n";
  my ($dev,$ino,$mode,$nlink,$uid,$gid,$dev, $size, $atime, $mtim, $ctime, $blksize,$blocks) = stat($file);
##--------
##  print "dev,ino,mode,nlink,uid,gid,dev, size, atime, mtim, ctime, blksize,blocks\n
##    $dev,$ino,$mode,$nlink,$uid,$gid,$dev, $size, $atime, $mtim, $ctime, $blksize,$blocks\n"; 
##  print "atime = $atime, mtim=$mtim, ctime=$ctime\n";
#  my ($sec, $min, $hour, $mday, $mon, $year, $wday, $yday, $isdst) = localtime($ctime);
##  print "year = $year, mon = $mon, day=$mday\n";
##  print "thisyear = $thisyear file year = $year\n";
#  next if $thisyear ne $year and $thismon ne $mon;
##--------
  my $Test = "Test";
  if ($file =~ /trs/) {$Test .= ".MC";}
  open(IN,$file) or die "Can't open $file";
  my $line;
  while ($line = <IN>) {
    next if $line !~ 'Processing bfc.C';
    $line =~ s/Processing //;
    $line =~ s/\.\.\.//;
    $line =~ s/bfc.C\C//;
    chomp($line);
    print "$line\n"; 
    my ($N,$res) = split ',', $line; print "N = $N\n";
    my ($dum1,$Chain,$dum2,$input) = split '"', $line; print "Chain = $Chain input = $input\n";
#    $line =~ s/\(\d*/\(10/; # no of events
#    $line =~ s/ITTF/ITTF\,evout/;
#    $line =~ s/ittf/ittf\,evout/;
#    print "$line\n";
    my $dir = File::Basename::dirname($file); print "dir = $dir\n";
    my $fileb = File::Basename::basename($file);
#    print "$file $fileb\n";
    my @words = split '/', $file;
    my $nw = $#words; 
    my $w0 = $words[$nw-4];
    my $w1 = $words[$nw-2];
    my $w2 = $words[$nw-1];
    if ($vers) {
      $w0 = $words[$nw-3];
      $w1 = $words[$nw-2];
      $w2 = $words[$nw-1];
    }
    my $ldir = $w1 ."/". $w2;# ."/" . $w0; #print "$file => $w1 $w2 $w0\n"; die;
    $ldir =~ s/_sl302//;
    $ldir =~ s/_daq//;
    $ldir =~ s/trs/sim/;
    $ldir =~ s/\.ittf//;
    my $lfileb = $fileb . ".ref"; if ($vers) {$lfileb .= "." . $vers;}
    my $lfile = $ldir . "/" . $lfileb;
#    print "nw = $nw  $w1 $w2 => $ldir\n" ;
#    die;
    if (! -d $ldir) {`mkdir -p "$ldir"`;}
    if (  -r $lfile) {`cd $ldir; rm $lfileb;`;}
    `cd $ldir; ln -s $file $lfileb;`;
    my @Files = glob "$dir" . "/.fz*"; print "in $dir Files = @Files\n";
    if ($#Files >= 0) {`cd $ldir; ln -s $dir/*.fz*  .;`;}
#    print "else if (Chain == \"" . $w1 . "_" . $w2 ."\")";
#    print "\t$line;\n";
    my $chain  = $Test ."." . $ldir;
    $chain =~ s|\/|_|g;
    my $script =  $chain;# $ldir . "/" . File::Basename::basename($ldir);
    if ($vers) {$script .= "." . $vers;}
    $script .= ".csh";
    print "script = $script\n";
    if (! -r  $script) {
      print "Create $script\n";
      open(OUT, ">$script") or die "Can't open $script";
      print OUT "#! /usr/local/bin/tcsh -f\n";
#      print OUT "setenv StarEndMakerShell\n";
#      if ($star_level) {print OUT "starver $star_level\n";}
#      if ($gcc_vers)   {print OUT "setup $gcc_vers\n";}
      print OUT "cd $ldir\n";
#      my $cmd = "root4star -q -b  '" . $line ."' >& " . $fileb;
      my $cmd = "root4star -q -b  'bfc.C(" . $N . ",\"" . $chain ."\")' >& " . $fileb;
      print OUT "$cmd\n";
      close(OUT);
      close (IN);
#  {"Test.year_1h_hc_standard","","","trs,mdc3,v0,xi,big,evout,-dstout,fzin","","","/star/rcf/"
#   "simu/cocktail/hadronic/default/standard/year_1h/half_field/hadronic_on/Gstardata/hc_standard.40_evts.fz"
#   ,                                                                                                kFALSE},
#      print '
#   {"'. $chain .'","","","'. $Chain .'","","",
#   "' . $input . '"
#   ,                                                                                                kFALSE},
#';
      print OUTC '
  {"'. $chain .'","","","'. $Chain .'","","",
   "' . $input . '"
   ,                                                                                                kFALSE},
';
      last;
    }
  }
}
close(OUTC);
