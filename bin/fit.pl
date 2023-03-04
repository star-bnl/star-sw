#! /usr/bin/env perl
use File::Basename;
use Sys::Hostname;
use Cwd;
use Env;
my $DIR = Cwd::cwd();
# list of all histogram to be fitted
if (! -r .sl73_x8664_gcc631) {`ln -s ~/macros/.sl* .`;}
$ENV{STARFPE} = "NO";
my @histG  = ();
my @histGF = (); #SecRow3C);
my @histGP = ();
my @histG0P = ();
my @histRL5 = ();
my @histNF = ();
my @histXF = ();
my @histADC = ();
my @histG4F = ();
my @histG4E = ();
my @histG4EX = ();
my @histG4EY = ();
my @histGG = ();
my @histGG2 = ();
my @histGG3 = ();
my @rootfiles = ();
my $all = 1;
my $time = 0;
my $adc = 0;
my $debug = 0;
my @FitOptions = qw (G GF GDP RL5 NF NX ADC G4F G4E G4EX G4EY G4EG GG GG2 GG3 GG4 GP);
my %Opts  = ();
my @opt;
foreach my $arg (@ARGV) {
  print "arg = $arg\n" if ($debug);
  if ($arg =~ /\.root$/) {
    my $rootf = File::Basename::basename($arg); print "rootf = $rootf\n" if ($debug);
    if ($rootf =~ /^Av/  || 
	$rootf =~ /^fit/ || 
	$rootf =~ /^G3/  || 
	$rootf =~ /^Z3/  || 
	$rootf =~ /^dX3/ ||
	$rootf =~ /^NP/  ||
	$rootf =~ /^Pre/ ||
	$rootf =~ /^Qcm/ ||
	$rootf =~ /^Eta/ ||
	$rootf =~ /^Sec/ || 
	$rootf =~ /^Time/ || 
	$rootf =~ /^TP/ || 
	$rootf =~ /^Vol/ || 
	$rootf =~ /^xy/) {next;}
    push @rootfiles, $arg;      
    print "$#rootfiles $arg\n";
    next;
  } 
  if ($arg =~ /^time/) {$time = 1;} 
  if ($arg =~ /=/) {
    $_ = $arg;
    my($key, $val) = /([^=]*)=(.*)/; 
    foreach my $opt (@FitOptions) {
      if ($key eq $opt) {
	my @values = split(',',$val);
	foreach my $v (@values) {
	  push @{ $Opts{$key} }, $v;
	}
#	print "$arg : key = $key => val = $val => @{ $Opts{$key} } \n";
	$all = 0;
      }
    }
  }
}

################################################################################
if ($all) {
#  push  @{ $Opts{GG} }, qw(dNdxVsBgC dNdxVsBgCI dNdxVsBgCO); 
  push  @{ $Opts{G4EY} }, qw(
			      SecRow3 SecRow3P SecRow3+SecRow3P SecRow3C SecRow3PC SecRow3C+SecRow3PC
			      Z3 Z3P  Z3+Z3P  Z3C+Z3P 
			      Z3C Z3PC Z3C+Z3PC 
			      xyPad3qB xyPad3qBC
			      xyPad3+xyPad3P xyPad3C+xyPad3PC
			      xyPad3 xyPad3P xyPad3C xyPad3PC 
			      Pressure PressureP Pressure+PressureP PressureC PressurePC PressureC+PressurePC
			      dX3 dx3C
			   );
#   push  @{ $Opts{G4EG} }, qw(
# 			      SecRow3 SecRow3P SecRow3+SecRow3P SecRow3C SecRow3PC SecRow3C+SecRow3PC
# 			      Z3+Z3P  Z3C+Z3PC 
# 			      xyPad3+xyPad3P xyPad3C+xyPad3PC
# 			      xyPad3 xyPad3P xyPad3C xyPad3PC 
# 			      Pressure PressureP Pressure+PressureP PressureC PressurePC PressureC+PressurePC
# 			   );
 push  @{ $Opts{GP} },  qw (TPoints70+TPoints70P TPointsF+TPointsFP TPoints70U+TPoints70UP TPointsFU+TPointsFUP  TPointsN+TPointsNP TPointsNU+TPointsNUP
	       NPoints70+NPoints70P NPointsF+NPointsFP NPoints70U+NPoints70UP NPointsFU+NPointsFUP  NPointsN+NPointsNP NPointsNU+NPointsNUP
               TPoints70 TPoints70P TPointsF TPointsFP TPoints70U TPoints70UP TPointsFU TPointsFUP  TPointsN TPointsNP TPointsNU TPointsNUP
	       NPoints70 NPoints70P NPointsF NPointsFP NPoints70U NPoints70UP NPointsFU NPointsFUP  NPointsN NPointsNP NPointsNU NPointsNUP
               xyPad3 xyPad3P xyPad3+xyPad3P
	      );
}
print "fit.pl for  @rootfiles \n"; 
my $no = 0;
foreach my $opt ( keys %Opts) {
  $no++;
  print "$no\t:$opt for @{ $Opts{$opt} }\n";
}
exit if $#rootfiles < 0 || $no <= 0;
my $XML = "fit.xml";
open (XML,">$XML") or die "Can't open $XML";
print XML '<?xml version="1.0" encoding="utf-8" ?>
<job  maxFilesPerProcess="1" simulateSubmission="false" fileListSyntax="paths" copyInputLocally="false">
	 <command>
setup 64b
setenv NODEBUG 
starver .DEV2
csh -x $INPUTFILE0
         </command>
	<stdout URL="file:./sched$JOBID.log" />
';
foreach my $rootfile (@rootfiles) {
  my $dirname =  File::Basename::dirname($rootfile); 
  if ($dirname eq '.')  {$dirname = "";}
  else                  {$dirname .= "_";}
#  print "$rootfile => $dirname\n";
  foreach my $fittype ( keys %Opts) {
#    print "$fittype => @{ $Opts{$fittype} }\n";
    foreach my $hist (@{ $Opts{$fittype} }) {
#      print "hist = $hist\n";
      my $sec1 = -1;
      my $sec2 = -1;
      for (my $sec = $sec1; $sec <= $sec2; $sec++) {
	my $ext = "";
	if ($sec >= 0) {$ext = "_X" . $sec;}
#	if ($hist =~ /Edge/ || $hist =~ /xyPad/) {$ext = "_y3";}
	my $dir = File::Basename::dirname($rootfile);
	my $fil = File::Basename::basename($rootfile);
	my $SCRIPT = $dirname . $hist . $fittype . $ext . $fil;
	$SCRIPT =~ s/\.root/\.csh/;
	my $newfilew = $dir . "/" . $hist . $fittype . $ext . $fil;
#	print "$rootfile -> $hist => $hist fittype = $fittype ext = $ext fill = $fil new file: $newfilew\n";
	next if -r $newfilew;
	my $log = $newfilew;
	$log =~ s/\.root/\.log/;
	#    my $cmd = "bsub -o " . $log ." -q star_cas_big ";
	my $rootcmd =  "root.exe -q -b lBichsel.C " . $rootfile;
	$rootcmd .= " 'dEdxFit.C+(\"";
	$rootcmd .= $hist;
	$rootcmd .= "\",\"" . $fittype ;
	if ($sec >= 0) {
	  $rootcmd .= "\",\"R\"," . $sec . ")'";
	} else {
	  if ($ext eq "_y3") {
	    $rootcmd .= "\",\"R\",-1,-1,1,3)'";
	  } else {
	    $rootcmd .=	"\")'";
	  }
	}
	$rootcmd .= " >& " . $log;
	print "job: $jobs : $rootcmd \n";
	print "Create $SCRIPT\n";
	open (OUT,">$SCRIPT") or die "Can't open $SCRIPT";
	print XML "<input URL=\"file:" . $DIR . "/" .  $SCRIPT ."\" />\n";
	print OUT "#!/bin/tcsh -v\n";
	print OUT "cd $DIR\n";
	print OUT "setenv STARFPE NO\n";
#	print OUT "setenv NODEBUG yes\n";
#	print OUT "setup 64b\n";
	#      print OUT "setup gcc\n";
#	print OUT "starver .DEV2\n";
	my $cmd = "test ! -r " . $newfilew . " && " . $rootcmd;
	print OUT "$cmd\n";
	close (OUT);
      }
    }
  }
}
print XML '
</job>
';
close(XML);
exit 0;
__END__
