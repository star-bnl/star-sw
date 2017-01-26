#! /usr/bin/env perl
use File::Basename;
use Sys::Hostname;
use Cwd;
use Env;
my $DIR = Cwd::cwd();
# list of all histogram to be fitted
$ENV{STARFPE} = "NO";
my @histGF = ();
my @histGP = ();
my @histRL5 = ();
my @histNF = ();
my @histXF = ();
my @rootfiles = ();
my $all = 0;
#my $all = 1;
#@histGF = qw (VoltageC);
foreach my $arg (@ARGV) {
  if ($arg =~ /\.root$/) {push @rootfiles, $arg; next;}
}
if (! $all and $#histGF < 0 and $#histGP < 0 and $#histRL5 < 0 and $#histNF < 0) {
  @histGF = qw (SecRow3 SecRow3C 
		PressureC 
		Time TimeC
		Voltage VoltageC Qcm AvCurrent QcmC AvCurrentC
		ppmOxygenInP  inputGasTemperatureP 
		flowRateExhaustP flowRateRecirculationP
		percentMethaneInP percentMethaneInPA percentMethaneInPC
		ppmWaterOutP ppmWaterOutPA ppmWaterOutPC
		Z3C 
		dX3C TanL3DC Edge3
	      ); # PressureTC Volt VoltC Z3OC Zdc3C 
  @histRL5 = @histGF;
  @histGP = qw (
		 TPoints70 TPointsF TPoints70U TPointsFU  TPointsN TPointsNU
		 I70eP I70eN I70protonP I70protonN I70kaonP I70kaonN I70piP I70piN I70muP I70muN I70deuteronP I70deuteronN I70tritonP I70tritonN I70He3P I70He3N I70alphaP I70alphaN 
		 fitZeP fitZeN fitZprotonP fitZprotonN fitZkaonP fitZkaonN fitZpiP fitZpiN fitZmuP fitZmuN fitZdeuteronP fitZdeuteronN fitZtritonP fitZtritonN fitZHe3P fitZHe3N fitZalphaP fitZalphaN 
		 fitNeP fitNeN fitNprotonP fitNprotonN fitNkaonP fitNkaonN fitNpiP fitNpiN fitNmuP fitNmuN fitNdeuteronP fitNdeuteronN fitNtritonP fitNtritonN fitNHe3P fitNHe3N fitNalphaP fitNalphaN 
		 PullI70 PullIfit PullNfit
	      ); 
  @histNF = qw(PressureN VoltageN AvCurrentN QcmN Z3N SecRow3N dX3N TanL3DN Edge3N); #Edge3N PressureTN VoltN Zdc3N  Z3ON 
#  @histXF = @histNF;
}
print "fit.pl for  @rootfiles \n";
if ($#histGF >= 0) {print " with GF: @histGF \n";}
if ($#histGP >= 0) {print " with GP: @histGP \n";}
if ($#histRL5 >= 0){print " with RL5:@histRL5\n";}
if ($#histNF >= 0) {print " with NF: @histNF \n";}
if ($#histXF >= 0) {print " with NF: @histXF \n";}
exit if $#rootfiles < 0;
my @opt = qw (GF GP NF);# XF);# RL5);
my $XML = "fit.xml";
open (XML,">$XML") or die "Can't open $XML";
print XML '<?xml version="1.0" encoding="utf-8" ?>
<job name="dEdxFit" maxFilesPerProcess="1" simulateSubmission="false" fileListSyntax="paths">
	 <command>
setenv NODEBUG yes
starver .DEV2
csh -x $INPUTFILE0
         </command>
	<stdout URL="file:/star/u/fisyak/prodlog/shed$JOBID.log" />
';
foreach my $rootfile (@rootfiles) {
  foreach my $fitype (@opt) {
    my @histos = ();
    my $NoSectors =  -1;
    if    ($fitype eq 'GP') {@histos = @histGP;}
    elsif ($fitype eq 'GF') {@histos = @histGF;}
    elsif ($fitype eq 'RL5'){@histos = @histRL5;}
    elsif ($fitype eq 'NF') {
      @histos = @histNF; 
			     $NoSectors = 24;}
    elsif ($fitype eq 'XF') {@histos = @histNF;} 
    else {next;}
    print "FitType : $fitype , histos = @histos\n";
    foreach my $hist (@histos) {
      my $sec1 = -1;
      my $sec2 = -1;
#      if ($fitype eq 'NF') {
#	$sec1 = 0;
#	if ($hist eq 'SecRow3N') {
#          $sec2 = 24;
#	} else {
#	  $sec2 = 45;
#	}
#      }
      for (my $sec = $sec1; $sec <= $sec2; $sec++) {
	my $ext = "";
	if ($sec >= 0) {$ext = "_X" . $sec;}
	my $dir = File::Basename::dirname($rootfile);
	my $fil = File::Basename::basename($rootfile);
	my $SCRIPT = $hist . $fitype . $ext . $fil;
	$SCRIPT =~ s/\.root/\.csh/;
	my $newfilew = $dir . "/" . $hist . $fitype . $ext . $fil;
	print "$rootfile -> $hist => $fitype new file: $newfilew\n";
	next if -r $newfilew;
	my $log = $newfilew;
	$log =~ s/\.root/\.log/;
	#    my $cmd = "bsub -o " . $log ." -q star_cas_big ";
	my $rootcmd =  "root.exe -q -b lBichsel.C " . $rootfile;
	$rootcmd .= " 'dEdxFit.C+(\"";
	$rootcmd .= $hist;
	$rootcmd .= "\",\"" . $fitype ;
	if ($sec >= 0) {
	  $rootcmd .= "\",\"R\"," . $sec . ")'";
	} else {
	  $rootcmd .=	"\")'";
	}
	$rootcmd .= " >& " . $log;
	print "job: $jobs : $rootcmd \n";
	print "Create $SCRIPT\n";
	open (OUT,">$SCRIPT") or die "Can't open $SCRIPT";
	print XML "<input URL=\"file:" . $DIR . "/" .  $SCRIPT ."\" />\n";
	print OUT "#!/bin/tcsh -v\n";
	print OUT "cd $DIR\n";
	print OUT "setenv STARFPE NO\n";
	print OUT "setenv NODEBUG yes\n";
	print OUT "setup 64b\n";
	#      print OUT "setup gcc\n";
	print OUT "starver .DEV2\n";
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
