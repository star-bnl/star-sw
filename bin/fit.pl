#! /usr/bin/env perl
use File::Basename;
use Sys::Hostname;
use Cwd;
use Env;
my $DIR = Cwd::cwd();
# list of all histogram to be fitted
`ln -s ~/macros/.sl* .`;
$ENV{STARFPE} = "NO";
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
my @rootfiles = ();
my $all = 0;
#my $all = 1;
#@histGF = qw (VoltageC);
my $time = 0;
my $adc = 0;
my @opt;
foreach my $arg (@ARGV) {
  if ($arg =~ /\.root$/) {
    if ($arg =~ /^Av/  || 
	$arg =~ /^fit/ || 
	$arg =~ /^G3/  || 
	$arg =~ /^NP/  ||
	$arg =~ /^Pre/ ||
	$arg =~ /^Qcm/ ||
	$arg =~ /^Sec/ || 
	$arg =~ /^Time/ || 
	$arg =~ /^TP/ || 
	$arg =~ /^Vol/ || 
	$arg =~ /^xy/) {next;}
    push @rootfiles, $arg;      
  } 
  if ($arg =~ /^time/) {$time = 1;} 
}
# if ($time) {
#   $all = 1; @histGP = qw(Z T TR);
#   @opt = qw(GP);;
#   print "histGP = @histGP\n";
# }
#  # # ADC
#     @histADC = qw(I3DZ I3DTanL I3DdX IC3DZ IC3DTanL IC3DdX I3Dnpads I3Dntmbks I3Dnpadtmbks
#   		O3DZ O3DTanL O3DdX IC3DZ IC3DTanL IC3DdX O3Dnpads O3Dntmbks O3Dnpadtmbks);
#   for (my $i = 1; $i <=13; $i++) {
#     push @histADC, "WI_" . $i;
#     push @histADC, "EI_" . $i;
#     push @histADC, "WO_" . $i;
#     push @histADC, "EO_" . $i;
#   }
#   my @opt = qw (ADC);
#  print "histADC = @histADC\n";
################################################################################
 if (! $all and $#histGF < 0 and $#histG4F < 0  and $#histG4E < 0 and $#histG4EX < 0 and $#histGP < 0 and $#histG0P < 0 and $#histRL5 < 0 and $#histNF < 0 and $#histADC < 0) {
#    @histGF = qw(
#  	       Pressure  Time  Voltage Voltage  Qcm  AvCurrent  Z3  xyPad3 G3
#  	       PressureC TimeC Voltage VoltageC QcmC AvCurrentC Z3C xyPad3C G3C
# 	       SecRow3 SecRow3C
# 	       SecRow3P SecRow3PC
#   	       SecRow3+SecRow3P SecRow3C+SecRow3PC 
# 	       dX3 dX3C
#  	     );
 # 	       SecRow3+SecRow3P SecRow3C+SecRow3PC 
 #             nTbk3C nTbk3PC nTbk3C+nTbk3PC
 #             nPad3C nPad3PC nPad3C+nPad3PC
 #             SecRow3 SecRow3C SecRow3P SecRow3PC 
 # 	       AdcI3 dZdY3 dXdY3  nPad3 nTbk3
 # 	       AdcI3C dZdY3C dXdY3C nPad3C nTbk3C
 # 	       AdcI3P dZdY3P dXdY3P  nPad3P nTbk3P
 # 	       AdcI3PC dZdY3PC dXdY3PC nPad3PC nTbk3PC
 #	       AdcI3P dZdY3P dXdY3P
 #	       AdcI3+AdcI3P dZdY3+dZdY3P dXdY3+dXdY3P
 # SecRow3  PressureTC Volt VoltCC  xyPad3C Edge3C TanL3DC TanL3DiTPCC   dX3iTPCC dX3C   SecRow3PC Z3OC Zdc3 Pressure Time 
 #		flowRateExhaustP flowRateRecirculationP Z3iTPCC
 #		ppmOxygenInP  inputGasTemperatureP 
 #		percentMethaneInP percentMethaneInPA percentMethaneInPC
 #		ppmWaterOutP ppmWaterOutPA ppmWaterOutPC
#    @histG4F = qw(
#  	       Pressure  Time  Voltage Voltage  Qcm  AvCurrent  Z3  xyPad3 G3
#  	       PressureC TimeC Voltage VoltageC QcmC AvCurrentC Z3C xyPad3C G3C
# 	       SecRow3 SecRow3C
# 	       SecRow3P SecRow3PC
#   	       SecRow3+SecRow3P SecRow3C+SecRow3PC 
# 	       dX3 dX3C
#  	     );
#   @histG4E = qw(Z3+Z3P);
#      @histG4E = qw(
#   	       SecRow3 SecRow3P SecRow3+SecRow3P
#   	       SecRow3C SecRow3PC SecRow3C+SecRow3PC
# 	       Z3+Z3P Z3C+Z3PC 
# 	       G3+G3P G3C+G3PC
# 	       xyPad3+xyPad3P xyPad3C+xyPad3PC
# 	       Pressure+PressureP PressureC+PressurePC
# 	       Voltage+VoltageP VoltageC+VoltagePC
# 	       G3+G3P G3C+G3PC
# 	       Qcm QcmC 
# 	       AvCurrent AvCurrentC
# 	       Time TimeC
#   	     );
     @histG4EX = qw(
  	       SecRow3 SecRow3P SecRow3+SecRow3P
  	       SecRow3C SecRow3PC SecRow3C+SecRow3PC
  	     );
#
# 	       dX3 dX3C 
#
# 	       Pressure  Time  Voltage Voltage  Qcm  AvCurrent  Z3  xyPad3 G3
# 	       PressureC TimeC Voltage VoltageC QcmC AvCurrentC Z3C xyPad3C G3C
#    @histRL5 = @histGF;
#     @histGP = qw (
#   		 TPoints70 TPointsF TPoints70U TPointsFU  TPointsN TPointsNU
#  		 TPoints70P TPointsFP TPoints70UP TPointsFUP  TPointsNP TPointsNUP 
#   		 TPoints70+TPoints70P TPointsF+TPointsFP TPoints70U+TPoints70UP TPointsFU+TPointsFUP  TPointsN+TPointsNP TPointsNU+TPointsNUP
#   		 NPoints70 NPointsF NPoints70U NPointsFU  NPointsN NPointsNU
#  		 NPoints70P NPointsFP NPoints70UP NPointsFUP  NPointsNP NPointsNUP 
#   		 NPoints70+NPoints70P NPointsF+NPointsFP NPoints70U+NPoints70UP NPointsFU+NPointsFUP  NPointsN+NPointsNP NPointsNU+NPointsNUP
#  	       );
#   		 TPoints270+TPoints270P TPoints2F+TPoints2FP TPoints270U+TPoints270UP TPoints2FU+TPoints2FUP  TPoints2N+TPoints2NP TPoints2NU+TPoints2NUP
#  		 TPoints270 TPoints2F TPoints270U TPoints2FU  TPoints2N TPoints2NU
#  		 TPoints270P TPoints2FP TPoints270UP TPoints2FUP  TPoints2NP TPoints2NUP
#  		 NPoints270 NPoints2F NPoints270U NPoints2FU  NPoints2N NPoints2NU
#  		 NPoints270P NPoints2FP NPoints270UP NPoints2FUP  NPoints2NP NPoints2NUP
#  		 NPoints70+NPoints70P NPointsF+NPointsFP NPoints70U+NPoints70UP NPointsFU+NPointsFUP  NPointsN+NPointsNP NPointsNU+NPointsNUP
#   		 NPoints270+NPoints270P NPoints2F+NPoints2FP NPoints270U+NPoints270UP NPoints2FU+NPoints2FUP  NPoints2N+NPoints2NP NPoints2NU+NPoints2NUP
 #  		 fitZeP fitZeN fitZprotonP fitZprotonN fitZkaonP fitZkaonN fitZpiP fitZpiN fitZmuP fitZmuN fitZdeuteronP fitZdeuteronN fitZtritonP fitZtritonN fitZHe3P fitZHe3N fitZalphaP fitZalphaN 
 #  		 fitNeP fitNeN fitNprotonP fitNprotonN fitNkaonP fitNkaonN fitNpiP fitNpiN fitNmuP fitNmuN fitNdeuteronP fitNdeuteronN fitNtritonP fitNtritonN fitNHe3P fitNHe3N fitNalphaP fitNalphaN 
 #		 TPoints270P TPoints2FP TPoints270UP TPoints2FUP  TPoints2NP TPoints2NUP
 #		 TPoints70 TPointsF TPoints70U TPointsFU  TPointsN TPointsNU
 # 		 I70eP I70eN I70protonP I70protonN I70kaonP I70kaonN I70piP I70piN I70muP I70muN I70deuteronP I70deuteronN I70tritonP I70tritonN I70He3P I70He3N I70alphaP I70alphaN 
 #		 EtaF Eta70 
 #		 SecRow3C SecRow3PC
 #		 EtaiTPCF EtaiTPC70 TPoints70iTPC TPointsFiTPC TPoints70UiTPC TPointsFUiTPC  TPointsNiTPC TPointsNUiTPC
 #	      );
# @histG0P = qw (
# fitNeP fitNeN fitNeA 
# fitNprotonP fitNprotonN fitNprotonA 
# fitNkaonP fitNkaonN fitNkaonA 
# fitNpiP fitNpiN fitNpiA 
# fitNmuP fitNmuN fitNmuA 
# fitNdeuteronP 
# fitNtritonP 
# fitNHe3P
# fitNalphaP
# );
 #  @histNF = qw(PressureN VoltageN AvCurrentN QcmN Z3N SecRow3N SecRow3PN dX3N TanL3DN); # Edge3N Edge3N PressureTN VoltN Zdc3N  Z3ON 
   #  @histXF = @histNF;
   @opt = qw (GF G4F G4E G4EX GP G0P NF);# XF);# RL5);
 }
print "fit.pl for  @rootfiles \n"; 
if ($#histGF >= 0) {print " with GF: @histGF \n";}
if ($#histG4F >= 0) {print " with G4F: @histG4F \n";}
if ($#histG4E >= 0) {print " with G4E: @histG4E \n";}
if ($#histG4EX >= 0) {print " with G4E: @histG4EX \n";}
if ($#histGP >= 0) {print " with GP: @histGP \n";}
if ($#histG0P >= 0) {print " with GP: @histG0P \n";}
if ($#histRL5 >= 0){print " with RL5:@histRL5\n";}
if ($#histNF >= 0) {print " with NF: @histNF \n";}
if ($#histXF >= 0) {print " with NF: @histXF \n";}
exit if $#rootfiles < 0;
#my @opt = qw (GF G4F G4E G$EX  GP NF);# XF);# RL5);
my $XML = "fit.xml";
open (XML,">$XML") or die "Can't open $XML";
print XML '<?xml version="1.0" encoding="utf-8" ?>
<job name="dEdxFit" maxFilesPerProcess="1" simulateSubmission="false" fileListSyntax="paths" copyInputLocally="false">
	 <command>
setup 64b
unsetenv NODEBUG 
starver .DEV2
csh -x $INPUTFILE0
         </command>
	<stdout URL="file:./sched$JOBID.log" />
';
foreach my $rootfile (@rootfiles) {
  foreach my $fitype (@opt) {
    my @histos = ();
    my $NoSectors =  -1;
    if    ($fitype eq 'GP') {@histos = @histGP;}
    elsif ($fitype eq 'G0P') {@histos = @histG0P;}
    elsif ($fitype eq 'ADC') {@histos = @histADC;}
    elsif ($fitype eq 'GF') {@histos = @histGF;}
    elsif ($fitype eq 'G4F') {@histos = @histG4F;}
    elsif ($fitype eq 'G4E') {@histos = @histG4E;}
    elsif ($fitype eq 'G4EX') {@histos = @histG4EX;}
    elsif ($fitype eq 'RL5'){@histos = @histRL5;}
    elsif ($fitype eq 'NF') {
      @histos = @histNF; 
			     $NoSectors = 24;}
    elsif ($fitype eq 'XF') {@histos = @histNF;} 
    else {next;}
    my $fittype = $fitype;
#    if ($fitype eq 'ADC') {$fittype = "GP";}
    print "FitType : $fittype , histos = @histos\n";
    foreach my $hist (@histos) {
      my $sec1 = -1;
      my $sec2 = -1;
#      if ($fittype eq 'NF') {
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
#	if ($hist =~ /Edge/ || $hist =~ /xyPad/) {$ext = "_y3";}
	my $dir = File::Basename::dirname($rootfile);
	my $fil = File::Basename::basename($rootfile);
	my $SCRIPT = $hist . $fittype . $ext . $fil;
	$SCRIPT =~ s/\.root/\.csh/;
	my $newfilew = $dir . "/" . $hist . $fittype . $ext . $fil;
	print "$rootfile -> $hist => $fittype new file: $newfilew\n";
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
