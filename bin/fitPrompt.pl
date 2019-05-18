#! /usr/bin/env perl
use File::Basename;
use Sys::Hostname;
use Cwd;
use Env;
my $DIR = Cwd::cwd();
# list of all histogram to be fitted
`ln -s ~/macros/.sl* .`;
$ENV{STARFPE} = "NO";
my @histGF = ();
my @histGP = ();
my @histRL5 = ();
my @histNF = ();
my @histXF = ();
my @histADC = ();
my @rootfiles = ();
my $all = 0;
#my $all = 1;
#@histGF = qw (VoltageC);
foreach my $arg (@ARGV) {
  if ($arg =~ /\.root$/) {push @rootfiles, $arg; next;}
}
  @histGP = qw (Z ZL T ZLM);

print "fit.pl for  @rootfiles \n"; 
if ($#histGF >= 0) {print " with GF: @histGF \n";}
if ($#histGP >= 0) {print " with GP: @histGP \n";}
if ($#histRL5 >= 0){print " with RL5:@histRL5\n";}
if ($#histNF >= 0) {print " with NF: @histNF \n";}
if ($#histXF >= 0) {print " with NF: @histXF \n";}
exit if $#rootfiles < 0;
my @opt = qw (GF GP NF);# XF);# RL5);
#my @opt = qw (ADC);
my $XML = "fit.xml";
open (XML,">$XML") or die "Can't open $XML";
print XML '<?xml version="1.0" encoding="utf-8" ?>
<job name="dEdxFit" maxFilesPerProcess="1" simulateSubmission="false" fileListSyntax="paths" copyInputLocally="false">
	 <command>
setup 64b
setenv NODEBUG yes
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
    elsif ($fitype eq 'ADC') {@histos = @histADC;}
    elsif ($fitype eq 'GF') {@histos = @histGF;}
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
	  if ($hist =~ /Edge/ || $hist =~ /xyPad/) {
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
