#! /usr/bin/env perl
#  ChainLog.pl | sort -u | more
use File::Basename;
use Cwd;
use lib qw($HOME/bin);
use Env;
use File::Find;
require "find.pl";
#use Chain;
#foreach my $key (sort keys %Chain) {
#  print "$key => |$Chain{$key}|\n";
#};
#die;
my $glob = "/star/rcf/test/dev/*.ittf/*/*/*/*.log";
#my $glob = "/star/rcf/test/dev/*.ittf/Sun/*/*/*.log";
#my $glob = "/star/rcf/test/dev/trs_sl302.ittf/Fri/year_2011/pp500_pileup/rcf10100_90_200evts_Wplus_enu.log";
my @list = glob $glob;
my $line;
#print '
#0 Bfc_st BFC[] = { // standard chains
#1  {"Key"         ,"Name"       ,"Chain"      ,"Opts"                      ,"Maker","Libs","Comment",kFALSE},
#2  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
#';
foreach my $log (@list) {
#  print "open file $log\n";
  if ($log =~ /embed/) {next;}
  if ($log =~ /tmp/) {next;}
  my $tag = "MC";
  if ($log =~ /daq/) {$tag = "RC";}
  my $Key= File::Basename::dirname($log);
  if ($Key=~ /Opt/) {next;}
  $Key=~ s/.*year_//;
  $Key=~ s/\//\./;
  $Key=~ s/\.csh//;
  $Key=~ s/_test_/_/;
  $Key=~ s/trsITTF/MC/;
  $Key=~ s/_daq./\.RC\./;
  $Key=~ s/Sti-CA/StiCA/;
  $Key=~ s/Stv-CA/StvCA/;
  $Key=~ s/simuMC/MC/;
  $Key=~ s/\.ittf//;
  $Key=~ s/trs/MC/;
  $Key=~ s/\.ITTF\./\./;
  $Key=~ s/ITTF//;
#  if ($Key!~ /y2012/) {next;}
#  if ($Key!~ /RC/)    {next;}
  open(in, $log) or die "Can't open $log";
#  print "open $log with $Key\n";
  while ($line = <in>) {
    if ($line !~ /Processing /) {next;}
    $line =~ s/.*\(//;
    $line =~ s/\).*//;
#    print $line;
    my @words = split('"',$line);
    my $nevents = $words[0];
    $nevents =~ s/,//g;
    my $chain = $words[1];
    my $file = $words[3];
    if (! -r $file) {
      my $dir = "/star/rcf/test/daq";
      if ($file =~ /\.fz/) {$dir = "/star/rcf/simu";}
#      print "$log\n";
      if ($file =~ /gfile/) {
	my @ws = split(' ',$file);
	my $f1 = $ws[0]; chop($f1); chop($f1); 
	my $f2 = $ws[3]; chop($f2); chop($f2);
	my $fp1 = `find $dir -name $f1 | tail -1`; chop($fp1);
	my $fp2 = `find $dir -name $f2 | tail -1`; chop($fp2);
#	print "$f1 => $fp1, $f2 => $fp2\n";
	$file =~ s/$f1/$fp1/;
	$file =~ s/$f2/$fp2/;
#	for (my $i = 0; $i < $#ws; $i++) {
#	  print "$i => $ws[$i]\n";
#	}
      } else {
	my $filep = `find $dir -name $file | tail -1`;
	chop($filep);
	#      print "use $filep instead of $file\n";
	$file = $filep;
      }
    }
#    print "nevent = $nevents, chain = $chain, file = $file\n";
    if ($chain =~ /xi2/) {$Key.= ".2";}
    $chain =~ s/,btof,BEmcChkStat,/,BEmcChkStat,btof,/g;
    $chain =~ s/\-dstout,//;
    $chain =~ s/,clearmem//g; 
    $chain =~ s/,logger//g; 
    $chain =~ s/fcf/tcl/g;  
    $chain =~ s/tpcRS/TpcRS/g;
    $chain =~ s/tpcrs/TpcRS/g;
    $chain =~ s/,EEfs//g;
    $chain =~ s/,,/,/;
    $chain =~ s/,ITTF,Sti/,Sti/;
#    print "chain1 = |$chain|\n";
    if ($chain =~ /genvtx/ && $chain !~ /VFMinuit,/) {$chain =~ s/genvtx/VFMinuit/;}
    if ($chain =~ /Test\.default\.Fast\.StvCA,/) {$chain =~ s/Test\.default\.Fast\.StvCA,/Test\.default\.Fast\.ITTF,/; $chain .= ",StvCA";}
    if ($chain =~ /Test\.default\.Fast\.Stv,/)   {$chain =~ s/Test\.default\.Fast\.Stv,/Test\.default\.Fast\.ITTF,/; $chain .= ",Stv";}
    if ($chain =~ /Fast/ && $chain =~ /TpcRS,/) {$chain =~ s/TpcRS,//;}
    if ($chain =~ /TpcRS/ && $chain =~ /TpxClu/) { $chain =~ s/,TpxClu//; $chain =~ s/TpcRS/TpcRS,TpxClu/;}
    if ($chain =~ /TpxClu/ && $chain =~ /,tcl/) {$chain =~ s/,tcl//g;}
#   if ($chain =~ /,EEfs,/)    { $chain =~ s/,EEfs,/,/; $chain .= ",EEfs";}
    if ($chain !~ /,btof,/ &&  $Key =~ /\.RC\./ && ($Key=~ /y2009/ || $Key=~ /y2011/)) {$chain =~ s/BEmcChkStat,Corr4/BEmcChkStat,btof,Corr4/;}
    if ($chain !~ /,pmdReco,/ &&  $Key =~ /\.RC\./ && ($Key=~ /y2011/)) {$chain .= ",pmdReco";}
    if ($chain !~ /,mtdDat,/ &&  $Key =~ /\.RC\./ && ($Key=~ /y2011/)) {$chain .= ",mtdDat";}
    if ($chain =~ /,fpd,/) { $chain =~ s/,fpd,/,/; $chain .= ",fpd";}
    if ($chain =~ /,v02,/) { $chain =~ s/,v02,/,/; $chain .= ",v02";}
    if ($chain =~ /,xi2,/) { $chain =~ s/,xi2,/,/; $chain .= ",xi2";}
    if ($chain =~ /,v0,/) { $chain =~ s/,v0,/,/; $chain .= ",v0";}
    if ($chain =~ /,xi,/) { $chain =~ s/,xi,/,/; $chain .= ",xi";}
    if ($chain =~ /,pmdReco,/) { $chain =~ s/,pmdReco,/,/; $chain .= ",pmdReco";}
    if ($chain =~ /,mtdDat,/)  { $chain =~ s/,mtdDat,/,/; $chain .= ",mtdDat";}
    if ($chain =~ /,fmsDat,/)  { $chain =~ s/,fmsDat,/,/; $chain .= ",fmsDat";}
    if ($chain =~ /,ReadAll,/) { $chain =~ s/,ReadAll,/,/; $chain .= ",ReadAll";}
    if ($chain =~ /,ITTF,/  )  { $chain =~ s/,ITTF,/,/; $chain .= ",ITTF";}
    if ($chain =~ /,Sti,/  )   { $chain =~ s/,Sti,/,/; $chain .= ",Sti";}
    if ($chain =~ /,StiPulls,/){ $chain =~ s/,StiPulls,/,/; $chain .= ",StiPulls";}
    if ($chain =~ /,StiCA,/)   { $chain =~ s/,StiCA,/,/; $chain .= ",StiCA";}
    if ($chain =~ /,Stv,/  )   { $chain =~ s/,Stv,/,/; $chain .= ",Stv";}
    if ($chain =~ /,StvPulls,/){ $chain =~ s/,StvPulls,/,/; $chain .= ",StvPulls";}
    if ($chain =~ /,StvCA,/)   { $chain =~ s/,StvCA,/,/; $chain .= ",StvCA";}
    if ($chain =~ /,AgML,/)    { $chain =~ s/,AgML,/,/; $chain .= ",AgML";}
#    print "chain2 = |$chain|\n";
    if ($Key =~ /\.MC\./ &&
	($Key=~ /y2004/ || 
	 $Key=~ /y2005/ || 
	 $Key=~ /y2006/ || 
	 $Key=~ /y2007/ || 
	 $Key=~ /y2008/ || 
	 $Key=~ /y2009/ ||
	 $Key=~ /y2010/ || 
	 $Key=~ /y2011/ ||
	 $Key=~ /y2012/ ) && 
	$chain !~ /,EEfs/) {$chain =~ s/,emcY2/,emcY2,EEfs/g;}
#    print "chain3 = |$chain|\n";
    if ($Key=~ /y2009$/) {$chain =~ s/,tofsim/,btofsim/g;}
    if ($Key=~ /y2007$/ && $Key=~ /RC/) {$chain =~ s/,BAna/,IAna/g;}
    if ($Key=~ /y2011$/ && $Key=~ /RC/ && $chain !~ /,mtdDat/ ) {$chain .= ",mtdDat";}
    $chain =~ s/,btof,BEmcChkStat,/,BEmcChkStat,btof,/g;
    $chain =~ s/,btof,VFPPVnoCTB,beamline,BEmcChkStat,/,VFPPVnoCTB,beamline,BEmcChkStat,btof,/;
    $chain =~ s/,-ITTF//;
#    print "chain4 = |$chain|\n";
    $chain =~ s/\s//g;
#    print "chain = |$chain|\n";
    foreach my $key (sort keys %Chain) {
#      print "$key => |$Chain{$key}|\n";
      if ($chain =~ /$Chain{$key}/) {
	$chain =~ s/$Chain{$key}/$key/;
#	print "new chain = $chain\n";
	last;
      }
    }
    $chain =~ s/,ITTF,Sti/,Sti/;
    $chain =~ s/,Sti,StiCA/,StiCA/;
    $chain =~ s/,Sti,Stv/,Stv/;
    if ($chain !~ /Sti/ && $chain !~ /Stv/) {$chain .= ",Sti";}
    my @words = split('\.',$f);
    print "  {\"test.$tag.$Key\",\"\",\"\",\"$chain\",\"\",\"\",\"$file\",kFALSE}, \n";
#    print "$tag {\"$f\",\"\",\"\",\"$chain\",\"\",\"\",\"$file\",kFALSE},\n";
#    print "$tag {\"$f\",\"\",\"\",\"$chain\"\n";
#    die;
    last;
 }
  close(in);
  #  die;
}
