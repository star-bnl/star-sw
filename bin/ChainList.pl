#! /usr/bin/env perl
#  ChainList.pl | sort -u | more
use File::Basename;
use Cwd;
use lib qw($HOME/bin);
#use Chain;
#foreach my $key (sort keys %Chain) {
#  print "$key => |$Chain{$key}|\n";
#};
#die;
my $glob = "devtest/test*.csh evaltest/test*.csh evaltest/*/test*.csh";
my @list = glob $glob;
my $line;
#print '
#0 Bfc_st BFC[] = { // standard chains
#1  {"Key"         ,"Name"       ,"Chain"      ,"Opts"                      ,"Maker","Libs","Comment",kFALSE},
#2  {"------------","-----------","-----------","------------------------------------------","","","",kFALSE},
#';
foreach my $script (@list) {
#  print "open file $f\n";
  my $f = $script;
  if ($f =~ /Opt/) {next;}
  my $flag = 0;
  if ($f =~ /devtest/) {$f =~ s/devtest\///; $flag = 1;}
  if ($f =~ /evaltest/){$f =~ s/evaltest\///; $flag = 2;}
  $f =~ s/\//_/;
  $f =~ s/\.csh//;
  $f =~ s/_test_/_/;
  $f =~ s/trsITTF/MC/;
  $f =~ s/_daq./\.RC\./;
  $f =~ s/Sti-CA/StiCA/;
  $f =~ s/Stv-CA/StvCA/;
  $f =~ s/simuMC/MC/;
  $f =~ s/\.ittf//;
  $f =~ s/trs/MC/;
  $f =~ s/\.ITTF\./\./;
  $f =~ s/ITTF//;
#  if ($f !~ /y2012/) {next;}
#  if ($f !~ /RC/)    {next;}
  open(in, $script) or die "Can't open $script";
#  print "open $script with $f \n";
  while ($line = <in>) {
#    print $line;
    if ($line =~ /Mixer/) {last;}
    if ($line !~ /bfc/) {next;}
    chop($line);                            #print "1 $line\n";
    $line =~ s/-b -q//;                     #print "2 $line\n";
    $line =~ s/root4star//;                 #print "3 $line\n";
    $line =~ s/root\.exe//;                 #print "4 $line\n";
    $line =~ s/.*(bfc\.C\()//;              #print "5 $line\n";
    $line =~ s/.*(bfc\.C\()//;              #print "5 $line\n";
    $line =~ s/\d*(,)//;                    #print "6 $line\n";
    $line =~ s/\>//;                        #print "7 $line\n";
    $line =~ s/\'//;                        #print "8 $line\n";
#    my @words = split("&", $line);
#    for (my $i = 0; $i <= $#words; $i++) {
#      print "$i => $words[$i]\n";
#    }
    my ($chain_file,$log) = split("&", $line); # print "$chain_file => $log\n";
    $log =~ s/\.log//;
    my ($dum1,$chain,$dum2,$file,$dum3) = split(/\"/,$chain_file);
#    print "$f chain = |$chain|\n";
    if ($chain =~ /xi2/) {$f .= ".2";}
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
    if ($chain !~ /,btof,/ &&  $f  =~ /\.RC\./ && ($f =~ /y2009/ || $f =~ /y2011/)) {$chain =~ s/BEmcChkStat,Corr4/BEmcChkStat,btof,Corr4/;}
    if ($chain !~ /,pmdReco,/ &&  $f  =~ /\.RC\./ && ($f =~ /y2011/)) {$chain .= ",pmdReco";}
    if ($chain !~ /,mtdDat,/ &&  $f  =~ /\.RC\./ && ($f =~ /y2011/)) {$chain .= ",mtdDat";}
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
    if ($f  =~ /\.MC\./ &&
	($f =~ /y2004/ || 
	 $f =~ /y2005/ || 
	 $f =~ /y2006/ || 
	 $f =~ /y2007/ || 
	 $f =~ /y2008/ || 
	 $f =~ /y2009/ ||
	 $f =~ /y2010/ || 
	 $f =~ /y2011/ ||
	 $f =~ /y2012/ ) && 
	$chain !~ /,EEfs/) {$chain =~ s/,emcY2/,emcY2,EEfs/g;}
#    print "chain3 = |$chain|\n";
    if ($f =~ /y2009$/) {$chain =~ s/,tofsim/,btofsim/g;}
    if ($f =~ /y2007$/ && $f =~ /RC/) {$chain =~ s/,BAna/,IAna/g;}
    if ($f =~ /y2011$/ && $f =~ /RC/ && $chain !~ /,mtdDat/ ) {$chain .= ",mtdDat";}
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
    my $tag = "MC";
    foreach my $word (@words) {
      if ($word eq 'RC' or $word eq 'MC') {$tag = $word;}
      if ($word =~ /^y/) {$tag .= "." . $word;}
#      print "$word => $tag\n";
    }
#    print "$log:$flag:{\"$f\",\"\",\"\",\"$chain\",\"\",\"\",\"$file\",kFALSE},\n";
    print "  /* $tag */{\"$f\",\"\",\"\",\"$chain\",\"\",\"\",\"$file\",kFALSE}, \n";
#    print "$tag {\"$f\",\"\",\"\",\"$chain\",\"\",\"\",\"$file\",kFALSE},\n";
#    print "$tag {\"$f\",\"\",\"\",\"$chain\"\n";
 }
  close(in);
#  die;
}
