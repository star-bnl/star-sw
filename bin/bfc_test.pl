#! /usr/bin/env perl
use Env;
use File::Basename; 
#              input file                                                         chain
my $Nevents = 10;
my @tests = 
  (
   'dAuStarndard' =>            {chain => 'in,DbV20040411,dau2003a,-tcl,fcf,est,beamLine,xi2,Kink2,XiSvt,' .
				 'svtdEdx,eemcD,hitfilt,CMuDst',
				 file  => '/star/rcf/test/daq/2003/041/st_physics_4041002_raw_0020001.daq'},
   daq2001_central  =>          {chain => 'P2001a,v0,xi,ZDCvtx,-dstout,CMuDst ', 
				 file => '/star/rcf/test/daq/2001/327/st_physics_2327038_raw_0010.daq'},
   daq2001_minbias  =>          {chain => 'P2001a,v0,xi,ZDCvtx,-dstout,CMuDst ', 
				 file => '/star/rcf/test/daq/2001/295/st_physics_2295030_raw_0010.daq'},
   daq2001_ppMinBias  =>        {chain => 'pp2001a,v0,xi,fpd,beamLine,est,-dstout,CMuDst ', 
				 file => '/star/rcf/test/daq/2002/008/st_physics_3008016_raw_0001.daq'},
   daq2003_dAuMinBias  =>       {chain => 'dau2003,v0,xi,l3onl,est,beamLine,-dstout,CMuDst', 
				 file => '/star/rcf/test/daq/2003/041/st_physics_4041002_raw_0020001.daq'},
   dAu2002_dAuMinBiasITTF  =>   {chain => 'in,DbV20040411,dau2003i,ITTF,SvtIT,hitfilt',
				 file  => '/star/rcf/test/daq/2003/041/st_physics_4041002_raw_0020001.daq'},
   daq2003_ppMinBias    =>      {chain => 'pp2003,v0,xi,l3onl,beamLine,est,-dstout,CMuDst', 
			         file => '/star/rcf/test/daq/2003/095/st_physics_4095050_raw_0010002.daq'},
   daq2004_AuAuMinBias  =>      {chain => 'P2004,svt_daqSvtD,EST,OShortR,Xi2,Kink2,eemcD,-dstout,CMuDst', 
				 file => '/star/rcf/test/daq/2004/028/st_physics_5028066_raw_1010003.daq'},
   daq2004_AuAu_prodHigh  =>    {chain => 'P2004,svt_daqSvtD,EST,OShortR,Xi2,Kink2,eemcD,-dstout,CMuDst', 
				 file => '/star/rcf/test/daq/2004/044/st_physics_5044102_raw_1010003.daq'},
   daq2004_AuAu_prodLow  =>     {chain => 'P2004,svt_daqSvtD,EST,OShortR,Xi2,Kink2,eemcD,-dstout,CMuDst', 
				 file => '/star/rcf/test/daq/2004/044/st_physics_5044116_raw_3010002.daq'},
   trs_2001h_hc_standard  =>    {chain => 'trs,mdc3,v0,xi,big,evout,-dstout,fzin', 
				 file => '/star/rcf/simu/cocktail/hadronic/default/standard/year_1h/' .
				 'half_field/hadronic_on/Gstardata/hc_standard.40_evts.fz'},
   trs_2001_hc_highdensity  =>  {chain => 'trs,srs,fss,rrs,y2001n,C2default,v0,xi,GeantOut,-dstout,CMuDST,big,fzin', 
				 file => '/star/rcf/simu/cocktail/hadronic/default/highdensity/year2001/' .
				 'hadronic_on/Gstardata/hc_highdensity.16_evts.fz'},
   trs_2001_hc_lowdensity  =>   {chain => 'trs,srs,fss,rrs,y2001n,C2default,v0,xi,GeantOut,-dstout,CMuDST,big,fzin', 
				 file => '/star/rcf/simu/cocktail/hadronic/default/lowdensity/year2001/hadronic_on/' . 
				 'Gstardata/hc_lowdensity.400_evts.fz'},
   trs_2001_hc_standard  =>     {chain => 'trs,srs,fss,rrs,y2001n,C2default,v0,xi,GeantOut,CMuDST,-dstout,big,fzin', 
				 file => '/star/rcf/simu/cocktail/hadronic/default/standard/year2001/hadronic_on/' .
				 'Gstardata/hc_standard.40_evts.fz'},
   trs_2001_ppl_minbias  =>     {chain => 'ppMDC4,v0,xi', 
				 file => 'pds0200_04_12812evts.fzd; gfile B pds0200_01_28950evts.fzd; ' . 
				 'mode SVTT back  22122; mode FTPC back 000; mode TPCE back 1881188; ' . 
				 'gback 188 188 0.0213 213. 2.5 '},
   trs_2001_pp_minbias  =>      {chain => 'MDC4New,v0,xi,-dstout CMuDst', 
				 file => '/star/rcf/simu/pp200/pythia/default/minbias/year2001/hadronic_on/' .
				 'gstardata/pds0200_04_12812evts.fzd'},
   trs_2003_dau_minbias  =>     {chain => 'dAuMDC,v0,xi,tofsim,Eefs,beamLine,-dstout,CMuDst,fzin', 
				 file => '/star/rcf/simu/rcf1197_05_5940evts.fzd'},
   trs_2004_auau_minbias  =>    {chain => 'trs,srs,y2004,tpc,l0,svt,Cdst,Kalman,tags,Tree,bbcsim,tofsim,' .
				 'evout,est,xi2,XiSvt,svtdEdx,SvtMatchVtx,emcY2,eefs,GeantOut,big,-dstout,CMuDst,fzin', 
				 file => '/star/rcf/simu/rcf1207_01_225evts.fzd'},
   trs_2004_auau_minbiasITTF => {chain => 'trs y2004 globT tcl TpcHitMover sim_T svt_T ftpcT ctf_T ' .
				 'l0 SvtCL svtDb ITTF genvtx Sti DstOut gen_T l3_T dst dEdxY2 EventQA ' .
				 'geant tags bbcSim tofsim EvOut analysis emcY2  -Match EEfs GeantOut ' .
				 'big V02 Xi2 Kink2 CMuDst fzin MiniMcMk', 
				 file => '/star/rcf/simu/rcf1207_01_225evts.fzd'}
  );

my $def = {@tests};
#my $workdir = "/star/data07/calib/fisyak/" . $STAR_LEVEL . "/test/" . $STAR_HOST_SYS; print "cd $workdir\n";
my $date =`date +%m%d%y`;
chomp($date);
my $workdir = "/star/data07/calib/fisyak/test/" . $date . "/" . $STAR_LEVEL . "/" . $STAR_HOST_SYS; 
`mkdir -p $workdir` if ! -d $workdir; 
foreach my $key ( sort keys %$def) {
  my $file  = $def->{$key}->{file};
  my $chain = $def->{$key}->{chain};
  my $subdir = "ref";
  chdir $workdir or die "Can't change dir to $workdir";
  $subdir = "ittf" if $key =~ /ITTF/;
  `mkdir -p $subdir` if ! -d $subdir;
  chdir $subdir or die "Can't change dir to $subdir";
#  print "$file => $chain\n";
  my $log = $key . "_" . File::Basename::basename($file,".daq");
  my $log = File::Basename::basename($log,".fzd"); 
  $log .= ".log";
  print "run in $workdir/$subdir\n";
  my $SCRIPT = $workdir . "/" . $subdir . "/" . $key . ".csh";
  if (-r $SCRIPT) {print "$SCRIPT has been done\n"; next;}
  print "Create $SCRIPT\n";
  open (OUT,">$SCRIPT") or die "Can't open $SCRIPT";
  print OUT "#! /usr/local/bin/tcsh -f\n";
  print OUT "cd $workdir/$subdir\n";
  print OUT "starver $STAR_LEVEL\n";
  my $Compiler = "gcc";
  if ($STAR_HOST_SYS =~ /icc/) {$Compiler = "icc";}
  print OUT "setup $Compiler\n";
  my $cmd = "root4star -b -q 'bfc.C(". $Nevents . ",\"" . $chain . "\",\"" . $file . "\")' >& " . $log;
  print "$key:\t$cmd \n";
  print OUT "$cmd\n";
  close (OUT);
#  my $rcc = system $cmd;
#  my $rc = 0xffff & $rcc;
#  print "system($cmd)\n";
#  printf "\treturned %30x: \n", $rc;
#  if    ($rc ==      0) {print "run with normal exit\n"; next} 
#  elsif ($rc == 0xff00) {print "job failed : $!\n";} 
#  elsif ($rc >    0x80) {$rc >>= 8; print "ran with non-zero exit status $rc\n";} 
#  else {print "ran with";
#	if ($rc &  0x80) {$rc &= ~0x80; print "coredump from ";}
#      }
#  print "signal $rc\n";
#  last;
}

