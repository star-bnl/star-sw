#! /usr/bin/perl

# Steering routine for running bfc on PDSF 
#
# Adapted from embedding routines, October '01, ELH
#
# The chain is set up such that the output filename of each process is the 
# input filename of the following process
#
# Usage: scriptname 
#    <setup file> <hpss mode> <daq dir> <data label> [nevent]
#
#---------------------------------------------------------
my $code_dir = "/auto/u/starofl/bfc";
#---------------------------------------------------------

use lib "/auto/u/starofl/bfc/bfcLib";

use EmbeddingUtilities;

use Process_object;
use Chain_object;
use File::Basename;
use Cwd;

use strict;

#---------------------------------------------------------
my ($SetupFile, $Mode, $DaqDir, $DataLabel, $NEvents) = GetArgs();

print("setupfile = $SetupFile\n");
print("mode= $Mode\n");
print("daqdir = $DaqDir\n");
print("datalabel = $DataLabel\n");
print("nevents = $NEvents\n");
#---------------------------------------------------------
my ($data_dir, $rescue_dir, $hpss_home) = SetupOutput($Mode);
#-------------------------------------------------------------------------------
# check values of arguments
CheckArgs($SetupFile, $Mode, $DaqDir);
#-------------------------------------------------------------------------------
# deduce the file names
my ($daq_file) = GetFiles($DaqDir, $DataLabel);
#--------------------------------------------------------------------------------
# read setup file

my @param_names = ("library","production","verbose");

my %SetupParams = ReadSetupFile($SetupFile, @param_names);
#------------------------------------------------------------------------------
# extract star version, runID, etc...

my $STAR_VERSION = $SetupParams{library};
my $PROD_VERSION = $SetupParams{production};

my $RunFseq = $DataLabel;
#(my $RunFseq = $DataLabel) =~ s/st_physics_//;
# leave in one underscore to seperate run and fseq
#$RunFseq =~ s/_raw//;
#--------------------------------------------------------------------------------
# user check

print "-" x 80, "\n";
print "Arguments and derived files and directories:\n",
  "SetupFile: $SetupFile\n",
  "Mode: $Mode\n",
  "DaqDir: $DaqDir\n",
  "DataDir: $data_dir\n",
  "DataLabel: $DataLabel\n",
  "NEvents: $NEvents\n",
  "daq_file: $daq_file\n".
  "RunFseq: $RunFseq\n",
  "STAR_VERSION: $STAR_VERSION\n";

$Mode eq 'HPSS' and print "HPSS home: $hpss_home\nRescueDir: $rescue_dir\n";


#----------------------------------------------------------------------------------
# define analysis chain

# chain name is extracted from filename of setup file
my ($chain_name, $path, $suffix) = fileparse( $SetupFile, '\.setup');

my $chain = new Chain_object($chain_name, $Mode, $data_dir, 
			     $STAR_VERSION, $PROD_VERSION, $RunFseq,
			     $rescue_dir);
$chain->SetupFile($SetupFile);

# remove these if you want to use the starofl default
#$chain->HpssHome($hpss_home);

#---------------------------------------------------------
# define bfc step

# bfc flags:
my @bfcArgs;

#$STAR_VERSION =~ /SL01e/ and @bfcArgs = ("Simu ry2000 in tpc_daq tpc rich Physics Kalman Cdst tags Tree evout NoHits allevent");


#-----------------------------------------
# standard SL05a tags: (use also for SL05b)
# $STAR_VERSION =~ /SL05b/ and @bfcArgs = ("P2004,DbV20050215,SCEbyE,OGridLeak,EST,svtdEdx,Xi2,xiSvt,Kink2,pmdRaw,CMuDst,hitfilt");

# SL05a tags of 200GeV full field (use Simu, take out P2004 corrections and SCEbyE):
 $STAR_VERSION =~ /SL05b/ and @bfcArgs = ("Simu,DbV20050215,B2004,l3onl,fcf,ToF,emcDY2,fpd,ftpc,trgd,ZDCvtx,EST,svtdEdx,Xi2,xiSvt,Kink2,pmdRaw,CMuDst,hitfilt");

#-----------------------------------------
# standard SL04i tags:
 $STAR_VERSION =~ /SL04i/ and @bfcArgs = ("P2004,DbV20040804,SCEbyE,ITTF,pmdRaw,hitfilt,-ExB");

# $STAR_VERSION =~ /SL04i/ and @bfcArgs = ("Simu,P2004,DbV20040804,SCEbyE,ITTF,pmdRaw,hitfilt");

# SL04i tags for 200GeV embedding: (use Simu, take out P2004 opts 1 by 1)
#$STAR_VERSION =~ /SL04i/ and @bfcArgs = ("Simu,DbV20040804,B2004,l3onl,fcf,tofDat,emcDY2,ExB,fpd,ftpc,trgd,ZDCvtx,SCEbyE,ITTF,pmdRaw,hitfilt");


#-----------------------------------------
# standard SL04k tags:
#$STAR_VERSION =~ /SL04k/ and @bfcArgs = ("P2004,DbV20041213,SCEbyE,svt_daq,svtD,EST,svtdEdx,eemcD,Xi2,xiSvt,Kink2,pmdRaw,CMuDst,hitfilt");

# alternate set:
#$STAR_VERSION =~ /SL04k/ and @bfcArgs = ("P2004,DbV20041213,EST,svtdEdx,Xi2,xiSvt,Kink2,pmdRaw,CMuDst,hitfilt");

# SL04k tags for 200GeV embedding: (use Simu, take out P2004 opts 1 by 1)
#$STAR_VERSION =~ /SL04k/ and @bfcArgs = ("Simu,DbV20041213,B2004,l3onl,fcf,ToF emcDY2,fpd,ftpc,trgd,ZDCvtx,SCEbyE,svt_daq,svtD,EST,svtdEdx,eemcD,Xi2,xiSvt,Kink2,pmdRaw,CMuDst,hitfilt");

# alternate for embedding (use Simu, take out P2004 opts 1 by 1)
$STAR_VERSION =~ /SL04k/ and @bfcArgs = ("Simu,DbV20041213,B2004,l3onl,fcf,ToF emcDY2,fpd,ftpc,trgd,ZDCvtx,EST,svtdEdx,Xi2,xiSvt,Kink2,pmdRaw,CMuDst,hitfilt");

#-----------------------------------------
# standard SL04e tags:
#$STAR_VERSION =~ /SL04e/ and @bfcArgs = ("P2004,DbV20040415,OShortR,svt_daq,svtD,EST,svtdEdx,eemcD,pmdRaw,Xi2,xiSvt,Kink2,CMuDst,ZDCvtx,hitfilt");

# standard SL04e tags without OShortR:
#$STAR_VERSION =~ /SL04e/ and @bfcArgs = ("P2004,DbV20040415,svt_daq,svtD,EST,svtdEdx,eemcD,pmdRaw,Xi2,xiSvt,Kink2,CMuDst,ZDCvtx,hitfilt");

# standard SL04e tags without OShortR:
#$STAR_VERSION =~ /SL04e/ and @bfcArgs = ("Simu,B2004,l3onl,fcf,tofDat,emcDY2,fpd,ftpc,trgd,DbV20040415,svt_daq,svtD,EST,svtdEdx,eemcD,pmdRaw,Xi2,xiSvt,Kink2,CMuDst,ZDCvtx");

# SL04e tags for 62GeV embedding: (use Simu, take out P2004 opts 1 by 1)
$STAR_VERSION =~ /SL04e/ and @bfcArgs = ("Simu,DbV20040415,B2004,l3onl,fcf,tofDat,emcDY2,fpd,ftpc,trgd,svt_daq,svtD,EST,svtdEdx,eemcD,pmdRaw,Xi2,xiSvt,Kink2,CMuDst,ZDCvtx");


#-----------------------------------------
# standard SL03h tags for dAu:
#$STAR_VERSION =~ /SL03h/ and @bfcArgs = ("DbV20031114,dau2003a,est,l3onl,beamLine,-xi,-v0,SvtHitFilt,hitfilt,XiSvt,svtdEdx,SvtMatchVtx,eemcD,CMuDst");

# SL03h tags for dAu embedding: (use Simu, take out dau2003a opts 1 by 1)
$STAR_VERSION =~ /SL03h/ and @bfcArgs = (" Simu,DbV20031114,B2003,ppOpt,-Prevtx,tofDat,emcDY2,fpd,svt_daq,SvtD,ftpc,trgd,est,l3onl,beamLine,-xi,-v0,SvtHitFilt,hitfilt,XiSvt,svtdEdx,SvtMatchVtx,eemcD,CMuDst");

# SL03h tags for 2001 AuAu embedding (SL02d embedding tags w/SL02e timestamp)
# these are used for TOFp
#$STAR_VERSION =~ /SL03h/ and @bfcArgs = ("Simu DbV20020402 l3onl ry2001 Db in tpc_daq tpc rich Physics Kalman Cdst tags Tree evout NoHits allevent");

#-------------------------------------------
# standard SL03f tags for 2001/2 pp data:
#$STAR_VERSION =~ /SL03f/ and @bfcArgs = ("DbV20030808,pp2001a,fpd,est,beamLine,-xi,-v0,SvtHitFilt,HitFilt,xi2,XiSvt,svtdEdx,SvtMatchVtx,CMuDst");

# SL03f tags for embedding: (use Simu, take out pp2001a opts 1 by 1)
$STAR_VERSION =~ /SL03f/ and @bfcArgs = ("Simu,DbV20030808,ppOpt,B2001,-PreVtx,L3onl,tofDat,emcDY2,svt,daq,SvtD,ftpc,fpd,est,beamLine,-xi,-v0,SvtHitFilt,HitFilt,xi2,XiSvt,svtdEdx,SvtMatchVtx,CMuDst,svt_daq");

#-------------------------------------------
# standard SL03d tags:
#$STAR_VERSION =~ /SL03d/ and @bfcArgs = ("DbV20030523,dau2003a,beamLine,hitfilt,CMuDst");

# SL03d tags for embedding: (use Simu, take out dau2003a opts 1 by 1)
$STAR_VERSION =~ /SL03d/ and @bfcArgs = (" Simu,DbV20030523,B2003,ppOpt,-Prevtx,tofDat,emcDY2,fpd,svt_daq,SvtD,ftpc,trgd,est,beamLine,hitfilt,CMuDst");

# used to create SL03d tags for 2001 AuAu embedding (SL02d embedding tags w/SL02e timestamp)
# these are used for TOFp
#$STAR_VERSION =~ /SL03d/ and @bfcArgs = ("Simu DbV20020402 l3onl ry2001 Db in tpc_daq tpc rich Physics Kalman Cdst tags Tree evout NoHits allevent");
#------------------------------------------
# standard SL03a tags:
#$STAR_VERSION =~ /SL03a/ and @bfcArgs = ("DbV20030408,dau2003,est,beamLine,hitfilt,CMuDst");

# SL03a tags for embedding: (use Simu, take out dau2003 opts 1 by 1)
$STAR_VERSION =~ /SL03a/ and @bfcArgs = (" Simu,DbV20030408,B2003,ppOpt,-Prevtx,tofDat,emcDY2,fpd,svt_daq,SvtD,ftpc,est,beamLine,hitfilt,CMuDst");
#-----------------------------------------

# used to create Sl01j tags for embedding:
#$STAR_VERSION =~ /SL01j/ and @bfcArgs = ("Simu ry2000a in tpc_daq tpc rich Physics Kalman Cdst tags Tree evout NoHits allevent");

# used to create SL02d tags for 2001 embedding:
$STAR_VERSION =~ /SL02d/ and @bfcArgs = ("Simu DbV20020226 l3onl ry2001 Db in tpc_daq tpc rich Physics Kalman Cdst tags Tree evout NoHits allevent");

# used for regular SL02d production:
#$STAR_VERSION =~ /SL02d/ and @bfcArgs = ("DbV20020226 P2001 ZDCvtx emcDY2 EmcMDST NoHits");

# used for SL02e pp production...
#$STAR_VERSION =~ /SL02e/ and @bfcArgs = ("DbV20020402 pp2001a fpd beamLine EmcMDst CMuDst NoHits");

# P02ge pp embedding:  expand pp2001a and remove Corr2...
$STAR_VERSION =~ /SL02e/ and @bfcArgs = ("DbV20020402 svt_daq SvtD ftpc pp B2001 -PreVtx -SpinTag l3onl tofDat emcDY2 fpd beamLine EmcMDst CMuDst NoHits");

# run in dev 1/19/03...  try this:
$STAR_VERSION =~ /SL02i/ and @bfcArgs = ("Simu dau2003 alltrigger -corr2");

# used to create Sl01l tags for 2001 embedding:
#$STAR_VERSION =~ /SL01l/ and @bfcArgs = ("Simu ry2001 Db in tpc_daq tpc rich Physics Kalman Cdst tags Tree evout NoHits allevent");

# used to create Sl01k tags for 2001 embedding:
#$STAR_VERSION =~ /SL01k/ and @bfcArgs = ("Simu y2001 in tpc_daq tpc rich Physics Kalman Cdst tags Tree evout NoHits allevent");

# for SL01j comparison to trs:
#$STAR_VERSION =~ /SL01j/ and @bfcArgs = ("ry2000a in tpc_daq tpc Kalman Cdst tags Tree evout allevent Eval QAC");

@bfcArgs == " " and die "STAR_VERSION $STAR_VERSION not supported!";

# if arg = "lib", library version will be taken using current STAR_LEVEL
# if macro not found using absolute path, chain will stop

#my $bfc_dir = "lib";
my $bfc_dir = $code_dir;

print "bfc_dir = $bfc_dir\n";

my $bfc = new Process_object("BFC", $daq_file, $bfc_dir, $NEvents, @bfcArgs);

$chain->Add($bfc);

#---------------------------------------------------------
# called after all elements of chain are defined: 
# default is that output file of each link is 
# input file to next link

$chain->SetupFilenames();

#---------------------------------------------------------
# run the chain

$chain->Run();

#---------------------------------------------------------
# all done

print "...all done on " . scalar localtime() ."\n";


#===========================================================================
