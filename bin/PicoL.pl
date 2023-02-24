#! /usr/bin/env perl
# /star/data14/GRID/NFS_FileList
# dir -ltrd /star/data*/reco/productio*/*/*
# dir -ltrd /star/data*/reco/productio*/*/* | awk -F\/ '{print $5"/"$7}' | sort -u
# production_11p5GeV_2020/P22ic_calib
# production_11p5GeV_2020/P23ia
# production_13p5GeV_fixedTarget_2020/P21id
# production_13p5GeV_fixedTarget_2020/P21id.SL22b
# production_13p5GeV_fixedTarget_2020/P21id.SL22c
# production_14p5GeV_2019/P21ic
# production_19GeV_2019b/P21ic
# production_19GeV_2019/P21ic
# production_19p5GeV_fixedTarget_2020/P21id
# production_26p5GeV_fixedTarget_2020/DEV
# production_26p5GeV_fixedTarget_2020/P21ic_calib
# production_27GeV_fixedTarget_2018/P19ie.SL20d
# production_31GeV_fixedTarget_2019/P21id
# production_31p2GeV_fixedTarget_2020/P21id
# production_3p85GeV_fixedTarget_2019/P21id
# production_4p59GeV_fixedTarget_2019/P21id
# production_5p75GeV_fixedTarget_2020/P21id
# production_7.3GeV_fixedTarget_2019/P21id
# production_7p3GeV_fixedTarget_2020/P21id
# production_7p7GeV_2021/DEV
# production_7p7GeV_2021/P22ia
# production_7p7GeV_2021/P22ia_calib
# production_7p7GeV_2021/P22ib
# production_7p7GeV_2021/p7p7_21_DEV_calib
# production_9p2GeV_2020b/P22ic_calib
# production_9p2GeV_2020b/P23ia
# production_9p2GeV_2020c/P22ic_calib
# production_9p2GeV_2020c/P23ia
# production_9p2GeV_2020/P22ic_calib
# production_9p2GeV_2020/P23ia
# production_9p8GeV_fixedTarget_2020/P21id
# production_AuAu200_2019/P21id
# production_AuAu200_2019/P22ia
# production_dAu200_2021/DEV
# production_FF_OO_200GeV_2021/dev
# production_FF_OO_200GeV_2021/P23ia_calib
# production_isobar_2018/P21id
# production_isobar_2018/P21id.SL21d
# production_isobar_2018/P22ia
# production_OO_200GeV_2021/dev
# production_OO_200GeV_2021/DEV
# production_OO_200GeV_2021/P23ia_calib
# production_OO_fcsTiming/dev
# production_pp500_2022/dev
# production_pp500_2022/DEV
# production_pp500_2022/pp500_22_DEV_fcs
# production_ps_OO_200GeV_2021/dev
# production_ps_OO_200GeV_2021/P23ia_calib
#--------------------------------------------------------------------------------
# production_27GeV_fixedTarget_2018/P19ie.SL20d
# production_isobar_2018/P21id
# production_isobar_2018/P21id.SL21d
# production_isobar_2018/P22ia

# production_14p5GeV_2019/P21ic
# production_19GeV_2019b/P21ic
# production_19GeV_2019/P21ic
# production_31GeV_fixedTarget_2019/P21id
# production_3p85GeV_fixedTarget_2019/P21id
# production_4p59GeV_fixedTarget_2019/P21id
# production_7.3GeV_fixedTarget_2019/P21id
# production_AuAu200_2019/P21id
# production_AuAu200_2019/P22ia

# production_11p5GeV_2020/P22ic_calib
# production_11p5GeV_2020/P23ia
# production_13p5GeV_fixedTarget_2020/P21id
# production_13p5GeV_fixedTarget_2020/P21id.SL22b
# production_13p5GeV_fixedTarget_2020/P21id.SL22c
# production_19p5GeV_fixedTarget_2020/P21id
# production_26p5GeV_fixedTarget_2020/DEV
# production_26p5GeV_fixedTarget_2020/P21ic_calib
# production_31p2GeV_fixedTarget_2020/P21id
# production_5p75GeV_fixedTarget_2020/P21id
# production_7p3GeV_fixedTarget_2020/P21id
# production_9p2GeV_2020b/P22ic_calib
# production_9p2GeV_2020b/P23ia
# production_9p2GeV_2020c/P22ic_calib
# production_9p2GeV_2020c/P23ia
# production_9p2GeV_2020/P22ic_calib
# production_9p2GeV_2020/P23ia
# production_9p8GeV_fixedTarget_2020/P21id

# production_7p7GeV_2021/DEV
# production_7p7GeV_2021/P22ia
# production_7p7GeV_2021/P22ia_calib
# production_7p7GeV_2021/P22ib
# production_7p7GeV_2021/p7p7_21_DEV_calib
# production_dAu200_2021/DEV
# production_FF_OO_200GeV_2021/dev
# production_FF_OO_200GeV_2021/P23ia_calib
# production_OO_200GeV_2021/dev
# production_OO_200GeV_2021/DEV
# production_OO_200GeV_2021/P23ia_calib
# production_OO_fcsTiming/dev

# production_pp500_2022/dev
# production_pp500_2022/DEV
# production_pp500_2022/pp500_22_DEV_fcs
# production_ps_OO_200GeV_2021/dev
# production_ps_OO_200GeV_2021/P23ia_calib

# dir -ltrd dir -ltrd /hlt/cephfs/reco/Pico/20*/*GeV* /hlt/cephfs/reco/Pico/20*/*/*GeV* /hlt/cephfs/reco/Pico/20*/*/*/*GeV*
use File::Basename;
use Cwd;
use File::stat;
use Time::localtime;
use Env;
use lib $STAR . "/bin";
use PicoDef;
# dir -ltrd /gpfs01/star/data*/reco/production_*/*/*/*
#my $timestamp = ctime(stat($fh)->mtime);
#my $glob =  "/net/l404/data/fisyak/Pico/BES-I/AuAu19_production/2011/???/*";
#my $glob =  "/net/l401/data/scratch2/kehw/reco/2019/???/*";
#my $glob =  "/net/l401/data/scratch2/kehw/reco/2019/TFG19d/???/*";
#my $glob =  "/net/l401/data/scratch2/kehw/reco/2019/14GeV_2019_StiCA/0??/*";
#my $glob =  "./Pico*/???/*/*.picoDst.root";
#____________________________________________________________
sub PrintHash($$) {
  my $env = shift; # print "Call PrintHash\n";
  my $prefix = shift;
  foreach my $key (sort keys %$env ) {
    print "{ $key }\t=> $env->{$key}\n";
  }
}
my $def = {@PicoDefs};# print "Runs = @Runs\n"
PrintHash($def,"Runs");# if ($debug);
die;
my $PICOPATH = "";
my $debug = 0;
if ($#ARGV >= 0) {
  $debug = $ARGV[0];
}
my $pwd = Cwd::cwd(); print "pwd = $pwd\n" if ($debug);
my $glob = "";
my $dayMin =  0;
my $dayMax =  0;
my $year = "y2020";
my $Njobs = 0;
my $DST = "picoDst";
if    ($pwd =~ /2022/) { $year = "y2022";}
elsif ($pwd =~ /2021/) { $year = "y2021";}
elsif ($pwd =~ /2020/) { $year = "y2020";}
elsif ($pwd =~ /2019/) { $year = "y2019";} 
elsif ($pwd =~ /2018/) { $year = "y2018";} 
elsif ($pwd =~ /2017/) { $year = "y2017";} 
elsif ($pwd =~ /2016/) { $year = "y2016";} 
elsif ($pwd =~ /2015/) { $year = "y2015";} 
elsif ($pwd =~ /2014/) { $year = "y2014";} 
elsif ($pwd =~ /2013/) { $year = "y2013";} 
elsif ($pwd =~ /2012/) { $year = "y2012";} 
elsif ($pwd =~ /2011/) { $year = "y2011";} 
elsif ($pwd =~ /2010/) { $year = "y2010";} 
if ($pwd =~ /dev/ or $pwd  =~ /DEV/ or $pwd =~ /P2/ or $pwd =~ /SL/) {
#  $PICOPATH = "/gpfs01/star/data*"; print "PICOPATH = $PICOPATH \n" if ($debug);
  $PICOPATH = "/star/data*"; print "PICOPATH = $PICOPATH \n" if ($debug);
  if    ($pwd =~ /2021\/p7p7_21_DEV_calib/)  {$PICOPATH = "/star/data*"; $glob = "/reco/production_7p7GeV_2021/ReversedFullField/p7p7_21_DEV_calib/2021/";}
  elsif ($pwd =~ /2021\/7p7GeV.P22ia_calib/)  {$glob = "/reco/production_7p7GeV_2021/ReversedFullField/P22ia_calib/2021/";}
  elsif ($pwd =~ /2020\/5p75GeV_fixedTarget_P21id/) {$glob = "/reco/production_5p75GeV_fixedTarget_2020/ReversedFullField/P21id/20*";}
  elsif ($pwd =~ /2020\/5p75GeV_fixedTarget/) {$glob = "/reco/production_5p75GeV_fixedTarget_2020/ReversedFullField/dev/20*";}
  elsif ($pwd =~ /2020\/11p5GeV_2020_P21ib/)  {$glob = "/reco/production_11p5GeV_2020/ReversedFullField/P21ib_calib/20*"; }
  elsif ($pwd =~ /2020\/11p5GeV.C/)           {$glob = "/reco/production_11p5GeV_2020/ReversedFullField/dev/20*"; $dayMin = 42;}
  elsif ($pwd =~ /2020\/11p5GeV/)             {$glob = "/reco/production_11p5GeV_2020/ReversedFullField/dev/20*"; $dayMax = 41;}
  elsif ($pwd =~ /2020\/13p5GeV_fixedTarget_P21id/) {$glob = "/reco/production_13p5GeV_fixedTarget_2020/ReversedFullField/P21id/20*";}
  elsif ($pwd =~ /2020\/19p5GeV_fixedTarget/) {$glob = "/reco/production_19p5GeV_fixedTarget_2020/ReversedFullField/dev/20*";}
  elsif ($pwd =~ /2020\/31p2GeV_fixedTarget/) {$glob = "/reco/production_31p2GeV_fixedTarget_2020/ReversedFullField/dev/20*";}
  elsif ($pwd =~ /2020\/7p3GeV_fixedTarget/)  {$glob = "/reco/production_7p3GeV_fixedTarget_2020/ReversedFullField/dev/20*";}
#       /star/data40/reco/production_9p2GeV_2020b/ReversedFullField/P23ia/2020/062/21062033/st_physics_21062033_raw_7000004.picoDst.root
# /gpfs01/star/data*/reco/production_9p2GeV_2020b/ReversedFullField/P21ib_calib/2020/???/*
  elsif ($pwd =~ /9p2GeV_2020_P23ia/)         {$glob = "/reco/production_9p2GeV_2020/ReversedFullField/P23ia/2020";}# print "$pwd => $glob\n";}
  elsif ($pwd =~ /9p2GeV_2020b_P23ia/)        {$glob = "/reco/production_9p2GeV_2020b/ReversedFullField/P23ia/2020";}# print "$pwd => $glob\n";}
  elsif ($pwd =~ /9p2GeV_2020c_P23ia/)        {$glob = "/reco/production_9p2GeV_2020c/ReversedFullField/P23ia/2020";}# print "$pwd => $glob\n";}
  elsif ($pwd =~ /2020\/9p2GeV_2020c/)        {$glob = "/reco/production_9p2GeV_2020c/ReversedFullField/P21ib_calib/2020";}
  elsif ($pwd =~ /2020\/9p2GeV_2020b/)        {$glob = "/reco/production_9p2GeV_2020b/ReversedFullField/P21ib_calib/2020";}
  elsif ($pwd =~ /2020\/9p2GeV_2020/)         {$glob = "/reco/productioxfn_9p2GeV_2020/ReversedFullField/P21ib_calib/2020";}
  elsif ($pwd =~ /2020\/9p2GeVc/)             {$glob = "/reco/production_9p2GeV_2020c/ReversedFullField/dev/2020";}
  elsif ($pwd =~ /2020\/9p2GeVb/)             {$glob = "/reco/production_9p2GeV_2020b/ReversedFullField/dev/2020";}
  elsif ($pwd =~ /2020\/9p2GeV/)              {$glob = "/reco/production_9p2GeV_2020/ReversedFullField/dev/20*";}
  elsif ($pwd =~ /2020\/9p8GeV_fixedTarget/)  {$glob = "/reco/production_9p8GeV_fixedTarget_2020/ReversedFullField/dev/20*";}
  elsif ($pwd =~ /2019\/19GeV_2019_P20ic/)    {$glob = "/reco/production_19GeV_2019/ReversedFullField/P20ic_calib/2019"; $DST = "MuDst";}
  elsif ($pwd =~ /2019\/19GeV_2019_P21ia/)    {$glob = "/reco/production_19GeV_2019/ReversedFullField/P21ia_calib/2019"; $DST = "MuDst"}
#  elsif ($pwd =~ /2019\/19GeV_2019_P21ib2/)   {$PICOPATH = "/gpfs01/star/data101"; $glob = "/reco/production_19GeV_2019/ReversedFullField/P21ib_calib/2019"; }
  elsif ($pwd =~ /2019\/19GeV_2019_P21ib2/)   {$PICOPATH = "/gpfs01/star/data97"; $glob = "/reco/production_19GeV_2019/ReversedFullField/P21ib_calib/2019"; }
  elsif ($pwd =~ /2019\/19GeV_2019_P21ib/)    {$PICOPATH = "/gpfs01/star/data100"; $glob = "/reco/production_19GeV_2019/ReversedFullField/P21ib_calib/2019"; }
  elsif ($pwd =~ /2019\/19GeV_2019_DEV/)      {$PICOPATH = "/gpfs01/star/subsys-tpc/fisyak/Pico/2019/production_19GeV_2019_DEV"; $glob = "";}
  elsif ($pwd =~ /2019\/19GeV_2019_SL21c/)    {$PICOPATH = "/gpfs01/star/scratch/kehw"; $glob = "/SL21c/reco/AuAu19/out"; }
  elsif ($pwd =~ /2019\/19GeV_2019_SL21e/)    {$PICOPATH = "/gpfs01/star/scratch/kehw"; $glob = "/SL21e/reco/AuAu19/out"; }
  elsif ($pwd =~ /2019\/19GeV_2019_P21ic/)    {$glob = "/reco/production_19GeV_2019/ReversedFullField/P21ic/2019"; }
  elsif ($pwd =~ /2019\/14p5GeV_2019_DEV/)    {$glob = "/reco/production_14p5GeV_2019/ReversedFullField/DEV_calib/2019";}
  elsif ($pwd =~ /2019\/14p5GeV_2019_P21ib2/) {$PICOPATH = "/gpfs01/star/data97"; $glob = "/reco/production_14p5GeV_2019/ReversedFullField/P21ib_calib/2019"; }
  elsif ($pwd =~ /2019\/14p5GeV_2019_P21ib/)  {$glob = "/reco/production_14p5GeV_2019/ReversedFullField/P21ib_calib/2019"; }
  elsif ($pwd =~ /2019\/14p5GeV_2019_P21ic/)  {$glob = "/reco/production_14p5GeV_2019/ReversedFullField/P21ic/2019"; }
  elsif ($pwd =~ /2019\/31GeV_fixedTarget_2019/)  {$glob = "/reco/production_31GeV_fixedTarget_2019/ReversedFullField/P21id/2019"; }
  elsif ($pwd =~ /2019\/7.3GeV_fixedTarget_2019/)  {$glob = "/reco/production_7.3GeV_fixedTarget_2019/ReversedFullField/P21id/2019"; }
  elsif ($pwd =~ /2019\/3p85GeV_fixedTarget_2019/) {$glob = "/reco/production_3p85GeV_fixedTarget_2019/ReversedFullField/P21id/2019/";}
  elsif ($pwd =~ /2019\/4p59GeV_fixedTarget_2019/) {$glob = "/reco/production_4p59GeV_fixedTarget_2019/ReversedFullField/P21id/2019/";}

  elsif ($pwd =~ /2017\/pp500/)               {$glob = "/reco/pp500_production_2017/ReversedFullField/P20ic/2017";}
  elsif ($pwd =~ /2021\/7p7GeV/)              {$glob = "/reco/production_7p7GeV_2021/ReversedFullField/dev/2021/";}
  elsif ($pwd =~ /2021\/44p5GeV_fixedTarget/) {$glob = "/reco/production_44p5GeV_fixedTarget_2021/ReversedFullField/dev/2021/";}
  elsif ($pwd =~ /2021\/70GeV_fixedTarget/)   {$glob = "/reco/production_70GeV_fixedTarget_2021/ReversedFullField/dev/2021/";}
  elsif ($pwd =~ /2021\/100GeV_fixedTarget/)  {$glob = "/reco/production_100GeV_fixedTarget_2021/ReversedFullField/dev/2021/";}
  elsif ($pwd =~ /2021\/OO_200GeV/)           {$glob = "/reco/production_OO_200GeV_2021/ReversedFullField/dev/2021/";}
  elsif ($pwd =~ /2021\/ps_OO_200GeV/)        {$glob = "/reco/production_ps_OO_200GeV_2021/ReversedFullField/dev/2021/";}
  elsif ($pwd =~ /2021\/17p3GeV/)             {$glob = "/reco/production_17p3GeV_2021/ReversedFullField/dev/2021/";}
  elsif ($pwd =~ /2021\/26p5GeV/)             {$glob = "/reco/production_26p5GeV_fixedTarget_2021/ReversedFullField/dev/2021/";}
  elsif ($pwd =~ /2018\/isobar/)              {$glob = "/reco/production_isobar_2018/ReversedFullField/P21id/2018/"; $DST = "MuDst";}
  elsif ($pwd =~ /2019\/AuAu200/)             {$glob = "/reco/production_AuAu200_2019/ReversedFullField/P22ia/2019"; $DST = "MuDst";}
  print "pwd = $pwd => glob = $glob\n" if ($debug);
} else {# TFG
  $PICOPATH = "/gpfs01/star/pwg_tasks/tfg02"; 
#  if (! -r  $PICOPATH) {$PICOPATH = "/net/l401/data/scratch1/reco";}
  if (! -r  $PICOPATH) {$PICOPATH = "/hlt/cephfs/Pico";}
  print "PICOPATH = $PICOPATH\n" if ($debug);
  if (! -r $PICOPATH) {exit 1;}
  if    ($pwd =~ /2010\/11GeV/)               {$glob = "/2010/11GeV";}
  elsif ($pwd =~ /2010\/19GeV/)               {$glob = "/2010/19GeV";}
  elsif ($pwd =~ /2010\/7GeV/)                {$glob = "/2010/7GeV";}
  elsif ($pwd =~ /2011\/27GeV/)               {$glob = "/2011/27GeV";}
  elsif ($pwd =~ /2014\/TFG21l/)              {$glob = "/2014/TFG21l";}
  elsif ($pwd =~ /2014\/SL21c/)               {$glob = "/2014/SL21c";}
  elsif ($pwd =~ /2018\/27GeV/)               {$PICOPATH = "/gpfs01/star/data*"; $glob = "/reco/production_27GeV_fixedTarget_2018/ReversedFullField/P19ie/2018";}
#  elsif ($pwd =~ /2018\/3p85GeV_fixedTarget/) {$glob = "/2018/3p85GeV_fixedTarget";}
  elsif ($pwd =~ /2018\/3p85GeV_fixedTarget/) {$PICOPATH = "/gpfs01/star/data*";$glob = "/reco/production_3p85GeV_fixedTarget_2018/ReversedFullField/P19ie.SL20d/2018";}

  elsif ($pwd =~ /2019\/14GeV/)               {$glob = "/2019/14GeV_2019_TFG19e";}
  elsif ($pwd =~ /2019\/19GeV_2019_TFG19e/)   {$PICOPATH = "/gpfs01/star/pwg/fisyak/Pico/2019/TFG19e/RF/19GeV"; $glob = "";}
  elsif ($pwd =~ /2019\/19GeV/)               {$glob = "/2019/19GeV_2019_TFG19e";}
  elsif ($pwd =~ /2019\/31GeV_fixedTarget/)   {$glob = "/2019/31GeV_fixedTarget_2019_TFG19l.Minuit";}
  elsif ($pwd =~ /2019\/26p2GeV_fixedTarget/)   {$glob = "/2019/26p2GeV_fixedTarget_2019_TFG19l.Minuit";}
  elsif ($pwd =~ /2019\/4p59GeV_fixedTarget/) {$glob = "/2019/4p59GeV_2019_TFG19e";}
  elsif ($pwd =~ /2019\/3p85GeV_fixedTarget/) {$glob = "/2019/3p85GeV_Fixed_2019_TFG19e";}
  elsif ($pwd =~ /2019\/7\.3GeV_fixedTarget/) {$glob = "/2019/7.3GeV_Fixed_2019_TFG19e";}
  elsif ($pwd =~ /2019\/7p7GeV/)              {$glob = "/2019/7p7GeV_2019_TFG19e";}
  elsif ($pwd =~ /2019\/9p2GeV/)              {$glob = "/2019/9p2GeV_2019_TFG19e";}
  elsif ($pwd =~ /2019\/AuAu200/)             {$glob = "/2019/AuAu200";}
#  TFG19m/RF/11p5GeV.B/            344-365 aka 11p5GeV
#  TFG19m/RF/11p5GeV.B/            001-027 
#  TFG19m/RF/5p75GeV_fixedTarget.B/355
#  TFG20a/RF/31p2GeV_fixedTarget/  028-029
#  TFG20a/RF/9p8GeV_fixedTarget/   029
#  TFG20a/RF/9p2GeV/               030
#  TFG20a/RF/9p8GeV_fixedTarget/   030-031
#  TFG20a/RF/19p5GeV_fixedTarget/  032
#  TFG20a/RF/9p8GeV_fixedTarget/   032
#  TFG20a/RF/13p5GeV_fixedTarget/  033
#  TFG20a/RF/19p5GeV_fixedTarget/  033
#  TFG20a/RF/13p5GeV_fixedTarget/  034
#  TFG20a/RF/9p2GeV/               034
#  TFG20a/RF/7p3GeV_fixedTarget/   035-036
#  TFG20a/RF/9p2GeV/               036-040
#  TFG20a/RF/9p2GeV/               041
#  TFG20a/RF/11p5GeV/              041-055 aka 11p5GeV.C
#  TFG20a/RF/5p75GeV_fixedTarget/  044
#  TFG20a/RF/5p75GeV_fixedTarget/  045
#  TFG20a/RF/9p2GeVb/              055-080
#  TFG20a/RF/9p2GeVc/              169-218
#  TFG20a/RF/26p5GeV_fixedTarget/  211
  elsif ($pwd =~ /2020\/5p75GeV_fixedTargetC/){$glob = "/2020/TFG20a/RF/5p75GeV_fixedTarget";}
  elsif ($pwd =~ /2020\/5p75GeV_fixedTarget/) {$glob = "/2020/TFG19m/RF/5p75GeV_fixedTarget.B";}
  elsif ($pwd =~ /2020\/11p5GeV.C/)           {$glob = "/2020/TFG20a/RF/11p5GeV";}
  elsif ($pwd =~ /2020\/11p5GeV/)             {$glob = "/2020/TFG19m/RF/11p5GeV.B";}
  elsif ($pwd =~ /2020\/31p2GeV_fixedTarget/) {$glob = "/2020/TFG20a/RF/31p2GeV_fixedTarget";}
  elsif ($pwd =~ /2020\/9p8GeV_fixedTarget/)  {$glob = "/2020/TFG20a/RF/9p8GeV_fixedTarget";}
  elsif ($pwd =~ /2020\/9p2GeVb/)             {$glob = "/2020/TFG20a/RF/9p2GeVb";}
  elsif ($pwd =~ /2020\/9p2GeVc/)             {$glob = "/2020/TFG20a/RF/9p2GeVc";
					       print "pwd = $pwd +> glob = $glob; PICOPATH = $PICOPATH\n" if ($debug);
					     }
  elsif ($pwd =~ /2020\/9p2GeV/)              {$glob = "/2020/TFG20a/RF/9p2GeV";}
  elsif ($pwd =~ /2020\/13p5GeV_fixedTarget/) {$glob = "/2020/TFG20a/RF/13p5GeV_fixedTarget";}
  elsif ($pwd =~ /2020\/19p5GeV_fixedTarget/) {$glob = "/2020/TFG20a/RF/19p5GeV_fixedTarget";}
  elsif ($pwd =~ /2020\/26p5GeV_fixedTarget/) {$glob = "/2020/TFG20a/RF/26p5GeV_fixedTarget";}
  elsif ($pwd =~ /2020\/7p3GeV_fixedTarget/)  {$glob = "/2020/TFG20a/RF/7p3GeV_fixedTarget";}
  elsif ($pwd =~ /2020\/9p2GeV/)              {$glob = "/2020/TFG20a/RF/9p2GeV";}
  elsif ($pwd =~ /2020\/7p7GeV/)              {$glob = "/2020/TFG20a/RF/7p7GeV";}
  elsif ($pwd =~ /2021\/7p7GeV_2021/) {
#    $PICOPATH = "/hlt/cephfs/reco";
#    print "PICOPATH = $PICOPATH\n" if ($debug);
#    if ($pwd =~ /7p7GeV_2021/)                {$glob = "/2021/RF/DEV2/7p7GeV_2021.C";}
#    if    ($pwd =~ /7p7GeV_2021\/TFG21c\.A/)   {$glob = "/2021/RF/TFG21c.A/7p7GeV_2021";}
#    elsif ($pwd =~ /7p7GeV_2021\/TFG21c\.B/)   {
#    else {die "Not set yet";}
    if ($pwd =~ /3p85GeV_fixedTarget_2021/)   {$glob = "/2021/FF/TFG21e/3p85GeV_fixedTarget_2021";}
    elsif ($pwd =~ /7p7GeV_2021_FF/)          {$glob = "/2021/FF/TFG21c.B/FF_7p7GeV_2021_FF";}
    elsif ($pwd =~ /7p7GeV_2021/)             {$glob = "/2021/RF/TFG21c.B/7p7GeV_2021";}
  } 
  elsif ($pwd =~ /2021\/3p85GeV_fixedTarget_2021\.TFG21g\.B/) {$glob = "/2021/RF/TFG21g.B/3p85GeV_fixedTarget_2021";}
  elsif ($pwd =~ /2021\/3p85GeV_fixedTarget_2021\.TFG21g/) {$glob = "/2021/RF/TFG21g/3p85GeV_fixedTarget_2021";}
  elsif ($pwd =~ /2021\/3p85GeV_fixedTarget_2021\.TFG21f/) {$glob = "/2021/RF/TFG21f/3p85GeV_fixedTarget_2021";}
  elsif ($pwd =~ /2021\/3p85GeV_fixedTarget_2021/) {$glob = "/2021/RF/TFG21e/3p85GeV_fixedTarget_2021";}
  elsif ($pwd =~ /2021\/100GeV_fixedTarget_2021/)  {$glob = "/2021/RF/TFG21e/100GeV_fixedTarget_2021";}
  elsif ($pwd =~ /2021\/44p5GeV_fixedTarget_2021/) {$glob = "/2021/RF/TFG21e/44p5GeV_fixedTarget_2021";}
  elsif ($pwd =~ /2021\/70GeV_fixedTarget_2021/)   {$glob = "/2021/RF/TFG21e/70GeV_fixedTarget_2021";}
  elsif ($pwd =~ /2021\/ps_OO_200GeV_2021/)        {$glob = "/2021/RF/TFG21e/ps_OO_200GeV_2021";}
  elsif ($pwd =~ /2021\/OO_200GeV_2021/)           {$glob = "/2021/RF/TFG21e/OO_200GeV_2021";}
  elsif ($pwd =~ /2021\/FF_OO_200GeV_2021f/)       {$glob = "/2021/FF/TFG21f/FF_OO_200GeV_2021";}
  elsif ($pwd =~ /2021\/FF_OO_200GeV_2022g/)       {$glob = "/2021/FF/TFG22g/FF_OO_200GeV_2021";}
  elsif ($pwd =~ /2021\/FF_OO_200GeV_2021/)        {$glob = "/2021/FF/TFG21e/FF_OO_200GeV_2021";}
  elsif ($pwd =~ /2021\/17p3GeV_2021/)             {$glob = "/2021/RF/TFG21e/17p3GeV_2021";}
  elsif ($pwd =~ /2021\/26p5GeV/)                  {$glob = "/2021/RF/TFG21g.B/26p5GeV_fixedTarget_2021/";}
  elsif ($pwd =~ /2022\/pp500_2022/)               {$glob = "/2022/RF/pp500_2022/";}
}
print "PICOPATH = $PICOPATH; days = $dayMin  - $dayMax : glob = $glob\n" if ($debug);
#if (! $glob) {die "glob = $glob";}
if (! $PICOPATH) {die "PICOPATH = $PICOPATH";}
#if ($glob == "" or $PICOPATH == "") {die "glob = $glob, PICOPATH = $PICOPATH";}
my $GLOB = $PICOPATH . $glob . "/???/*";
my @files = glob $GLOB; print "$GLOB => found $#files\n" if ($debug);
# if ($#files < 0) {
#   $GLOB = $PICOPATH . $glob; 
#   @files = glob $GLOB; print "$GLOB => found $#files\n" if ($debug);
# }
print "PICOPATH = $PICOPATH; days = $dayMin  - $dayMax : GLOB = $GLOB\n" if ($debug);
my %Runs= ();
foreach my $run (@files) {
  my $f = File::Basename::basename($run);
  my $day = int ($f/1000);       # print "day = $day\n";
  my $Y = int ($day/1000);    # print "Y = $Y\n";
  $day -=  1000*$Y;           # print "day = $day\n";
  if ($dayMin > 0 && $day < $dayMin) {next;}
  if ($dayMax > 0 && $day > $dayMax) {next;}
  my @files =  glob $run . "/*" . $DST . ".root";
  my $NF = $#files + 1;
  my $step = 20;
  for (my $i = 0; $i < $NF; $i += $step) {
    my @list = ();
    my @listB = ();
    for (my $k = 0; $k < $step; $k++) {
      my $j = $k + $i;
      if ($k < $NF && $files[$j]) {
	push @list, $files[$j];
	my $fileB =  File::Basename::basename( $files[$j] );
	push @listB,$fileB;
      } 
    }
    $Runs{$f}++;
    my $ana = $f . "_" . $Runs{$f} . ".root"; print "ana = $ana\n" if ($debug);
    if ( -r $ana) {
      my $mtime = stat($ana)->mtime;
      my $Mtime = ctime($mtime);
      #     my ($dev,$ino,$mode,$nlink,$uid,$gid,$dev, $size, $atime, $mtime, $ctime, $blksize,$blocks) = stat($ana);
      #    my @files = glob $run . "/*.root";
      if ($#list < 0) {next;}
      my $mtimeA = -1;
      my $MtimeA;
      foreach my $file (@list) {
	my $mtimeR = stat($file)->mtime;
	my $MtimeR = ctime($mtimeR);
	#       my ($devR,$inoR,$modeR,$nlinkR,$uidR,$gidR,$devR, $sizeR, $atimeR, $mtimeR, $ctimeR, $blksizeR,$blocksR) = stat($run);
	my $dt = $mtime - $mtimeR;
	#       print "$ana: $mtime,$Mtime  $file: $mtimeR,$MtimeR dt = $dt\n";
	#       my @list = `ls -l $ana $file`; print "@list\n";
	if ( $mtimeR > $mtimeA) { $mtimeA =  $mtimeR; $MtimeA = $MtimeR;}
      }
      my $dt = $mtime - $mtimeA;
      print "$day, $ana $mtime,$Mtime, $run $mtimeA,$MtimeA, dt = $dt\n" if ($debug);
      if ($dt > 0) {next;}
      my $cmd = "mv ". $ana ." ". $ana .".BAK";
      print "$cmd \n" if ($debug);
      my $flag = system($cmd);
    }
    print "string:$run:$ana:$year:$DST:@listB\n";
    $Njobs++;
  }
}
if (! $Njobs) {die "Don't have input files\n";}
