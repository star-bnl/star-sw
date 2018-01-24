How to set enviromnet on HLT farm
1. Setup .DEV2 with compiler and optimization version.
  (a) Reset GROUP_DIR set version .DEV2, list of TFG release will be printed
  source /afs/rhic.bnl.gov/star/packages/.DEV2/setupDEV2.csh
  (b) Default complier is your default compiler. To change it 
  setup gcc
  setup 32b
  (c) Default optimization level is DEBUG. To set optimized version
  setenv NODEBUG yes
  (d) Set release version
  starver .DEV2 // for HEAD of git repository now it is taged as TFG18b
  starver TFG18a 


2. 2016 AuAu200 Hijing simulation
     new CA in .DEV2 
       /net/l404/data/fisyak/reco/2016/Hijing/VMC.TFG18b.newdCA
     old CA in TFG18a
       /net/l404/data/fisyak/reco/2016/Hijing/VMC.TFG18a.oldCA
There are log files *B.log where you can see the chain to run
more /net/l404/data/fisyak/reco/2016/Hijing/VMC.TFG18a.oldCA/hijingAuAu200_99_1000B.log
...
Processing bfc.C(100,"genIn,MC.2016a,istSlowSim,-bbcSim,StiCA,-hitfilt,StiHftC,-geantOut,-evout,vmc,VMCAlignment,CorrX,OSpaceZ2,OGridLeak3D,-useXgeom,NoHistos,noTags,noRunco,sdt20160301,ZCut5cm","/net/l401/data/scratch2/fisyak/simu/Hijing/AuAu200/hijingAuAu200_99_1000.gener.root")...
...
i.e. 
   you need to run
 
root.exe 'bfc.C(100,"genIn,MC.2016a,istSlowSim,-bbcSim,StiCA,-hitfilt,StiHftC,-geantOut,-evout,vmc,VMCAlignment,CorrX,OSpaceZ2,OGridLeak3D,-useXgeom,NoHistos,noTags,noRunco,sdt20160301,ZCut5cm","/net/l401/data/scratch2/fisyak/simu/Hijing/AuAu200/hijingAuAu200_99_1000.gener.root")'
