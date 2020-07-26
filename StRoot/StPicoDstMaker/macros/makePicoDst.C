/* 
   root.exe 'lMuDst.C(-1,"/gpfs02/eic/ayk/STAR/reco/MuDst/AuAu_200_production_2016/ReversedFullField/P16ij/2016/125/17125034/st_physics_adc_17125034_raw_1000007.MuDst.root","RMuDst,mysql,magF,nodefault,picoWrite,quiet")' 'makePicoDst.C+("y2016")'
   root.exe 'lMuDst.C(-1,"/gpfs02/eic/ayk/STAR/reco/MuDst/AuAu_200_production_2016/ReversedFullField/P16ij/2016/125/17125034/st_physics_adc_17125034_raw_1000007.MuDst.root","RMuDst,mysql,magF,nodefault,picoWrite")' 'makePicoDst.C+("y2016")'
   root.exe 'lMuDst.C(-1,"/gpfs02/eic/ayk/STAR/reco/MuDst/AuAu_200_production_2016/ReversedFullField/P16ij/2016/125/17125034/st_physics_adc_17125034_raw_1000007.MuDst.root","RMuDst,mysql,magF,nodefault,picoWrite,quiet,TTreeFile")' 'makePicoDst.C+("y2016")'
   root.exe 'lMuDst.C(-1,"/gpfs02/eic/ayk/STAR/reco/MuDst/2010/114/11114040/*.MuDst.root","RMuDst,mysql,magF,nodefault,picoWrite,quiet,TTreeFile",0,"11114040.picoDst.root")' makePicoDst.C+
   root.exe 'lMuDst.C(-1,"/gpfs02/eic/ayk/STAR/reco/MuDst/2010/114/11114040/st_physics_adc_11114040_raw_1520001.MuDst.root","RMuDst,mysql,magF,nodefault,picoWrite,quiet,TTreeFile")' makePicoDst.C+
   root.exe 'lMuDst.C(-1,"/net/l404/data/fisyak/reco/2016/AuAu200_adc/st_physics_adc_17134047_raw_3500050.MuDst.root","RMuDst,mysql,magF,nodefault,picoWrite,quiet,TTreeFile")' makePicoDst.C+
   root.exe 'lMuDst.C(-1,"/net/l401/data/scratch2/fisyak/MuDst/2016/125/17125034/st_physics_17125034_raw_1000002.MuDst.root","RMuDst,mysql,magF,nodefault,picoWrite,quiet,TTreeFile")' makePicoDst.C+
   root.exe 'lMuDst.C(-1,"/net/l404/data/fisyak/reco/2016/Hijing/VMC.Real.TFG17j/hijingAuAu200_65_1000.MuDst.root","RMuDst,mysql,magF,nodefault,picoWrite,quiet,TTreeFile")' makePicoDst.C+
   root.exe 'lMuDst.C(-1,"root://xrdstar.rcf.bnl.gov:port//home/starlib/home/starreco/reco/AuAu62_production/ReversedFullField/P10ik/2010/078/11078018/st_physics_11078018_raw_5020001.MuDst.root","RMuDst,mysql,magF,nodefault,picoWrite,quiet,TTreeFile")' 'makePicoDst.C+("y2011")'
   root.exe 'lMuDst.C(-1,"root://xrdstar.rcf.bnl.gov:1095//home/starlib/home/starreco/reco/AuAu11_production/ReversedFullField/P10ih/2010/149/11149011/st_physics_11149011_raw_1010001.MuDst.root","RMuDst,mysql,magF,nodefault,picoWrite,PicoVtxDefault,quiet")' 'makePicoDst.C("y2010")'
   root.exe  'lMuDst.C(-1,"root://xrdstar.rcf.bnl.gov:1095//home/starlib/home/starreco/reco/AuAu7_production/ReversedFullField/P10ih/2010/114/11114040/st_physics_11114040_raw_1010001.MuDst.root","RMuDst,mysql,magF,nodefault,picoWrite,PicoVtxDefault,quiet")' 'makePicoDst.C("y2010")'
*/
#include "TSystem.h"
#include "Riostream.h"
#if !defined(__CINT__) &&  !defined(__CLING__)
#include "StChain/StMaker.h"
#include "StBFChain/StBFChain.h" 
#include "StPicoDstMaker/StPicoDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h" 
#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"
#include "StPreEclMaker/StPreEclMaker.h"
#include "StEpcMaker/StEpcMaker.h"
#include "StTriggerUtilities/StTriggerSimuMaker.h"
#include "StTriggerUtilities/Bemc/StBemcTriggerSimu.h"
#include "StPicoDstMaker/StPicoDstMaker.h"
#include "StEventUtilities/StGoodTrigger.h"
#else
class StGoodTrigger;
#endif 
void makePicoDst(TString triggerSet = "y2019") {
  Int_t nEvents = 10000000;
  StBFChain *chain = (StBFChain *) StMaker::GetTopChain();
  StMuDstMaker *MuDstMaker = (StMuDstMaker *) chain->Maker("MuDst");
  MuDstMaker->SetStatus("*",0);
  MuDstMaker->SetStatus("MuEvent",1);
  MuDstMaker->SetStatus("PrimaryVertices",1);
  MuDstMaker->SetStatus("PrimaryTracks",1);
  MuDstMaker->SetStatus("GlobalTracks",1);
  MuDstMaker->SetStatus("CovGlobTrack",1);
  MuDstMaker->SetStatus("BTof*",1);
  MuDstMaker->SetStatus("Emc*",1);
  MuDstMaker->SetStatus("MTD*",1);
#if 0
  MuDstMaker->SetStatus("StStMuMcVertex",1);
  MuDstMaker->SetStatus("StStMuMcTrack",1);
#endif
  StMaker *detDb = chain->Maker("detDb");
  detDb->SetActive(kFALSE);
  StMaker *tpcDB = chain->Maker("tpcDB");
  tpcDB->SetActive(kFALSE);
  StPicoDstMaker *PicoDstMaker = (StPicoDstMaker *) chain->Maker("PicoDst");
#if 0
  if (triggerSet.Contains("y2014",TString::kIgnoreCase) || triggerSet.Contains("y2016",TString::kIgnoreCase)) {
    PicoDstMaker->SetVxZrange(-6,6);
    PicoDstMaker->SetVxRmax(2);
  } else {
    PicoDstMaker->SetVxZrange(-70,70);
    PicoDstMaker->SetVxRmax(2);
  }
  PicoDstMaker->SetMaxTrackDca(0);
#endif
  chain->SetAttr(".Privilege",1,"StMuDstMaker::*");
  StGoodTrigger tiggers(triggerSet);
  chain->Init();
  chain->EventLoop(nEvents);
}
/* /net/l401/data/scratch2/fisyak/MuDst/2016/125/17125034/st_physics_17125034_raw_1500052.MuDst.root
  10/06/17 MuDst: 3895236991 default PicoDst  : 571095720 : ratio = 6.82
            dcaG2            add dca          :1442666170 :         2.7   a factor 2.5
  10/09/17  1                 Float16_t       :1054673602 :         3.7   a factor 1.84
  10/10/17  2                 & dca3D < 10 cm : 380350741 :        10.2   a factor 0.67
  10/10/17  3                 & dca3D < 50 cm : 659035612 :         5.91  a factor 1.15, reduction 1.6 wrt no DcaCut
  10/11/17  4                 & picoTrack     : 450660011 :         8.64 
  10/11/17  5                  no dca3D cut   : 712392776 :         5.47 
  10/11/17  6                 Mtd Float16     : 712377335 :         5.47
  10/14/17  7                                 : 712262142 :
  10/14/17  8                 DcaCut and Event: 312097307 :        12.48
  10/16/17
  11/15/17  9                 Update packing  : 268419555 :        14.51
  11/21/17 10                 Repack          : 306863894 :        12.69
  11/22/17 11                 Don't use Vx cut: 344517924 :        11.31
5.3T    scratch1/MuDst/2016
13T     scratch2/MuDst/2016  chained 6071 files      with total 29483322 events
1.3T    work/Pico/2016       chained 6071 files      with total 19120471 events
=====
ratio = 14

*/
