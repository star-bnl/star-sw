/* 
   root.exe 'lMuDst.C(-1,"/gpfs02/eic/ayk/STAR/reco/MuDst/AuAu_200_production_2016/ReversedFullField/P16ij/2016/125/17125034/st_physics_adc_17125034_raw_1000007.MuDst.root","RMuDst,mysql,magF,nodefault,picoWrite,quiet")' 'makePicoDst.C+("y2016")'
   root.exe 'lMuDst.C(-1,"/gpfs02/eic/ayk/STAR/reco/MuDst/AuAu_200_production_2016/ReversedFullField/P16ij/2016/125/17125034/st_physics_adc_17125034_raw_1000007.MuDst.root","RMuDst,mysql,magF,nodefault,picoWrite")' 'makePicoDst.C+("y2016")'
   root.exe 'lMuDst.C(-1,"/gpfs02/eic/ayk/STAR/reco/MuDst/AuAu_200_production_2016/ReversedFullField/P16ij/2016/125/17125034/st_physics_adc_17125034_raw_1000007.MuDst.root","RMuDst,mysql,magF,nodefault,picoWrite,quiet,TTreeFile")' 'makePicoDst.C+("y2016")'
   root.exe 'lMuDst.C(-1,"/gpfs02/eic/ayk/STAR/reco/MuDst/2010/114/11114040/*.MuDst.root","RMuDst,mysql,magF,nodefault,picoWrite,quiet,TTreeFile",0,"11114040.picoDst.root")' makePicoDst.C+
   root.exe 'lMuDst.C(-1,"/gpfs02/eic/ayk/STAR/reco/MuDst/2010/114/11114040/st_physics_adc_11114040_raw_1520001.MuDst.root","RMuDst,mysql,magF,nodefault,picoWrite,quiet,TTreeFile")' makePicoDst.C+
   root.exe 'lMuDst.C(-1,"/net/l404/data/fisyak/reco/2016/AuAu200_adc/st_physics_adc_17134047_raw_3500050.MuDst.root","RMuDst,mysql,magF,nodefault,picoWrite,quiet,TTreeFile")' makePicoDst.C+
   root.exe 'lMuDst.C(-1,"/net/l401/data/scratch2/fisyak/MuDst/2016/125/17125034/st_physics_17125034_raw_1000002.MuDst.root","RMuDst,mysql,magF,nodefault,picoWrite,quiet,TTreeFile")' makePicoDst.C+
   root.exe 'lMuDst.C(-1,"/net/l404/data/fisyak/reco/2016/Hijing/VMC.Real.TFG17j/hijingAuAu200_65_1000.MuDst.root","RMuDst,mysql,magF,nodefault,picoWrite,quiet,TTreeFile")' makePicoDst.C+
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
void makePicoDst(const Char_t *triggerSet = "y2016", const bool creatingPhiWgt = kFALSE, const int prodMod = 0, const int emcMode=1) {
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
#ifdef __Y2010__
  PicoDstMaker->SetVxZrange(-70,70);
  PicoDstMaker->SetVxRmax(2);
#else /* __Y2014__ || __Y2016__ introduced 02/08/2018 for the 2-nd par of y2016 pico production*/
  PicoDstMaker->SetVxZrange(-6,6);
  PicoDstMaker->SetVxRmax(2);
  chain->SetAttr(".Privilege",1,"StMuDstMaker::*");
  StGoodTrigger tiggers(triggerSet);
#endif
#if 0  
  if(!creatingPhiWgt&&emcMode) {
    StEmcADCtoEMaker *adc2e = (StEmcADCtoEMaker *) chain->Maker("bemcA2E");
    //		adc2e->setPrint(false);
    adc2e->saveAllStEvent(true);
    StPreEclMaker *pre_ecl = (StPreEclMaker *) chain->Maker("preecl");
    pre_ecl->setPrint(kFALSE);
    StEpcMaker *epc= (StEpcMaker *)  chain->Maker("epc");
    epc->setPrint(kFALSE);
    // Trigger simulator
    StTriggerSimuMaker* trigSimu = (StTriggerSimuMaker *) chain->Maker("StarTrigSimu");
    trigSimu->setMC(false);
    trigSimu->useBemc();
    trigSimu->useEemc();
    trigSimu->useOnlineDB();
    trigSimu->bemc->setConfig(StBemcTriggerSimu::kOffline);
  }
#endif
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
