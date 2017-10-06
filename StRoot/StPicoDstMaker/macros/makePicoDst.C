/* 
   root.exe 'lMuDst.C(-1,"/star/subsys/tpc/fisyak/reco/2014/50M/SL15StiCAKFV/130/15130037/st_physics_*15130037*.MuDst.root","StEvent,RMuDst,mysql,tpcDb,eemcDb,magF,nodefault,CorrX,TRGDef,mtdMatch,mtdCalib,eemcD,emcAtoE,PreEcl,Epc,trgSim,picoDST")' makePicoDst.C+
 */
#include "TSystem.h"
#include "Riostream.h"

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
void makePicoDst(const bool creatingPhiWgt = kFALSE, const int prodMod = 0, const int emcMode=1) {
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
  StPicoDstMaker *picoMaker = (StPicoDstMaker *) chain->Maker("PicoDst");
  picoMaker->setProdMode(prodMod); // 0-mb, 1-central, 2-ht
  picoMaker->setEmcMode(emcMode); // 0-No EMC, 1-EMC ON
  //        picoMaker->SetDebug(1);
  chain->Init();
  chain->EventLoop(nEvents);
}
