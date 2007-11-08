//////////////////////////////////////////////////////////////////////////
//
//
// StTriggerSimuMaker R.Fatemi, Adam Kocoloski , Jan Balewski  (Fall, 2007)
//
// Goal: generate trigger response based on ADC
// implemented BEMC,EEMC,....
// >StTriggerSimu/*SUB*/St*SUB*TriggerSimu.h
// >where *SUB* are the subsystems: Eemc, Bemc, Bbc,.... 
// > L2 is served by a separate maker
//
//////////////////////////////////////////////////////////////////////////


#include <Stiostream.h>
#include "StChain.h"
#include "TFile.h"
#include <math.h>

#include <fstream>
#include "StDaqLib/EMC/StEmcDecoder.h"

//#include "StEEmcPool/StL2Emulator/StL2EmulatorMaker.h"
#include "St_db_Maker/St_db_Maker.h" // just for time stamp
#include "StEEmcUtil/EEdsm/EMCdsm2Tree.h"// to access Etot

//StEvent
#include "StEvent/StEvent.h"

//get  EEMC
#include "Eemc/StEemcTriggerSimu.h"
#include "Eemc/EemcHttpInfo.h"
#include "StEEmcUtil/EEdsm/EEfeeTPTree.h" // for printouts only
#include "StEEmcUtil/EEdsm/EEfeeTP.h"  // for printouts only

//get BEMC
#include "StTriggerUtilities/Bemc/StBemcTriggerSimu.h"
#include "StEmcRawMaker/StBemcTables.h"
#include "StEmcUtil/geometry/StEmcGeom.h"

//get BBC
#include "Bbc/StBbcTriggerSimu.h"

//get L2
#include "L2Emulator/StL2TriggerSimu.h"

//get HEAD Maker
#include "StTriggerSimuMaker.h"


ClassImp(StTriggerSimuMaker)

StTriggerSimuMaker::StTriggerSimuMaker(const char *name):StMaker(name) {
  mDbMk = dynamic_cast<St_db_Maker*> ( this->GetMakerInheritsFrom("St_db_Maker") );
  mYear=-1;
  mMCflag=0;
  eemc=0;
  bbc=0;
  bemc=0;
  lTwo=0;
}

//========================================
StTriggerSimuMaker::~StTriggerSimuMaker(){
}

//========================================
void 
StTriggerSimuMaker::useEemc(int flag){
  eemc=new StEemcTriggerSimu();
  eemc->setConfig(flag);
  mSimulators.push_back(eemc);
}

//========================================
void 
StTriggerSimuMaker::useBbc(){
  bbc=new StBbcTriggerSimu;
  mSimulators.push_back(bbc);
}

//========================================
void
StTriggerSimuMaker::useBemc(){
  bemc=new StBemcTriggerSimu;
  bemc->setHeadMaker(this);
  mSimulators.push_back(bemc);
}

//________________________________________________
void
StTriggerSimuMaker::useL2(){
  lTwo=new StL2TriggerSimu;
  mSimulators.push_back(lTwo);
}

//_____________________________________________________________________________
Int_t 
StTriggerSimuMaker::Init() {
  LOG_INFO <<Form("StTriggerSimuMaker::Init(), MCflag=%d",mMCflag)<<endm;
  
  if(eemc) {
    eemc->setHList(mHList);
  }

  if(bemc) {
    bemc->setHList(mHList);
  }
  
  for(unsigned i=0; i<mSimulators.size(); i++) {
    mSimulators[i]->setMC(mMCflag);
    mSimulators[i]->Init();
  }

 return StMaker::Init();
}

//========================================
void 
StTriggerSimuMaker::Clear(const Option_t*){
  LOG_DEBUG<<"StTriggerSimuMaker::Clear()"<<endm;
  
  mTriggerList.clear();
  
  for(unsigned i=0; i<mSimulators.size(); i++) {
    mSimulators[i]->Clear();
  }
}


//========================================
Int_t
StTriggerSimuMaker::InitRun  (int runNumber){  
  for(unsigned i=0; i<mSimulators.size(); i++) {
    mSimulators[i]->InitRun(runNumber);
  }

  assert(mDbMk);
  mYear=mDbMk->GetDateTime().GetYear();
  int yyyymmdd=mDbMk->GetDateTime().GetDate(); //form of 19971224 (i.e. 24/12/1997)
  int hhmmss=mDbMk->GetDateTime().GetTime(); //form of 123623 (i.e. 12:36:23)
  
  LOG_INFO<<Form("InitRun() run=%d yyyymmdd=%d  hhmmss=%06d\n",runNumber, yyyymmdd, hhmmss )<<endm;
  
  return kStOK;
}

//========================================
Int_t 
StTriggerSimuMaker::Make(){
  for(unsigned i=0; i<mSimulators.size(); i++) {
    mSimulators[i]->Make();
  }
  
  return kStOK;
}


//========================================
bool StTriggerSimuMaker::isTrigger(int trigId) {
  for(unsigned i=0; i<mSimulators.size(); i++) {
    if(mSimulators[i]->triggerDecision(trigId) == kNo) return false;
  }
  return true;
}

//========================================
Int_t 
StTriggerSimuMaker::Finish() {
  return StMaker::Finish();
}

// $Id: StTriggerSimuMaker.cxx,v 1.17 2007/11/08 20:59:33 kocolosk Exp $
//
// $Log: StTriggerSimuMaker.cxx,v $
// Revision 1.17  2007/11/08 20:59:33  kocolosk
// subdet isTrigger returns a bool
// triggerDecision returns enumerator including kDoNotCare
//
// Revision 1.16  2007/10/12 20:10:23  balewski
// cleanup
//
// Revision 1.15  2007/10/12 17:19:16  kocolosk
// move BEMC-specific code to StBemcTriggerSimu
// replace some config methods like setDbMaker with code that finds the Maker automatically
//
// Revision 1.14  2007/10/12 14:36:00  balewski
// added L2 interface
//
// Revision 1.13  2007/10/11 00:32:56  balewski
// L2algo added
//
// Revision 1.12  2007/09/25 18:19:35  rfatemi
// Update for TP work
//
// Revision 1.11  2007/09/24 18:08:11  kocolosk
// some code restructuring
//
// Revision 1.10  2007/09/21 18:45:40  rfatemi
// End of week update
//
// Revision 1.9  2007/08/13 02:51:29  rfatemi
// update before rcf goes offline
//
// Revision 1.8  2007/08/12 01:03:22  rfatemi
// Added flag for offline/online/expert settings
//
// Revision 1.7  2007/08/07 15:48:20  rfatemi
// Added BEMC access
//
// Revision 1.6  2007/07/24 01:32:43  balewski
// *** empty log message ***
//
// Revision 1.5  2007/07/23 02:59:48  balewski
// cleanup, bbc for M-C still not working
//
// Revision 1.4  2007/07/22 23:09:34  rfatemi
// Add Bbc access
//
// Revision 1.2  2007/07/21 23:35:24  balewski
// works for M-C
//
// Revision 1.1  2007/07/20 21:03:18  balewski
// start
//
