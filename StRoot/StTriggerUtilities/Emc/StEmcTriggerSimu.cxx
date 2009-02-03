//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 12 Jan 2009
//

#include "St_db_Maker/St_db_Maker.h"
#include "StTriggerUtilities/Bemc/StBemcTriggerSimu.h"
#include "StTriggerUtilities/Eemc/StEemcTriggerSimu.h"
#include "StTriggerUtilities/StDSMUtilities/StDSM2009Utilities.hh"
#include "StEmcTriggerSimu.h"

ClassImp(StEmcTriggerSimu);

StEmcTriggerSimu::StEmcTriggerSimu()
{
  mYear = 0;
  mBemc = 0;
  mEemc = 0;
  mEM201 = new DSMLayer_EM201_2009;
}

StEmcTriggerSimu::~StEmcTriggerSimu()
{
  delete mEM201;
}

void StEmcTriggerSimu::setBemc(StBemcTriggerSimu* bemc)
{
  mBemc = bemc;
}

void StEmcTriggerSimu::setEemc(StEemcTriggerSimu* eemc)
{
  mEemc = eemc;
}

void StEmcTriggerSimu::InitRun(int runNumber)
{
  St_db_Maker* starDb = (St_db_Maker*) StMaker::GetChain()->GetMakerInheritsFrom("St_db_Maker");
  assert(starDb);
  mYear = starDb->GetDateTime().GetYear();

  mAllTriggers.insert(2009500); //test trigger for 2009 500 GeV
}

void StEmcTriggerSimu::Make()
{



  if (mYear < 2009) return;

  switch (mYear) {
  case 2009:
    if (mBemc) mBemc->get2009_DSMLayer1_Result()->write(*mEM201);
    //if (mEemc) mEemc->get2009_DSMLayer1_Result()->write(*mEM201);
    break;
    }

  mEM201->run();
}


StTriggerSimuDecision StEmcTriggerSimu::triggerDecision(int trigId)
{
  enum {
    BHT0 = 0,
    BHT1 = 1,
    BHT2 = 2,
    BHT3 = 3,
    EHT2 = 4,
    EHT4 = 5,
    JP1  = 6,
    JP2  = 7,
    BJP1 = 8,
    BJP2 = 9,
    EJP1 = 10,
    EJP2 = 11,
    AJP  = 12,
    BAJP = 13,
    EAJP = 14
  };



  int out = (*mEM201)[0].output;
  int decision = 0;

  switch (trigId) {
  case BHT0: decision = out >>  0 & 0x1; break;
  case BHT1: decision = out >>  1 & 0x1; break;
  case BHT2: decision = out >>  2 & 0x1; break;
  case BHT3: decision = out >>  3 & 0x1; break;
  case EHT2: decision = out >>  4 & 0x1; break;
  case EHT4: decision = out >>  5 & 0x1; break;
  case JP1 : decision = out >>  6 & 0x1; break;
  case JP2 : decision = out >>  7 & 0x1; break;
  case BJP1: decision = out >>  8 & 0x1; break;
  case BJP2: decision = out >>  9 & 0x1; break;
  case EJP1: decision = out >> 10 & 0x1; break;
  case EJP2: decision = out >> 11 & 0x1; break;
  case AJP : decision = out >> 12 & 0x1; break;
  case BAJP: decision = out >> 13 & 0x1; break;
  case EAJP: decision = out >> 14 & 0x1; break;
  }


  //first check if it fired

  mFiredTriggers.push_back(2009500); 

  for(unsigned i=0; i<mFiredTriggers.size(); i++) {
    if(trigId == mFiredTriggers[i]) return kYes;
  }
  
  //now check if we care
  if(mAllTriggers.find(trigId) == mAllTriggers.end()) {
    return kDoNotCare;
  }
  else {
    return kNo;
  }

}






