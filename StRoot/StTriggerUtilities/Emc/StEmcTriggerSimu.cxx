//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 12 Jan 2009
//

// STAR
#include "StMaker.h"

// Local
#include "StTriggerUtilities/Bemc/StBemcTriggerSimu.h"
#include "StTriggerUtilities/Eemc/StEemcTriggerSimu.h"
#include "StTriggerUtilities/StDSMUtilities/StDSM2009Utilities.hh"
#include "StEmcTriggerSimu.h"

ClassImp(StEmcTriggerSimu);

StEmcTriggerSimu::StEmcTriggerSimu()
  : mBemc(0)
  , mEemc(0)
  , mTcu(new TCU)
{
}

StEmcTriggerSimu::~StEmcTriggerSimu()
{
  delete mEM201; mEM201 = 0;
  delete mLD301; mLD301 = 0;
  delete mTcu; mTcu = 0;
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
  // Get DB timestamp
  StMaker* chain = StMaker::GetChain();
  assert(chain);
  mDBTime = chain->GetDBTime();
  mYear = mDBTime.GetYear();

  mLD301 = new DSMLayer_LD301_2009;
  if(mYear == 2013){
     if(runNumber < 14081067){
       mEM201 = new DSMLayer_EM201_2009;
     }else if(runNumber < 14084042){
       mEM201 = new DSMLayer_EM201_2013_A;
     }else{
       mEM201 = new DSMLayer_EM201_2013;
     }

  }else if(mYear == 2015){
   mEM201 = new DSMLayer_EM201_2015;
  }else if(mYear == 2016){
   mEM201 = new DSMLayer_EM201_2014_B;
  }else 
    mEM201 = new DSMLayer_EM201_2009;
}

void StEmcTriggerSimu::Make()
{
  if (mYear >= 2009) {
    if (mBemc) mBemc->get2009_DSMLayer1_Result()->write(*mEM201);
    if (mEemc) mEemc->get2009_DSMLayer1_Result()->write(*mEM201);

    TString EM201String = "EM201: ";
    for (int ch = 0; ch < 8; ++ch) EM201String += Form("%04x ",(*mEM201)[0].channels[ch]);
    LOG_DEBUG << EM201String << endm;
    
    mEM201->run();

    if(mYear == 2013){
      LOG_INFO << Form("EM201: BHT0=%d BHT1=%d BHT2=%d BHT3=%d EHT0=%d EHT1=%d JP1=%d JP2=%d BJP1=%d EEMCdijet=%d EJP1=%d JP1dijet=%d JP0dijet=%d BAJP=%d DAQ10k=%d JP0=%d",
		     BHT0(),BHT1(),BHT2(),BHT3(),EHT0(),EHT1(),JP1(),JP2(),BJP1(),EEMCdijet(),EJP1(),JP1dijet(),JP0dijet(),BAJP(),DAQ10k(),JP0()) << endm;
    }else if(mYear == 2015){
      LOG_INFO << Form("EM201: BHT0=%d BHT1=%d BHT2=%d HTTP=%d EHT0=%d EHT1=%d JP1=%d JP2=%d BJP1=%d BJP2=%d EJP1=%d EJP2=%d AJP=%d BAJP=%d EB2B=%d JP0=%d",
		     BHT0(),BHT1(),BHT2(),HTTP(),EHT0(), EHT1(), JP1(), JP2(), BJP1(),BJP2(), EJP1(),EJP2(),AJP(), BAJP(), EB2B(), JP0()) << endm;
    }else if(mYear == 2016){
      LOG_INFO << Form("EM201: BHT0=%d BHT1=%d BHT2=%d BHT3=%d BHT4=%d BHTUPC=%d BTP=%d BHTTP=%d BTPtopo=%d BHTTPtopo=%d BHT4topo=%d EHT0=%d EHT1=%d DAQ10k=%d",
		     BHT0(),BHT1(),BHT2(),BHT3(),BHT4(), BHTUPC(), BTP(), BHTTP(),BTPtopo(),BHTTPtopo(),BHT4topo(),EHT0_2014(),EHT1_2014(),DAQ10k_2014()) << endm;
    }else{
      LOG_INFO << Form("EM201: BHT0=%d BHT1=%d BHT2=%d BHT3=%d EHT0=%d EHT1=%d JP1=%d JP2=%d BJP1=%d BJP2=%d EJP1=%d EJP2=%d AJP=%d BAJP=%d EAJP=%d JP0=%d",
		     BHT0(),BHT1(),BHT2(),BHT3(),EHT0(),EHT1(),JP1(),JP2(),BJP1(),BJP2(),EJP1(),EJP2(),AJP(),BAJP(),EAJP(),JP0()) << endm;
    }

    //for year 2009 set EM201 to LD301 then to TCU
    if(mYear == 2009)
      {
	mEM201->write(*mLD301);

	TString LD301String = "LD301: ";

	for (int ch = 0; ch < 8; ++ch) LD301String += Form("%04x ",(*mLD301)[0].channels[ch]);
	LOG_DEBUG << LD301String << endm; //changed this line LOG_INFO

	mLD301->run();

        mTcu->setInput((*mLD301)[0].output); //Run9 TCU setup
      }else if(mYear > 2010)
      {
	mTcu->setInput((*mEM201)[0].output); //Run11 and Run12 TCU EM201 part -- zchang
      }
    LOG_DEBUG << Form("TCU: 0x%04x",mTcu->input() & 0xffff) << endm;
  }
}

bool StEmcTriggerSimu::isTrigger(int trigId)
{
  return mTcu->isTrigger(trigId);
}

set<int> StEmcTriggerSimu::triggerIds() const
{
  return mTcu->triggerIds();
}

StTriggerSimuDecision StEmcTriggerSimu::triggerDecision(int trigId)
{
  return isTrigger(trigId) ? kYes : kNo;}

void StEmcTriggerSimu::defineTrigger(TriggerDefinition& trigdef)
{
  // Run11 and Run12 move EM201 output(onbit1 higher 16 bits) to onbits -- zchang
  // LOG_INFO<<Form("use year %d trigger definition", mYear)<<endm;
  if(mYear > 2010)
    {
      trigdef.onbits = trigdef.onbits1;
      trigdef.onbits = trigdef.onbits >> 16;
    }
  mTcu->defineTrigger(trigdef);
}

void StEmcTriggerSimu::defineTrigger(int triggerIndex, const char* name, int triggerId, unsigned int onbits, unsigned int offbits, unsigned int onbits1, unsigned int onbits2, unsigned int onbits3, unsigned int offbits1, unsigned int offbits2, unsigned int offbits3)
{
  TriggerDefinition triggerDefinition;
  triggerDefinition.triggerIndex = triggerIndex;
  strcpy(triggerDefinition.name,name);
  triggerDefinition.triggerId = triggerId;
  triggerDefinition.onbits = onbits;
  triggerDefinition.offbits = offbits;
  triggerDefinition.onbits1 = onbits1;
  triggerDefinition.onbits2 = onbits2;
  triggerDefinition.onbits3 = onbits3;
  triggerDefinition.offbits1 = offbits1;
  triggerDefinition.offbits2 = offbits2;
  triggerDefinition.offbits3 = offbits3;
  LOG_INFO <<"New Defined Trigger: "
           << Form("triggerIndex=%d name=%s triggerId=%d onbits=0x%04x offbits=0x%04x onbit1=0x%04x onbits2=0x%04x onbits3=0x%04x offbits1=0x%04x offbits2=0x%04x offbits3=0x%04x\n", triggerDefinition.triggerIndex,triggerDefinition.name,triggerDefinition.triggerId,triggerDefinition.onbits, triggerDefinition.offbits, triggerDefinition.onbits1, triggerDefinition.onbits2, triggerDefinition.onbits3, triggerDefinition.offbits1, triggerDefinition.offbits2, triggerDefinition.offbits3) << endm;

  defineTrigger(triggerDefinition);
}

int StEmcTriggerSimu::EM201output() const { return (*mEM201)[0].output; }

int StEmcTriggerSimu::overlapJetPatchTh(int i) const { return mEM201->getRegister(i); }

void StEmcTriggerSimu::getOverlapJetPatchAdc(int i, int& jp, int& adc) const
{
  int jp_partial = (*mEM201)[0].channels[6+i] >> 12 & 0x3;
  jp  = (1-i)*3+jp_partial-1;
  adc = (*mEM201)[0].info[i];
}
