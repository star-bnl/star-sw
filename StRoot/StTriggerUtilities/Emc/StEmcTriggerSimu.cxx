//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 12 Jan 2009
//

// STAR
#include "St_db_Maker/St_db_Maker.h"
#include "tables/St_trgDsmReg_Table.h"
#include "tables/St_triggerInfo_Table.h"

// Local
#include "StTriggerUtilities/Bemc/StBemcTriggerSimu.h"
#include "StTriggerUtilities/Eemc/StEemcTriggerSimu.h"
#include "StTriggerUtilities/StDSMUtilities/StDSM2009Utilities.hh"
#include "StEmcTriggerSimu.h"

ClassImp(StEmcTriggerSimu);

StEmcTriggerSimu::StEmcTriggerSimu()
  : mBemc(0)
  , mEemc(0)
  , mEM201(new DSMLayer_EM201_2009)
  , mLD301(new DSMLayer_LD301_2009)
  , mTcu(new TCU)
{
  fill(mOverlapJetPatchTh,mOverlapJetPatchTh+3,-1);
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

  //get2009_DSMRegisters(runNumber);
  //defineTriggers(runNumber);
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
    mEM201->write(*mLD301);

    LOG_DEBUG << Form("EM201 OUTPUT: BHT=%d EHT=%d JP1=%d JP2=%d BJP1=%d BJP2=%d EJP1=%d EJP2=%d AJP=%d BAJP=%d EAJP=%d",
                      BHT(),EHT(),JP1(),JP2(),BJP1(),BJP2(),EJP1(),EJP2(),AJP(),BAJP(),EAJP()) << endm;

    TString LD301String = "LD301: ";
    for (int ch = 0; ch < 8; ++ch) LD301String += Form("%04x ",(*mLD301)[0].channels[ch]);
    LOG_DEBUG << LD301String << endm;

    mLD301->run();
    mTcu->setInput((*mLD301)[0].output);

    LOG_DEBUG << Form("TCU: %04x",mTcu->input() & 0xffff) << endm;
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
  return isTrigger(trigId) ? kYes : kNo;
}

int StEmcTriggerSimu::get2009_DSMRegisters(int runNumber)
{
  // Get chain
  StMaker* chain = StMaker::GetChain();
  if (!chain) {
    LOG_WARN << "Can't get chain" << endm;
    return kStWarn;
  }

  // Retrieve DSM threshold table from offline DB
  TDataSet* db = chain->GetDataBase("RunLog/onl/trgDsmReg");

  if (!db) {
    LOG_WARN << "Can't get DB table RunLog/onl/trgDsmReg" << endm;
    return kStWarn;
  }

  // Fetch ROOT descriptor of DB table
  St_trgDsmReg* des = (St_trgDsmReg*)db->Find("trgDsmReg");

  if (!des) {
    LOG_WARN << "Can't get DB table descriptor trgDsmReg" << endm;
    return kStWarn;
  }

  trgDsmReg_st* table = des->GetTable();
  int nrows = des->GetNRows();

  LOG_INFO << "Found " << nrows << " rows in table trgDsmReg for run " << chain->GetRunNumber() << endm;

  // Loop over rows and set DSM thresholds in registers

  LOG_INFO << setw(20) << "register"
           << setw(30) << "label"
           << setw(20) << "value"
           << endm;

  for (int i = 0; i < nrows; ++i) {
    int object = table[i].dcObject;
    int index  = table[i].dcIndex;
    int reg    = table[i].dcRegister;
    TString label = table[i].dcLabel;
    int value  = table[i].dcValue != -1 ? table[i].dcValue : table[i].dcDefaultvalue;

    // EM201
    if (object == 1 && index == 20) {
      LOG_INFO << setw(20) << reg
               << setw(30) << label
               << setw(20) << value
               << endm;

      mEM201->setRegister(reg, value);
    }

    // LD301
    if (object == 1 && index == 30) {
      LOG_INFO << setw(20) << reg
               << setw(30) << label
               << setw(20) << value
               << endm;

      mLD301->setRegister(reg, value);
    }
  } // End loop over rows

  // Overwrite thresholds from the database if set explicitly
  LOG_INFO << "The following registers have new values:" << endm;

  for (int reg = 0; reg < 3; ++reg) {
    int value = mOverlapJetPatchTh[reg];
    if (value != -1) {
      LOG_INFO << setw(20) << reg
	       << setw(30) << "BEMC-EEMC-overlap-JP-th" << reg
	       << setw(20) << value
	       << endm;
      mEM201->setRegister(reg,value);
    }
  }

  return kStOk;
}

void StEmcTriggerSimu::defineTrigger(const TriggerDefinition& trigdef)
{
  mTcu->defineTrigger(trigdef);
}

int StEmcTriggerSimu::EM201output() const { return (*mEM201)[0].output; }

int StEmcTriggerSimu::overlapJetPatchTh(int i) const { return mEM201->getRegister(i); }

void StEmcTriggerSimu::getOverlapJetPatchAdc(int i, int& jp, int& adc) const
{
  int jp_partial = (*mEM201)[0].channels[6+i] >> 12 & 0x3;
  jp  = (1-i)*3+jp_partial-1;
  adc = (*mEM201)[0].info[i];
}
