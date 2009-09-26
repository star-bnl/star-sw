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

  get2009_DSMRegisters(runNumber);
  defineTriggers();
}

void StEmcTriggerSimu::Make()
{
  switch (mDBTime.GetYear()) {
  case 2009:
    if (mBemc) mBemc->get2009_DSMLayer1_Result()->write(*mEM201);
    if (mEemc) mEemc->get2009_DSMLayer1_Result()->write(*mEM201);
    mEM201->run();
    mEM201->write(*mLD301);
    mLD301->run();
    mTcu->setInput((*mLD301)[0].output);
    break;
  default:
    return;
  }
}

bool StEmcTriggerSimu::isTrigger(int trigId)
{
  return mTcu->isTrigger(trigId);
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

  return kStOk;
}

int StEmcTriggerSimu::defineTriggers()
{
  // Get chain
  StMaker* chain = StMaker::GetChain();
  if (!chain) {
    LOG_WARN << "Can't get chain" << endm;
    return kStWarn;
  }

  // Retrieve triggerInfo table from offline DB
  TDataSet* db = chain->GetDataBase("RunLog/onl/triggerInfo");

  if (!db) {
    LOG_WARN << "Can't get offline DB table RunLog/onl/triggerInfo" << endm;
    return kStWarn;
  }

  // Fetch ROOT descriptor of DB table
  St_triggerInfo* des = (St_triggerInfo*)db->Find("triggerInfo");

  if (!des) {
    LOG_WARN << "Can't get DB table descriptor for triggerInfo" << endm;
    return kStWarn;
  }

  triggerInfo_st* table = des->GetTable();
  int nrows = des->GetNRows();

  LOG_INFO << "Found " << nrows << " rows in table triggerInfo for run " << chain->GetRunNumber() << endm;

  // Loop over rows
  LOG_INFO << setw(20) << "idxTrg"
	   << setw(30) << "name"
	   << setw(20) << "oflineTrgId"
	   << setw(20) << "detectorLiveOnBits"
	   << endm;

  mTcu->clear();

  for (int i = 0; i < nrows; ++i) {
    TriggerDefinition trgDef;

    trgDef.idx_trigger = table[i].idxTrg;
    trgDef.name        = table[i].name;
    trgDef.triggerId   = table[i].offlineTrgId;
    trgDef.physicsBits = table[i].detectorLiveOnBits;

    LOG_INFO << setw(20) << trgDef.idx_trigger
	     << setw(30) << trgDef.name
	     << setw(20) << trgDef.triggerId
	     << setw(20) << trgDef.physicsBits
	     << endm;

    mTcu->defineTrigger(trgDef);
  } // End loop over rows

  return kStOk;
}
