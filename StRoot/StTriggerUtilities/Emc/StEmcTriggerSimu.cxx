//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 12 Jan 2009
//

// ROOT MySQL
#include "TMySQLServer.h"
#include "TMySQLResult.h"
#include "TMySQLRow.h"

// STAR
#include "St_db_Maker/St_db_Maker.h"

// Local
#include "StTriggerUtilities/Bemc/StBemcTriggerSimu.h"
#include "StTriggerUtilities/Eemc/StEemcTriggerSimu.h"
#include "StTriggerUtilities/StDSMUtilities/StDSM2009Utilities.hh"
#include "StEmcTriggerSimu.h"

ClassImp(StEmcTriggerSimu);

StEmcTriggerSimu::StEmcTriggerSimu()
  : mYear(0)
  , mBemc(0)
  , mEemc(0)
  , mEM201(new DSMLayer_EM201_2009)
  , mLD301(new DSMLayer_LD301_2009)
{
}

StEmcTriggerSimu::~StEmcTriggerSimu()
{
  delete mEM201; mEM201 = 0;
  delete mLD301; mLD301 = 0;
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
  mTriggers.clear();
  get2009_DSMRegisters(runNumber);
}

void StEmcTriggerSimu::Make()
{
  switch (mYear) {
  case 2009:
    if (mBemc) mBemc->get2009_DSMLayer1_Result()->write(*mEM201);
    if (mEemc) mEemc->get2009_DSMLayer1_Result()->write(*mEM201);
    break;
  default:
    return;
  }

  mEM201->run();
  mEM201->write(*mLD301);
  mLD301->run();

  LOG_DEBUG << setw(20) << "idx_trigger"
	    << setw(20) << "name"
	    << setw(20) << "triggerId"
	    << setw(20) << "physicsBits"
	    << setw(20) << "isPhysicsBits"
	    << endm;

  for (map<int, TriggerDefinition>::const_iterator i = mTriggers.begin(); i != mTriggers.end(); ++i) {
    LOG_DEBUG << setw(20) << i->second.idx_trigger
	      << setw(20) << i->second.name
	      << setw(20) << i->second.triggerId
	      << setw(20) << Form("0x%04x", i->second.physicsBits)
	      << setw(20) << isPhysicsBits(i->second.physicsBits)
	      << endm;
  }
}

bool StEmcTriggerSimu::isPhysicsBits(int physicsBits) const
{
  return ((*mLD301)[0].output & physicsBits) == physicsBits;
}

bool StEmcTriggerSimu::isTrigger(int trigId)
{
  typedef map<int, TriggerDefinition>::const_iterator MI;
  pair<MI, MI> p = mTriggers.equal_range(trigId);
  for (MI i = p.first; i != p.second; ++i)
    if (isPhysicsBits(i->second.physicsBits)) return true;
  return false;
}

StTriggerSimuDecision StEmcTriggerSimu::triggerDecision(int trigId)
{
  return isTrigger(trigId) ? kYes : kNo;
}

int StEmcTriggerSimu::get2009_DSMRegisters(int runNumber)
{
  // Open connection to Run 9 database

  LOG_INFO << "Open connection to Run 9 database" << endm;

  TString database = "mysql://dbbak.starp.bnl.gov:3408/Conditions_rts";
  TString user = "";
  TString pass = "";

  LOG_INFO << "database:\t" << database << endm;
  LOG_INFO << "user:\t" << user << endm;
  LOG_INFO << "pass:\t" << pass << endm;
  
  TMySQLServer* mysql = (TMySQLServer*)TMySQLServer::Connect(database, user, pass);

  if (!mysql) {
    LOG_WARN << "Could not connect to Run 9 database" << endm;
    return kStWarn;
  }

  // Trigger definitions

  LOG_INFO << "Get triggers for run " << runNumber << endm;

  TriggerDefinition triggers[32];

  TString query = Form("select idx_trigger,name,offlineBit from triggers where idx_rn = %d", runNumber);

  LOG_INFO << query << endm;

  if (TMySQLResult* result = (TMySQLResult*)mysql->Query(query)) {
    while (TMySQLRow* row = (TMySQLRow*)result->Next()) {
      int idx_trigger = atoi(row->GetField(0));
      triggers[idx_trigger].idx_trigger = idx_trigger;
      triggers[idx_trigger].name = row->GetField(1);
      triggers[idx_trigger].triggerId = atoi(row->GetField(2));
      delete row;
    }
    delete result;
  }

  TString query2 = Form("select idx_idx,onbits from pwc where idx_rn = %d", runNumber);

  LOG_INFO << query2 << endm;

  if (TMySQLResult* result = (TMySQLResult*)mysql->Query(query2)) {
    while (TMySQLRow* row = (TMySQLRow*)result->Next()) {
      int idx_trigger = atoi(row->GetField(0));
      triggers[idx_trigger].physicsBits = atoi(row->GetField(1));
      mTriggers.insert(make_pair(triggers[idx_trigger].triggerId, triggers[idx_trigger]));
      delete row;
    }
    delete result;
  }

  LOG_INFO << setw(20) << "idx_trigger"
	   << setw(20) << "name"
	   << setw(20) << "offlineBit"
	   << setw(20) << "physicsBits"
	   << endm;

  for (multimap<int, TriggerDefinition>::const_iterator i = mTriggers.begin(); i != mTriggers.end(); ++i) {
    LOG_INFO << setw(20) << i->second.idx_trigger
	     << setw(20) << i->second.name
	     << setw(20) << i->second.triggerId
	     << setw(20) << Form("0x%04x", i->second.physicsBits)
	     << endm;
  }

  // EM201

  LOG_INFO << "Get DSM registers for EMC layer 2" << endm;

  TString query7 = Form("select reg,label,value,defaultvalue from dict where object = 1 and idx = 20 and hash = (select dicthash from run where idx_rn = %d)", runNumber);

  LOG_INFO << query7 << endm;

  if (TMySQLResult* result = (TMySQLResult*)mysql->Query(query7)) {
    LOG_INFO << setw(25) << "reg"
	     << setw(25) << "label"
	     << setw(25) << "value"
	     << setw(25) << "defaultvalue"
	     << endm;
    while (TMySQLRow* row = (TMySQLRow*)result->Next()) {
      int reg = atoi(row->GetField(0));
      TString label = row->GetField(1);
      int value = atoi(row->GetField(2));
      int defaultvalue = atoi(row->GetField(3));
      mEM201->setRegister(reg, (value == -1) ? defaultvalue : value);
      LOG_INFO << setw(25) << reg
	       << setw(25) << label
	       << setw(25) << value
	       << setw(25) << defaultvalue
	       << endm;
      delete row;
    }
    delete result;
  }

  // LD301

  LOG_INFO << "Get last DSM registers" << endm;

  TString query8 = Form("select reg,label,value,defaultvalue from dict where object = 1 and idx = 30 and hash = (select dicthash from run where idx_rn = %d)", runNumber);

  LOG_INFO << query8 << endm;

  if (TMySQLResult* result = (TMySQLResult*)mysql->Query(query8)) {
    LOG_INFO << setw(25) << "reg"
             << setw(25) << "label"
             << setw(25) << "value"
	     << setw(25) << "defaultvalue"
             << endm;
    while (TMySQLRow* row = (TMySQLRow*)result->Next()) {
      int reg = atoi(row->GetField(0));
      TString label = row->GetField(1);
      int value = atoi(row->GetField(2));
      int defaultvalue = atoi(row->GetField(3));
      mLD301->setRegister(reg, (value == -1) ? defaultvalue : value);
      LOG_INFO << setw(25) << reg
               << setw(25) << label
               << setw(25) << value
               << setw(25) << defaultvalue
               << endm;
      delete row;
    }
    delete result;
  }

  // Close connection to Run 9 database

  LOG_INFO << "Close connection to Run 9 database" << endm;

  mysql->Close();

  return kStOk;
}
