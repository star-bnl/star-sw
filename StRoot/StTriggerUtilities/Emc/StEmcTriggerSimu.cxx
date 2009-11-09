//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 12 Jan 2009
//

// MySQL C API
#include "/usr/include/mysql/mysql.h"

#if 0
// ROOT MySQL
#include "TMySQLServer.h"
#include "TMySQLResult.h"
#include "TMySQLRow.h"
#endif

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
  // Get run number from chain
  StMaker* chain = StMaker::GetChain();
  if (!chain) {
    LOG_WARN << "Can't get chain" << endm;
    return kStWarn;
  }

  int runNumber = chain->GetRunNumber();

#if 0
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
#endif

#if 0
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
    LOG_INFO << setw(20) << "idx_trigger"
             << setw(20) << "name"
             << setw(20) << "offlineBit"
             << setw(20) << "physicsBits"
             << endm;

    while (TMySQLRow* row = (TMySQLRow*)result->Next()) {
      int idx_trigger = atoi(row->GetField(0));
      triggers[idx_trigger].physicsBits = atoi(row->GetField(1));
      mTcu->defineTrigger(triggers[idx_trigger]);

      LOG_INFO << setw(20) << idx_trigger
               << setw(20) << triggers[idx_trigger].name
               << setw(20) << triggers[idx_trigger].triggerId
               << setw(20) << Form("0x%04x", triggers[idx_trigger].physicsBits)
               << endm;

      delete row;
    }
    delete result;
  }
#endif

  // Open connection to Run 9 database

  MYSQL mysql;
  const char* host = "dbbak.starp.bnl.gov";
  const char* user = "";
  const char* pass = "";
  unsigned int port = 3408;
  const char* database = "Conditions_rts";
  const char* unix_socket = NULL;
  unsigned long client_flag = 0;

  mysql_init(&mysql);

  if (!mysql_real_connect(&mysql,host,user,pass,database,port,unix_socket,client_flag)) {
    LOG_WARN << "Can't connect to database: " << mysql_error(&mysql) << endm;
    return kStWarn;
  }

  // Trigger definitions

  TriggerDefinition triggers[32];

  TString query = Form("select idx_trigger,name,offlineBit from triggers where idx_rn = %d", runNumber);
  LOG_INFO << query << endm;
  mysql_query(&mysql,query);

  if (MYSQL_RES* result = mysql_store_result(&mysql)) {
    while (MYSQL_ROW row = mysql_fetch_row(result)) {
      int idx_trigger = atoi(row[0]);
      triggers[idx_trigger].idx_trigger = idx_trigger;
      triggers[idx_trigger].name = row[1];
      triggers[idx_trigger].triggerId = atoi(row[2]);
    }
    mysql_free_result(result);
  }

  TString query2 = Form("select idx_idx,onbits from pwc where idx_rn = %d", runNumber);
  LOG_INFO << query2 << endm;
  mysql_query(&mysql,query2);

  if (MYSQL_RES* result = mysql_store_result(&mysql)) {
    LOG_INFO << setw(20) << "idx_trigger"
             << setw(20) << "name"
             << setw(20) << "offlineBit"
             << setw(20) << "physicsBits"
             << endm;

    while (MYSQL_ROW row = mysql_fetch_row(result)) {
      int idx_trigger = atoi(row[0]);
      triggers[idx_trigger].physicsBits = atoi(row[1]);
      mTcu->defineTrigger(triggers[idx_trigger]);
      LOG_INFO << setw(20) << idx_trigger
               << setw(20) << triggers[idx_trigger].name
               << setw(20) << triggers[idx_trigger].triggerId
               << setw(20) << Form("0x%04x", triggers[idx_trigger].physicsBits)
               << endm;
    }
    mysql_free_result(result);
  }

  mysql_close(&mysql);

  return kStOk;
}
