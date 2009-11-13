//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 12 Jan 2009
//

// MySQL C API
#include "/usr/include/mysql/mysql.h"

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
  mYear = mDBTime.GetYear();

  get2009_DSMRegisters(runNumber);
  defineTriggers(runNumber);
}

void StEmcTriggerSimu::Make()
{
  switch (mYear) {
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

int StEmcTriggerSimu::defineTriggers(int runNumber)
{
  // Open connection to Run 9 database

  MYSQL mysql;
  const char* host = "dbbak.starp.bnl.gov";
  const char* user = "";
  const char* pass = "";
  unsigned int port = 3408;
  const char* database = "Conditions_rts";
  const char* unix_socket = NULL;
  unsigned long client_flag = 0;

  LOG_INFO << Form("host=%s user=\"%s\" pass=\"%s\" port=%d database=%s",host,user,pass,port,database) << endm;

  mysql_init(&mysql);

  if (!mysql_real_connect(&mysql,host,user,pass,database,port,unix_socket,client_flag)) {
    LOG_WARN << "Can't connect to database: " << mysql_error(&mysql) << endm;
    return kStWarn;
  }

  // For simulation, get run number from DB time stamp

  if (mMCflag) {
    TString query3 = Form("select idx_rn from triggers where beginTime >= '%s' limit 1", mDBTime.AsSQLString());
    LOG_INFO << query3 << endm;
    mysql_query(&mysql,query3);

    if (MYSQL_RES* result = mysql_store_result(&mysql)) {
      while (MYSQL_ROW row = mysql_fetch_row(result)) {
	runNumber = atoi(row[0]);
      }
    }
    LOG_INFO << "DB Time = " << mDBTime.AsSQLString() << endm;
    LOG_INFO << "Run Number = " << runNumber << endm;
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
