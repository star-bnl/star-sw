/*******************************************************************
 *
 * $Id: StBTofTables.cxx,v 1.4 2017/10/20 17:50:33 smirnovd Exp $
 *
 *****************************************************************
 *
 * $Log: StBTofTables.cxx,v $
 * Revision 1.4  2017/10/20 17:50:33  smirnovd
 * Squashed commit of the following:
 *
 *     StBTof: Remove outdated ClassImp macro
 *
 *     Prefer explicit namespace for std:: names in header files
 *
 *     Removed unnecessary specification of default std::allocator
 *
 * Frank signed-off
 *
 * Revision 1.3  2017/10/03 22:54:45  geurts
 * restore access to TOF status table (Calibrations/tof/tofStatus)
 *
 * Revision 1.2  2012/12/14 06:35:41  geurts
 * Changed global database calls to direct table access and/or removed deprecated database access code.
 *
 * Revision 1.1  2009/08/28 17:22:11  dongx
 * first release
 *
 *
 *******************************************************************/
#include "StBTofTables.h"
#include "StMaker.h"


//____________________________________________________________________
StBTofTables::StBTofTables() {
  Reset();
}

//____________________________________________________________________                            
StBTofTables::~StBTofTables() {
  Reset();
}

//____________________________________________________________________                            
void StBTofTables::Reset() {
  memset(mBTofTrayConfig, 0, sizeof(mBTofTrayConfig));
  memset(mBTofStatus, 0, sizeof(mBTofStatus));
}

//____________________________________________________________________                            
void StBTofTables::loadTables(StMaker* maker) {

  LOG_INFO << "StBTofTables -- loading the BTOF tray/channel status tables ..." << endm;

  TDataSet *mDbTOFDataSet = maker->GetDataBase("Calibrations/tof/tofTrayConfig");
  if(!mDbTOFDataSet) {
    LOG_ERROR << "unable to access Calibrations TOF parameters for config" << endm;
    //    assert(mDbTOFDataSet);
    return; // kStErr;
  }

  St_tofTrayConfig* trayConfig = static_cast<St_tofTrayConfig*>(mDbTOFDataSet->Find("tofTrayConfig"));
  if(!trayConfig) {
    LOG_ERROR << "unable to get tof tray configuration" << endm;
    return; // kStErr;
  }
  tofTrayConfig_st* trayconf = static_cast<tofTrayConfig_st*>(trayConfig->GetArray());
  if(maker->Debug()) { LOG_INFO << " Valid Trays: " << endm; }

  Int_t nValidTrays = (Int_t)(trayconf[0].entries);
  for (Int_t i=0;i<nValidTrays;i++) {
    int trayId = (Int_t)(trayconf[0].iTray[i]);
    mBTofTrayConfig[trayId-1] = 1;
    if(maker->Debug()) {
      LOG_INFO << " " << trayId;
    }
  }
  if(maker->Debug()) { LOG_INFO << endm; }

  mDbTOFDataSet = maker->GetDataBase("Calibrations/tof/tofStatus");
  if(!mDbTOFDataSet) {
    LOG_ERROR << "unable to access Calibrations TOF parameters for status" << endm;
    //    assert(mDbTOFDataSet);
    return; // kStErr;
  }
  St_tofStatus* tofStatus = static_cast<St_tofStatus*>(mDbTOFDataSet->Find("tofStatus"));
  if(!tofStatus) {
    LOG_ERROR << "unable to get tof status table" << endm;
    return; // kStErr;
  }

  tofStatus_st* status = static_cast<tofStatus_st*>(tofStatus->GetArray());
  for(int i=0;i<mNChanMax;i++) {
    int trayId = i/(mNModule*mNCell) + 1;
    int moduleId = (i%(mNModule*mNCell))/mNCell + 1;
    int cellId = i%mNCell + 1;
    if(trayId<=0||trayId>mNTray||moduleId<=0||moduleId>mNModule||cellId<=0||cellId>mNCell) continue;
    mBTofStatus[trayId-1][moduleId-1][cellId-1] = (UShort_t)status[0].status[i];
  }

  return;
}

//____________________________________________________________________                            
bool StBTofTables::trayValid(int trayId) const {
  return mBTofTrayConfig[trayId-1];
}

//____________________________________________________________________                            
int StBTofTables::status(int trayId, int moduleId, int cellId) const {
  return mBTofStatus[trayId-1][moduleId-1][cellId-1];
}
