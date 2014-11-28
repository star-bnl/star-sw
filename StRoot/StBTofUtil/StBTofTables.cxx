/*******************************************************************
 *
 * $Id: StBTofTables.cxx,v 1.2 2012/12/14 06:35:41 geurts Exp $
 *
 *****************************************************************
 *
 * $Log: StBTofTables.cxx,v $
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

#include "StDetectorDbMaker/St_tofTrayConfigC.h"
#include "StDetectorDbMaker/St_tofStatusC.h"

ClassImp(StBTofTables)

//____________________________________________________________________
void StBTofTables::Reset() {
#if 0
  memset(mBTofTrayConfig, 0, sizeof(mBTofTrayConfig));
  memset(mBTofStatus, 0, sizeof(mBTofStatus));
#endif
}

//____________________________________________________________________                            
void StBTofTables::loadTables() {
#if 0
  Int_t nValidTrays = (Int_t)(St_tofTrayConfigC::instance()->entries());
  for (Int_t i=0;i<nValidTrays;i++) {
    int trayId = (Int_t)(St_tofTrayConfigC::instance()->iTray()[i]);
    mBTofTrayConfig[trayId-1] = 1;
  }
  // i = (cellId -1) + mNCell*((moduleId-1) + mNModule*(trayId-1))
  // i = (cell-1) + 6*((module-1) + 32*(tray-1))
  for(int i=0;i<mNChanMax;i++) {
    int trayId = i/(mNModule*mNCell) + 1;
    int moduleId = (i%(mNModule*mNCell))/mNCell + 1;
    int cellId = i%mNCell + 1;
    if(trayId<=0||trayId>mNTray||moduleId<=0||moduleId>mNModule||cellId<=0||cellId>mNCell) continue;
    mBTofStatus[trayId-1][moduleId-1][cellId-1] = (UShort_t)St_tofStatusC::instance()->status()[i];
  }
#endif
}
#if 0
//____________________________________________________________________                            
bool StBTofTables::trayValid(int trayId) const {
  return mBTofTrayConfig[trayId-1];
}
#endif
//____________________________________________________________________                            
Int_t StBTofTables::status(Int_t trayId, Int_t moduleId, Int_t cellId) {
  static const Int_t mNModule = 32;
  static const Int_t mNCell = 6;
  Int_t i = (cellId -1) + mNCell*((moduleId-1) + mNModule*(trayId-1));
#if 0
  return mBTofStatus[trayId-1][moduleId-1][cellId-1];
#else
  return (Int_t) St_tofStatusC::instance()->status()[i];
#endif
}
