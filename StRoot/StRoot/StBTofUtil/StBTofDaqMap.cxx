/*******************************************************************
 *
 * $Id: StBTofDaqMap.cxx,v 1.5 2012/12/14 06:35:41 geurts Exp $
 *
 * Author: Xin Dong
 *****************************************************************
 *
 * Description: (1) Mapping between Daq channel numbers and Cell numbers
 *              (2) Parameters initalize from dbase
 *
 *****************************************************************
 *
 * $Log: StBTofDaqMap.cxx,v $
 * Revision 1.5  2012/12/14 06:35:41  geurts
 * Changed global database calls to direct table access and/or removed deprecated database access code.
 *
 * Revision 1.4  2010/05/25 22:09:44  geurts
 * improved database handling and reduced log output
 *
 * Revision 1.3  2009/03/17 18:32:26  fine
 * make the print outs usefull
 *
 * Revision 1.2  2009/02/13 19:47:32  dongx
 * mNValidTrays set by the tofTrayConfig in db now
 *
 * Revision 1.1  2009/02/02 21:56:34  dongx
 * first release - Barrel TOF daq mapping
 *
 *
 *******************************************************************/
#include <iostream>
#include "tables/St_tofDaqMap_Table.h"
#include "tables/St_tofTrayConfig_Table.h"
#include "StMessMgr.h"
#include "StMaker.h"
#include "StBTofDaqMap.h"

StBTofDaqMap::StBTofDaqMap()
{ 
  Reset();
}

StBTofDaqMap::~StBTofDaqMap()
{ 
  Reset();
}

/*!
 * Main initial function to access data base to retrieve the daq map parameters
 */
void StBTofDaqMap::Init(StMaker *maker) {

  LOG_INFO << "[StBTofDaqMap] retrieving BTOF DAQ map and tray config ..." << endm;
  ///////////////////////////////////////////////////////
  // Load configuration parameters from dbase
  //    need "[shell] setenv Calibrations_tof reconV0"
  ///////////////////////////////////////////////////////

  TDataSet *mDbTOFDataSet = maker->GetDataBase("Calibrations/tof/tofDaqMap");
  St_tofDaqMap* tofDaqMap = static_cast<St_tofDaqMap*>(mDbTOFDataSet->Find("tofDaqMap"));
  if(!tofDaqMap) {
    LOG_ERROR << "unable to get tof Module map table" << endm;
    return; // kStErr;
  }
  tofDaqMap_st* daqmap = static_cast<tofDaqMap_st*>(tofDaqMap->GetArray());
  for (Int_t i=0;i<mNTOF;i++) {
    mMRPC2TDIGChan[i] = (Int_t)(daqmap[0].MRPC2TDIGChanMap[i]);
    mTDIG2MRPCChan[mMRPC2TDIGChan[i]] = i;
    if(maker->Debug()) {
      LOG_DEBUG << " MRPC = " << i << "  TDC chan = " << mMRPC2TDIGChan[i] << endm;
    }
  }
  for (Int_t i=0;i<mNVPD;i++) {
    mWestPMT2TDIGLeChan[i] = (Int_t)(daqmap[0].PMT2TDIGLeChanMap[i]);
    mWestPMT2TDIGTeChan[i] = (Int_t)(daqmap[0].PMT2TDIGTeChanMap[i]);
    if(maker->Debug()) {
      LOG_DEBUG << " VPD = " << i << "  TDC Lechan = " << mWestPMT2TDIGLeChan[i] << "  TDC TeChan = " << mWestPMT2TDIGTeChan[i] << endm;
    }
    mTDIGLe2WestPMTChan[mWestPMT2TDIGLeChan[i]] = i;
    mTDIGTe2WestPMTChan[mWestPMT2TDIGTeChan[i]] = i;

    int j=i+mNVPD;

    mEastPMT2TDIGLeChan[i] = (Int_t)(daqmap[0].PMT2TDIGLeChanMap[j]);
    mEastPMT2TDIGTeChan[i] = (Int_t)(daqmap[0].PMT2TDIGTeChanMap[j]);
    if(maker->Debug()) {
      LOG_DEBUG << " VPD = " << i << "  TDC Lechan = " << mEastPMT2TDIGLeChan[i] << "  TDC TeChan = " << mEastPMT2TDIGTeChan[i] << endm;
    }
    mTDIGLe2EastPMTChan[mEastPMT2TDIGLeChan[i]] = i;
    mTDIGTe2EastPMTChan[mEastPMT2TDIGTeChan[i]] = i;
  }

  // valid tray Id
  mDbTOFDataSet = maker->GetDataBase("Calibrations/tof/tofTrayConfig");
  St_tofTrayConfig* trayConfig = static_cast<St_tofTrayConfig*>(mDbTOFDataSet->Find("tofTrayConfig"));
  if(!trayConfig) {
    LOG_ERROR << "unable to get tof tray configuration" << endm;
    return; // kStErr;
  }
  tofTrayConfig_st* trayconf = static_cast<tofTrayConfig_st*>(trayConfig->GetArray());
  if(maker->Debug()) { LOG_DEBUG << " Valid Trays: " << endm; }
  mNValidTrays = (Int_t)(trayconf[0].entries);
  for (Int_t i=0;i<mNValidTrays;i++) {
    mValidTrayId[i] = (Int_t)(trayconf[0].iTray[i]);
    if(maker->Debug()) {
      LOG_DEBUG << " " << mValidTrayId[i];
    }
  }
  if(maker->Debug()) { LOG_DEBUG << endm; }

  LOG_DEBUG << "[StBTofDaqMap] ... done." << endm;
  return;
}


void StBTofDaqMap::Reset() {
  for(int i=0;i<mNTOF;i++) {
    mMRPC2TDIGChan[i] = -1;
    mTDIG2MRPCChan[i] = -1;
    mTDIGLe2WestPMTChan[i] = -1;
    mTDIGTe2WestPMTChan[i] = -1;
    mTDIGLe2EastPMTChan[i] = -1;
    mTDIGTe2EastPMTChan[i] = -1;
  }
  for(int i=0;i<mNVPD;i++) {
    mEastPMT2TDIGLeChan[i] = -1;
    mEastPMT2TDIGTeChan[i] = -1;
    mWestPMT2TDIGLeChan[i] = -1;
    mWestPMT2TDIGTeChan[i] = -1;
  }

  mNValidTrays = 0;
}

// tof8++
IntVec StBTofDaqMap::TDIGChan2Cell( const Int_t iTdc)
{
  IntVec map;
  map.clear();

  if ( iTdc<0 || iTdc>=mNTOF ) {
    LOG_ERROR << "[TDIGChan2Cell] Uncorrected TDC Channel number for Tof! " << endm;
    return map;
  }

  Int_t ModuleChan = mTDIG2MRPCChan[iTdc];
  Int_t Module = ModuleChan / mNCell + 1;
  Int_t Cell   = ModuleChan % mNCell + 1;
  map.push_back(Module);
  map.push_back(Cell);

  return map;
}

Int_t StBTofDaqMap::Cell2TDIGChan( const Int_t iModule, const Int_t iCell )
{

  if(iModule<1 || iModule>mNModule ) {
    LOG_ERROR<<"[Cell2TDIGChan] Wrong module number !"<<endm;
    return -1;
  }
  if(iCell <1 || iCell > mNCell) {
    LOG_ERROR<<"[Cell2TDIGChan] Wrong cell number ! "<<endm; 
    return -1;
  }

  Int_t modulechan = (iModule-1)*mNCell+(iCell-1);

//  if (modulechan<1 || modulechan>=mNTOF) {
  if (modulechan<0 || modulechan>=mNTOF) {
    LOG_ERROR<<"[Cell2TDIGChan] Wrong Module-Cell channel number!"<<endm;
    return -1;
  }

  return mMRPC2TDIGChan[modulechan];
}

Int_t StBTofDaqMap::WestPMT2TDIGLeChan( const Int_t iTube )
{
  if ( iTube<1 || iTube>mNVPD ) {
    LOG_ERROR<<"[WestPMT2TDIGLeChan] Wrong vpd tube number ! "<<endm; 
    return -1;
  }

  return mWestPMT2TDIGLeChan[iTube-1];
}

Int_t StBTofDaqMap::WestPMT2TDIGTeChan( const Int_t iTube )
{
  if ( iTube<1 || iTube>mNVPD ) {
    LOG_ERROR<<"[WestPMT2TDIGTeChan]  Wrong vpd tube number ! "<< iTube<<endm; 
    return -1;
  }

  return mWestPMT2TDIGTeChan[iTube-1];
}

Int_t StBTofDaqMap::TDIGLeChan2WestPMT( const Int_t iTdc )
{
  if ( iTdc<0 || iTdc>=mNTOF ) {
    LOG_ERROR<<"[TDIGLeChan2WestPMT] Wrong tdc channel number ! "<< iTdc<<endm; 
    return -1;
  }

  return mTDIGLe2WestPMTChan[iTdc] + 1;
}

Int_t StBTofDaqMap::TDIGTeChan2WestPMT( const Int_t iTdc )
{
  if ( iTdc<0 || iTdc>=mNTOF ) {
    LOG_ERROR<<"[TDIGTeChan2WestPMT] Wrong tdc channel number ! "<< iTdc <<endm; 
    return -1;
  }

  return mTDIGTe2WestPMTChan[iTdc] + 1;
}

Int_t StBTofDaqMap::EastPMT2TDIGLeChan( const Int_t iTube )
{
  if ( iTube<1 || iTube>mNVPD ) {
    LOG_ERROR<<"[EastPMT2TDIGLeChan] Wrong vpd tube number ! "<<iTube<< endm; 
    return -1;
  }

  return mEastPMT2TDIGLeChan[iTube-1];
}

Int_t StBTofDaqMap::EastPMT2TDIGTeChan( const Int_t iTube )
{
  if ( iTube<1 || iTube>mNVPD ) {
    LOG_ERROR<<"[EastPMT2TDIGTeChan] Wrong vpd tube number ! "<<iTube<< endm; 
    return -1;
  }

  return mEastPMT2TDIGTeChan[iTube-1];
}

Int_t StBTofDaqMap::TDIGLeChan2EastPMT( const Int_t iTdc )
{
  if ( iTdc<0 || iTdc>=mNTOF ) {
    LOG_ERROR<<"StBTofDaqMap::TDIGLeChan2EastPMT: Wrong tdc channel number ! "<<iTdc<<endm; 
    return -1;
  }

  return mTDIGLe2EastPMTChan[iTdc] + 1;
}

Int_t StBTofDaqMap::TDIGTeChan2EastPMT( const Int_t iTdc )
{
  if ( iTdc<0 || iTdc>=mNTOF ) {
    LOG_ERROR<<"StBTofDaqMap::TDIGTeChan2EastPMT: Wrong tdc channel number ! "<<iTdc << endm; 
    return -1;
  }

  return mTDIGTe2EastPMTChan[iTdc] + 1;
}

IntVec StBTofDaqMap::ValidTrays()
{
  IntVec trayId;
  for(int i=0;i<mNValidTrays;i++) {
    trayId.push_back(mValidTrayId[i]);
  }

  return trayId;
}
