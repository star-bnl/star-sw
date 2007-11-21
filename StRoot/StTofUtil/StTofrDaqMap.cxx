/*******************************************************************
 *
 * $Id: StTofrDaqMap.cxx,v 1.10 2007/11/21 19:31:28 dongx Exp $
 *
 * Author: Xin Dong
 *****************************************************************
 *
 * Description: (1) Mapping between Daq channel numbers, ADC(TDC) channel
 *              numbers and TOFr cell numbers
 *              (2) Parameters initalize from dbase
 *
 *****************************************************************
 *
 * $Log: StTofrDaqMap.cxx,v $
 * Revision 1.10  2007/11/21 19:31:28  dongx
 * added ValidTrays() for multi-tray system
 *
 * Revision 1.8  2007/04/17 23:01:52  dongx
 * replaced with standard STAR Loggers
 *
 * Revision 1.7  2007/02/06 20:20:44  dongx
 * update for tofr5Maptable I/O (new release)
 *
 * Revision 1.6  2005/04/12 17:23:15  dongx
 * Update for year 5 new data format, writter by Jing Liu
 *
 * Revision 1.5  2004/05/03 22:58:26  dongx
 * using a switch to set the ouput message
 *
 * Revision 1.4  2004/04/09 15:17:19  jeromel
 * void function returning a value is forbidden by standards
 *
 * Revision 1.3  2004/04/08 21:11:11  dongx
 * remove assert() functions
 *
 * Revision 1.2  2004/03/09 17:43:04  dongx
 * first release
 *
 *
 *******************************************************************/
#include <iostream>
#include "tables/St_tofModuleConfig_Table.h"
#include "tables/St_tofCamacDaqMap_Table.h"
#include "tables/St_tofr5Maptable_Table.h"
#include "tables/St_tofDaqMap_Table.h"
#include "tables/St_tofTrayConfig_Table.h"
#include "StMessMgr.h"
#include "StMaker.h"
#include "StTofrDaqMap.h"

StTofrDaqMap::StTofrDaqMap()
{ 
  Reset();
}

StTofrDaqMap::~StTofrDaqMap()
{ 
  Reset();
}

void StTofrDaqMap::init() {
  Reset();
}

void StTofrDaqMap::init(StMaker *maker) {
  initFromDbase(maker);
}

void StTofrDaqMap::initFromDbase(StMaker *maker) {

  gMessMgr->Info("StTofrDaqMap -- rertieving the daq mapping","OS");
  ///////////////////////////////////////////////////////
  // Load configuration parameters from dbase
  //    need "[shell] setenv Calibrations_tof reconV0"
  ///////////////////////////////////////////////////////

  TDataSet *mDbTOFDataSet = maker->GetDataBase("Calibrations/tof");
  if(!mDbTOFDataSet) {
    gMessMgr->Error("unable to access Calibrations TOF parameters","OS");
    //    assert(mDbTOFDataSet);
    return; // kStErr;
  }

  St_tofModuleConfig* tofModuleConf = static_cast<St_tofModuleConfig*>(mDbTOFDataSet->Find("tofModuleConfig"));
  if(!tofModuleConf) {
    gMessMgr->Error("unable to get tof Module Configuration parameters","OS");
    //    assert(tofModuleConf);
    return; // kStErr;
  }
  tofModuleConfig_st* moduleConf = static_cast<tofModuleConfig_st*>(tofModuleConf->GetArray());
  //  Int_t numRows = tofModuleConf->GetNRows();
  Int_t entries = (Int_t)(moduleConf[0].entries);
  if(entries*mNCell!=mNTOFR) {
    gMessMgr->Warning(" # TOFr channels incosistency in dbase");
  }
  for (Int_t i=0;i<entries;i++) {
    for (Int_t j=0;j<mNCell;j++) {
      Int_t ic = i*6+j;
      Int_t id = (Int_t)(moduleConf[0].iChannel[ic]);
      mTrayId[id] = (Int_t)(moduleConf[0].iTray[i]);
      mModuleId[id] = (Int_t)(moduleConf[0].iModule[i]);
      mCellId[id] = j+1;
      if(maker->Debug()) {
	LOG_INFO << " id=" << id << "  tray=" << mTrayId[id] << " module=" << mModuleId[id] << "  cell=" << mCellId[id] << endm;
      }
    }
  }

  St_tofCamacDaqMap* tofDaqMap = static_cast<St_tofCamacDaqMap*>(mDbTOFDataSet->Find("tofCamacDaqMap"));
  if(!tofDaqMap) {
    gMessMgr->Error("unable to get tof daq map","OS");
    //    assert(tofDaqMap);
    return; // kStErr;
  }
  tofCamacDaqMap_st* tofmap = static_cast<tofCamacDaqMap_st*>(tofDaqMap->GetArray());
  //  Int_t ndaqRows = tofDaqMap->GetNRows();
  Int_t daqentries = (Int_t)(tofmap[0].entries);
  Int_t nChans = 0;
  bool chanMatch = kTRUE;
  for(Int_t i=0;i<daqentries;i++) {
    // not tofr detector or TOTs
    if(tofmap[0].detectorId[i]!=2||tofmap[0].adcChan[i]<0) continue;
    if(nChans>=mNTOFR) {
      break;
      chanMatch = kFALSE;
    }
    Int_t daqChan = (Int_t)(tofmap[0].daqChannel[i]);
    mAdc[daqChan] = (Int_t)(tofmap[0].adcChan[i]);
    mTdc[daqChan] = (Int_t)(tofmap[0].tdcChan[i]);
    nChans++;
  }
  if(!chanMatch || nChans!=mNTOFR) {
    gMessMgr->Warning(" # TOFr daq channels inconsistency in dbase");
  }

}
// for tofr5------------------------------------------------------------------------
void StTofrDaqMap::initFromDbaseY5(StMaker *maker) {

  gMessMgr->Info("StTofrDaqMap -- rertieving the tofr5 channel mapping","OS");
  ///////////////////////////////////////////////////////
  // Load configuration parameters from dbase
  //    need "[shell] setenv Calibrations_tof reconV0"
  ///////////////////////////////////////////////////////

  TDataSet *mDbTOFDataSet = maker->GetDataBase("Calibrations/tof");
  if(!mDbTOFDataSet) {
    gMessMgr->Error("unable to access Calibrations TOF parameters","OS");
    //    assert(mDbTOFDataSet);
    return; // kStErr;
  }

  St_tofr5Maptable* tofr5maptable = static_cast<St_tofr5Maptable*>(mDbTOFDataSet->Find("tofr5Maptable"));
  if(!tofr5maptable) {
    gMessMgr->Error("unable to get tof Module map table","OS");
    return; // kStErr;
  }
  tofr5Maptable_st* maptable = static_cast<tofr5Maptable_st*>(tofr5maptable->GetArray());
  for (Int_t i=0;i<mNTOFR5;i++) {
      mGlobalTDCChan[i]=(Int_t)(maptable[0].tdigchan[i]);
      mGlobalModuleChan[i]=(Int_t)(maptable[0].modulechan[i]);
      if(maker->Debug()) {
      LOG_INFO << " i=" << i << "  TDC chan =" << mGlobalTDCChan[i] << " module chan=" <<mGlobalModuleChan[i]<<endm;
      }
  }


  return;
}

void StTofrDaqMap::initFromDbaseGeneral(StMaker *maker) {

  gMessMgr->Info("StTofrDaqMap -- rertieving the tofr5 channel mapping","OS");
  ///////////////////////////////////////////////////////
  // Load configuration parameters from dbase
  //    need "[shell] setenv Calibrations_tof reconV0"
  ///////////////////////////////////////////////////////

  TDataSet *mDbTOFDataSet = maker->GetDataBase("Calibrations/tof");
  if(!mDbTOFDataSet) {
    gMessMgr->Error("unable to access Calibrations TOF parameters","OS");
    //    assert(mDbTOFDataSet);
    return; // kStErr;
  }

  St_tofDaqMap* tofDaqMap = static_cast<St_tofDaqMap*>(mDbTOFDataSet->Find("tofDaqMap"));
  if(!tofDaqMap) {
    gMessMgr->Error("unable to get tof Module map table","OS");
    return; // kStErr;
  }
  tofDaqMap_st* daqmap = static_cast<tofDaqMap_st*>(tofDaqMap->GetArray());
  for (Int_t i=0;i<mNTOF;i++) {
    mMRPC2TDIGChan[i] = (Int_t)(daqmap[0].MRPC2TDIGChanMap[i]);
    if(maker->Debug()) {
      LOG_INFO << " MRPC = " << i << "  TDC chan = " << mMRPC2TDIGChan[i] << endm;
    }
    mTDIG2MRPCChan[mMRPC2TDIGChan[i]] = i;
  }
  for (Int_t i=0;i<mNVPD;i++) {
    mPMT2TDIGLeChan[i] = (Int_t)(daqmap[0].PMT2TDIGLeChanMap[i]);
    mPMT2TDIGTeChan[i] = (Int_t)(daqmap[0].PMT2TDIGTeChanMap[i]);
    if(maker->Debug()) {
      LOG_INFO << " VPD = " << i << "  TDC Lechan = " << mPMT2TDIGLeChan[i] << "  TDC TeChan = " << mPMT2TDIGTeChan[i] << endm;
    }
    mTDIGLe2PMTChan[mPMT2TDIGLeChan[i]] = i;
    mTDIGTe2PMTChan[mPMT2TDIGTeChan[i]] = i;
  }

  // valid tray Id
  St_tofTrayConfig* trayConfig = static_cast<St_tofTrayConfig*>(mDbTOFDataSet->Find("tofTrayConfig"));
  if(!trayConfig) {
    gMessMgr->Error("unable to get tof tray configuration","OS");
    return; // kStErr;
  }
  tofTrayConfig_st* trayconf = static_cast<tofTrayConfig_st*>(trayConfig->GetArray());
  if(maker->Debug()) LOG_INFO << " Valid Trays: " << endm;
  for (Int_t i=0;i<mNTray;i++) {
    mValidTrayId[i] = (Int_t)(trayconf[0].iTray[i]);
    if(maker->Debug()) {
      LOG_INFO << " " << mValidTrayId[i];
    }
  }
  if(maker->Debug()) LOG_INFO << endm;

  return;
}


void StTofrDaqMap::Reset() {
  for(Int_t i=0;i<mNTOFR;i++) {
    mTrayId[i] = 0;
    mModuleId[i] = 0;
    mCellId[i] = 0;
    mAdc[i] = -1;
    mTdc[i] = -1;
  }
  // tofr5
  for(int i=0;i<mNTOFR5;i++){
    mGlobalTDCChan[i]=0;
    mGlobalModuleChan[i]=0;
  }
  // tof8++
  for(int i=0;i<mNTOF;i++) {
    mMRPC2TDIGChan[i] = -1;
    mTDIG2MRPCChan[i] = -1;
    mTDIGLe2PMTChan[i] = -1;
    mTDIGTe2PMTChan[i] = -1;
  }
  for(int i=0;i<mNVPD;i++) {
    mPMT2TDIGLeChan[i] = -1;
    mPMT2TDIGTeChan[i] = -1;
  }

}

IntVec StTofrDaqMap::DaqChan2Cell( const Int_t iTofrDaq )
{
  IntVec map;
  map.clear();
  if ( iTofrDaq<0 || iTofrDaq>=mNTOFR ) {
    LOG_INFO << " ERROR! Uncorrected iTofrDaq number for Tofr! " << endm;
    return map;
  }

  map.push_back(mTrayId[iTofrDaq]);
  map.push_back(mModuleId[iTofrDaq]);
  map.push_back(mCellId[iTofrDaq]);

  return map;
}



Int_t StTofrDaqMap::Cell2DaqChan( const Int_t iTray, const Int_t iModule, const Int_t iCell )
{
  Int_t daq = mDAQOVERFLOW;

  for(Int_t i=0;i<mNTOFR;i++) {
    if( mTrayId[i]==iTray && mModuleId[i]==iModule && mCellId[i]==iCell ) {
      daq = i;
      break;
    }
  }

  return daq;
}


IntVec StTofrDaqMap::ADCChan2Cell( const Int_t iAdc )
{
  Int_t daq = ADCChan2DaqChan(iAdc);

  IntVec map;
  map.clear();
  if(daq==mDAQOVERFLOW) {
    gMessMgr->Warning(" iAdc is out of ADC channels");
    return map;
  }

  return DaqChan2Cell(daq);
}

IntVec StTofrDaqMap::TDCChan2Cell( const Int_t iTdc )
{
  Int_t daq = TDCChan2DaqChan(iTdc);

  IntVec map;
  map.clear();
  if(daq==mDAQOVERFLOW) {
    gMessMgr->Warning(" iTdc is out of TDC channels");
    return map;
  }

  return DaqChan2Cell(daq);
}

Int_t StTofrDaqMap::Cell2ADCChan( const Int_t iTray , const Int_t iModule, const Int_t iCell )
{
  Int_t daq = Cell2DaqChan(iTray, iModule, iCell);
  if(daq==mDAQOVERFLOW) {
    return -1;
  }
  return mAdc[daq];
}

Int_t StTofrDaqMap::Cell2TDCChan( const Int_t iTray , const Int_t iModule, const Int_t iCell )
{
  Int_t daq = Cell2DaqChan(iTray, iModule, iCell);
  if (daq==mDAQOVERFLOW) {
    return -1;
  }
  return mTdc[daq];
}

Int_t StTofrDaqMap::DaqChan2ADCChan( const Int_t iTofrDaq )
{
  if(iTofrDaq<0 || iTofrDaq>=mNTOFR ) {
    LOG_INFO << " ERROR! Uncorrected iTofrDaq number for Tofr! " << endm;
    return -1;
  }

  return mAdc[iTofrDaq];
}

Int_t StTofrDaqMap::DaqChan2TDCChan( const Int_t iTofrDaq )
{
  if(iTofrDaq<0 || iTofrDaq>=mNTOFR ) {
    LOG_INFO << " ERROR! Uncorrected iTofrDaq number for Tofr! " << endm;
    return -1;
  }

  return mTdc[iTofrDaq];
}


Int_t StTofrDaqMap::ADCChan2DaqChan( const Int_t iAdc )
{
  Int_t daq = mDAQOVERFLOW;
  for(Int_t i=0;i<mNTOFR;i++) {
    if(mAdc[i]==iAdc) {
      daq = i;
      break;
    }
  }

  return daq;
}

Int_t StTofrDaqMap::TDCChan2DaqChan( const Int_t iTdc )
{
  Int_t daq = mDAQOVERFLOW;
  for(Int_t i=0;i<mNTOFR;i++) {
    if(mTdc[i]==iTdc) {
      daq = i;
      break;
    }
  }
  return daq;
}

//tofr5 

IntVec StTofrDaqMap::Tofr5TDCChan2Cell( const Int_t iTdc)
{
  IntVec map;
  map.clear();

  if ( iTdc<0 || iTdc>=mNTOFR5 ) {
    LOG_INFO << " ERROR! Uncorrected TDC Channel number for Tofr5! " << endm;
    return map;
  }

  Int_t Tray=93;   // only tray 93 is installed in tofr5
  Int_t ModuleChan = mGlobalModuleChan[iTdc];
  Int_t Module = ModuleChan/6+1;
  Int_t Cell   = ModuleChan%6+1;
  map.push_back(Tray);
  map.push_back(Module);
  map.push_back(Cell);

  return map;
}

Int_t StTofrDaqMap::Tofr5Cell2TDCChan( const Int_t iTray , const Int_t iModule, const Int_t iCell )
{

  if(iTray!=93 ) {
    LOG_INFO<<"ERROR!!! Wrong tray number !"<<endm;
    return -1;
  }
  if(iModule<1 || iModule>32 ) {
    LOG_INFO<<"ERROR!!! Wrong module number !"<<endm;
    return -1;
  }
  if(iCell <1 || iCell > 6) {
    LOG_INFO<<"ERROR!!! Wrong cell number ! "<<endm; 
    return -1;
  }

  Int_t modulechan = (iModule-1)*6+(iCell-1);

  if (modulechan<1 || modulechan>=mNTOFR5) {
    LOG_INFO<<"ERROR!!! Wrong Module Cell channel number!"<<endm;
    return -1;
  }

  return mGlobalTDCChan[modulechan];
}

//tof8++
IntVec StTofrDaqMap::TDIGChan2Cell( const Int_t iTdc)
{
  IntVec map;
  map.clear();

  if ( iTdc<0 || iTdc>=mNTOF ) {
    LOG_INFO << " ERROR! Uncorrected TDC Channel number for Tof! " << endm;
    return map;
  }

  Int_t ModuleChan = mTDIG2MRPCChan[iTdc];
  Int_t Module = ModuleChan / mNCell + 1;
  Int_t Cell   = ModuleChan % mNCell + 1;
  map.push_back(Module);
  map.push_back(Cell);

  return map;
}

Int_t StTofrDaqMap::Cell2TDIGChan( const Int_t iModule, const Int_t iCell )
{

  if(iModule<1 || iModule>mNModule ) {
    LOG_INFO<<"ERROR!!! Wrong module number !"<<endm;
    return -1;
  }
  if(iCell <1 || iCell > mNCell) {
    LOG_INFO<<"ERROR!!! Wrong cell number ! "<<endm; 
    return -1;
  }

  Int_t modulechan = (iModule-1)*mNCell+(iCell-1);

  if (modulechan<1 || modulechan>=mNTOF) {
    LOG_INFO<<"ERROR!!! Wrong Module Cell channel number!"<<endm;
    return -1;
  }

  return mMRPC2TDIGChan[modulechan];
}

Int_t StTofrDaqMap::PMT2TDIGLeChan( const Int_t iTube )
{
  if ( iTube<1 || iTube>mNVPD ) {
    LOG_INFO<<"ERROR!!! Wrong vpd tube number ! "<<endm; 
    return -1;
  }

  return mPMT2TDIGLeChan[iTube];
}

Int_t StTofrDaqMap::PMT2TDIGTeChan( const Int_t iTube )
{
  if ( iTube<1 || iTube>mNVPD ) {
    LOG_INFO<<"ERROR!!! Wrong vpd tube number ! "<<endm; 
    return -1;
  }

  return mPMT2TDIGTeChan[iTube];
}

Int_t StTofrDaqMap::TDIGLeChan2PMT( const Int_t iTdc )
{
  if ( iTdc<1 || iTdc>mNTOF ) {
    LOG_INFO<<"ERROR!!! Wrong tdc channel number ! "<<endm; 
    return -1;
  }

  return mTDIGLe2PMTChan[iTdc];
}

Int_t StTofrDaqMap::TDIGTeChan2PMT( const Int_t iTdc )
{
  if ( iTdc<1 || iTdc>mNTOF ) {
    LOG_INFO<<"ERROR!!! Wrong tdc channel number ! "<<endm; 
    return -1;
  }

  return mTDIGTe2PMTChan[iTdc];
}

IntVec StTofrDaqMap::ValidTrays()
{
  IntVec trayId;
  for(int i=0;i<mNTray;i++) {
    trayId.push_back(mValidTrayId[i]);
  }

  return trayId;
}
