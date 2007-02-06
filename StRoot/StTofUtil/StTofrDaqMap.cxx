/*******************************************************************
 *
 * $Id: StTofrDaqMap.cxx,v 1.7 2007/02/06 20:20:44 dongx Exp $
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
	cout << " id=" << id << "  tray=" << mTrayId[id] << " module=" << mModuleId[id] << "  cell=" << mCellId[id] << endl;
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
      cout << " i=" << i << "  TDC chan =" << mGlobalTDCChan[i] << " module chan=" <<mGlobalModuleChan[i]<<endl;
      }
  }


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

}

IntVec StTofrDaqMap::DaqChan2Cell( const Int_t iTofrDaq )
{
  IntVec map;
  map.clear();
  if ( iTofrDaq<0 || iTofrDaq>=mNTOFR ) {
    cout << " ERROR! Uncorrected iTofrDaq number for Tofr! " << endl;
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
    cout << " ERROR! Uncorrected iTofrDaq number for Tofr! " << endl;
    return -1;
  }

  return mAdc[iTofrDaq];
}

Int_t StTofrDaqMap::DaqChan2TDCChan( const Int_t iTofrDaq )
{
  if(iTofrDaq<0 || iTofrDaq>=mNTOFR ) {
    cout << " ERROR! Uncorrected iTofrDaq number for Tofr! " << endl;
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
    cout << " ERROR! Uncorrected TDC Channel number for Tofr5! " << endl;
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
    cout<<"ERROR!!! Wrong tray number !"<<endl;
    return -1;
  }
  if(iModule<1 || iModule>32 ) {
    cout<<"ERROR!!! Wrong module number !"<<endl;
    return -1;
  }
  if(iCell <1 || iCell > 6) {
    cout<<"ERROR!!! Wrong cell number ! "<<endl; 
    return -1;
  }

  Int_t modulechan = (iModule-1)*6+(iCell-1);

  if (modulechan<1 || modulechan>=mNTOFR5) {
    cout<<"ERROR!!! Wrong Module Cell channel number!"<<endl;
    return -1;
  }

  return mGlobalTDCChan[modulechan];
}
