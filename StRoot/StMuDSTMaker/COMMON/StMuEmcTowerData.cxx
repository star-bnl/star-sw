//###########################################################
// EMC Tower Data
// Author: Marco van Leeuwen
// initial version 09/2004
//
//########################################################### 
#include <string.h>

#include "StMuEmcTowerData.h"
#include "Stiostream.h"
#include "StMuEmcUtil.h"
static StMuEmcUtil util; // to ease decoding of EEMC hits
ClassImp(StMuEmcTowerData)

StMuEmcTowerData::StMuEmcTowerData()
{    
  memset(mTowerADC,0,sizeof(mTowerADC));
  memset(mEndcapTowerADC,0,sizeof(mEndcapTowerADC));
  memset(mBTowCrateFlags,0,sizeof(mBTowCrateFlags));
  memset(mBSmdCrateFlags,0,sizeof(mBSmdCrateFlags));
  memset(mBPrsCrateFlags,0,sizeof(mBPrsCrateFlags));
  memset(mETowCrateFlags,0,sizeof(mETowCrateFlags));
  memset(mESmdCrateFlags,0,sizeof(mESmdCrateFlags));
  memset(mEPrsCrateFlags,0,sizeof(mEPrsCrateFlags));
} 

StMuEmcTowerData::StMuEmcTowerData(StMuEmcTowerData& o) {
  cout << "StMuEmcTowerData::StMuEmcTowerData(StMuEmcTowerData&) not implemented. Exiting." << endl;
  exit(1);
}

StMuEmcTowerData::~StMuEmcTowerData()
{
}

int StMuEmcTowerData::towerADC(int id, int detector)
{
  if(detector == bemc)
  {
    if(id<1 || id>nEmcTowers) return 0;
    return (int)mTowerADC[id-1];
  }
  if(detector == eemc)
  {
    if(id<1 || id>nEndcapTowers) return 0;
    return (int)mEndcapTowerADC[id-1];
  }
  return 0;
}

StEmcCrateStatus StMuEmcTowerData::crateStatus(int crate, int detector) {
  switch (detector) {
  case bemc:
    if (crate>0 && crate<=nBTowCrates)
      return (StEmcCrateStatus) mBTowCrateFlags[crate-1];
    break;
   
  case bsmde:
  case bsmdp:
    if (crate>0 && crate<=nBSmdCrates)
      return (StEmcCrateStatus) mBSmdCrateFlags[crate-1];
    break;
    
  case bprs:
    if (crate>0 && crate<=nBPrsCrates)
      return (StEmcCrateStatus) mBPrsCrateFlags[crate-1];
    break;

  case eemc:
    if (crate>0 && crate<=nETowCrates)
      return (StEmcCrateStatus) mETowCrateFlags[crate-1];
    break;
   
  case esmdu:
  case esmdv:
    if (crate>0 && crate<=nESmdCrates)
      return (StEmcCrateStatus) mESmdCrateFlags[crate-1];
    break;
    
  case eprs:
    if (crate>0 && crate<=nEPrsCrates)
      return (StEmcCrateStatus) mEPrsCrateFlags[crate-1];
    break;
  }
  return crateUnknown;
}

void StMuEmcTowerData::setTowerADC(int id,int adc, int detector)
{
  if(detector == bemc)
  {
    if(id<1 || id>nEmcTowers) return;
    mTowerADC[id-1]=adc;
  }
  if(detector == eemc)
  {
    if(id<1 || id>nEndcapTowers) return;
    mEndcapTowerADC[id-1]=adc;
  }
  return;
}

void StMuEmcTowerData::setCrateStatus(StEmcCrateStatus status, int crate, int detector) {
  switch (detector) {
  case bemc:
    if (crate>0 && crate<=nBTowCrates)
      mBTowCrateFlags[crate-1] = (unsigned char) status;
    break;
   
  case bsmde:
  case bsmdp:
    if (crate>0 && crate<=nBSmdCrates)
      mBSmdCrateFlags[crate-1] = (unsigned char) status;
    break;
    
  case bprs:
    if (crate>0 && crate<=nBPrsCrates)
      mBPrsCrateFlags[crate-1] = (unsigned char) status;
    break;

  case eemc:
    if (crate>0 && crate<=nETowCrates)
      mETowCrateFlags[crate-1] = (unsigned char) status;
    break;
   
  case esmdu:
  case esmdv:
    if (crate>0 && crate<=nESmdCrates)
      mESmdCrateFlags[crate-1] = (unsigned char) status;
    break;

  case eprs:
    if (crate>0 && crate<=nEPrsCrates)
      mEPrsCrateFlags[crate-1] = (unsigned char) status;
    break;
  }
}


void StMuEmcTowerData
::getEndcapTowerADC(int ihit1, int &adc, int &sec, int &sub, int & eta)
{
  int ihit=ihit1+1;  // it was not my idea to abort on index=0, JB
  adc=towerADC(ihit,eemc);
  if(! util.getEndcapBin(eemc,ihit,sec,eta,sub))  return ;
  adc=sec=sub=eta=-1;
  return;
}


