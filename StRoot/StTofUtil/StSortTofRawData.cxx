#include <iostream>
#include "StMessMgr.h"
#include "StSortTofRawData.h"
#include "StEvent.h"
#include "StTofCollection.h"
#include "StTofRawData.h"

StSortTofRawData::StSortTofRawData() {
  Reset();
}

StSortTofRawData::StSortTofRawData(StTofCollection *tofColl) {
  Reset();
  Init(tofColl);
}

StSortTofRawData::~StSortTofRawData() {
  Reset();
}

void StSortTofRawData::Reset() {
  mRawHitVec.clear();
}

void StSortTofRawData::Init(StTofCollection *tofColl) {
  if(tofColl && tofColl->rawdataPresent()) {
    StSPtrVecTofRawData &tofRawData = tofColl->tofRawData();
    for(size_t i=0; i<tofRawData.size(); i++) {
      if(tofRawData[i]->leteFlag()!=1) continue; // leading
      int itray = tofRawData[i]->tray();
      int ichan = tofRawData[i]->channel();
      bool iexist = kFALSE;
      for(size_t ii=0; ii<mRawHitVec.size(); ii++) {
	if(itray==mRawHitVec[ii].tray && ichan==mRawHitVec[ii].channel) {
	  iexist = kTRUE;
	  break;
	}
      }
      if(iexist) continue;
      TOFRawHit aRawHit;
      aRawHit.tray = itray;
      aRawHit.channel = ichan;
      aRawHit.leadingTdc.push_back((int)(tofRawData[i]->tdc()));
      for(size_t j=i+1;j<tofRawData.size();j++) {
	if(tofRawData[j]->leteFlag()==1 &&
	   itray==tofRawData[j]->tray() && ichan==tofRawData[j]->channel()) {
	  aRawHit.leadingTdc.push_back((int)(tofRawData[j]->tdc()));
	}
      }
      
      // trailing
      for(size_t j=0;j<tofRawData.size();j++) {
	if(tofRawData[j]->leteFlag()==2 &&
	   itray==tofRawData[j]->tray() && ichan==tofRawData[j]->channel()) {
	  aRawHit.trailingTdc.push_back((int)(tofRawData[j]->tdc()));
	}
      }
      if(aRawHit.trailingTdc.size()) mRawHitVec.push_back(aRawHit);
    }
  } else {
    gMessMgr->Warning("","OS") << " No Tof Collection !!! " << endm;
  }
}

IntVec StSortTofRawData::GetValidChannel() {
  IntVec chanVec;
  for(size_t i=0 ; i<mRawHitVec.size() ; i++) {
    chanVec.push_back(mRawHitVec[i].channel);
  }
  return chanVec;
}

IntVec StSortTofRawData::GetLeadingTdc(int channel) {
  IntVec leTdc;
  for(size_t i=0 ; i<mRawHitVec.size() ; i++) {
    if(mRawHitVec[i].channel!=channel) continue;
    leTdc = mRawHitVec[i].leadingTdc;
  }
  return leTdc;
}

IntVec StSortTofRawData::GetTrailingTdc(int channel) {
  IntVec teTdc;
  for(size_t i=0 ; i<mRawHitVec.size() ; i++) {
    if(mRawHitVec[i].channel!=channel) continue;
    teTdc = mRawHitVec[i].trailingTdc;
  }
  return teTdc;
}

IntVec StSortTofRawData::GetValidChannel(int tray) {
  IntVec chanVec;
  for(size_t i=0 ; i<mRawHitVec.size() ; i++) {
    if( mRawHitVec[i].tray == tray )
      chanVec.push_back(mRawHitVec[i].channel);
  }
  return chanVec;
}

IntVec StSortTofRawData::GetLeadingTdc(int tray, int channel) {
  IntVec leTdc;
  for(size_t i=0 ; i<mRawHitVec.size() ; i++) {
    if(mRawHitVec[i].tray!=tray) continue;
    if(mRawHitVec[i].channel!=channel) continue;
    leTdc = mRawHitVec[i].leadingTdc;
  }
  return leTdc;
}

IntVec StSortTofRawData::GetTrailingTdc(int tray, int channel) {
  IntVec teTdc;
  for(size_t i=0 ; i<mRawHitVec.size() ; i++) {
    if(mRawHitVec[i].tray!=tray) continue;
    if(mRawHitVec[i].channel!=channel) continue;
    teTdc = mRawHitVec[i].trailingTdc;
  }
  return teTdc;
}
