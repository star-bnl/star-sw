#include <iostream>
#include "StMessMgr.h"
#include "StSortTofRawData.h"
#include "StEvent.h"
#include "StTofCollection.h"
#include "StTofRawData.h"
#include "StTofrDaqMap.h"

StSortTofRawData::StSortTofRawData() {
  Reset();
}

StSortTofRawData::StSortTofRawData(StTofCollection *tofColl) {
  Reset();
  Init(tofColl);
}

StSortTofRawData::StSortTofRawData(StTofCollection *tofColl, StTofrDaqMap *daqMap) {
  Reset();
  SetVPDMap(daqMap);
  InitRun8(tofColl);
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

void StSortTofRawData::InitRun8(StTofCollection *tofColl) {
  if(tofColl && tofColl->rawdataPresent()) {

    //--added by Zebo
    //initial time windows
    for(int i=0;i<122;i++){
      for(int j=0;j<2;j++) mTimeWindow[i][j] = 0.;
      if(i>=75&&i<80){
        mTimeWindow[i][0] = 3270;
        mTimeWindow[i][1] = 3390;
      }
      if(i==120){
        mTimeWindow[i][0] = 3230;
        mTimeWindow[i][1] = 3340;       
      }
      if(i==121){
        mTimeWindow[i][0] = 3290;
        mTimeWindow[i][1] = 3400;
      }
    }
    //--end
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
      aRawHit.triggertime = tofRawData[i]->triggertime();
      aRawHit.leadingTdc.push_back((int)(tofRawData[i]->tdc()));
      for(size_t j=i+1;j<tofRawData.size();j++) {
	if(tofRawData[j]->leteFlag()==1 &&
	   itray==tofRawData[j]->tray() && ichan==tofRawData[j]->channel()) {
	  aRawHit.leadingTdc.push_back((int)(tofRawData[j]->tdc()));
	}
      }
      
      // trailing
      if(itray<=120) { // trays
        for(size_t j=0;j<tofRawData.size();j++) {
          if(tofRawData[j]->leteFlag()==2 &&
	   itray==tofRawData[j]->tray() && ichan==tofRawData[j]->channel()) {
	   aRawHit.trailingTdc.push_back((int)(tofRawData[j]->tdc()));
	  }
        }
      } else {
        int iTube = (itray==121) ? mTDIGLeChan2WestPMT[ichan] : mTDIGLeChan2EastPMT[ichan];
        for(size_t j=0;j<tofRawData.size();j++) {
          if(tofRawData[j]->leteFlag()==2 && itray== tofRawData[j]->tray()) {
             int jTube = (itray==121) ? mTDIGTeChan2WestPMT[tofRawData[j]->channel()] : mTDIGTeChan2EastPMT[tofRawData[j]->channel()];
             if(iTube==jTube) {
                aRawHit.trailingTdc.push_back((int)(tofRawData[j]->tdc()));
             }
          }
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

IntVec StSortTofRawData::GetLeadingTdc(int tray, int channel)
 {
  IntVec leTdc;
  for(size_t i=0 ; i<mRawHitVec.size() ; i++) {
    if(mRawHitVec[i].tray!=tray) continue;
    if(mRawHitVec[i].channel!=channel) continue;
    leTdc = mRawHitVec[i].leadingTdc;
  }
  return leTdc;
}

//--added by Zebo
IntVec StSortTofRawData::GetLeadingTdc(int tray, int channel, bool triggerevent)
{
  IntVec leTdc;
  for(size_t i=0 ; i<mRawHitVec.size() ; i++) {
    if(mRawHitVec[i].tray!=tray) continue;
    if(mRawHitVec[i].channel!=channel) continue;
    for(size_t j=0; j<mRawHitVec[i].leadingTdc.size(); j++) {
       if(triggerevent) {
         float trgTime = 25.*(mRawHitVec[i].triggertime & 0xfff) - 2775.;
         float timeDiff = mRawHitVec[i].leadingTdc[j]*25./1024 - trgTime;
         while(timeDiff<0) timeDiff += 51200;
         if(timeDiff>=mTimeWindow[tray-1][0]&&timeDiff<=mTimeWindow[tray-1][1])
           leTdc.push_back(mRawHitVec[i].leadingTdc[j]);   
       } else {
         leTdc.push_back(mRawHitVec[i].leadingTdc[j]);
       }
    }
  }
  return leTdc;
}
//--end

IntVec StSortTofRawData::GetTrailingTdc(int tray, int channel) {
  IntVec teTdc;
  for(size_t i=0 ; i<mRawHitVec.size() ; i++) {
    if(mRawHitVec[i].tray!=tray) continue;
    if(mRawHitVec[i].channel!=channel) continue;
    teTdc = mRawHitVec[i].trailingTdc;
  }
  return teTdc;
}

//--added by Zebo
IntVec StSortTofRawData::GetTrailingTdc(int tray, int channel, bool triggerevent)
{
  IntVec teTdc;
  for(size_t i=0 ; i<mRawHitVec.size() ; i++) {
    if(mRawHitVec[i].tray!=tray) continue;
    if(mRawHitVec[i].channel!=channel) continue;
    for(size_t j=0; j<mRawHitVec[i].trailingTdc.size(); j++) {
       if(triggerevent) {
         float trgTime = 25.*(mRawHitVec[i].triggertime & 0xfff) - 2775.;
         float timeDiff = mRawHitVec[i].trailingTdc[j]*25./1024 - trgTime;
         while(timeDiff<0) timeDiff += 51200;
         if(timeDiff>=mTimeWindow[tray-1][0]&&timeDiff<=mTimeWindow[tray-1][1])
           teTdc.push_back(mRawHitVec[i].trailingTdc[j]);
       } else {
         teTdc.push_back(mRawHitVec[i].trailingTdc[j]);
       }
    }
  }
  return teTdc;
}
//--end

Int_t StSortTofRawData::GetTriggerTime(int tray, int channel) {
  Int_t triggertime;
  for(size_t i=0 ; i<mRawHitVec.size() ; i++) {
    if(mRawHitVec[i].tray!=tray) continue;
    if(mRawHitVec[i].channel!=channel) continue;
    triggertime = mRawHitVec[i].triggertime;
  }
  return triggertime;
}

void StSortTofRawData::SetVPDMap(StTofrDaqMap* daqMap) {
  for(int i=0;i<mNTOF;i++) {
    mTDIGLeChan2WestPMT[i] = daqMap->TDIGLeChan2WestPMT(i);
    mTDIGTeChan2WestPMT[i] = daqMap->TDIGTeChan2WestPMT(i);
    mTDIGLeChan2EastPMT[i] = daqMap->TDIGLeChan2EastPMT(i);
    mTDIGTeChan2EastPMT[i] = daqMap->TDIGTeChan2EastPMT(i);
  }
  return;
}
