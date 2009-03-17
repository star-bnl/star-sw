/*******************************************************************
 *
 * $Id: StBTofSortRawHit.cxx,v 1.4 2009/03/17 18:32:26 fine Exp $
 *  
 * Author: Xin Dong   
 *****************************************************************    
 *     
 * Description:  Utilities to group the raw Tof hits for convenient use
 *
 *****************************************************************
 *
 *******************************************************************/

#include <iostream>
#include "StMessMgr.h"
#include "StBTofSortRawHit.h"
#include "StEvent.h"
#include "StBTofCollection.h"
#include "StBTofRawHit.h"
#include "StBTofDaqMap.h"
#include "tables/St_tofTrgWindow_Table.h"

StBTofSortRawHit::StBTofSortRawHit() {
  mDaqMap = 0;
  mDebug  = kFALSE;
  Reset();
}

StBTofSortRawHit::~StBTofSortRawHit() {
  Reset();
}

void StBTofSortRawHit::Reset() {
  for(int i=0;i<mNTRAY;i++) mRawHitVec[i].clear();
  memset(mTriggerTime,0,sizeof(mTriggerTime));
}

void StBTofSortRawHit::Init() {
  Reset();
  memset(mTriggerTimeWindow,0,sizeof(mTriggerTimeWindow));
}

void StBTofSortRawHit::Init(StMaker *maker, StBTofDaqMap *daqMap) {
  // initial time windows from dbase
  Init();

  // for test set by hand now
  ///initial time windows
  LOG_INFO << "StBTofSortRawHit -- retrieving the tof trigger time window cuts" << endm;

  TDataSet *mDbTOFDataSet = maker->GetDataBase("Calibrations/tof");
  if(!mDbTOFDataSet) {
    LOG_ERROR << "unable to access Calibrations TOF parameters" << endm;
    //    assert(mDbTOFDataSet);
    return; // kStErr;
  }
 
  St_tofTrgWindow* tofTrgWindow = static_cast<St_tofTrgWindow*>(mDbTOFDataSet->Find("tofTrgWindow"));
  if(!tofTrgWindow) {
    LOG_ERROR << "unable to get tof Module map table" << endm;
    return; // kStErr;
  }
  tofTrgWindow_st* trgWin = static_cast<tofTrgWindow_st*>(tofTrgWindow->GetArray());
  for (Int_t i=0;i<mNTRAY;i++) {
    mTriggerTimeWindow[i][0] = (Float_t)trgWin[i].trgWindow_Min;
    mTriggerTimeWindow[i][1] = (Float_t)trgWin[i].trgWindow_Max;
    if(maker->Debug()) {
      LOG_INFO << " Tray = " << i+1 << " Trigger Window = " << mTriggerTimeWindow[i][0] << " " << mTriggerTimeWindow[i][1] << endm;
    }
  }

  mDaqMap = daqMap;

  if(maker->Debug()) mDebug = kTRUE;

  return;
}

void StBTofSortRawHit::setBTofCollection(StBTofCollection* tofColl) {

  /// clean up the pool
  Reset();

  if(tofColl && tofColl->tofHeader()) {
    for(int i=0;i<4;i++) {
      mTriggerTime[i] = tofColl->tofHeader()->triggerTime(i);
    }
    if(mDebug) {
      LOG_INFO << " Trigger Time Stamps " << endm;
      LOG_INFO << mTriggerTime[0] << " " << mTriggerTime[1] << " " <<
                  mTriggerTime[2] << " " << mTriggerTime[3] << endm;
    }
  }

  if(tofColl && tofColl->rawHitsPresent()) {

    StSPtrVecBTofRawHit &tofRawHits = tofColl->tofRawHits();

    // test 
    if(mDebug) {
      LOG_INFO << " INPUT ...... " << endm;
      for(size_t i=0; i<tofRawHits.size(); i++) {
        LOG_INFO << " flag=" << tofRawHits[i]->flag()
                 << " tray=" << tofRawHits[i]->tray() << " channel="
                 << tofRawHits[i]->channel() << " tdc="
                 << tofRawHits[i]->tdc() << endm;
      }
    }
    //

    for(size_t i=0; i<tofRawHits.size(); i++) {
      if(!tofRawHits[i]->leadingEdge()) continue; // leading
      int itray = tofRawHits[i]->tray();
      int ichan = tofRawHits[i]->channel();
      int ifiber = tofRawHits[i]->fiberId();
      bool iexist = kFALSE;
      if (itray > mNTRAY ) {
         LOG_FATAL << " StBTofSortRawHit::setBTofCollection:: "
                   << "i="<< i 
                   << ": itray=" << itray 
                   << ": ichan=" << ichan 
                   << ": ifiber=" << ifiber
                   << endm;
      }
      for(size_t ii=0; ii<mRawHitVec[itray-1].size(); ii++) {
	if(itray==mRawHitVec[itray-1][ii].tray && ichan==mRawHitVec[itray-1][ii].channel) {
	  iexist = kTRUE;
	  break;
	}
      }
      if(iexist) continue;
      TOFRawHit aRawHit;
      aRawHit.fiberId = ifiber;
      aRawHit.tray = itray;
      aRawHit.channel = ichan;
      aRawHit.leadingTdc.push_back(tofRawHits[i]->tdc());
      for(size_t j=i+1;j<tofRawHits.size();j++) {
	if(tofRawHits[j]->leadingEdge() &&
	   itray==tofRawHits[j]->tray() && ichan==tofRawHits[j]->channel()) {
	  aRawHit.leadingTdc.push_back(tofRawHits[j]->tdc());
	}
      }
      
      // trailing
      if(itray<=120) { // trays
        for(size_t j=0;j<tofRawHits.size();j++) {
          if(tofRawHits[j]->trailingEdge() &&
	     itray==tofRawHits[j]->tray() && ichan==tofRawHits[j]->channel()) {
	    aRawHit.trailingTdc.push_back(tofRawHits[j]->tdc());
	  }
        }
      } else {
        if(!mDaqMap) {
          LOG_INFO << " No Daq Map for VPD, continue!" << endm;
          continue;
        }
        int iTube = (itray==121) ? mDaqMap->TDIGLeChan2WestPMT(ichan) : mDaqMap->TDIGLeChan2EastPMT(ichan);
        for(size_t j=0;j<tofRawHits.size();j++) {
          if(tofRawHits[j]->trailingEdge() && itray==tofRawHits[j]->tray()) {
            int techan = (int)(tofRawHits[j]->channel());
	    int jTube = (itray==121) ? mDaqMap->TDIGTeChan2WestPMT(techan) : mDaqMap->TDIGTeChan2EastPMT(techan);
	    if(iTube==jTube) {
	      aRawHit.trailingTdc.push_back(tofRawHits[j]->tdc());
	    }
          }
        }
      }
      if(aRawHit.trailingTdc.size()) mRawHitVec[itray-1].push_back(aRawHit);
    }
  } else {
    LOG_WARN << " No Tof Collection !!! " << endm;
  }

  if(mDebug) {
    for(int i=0;i<122;i++) {
      for(size_t m=0;m<mRawHitVec[i].size();m++) {
        LOG_DEBUG << " tray = " << i+1 << " " << mRawHitVec[i][m].tray << " channel = " << mRawHitVec[i][m].channel << endm;
        LOG_DEBUG << " leading tdcs = ";
        for(size_t j=0;j<mRawHitVec[i][m].leadingTdc.size();j++) {
          LOG_DEBUG << " " << mRawHitVec[i][m].leadingTdc[j];
        }
        LOG_DEBUG << endm;
        LOG_DEBUG << " trailing tdcs = ";
        for(size_t j=0;j<mRawHitVec[i][m].trailingTdc.size();j++) {
          LOG_DEBUG << " " << mRawHitVec[i][m].trailingTdc[j];
        }
        LOG_DEBUG << endm;
      }
    }
  }
}

IntVec StBTofSortRawHit::GetValidChannel(int tray) {
  IntVec chanVec;
  for(size_t i=0 ; i<mRawHitVec[tray-1].size() ; i++) {
    if( mRawHitVec[tray-1][i].tray == tray )
      chanVec.push_back(mRawHitVec[tray-1][i].channel);
  }
  return chanVec;
}

UIntVec StBTofSortRawHit::GetLeadingTdc(int tray, int channel, bool triggerevent)
{
  UIntVec leTdc;
  for(size_t i=0 ; i<mRawHitVec[tray-1].size() ; i++) {
    if(mRawHitVec[tray-1][i].tray!=tray) continue;
    if(mRawHitVec[tray-1][i].channel!=channel) continue;
    int fiberId = mRawHitVec[tray-1][i].fiberId;
    double ftime = -1e5;
    for(size_t j=0; j<mRawHitVec[tray-1][i].leadingTdc.size(); j++) {
      float trgTime = 25.*(mTriggerTime[fiberId] & 0xfff);
      float timeDiff = mRawHitVec[tray-1][i].leadingTdc[j]*25./1024 - trgTime;
      while(timeDiff<0) timeDiff += 51200;
      if(tray<=120){  //trays, keep all hits
	if(triggerevent){ 
	  if(timeDiff>=mTriggerTimeWindow[tray-1][0]&&timeDiff<=mTriggerTimeWindow[tray-1][1])
	    leTdc.push_back(mRawHitVec[tray-1][i].leadingTdc[j]);   
	}
	else {
	  leTdc.push_back(mRawHitVec[tray-1][i].leadingTdc[j]);
	}
      } 
      else {   
	if(triggerevent){ //vpds, keep physical hits in trigger window
	  double stime = timeDiff;
	  if(stime > ftime+300.){ //all hits thereafter within 300ns are not physical
	    if(timeDiff>=mTriggerTimeWindow[tray-1][0]&&timeDiff<=mTriggerTimeWindow[tray-1][1])
	      leTdc.push_back(mRawHitVec[tray-1][i].leadingTdc[j]);   
	    ftime = stime;
	  }
	}
	else {
	  leTdc.push_back(mRawHitVec[tray-1][i].leadingTdc[j]);   
	}
      }
    }
  }
  return leTdc;
}

UIntVec StBTofSortRawHit::GetTrailingTdc(int tray, int channel, bool triggerevent)
{
  UIntVec teTdc;
  for(size_t i=0 ; i<mRawHitVec[tray-1].size() ; i++) {
    if(mRawHitVec[tray-1][i].tray!=tray) continue;
    if(mRawHitVec[tray-1][i].channel!=channel) continue;
    int fiberId = mRawHitVec[tray-1][i].fiberId;
    double ftime = -1e5;
    for(size_t j=0; j<mRawHitVec[tray-1][i].trailingTdc.size(); j++) {
      float trgTime = 25.*(mTriggerTime[fiberId] & 0xfff);
      float timeDiff = mRawHitVec[tray-1][i].trailingTdc[j]*25./1024 - trgTime;
      while(timeDiff<0) timeDiff += 51200;
      if(tray<=120){  //trays, keep all hits
	if(triggerevent){ 
	  if(timeDiff>=mTriggerTimeWindow[tray-1][0]&&timeDiff<=mTriggerTimeWindow[tray-1][1])
	    teTdc.push_back(mRawHitVec[tray-1][i].trailingTdc[j]);   
	}
	else {
	  teTdc.push_back(mRawHitVec[tray-1][i].trailingTdc[j]);
	}
      } 
      else {  
	if(triggerevent){  //vpds, keep physical hits in trigger window
	  double stime = timeDiff;
	  if(stime > ftime+300.){ //all hits thereafter within 300ns are not physical
	    if(timeDiff>=mTriggerTimeWindow[tray-1][0]&&timeDiff<=mTriggerTimeWindow[tray-1][1])
	      teTdc.push_back(mRawHitVec[tray-1][i].trailingTdc[j]);   
	    ftime = stime;
	  }
	}
	else {
	  teTdc.push_back(mRawHitVec[tray-1][i].trailingTdc[j]);   
	}
      }
    }
  }
  return teTdc;
}
