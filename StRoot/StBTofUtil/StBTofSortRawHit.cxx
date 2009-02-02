/*******************************************************************
 *
 * $Id: StBTofSortRawHit.cxx,v 1.1 2009/02/02 21:58:06 dongx Exp $
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

StBTofSortRawHit::StBTofSortRawHit() {
}

StBTofSortRawHit::~StBTofSortRawHit() {
  Reset();
}

void StBTofSortRawHit::Reset() {
  for(unsigned int i=0;i<mNTRAY;i++) mRawHitVec[i].clear();
}

void StBTofSortRawHit::Init() {
  Reset();
}

void StBTofSortRawHit::Init(StMaker *maker, StBTofCollection *tofColl, StBTofDaqMap *daqMap) {
  // initial time windows from dbase
  Init();

  // for test set by hand now
  ///initial time windows
  for(unsigned int i=0;i<mNTRAY;i++){
    for(int j=0;j<2;j++) mTriggerTimeWindow[i][j] = 0.;
    if(i>=75&&i<80){
      mTriggerTimeWindow[i][0] = 3270;
      mTriggerTimeWindow[i][1] = 3390;
    }
    if(i==120){
      mTriggerTimeWindow[i][0] = 3230;
      mTriggerTimeWindow[i][1] = 3340;
    }
    if(i==121){
      mTriggerTimeWindow[i][0] = 3290;
      mTriggerTimeWindow[i][1] = 3400;
    }
  }  

  if(tofColl && tofColl->tofHeader()) {
    for(int i=0;i<4;i++) {
      mTriggerTime[i] = tofColl->tofHeader()->triggerTime(i);
    }
    if(maker->Debug()) {
      LOG_INFO << " Trigger Time Stamps " << endm;
      LOG_INFO << mTriggerTime[0] << " " << mTriggerTime[1] << " " <<
                  mTriggerTime[2] << " " << mTriggerTime[3] << endm;
    }
  }

  if(tofColl && tofColl->rawHitsPresent()) {

    StSPtrVecBTofRawHit &tofRawHits = tofColl->tofRawHits();

    // test 
    if(maker->Debug()) {
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
        if(!daqMap) {
          LOG_INFO << " No Daq Map for VPD, continue!" << endm;
          continue;
        }
        int iTube = (itray==121) ? daqMap->TDIGLeChan2WestPMT(ichan) : daqMap->TDIGLeChan2EastPMT(ichan);
        for(size_t j=0;j<tofRawHits.size();j++) {
          if(tofRawHits[j]->trailingEdge() && itray==tofRawHits[j]->tray()) {
            int techan = (int)(tofRawHits[j]->channel());
	    int jTube = (itray==121) ? daqMap->TDIGTeChan2WestPMT(techan) : daqMap->TDIGTeChan2EastPMT(techan);
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

  if(maker->Debug()) {
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
      float trgTime = 25.*(mTriggerTime[fiberId] & 0xfff) - 2775.;
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
      float trgTime = 25.*(mTriggerTime[fiberId] & 0xfff) - 2775.;
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
