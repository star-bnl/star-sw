/*******************************************************************
 *
 * $Id: StBTofSortRawHit.cxx,v 1.10 2010/12/10 21:37:45 geurts Exp $
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
#include "tables/St_vpdDelay_Table.h"

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
  memset(mVpdDelay,0,sizeof(mVpdDelay));
}

void StBTofSortRawHit::Init(StMaker *maker, StBTofDaqMap *daqMap) {
  // initial time windows from dbase
  Init();

  // for test set by hand now
  ///initial time windows
  LOG_INFO << "[StBTofSortRawHit] retrieving BTOF trigger time window cuts" << endm;
  TDataSet *mDbTOFDataSet = maker->GetDataBase("Calibrations/tof/tofTrgWindow");
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
      LOG_DEBUG << " Tray = " << i+1 << " Trigger Window = " << mTriggerTimeWindow[i][0] << " " << mTriggerTimeWindow[i][1] << endm;
    }
  }

  mDaqMap = daqMap;

  if(maker->Debug()) mDebug = kTRUE;

  LOG_INFO << "[StBTofSortRawHit] retrieving VPD delay settings" << endm;
  mDbTOFDataSet = maker->GetDataBase("Calibrations/tof/vpdDelay");

  //fg  
  if (mDbTOFDataSet){
    St_vpdDelay *vpdDelayTable = static_cast<St_vpdDelay*>(mDbTOFDataSet->Find("vpdDelay"));
    if (!vpdDelayTable) {
      LOG_ERROR << "unable to find vpdDelay table" << endm;
      return;
    }
    vpdDelay_st* vpdDelay = static_cast<vpdDelay_st*>(vpdDelayTable->GetArray());
    if (!vpdDelay) {
      LOG_ERROR << "unable to get vpdDelay data" << endm;
      return;
    }
    for (int i=0;i<2*mNVPD;i++){
      mVpdDelay[i] = vpdDelay->delay[i]; 
    }
  } 
  else {
    // Note: this construction addresses RT#1996 and allows backward compatibility with older database
    //       timestamps at which these database structures did not exist.
    LOG_WARN << "unable to find vpdDelay dataset:  assuming Run-9 (200GeV) default values for vpdDelay" << endm;
    float delay[2*mNVPD]={	 
      0,-0.564753,-4.62291,-4.84402,-4.05943,6.32389,-9.4035,-10.3113,-17.0374,-17.3734,-6.04608,-11.9614,-12.7579,8.79609,3.8467,-17.2994,-17.6424,-21.4749,-22.9736,	 
      0,-2.1707,  -4.8195, -6.5161, -4.3109, 6.3116, -8.8655,-10.1037,-16.5970,-17.9588,-5.2079, -12.1249,-12.2412,8.4001, 5.5702,-16.5936,-16.4152,-21.3076,-21.1452};

    for (int i=0;i<2*mNVPD;i++){
      mVpdDelay[i] = delay[i]; 
    }
  }

  return;
}

void StBTofSortRawHit::setBTofCollection(StBTofCollection* tofColl) {

  /// clean up the pool
  Reset();

  if(tofColl && tofColl->tofHeader()) {
    for(int i=0;i<mNFIBER;i++) {
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
      if (itray<=0 || itray>mNTRAY || ichan<0 || ichan>=mNCHAN || ifiber<0 || ifiber>=mNFIBER ) {
         LOG_FATAL << " StBTofSortRawHit::setBTofCollection:: "
                   << ": itray=" << itray 
                   << ": ichan=" << ichan 
                   << ": ifiber=" << ifiber
                   << endm;
         continue;
      }
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

          // add Vpd delay
          int iTube = (tray==121) ? mDaqMap->TDIGLeChan2WestPMT(channel) : mDaqMap->TDIGLeChan2EastPMT(channel);
          timeDiff -= mVpdDelay[(tray-121)*mNVPD+(iTube-1)];
          while(timeDiff<0) timeDiff += 51200;

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
	  if(timeDiff>=mTriggerTimeWindow[tray-1][0]&&timeDiff<=mTriggerTimeWindow[tray-1][1]+25.)  // trailing edge - allow 25 ns shaping time
	    teTdc.push_back(mRawHitVec[tray-1][i].trailingTdc[j]);   
	}
	else {
	  teTdc.push_back(mRawHitVec[tray-1][i].trailingTdc[j]);
	}
      } 
      else {  
	if(triggerevent){  //vpds, keep physical hits in trigger window

          // add Vpd delay - Note: channel should be the corresponding leading edge channel
          int iTube = (tray==121) ? mDaqMap->TDIGLeChan2WestPMT(channel) : mDaqMap->TDIGLeChan2EastPMT(channel);
          timeDiff -= mVpdDelay[(tray-121)*mNVPD+(iTube-1)];
          while(timeDiff<0) timeDiff += 51200;

	  double stime = timeDiff;
	  if(stime > ftime+300.){ //all hits thereafter within 300ns are not physical
	    if(timeDiff>=mTriggerTimeWindow[tray-1][0]&&timeDiff<=mTriggerTimeWindow[tray-1][1]+25.)  // trailing edge - allow 25ns shaping time
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
