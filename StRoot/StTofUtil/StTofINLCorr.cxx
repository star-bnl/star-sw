/*******************************************************************
 *
 * $Id: StTofINLCorr.cxx,v 1.1 2007/11/21 18:05:49 dongx Exp $
 *
 * Author: Xin Dong
 *****************************************************************
 *
 * Description: INL correction for all TDIG board channels
 *
 *****************************************************************
 *
 * $Log: StTofINLCorr.cxx,v $
 * Revision 1.1  2007/11/21 18:05:49  dongx
 * first release for run8++
 *
 *
 *
 *******************************************************************/
#include <iostream>
#include "tables/St_tofTDIGOnTray_Table.h"
#include "tables/St_tofINLCorr_Table.h"
#include "StMessMgr.h"
#include "StMaker.h"
#include "StTofINLCorr.h"

StTofINLCorr::StTofINLCorr()
{ 
  Reset();
}

StTofINLCorr::~StTofINLCorr()
{ 
  Reset();
}

void StTofINLCorr::init() {
  Reset();
}

void StTofINLCorr::init(StMaker *maker) {
  initFromDbase(maker);
}

void StTofINLCorr::initFromDbase(StMaker *maker) {

  gMessMgr->Info("StTofINLCorr -- rertieving the INL correction table","OS");
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

  St_tofTDIGOnTray* tofTDIGOnTray = static_cast<St_tofTDIGOnTray*>(mDbTOFDataSet->Find("tofTDIGOnTray"));
  if(!tofTDIGOnTray) {
    gMessMgr->Error("unable to get tof INL correction parameters","OS");
    //    assert(tofTDIGOnTray);
    return; // kStErr;
  }
  tofTDIGOnTray_st* tdigOnTray = static_cast<tofTDIGOnTray_st*>(tofTDIGOnTray->GetArray());

  Int_t numRows = tofTDIGOnTray->GetNRows();
  for (Int_t i=0;i<numRows;i++) {
    Int_t trayId = (Int_t)tdigOnTray[i].trayId;

    if(trayId==mEastVpdTrayId) {  // east vpd
      for(Int_t j=0;j<mNTDIGOnTray;j++)
	mTdigOnEastVpd[j] = (Int_t)tdigOnTray[i].tdigId[j];
    } else if (trayId==mWestVpdTrayId) {  // west vpd
      for(Int_t j=0;j<mNTDIGOnTray;j++)
	mTdigOnWestVpd[j] = (Int_t)tdigOnTray[i].tdigId[j];
    } else if (trayId>0 && trayId<= mNTray) { // barrel Tray
      for(Int_t j=0;j<mNTDIGOnTray;j++)
	mTdigOnTray[trayId-1][j] = (Int_t)tdigOnTray[i].tdigId[j];
    }

    if(maker->Debug()) {
      LOG_INFO << " tray id=" << trayId;
      for(int j=0;j<mNTDIGOnTray;j++) {
	LOG_INFO << "  " << tdigOnTray[i].tdigId[j];
      }
      LOG_INFO << endm;
    }

  }

  St_tofINLCorr* tofINLCorr = static_cast<St_tofINLCorr*>(mDbTOFDataSet->Find("tofINLCorr"));
  if(!tofINLCorr) {
    gMessMgr->Error("unable to get tof INL correction parameters","OS");
    //    assert(tofINLCorr);
    return; // kStErr;
  }
  tofINLCorr_st* inlcorr = static_cast<tofINLCorr_st*>(tofINLCorr->GetArray());

  numRows = tofINLCorr->GetNRows();
  if(numRows>mNTDIGMAX*mNChanOnTDIG) {
    LOG_INFO << " !!! # of Rows in tofINLCorr table exceed the array limit in this function !!! Trancated !!! " << endm;
  }
  Int_t NTdig = 0;
  Int_t tdigId_old = 0;
  for (Int_t i=0;i<numRows;i++) {
    if(NTdig>mNTDIGMAX) {
      LOG_INFO << " !!! # of boards read-in exceeds the array limit in this function !!! Trancated !!! " << endm;
      NTdig = mNTDIGMAX;
      break;
    }

    int tdigId = (Int_t)(inlcorr[i].tdigId);
    int tdcChanId = (Int_t)(inlcorr[i].tdcChanId);
    if(tdigId!=tdigId_old) {
      mBoardId[NTdig] = tdigId;
      NTdig++;
    }
        
    tdigId_old = tdigId;

    if(maker->Debug()) LOG_INFO << " tdigId=" << tdigId << "  tdcChanId=" << tdcChanId << endm;
    for(Int_t j=0;j<mNChanMAX;j++) {
      float corr = (Float_t)(inlcorr[i].INLCorr[j]);
      mINLCorr[NTdig-1][tdcChanId][j] = corr;
      
      if(maker->Debug()&&(j%200==0)) {
	LOG_INFO << " " << corr;
      }
    }
    LOG_INFO << endm;
  }

  LOG_INFO << " Total # of boards read in : " << NTdig << endm;

  // re-organize
  for(Int_t i=0;i<NTdig;i++) {
    int boardId = mBoardId[i];
    if(boardId>0 && boardId<=mNBoardIdMAX) {
      mBoardId2Index[boardId] = i;
    } else {
      LOG_INFO << " Warning! boardId " << boardId << " out of range!" << endm;
    }
  }


}

void StTofINLCorr::Reset() {

  for(Int_t i=0;i<mNTray;i++) {
    for(Int_t j=0;j<mNTDIGOnTray;j++) {
      mTdigOnTray[i][j] = 0;
    }
  }
  for(Int_t i=0;i<mNTDIGOnTray;i++) {
    mTdigOnEastVpd[i] = 0;
    mTdigOnWestVpd[i] = 0;
  }

  for(Int_t i=0;i<mNTDIGMAX;i++) {
    mBoardId[i] = 0;
    for(int j=0;j<mNChanOnTDIG;j++) {
      for(int k=0;k<mNChanMAX;k++) {
	mINLCorr[i][j][k] = 0.0;
      }
    }
  }
  for(Int_t i=0;i<mNBoardIdMAX;i++) {
    mBoardId2Index[i] = -1;
  }

}

float StTofINLCorr::getTrayINLCorr(int trayId, int globalTdcChan, int bin) {
  if(trayId<=0 || trayId>mNTray) return 0.0;  // trayId 1-120
  if(globalTdcChan<0 || globalTdcChan>=mNGLOBALCHANMAX) return 0.0;
  if(bin<0 || bin>=mNChanMAX) return 0.0;

  int iTdig = globalTdcChan/mNChanOnTDIG;  // 0-7
  int boardId = mTdigOnTray[trayId-1][iTdig];
  if(boardId<=0 || boardId>mNBoardIdMAX) return 0.0;

  int index = mBoardId2Index[boardId];          // index in the inl array
  int tdcChan = globalTdcChan % mNChanOnTDIG;   // 0-23
  return mINLCorr[index][tdcChan][bin];
}

float StTofINLCorr::getVpdINLCorr(int ewId, int globalTdcChan, int bin) {
  if(ewId!=1 && ewId!=2) return 0.0;
  if(globalTdcChan<0 || globalTdcChan>=mNGLOBALCHANMAX) return 0.0;
  if(bin<0 || bin>=mNChanMAX) return 0.0;
  int iTdig = globalTdcChan/mNChanOnTDIG;  // 0-7

  int boardId;
  if(ewId==1) { // east pvpd
    boardId = mTdigOnEastVpd[iTdig];
  } else {  // west pvpd
    boardId = mTdigOnWestVpd[iTdig];
  }
  if(boardId<=0 || boardId>mNBoardIdMAX) return 0.0;

  int index = mBoardId2Index[boardId];          // index in the inl array
  int tdcChan = globalTdcChan % mNChanOnTDIG;   // 0-23
  return mINLCorr[index][tdcChan][bin];
}
