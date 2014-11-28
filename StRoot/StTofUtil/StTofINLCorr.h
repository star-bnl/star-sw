/*******************************************************************
 *
 * $Id: StTofINLCorr.h,v 1.4 2008/03/27 00:15:38 dongx Exp $
 *
 * Author: Xin Dong
 *****************************************************************
 *
 * Description: INL correction for all TDIG board channels
 *
 *****************************************************************
 *
 * $Log: StTofINLCorr.h,v $
 * Revision 1.4  2008/03/27 00:15:38  dongx
 *  Update for Run8 finished.
 *
 * Revision 1.3  2007/11/29 22:39:30  dongx
 * vpd trayId changed to 121 (East) and 122 (West), be consistent
 *
 * Revision 1.2  2007/11/22 00:04:13  dongx
 * - update for tof8++
 * - added ValidTrays() function
 *
 *
 *******************************************************************/
#ifndef STTOFINLCORR_H
#define STTOFINLCORR_H

#include "StObject.h"
#include "StMaker.h"
#include <assert.h>
#include "Stypes.h"
#include "TDataSet.h"
#include "TDataSetIter.h"
#include "TObjectSet.h"
#include <string>


class StTofINLCorr{
 private:
  static const Int_t mNTray = 120;
  static const Int_t mNTDIGOnTray = 8;
  static const Int_t mNGLOBALCHANMAX = 192;

  static const Int_t mNTDIGMAX = 1200;
  static const Int_t mNChanOnTDIG = 24;
  static const Int_t mNChanMAX = 1024;
  static const Int_t mNBoardIdMAX = 4800;

  static const Int_t mEastVpdTrayId = 122;
  static const Int_t mWestVpdTrayId = 121;

  Int_t mTdigOnTray[mNTray][mNTDIGOnTray];
  Int_t mTdigOnEastVpd[mNTDIGOnTray];
  Int_t mTdigOnWestVpd[mNTDIGOnTray];

  Int_t mBoardId[mNTDIGMAX];
  Int_t mBoardId2Index[mNBoardIdMAX];   // index in mNTDIGMAX for board #Id
  Float_t mINLCorr[mNTDIGMAX][mNChanOnTDIG][mNChanMAX];

  Int_t mNValidTrays;
  
 public:
  StTofINLCorr();
  ~StTofINLCorr();

  void init();
  void init(StMaker *maker);
  void initFromDbase(StMaker *maker);
  void Reset();

  float getTrayINLCorr(int trayId, int globalTdcChan, int bin);
  float getVpdINLCorr(int ewId, int globalTdcChan, int bin);
  
  void setNValidTrays(int ntrays);
};

inline void StTofINLCorr::setNValidTrays(int ntrays) { mNValidTrays = ntrays; }

#endif
