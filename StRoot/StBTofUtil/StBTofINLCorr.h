/*******************************************************************
 *
 * $Id: StBTofINLCorr.h,v 1.1 2009/02/02 21:57:51 dongx Exp $
 *
 * Author: Xin Dong
 *****************************************************************
 *
 * Description: INL correction for all TDIG board channels
 *
 *****************************************************************
 *
 * $Log: StBTofINLCorr.h,v $
 * Revision 1.1  2009/02/02 21:57:51  dongx
 * first release - Barrel TOF INL correction functions
 *
 *
 *******************************************************************/
#ifndef STBTOFINLCORR_H
#define STBTOFINLCORR_H

#include "StObject.h"
#include "StMaker.h"
#include <assert.h>
#include "Stypes.h"
#include "TDataSet.h"
#include "TDataSetIter.h"
#include "TObjectSet.h"
#include <string>
#include "StEnumerations.h"

/**
   \class StBTofINLCorr
   Class to retrieve the INL tables from data base
 */
class StBTofINLCorr{
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
  StBTofINLCorr();
  ~StBTofINLCorr();

  void init();
  void init(StMaker *maker);
  /// Initial function to access the data base and retrieve INL tables
  void initFromDbase(StMaker *maker);
  void Reset();

  /// To get the INL correction tables for trays
  float getTrayINLCorr(int trayId, int globalTdcChan, int bin);
  /// To get the INL correction tables for vpds
  float getVpdINLCorr(StBeamDirection eastwest, int globalTdcChan, int bin);
  
  void setNValidTrays(int ntrays);
};

inline void StBTofINLCorr::setNValidTrays(int ntrays) { mNValidTrays = ntrays; }

#endif
