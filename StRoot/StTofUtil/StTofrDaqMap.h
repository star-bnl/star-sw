/*******************************************************************
 *
 * $Id: StTofrDaqMap.h,v 1.2 2004/03/09 17:43:04 dongx Exp $
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
 * $Log: StTofrDaqMap.h,v $
 * Revision 1.2  2004/03/09 17:43:04  dongx
 * first release
 *
 *
 *******************************************************************/
#ifndef STTOFRDAQMAP_H
#define STTOFRDAQMAP_H

#include "StObject.h"
#include "StMaker.h"
#include <assert.h>
#include "Stypes.h"
#include "TDataSet.h"
#include "TDataSetIter.h"
#include "TObjectSet.h"
#include <string>

#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

#if !defined(ST_NO_TEMPLATE_DEF_ARGS) || defined(__CINT__)
//#ifndef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<Int_t>  IntVec;
#else
typedef vector<Int_t, allocator<Int_t>>  IntVec;
#endif

class StTofrDaqMap{
 private:
  static const Int_t mDAQOVERFLOW = 255;   // daq max channel #
  static const Int_t mNTOFR = 120;   // 72
  static const Int_t mNTray = 1;
  static const Int_t mNModule = 20;   // 12
  static const Int_t mNCell = 6;
  Int_t mTrayId[mNTOFR], mModuleId[mNTOFR], mCellId[mNTOFR];
  Int_t mAdc[mNTOFR], mTdc[mNTOFR];

 public:
  StTofrDaqMap();
  ~StTofrDaqMap();

  void init();
  void init(StMaker *maker);
  void initFromDbase(StMaker *maker);
  void Reset();

  IntVec DaqChan2Cell( const Int_t iTofrDaq );
  Int_t Cell2DaqChan( const Int_t iTray, const Int_t iModule, const Int_t iCell );
  IntVec ADCChan2Cell( const Int_t iAdc );
  IntVec TDCChan2Cell( const Int_t iTdc );
  Int_t Cell2ADCChan( const Int_t iTray, const Int_t iModule, const Int_t iCell );
  Int_t Cell2TDCChan( const Int_t iTray, const Int_t iModule, const Int_t iCell );
  Int_t DaqChan2ADCChan( const Int_t iTofrDaq );
  Int_t DaqChan2TDCChan( const Int_t iTofrDaq );
  Int_t ADCChan2DaqChan( const Int_t iAdc );
  Int_t TDCChan2DaqChan( const Int_t iTdc );

};

#endif
