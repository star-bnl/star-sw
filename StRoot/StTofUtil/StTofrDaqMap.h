/*******************************************************************
 *
 * $Id: StTofrDaqMap.h,v 1.5 2007/11/21 19:31:29 dongx Exp $
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
 * Revision 1.5  2007/11/21 19:31:29  dongx
 * added ValidTrays() for multi-tray system
 *
 * Revision 1.3  2005/04/12 17:23:15  dongx
 * Update for year 5 new data format, writter by Jing Liu
 *
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
  static const Int_t mNTOFR5 = 192;   // 192 for tofr5

  static const Int_t mNTOF = 192;    // 192 for tof in Run 8++
  static const Int_t mNTray = 5;     // # of valid trays
  static const Int_t mNModule = 32;  // 32 for tofr5++ 
  static const Int_t mNCell = 6;
  static const Int_t mNVPD = 19;    // 19 on each side

  Int_t mTrayId[mNTOFR], mModuleId[mNTOFR], mCellId[mNTOFR];
  Int_t mAdc[mNTOFR], mTdc[mNTOFR];

  // new arrays for tofr5
  Int_t mGlobalTDCChan[mNTOFR5], mGlobalModuleChan[mNTOFR5];

  // General arrays for tof8++
  Int_t mMRPC2TDIGChan[mNTOF]; // tdc channel # of MRPC channel
  Int_t mTDIG2MRPCChan[mNTOF]; // MRPC channel # of tdc channel
  Int_t mPMT2TDIGLeChan[mNVPD], mPMT2TDIGTeChan[mNVPD]; // tdc channel of vpd PMTs
  Int_t mTDIGLe2PMTChan[mNTOF], mTDIGTe2PMTChan[mNTOF]; // vpd PMT tube of tdc channels

  // Valid tray Ids
  Int_t mValidTrayId[mNTray];

 public:
  StTofrDaqMap();
  ~StTofrDaqMap();

  void init();
  void init(StMaker *maker);
  void initFromDbase(StMaker *maker);
  void initFromDbaseY5(StMaker *maker);    // tofr5 
  void initFromDbaseGeneral(StMaker *maker);    // tof8++, general
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

  // tofr5 interface, Jing Liu
  IntVec Tofr5TDCChan2Cell( const Int_t iTdc );
  Int_t Tofr5Cell2TDCChan( const Int_t iTray, const Int_t iModule, const Int_t iCell );

  // general interface, tray number obseleted
  IntVec TDIGChan2Cell( const Int_t iTdc );
  Int_t Cell2TDIGChan( const Int_t iModule, const Int_t iCell );
  Int_t PMT2TDIGLeChan( const Int_t iTube );
  Int_t TDIGLeChan2PMT( const Int_t iTdc );
  Int_t PMT2TDIGTeChan( const Int_t iTube );
  Int_t TDIGTeChan2PMT( const Int_t iTdc );

  IntVec ValidTrays();
};

#endif
