/*******************************************************************
 *
 * $Id: StTofrDaqMap.h,v 1.7 2008/03/27 00:15:39 dongx Exp $
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
 * Revision 1.7  2008/03/27 00:15:39  dongx
 *  Update for Run8 finished.
 *
 * Revision 1.6  2007/11/22 00:04:13  dongx
 * - update for tof8++
 * - added ValidTrays() function
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
  static const Int_t mNTray = 120;     // # of trays
  static const Int_t mNModule = 32;  // 32 for tofr5++ 
  static const Int_t mNCell = 6;
  static const Int_t mNVPD = 19;    // 19 on each side

  Int_t mNValidTrays;
  
  Int_t mTrayId[mNTOFR], mModuleId[mNTOFR], mCellId[mNTOFR];
  Int_t mAdc[mNTOFR], mTdc[mNTOFR];

  // new arrays for tofr5
  Int_t mGlobalTDCChan[mNTOFR5], mGlobalModuleChan[mNTOFR5];

  // General arrays for tof8++
  Int_t mMRPC2TDIGChan[mNTOF]; // tdc channel # of MRPC channel
  Int_t mTDIG2MRPCChan[mNTOF]; // MRPC channel # of tdc channel
  Int_t mEastPMT2TDIGLeChan[mNVPD], mEastPMT2TDIGTeChan[mNVPD]; // tdc channel of vpd PMTs
  Int_t mTDIGLe2EastPMTChan[mNTOF], mTDIGTe2EastPMTChan[mNTOF]; // vpd PMT tube of tdc channels
  Int_t mWestPMT2TDIGLeChan[mNVPD], mWestPMT2TDIGTeChan[mNVPD]; // tdc channel of vpd PMTs
  Int_t mTDIGLe2WestPMTChan[mNTOF], mTDIGTe2WestPMTChan[mNTOF]; // vpd PMT tube of tdc channels

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
  
  void setNValidTrays(int ntrays);

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
  Int_t EastPMT2TDIGLeChan( const Int_t iTube );
  Int_t TDIGLeChan2EastPMT( const Int_t iTdc );
  Int_t EastPMT2TDIGTeChan( const Int_t iTube );
  Int_t TDIGTeChan2EastPMT( const Int_t iTdc );
  Int_t WestPMT2TDIGLeChan( const Int_t iTube );
  Int_t TDIGLeChan2WestPMT( const Int_t iTdc );
  Int_t WestPMT2TDIGTeChan( const Int_t iTube );
  Int_t TDIGTeChan2WestPMT( const Int_t iTdc );

  IntVec ValidTrays();
};

inline void StTofrDaqMap::setNValidTrays(int ntrays) { mNValidTrays = ntrays; }

#endif
