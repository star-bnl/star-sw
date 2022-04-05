/*******************************************************************
 *
 * $Id: StBTofDaqMap.h,v 1.3 2017/10/20 17:50:33 smirnovd Exp $
 *
 * Author: Xin Dong
 *****************************************************************
 *
 * Description: (1) Mapping between Daq channel numbers and Cell numbers
 *              (2) Parameters initalize from dbase
 *
 *****************************************************************
 *
 * $Log: StBTofDaqMap.h,v $
 * Revision 1.3  2017/10/20 17:50:33  smirnovd
 * Squashed commit of the following:
 *
 *     StBTof: Remove outdated ClassImp macro
 *
 *     Prefer explicit namespace for std:: names in header files
 *
 *     Removed unnecessary specification of default std::allocator
 *
 * Frank signed-off
 *
 * Revision 1.2  2009/02/13 19:47:33  dongx
 * mNValidTrays set by the tofTrayConfig in db now
 *
 * Revision 1.1  2009/02/02 21:56:34  dongx
 * first release - Barrel TOF daq mapping
 *
 *
 *******************************************************************/
#ifndef STBTOFDAQMAP_H
#define STBTOFDAQMAP_H

#include "StObject.h"
#include "StMaker.h"
#include <assert.h>
#include "Stypes.h"
#include "TDataSet.h"
#include "TDataSetIter.h"
#include "TObjectSet.h"
#include <string>

#include <vector>

typedef std::vector<Int_t>  IntVec;

/**
   \class StBTofDaqMap
   Class to retrieve the daq map from data base
 */
class StBTofDaqMap{
 private:
  static const Int_t mDAQOVERFLOW = 255;   // daq max channel #
  static const Int_t mNTOF = 192;    // 192 for tof in Run 8++
  static const Int_t mNTray = 120;     // # of trays
  static const Int_t mNModule = 32;  // 32 for tofr5++ 
  static const Int_t mNCell = 6;
  static const Int_t mNVPD = 19;    // 19 on each side

  Int_t mNValidTrays;
  
  Int_t mMRPC2TDIGChan[mNTOF]; /// tdc channel # of MRPC channel
  Int_t mTDIG2MRPCChan[mNTOF]; /// MRPC channel # of tdc channel
  Int_t mEastPMT2TDIGLeChan[mNVPD], mEastPMT2TDIGTeChan[mNVPD]; /// tdc channel of vpd PMTs
  Int_t mTDIGLe2EastPMTChan[mNTOF], mTDIGTe2EastPMTChan[mNTOF]; /// vpd PMT tube of tdc channels
  Int_t mWestPMT2TDIGLeChan[mNVPD], mWestPMT2TDIGTeChan[mNVPD]; /// tdc channel of vpd PMTs
  Int_t mTDIGLe2WestPMTChan[mNTOF], mTDIGTe2WestPMTChan[mNTOF]; /// vpd PMT tube of tdc channels

  /// Valid tray Ids
  Int_t mValidTrayId[mNTray];

 public:
  /// Default constructor
  StBTofDaqMap();
  ~StBTofDaqMap();

  /// Initial function, need a maker to access the data base
  void Init(StMaker *maker);
  void Reset();
  
  /// Set the number valid trays
  void setNValidTrays(int ntrays);

  /// To convert TDIG channel number to module/cell number
  IntVec TDIGChan2Cell( const Int_t iTdc );
  /// To convert module/cell number to TDIG channel number
  Int_t Cell2TDIGChan( const Int_t iModule, const Int_t iCell );
  
  /// The following VPD functions considering the asymmetric cabling in E/W VPDs
  /// To convert east VPD PMT number to TDIG leading channel number
  Int_t EastPMT2TDIGLeChan( const Int_t iTube );
  /// To convert TDIG leading channel number to east VPD PMT number
  Int_t TDIGLeChan2EastPMT( const Int_t iTdc );
  /// To convert east VPD PMT number to TDIG trailing channel number
  Int_t EastPMT2TDIGTeChan( const Int_t iTube );
  /// To convert TDIG trailing channel number to east VPD PMT number  
  Int_t TDIGTeChan2EastPMT( const Int_t iTdc );
  /// To convert west VPD PMT number to TDIG leading channel number
  Int_t WestPMT2TDIGLeChan( const Int_t iTube );
  /// To convert TDIG leading channel number to west VPD PMT number
  Int_t TDIGLeChan2WestPMT( const Int_t iTdc );
  /// To convert west VPD PMT number to TDIG trailing channel number
  Int_t WestPMT2TDIGTeChan( const Int_t iTube );
  /// To convert TDIG trailing channel number to west VPD PMT number  
  Int_t TDIGTeChan2WestPMT( const Int_t iTdc );

  /// Returns the list of valid tray Ids
  IntVec ValidTrays();
  /// Returns the number of valid trays
  Int_t  numberOfValidTrays();
};

inline void StBTofDaqMap::setNValidTrays(int ntrays) { mNValidTrays = ntrays; }
inline Int_t StBTofDaqMap::numberOfValidTrays() { return mNValidTrays; }
#endif
