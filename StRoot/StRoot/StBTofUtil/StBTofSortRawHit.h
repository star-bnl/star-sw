/*******************************************************************
 *
 * $Id: StBTofSortRawHit.h,v 1.7 2017/10/20 17:50:33 smirnovd Exp $
 *
 * Author: Xin Dong
 *****************************************************************
 *
 * Description:  Utilities to group the raw Tof hits for convenient use
 *
 *****************************************************************
 *
 *******************************************************************/

#ifndef StBTOFSORTRAWHIT_H
#define StBTOFSORTRAWHIT_H

class StMaker;
class StEvent;
class StBTofRawHit;
class StBTofCollection;
class StBTofHeader;
class StBTofRawHitCollection;
class StBTofDaqMap;

#include "StObject.h"
#include "TDataSet.h"
#include "TDataSetIter.h"
#include "TObjectSet.h"

#include <vector>

typedef std::vector<Int_t>  IntVec;
typedef std::vector<UInt_t>  UIntVec;

struct TOFRawHit {
  int fiberId;
  int tray;
  int channel;
  UIntVec leadingTdc;
  UIntVec trailingTdc;
};

  typedef std::vector<TOFRawHit> tofRawHitVector;
  typedef std::vector<TOFRawHit>::iterator tofRawHitVectorIter;

/**
   \class StBtofSortRawHit
   Class to sort the BTofRawHit vector. Raw hits will be grouped for each tray/channel.
   The leading and trailing tdcs are grouped together as well.
 */ 
class StBTofSortRawHit : public StObject {
 private:
  // increase #trays to 124 (120 TOF + 2 VPD + 1 MTD)
  static const Int_t mNTRAY = 124;
  static const Int_t mNCHAN = 192;
  static const Int_t mNFIBER = 4;
  static const Int_t mNVPD  = 19;
  tofRawHitVector mRawHitVec[mNTRAY];
  
  Float_t mTriggerTimeWindow[mNTRAY][2];    //
  Float_t mTriggerOffset;                   //
  UInt_t  mTriggerTime[mNFIBER];            //
  
  Float_t mVpdDelay[2*mNVPD];               //! VPD delays

  Bool_t  mDebug;                           //! switch for debugging 
  StBTofDaqMap     *mDaqMap;

 public:
  StBTofSortRawHit();
  ~StBTofSortRawHit();
  
  void Init();
  /// Initial function from StBTofCollection
  /// Need Daq Map for VPD Le/Te mapping
  void Init(StMaker *maker, StBTofDaqMap *daqMap);
  void Reset();
  /// set to use the VPD delays
  //void setVpdDelay(Int_t runnumber);
  
  void setBTofCollection(StBTofCollection* tofColl);

  /// Returns the valid channel Ids for a tray
  IntVec GetValidChannel(int tray);
  /// Returns the leading Tdcs for one channel. triggerevent used as a switch for physical hits selection
  UIntVec GetLeadingTdc(int tray, int channel, bool triggerevent=true);
  /// Returns the trailing Tdcs for one channel. triggerevent used as a switch for physical hits selection  
  UIntVec GetTrailingTdc(int tray, int channel, bool triggerevent=true);

};
#endif
