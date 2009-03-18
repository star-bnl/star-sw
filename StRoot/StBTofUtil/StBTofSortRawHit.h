/*******************************************************************
 *
 * $Id: StBTofSortRawHit.h,v 1.3 2009/03/18 20:14:10 dongx Exp $
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
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

#if !defined(ST_NO_TEMPLATE_DEF_ARGS) || defined(__CINT__)
//#ifndef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<Int_t>  IntVec;
typedef vector<UInt_t>  UIntVec;
#else
typedef vector<Int_t, allocator<Int_t>>  IntVec;
typedef vector<UInt_t, allocator<UInt_t>>  UIntVec;
#endif

struct TOFRawHit {
  int fiberId;
  int tray;
  int channel;
  UIntVec leadingTdc;
  UIntVec trailingTdc;
};

#ifndef ST_NO_TEMPLATE_DEF_ARGS
  typedef vector<TOFRawHit> tofRawHitVector;
#else
  typedef vector<TOFRawHit,allocator<TOFRawHit>> tofRawHitVector;
#endif
  typedef vector<TOFRawHit>::iterator tofRawHitVectorIter;

/**
   \class StBtofSortRawHit
   Class to sort the BTofRawHit vector. Raw hits will be grouped for each tray/channel.
   The leading and trailing tdcs are grouped together as well.
 */ 
class StBTofSortRawHit : public StObject {
 private:
  static const Int_t mNTRAY = 122;
  static const Int_t mNCHAN = 192;
  static const Int_t mNFIBER = 4;
  tofRawHitVector mRawHitVec[mNTRAY];
  
  Float_t mTriggerTimeWindow[mNTRAY][2];    //
  Float_t mTriggerOffset;                   //
  UInt_t  mTriggerTime[mNFIBER];            //

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
  
  void setBTofCollection(StBTofCollection* tofColl);

  /// Returns the valid channel Ids for a tray
  IntVec GetValidChannel(int tray);
  /// Returns the leading Tdcs for one channel. triggerevent used as a switch for physical hits selection
  UIntVec GetLeadingTdc(int tray, int channel, bool triggerevent=true);
  /// Returns the trailing Tdcs for one channel. triggerevent used as a switch for physical hits selection  
  UIntVec GetTrailingTdc(int tray, int channel, bool triggerevent=true);

};
#endif
