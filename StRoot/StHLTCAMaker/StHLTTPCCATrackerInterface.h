#ifndef __StHLTTPCCATrackerInterface_h__
#define __StHLTTPCCATrackerInterface_h__
#include "Sti/StiTPCCATrackerInterface.h"

#include <stdio.h>

// #define KEHW_DEBUG

class online_tracking_TpcHitMap;

class StHLTTPCCATrackerInterface : public StiTPCCATrackerInterface {
public:
  // Instance
  static StHLTTPCCATrackerInterface &Instance();
  
  // constructor - destructor
  StHLTTPCCATrackerInterface();
  ~StHLTTPCCATrackerInterface();
  virtual void MakeHits();     // fill fCaHits & fSeedHits

private:
#ifdef KEHW_DEBUG
  FILE* hit_info;
#endif // KEHW_DEBUG
  static const int nTpcSectors = 24;
  online_tracking_TpcHitMap* hitMap[nTpcSectors]; // STAR HLT TPC hit map 
  void HLTHitG2L(const int iSector, const int iPadrow, const double globalXyz[3], double HLTLocalXyz[3]);
};
#endif //  __StHLTTPCCATrackerInterface_h__
