#ifndef __StMuMtdHeader_hh__
#define __StMuMtdHeader_hh__

#include "TObject.h"

using namespace std;

class StMtdHeader;

class StMuMtdHeader : public TObject {

 public:

  StMuMtdHeader();
  StMuMtdHeader(const StMtdHeader* header);
  ~StMuMtdHeader() {; }

  short          fiberHeader(int fiberId) const;
  unsigned int   fiberTriggerWord(int fiberId) const;
  unsigned int   triggerTime(int fiberId) const;
  int            shouldHaveRejectEvent() const;
  unsigned int   tpcSectorMask() const;
    
 protected:
  short mFiberHeader1;
  short mFiberHeader2;
  unsigned int mFiberTriggerWord1;
  unsigned int mFiberTriggerWord2;
  unsigned int mTriggerTime1;
  unsigned int mTriggerTime2;
  int          mShouldHaveRejectEvent; // indication of event status in filtering
                                       // 0 - events not triggered di-muon
                                       // 1 - events should have been rejected 
                                       // if only triggered by di-muon
                                       // 2 - events pass filtering cuts
  unsigned int mTpcSectorMask;         // Mask of TPC sectors for tracking in the first iteration
    
  ClassDef(StMuMtdHeader,2)

};

#endif
