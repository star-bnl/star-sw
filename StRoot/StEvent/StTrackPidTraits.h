/***************************************************************************
 *
 * $Id: StTrackPidTraits.h,v 1.6 1999/05/02 00:00:18 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackPidTraits.h,v $
 * Revision 1.6  1999/05/02 00:00:18  fisyak
 * Add default ctors
 *
 * Revision 1.6  1999/05/02 00:00:18  fisyak
 * Add default ctors
 *
 * Revision 1.5  1999/04/30 13:16:30  fisyak
 * add StArray for StRootEvent
 *
 * Revision 1.4  1999/04/28 22:27:38  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.3  1999/04/08 15:01:18  ullrich
 * Added dE/dx PID from TPC.
 *
 * Revision 1.2  1999/01/15 22:54:10  wenaus
 * version with constructors for table-based loading
 *
 * Revision 2.2  1999/11/15 18:48:28  ullrich
 * Adapted new enums for dedx and track reco methods.
 *
 *
class StGlobalTrack;
class StDedxPid;
 * Completely Revised for New Version
#include "StObject.h"
 *
 **************************************************************************/
  StTrackPidTraits() : mTpcDedxPid() {/* noop */};
  StTrackPidTraits(StGlobalTrack*);
  ~StTrackPidTraits();
    StTrackPidTraits();
  const StDedxPid* tpcDedxPid() const;
    Short_t method() const;
private:
  StDedxPid *mTpcDedxPid;
  // StDedxPid *mSvtDedxPid;  
  ClassDef(StTrackPidTraits,1)  //StTrackPidTraits structure
    StDedxMethod method() const;

inline const StDedxPid* StTrackPidTraits::tpcDedxPid() const {return mTpcDedxPid;}

    

protected:
    Short_t mDetectorId;
    Short_t mMethod;

    virtual StObject* clone() = 0;
    ClassDef(StTrackPidTraits,1)
};
#endif
