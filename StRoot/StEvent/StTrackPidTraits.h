/***************************************************************************
 *
 * $Id: StTrackPidTraits.h,v 1.2 1999/02/09 23:11:18 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackPidTraits.h,v $
 * Revision 1.2  1999/02/09 23:11:18  fisyak
 * Torre stuff
 *
 * Revision 1.3  1999/04/08 15:01:18  ullrich
 * Added dE/dx PID from TPC.
 *
 * Revision 1.2  1999/01/15 22:54:10  wenaus
 * version with constructors for table-based loading
 *
 * Revision 2.2  1999/11/15 18:48:28  ullrich
#ifdef __ROOT__
#include "TObject.h"
#endif
class StTrackPidTraits : public TObject {
 * Completely Revised for New Version
//     StTrackPidTraits();
//     StTrackPidTraits(const StTrackPidTraits&);
//     ~StTrackPidTraits();
//     const StTrackPidTraits & operator=(const StTrackPidTraits&);
//     Int_t operator==(const StTrackPidTraits&) const;
//     Int_t operator!=(const StTrackPidTraits&) const;
protected:
    StTrackPidTraits();
#ifdef __ROOT__
	ClassDef(StTrackPidTraits,1)  //StTrackPidTraits structure
#endif
  StDedxPid *mTpcDedxPid;
    StDedxMethod method() const;

inline const StDedxPid* StTrackPidTraits::tpcDedxPid() const {return mTpcDedxPid;}

    

protected:
    Short_t mDetectorId;
    Short_t mMethod;

    virtual StObject* clone() = 0;
    ClassDef(StTrackPidTraits,1)
};
#endif
