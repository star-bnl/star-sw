/***************************************************************************
 *
 * $Id: StHit.h,v 2.3 2000/06/07 09:43:21 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StHit.h,v $
 * Revision 2.3  2000/06/07 09:43:21  ullrich
 * Changed return type of flag() to UInt_t
 *
 * Revision 2.2  2000/06/01 21:38:56  ullrich
 * Added member mFlag and access member flag() and setFlag().
 *
 * Revision 2.1  1999/10/28 22:25:50  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:42:21  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#ifndef StHit_hh
#define StHit_hh

#include "StMeasuredPoint.h"
#include "StEnumerations.h"
#include "StContainers.h"

class StTrackNode;
class StTrack;

class StHit : public StMeasuredPoint {
public:
    StHit();
    StHit(const StThreeVectorF&,
          const StThreeVectorF&,
          ULong_t, Float_t, UChar_t = 0);
    // StHit(const StHit&);            use default
    // StHit& operator=(const StHit&); use default
    ~StHit();

    Int_t operator==(const StHit&) const;
    Int_t operator!=(const StHit&) const;
    
    Float_t         charge() const;
    UInt_t          trackReferenceCount() const;
    StDetectorId    detector() const;
    UInt_t          flag() const;
    StThreeVectorF  positionError() const;     // overwrite inherited
    StMatrixF       covariantMatrix() const;   // overwrite inherited
    
    void setCharge(Float_t);
    void setFlag(UChar_t);
    void setTrackReferenceCount(UChar_t);
    void setHardwarePosition(ULong_t);
    void setPositionError(const StThreeVectorF&);
    
    virtual StPtrVecTrack relatedTracks(const StSPtrVecTrackNode&, StTrackType);
    
protected:
    ULong_t bits(UInt_t, UInt_t) const;
    
    ULong_t        mHardwarePosition;
    Float_t        mCharge;
    UChar_t        mTrackRefCount;
    StThreeVectorF mPositionError;
    UChar_t        mFlag;

    StObject* clone();
    ClassDef(StHit,1)
};

inline ULong_t StHit::bits(UInt_t bit, UInt_t nbits) const
{
    return (mHardwarePosition>>bit) & ~(~0UL<<nbits);
}
#endif
