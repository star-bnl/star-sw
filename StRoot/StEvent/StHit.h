/***************************************************************************
 *
 * $Id: StHit.h,v 1.3 1999/04/27 01:24:21 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 *
 * History:
 * 15/01/1999 T. Wenaus  Add table-based constructor
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StHit.h,v $
 * Revision 1.3  1999/04/27 01:24:21  fisyak
 * Fix intermidaiate version with pointer instead of referencies
 *
 * Revision 1.5  1999/04/19 20:46:06  ullrich
 * Made virtual class
 *
 * Revision 1.4  1999/03/23 21:51:47  ullrich
 * Removed table-based constructor.
 *
 * Revision 1.3  1999/01/30 23:03:13  wenaus
 * table load intfc change; include ref change
 *
 * Revision 1.2  1999/01/15 22:53:46  wenaus
 * version with constructors for table-based loading
 *
 * Revision 2.2  2000/06/01 21:38:56  ullrich
#include "TObject.h"
 * Revision 2.1  1999/10/28 22:25:50  ullrich
#include "StObject.h"
class StHit : public TObject {
#include "StThreeVectorF.hh"
    StHit();
    StHit(const StThreeVectorF&,
	  const StThreeVectorF&,
	  Float_t, UChar_t = 0);
    // StHit(const StSvtHit&);                  use default
    // const StHit & operator=(const StHit&);   use default
    ~StHit();
  // StHit(const StSvtHit&);                  use default
    Int_t operator==(const StHit&) const;
    Int_t operator!=(const StHit&) const;
    
    const StThreeVectorF& position() const;
    const StThreeVectorF& positionError() const;
    Float_t                       charge() const;
    UChar_t               trackReferenceCount() const;	

    void setPosition(const StThreeVectorF&);
    void setPositionError(const StThreeVectorF&);
    void setCharge(Float_t);
    void setTrackReferenceCount(UChar_t);
  virtual void setPositionError(const StThreeVectorF&);
  virtual void setCharge(Float_t);
    StThreeVectorF mPosition;
    StThreeVectorF mPositionError;
    Float_t                mCharge;
    UChar_t        mTrackRefCount;    
    ClassDef(StHit,1)  //StHit structure
  Float_t                mCharge;
#ifndef __CINT__
  ClassDef(StHit,1)  //StHit structure
protected:
    
ostream&  operator<<(ostream& os, const StHit&);

inline const StThreeVectorF& StHit::position() const { return mPosition; }

inline const StThreeVectorF& StHit::positionError() const { return mPositionError; }

#endif

inline UChar_t StHit::trackReferenceCount() const { return mTrackRefCount; }	


    StObject* clone();
    ClassDef(StHit,1)
};

inline ULong_t StHit::bits(UInt_t bit, UInt_t nbits) const
{
    return (mHardwarePosition>>bit) & ~(~0UL<<nbits);
}
#endif
