/***************************************************************************
 *
 * $Id: StHit.h,v 1.4 1999/04/28 22:27:33 fisyak Exp $
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
 * Revision 1.4  1999/04/28 22:27:33  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.4  1999/04/28 22:27:33  fisyak
 * New version with pointer instead referencies
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
 *
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
    virtual ~StHit();
  // StHit(const StSvtHit&);                  use default
    Int_t operator==(const StHit&) const;
    Int_t operator!=(const StHit&) const;
    
    virtual const StThreeVectorF& position() const;
    virtual const StThreeVectorF& positionError() const;
    virtual Float_t                       charge() const;
    virtual UChar_t               trackReferenceCount() const;	
  virtual Float_t                       charge() const;
    virtual void setPosition(const StThreeVectorF&);
    virtual void setPositionError(const StThreeVectorF&);
    virtual void setCharge(Float_t);
    virtual void setTrackReferenceCount(UChar_t);
  virtual void setPositionError(const StThreeVectorF&);
  virtual void setCharge(Float_t);
    StThreeVectorF mPosition;
    StThreeVectorF mPositionError;
    Float_t                mCharge;
    UChar_t        mTrackRefCount;    
  StThreeVectorF mPositionError;
  Float_t                mCharge;
  UChar_t        mTrackRefCount;    
  ClassDef(StHit,1)  //StHit structure
protected:
    
ostream&  operator<<(ostream& os, const StHit&);

inline const StThreeVectorF& StHit::position() const { return mPosition; }

inline const StThreeVectorF& StHit::positionError() const { return mPositionError; }

inline Float_t StHit::charge() const { return mCharge; }

inline UChar_t StHit::trackReferenceCount() const { return mTrackRefCount; }	


    StObject* clone();
    ClassDef(StHit,1)
};

inline ULong_t StHit::bits(UInt_t bit, UInt_t nbits) const
{
    return (mHardwarePosition>>bit) & ~(~0UL<<nbits);
}
#endif
