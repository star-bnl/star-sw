/***************************************************************************
 *
 * $Id: StHit.hh,v 1.3 1999/01/30 23:03:13 wenaus Exp $
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
 * $Log: StHit.hh,v $
 * Revision 1.3  1999/01/30 23:03:13  wenaus
 * table load intfc change; include ref change
 *
 * Revision 1.3  1999/01/30 23:03:13  wenaus
 * table load intfc change; include ref change
 *
 * Revision 1.2  1999/01/15 22:53:46  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StHit_hh
#define StHit_hh

#include <iostream.h>
#include "StThreeVector.hh"
#include "tables/dst_point.h"

class StHit {
public:
    StHit();
    StHit(const StThreeVector<float>&,
	  const StThreeVector<float>&,
	  float, unsigned char = 0);
    StHit(dst_point_st*);
    // StHit(const StSvtHit&);                  use default
    // const StHit & operator=(const StHit&);   use default
    ~StHit();
    
    int operator==(const StHit&) const;
    int operator!=(const StHit&) const;
    
    const StThreeVector<float>& position() const;
    const StThreeVector<float>& positionError() const;
    float                       charge() const;
    unsigned char               trackReferenceCount() const;	

    void setPosition(const StThreeVector<float>&);
    void setPositionError(const StThreeVector<float>&);
    void setCharge(float);
    void setTrackReferenceCount(unsigned char);
    
protected:
    StThreeVector<float> mPosition;
    StThreeVector<float> mPositionError;
    float                mCharge;
    unsigned char        mTrackRefCount;    
};

ostream&  operator<<(ostream& os, const StHit&);

inline const StThreeVector<float>& StHit::position() const { return mPosition; }

inline const StThreeVector<float>& StHit::positionError() const { return mPositionError; }

inline float StHit::charge() const { return mCharge; }

inline unsigned char StHit::trackReferenceCount() const { return mTrackRefCount; }	

#endif
