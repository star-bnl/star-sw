/***************************************************************************
 *
 * $Id: StTofHit.h,v 2.2 2001/03/24 03:34:59 perev Exp $
 *
 * Author: Wei-Ming Zhang, Dec 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 *
 * $Log: StTofHit.h,v $
 * Revision 2.2  2001/03/24 03:34:59  perev
 * clone() -> clone() const
 *
 * Revision 2.1  2000/12/21 23:52:25  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StTofHit_hh
#define StTofHit_hh

#include <iostream.h>
#include "StHit.h"

class StTofHit : public StHit {
public:
    StTofHit();
    
    //StTofHit(const StTofHit&);
    //StTofHit& operator=(const StTofHit&);
    ~StTofHit();

    UShort_t slatIndex()      const;
    UShort_t numberOfMips()   const;
    Float_t  flightTime()     const;

    void setSlatIndex(UShort_t);
    void setNumberOfMips(UShort_t);
    void setFlightTime(Float_t);
    
protected:
    StObject* clone() const;

    UShort_t                  mSlatIndex;
    UShort_t                  mNumberOfMips;
    Float_t                   mFlightTime;

    ClassDef(StTofHit,1)
};

inline UShort_t
StTofHit::slatIndex() const { return mSlatIndex; }

inline UShort_t
StTofHit::numberOfMips() const {return mNumberOfMips;}

inline Float_t
StTofHit::flightTime() const {return mFlightTime;}

inline void
StTofHit::setSlatIndex(UShort_t slatId) {mNumberOfMips = slatId;}

inline void
StTofHit::setNumberOfMips(UShort_t nm) {mNumberOfMips = nm;}

inline void
StTofHit::setFlightTime(Float_t tof) {mFlightTime = tof;}

//non-members
ostream& operator<<(ostream&, const StTofHit&);

#endif
