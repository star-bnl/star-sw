/*!
 * \class StTofHit 
 * \author Wei-Ming Zhang, Dec 2000
 */
/***************************************************************************
 *
 * $Id: StTofHit.h,v 2.5 2002/02/22 22:56:51 jeromel Exp $
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
 * Revision 2.5  2002/02/22 22:56:51  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.4  2001/04/16 20:49:00  ullrich
 * Fixed typo in setSlatIndex().
 *
 * Revision 2.3  2001/04/05 04:00:43  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
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

    unsigned short slatIndex()      const;
    unsigned short numberOfMips()   const;
    float          flightTime()     const;

    void setSlatIndex(unsigned short);
    void setNumberOfMips(unsigned short);
    void setFlightTime(float);
    
protected:
    StObject* clone() const;

    UShort_t  mSlatIndex;
    UShort_t  mNumberOfMips;
    Float_t   mFlightTime;

    ClassDef(StTofHit,1)
};

inline unsigned short
StTofHit::slatIndex() const { return mSlatIndex; }

inline unsigned short
StTofHit::numberOfMips() const {return mNumberOfMips;}

inline float
StTofHit::flightTime() const {return mFlightTime;}

inline void
StTofHit::setSlatIndex(unsigned short slatId) {mSlatIndex = slatId;}

inline void
StTofHit::setNumberOfMips(unsigned short nm) {mNumberOfMips = nm;}

inline void
StTofHit::setFlightTime(float tof) {mFlightTime = tof;}

//non-members
ostream& operator<<(ostream&, const StTofHit&);

#endif
