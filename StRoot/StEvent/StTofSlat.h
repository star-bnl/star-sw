/***************************************************************************
 *
 * $Id: StTofSlat.h,v 2.1 2000/12/21 23:52:23 ullrich Exp $
 *
 * Author: Wei-Ming Zhang, Dec 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 *
 * $Log: StTofSlat.h,v $
 * Revision 2.1  2000/12/21 23:52:23  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StTofSlat_hh
#define StTofSlat_hh

#include "StObject.h"

class StTofSlat : public StObject {
public:
    StTofSlat();
    StTofSlat(UShort_t, UShort_t, UShort_t);
    // StTofSlat(const StTofSlat&);            use default
    // StTofSlat& operator=(const StTofSlat&); use default
    ~StTofSlat();
    
    Int_t operator==(const StTofSlat&) const;
    Int_t operator!=(const StTofSlat&) const;

    UShort_t  slatIndex() const;
    UShort_t  adc() const;
    UShort_t  tdc() const;
    
    void      setSlatIndex(UShort_t);
    void      setAdc(UShort_t);
    void      setTdc(UShort_t);
    
protected:
    UShort_t  mSlatIndex;
    UShort_t  mAdc;
    UShort_t  mTdc;
    
    ClassDef(StTofSlat,1)
};

inline void
StTofSlat::setSlatIndex(UShort_t slatId)
{
    mSlatIndex = slatId;
}

inline void
StTofSlat::setAdc(UShort_t rawAdc)
{
    mAdc = rawAdc;
}

inline void
StTofSlat::setTdc(UShort_t rawTdc)
{
    mTdc = rawTdc;
}

inline UShort_t
StTofSlat::slatIndex() const
{
    return mSlatIndex;
}

inline UShort_t
StTofSlat::adc()  const
{
    return mAdc;
}

inline UShort_t
StTofSlat::tdc()  const
{
    return mTdc;
}

#endif
