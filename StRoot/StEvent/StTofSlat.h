/*!
 * \class StTofSlat 
 * \author Wei-Ming Zhang, Dec 2000
 */
/***************************************************************************
 *
 * $Id: StTofSlat.h,v 2.4 2003/05/21 18:23:18 ullrich Exp $
 *
 * Author: Wei-Ming Zhang, Dec 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTofSlat.h,v $
 * Revision 2.4  2003/05/21 18:23:18  ullrich
 * Major Revision of ToF classes (F. Geurts)
 *
 * Revision 2.3  2002/02/22 22:56:52  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.2  2001/04/05 04:00:43  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.1  2000/12/21 23:52:23  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StTofSlat_hh
#define StTofSlat_hh

#include "StObject.h"
class StTrack;

class StTofSlat : public StObject {
public:
    StTofSlat();
    StTofSlat(unsigned short, unsigned short, unsigned short, StTrack*);
    ~StTofSlat();
    
    int operator==(const StTofSlat&) const;
    int operator!=(const StTofSlat&) const;

    unsigned short  slatIndex() const;
    unsigned short  adc() const;
    unsigned short  tdc() const;
    
    StTrack*       associatedTrack();
    const StTrack* associatedTrack() const;
    
    void      setSlatIndex(unsigned short);
    void      setAdc(unsigned short);
    void      setTdc(unsigned short);
    void      setAssociatedTrack(StTrack*);
    
protected:
    UShort_t  mSlatIndex;
    UShort_t  mAdc;
    UShort_t  mTdc;
    StTrack  *mAssociatedTrack; //$LINK
    
    ClassDef(StTofSlat,2)
};

inline void
StTofSlat::setSlatIndex(unsigned short slatId)
{
    mSlatIndex = slatId;
}

inline void
StTofSlat::setAdc(unsigned short rawAdc)
{
    mAdc = rawAdc;
}

inline void
StTofSlat::setTdc(unsigned short rawTdc)
{
    mTdc = rawTdc;
}

inline unsigned short
StTofSlat::slatIndex() const
{
    return mSlatIndex;
}

inline unsigned short
StTofSlat::adc()  const
{
    return mAdc;
}

inline unsigned short
StTofSlat::tdc()  const
{
    return mTdc;
}

#endif
