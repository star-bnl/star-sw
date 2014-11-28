/*!
 * \class StTofSlat 
 * \author Wei-Ming Zhang, Dec 2000
 */
/***************************************************************************
 *
 * $Id: StTofSlat.h,v 2.9 2004/02/05 17:58:51 ullrich Exp $
 *
 * Author: Wei-Ming Zhang, Dec 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTofSlat.h,v $
 * Revision 2.9  2004/02/05 17:58:51  ullrich
 * Changed $LINK to StLink mechanism.
 *
 * Revision 2.8  2003/07/28 21:00:22  ullrich
 * Revised version: new but not inheriting from StHit as before.
 *
 * Revision 2.7  2003/07/11 00:01:04  jeromel
 * Re-adding preceeding revision
 *
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
#include "StThreeVectorF.hh"
#include "StContainers.h"
class StTrack;

class StTofSlat : public StObject {
public:
    StTofSlat();
    StTofSlat(unsigned short, unsigned short, unsigned short, StTrack*,
              float, unsigned short, unsigned short);
    ~StTofSlat();
    
    int operator==(const StTofSlat&) const;
    int operator!=(const StTofSlat&) const;

    unsigned short        slatIndex() const;
    unsigned short        adc() const;
    unsigned short        tdc() const;
    StTrack*              associatedTrack();
    const StTrack*        associatedTrack() const;
    float                 zHit() const;
    unsigned short        hitProf() const;
    unsigned short        matchFlag() const;
    const StThreeVectorF& position() const;
    
    void setSlatIndex(unsigned short);
    void setAdc(unsigned short);
    void setTdc(unsigned short);
    void setAssociatedTrack(StTrack*);
    void setZHit(float);
    void setHitProf(unsigned short);
    void setMatchFlag(unsigned short);
    void setPosition(const StThreeVectorF&);
    
protected:
    UShort_t  mSlatIndex;
    UShort_t  mAdc;
    UShort_t  mTdc;
    // StTrack  *mAssociatedTrack; //$LINK
#ifdef __CINT__
    StObjLink        mAssociatedTrack;		
#else
    StLink<StTrack>  mAssociatedTrack;		
#endif //__CINT__
    Float_t   mZhit;
    UShort_t  mHitProf;
    UShort_t  mMatchFlag;
    StThreeVectorF mPosition;

    ClassDef(StTofSlat,4)
};

inline void
StTofSlat::setSlatIndex(unsigned short slatId) {mSlatIndex = slatId;}

inline void
StTofSlat::setAdc(unsigned short rawAdc) {mAdc = rawAdc;}

inline void
StTofSlat::setTdc(unsigned short rawTdc) {mTdc = rawTdc;}

inline void
StTofSlat::setZHit(float zhit) {mZhit = zhit;}

inline void
StTofSlat::setHitProf(unsigned short hitprof) {mHitProf = hitprof;}

inline void
StTofSlat::setMatchFlag(unsigned short matchflag) {mMatchFlag = matchflag;}

inline unsigned short
StTofSlat::slatIndex() const {return mSlatIndex;}

inline unsigned short
StTofSlat::adc()  const {return mAdc;}

inline unsigned short
StTofSlat::tdc()  const {return mTdc;}

inline float
StTofSlat::zHit() const {return mZhit;}

inline unsigned short
StTofSlat::hitProf() const {return mHitProf;}

inline unsigned short
StTofSlat::matchFlag() const {return mMatchFlag;}

#endif
