/***************************************************************************
 *
 * $Id: StRichHit.h,v 2.2 2000/11/01 16:44:22 lasiuk Exp $
 *
 * Author: Brian Lasiuk, May 2000
 ***************************************************************************
 *
 * Description: Definition of the persistent Hit object 
 *
 ***************************************************************************
 *
 * $Log: StRichHit.h,v $
 * Revision 2.2  2000/11/01 16:44:22  lasiuk
 * SCHEMA EVOL: cluster number is now signed and the number of pads
 * that were used in creating a hit is stored.  Bit manipulation
 * functions were changed to use StRichHitFlags from StEnumerations.
 *
 * Revision 2.1  2000/05/22 21:44:50  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StRichHit_hh
#define StRichHit_hh

#include <iostream.h>
#include "StHit.h"

class StRichHit : public StHit {
public:
    StRichHit();
    StRichHit(const StThreeVectorF& xg, const StThreeVectorF& dx);
    StRichHit(const StThreeVectorF& xg, const StThreeVectorF& dx,
	      ULong_t hp, Float_t q, Float_t maxAdc, UChar_t tc);
    
    virtual ~StRichHit();
    //StRichHit(const StRichHit&){ /* nopt */ }
    //StRichHit& operator=(const StRichHit&){/* nopt */}

    const    StThreeVectorF& local()    const;
    const    StThreeVectorF& internal() const;
    Float_t  maxAmplitude()             const;
//     ULong_t  clusterNumber()            const;
    Long_t   clusterNumber()            const;
    ULong_t  track()                    const;
    UShort_t numberOfPads()             const;

    // used for flags
    ULong_t  reservedLong()             const;
    Float_t  reservedFloat()            const;
    
    StThreeVectorF&  local();
    StThreeVectorF&  internal();

    void setMaxAmplitude(Float_t);
    void setClusterNumber(Long_t);
//     void setClusterNumber(ULong_t);
    void setNumberOfPads(UShort_t);
    void setTrack(ULong_t);

    void setReservedLong(ULong_t);
    void setReservedFloat(Float_t);

    bool    isSet(StRichHitFlag f) const;
    void    setBit(StRichHitFlag f);
    void    unSetBit(StRichHitFlag f);
    ULong_t flags()                  const;

protected:
    StObject* clone();
    
    StThreeVectorF mLocal;
    StThreeVectorF mLError;
    StThreeVectorF mInternal;
    StThreeVectorF mSigma;

    Float_t        mMaxAmplitude;
       Long_t        mClusterNumber;
//     ULong_t        mClusterNumber;
    ULong_t        mTrack;

    ULong_t        mReservedLong;
    Float_t        mReservedFloat;

    UShort_t         mNumberOfPads;
    
    ClassDef(StRichHit,1) 
};

inline const StThreeVectorF& StRichHit::local() const {return mLocal;}
inline StThreeVectorF& StRichHit::local() {return mLocal;}
inline const StThreeVectorF& StRichHit::internal() const {return mInternal;}
inline StThreeVectorF& StRichHit::internal() {return mInternal;}
inline void StRichHit::setMaxAmplitude(Float_t m) {mMaxAmplitude = m;}
inline Float_t StRichHit::maxAmplitude() const {return mMaxAmplitude;}
   inline void StRichHit::setClusterNumber(Long_t no) {mClusterNumber = no;}
   inline Long_t StRichHit::clusterNumber() const {return mClusterNumber;}
//   inline void StRichHit::setClusterNumber(ULong_t no) {mClusterNumber = no;}
//   inline ULong_t StRichHit::clusterNumber() const {return mClusterNumber;}
inline void StRichHit::setTrack(ULong_t tck) { mTrack = tck;}
inline ULong_t StRichHit::track() const {return mTrack;}
inline UShort_t StRichHit::numberOfPads() const { return mNumberOfPads;}
inline void StRichHit::setNumberOfPads(UShort_t n) { mNumberOfPads = n;}

inline ULong_t StRichHit::reservedLong() const {return mReservedLong; }
inline Float_t StRichHit::reservedFloat() const {return mReservedFloat; }
inline void StRichHit::setReservedLong(ULong_t el) {mReservedLong = el;}
inline void StRichHit::setReservedFloat(Float_t fl) {mReservedFloat = fl;}

inline bool StRichHit::isSet(StRichHitFlag f) const { return (mReservedLong & f); }
inline void StRichHit::setBit(StRichHitFlag f) { mReservedLong |= f; }
inline void StRichHit::unSetBit(StRichHitFlag f) { mReservedLong &= ~(f);}
inline ULong_t StRichHit::flags() const{ return (mReservedLong); }

//non-members
ostream& operator<<(ostream& os, const StRichHit& hit);

#endif
