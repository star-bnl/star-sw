/***************************************************************************
 *
 * $Id: StRichHit.h,v 2.1 2000/05/22 21:44:50 ullrich Exp $
 *
 * Author: Brian Lasiuk, May 2000
 ***************************************************************************
 *
 * Description: Definition of the persistent Hit object 
 *
 ***************************************************************************
 *
 * $Log: StRichHit.h,v $
 * Revision 2.1  2000/05/22 21:44:50  ullrich
 * Initial Revision
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
    //StRichHit(const StRichHit&){}
    //StRichHit& operator=(const StRichHit&){}

    const    StThreeVectorF& local()    const;
    const    StThreeVectorF& internal() const;
    Float_t  maxAmplitude()             const;
    ULong_t  clusterNumber()            const;
    ULong_t  track()                    const;

    ULong_t  reservedLong()             const;
    Float_t  reservedFloat()            const;
    
    StThreeVectorF&  local();
    StThreeVectorF&  internal();

    void setMaxAmplitude(Float_t);
    void setClusterNumber(UShort_t);
    void setTrack(ULong_t);

    void setReservedLong(ULong_t);
    void setReservedFloat(Float_t);
    
protected:
    StObject* clone();
    
    StThreeVectorF mLocal;
    StThreeVectorF mLError;
    StThreeVectorF mInternal;
    StThreeVectorF mSigma;

    Float_t        mMaxAmplitude;
    ULong_t        mClusterNumber;
    ULong_t        mTrack;

    ULong_t        mReservedLong;
    Float_t        mReservedFloat;
    
    ClassDef(StRichHit,1) 
};

inline const StThreeVectorF& StRichHit::local() const {return mLocal;}
inline StThreeVectorF& StRichHit::local() {return mLocal;}
inline const StThreeVectorF& StRichHit::internal() const {return mInternal;}
inline StThreeVectorF& StRichHit::internal() {return mInternal;}
inline void StRichHit::setMaxAmplitude(Float_t m) {mMaxAmplitude = m;}
inline Float_t StRichHit::maxAmplitude() const {return mMaxAmplitude;}
inline void StRichHit::setClusterNumber(UShort_t no) {mClusterNumber = no;}
inline ULong_t StRichHit::clusterNumber() const {return mClusterNumber;}
inline void StRichHit::setTrack(ULong_t tck) { mTrack = tck;}
inline ULong_t StRichHit::track() const {return mTrack;}

inline ULong_t StRichHit::reservedLong() const {return mReservedLong; }
inline Float_t StRichHit::reservedFloat() const {return mReservedFloat; }
inline void StRichHit::setReservedLong(ULong_t el) {mReservedLong = el;}
inline void StRichHit::setReservedFloat(Float_t fl) {mReservedFloat = fl;}

//non-members
ostream& operator<<(ostream& os, const StRichHit& hit);

#endif
