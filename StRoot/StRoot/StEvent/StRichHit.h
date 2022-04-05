/*!
 * \class StRichHit 
 * \author Brian Lasiuk, May 2000
 */
/***************************************************************************
 *
 * $Id: StRichHit.h,v 2.8 2016/02/25 17:10:20 ullrich Exp $
 *
 * Author: Brian Lasiuk, May 2000
 ***************************************************************************
 *
 * Description: Definition of the persistent Hit object
 *
 ***************************************************************************
 *
 * $Log: StRichHit.h,v $
 * Revision 2.8  2016/02/25 17:10:20  ullrich
 * Implemented detector() which is now a pure abstract method in StHit.
 *
 * Revision 2.7  2004/07/15 16:36:25  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.6  2003/09/02 17:58:05  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.5  2002/02/22 22:56:49  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.4  2001/04/05 04:00:40  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.3  2001/03/24 03:34:54  perev
 * clone() -> clone() const
 *
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

#include <Stiostream.h>
#include "StHit.h"

class StRichHit : public StHit {
public:
    StRichHit();
    StRichHit(const StThreeVectorF& xg, const StThreeVectorF& dx);
    StRichHit(const StThreeVectorF& xg, const StThreeVectorF& dx,
              unsigned int hp, float q, float maxAdc, unsigned char tc);
    
    virtual ~StRichHit();
    //StRichHit(const StRichHit&){ /* nopt */ }
    //StRichHit& operator=(const StRichHit&){/* nopt */}

    const    StThreeVectorF& local()    const;
    const    StThreeVectorF& internal() const;
    float    maxAmplitude()             const;
//  unsigned int  clusterNumber()            const;
    int            clusterNumber()            const;
    unsigned int   track()                    const;
    unsigned short numberOfPads()             const;

    // used for flags
    unsigned int  reservedLong()             const;
    float         reservedFloat()            const;
    
    StThreeVectorF&  local();
    StThreeVectorF&  internal();

    void setMaxAmplitude(float);
    void setClusterNumber(int);
//  void setClusterNumber(unsigned int);
    void setNumberOfPads(unsigned short);
    void setTrack(unsigned int);

    void setReservedLong(unsigned int);
    void setReservedFloat(float);

    bool         isSet(StRichHitFlag f) const;
    void         setBit(StRichHitFlag f);
    void         unSetBit(StRichHitFlag f);
    unsigned int flags() const;
    StDetectorId detector() const;

protected:
    StThreeVectorF mLocal;
    StThreeVectorF mLError;
    StThreeVectorF mInternal;
    StThreeVectorF mSigma;

    Float_t        mMaxAmplitude;
    Int_t          mClusterNumber;
//  UInt_t         mClusterNumber;
    UInt_t         mTrack;

    UInt_t         mReservedLong;
    Float_t        mReservedFloat;

    UInt_t         mNumberOfPads;
    
    ClassDef(StRichHit,1)
};

inline StDetectorId StRichHit::detector() const {return kRichId;}

inline const StThreeVectorF& StRichHit::local() const {return mLocal;}
inline StThreeVectorF& StRichHit::local() {return mLocal;}
inline const StThreeVectorF& StRichHit::internal() const {return mInternal;}
inline StThreeVectorF& StRichHit::internal() {return mInternal;}
inline void StRichHit::setMaxAmplitude(float m) {mMaxAmplitude = m;}
inline float StRichHit::maxAmplitude() const {return mMaxAmplitude;}
inline void StRichHit::setClusterNumber(int no) {mClusterNumber = no;}
inline int StRichHit::clusterNumber() const {return mClusterNumber;}
//   inline void StRichHit::setClusterNumber(unsigned int no) {mClusterNumber = no;}
//   inline unsigned int StRichHit::clusterNumber() const {return mClusterNumber;}
inline void StRichHit::setTrack(unsigned int tck) { mTrack = tck;}
inline unsigned int StRichHit::track() const {return mTrack;}
inline unsigned short StRichHit::numberOfPads() const { return mNumberOfPads;}
inline void StRichHit::setNumberOfPads(unsigned short n) { mNumberOfPads = n;}

inline unsigned int StRichHit::reservedLong() const {return mReservedLong; }
inline float StRichHit::reservedFloat() const {return mReservedFloat; }
inline void StRichHit::setReservedLong(unsigned int el) {mReservedLong = el;}
inline void StRichHit::setReservedFloat(float fl) {mReservedFloat = fl;}

inline bool StRichHit::isSet(StRichHitFlag f) const { return (mReservedLong & f); }
inline void StRichHit::setBit(StRichHitFlag f) { mReservedLong |= f; }
inline void StRichHit::unSetBit(StRichHitFlag f) { mReservedLong &= ~(f);}
inline unsigned int StRichHit::flags() const{ return (mReservedLong); }

//non-members
ostream& operator<<(ostream& os, const StRichHit& hit);

#endif
