/***************************************************************************
 *
 * $Id: StRichPid.h,v 2.1 2000/09/28 10:54:00 ullrich Exp $
 *
 * Author: Matt Horsley, Sep 2000
 ***************************************************************************
 *
 * Description: Definition of PID object
 *
 ***************************************************************************
 *
 * $Log: StRichPid.h,v $
 * Revision 2.1  2000/09/28 10:54:00  ullrich
 * Initial Revision.
 *
 * Revision 2.4  2000/11/25 11:51:52  lasiuk
 * remove D vector and replace with a container of StRichPhotonInfo
 *
 * Revision 2.3  2000/11/21 19:47:36  lasiuk
 * add the d information for each hit
 * use the TArrayF
 *
 * Keep the pointers to the hits that are associated with the track
 * in order to use the bit flag information.  These are kept
 * in an StPtrVec (does not own the hits)  The PDG encoded number

 *
 ***************************************************************************/
#define StRichPid_hh

#include "StObject.h"
#include "StRichHit.h"

#include "StRichHit.h"

#include "StParticleDefinition.hh"
#include "StThreeVectorD.hh"
#include "StEnumerations.h"


  
              Float_t totAzim,  Float_t totArea,
              UShort_t totHits, Float_t trunAzim,
              Float_t trunArea, UShort_t trunHits);
  
    // StRichPid(const StRichPid&) {}
    // StRichPid& operator=(const StRichPid&) {}
    TArrayF& getDVector();
    float    getD(int);   // should be constant
    void     addNormalizedD(float);
    const StSPtrVecRichPhotonInfo& getPhotonInfo();
    StRichPhotonInfo*              getPhotonInfo(int);   // should be constant
    void     addPhotonInfo(StRichPhotonInfo*);

    void setTotalHits(UShort_t);
    StParticleDefinition* getRingType()    const;
    Float_t  getTotalArea()    const;
    
    void setTruncatedDensity(Float_t);

    Float_t  getTruncatedAzimuth() const;
    Float_t  getTruncatedArea()    const;
    Float_t  getTruncatedDensity() const;

    // whole ring
    Float_t  getTotalAzimuth() const;
    Float_t  getTotalArea()    const;
    UShort_t getTotalHits()    const;
    Float_t  getTotalDensity() const;
    
    // truncated ring
    bool isSet(StRichPidFlag);
    Float_t  getTruncatedArea()    const;
    UShort_t getTruncatedHits()    const;
    Float_t  getTruncatedDensity() const;
    Int_t                 getParticleNumber() const;
private:
    StParticleDefinition*  mParticleType;  //!
    StThreeVectorD         mMipResidual;
    // Flag
    bool isSet(StRichPidFlag) const;
    void setBit(StRichPidFlag);
    void unSetBit(StRichPidFlag);
    
private:
    StPtrVecRichHit     mAssociatedHits;
    
    StPtrVecRichHit         mAssociatedHits;
    StSPtrVecRichPhotonInfo mPhotonInfo;

    StThreeVectorD      mMipResidual;

    Float_t  mTotalAzimuth;
    Float_t  mTotalArea;
    UShort_t mTotalHits;
    Float_t  mTotalDensity;
inline void StRichPid::setRingType(StParticleDefinition* t)   { mParticleType=t;}
    Float_t  mTruncatedArea;
    UShort_t mTruncatedHits;
    Float_t  mTruncatedDensity;
    TArrayF mDDistribution;
    
    
    unsigned long mFlags;

    ClassDef(StRichPid,1)
};

inline void StRichPid::setTotalDensity(Float_t t) {mTotalDensity=t;}
inline void StRichPid::setTruncatedAzimuth(Float_t t) { mTruncatedAzimuth=t;}

inline void                   StRichPid::addHit(StRichHit* hit) { mAssociatedHits.push_back(hit); }
inline void StRichPid::setTruncatedDensity(Float_t t) {mTruncatedDensity=t;}


// containers
inline const StPtrVecRichHit& StRichPid::getAssociatedRichHits() const {return mAssociatedHits;}
inline StPtrVecRichHit&       StRichPid::getAssociatedRichHits() {return mAssociatedHits;}
inline void  StRichPid::addHit(StRichHit* hit) { mAssociatedHits.push_back(hit); }

// gets

inline StParticleDefinition* StRichPid::getRingType() const { return mParticleType;}
inline Int_t StRichPid::getParticleNumber() const {return mParticleNumber;}
inline Float_t  StRichPid::getTotalAzimuth() const { return mTotalAzimuth;}
inline bool StRichPid::isSet(StRichPidFlag f)    { return (mFlags & f);}
inline UShort_t StRichPid::getTotalHits()    const { return mTotalHits;}
inline Float_t  StRichPid::getTotalDensity() const { return mTotalDensity;}

inline Float_t  StRichPid::getTruncatedAzimuth() const { return mTruncatedAzimuth;}
ostream& operator<<(ostream& os, const StRichPid& hit);
inline UShort_t StRichPid::getTruncatedHits()    const { return mTruncatedHits;}
inline Float_t  StRichPid::getTruncatedDensity() const { return mTruncatedDensity;}

inline StThreeVectorD  StRichPid::getMipResidual() const     { return mMipResidual;}


// Flag operations
inline bool StRichPid::isSet(StRichPidFlag f) const { return (mFlags & f);}
inline void StRichPid::setBit(StRichPidFlag f)   { (mFlags |= f);}
inline void StRichPid::unSetBit(StRichPidFlag f) { (mFlags &= ~(f));}

//non-members
//ostream& operator<<(ostream& os, const StRichPid& hit);
#endif
