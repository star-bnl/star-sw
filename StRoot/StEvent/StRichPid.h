/***************************************************************************
 *
 * $Id: StRichPid.h,v 2.5 2000/11/27 17:19:09 lasiuk Exp $
 *
 * Author: Matt Horsley, Sep 2000
 ***************************************************************************
 *
 * Description: Definition of PID object
 *
 ***************************************************************************
 *
 * $Log: StRichPid.h,v $
 * Revision 2.5  2000/11/27 17:19:09  lasiuk
 * keep the constant angle
 *
 * Revision 2.4  2000/11/25 11:51:52  lasiuk
 * remove D vector and replace with a container of StRichPhotonInfo
 *
 * Revision 2.3  2000/11/21 19:47:36  lasiuk
 * add the d information for each hit
 * use the TArrayF
 *
 * Revision 2.2  2000/11/01 16:45:46  lasiuk
 * Keep the pointers to the hits that are associated with the track
 * in order to use the bit flag information.  These are kept
 * in an StPtrVec (does not own the hits)  The PDG encoded number
 * is kept as a data member now
 *
 * Revision 2.1  2000/09/28 10:54:00  ullrich
 * Initial Revision.
 *
 ***************************************************************************/
#ifndef StRichPid_hh
#define StRichPid_hh

#include "StObject.h"
#include "StContainers.h"

#include "StRichHit.h"

#include "StParticleDefinition.hh"
#include "StThreeVectorD.hh"
#include "StEnumerations.h"


class StRichPid : public StObject {
public:
    StRichPid();
    ~StRichPid();
    StRichPid(StParticleDefinition* particle, StThreeVectorD resid,
              Float_t totAzim,  Float_t totArea,
              UShort_t totHits, Float_t trunAzim,
              Float_t trunArea, UShort_t trunHits);
  
    // StRichPid(const StRichPid&) {}
    // StRichPid& operator=(const StRichPid&) {}
  
    Int_t operator==(const StRichPid&) const;

    const StPtrVecRichHit&      getAssociatedRichHits() const;
    StPtrVecRichHit&            getAssociatedRichHits();
    void                        addHit(StRichHit*);

    const StSPtrVecRichPhotonInfo& getPhotonInfo();
    StRichPhotonInfo*              getPhotonInfo(int);   // should be constant
    void     addPhotonInfo(StRichPhotonInfo*);

    void setRingType(StParticleDefinition* particle);
    void setMipResidual(StThreeVectorD t);
    
    // whole ring
    void setTotalAzimuth(Float_t);
    void setTotalArea(Float_t);
    void setTotalHits(UShort_t);
    void setTotalDensity(Float_t);
    
    Float_t  getTotalAzimuth() const;
    Float_t  getTotalArea()    const;
    UShort_t getTotalHits()    const;
    Float_t  getTotalDensity() const;
    
    // constant area cut
    void setTruncatedAzimuth(Float_t);
    void setTruncatedArea(Float_t);
    void setTruncatedHits(UShort_t);
    void setTruncatedDensity(Float_t);

    Float_t  getTruncatedAzimuth() const;
    Float_t  getTruncatedArea()    const;
    UShort_t getTruncatedHits()    const;
    Float_t  getTruncatedDensity() const;

    Float_t  getConstantAreaCut()  const;
    void     setConstantAreaCut(Float_t);
    
    StParticleDefinition* getRingType()       const;
    Int_t                 getParticleNumber() const;

    
    StThreeVectorD        getMipResidual() const;
    
    // Flag
    bool isSet(StRichPidFlag) const;
    void setBit(StRichPidFlag);
    void unSetBit(StRichPidFlag);
    
private:
    StParticleDefinition*  mParticleType;//!
    Int_t               mParticleNumber;
    
    StPtrVecRichHit         mAssociatedHits;
    StSPtrVecRichPhotonInfo mPhotonInfo;

    StThreeVectorD      mMipResidual;

    Float_t  mTotalAzimuth;
    Float_t  mTotalArea;
    UShort_t mTotalHits;
    Float_t  mTotalDensity;
    
    Float_t  mTruncatedAzimuth;
    Float_t  mTruncatedArea;
    UShort_t mTruncatedHits;
    Float_t  mTruncatedDensity;

    Float_t  mConstantAreaCut;
    
    unsigned long mFlags;

    ClassDef(StRichPid,1)
};


// sets
inline void StRichPid::setMipResidual(StThreeVectorD t) { mMipResidual=t;}

inline void StRichPid::setTotalAzimuth(Float_t t) { mTotalAzimuth=t;}
inline void StRichPid::setTotalArea(Float_t t)    { mTotalArea=t;}
inline void StRichPid::setTotalHits(UShort_t t)   { mTotalHits=t;}
inline void StRichPid::setTotalDensity(Float_t t) {mTotalDensity=t;}

inline void StRichPid::setTruncatedAzimuth(Float_t t) { mTruncatedAzimuth=t;}
inline void StRichPid::setTruncatedArea(Float_t t)    { mTruncatedArea=t;}
inline void StRichPid::setTruncatedHits(UShort_t t)   { mTruncatedHits=t;}
inline void StRichPid::setTruncatedDensity(Float_t t) {mTruncatedDensity=t;}


// containers
inline const StPtrVecRichHit& StRichPid::getAssociatedRichHits() const {return mAssociatedHits;}
inline StPtrVecRichHit&       StRichPid::getAssociatedRichHits() {return mAssociatedHits;}
inline void  StRichPid::addHit(StRichHit* hit) { mAssociatedHits.push_back(hit); }

// gets

inline StParticleDefinition* StRichPid::getRingType() const { return mParticleType;}
inline Int_t StRichPid::getParticleNumber() const {return mParticleNumber;}
inline Float_t  StRichPid::getTotalAzimuth() const { return mTotalAzimuth;}
inline Float_t  StRichPid::getTotalArea()    const { return mTotalArea;}
inline UShort_t StRichPid::getTotalHits()    const { return mTotalHits;}
inline Float_t  StRichPid::getTotalDensity() const { return mTotalDensity;}

inline Float_t  StRichPid::getTruncatedAzimuth() const { return mTruncatedAzimuth;}
inline Float_t  StRichPid::getTruncatedArea()    const { return mTruncatedArea;}
inline UShort_t StRichPid::getTruncatedHits()    const { return mTruncatedHits;}
inline Float_t  StRichPid::getTruncatedDensity() const { return mTruncatedDensity;}

inline Float_t  StRichPid::getConstantAreaCut() const {return mConstantAreaCut;}
inline void     StRichPid::setConstantAreaCut(Float_t c) { mConstantAreaCut = c;}



inline StThreeVectorD  StRichPid::getMipResidual() const     { return mMipResidual;}


// Flag operations
inline bool StRichPid::isSet(StRichPidFlag f) const { return (mFlags & f);}
inline void StRichPid::setBit(StRichPidFlag f)   { (mFlags |= f);}
inline void StRichPid::unSetBit(StRichPidFlag f) { (mFlags &= ~(f));}

//non-members
//ostream& operator<<(ostream& os, const StRichPid& hit);
#endif
