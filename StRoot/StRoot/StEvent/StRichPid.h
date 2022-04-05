/*!
 * \class StRichPid 
 * \author Matt Horsley, Sep 2000
 */
/***************************************************************************
 *
 * $Id: StRichPid.h,v 2.7 2002/02/22 22:56:49 jeromel Exp $
 *
 * Author: Matt Horsley, Sep 2000
 ***************************************************************************
 *
 * Description: Definition of PID object
 *
 ***************************************************************************
 *
 * $Log: StRichPid.h,v $
 * Revision 2.7  2002/02/22 22:56:49  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.6  2001/04/05 04:00:40  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
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
              float totAzim,  float totArea,
              unsigned short totHits, float trunAzim,
              float trunArea, unsigned short trunHits);
  
    // StRichPid(const StRichPid&) {}
    // StRichPid& operator=(const StRichPid&) {}
  
    int operator==(const StRichPid&) const;

    const StPtrVecRichHit&      getAssociatedRichHits() const;
    StPtrVecRichHit&            getAssociatedRichHits();
    void                        addHit(StRichHit*);

    const StSPtrVecRichPhotonInfo& getPhotonInfo();
    StRichPhotonInfo*              getPhotonInfo(int);   // should be constant
    void                           addPhotonInfo(StRichPhotonInfo*);

    void setRingType(StParticleDefinition* particle);
    void setMipResidual(StThreeVectorD t);
    
    // whole ring
    void setTotalAzimuth(float);
    void setTotalArea(float);
    void setTotalHits(unsigned short);
    void setTotalDensity(float);
    
    float  getTotalAzimuth() const;
    float  getTotalArea()    const;
    unsigned short getTotalHits()    const;
    float  getTotalDensity() const;
    
    // constant area cut
    void setTruncatedAzimuth(float);
    void setTruncatedArea(float);
    void setTruncatedHits(unsigned short);
    void setTruncatedDensity(float);

    float  getTruncatedAzimuth() const;
    float  getTruncatedArea()    const;
    unsigned short getTruncatedHits()    const;
    float  getTruncatedDensity() const;

    float  getConstantAreaCut()  const;
    void   setConstantAreaCut(float);
    
    StParticleDefinition* getRingType()       const;
    int                 getParticleNumber() const;

    
    StThreeVectorD        getMipResidual() const;
    
    // Flag
    bool isSet(StRichPidFlag) const;
    void setBit(StRichPidFlag);
    void unSetBit(StRichPidFlag);
    
private:
    StParticleDefinition*  mParticleType;//!
    Int_t                  mParticleNumber;
    
    StPtrVecRichHit         mAssociatedHits;
    StSPtrVecRichPhotonInfo mPhotonInfo;

    StThreeVectorD          mMipResidual;

    Float_t  mTotalAzimuth;
    Float_t  mTotalArea;
    UShort_t mTotalHits;
    Float_t  mTotalDensity;
    
    Float_t  mTruncatedAzimuth;
    Float_t  mTruncatedArea;
    UShort_t mTruncatedHits;
    Float_t  mTruncatedDensity;

    Float_t  mConstantAreaCut;
    
    UInt_t   mFlags;

    ClassDef(StRichPid,1)
};


// sets
inline void StRichPid::setMipResidual(StThreeVectorD t) { mMipResidual=t;}

inline void StRichPid::setTotalAzimuth(float t) { mTotalAzimuth=t;}
inline void StRichPid::setTotalArea(float t)    { mTotalArea=t;}
inline void StRichPid::setTotalHits(unsigned short t)   { mTotalHits=t;}
inline void StRichPid::setTotalDensity(float t) {mTotalDensity=t;}

inline void StRichPid::setTruncatedAzimuth(float t) { mTruncatedAzimuth=t;}
inline void StRichPid::setTruncatedArea(float t)    { mTruncatedArea=t;}
inline void StRichPid::setTruncatedHits(unsigned short t)   { mTruncatedHits=t;}
inline void StRichPid::setTruncatedDensity(float t) {mTruncatedDensity=t;}


// containers
inline const StPtrVecRichHit& StRichPid::getAssociatedRichHits() const {return mAssociatedHits;}
inline StPtrVecRichHit&       StRichPid::getAssociatedRichHits() {return mAssociatedHits;}
inline void  StRichPid::addHit(StRichHit* hit) { mAssociatedHits.push_back(hit); }

// gets

inline StParticleDefinition* StRichPid::getRingType() const { return mParticleType;}
inline int StRichPid::getParticleNumber() const {return mParticleNumber;}
inline float  StRichPid::getTotalAzimuth() const { return mTotalAzimuth;}
inline float  StRichPid::getTotalArea()    const { return mTotalArea;}
inline unsigned short StRichPid::getTotalHits()    const { return mTotalHits;}
inline float  StRichPid::getTotalDensity() const { return mTotalDensity;}

inline float  StRichPid::getTruncatedAzimuth() const { return mTruncatedAzimuth;}
inline float  StRichPid::getTruncatedArea()    const { return mTruncatedArea;}
inline unsigned short StRichPid::getTruncatedHits()    const { return mTruncatedHits;}
inline float  StRichPid::getTruncatedDensity() const { return mTruncatedDensity;}

inline float  StRichPid::getConstantAreaCut() const {return mConstantAreaCut;}
inline void     StRichPid::setConstantAreaCut(float c) { mConstantAreaCut = c;}



inline StThreeVectorD  StRichPid::getMipResidual() const     { return mMipResidual;}


// Flag operations
inline bool StRichPid::isSet(StRichPidFlag f) const { return (mFlags & f);}
inline void StRichPid::setBit(StRichPidFlag f)   { (mFlags |= f);}
inline void StRichPid::unSetBit(StRichPidFlag f) { (mFlags &= ~(f));}

//non-members
//ostream& operator<<(ostream& os, const StRichPid& hit);
#endif
