/***************************************************************************
 *
 * $Id: StRichPidTraits.h,v 2.5 2001/03/24 03:34:57 perev Exp $
 *
 * Author: Matt Horsley, Sep 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRichPidTraits.h,v $
 * Revision 2.5  2001/03/24 03:34:57  perev
 * clone() -> clone() const
 *
 * Revision 2.4  2001/02/22 21:05:02  lasiuk
 * add production version, associated MIP, dca residual 3Vectors
 *
 * Revision 2.3  2000/11/25 11:53:13  lasiuk
 * introduction of hypothesis and probability
 *
 * Revision 2.2  2000/11/01 16:47:02  lasiuk
 * Keep the StRichPid as the owner (use a StSPtrVec)
 * also check the pdg encoded number now
 *
 * Revision 2.1  2000/09/28 10:54:48  ullrich
 * Initial Revision.
 *
 ***************************************************************************/
#ifndef StRichPidTraits_hh
#define StRichPidTraits_hh

#include <iostream.h>

#include "StTrackPidTraits.h"

#include "StParticleDefinition.hh"

#include "StContainers.h"
#include "StRichPid.h"
#include "StParticleTypes.hh"


class StRichPidTraits : public StTrackPidTraits {
public:
    StRichPidTraits();
    ~StRichPidTraits();
    
    //StRichPidTraits(const StRichPidTraits&) {/* nopt */}
    //StRichPidTraits& operator=(const StRichPidTraits&) {/* nopt */}
    
    Int_t  operator==(const StRichPidTraits&) const;
    
    void                    addPid(StRichPid* );
  
    const StSPtrVecRichPid& getAllPids() const;
    StSPtrVecRichPid&       getAllPids();
    
    StRichPid*              getPid(StParticleDefinition* t);
    const StRichPid*        getPid(StParticleDefinition* t)  const;

    void setProductionVersion(Int_t);
    void setId(Int_t);
    void setProbability(Float_t);
    void setAssociatedMip(StRichHit*);
    void setMipResidual(const StThreeVectorF&);
    void setRefitResidual(const StThreeVectorF&);
    void setSignedDca2d(Float_t);
    void setSignedDca3d(Float_t);
    
    Int_t   productionVersion() const;
    Int_t   id() const;
    Float_t probability() const;

    StRichHit* associatedMip() const;
    const StThreeVectorF& mipResidual() const;
    const StThreeVectorF& refitResidual() const;
    Float_t signedDca2d() const;
    Float_t signedDca3d() const;
    
private:
    StSPtrVecRichPid mThePids;

    Int_t            mProductionVersion;
    Int_t            mId;
    Float_t          mProbability;

    StRichHit*       mAssociatedMip;//$LINK
    StThreeVectorF   mMipResidual;
    StThreeVectorF   mRefitResidual;

    Float_t          mSigned3dDca;
    Float_t          mSigned2dDca;
    
    StObject* clone() const;

    ClassDef(StRichPidTraits,1)
};

// sets

inline       StSPtrVecRichPid& StRichPidTraits::getAllPids()        { return mThePids;}
inline const StSPtrVecRichPid&  StRichPidTraits::getAllPids() const { return mThePids;}

inline void  StRichPidTraits::addPid(StRichPid* t) {mThePids.push_back(t);}

inline StObject* StRichPidTraits::clone() const {return new StRichPidTraits(*this);}

inline void StRichPidTraits::setProductionVersion(Int_t id) {mProductionVersion = id;}
inline void StRichPidTraits::setId(Int_t id) {mId = id;} 
inline void StRichPidTraits::setProbability(Float_t p) {mProbability = p;}
inline void StRichPidTraits::setAssociatedMip(StRichHit* hit) {mAssociatedMip = hit;}
inline void StRichPidTraits::setMipResidual(const StThreeVectorF& res) {mMipResidual = res;}
inline void StRichPidTraits::setRefitResidual(const StThreeVectorF& res) {mRefitResidual = res;}
inline void StRichPidTraits::setSignedDca2d(Float_t v) {mSigned2dDca = v;}
inline void StRichPidTraits::setSignedDca3d(Float_t v) {mSigned3dDca = v;}

inline Int_t StRichPidTraits::productionVersion() const { return mProductionVersion;}
inline Int_t StRichPidTraits::id() const {return mId;}
inline Float_t StRichPidTraits::probability() const {return mProbability;}

inline StRichHit* StRichPidTraits::associatedMip() const { return mAssociatedMip;}
inline const StThreeVectorF& StRichPidTraits::mipResidual() const {return mMipResidual;}
inline const StThreeVectorF& StRichPidTraits::refitResidual() const { return mRefitResidual;}
inline Float_t StRichPidTraits::signedDca2d() const { return mSigned2dDca;}
inline Float_t StRichPidTraits::signedDca3d() const { return mSigned3dDca;}

//non-members
ostream& operator<<(ostream& os, const StRichPidTraits& t);
#endif
