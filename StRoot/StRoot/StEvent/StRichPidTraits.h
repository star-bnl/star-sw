/*!
 * \class StRichPidTraits 
 * \author Matt Horsley, Sep 2000
 */
/***************************************************************************
 *
 * $Id: StRichPidTraits.h,v 2.15 2007/10/25 19:20:00 ullrich Exp $
 *
 * Author: Matt Horsley, Sep 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRichPidTraits.h,v $
 * Revision 2.15  2007/10/25 19:20:00  ullrich
 * Removed obsolete == operator
 *
 * Revision 2.14  2004/07/15 16:36:25  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.13  2003/09/02 17:58:05  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.12  2002/02/22 22:56:50  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.11  2002/02/19 23:21:30  ullrich
 * Added copy constructor and assignment operator. New destructor.
 *
 * Revision 2.10  2002/02/19 17:40:49  ullrich
 * Changed version number from 2 to 3.
 *
 * Revision 2.9  2002/02/19 16:54:33  ullrich
 * Minor changes - code not altered.
 *
 * Revision 2.8  2002/02/19 04:24:03  lasiuk
 * addition of StRichSpectra information for uDST purposes
 *
 * Revision 2.7  2001/05/30 17:45:54  perev
 * StEvent branching
 *
 * Revision 2.6  2001/04/05 04:00:41  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
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

#include <Stiostream.h>
#include "StTrackPidTraits.h"
#include "StParticleDefinition.hh"
#include "StContainers.h"
#include "StRichPid.h"
#include "StRichSpectra.h"
#include "StParticleTypes.hh"


class StRichPidTraits : public StTrackPidTraits {
public:
    StRichPidTraits();
    ~StRichPidTraits();
    
    StRichPidTraits(const StRichPidTraits&);
    StRichPidTraits& operator=(const StRichPidTraits&);
    
    void                    addPid(StRichPid* );
  
    const StSPtrVecRichPid& getAllPids() const;
    StSPtrVecRichPid&       getAllPids();
    
    StRichPid*              getPid(StParticleDefinition* t);
    const StRichPid*        getPid(StParticleDefinition* t)  const;

    StRichSpectra*          getRichSpectra();
    const StRichSpectra*    getRichSpectra() const;

    void                    setRichSpectra(StRichSpectra*);

    void  setProductionVersion(int);
    void  setId(int);
    void  setProbability(float);
    void  setAssociatedMip(StRichHit*);
    void  setMipResidual(const StThreeVectorF&);
    void  setRefitResidual(const StThreeVectorF&);
    void  setSignedDca2d(float);
    void  setSignedDca3d(float);
    
    int   productionVersion() const;
    int   id() const;
    float probability() const;

    const StRichHit*       associatedMip() const;
    const StThreeVectorF&  mipResidual() const;
    const StThreeVectorF&  refitResidual() const;
    float                  signedDca2d() const;
    float                  signedDca3d() const;
    
private:
    StSPtrVecRichPid  mThePids;
    Int_t             mProductionVersion;
    Int_t             mId;
    Float_t           mProbability;
#ifdef __CINT__
    StObjLink	      mAssociatedMip; 
#else
    StLink<StRichHit> mAssociatedMip; 
#endif //__CINT__
    StThreeVectorF    mMipResidual;
    StThreeVectorF    mRefitResidual;
    Float_t           mSigned3dDca;
    Float_t           mSigned2dDca;
    StRichSpectra*    mRichSpectra;
    
    ClassDef(StRichPidTraits,3)
};

// sets

inline StSPtrVecRichPid& StRichPidTraits::getAllPids()        { return mThePids;}
inline const StSPtrVecRichPid&  StRichPidTraits::getAllPids() const { return mThePids;}
inline void  StRichPidTraits::addPid(StRichPid* t) {mThePids.push_back(t);}
inline void StRichPidTraits::setProductionVersion(int id) {mProductionVersion = id;}
inline void StRichPidTraits::setId(int id) {mId = id;}
inline void StRichPidTraits::setProbability(float p) {mProbability = p;}
inline void StRichPidTraits::setAssociatedMip(StRichHit* hit) {mAssociatedMip = hit;}
inline void StRichPidTraits::setMipResidual(const StThreeVectorF& res) {mMipResidual = res;}
inline void StRichPidTraits::setRefitResidual(const StThreeVectorF& res) {mRefitResidual = res;}
inline void StRichPidTraits::setSignedDca2d(float v) {mSigned2dDca = v;}
inline void StRichPidTraits::setSignedDca3d(float v) {mSigned3dDca = v;}
inline int StRichPidTraits::productionVersion() const { return mProductionVersion;}
inline int StRichPidTraits::id() const {return mId;}
inline float StRichPidTraits::probability() const {return mProbability;}
inline const StRichHit* StRichPidTraits::associatedMip() const { return mAssociatedMip;}
inline const StThreeVectorF& StRichPidTraits::mipResidual() const {return mMipResidual;}
inline const StThreeVectorF& StRichPidTraits::refitResidual() const { return mRefitResidual;}
inline float StRichPidTraits::signedDca2d() const { return mSigned2dDca;}
inline float StRichPidTraits::signedDca3d() const { return mSigned3dDca;}
inline StRichSpectra* StRichPidTraits::getRichSpectra() {return mRichSpectra;}
inline const StRichSpectra* StRichPidTraits::getRichSpectra() const {return mRichSpectra;}
inline void StRichPidTraits::setRichSpectra(StRichSpectra* sp) {mRichSpectra = sp;}

//non-members
ostream& operator<<(ostream& os, const StRichPidTraits& t);
#endif
