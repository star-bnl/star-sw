/***************************************************************************
 *
 * $Id: StRichPidTraits.h,v 2.3 2000/11/25 11:53:13 lasiuk Exp $
 *
 * Author: Matt Horsley, Sep 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRichPidTraits.h,v $
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

    void setId(Int_t);
    void setProbability(Float_t);

    Int_t   id() const;
    Float_t probability() const;

private:
    StSPtrVecRichPid mThePids;

    Int_t            mId;
    Float_t          mProbability;
    
    StObject* clone();

    ClassDef(StRichPidTraits,1)
};

// sets

inline       StSPtrVecRichPid& StRichPidTraits::getAllPids()        { return mThePids;}
inline const StSPtrVecRichPid&  StRichPidTraits::getAllPids() const { return mThePids;}

inline void  StRichPidTraits::addPid(StRichPid* t) {mThePids.push_back(t);}

inline StObject* StRichPidTraits::clone() {return new StRichPidTraits(*this);}
inline void StRichPidTraits::setId(Int_t id) {mId = id;} 
inline void StRichPidTraits::setProbability(Float_t p) {mProbability = p;}

inline Int_t StRichPidTraits::id() const {return mId;}
inline Float_t StRichPidTraits::probability() const {return mProbability;}
#endif
