/***************************************************************************
 *
 * $Id: StRichPidTraits.h,v 2.2 2000/11/01 16:47:02 lasiuk Exp $
 *
 * Author: Matt Horsley, Sep 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRichPidTraits.h,v $
 * Revision 2.2  2000/11/01 16:47:02  lasiuk
 * Keep the StRichPid as the owner (use a StSPtrVec)
 * also check the pdg encoded number now
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
  
private:
    StSPtrVecRichPid mThePids;
    
    StObject* clone();

    ClassDef(StRichPidTraits,1)
};

// sets

inline       StSPtrVecRichPid& StRichPidTraits::getAllPids()        { return mThePids;}
inline const StSPtrVecRichPid&  StRichPidTraits::getAllPids() const { return mThePids;}

inline void  StRichPidTraits::addPid(StRichPid* t) {mThePids.push_back(t);}

inline StObject* StRichPidTraits::clone() {return new StRichPidTraits(*this);}
#endif
