/***************************************************************************
 *
 * $Id: StRichPidTraits.cxx,v 2.3 2000/11/25 11:53:36 lasiuk Exp $
 *
 * Author: Matt Horsley, Sep 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRichPidTraits.cxx,v $
 * Revision 2.3  2000/11/25 11:53:36  lasiuk
 * initialize data members in c'tor
 *
 * Revision 2.2  2000/11/01 16:46:59  lasiuk
 * Keep the StRichPid as the owner (use a StSPtrVec)
 * also check the pdg encoded number now
 *
 * Revision 2.1  2000/09/28 10:54:46  ullrich
 * Initial Revision.
 *
 ***************************************************************************/
#include "StRichPidTraits.h"

#include "StRichPid.h"

static const char rcsid[] = "$Id: StRichPidTraits.cxx,v 2.3 2000/11/25 11:53:36 lasiuk Exp $";

ClassImp(StRichPidTraits)

StRichPidTraits::StRichPidTraits()
    : StTrackPidTraits(kRichId), mId(0), mProbability(0) {
    mThePids.clear();
    mThePids.resize(0);
}

StRichPidTraits::~StRichPidTraits() { /* noop */ }

StRichPid* StRichPidTraits::getPid(StParticleDefinition* part) {

    for (size_t index=0;index<mThePids.size();index++) {

	if ( (mThePids[index]->getRingType()==part) ||
	     (mThePids[index]->getParticleNumber() == part->pdgEncoding()) ) {
	    return mThePids[index];
	}

    }
    return 0;
}


const StRichPid* StRichPidTraits::getPid(StParticleDefinition* part) const {

    for (size_t index=0;index<mThePids.size();index++) {

	if ( (mThePids[index]->getRingType()==part) ||
	     (mThePids[index]->getParticleNumber() == part->pdgEncoding()) ) {
	    return mThePids[index];
	}
	
    }
    return 0;
}
