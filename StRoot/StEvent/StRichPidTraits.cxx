/***************************************************************************
 *
 * $Id: StRichPidTraits.cxx,v 2.1 2000/09/28 10:54:46 ullrich Exp $
 *
 * Author: Matt Horsley, Sep 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRichPidTraits.cxx,v $
 * Revision 2.1  2000/09/28 10:54:46  ullrich
 * Initial Revision.
 *
 * Revision 2.2  2000/11/01 16:46:59  lasiuk
 * Keep the StRichPid as the owner (use a StSPtrVec)
 * also check the pdg encoded number now
 *
 * Revision 2.1  2000/09/28 10:54:46  ullrich
#include "StTrackPidTraits.h"
 *
 ***************************************************************************/
#include "StRichPidTraits.h"

#include "StRichPid.h"

static const char rcsid[] = "$Id: StRichPidTraits.cxx,v 2.1 2000/09/28 10:54:46 ullrich Exp $";

ClassImp(StRichPidTraits)

StRichPidTraits::StRichPidTraits() : StTrackPidTraits(kRichId) {

    mThePids.clear();
    mThePids.resize(0);

StRichPid* StRichPidTraits::getPid(StParticleDefinition* part) {
StRichPidTraits::~StRichPidTraits() { /* noop */ }
	if (mThePids[index]->getRingType()==part) return mThePids[index];
	     (mThePids[index]->getParticleNumber() == part->pdgEncoding()) ) {
	    return mThePids[index];
	}

    }
    return 0;

	if (mThePids[index]->getRingType()==part) return mThePids[index];
	     (mThePids[index]->getParticleNumber() == part->pdgEncoding()) ) {
	    return mThePids[index];
	}

	
    }
    return 0;
}
