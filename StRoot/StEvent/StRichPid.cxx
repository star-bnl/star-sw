/***************************************************************************
 *
 * $Id: StRichPid.cxx,v 2.1 2000/09/28 10:53:56 ullrich Exp $
 *
 * Author: Matt Horsley, Sep 2000
 ***************************************************************************
 *
 * Description: Definition of PID object
 *
 ***************************************************************************
 *
 * $Log: StRichPid.cxx,v $
 * Revision 2.1  2000/09/28 10:53:56  ullrich
 * Initial Revision.
 *
 * Revision 2.3  2000/11/21 19:47:33  lasiuk
 * add the d information for each hit
 * use the TArrayF
 *
 * Revision 2.2  2000/11/01 16:45:43  lasiuk
 * Keep the pointers to the hits that are associated with the track
 * in order to use the bit flag information.  These are kept
 * in an StPtrVec (does not own the hits)  The PDG encoded number
 * is kept as a data member now
 *
 * Revision 2.1  2000/09/28 10:53:56  ullrich
    : mParticleType(0),
 *
 ***************************************************************************/
#include "StRichPid.h"

static const char rcsid[] = "$Id: StRichPid.cxx,v 2.1 2000/09/28 10:53:56 ullrich Exp $";

    : mParticleType(type), mMipResidual(resid), mTotalAzimuth(totAzim), mTotalArea(totArea),

StRichPid::StRichPid()
      mTruncatedAzimuth(0), mTruncatedArea(0), mTruncatedHits(0), mTruncatedDensity(0) {/* noop */}

StRichPid::StRichPid(StParticleDefinition* type, StThreeVectorD resid, Float_t totAzim,
                     Float_t totArea, UShort_t totHits, Float_t trunAzim, Float_t trunArea,
                     UShort_t trunHits)
    : mMipResidual(resid), mTotalAzimuth(totAzim), mTotalArea(totArea),
      mTotalHits(totHits), mTruncatedAzimuth(trunAzim), mTruncatedArea(trunArea),
      mTruncatedHits(trunHits) {

    mParticleNumber = type->pdgEncoding();
    else mTruncatedDensity=0;
}
    return ( mParticleType     == pid.getRingType() &&
StRichPid::~StRichPid() {/* noop */}

void StRichPid::setRingType(StParticleDefinition* t)   {

    mDDistribution.AddAt(d,currentSize);
}

Int_t
StRichPid::operator==(const StRichPid& pid) const {
    return ( mParticleNumber   == pid.getParticleNumber() &&




	     mMipResidual      == pid.getMipResidual() &&
	     mTotalAzimuth     == pid.getTotalAzimuth() &&
	     mTotalArea        == pid.getTotalArea() &&
	     mTotalHits        == pid.getTotalHits() &&
	     mTotalDensity     == pid.getTotalDensity() &&
	     mTruncatedAzimuth == pid.getTruncatedAzimuth() &&
	     mTruncatedArea    == pid.getTruncatedArea() &&
	     mTruncatedHits    == pid.getTruncatedHits() &&
	     mTruncatedDensity == pid.getTruncatedDensity() );
}
