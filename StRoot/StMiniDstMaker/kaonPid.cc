/***************************************************************************
 *
 * $Id: kaonPid.cc,v 1.2 2000/10/16 19:35:47 ullrich Exp $
 *
 * Author: Thomas Ullrich, Oct 1999
 ***************************************************************************
 *
 * Description:  
 *
 ***************************************************************************
 *
 * $Log: kaonPid.cc,v $
 * Revision 1.2  2000/10/16 19:35:47  ullrich
 * Updated to run on Sun/CC5.
 *
 * Revision 1.1  2000/10/13 19:26:19  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include <typeinfo>
#include <math.h>
#include "kaonPid.hh"
#include "StTrack.h"
#include "StParticleTypes.hh"
#include "StEnumerations.h"
#include "StDedxPidTraits.h"
#include "StTrackGeometry.h"
#include "BetheBloch.h"


kaonPid::kaonPid()
    : mTraits(0),  mTrack(0)
{/* noop */}

StParticleDefinition*
kaonPid::operator() (const StTrack& track, const StSPtrVecTrackPidTraits& vec)
{
    //
    //  Select the info we need.
    //  Here we select truncated mean in the TPC.
    //
    mTraits = 0;
    mTrack  = &track;
    for (unsigned int i=0; i<vec.size(); i++) {
        const StDedxPidTraits *p = dynamic_cast<StDedxPidTraits*>(vec[i]);
        if (p && p->detector() == kTpcId && p->method() == kTruncatedMeanId) mTraits = p;
    }
    if (!mTraits) return 0;    // no info available

    //
    //  do PID
    //
    BetheBloch   bb;
    StParticleDefinition* particle =
	track.geometry()->charge() > 0 ? (StParticleDefinition*)StKaonPlus::instance() : (StParticleDefinition*)StKaonMinus::instance();

    double p = abs(track.geometry()->momentum());
    double dedx = mTraits->mean();
    double betagamma = p/particle->mass();
    double z = dedx/bb(betagamma);

    return (fabs(log(z)) < 0.2) ? particle : 0;  // return K+ or K-
}

const StDedxPidTraits*
kaonPid::traits() const { return mTraits; }
 
