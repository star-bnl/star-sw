/***************************************************************************
 *
 * $Id: StTpcDedxPidAlgorithm.cxx,v 2.7 2000/04/20 16:47:31 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcDedxPidAlgorithm.cxx,v $
 * Revision 2.7  2000/04/20 16:47:31  ullrich
 * Check for null pointer added.
 *
 * Revision 2.7  2000/04/20 16:47:31  ullrich
 * Check for null pointer added.
 *
 * Revision 2.6  2000/03/02 12:43:49  ullrich
 * Method can be passed as argument to constructor. Default
 * method is truncated mean.
 *
 * Revision 2.5  1999/12/21 15:09:11  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 2.4  1999/12/03 13:18:18  ullrich
 * Fixed problem on Sun CC4.2 (dynamic_cast) and switched
 * off cut on number of points.
 *
 * Revision 2.3  1999/12/02 16:35:34  ullrich
 * Added method to return the stored dE/dx traits
 *
 * Revision 2.2  1999/10/28 22:27:01  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:45:24  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <typeinfo>
#include <math.h>
#include "StTpcDedxPidAlgorithm.h"
#include "StTrack.h"
#include "StParticleTypes.hh"
#include "StEnumerations.h"
#include "StDedxPidTraits.h"
#include "StTrackGeometry.h"

static const char rcsid[] = "$Id: StTpcDedxPidAlgorithm.cxx,v 2.7 2000/04/20 16:47:31 ullrich Exp $";

StTpcDedxPidAlgorithm::StTpcDedxPidAlgorithm(StDedxMethod dedxMethod)
    : mTraits(0),  mTrack(0), mDedxMethod(dedxMethod)
{
    //
    //  Add all particles we want to get
    //  checked in operator().
    //
    mParticles.push_back(StPionMinus::instance());
    mParticles.push_back(StPionPlus::instance());
    mParticles.push_back(StKaonMinus::instance());
    mParticles.push_back(StKaonPlus::instance());
    mParticles.push_back(StProton::instance());
}

StParticleDefinition*
StTpcDedxPidAlgorithm::operator() (const StTrack& track, const StSPtrVecTrackPidTraits& vec)
{
    //
    //  Select the info we need.
    //  Here we ignore different kinds of dE/dx calculations in
    //  the TPC and select the method
    //
    mTraits = 0;
    mTrack  = &track;
    for (unsigned int i=0; i<vec.size(); i++) {
#if defined (__SUNPRO_CC) && __SUNPRO_CC < 0x500
        const StDedxPidTraits *p = dynamic_cast<StDedxPidTraits*>((StTrackPidTraits*)vec[i]);
#else
        const StDedxPidTraits *p = dynamic_cast<StDedxPidTraits*>(vec[i]);
#endif
        if (p && p->detector() == kTpcId && p->method() == mDedxMethod) mTraits = p;
    }
    if (!mTraits) return 0;    // no info available

    //
    //  Scan the list of particles we want to check and
    //  return the most probable.
    //
    double       sigma, minSigma = 100000;
    unsigned int minIndex = mParticles.size();
    for (unsigned int k=0; k<mParticles.size(); k++) {
        if (mParticles[k]->charge()*mTrack->geometry()->charge() > 0)  { // require same charge sign
            if ((sigma = fabs(numberOfSigma(mParticles[k]))) < minSigma) {
                minIndex = k;
                minSigma = fabs(sigma);
            }
        }
    }
    return minIndex < mParticles.size() ? mParticles[minIndex] : 0;
}

const StDedxPidTraits*
StTpcDedxPidAlgorithm::traits() const { return mTraits; }
 
double
StTpcDedxPidAlgorithm::numberOfSigma(const StParticleDefinition* particle) const
{
    if (!mTraits) return 0;
    
    // returns the number of sigma a tracks dedx is away from
    // the expected mean for a track for a particle of this mass
            
    double dedx_expected   = meanPidFunction(particle) ;
    double dedx_resolution = sigmaPidFunction(particle) ;
    
    return (mTraits->mean() - dedx_expected)/dedx_resolution ;
}

double
StTpcDedxPidAlgorithm::meanPidFunction(const StParticleDefinition* particle) const
{
    if (!mTrack) return 0;

    double momentum  = abs(mTrack->geometry()->momentum());
    
    // placeholder constants for charcaterizeing bethe-bloch curve
    // use some data-base solution
    
    double bpar[3] = {0.1221537e-06,-4.608514, 5613.} ;
    
    double gamma =sqrt(pow(momentum/particle->mass(),2)+1.);
    double beta = sqrt(1. - 1./pow(gamma,2));
    double rise = bpar[2]*pow(beta*gamma,2);

    return beta > 0 ? bpar[0]/pow(beta,2)*(0.5*log(rise)-pow(beta,2)-bpar[1]) : 1000;
}

double
StTpcDedxPidAlgorithm::sigmaPidFunction(const StParticleDefinition* particle) const
{
    if (!mTraits) return 0;
    
    // calcuates average resolution of tpc dedx
    double dedx_expected = meanPidFunction(particle);
    
    // resolution depends on the number of points used in truncated mean
    
    double nhit = mTraits->numberOfPoints() ;
    
    return nhit > 0 ? 0.4 * dedx_expected/sqrt(nhit) : 1000.;
}
