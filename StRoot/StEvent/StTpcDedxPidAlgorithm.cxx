/***************************************************************************
 *
 * $Id: StTpcDedxPidAlgorithm.cxx,v 2.15 2002/03/25 21:02:51 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcDedxPidAlgorithm.cxx,v $
 * Revision 2.15  2002/03/25 21:02:51  ullrich
 * Made BetheBloch a static global variable.
 *
 * Revision 2.14  2002/02/06 23:00:54  ullrich
 * Added float.h.
 *
 * Revision 2.13  2001/04/27 21:41:07  ullrich
 * Fixed bug.
 *
 * Revision 2.12  2001/04/24 15:38:08  fisyak
 * Add switch (use length) for calibrated and non calibrated data
 *
 * Revision 2.11  2001/04/05 04:00:56  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.10  2000/10/26 18:33:20  calderon
 * Return DBL_MAX in the numberOfSigma() function when
 * the number of dE/dx points is equal to zero.
 * The function sigmaPidFunction() already checked this, but
 * when this happens, it probably means that mTraits->mean()
 * is undefined, so it's better to exit immediately from the
 * function.
 *
 * Revision 2.9  2000/07/28 16:55:35  calderon
 * Mike's version of StTpcDedxPidAlgorithm using BetheBloch
 * class lookup table from
 * StarClassLibrary and parameterization of resolution obtained
 * from the first two weeks of July 2000 Data.
 *
 * Revision 2.8  2000/05/19 18:33:45  ullrich
 * Minor changes (add const) to cope with modified StArray.
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
#include <float.h>
#include "StTpcDedxPidAlgorithm.h"
#include "StTrack.h"
#include "StParticleTypes.hh"
#include "StEnumerations.h"
#include "StDedxPidTraits.h"
#include "StTrackGeometry.h"
#include "BetheBloch.h"

static BetheBloch theBetheBloch;
static const char rcsid[] = "$Id: StTpcDedxPidAlgorithm.cxx,v 2.15 2002/03/25 21:02:51 ullrich Exp $";

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
        const StDedxPidTraits *p = dynamic_cast<const StDedxPidTraits*>(vec[i]);
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

    if (mTraits->numberOfPoints()==0) return DBL_MAX;
    // sigmaPidFunction already checks this, but when number of dE/dx points is = 0,
    // mTraits->mean() is probably undefined too (have to check this)
    // so might as well exit here.

    // returns the number of sigma a tracks dedx is away from
    // the expected mean for a track for a particle of this mass
    double dedx_expected;
    double dedx_resolution;
    double momentum;
    double z;
    if (mTrack && mTraits->length() > 0) {
      momentum  = abs(mTrack->geometry()->momentum());
      dedx_expected = 1.e-6*BetheBloch::Sirrf(momentum/particle->mass(),mTraits->length(),
					      abs(particle->pdgEncoding())==11);
      dedx_resolution = mTraits->errorOnMean();
      if (dedx_resolution <= 0) dedx_resolution = sigmaPidFunction(particle) ;
    }
    else {
	dedx_expected   = meanPidFunction(particle) ;
	dedx_resolution = sigmaPidFunction(particle) ;
	//     return (mTraits->mean() - dedx_expected)/dedx_resolution ;
    }
    z = log(mTraits->mean()/dedx_expected);
    return z/dedx_resolution ;
}

double
StTpcDedxPidAlgorithm::meanPidFunction(const StParticleDefinition* particle) const
{
    if (!mTrack) return 0;

    double momentum  = abs(mTrack->geometry()->momentum());
    return theBetheBloch(momentum/particle->mass());
}

double
StTpcDedxPidAlgorithm::sigmaPidFunction(const StParticleDefinition* particle) const
{
    if (!mTraits) return 0;
    
    // calcuates average resolution of tpc dedx
    // double dedx_expected = meanPidFunction(particle);
    
    // resolution depends on the number of points used in truncated mean
    
    double nDedxPoints = mTraits->numberOfPoints() ;
    
    return nDedxPoints > 0 ? 0.45  /sqrt(nDedxPoints) : 1000.;
}
