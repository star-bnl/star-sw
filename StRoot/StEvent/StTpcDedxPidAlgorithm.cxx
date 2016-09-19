/***************************************************************************
 *
 * $Id: StTpcDedxPidAlgorithm.cxx,v 2.31 2016/09/18 23:02:11 fisyak Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcDedxPidAlgorithm.cxx,v $
 * Revision 2.31  2016/09/18 23:02:11  fisyak
 * Add dNdx
 *
 * Revision 2.30  2014/01/09 20:06:45  fisyak
 * Add numberOfSigma() for kLikelihoodFitId method
 *
 * Revision 2.29  2012/05/07 14:42:58  fisyak
 * Add handilings for Track to Fast Detectors Matching
 *
 * Revision 2.28  2010/11/15 22:11:45  fisyak
 * Restore proper nSigma
 *
 * Revision 2.27  2010/08/31 20:15:11  fisyak
 * Clean up
 *
 * Revision 2.26  2008/03/13 16:56:39  ullrich
 * Add protection against missing or wrong mTraits->mean()
 *
 * Revision 2.25  2005/05/13 20:31:14  fisyak
 * Calculated nSigma wrt modified Bichsel I70M to account saturation
 *
 * Revision 2.24  2004/03/30 02:45:07  fisyak
 * return DBL_MAX instead of 0 if No mTraits
 *
 * Revision 2.23  2003/10/25 00:12:48  fisyak
 * Replace BetheBloch::Sirrf by m_Bichsel->GetI70 for nSigma calculations
 *
 * Revision 2.22  2003/10/21 14:23:23  fisyak
 * Switch from primary to global track momentum for Nsigma calculations
 *
 * Revision 2.21  2003/09/02 17:58:06  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.20  2003/08/02 01:14:11  perev
 * warnOff
 *
 * Revision 2.19  2003/04/30 18:05:55  fisyak
 * Add P03ia flag, which fixes P03ia MuDst
 *
 * Revision 2.18  2002/04/04 01:42:34  jeromel
 * Extra fix for multiple instantiation (and multiple delete).
 *
 * Revision 2.17  2002/04/01 20:13:56  jeromel
 * Heuu !! if (theBetheBloch) delete ...
 *
 * Revision 2.16  2002/03/26 23:10:09  ullrich
 * Changed instantiaton of BetheBloch.
 *
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
#include <cmath>
#include <float.h>
#include "StTpcDedxPidAlgorithm.h"
#include "StTrack.h"
#include "StGlobalTrack.h"
#include "StParticleTypes.hh"
#include "StEnumerations.h"
#include "StDedxPidTraits.h"
#include "StTrackNode.h"
#include "StTrackGeometry.h"
#if 0
#include "BetheBloch.h"
#endif
#include "StBichsel/Bichsel.h"
#include "StBichsel/StdEdxModel.h"
#include "TMath.h"
static Bichsel *m_Bichsel = 0;
static const char rcsid[] = "$Id: StTpcDedxPidAlgorithm.cxx,v 2.31 2016/09/18 23:02:11 fisyak Exp $";

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
    for (UInt_t i=0; i<vec.size(); i++) {
        const StDedxPidTraits *p = dynamic_cast<const StDedxPidTraits*>(vec[i]);
        if (p && p->detector() == kTpcId && p->method() == mDedxMethod) mTraits = p;
    }
    if (!mTraits) return 0;    // no info available

    //
    //  Scan the list of particles we want to check and
    //  return the most probable.
    //
    Double_t       sigma, minSigma = 100000;
    UInt_t minIndex = mParticles.size();
    for (UInt_t k=0; k<mParticles.size(); k++) {
        if (mParticles[k]->charge()*mTrack->geometry()->charge() > 0)  { // require same charge sign
            if ((sigma = fabs(numberOfSigma(mParticles[k]))) < minSigma) {
                minIndex = k;
                minSigma = fabs(sigma);
            }
        }
    }
    return minIndex < mParticles.size() ? mParticles[minIndex] : 0;
}

Double_t
StTpcDedxPidAlgorithm::numberOfSigma(const StParticleDefinition* particle) const
{
    if (!mTraits) return DBL_MAX;

    if (mTraits->numberOfPoints()==0) return DBL_MAX;
    // sigmaPidFunction already checks this, but when number of dE/dx points is = 0,
    // mTraits->mean() is probably undefined too (have to check this)
    // so might as well exit here.

    // returns the number of sigma a tracks dedx is away from
    // the expected mean for a track for a particle of this mass
    Double_t dedx_expected;
    Double_t dedx_resolution;
    Double_t momentum;
    Double_t z = -999;
    if (mTraits->mean() > 0) {
      const StGlobalTrack *gTrack = 
	static_cast<const StGlobalTrack*>( mTrack->node()->track(global));
      if (gTrack && mTraits->length() > 0 ) {
	Double_t pq  = abs(gTrack->geometry()->momentum())*TMath::Abs(particle->charge());
	Double_t bg = pq/particle->mass();
	if (! m_Bichsel) m_Bichsel = Bichsel::Instance();
	if (mDedxMethod == kTruncatedMeanId) {
	  Double_t log2dX = mTraits->log2dX();
	  if (log2dX <= 0) log2dX = 1;
	  dedx_expected = 1.e-6*m_Bichsel->GetI70M(TMath::Log10(bg),log2dX);
	  dedx_resolution = mTraits->errorOnMean();
	  if (dedx_resolution > 0)
	    z = TMath::Log(mTraits->mean()/dedx_expected)/dedx_resolution;
	} else if (mDedxMethod == kLikelihoodFitId) {
	  dedx_expected = 1.e-6*TMath::Exp(m_Bichsel->GetMostProbableZ(TMath::Log10(bg)));
	  dedx_resolution = mTraits->errorOnMean();
	  if (dedx_resolution > 0)
	    z = TMath::Log(mTraits->mean()/dedx_expected)/dedx_resolution;
	} else if (mDedxMethod == kOtherMethodId) {
	  dedx_expected = StdEdxModel::instance()->dNdx(bg);
	  dedx_resolution = mTraits->errorOnMean();
	  if (dedx_resolution > 0)
	    z = TMath::Log(mTraits->mean()/dedx_expected)/dedx_resolution;
	}
      }
    }
    return z;
}
