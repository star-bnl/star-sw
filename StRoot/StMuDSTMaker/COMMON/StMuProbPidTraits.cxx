/***************************************************************************
 *
 * $Id: StMuProbPidTraits.cxx,v 1.2 2003/11/07 15:23:26 laue Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#include "StMuProbPidTraits.h"

ClassImp(StMuProbPidTraits) ;

StMuProbPidTraits::StMuProbPidTraits() : mNDF(0),
					 mdEdxFit(0), mdEdxErrorFit(0),
					 mdEdxTruncated(0),mdEdxErrorTruncated(0),
					 mdEdxTrackLength(0) {
        memset(mProbabilities, 0, sizeof(mProbabilities[__NPARTICLES__]));
}
    
/***************************************************************************
 *
 * $Log: StMuProbPidTraits.cxx,v $
 * Revision 1.2  2003/11/07 15:23:26  laue
 * added error on dEdx measurements to the StMuProbPidTraits
 *
 * Revision 1.1  2002/11/18 20:28:52  laue
 * New class. Wrapper for Yuri's new StProbPidTraits.
 *
 *
 **************************************************************************/
