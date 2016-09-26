/***************************************************************************
 *
 * $Id: StMuProbPidTraits.cxx,v 1.4 2016/09/26 22:16:24 fisyak Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#include "StMuProbPidTraits.h"

ClassImp(StMuProbPidTraits) ;

StMuProbPidTraits::StMuProbPidTraits() : mNDF(0),
					 mdEdxFit(0), mdEdxErrorFit(0),
					 mdEdxTruncated(0),mdEdxErrorTruncated(0),
					 mdNdxFit(0),mdNdxErrorFit(0),
					 mdEdxTrackLength(0) {
        memset(mProbabilities, 0, sizeof(mProbabilities));
}
    
/***************************************************************************
 *
 * $Log: StMuProbPidTraits.cxx,v $
 * Revision 1.4  2016/09/26 22:16:24  fisyak
 * Add initialization for dNdx
 *
 * Revision 1.3  2004/10/19 01:34:53  mvl
 * Fixed initialisation in constructor
 *
 * Revision 1.2  2003/11/07 15:23:26  laue
 * added error on dEdx measurements to the StMuProbPidTraits
 *
 * Revision 1.1  2002/11/18 20:28:52  laue
 * New class. Wrapper for Yuri's new StProbPidTraits.
 *
 *
 **************************************************************************/
