/***************************************************************************
 *
 * $Id: StarMuL3Filter.cxx,v 1.1 2002/03/05 15:41:10 jeromel Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#include "StarMuL3Filter.h"
#include "StEvent/StTrack.h"
#include "StEvent/StTrackGeometry.h"
#include "StEvent/StTrackDetectorInfo.h"
#include "StEvent/StContainers.h"
#include "StEvent/StDedxPidTraits.h"


#include "StarClassLibrary/BetheBloch.h"

ClassImp(StarMuL3Filter)

bool StarMuL3Filter::accept( const StEvent* e) { cout << "StarMuL3Filter::accept( const StEvent* e) not overwritten, returning true" << endl; return true;}
bool StarMuL3Filter::accept( const StV0Vertex* v) { cout << "StarMuL3Filter::accept(const StV0Vertex* v) not overwritten, returning true" << endl; return true;}
bool StarMuL3Filter::accept( const StXiVertex* x) { cout << "StarMuL3Filter::accept(const StXiVertex* x) not overwritten, returning true" << endl; return true;}
bool StarMuL3Filter::accept( const StKinkVertex* k) { cout << "StarMuL3Filter::accept(const StKinkVertex* k) not overwritten, returning true" << endl; return true;}



bool StarMuL3Filter::accept(const StTrack* track) {
  static BetheBloch* bb = 0;
  
  float pCutHigh        = 2.0;    // high momentum cut for RICH/Upsilon candidates 
  int   nHitsCutHighP   = 10;     // nHits cut for all tracks

  // following cuts apply only for tracks with pCutLow < p <pHigh
  float pCutLow             = 0.2;    // low momentum cut
  int   nHitsCutLowP        = 15;    
  int   chargeForLowP       = -1;     // charge for tracks with pCutLow < p < pCutHigh, set to 0 for all tracks
  float dEdxMassCutHigh     = 0.939;  // cut below BetheBloch(p/dEdxMassCutHigh), e.g. proton-band
  float dEdxFractionCutHigh = 0.6;    // cut fraction of dEdx-band, i.e. dEdxFractionCut * BetheBloch(p/dEdxMassCut)
  float dEdxMassCutLow      = 0.494;  // cut above BetheBloch(p/dEdxMassCutLow), e.g. kaon-band
  float dEdxFractionCutLow  = 1.1;

  int iret = 0;

  // next: take all tracks above pCutHigh
  if (track->geometry()->momentum().magnitude() > pCutHigh
      && track->detectorInfo()->numberOfPoints() >= nHitsCutHighP)
        iret = 1;

  // otherwise: take only neg. tracks in a certain dEdx-range
  else {
        if (track->detectorInfo()->numberOfPoints() >= nHitsCutLowP
	    && track->geometry()->momentum().magnitude() > pCutLow) {

	      int chargeOK = 0;
	      int dedxOK = 0;

	      // check charge
	      if (chargeForLowP==0) 
		    chargeOK = 1;
	      else if (track->geometry()->charge() == chargeForLowP) 
		    chargeOK = 1;

	      // check dEdx
	      if (bb==0) bb = new BetheBloch();
	      float p = track->geometry()->momentum().magnitude();
	      float dedxHigh = dEdxFractionCutHigh * bb->Sirrf(p/dEdxMassCutHigh);
	      float dedxLow = dEdxFractionCutLow * bb->Sirrf(p/dEdxMassCutLow);

	      float dedx = 0;
	      // get track dEdx
	      const StSPtrVecTrackPidTraits& traits = track->pidTraits();
	      StDedxPidTraits* dedxPidTr;
	      for (unsigned int itrait = 0; itrait < traits.size(); itrait++){
		    dedxPidTr = 0;
		    if (traits[itrait]->detector() == kTpcId) {
		          StTrackPidTraits* thisTrait = traits[itrait];
			  dedxPidTr = dynamic_cast<StDedxPidTraits*>(thisTrait);
			  if (dedxPidTr && dedxPidTr->method() == kTruncatedMeanId) {
			        // adjust L3 dE/dx by a factor of 2 to match offline
			        dedx = 2 * dedxPidTr->mean();
			  }
		    }
	      }
	      if (dedx > dedxHigh && dedx > dedxLow) 
		    dedxOK = 1;

	      // final answer
	      iret = chargeOK * dedxOK;
	} // if (pCutLow && nHitsCutLowP)

  }

  return (bool)iret;
}


/***************************************************************************
 *
 * $Log: StarMuL3Filter.cxx,v $
 * Revision 1.1  2002/03/05 15:41:10  jeromel
 * First version of Frank's Commone MicroDST.
 *
 *
 **************************************************************************/
