/***************************************************************************
 *
 * $Id: StMuL3Filter.cxx,v 1.5 2003/10/20 19:50:13 perev Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#include "StMuL3Filter.h"
#include "StMuDebug.h"
#include "StMuException.hh"
#include "StEvent/StTrack.h"
#include "StEvent/StTrackGeometry.h"
#include "StEvent/StTrackDetectorInfo.h"
#include "StEvent/StContainers.h"
#include "StEvent/StDedxPidTraits.h"


#include "StarClassLibrary/BetheBloch.h"

ClassImp(StMuL3Filter)

bool StMuL3Filter::accept( const StEvent* e) { cout << "StMuL3Filter::accept( const StEvent* e) not overwritten, returning true" << endl; return true;}
bool StMuL3Filter::accept( const StV0Vertex* v) { cout << "StMuL3Filter::accept(const StV0Vertex* v) not overwritten, returning true" << endl; return true;}
bool StMuL3Filter::accept( const StXiVertex* x) { cout << "StMuL3Filter::accept(const StXiVertex* x) not overwritten, returning true" << endl; return true;}
bool StMuL3Filter::accept( const StKinkVertex* k) { cout << "StMuL3Filter::accept(const StKinkVertex* k) not overwritten, returning true" << endl; return true;}
bool StMuL3Filter::accept( const StV0MuDst* v) { cout << "StMuL3Filter::accept(const StV0MuDst* v) not overwritten, returning true" << endl; return true;}
bool StMuL3Filter::accept( const StXiMuDst* x) { cout << "StMuL3Filter::accept(const StXiMuDst* x) not overwritten, returning true" << endl; return true;}
bool StMuL3Filter::accept( const StKinkMuDst* k) { cout << "StMuL3Filter::accept(const StKinkMuDst* k) not overwritten, returning true" << endl; return true;}
 

StMuL3Filter::StMuL3Filter()  {
  DEBUGMESSAGE3("");
  cerr << "StMuL3Filter::StMuL3Filter(): called. Next BetheBloch instance is made." << endl;
  mBB = new BetheBloch();
  cerr << "StMuL3Filter::StMuL3Filter(): did you see the BetheBloch warning?" << endl;
}

StMuL3Filter::~StMuL3Filter()  {
  delete mBB; mBB = 0;
}

bool StMuL3Filter::accept(const StTrack* track) {
  
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
	      //	      if (mBB==0) mBB = new BetheBloch();
	      float p = track->geometry()->momentum().magnitude();
	      float dedxHigh = dEdxFractionCutHigh * mBB->Sirrf(p/dEdxMassCutHigh);
	      float dedxLow = dEdxFractionCutLow * mBB->Sirrf(p/dEdxMassCutLow);

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
 * $Log: StMuL3Filter.cxx,v $
 * Revision 1.5  2003/10/20 19:50:13  perev
 * workaround added for TClonesArray::Delete + some cleanup of MuEmc
 *
 * Revision 1.4  2003/01/23 21:59:50  laue
 * Modification to compile on Solaris.
 *
 * Revision 1.3  2002/05/04 23:56:30  laue
 * some documentation added
 *
 * Revision 1.2  2002/03/20 16:04:12  laue
 * minor changes, mostly added access functions
 *
 * Revision 1.1  2002/03/08 17:04:18  laue
 * initial revision
 *
 *
 **************************************************************************/
