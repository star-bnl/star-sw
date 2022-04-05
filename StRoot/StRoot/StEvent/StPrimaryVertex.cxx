/***************************************************************************
 *
 * $Id: StPrimaryVertex.cxx,v 2.20 2013/01/15 23:31:05 fisyak Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StPrimaryVertex.cxx,v $
 * Revision 2.20  2013/01/15 23:31:05  fisyak
 * Soft requirement for matching with EMC
 *
 * Revision 2.19  2013/01/15 23:21:06  fisyak
 * improve printouts
 *
 * Revision 2.18  2012/10/23 20:18:33  fisyak
 * Add/modify print outs
 *
 * Revision 2.17  2012/09/16 21:37:13  fisyak
 * Add no. of Tpc West Only and East only tracks
 *
 * Revision 2.16  2012/05/07 14:42:58  fisyak
 * Add handlings for Track to Fast Detectors Matching
 *
 * Revision 2.15  2009/11/23 22:25:21  ullrich
 * Added new member mNumMatchesWithBTOF and related access fcts.
 *
 * Revision 2.14  2009/11/23 16:34:06  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.13  2006/04/07 18:21:28  ullrich
 * Added data member mMeanDip incl. access functions (Marco).
 *
 * Revision 2.12  2005/07/15 20:17:35  ullrich
 * Corrected spelling in membrane
 *
 * Revision 2.11  2005/06/15 21:50:32  ullrich
 * Added members and methods to identify used vertex finder and store vertex quality.
 *
 * Revision 2.10  2004/07/15 16:36:24  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.9  2003/09/02 17:58:05  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.8  2002/04/18 23:38:21  jeromel
 * Implementation of the SVT 2 tables scheme ...
 *
 * Revision 2.7  2001/04/05 04:00:52  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.6  2001/03/24 03:34:53  perev
 * clone() -> clone() const
 *
 * Revision 2.5  2000/04/03 15:30:23  ullrich
 * addDaughter() now assigns the right vertex, i.e. this, to
 * the primary tracks stored within this primary vertex.
 *
 * Revision 2.4  1999/11/09 15:44:11  ullrich
 * Removed method unlink() and all calls to it.
 *
 * Revision 2.3  1999/11/04 20:36:17  ullrich
 * New method to obtain daughter container directly
 *
 * Revision 2.2  1999/10/28 22:26:16  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:45:02  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <Stiostream.h>
#include "StPrimaryVertex.h"
#include "StPrimaryTrack.h"
#include "StTrack.h"
#include "StFunctional.h"
#include "StTrackNode.h"
#include "StGlobalTrack.h"
#include "TMath.h"
#include "StTrackGeometry.h"
ClassImp(StPrimaryVertex)

static const char rcsid[] = "$Id: StPrimaryVertex.cxx,v 2.20 2013/01/15 23:31:05 fisyak Exp $";

StPrimaryVertex::StPrimaryVertex()
{init();}

void StPrimaryVertex::init()
{
    mType = kEventVtxId;
    mVertexFinderId = undefinedVertexFinder; 
    memset(mBeg, 0, mEnd-mBeg+1);
}

StPrimaryVertex::~StPrimaryVertex() {/* noop */};

StVertexId
StPrimaryVertex::type() const { return kEventVtxId; }

UInt_t
StPrimaryVertex::numberOfDaughters() const
{
    return mDaughters.size();
}
UInt_t StPrimaryVertex::numberOfGoodTracks() const {
  UInt_t no = 0;
  for (UInt_t i=0; i<mDaughters.size(); i++) {
    const StTrack *track = daughter(i);
    if (track && track->flag() >= 0 && track->fitTraits().numberOfFitPoints() >=  NoFitPointCutForGoodTrack()) no++;
  }  
  return no;
}

StTrack*
StPrimaryVertex::daughter(UInt_t i)
{
    return i < mDaughters.size() ? mDaughters[i] : 0;
}

const StTrack*
StPrimaryVertex::daughter(UInt_t i) const
{
    return i < mDaughters.size() ? mDaughters[i] : 0;
}

StPtrVecTrack
StPrimaryVertex::daughters(StTrackFilter& filter)
{
    StPtrVecTrack vec;
    for (UInt_t i=0; i<mDaughters.size(); i++)
	if (filter(mDaughters[i])) vec.push_back(mDaughters[i]);
    return vec;
}

void
StPrimaryVertex::addDaughter(StTrack* t)
{
    StPrimaryTrack* p = dynamic_cast<StPrimaryTrack*>(t);
    if (p) {
	if (p->type() == primary) {
	    mDaughters.push_back(p);
	    p->setVertex(this);
	}
    }
}

void
StPrimaryVertex::removeDaughter(StTrack* t)
{
    StPrimaryTrack* p = dynamic_cast<StPrimaryTrack*>(t);
    if (!p) return;
    StSPtrVecPrimaryTrackIterator iter;
    if (p->type() == primary) {
	for (iter=mDaughters.begin(); iter != mDaughters.end(); iter++)
	    if (*iter == t) {
		mDaughters.erase(iter);
		p->setVertex(0);
	    }
    }
}

void
StPrimaryVertex::setParent(StTrack*)
{
    cerr << "StPrimaryVertex::setParent(): StPrimaryVertex cannot have a parent." << endl;
}
//________________________________________________________________________________
void StPrimaryVertex::setTrackNumbers() {
  mNumMatchesWithTOF = 0;
  mNumMatchesWithCTB = 0;
  mNumMatchesWithBEMC = 0;
  mNumMatchesWithEEMC = 0;
  mNumNotMatchesWithTOF = 0;
  mNumNotMatchesWithCTB = 0;
  mNumNotMatchesWithBEMC = 0;
  mNumNotMatchesWithEEMC = 0;
  mNumTracksCrossingCentralMembrane = 0;
  mNumPostXTracks = 0;
  mNumTracksWithPromptHit = 0;
  mNumTracksTpcWestOnly = mNumTracksTpcEastOnly = 0;
  mMeanDip = 0;
  mSumOfTrackPt = 0;
  UInt_t nDaughters = numberOfDaughters();
  UShort_t n_trk_vtx = 0;
  for (UInt_t i = 0; i < nDaughters; i++) {
    StPrimaryTrack* pTrack = (StPrimaryTrack*) daughter(i);
    if (! pTrack) continue;
    n_trk_vtx++;
    StThreeVectorD g3 = pTrack->geometry()->momentum();
    mMeanDip += TMath::PiOver2() - g3.theta();
    mSumOfTrackPt   += g3.perp();
    if (! pTrack->flagExtension()) { // check consitency with global track
      const StTrackNode* node = pTrack->node();
      const StTrack *gTrack = node->track(global);
      if (gTrack) pTrack->setFlagExtension(gTrack->flagExtension());
    }
    if (pTrack->isCtbMatched()    )        mNumMatchesWithCTB++;    
    if (pTrack->isCtbNotMatched() ) 	   mNumNotMatchesWithCTB++; 
    if (pTrack->isToFMatched()    ) 	   mNumMatchesWithTOF++;    
    if (pTrack->isToFNotMatched() ) 	   mNumNotMatchesWithTOF++; 
    if ((pTrack->flagExtension() & 7) > 0) {
      if (pTrack->isBemcMatched()   ) 	   mNumMatchesWithBEMC    += 1;// << ((pTrack->flagExtension() & 7) - 1);
      if (pTrack->isEemcMatched()   ) 	   mNumMatchesWithEEMC    += 1;// << ((pTrack->flagExtension() & 7) - 1);
    }
    if (pTrack->isBemcNotMatched())        mNumNotMatchesWithBEMC++;
    if (pTrack->isEemcNotMatched()) 	   mNumNotMatchesWithEEMC++;
    if (pTrack->isMembraneCrossingTrack()) mNumTracksCrossingCentralMembrane++;
    if (pTrack->isPostXTrack())            mNumPostXTracks++;
    if (pTrack-> isPromptTrack())          mNumTracksWithPromptHit++;
    if (pTrack->isWestTpcOnly())           mNumTracksTpcWestOnly++;
    if (pTrack->isEastTpcOnly())           mNumTracksTpcEastOnly++;
  }
  if (n_trk_vtx > 0) mMeanDip /= n_trk_vtx;
}
//________________________________________________________________________________
ostream&  operator<<(ostream& os,  const StPrimaryVertex& v) {
  UInt_t nGoodTpcTracks = 0, nTpcTracks = 0;
  UInt_t nDaughters = v.numberOfDaughters();
  for (UInt_t i=0; i < nDaughters; i++) {
    StPrimaryTrack* pTrack = (StPrimaryTrack*) v.daughter(i);
    if (! pTrack) continue;
    Int_t good = (pTrack->flag() > 0 && pTrack->fitTraits().numberOfFitPoints() >=  StVertex::NoFitPointCutForGoodTrack()) ? 1 : 0;
    if (pTrack->fitTraits().numberOfFitPoints(kTpcId)) {
      nTpcTracks++; nGoodTpcTracks+=good;
    } 
  }
  const Char_t *beam = (v.isBeamConstrained()) ? "B" : " ";
  //  os << Form("%2s:C/P/X/T/E %i/%i/%i/%i/%i: %8.3f,%8.3f,%8.3f",
  os << Form("%1s:",beam);
  if (v.numPostXTracks() < 10) os << Form("%i/",v.numPostXTracks());
  else                                os <<       "*/";
  if (v.numTracksWithPromptHit() < 10) os << Form("%i/",v.numTracksWithPromptHit());
  else                                os <<       "*/";
  if (v.numTracksCrossingCentralMembrane() < 10) os << Form("%i/",v.numTracksCrossingCentralMembrane());
  else                                os <<       "*/";
  if ((v.numMatchesWithCTB()+v.numMatchesWithBTOF()) < 10) os << Form("%i/",(v.numMatchesWithCTB()+v.numMatchesWithBTOF()));
  else                                os <<       "*/";
  if ((v.numMatchesWithBEMC()+v.numMatchesWithEEMC()) < 10) os << Form("%i/",(v.numMatchesWithBEMC()+v.numMatchesWithEEMC()));
  else                                os <<       "*/";
  if (v.numTracksTpcWestOnly() < 10) os << Form("%i/",v.numTracksTpcWestOnly());
  else                                os <<       "*/";
  if (v.numTracksTpcEastOnly() < 10) os << Form("%i",v.numTracksTpcEastOnly());
  else                                os <<       "*";
  const Float_t *xyz = v.position().xyz();
  const Float_t *dxyz = v.positionError().xyz();
  for (Int_t i = 0; i < 3; i++)     os << Form("%8.3f+/-%5.3f,",xyz[i],dxyz[i]);
  os << " Prob/Chi2: " << Form("%5.3f/%7.2f",v.probChiSquared(),v.chiSquared())
     << " Rank: "      << Form("%8.1f",v.ranking())
    << Form(" U/T/G: %4i,%4i,%4i", v.numTracksUsedInFinder(),nDaughters,v.numberOfGoodTracks());
  if (nTpcTracks != nDaughters || nGoodTpcTracks != v.numberOfGoodTracks()) {
    os << Form(" TPC:%4i,%4i",nTpcTracks,nGoodTpcTracks);
  }
  if (v.idTruth())
    os << Form(" IdT: %5i Q: %4i", v.idTruth(), v.qaTruth());
  return os;
}

