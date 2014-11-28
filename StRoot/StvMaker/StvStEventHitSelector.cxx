
// $Id: StvStEventHitSelector.cxx,v 1.1 2013/05/24 16:35:42 perev Exp $
/*!
\author V Perev 2013

Hit selector. Using track info, marks selected hits. Used for hit error fit
to use only hits from "good" tracks
<br>
*/
#include "StEvent/StEvent.h"
#include "StEvent/StHit.h"
#include "StEvent/StTrack.h"
#include "StEvent/StTrackGeometry.h"
#include "StEvent/StTrackNode.h"
#include "StEvent/StTrackDetectorInfo.h"
#include "StarClassLibrary/StThreeVectorF.hh"
#include "StvStEventHitSelector.h"


ClassImp(StvStEventHitSelector)
  
//_____________________________________________________________________________
StvStEventHitSelector::StvStEventHitSelector(const char *name) : TNamed(name,"")
{
  mPtMin = 0.5;
}

//_____________________________________________________________________________
int StvStEventHitSelector::Edit(const StEvent *event)
{

  int nSel = 0;
  if (!event) return 0;
  const StSPtrVecTrackNode& trackNode = event->trackNodes();
  int nTracks = trackNode.size();
  if (!nTracks) 			return -1; //No track info
  for (int itk=0; itk < nTracks; itk++) {
    StTrackNode *node = trackNode[itk]; 
    if (!node) 				continue;
    StTrack *glTrack = node->track(global);
    if (!glTrack) 			continue;
    StTrackGeometry *geo = glTrack->geometry();
    if (!geo) 				continue;
    StThreeVectorF mom = geo->momentum();
    if (mom.perp2()< mPtMin*mPtMin)	continue;
    const StPtrVecHit& hits = glTrack->detectorInfo()->hits();
    int nHits = hits.size();
    if (nHits <15) 			continue;
    for(int ih=0; ih<nHits; ih++){
      StHit* hit = hits[ih];
      if(!hit) 				continue;
      hit->SetBit(kMarked); nSel++;
    }
  }
  return nSel;
}
