//*-- Author : James Dunlop
// 
// $Id: StHitFilterMaker.cxx,v 1.7 2013/05/07 18:37:43 jeromel Exp $
// $Log: StHitFilterMaker.cxx,v $
// Revision 1.7  2013/05/07 18:37:43  jeromel
// Modified HitFilter takes a WestEta cut to keep hits in the FGT direction - requested Akio on behalf of the FGT effort
//
// Revision 1.6  2007/04/28 17:56:18  perev
// Redundant StChain.h removed
//
// Revision 1.5  2006/05/17 23:42:42  fisyak
// Use option KeepTpcHit and KeepSvtHit for StHitFilterMaker to keep corresponing hits regardless of track selection
//
// Revision 1.4  2004/09/02 19:13:44  fisyak
// Keep Tpc hits for tracks with Tof Pid Traits
//
// Revision 1.3  2004/04/08 19:28:55  caines
// Make Hitfilter take out those SVT hits not on tracks defined in the constructor - same as TPC filtering
//
// Revision 1.2  2003/07/30 15:27:00  caines
// Set options so you delete TPC and SVT hit if Zert >30. If ZVert<30cm save all good svt hits and TPC hits on tracks
//
// Revision 1.1  2003/02/07 02:16:15  jeromel
// First version of a generlized HitFilter/removal maker. Expeditious review
// done.
//
//


#include "StHitFilterMaker.h"
#include "StMessMgr.h"


#include "StEventTypes.h"
#include <algorithm>
#ifndef ST_NO_NAMESPACES
using std::binary_search;
using std::unique;
using std::sort;

#endif


ClassImp(StHitFilterMaker)

//_____________________________________________________________________________
/// StHitFilterMaker constructor
  StHitFilterMaker::StHitFilterMaker(const char *name, Double_t ptl, Double_t pth, Double_t eta, Double_t zvert, Double_t WestEta): StMaker(name),mPtLowerCut(ptl),mPtUpperCut(pth), mAbsEtaCut(eta), mAbsZVertCut(zvert), mKeepWestHighEtaHits(WestEta){
  //  This has defaults for the cuts in the include file
}

/// Dummy destructor
StHitFilterMaker::~StHitFilterMaker(){
    //NOOP
}

/// Init - is a first method the top level StChain calls to initialize all its makers 
Int_t StHitFilterMaker::Init(){

  // Output the cuts
  gMessMgr->Info() << "StHitFilterMaker::Init():  keeping tracks between      " << 
    mPtLowerCut << " and " << mPtUpperCut << "GeV/c (negative = no cut)" << endm;
  gMessMgr->Info() << "StHitFilterMaker::Init():  keeping tracks with |eta| < " << 
    mAbsEtaCut << endm;
   gMessMgr->Info() << "StHitFilterMaker::Init():  keeping tracks with |ZVert| < " << 
    mAbsZVertCut << endm;
  return StMaker::Init();
}


//_____________________________________________________________________________
/*!
  Get a pointer to stevent using the GetInputDS() method, loops
  over track nodes and select based on the accpt() method. Also
  strips the runaway hits (hits not on tracks).
 */
Int_t StHitFilterMaker::Make(){
    //	Get StEvent
    //
    StEvent* event;
    event = (StEvent *) GetInputDS("StEvent");
    
    
    if (!event) {
        gMessMgr->Info() <<"StHitFilterMaker::Make(): not a valid StEvent: " << endm;
	

	return kStOK;
    }
    // Fill up the ones to keep if ZVert in range
    vector<StTrackNode*> keptTrackNodes;
    if( accept(event)){
      StSPtrVecTrackNode& nodes = event->trackNodes();
      for (StSPtrVecTrackNodeIterator nodeIter = nodes.begin();
	   nodeIter != nodes.end(); ++nodeIter) {
	bool isAcceptedTrack = false;
	
	for (UInt_t ientry=0;
	     ientry < (*nodeIter)->entries(); ++ientry) {
	  StTrack *track = (*nodeIter)->track(ientry);
	  
	  // Check that it's got tpc hits
	  
	  if (!(track->topologyMap().hasHitInDetector(kTpcId))) {
	    continue;
	  }
	  
	  if (accept(track)) {
	    isAcceptedTrack = true;
	    break;
	  }
	}
	if (isAcceptedTrack) {
	  keptTrackNodes.push_back(*nodeIter);
	}
      }
      // Sort it and keep unique
      sort(keptTrackNodes.begin(),keptTrackNodes.end());
      vector<StTrackNode*>::iterator uniquePos = 
	unique(keptTrackNodes.begin(),keptTrackNodes.end());
      keptTrackNodes.erase(uniquePos,keptTrackNodes.end());
    }
    
    gMessMgr->Info() << "StHitFilterMaker::Make(): keeping TPC hits on " <<
      keptTrackNodes.size() << " track nodes " << endm;
    if (! TESTBIT(m_Mode, kTpcId)) 
    this->removeTpcHitsNotOnTracks(event,keptTrackNodes);
    if (! TESTBIT(m_Mode, kSvtId)) {
      this->removeSvtHitsNotOnTracks(event,keptTrackNodes);
      this->removeBadSvtHits(event);
    }
    return kStOK;
}

bool StHitFilterMaker::accept(StEvent *event) {

  if( !(event->primaryVertex()))  return false;
  if( fabs(event->primaryVertex()->position().z()) > mAbsZVertCut) return false;
  return true;
}

bool StHitFilterMaker::accept(StTrack *track) {
    if (track->flag() <= 0) return false;
    StSPtrVecTrackPidTraits &traits = track->pidTraits();
    unsigned int size = traits.size();
    if (size) {
      for (unsigned int i = 0; i < size; i++) {
	if (! traits[i]) continue;
	if ( traits[i]->IsZombie()) continue;
	StTofPidTraits* p = dynamic_cast<StTofPidTraits*>(traits[i]);
	if (p) {return true;}
      }
    }
    if (track->geometry()) {
	if (mPtLowerCut > 0 && track->geometry()->momentum().perp() < mPtLowerCut )
	    return false;
	if (mPtUpperCut > 0 && track->geometry()->momentum().perp() > mPtUpperCut )
	    return false;
	if (mAbsEtaCut > 0 && fabs(track->geometry()->momentum().pseudoRapidity()) > mAbsEtaCut) 
	    return false;
	
    }
    
    return true;
}

bool StHitFilterMaker::accept(StHit *hit) {
  if( hit->flag() > 3) return false;
  return true;
}


/*!
  This is a pretty simple cut and paste + extension from StEventScavenger
  Only difference is that it searches the vector of keptTrackNodes
  for good nodes, rather than relying on zombies.
*/	
bool StHitFilterMaker::removeTpcHitsNotOnTracks(StEvent *event, 
						vector<StTrackNode*>& keptTrackNodes) 
{
  Int_t totalHits = 0;
  Int_t removedHits1 = 0, removedHits2 = 0;
  Int_t keep4fgt1=0, keep4fgt2=0;
    
  if (event && event->tpcHitCollection()) {
    // first remove all hits not associated with a track at all
    StTpcHitCollection *theHits = event->tpcHitCollection();
    for (unsigned int n=0; n<theHits->numberOfSectors(); n++) {
      for (unsigned int m=0; m<theHits->sector(n)->numberOfPadrows(); m++) {
	
	for (unsigned int h=0; h<theHits->sector(n)->padrow(m)->hits().size(); h++) {
	  totalHits++;
	  if (theHits->sector(n)->padrow(m)->hits()[h]->trackReferenceCount() == 0) {
	    if (! (theHits->sector(n)->padrow(m)->hits()[h]->isZombie())) {
	      if(mKeepWestHighEtaHits<=0.0 || 
		 checkHitTowardFgt(theHits->sector(n)->padrow(m)->hits()[h])==0 ){
		theHits->sector(n)->padrow(m)->hits()[h]->makeZombie();
		++removedHits1;
	      }else{
		keep4fgt1++;
	      }
	    }
	  }
	}
      }
    }
    gMessMgr->Info() << "StHitFilterMaker::removeTpcHitsNotOnTracks.  Total "<<totalHits<<
      ", Removed " << removedHits1 << " TPC hits not on any tracks" << endm;

    if(mKeepWestHighEtaHits>0.0) gMessMgr->Info() << " But keeping "<<keep4fgt1<<" hits for fgt"<<endm;

    // now all hits not associated 
    StSPtrVecTrackNode& nodes = event->trackNodes();
    for (unsigned int i = 0; i < nodes.size(); i++) {   // loop nodes
      StTrackNode* node = nodes[i];
      if (!binary_search(keptTrackNodes.begin(),
			 keptTrackNodes.end(),
			 node)) 
	{
	  
	  for (unsigned int j = 0; j < node->entries(); j++) {   // loop tracks in node
	    StTrack* track = node->track(j);
	    // Don't do anything if it doesn't have a TPC hit
	    if (!(track->topologyMap().hasHitInDetector(kTpcId))) {
	      continue;
	    }
	    
	    StTrackDetectorInfo* info = track->detectorInfo();
	    if (info) {
	      StPtrVecHit& hitList = info->hits();
	      for (unsigned int k = 0; k < hitList.size(); k++)   // loop hits
		if (hitList[k]->detector() == kTpcId && 
		    !(hitList[k]->isZombie()) ) {
		  if(mKeepWestHighEtaHits<=0.0 || checkHitTowardFgt(hitList[k])==0 ){
		    hitList[k]->makeZombie();
		    ++removedHits2;
		  }else{
		     keep4fgt2++;
		  }
		}
	      
	    }
	  }
	}
    }
    gMessMgr->Info() << "StHitFilterMaker::removeTpcHitsNotOnTracks.  Removed " <<
      removedHits2 << " TPC hits not on passed tracks" << endm;
    if(mKeepWestHighEtaHits>0.0) gMessMgr->Info() << " But keeping "<<keep4fgt2<<" hits for fgt"<<endm;
    gMessMgr->Info() << "StHitFilterMaker::removeTpcHitsNotOnTracks.  Saving " <<
      (totalHits - removedHits1 - removedHits2) << " after filter" << endm;
    
    return true;
  }
  else
    return false;
}

/*!
  searches the vector of keptTrackNodes
  for good nodes, rather than relying on zombies. and removes SVT hits not on tracks
*/	
bool StHitFilterMaker::removeSvtHitsNotOnTracks(StEvent *event, 
						vector<StTrackNode*>& keptTrackNodes) 
{
  Int_t removedHits = 0;
    
  if (event && event->svtHitCollection()) {
    // first remove all hits not associated with a track at all
    StSvtHitCollection *theHits = event->svtHitCollection();
    for (unsigned int l=0; l<theHits->numberOfBarrels(); l++) {
      for (unsigned int m=0; m<theHits->barrel(l)->numberOfLadders(); m++) {
	for (unsigned int n=0; n<theHits->barrel(l)->ladder(m)->numberOfWafers(); n++) {
	  for (unsigned int h=0; h<theHits->barrel(l)->ladder(m)->wafer(n)->hits().size(); h++) {   
	    
	    if (theHits->barrel(l)->ladder(m)->wafer(n)->hits()[h]->trackReferenceCount() == 0) {
	      if (! (theHits->barrel(l)->ladder(m)->wafer(n)->hits()[h]->isZombie())) {
		theHits->barrel(l)->ladder(m)->wafer(n)->hits()[h]->makeZombie();
		++removedHits;
	      }
	    }
	  }
	}
      }
    }

    gMessMgr->Info() << "StHitFilterMaker::removedSvtHitsNotOnTracks.  Removed " <<
      removedHits << " SVT hits not on any tracks" << endm;
    removedHits = 0;
	
    // now all hits not associated 
    StSPtrVecTrackNode& nodes = event->trackNodes();
    for (unsigned int i = 0; i < nodes.size(); i++) {   // loop nodes
      StTrackNode* node = nodes[i];
      if (!binary_search(keptTrackNodes.begin(),
			 keptTrackNodes.end(),
			 node)) 
	{
	  
	  for (unsigned int j = 0; j < node->entries(); j++) {   // loop tracks in node
	    StTrack* track = node->track(j);
	    // Don't do anything if it doesn't have a SVT hit
	    if (!(track->topologyMap().hasHitInDetector(kSvtId))) {
	      continue;
	    }
	    
	    StTrackDetectorInfo* info = track->detectorInfo();
	    if (info) {
	      StPtrVecHit& hitList = info->hits();
	      for (unsigned int k = 0; k < hitList.size(); k++)   // loop hits
		if (hitList[k]->detector() == kSvtId && 
		    !(hitList[k]->isZombie()) ) {
		  hitList[k]->makeZombie();
		  ++removedHits;
		}
	      
	    }
	  }
	}
    }
    gMessMgr->Info() << "StHitFilterMaker::removeSvtHitsNotOnTracks.  Removed " <<
      removedHits << " SVT hits not on passed tracks" << endm;
    
    return true;
  }
  else
    return false;
}


/*!
  This is a pretty simple cut and paste + extension from StEventScavenger
  Only difference is that it searches the vector of keptTrackNodes
  for good nodes, rather than relying on zombies.
*/	
bool StHitFilterMaker::removeBadSvtHits(StEvent *event) 
{
  Int_t removedHits = 0;
  
  if (event && event->svtHitCollection()) {
    // first remove all hits with flag > 3
    StSvtHitCollection *theHits = event->svtHitCollection();
    for (unsigned int l=0; l<theHits->numberOfBarrels(); l++) {
      for (unsigned int m=0; m<theHits->barrel(l)->numberOfLadders(); m++) {
	for (unsigned int n=0; n<theHits->barrel(l)->ladder(m)->numberOfWafers(); n++) {
	  for (unsigned int h=0; h<theHits->barrel(l)->ladder(m)->wafer(n)->hits().size(); h++) {   
	    if (! (accept(theHits->barrel(l)->ladder(m)->wafer(n)->hits()[h])) ||
		!accept(event)) {
	      if (! (theHits->barrel(l)->ladder(m)->wafer(n)->hits()[h]->isZombie())) {
		theHits->barrel(l)->ladder(m)->wafer(n)->hits()[h]->makeZombie();
		++removedHits;
	      }
	    }
	  }
	}
      }
    }

    gMessMgr->Info() << "StHitFilterMaker::removeBadSvtHits.  Removed " <<
      removedHits << " bad SVT hits" << endm;
  return true;
  }
  else
    return false;
}


Int_t StHitFilterMaker::checkHitTowardFgt(StHit* hit){
  if(hit->position().z() < 0) return 0;
  if(hit->position().pseudoRapidity()>mKeepWestHighEtaHits) return 1;
  return 0;
}

