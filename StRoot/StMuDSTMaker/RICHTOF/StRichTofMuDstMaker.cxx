/***************************************************************************
 *
 * $Id: StRichTofMuDstMaker.cxx,v 1.5 2002/03/10 17:59:33 dunlop Exp $
 *
 * Author: Thomas Ullrich, Oct 2000
 ***************************************************************************
 *
 * Description:  Example program to write miniDSTs based on StEvent.
 *               The miniDST will contain primary K+/K- tracks only.
 *
 ***************************************************************************
 *
 * $Log: StRichTofMuDstMaker.cxx,v $
 * Revision 1.5  2002/03/10 17:59:33  dunlop
 * More clever with removing of RICH collection and pid traits when not wanted.
 *
 * Revision 1.4  2002/03/10 15:49:54  dunlop
 * Remove StRichCollection if detectorState(kRichId) is bad
 *
 * Revision 1.3  2002/03/09 18:46:49  dunlop
 * Modified logic when bad RICH event (don't try v0's, don't keep l3).
 * Keep event even if no rich or tof tracks, for scalars in minbias.
 *
 * Revision 1.2  2002/02/23 01:54:56  dunlop
 * Tightened cut on tof phi
 *
 * Revision 1.1  2002/02/20 01:57:26  dunlop
 * New Rich and Tof combined maker
 *
 * Revision 1.2  2000/10/16 19:35:41  ullrich
 * Updated to run on Sun/CC5.
 *
 * Revision 1.1  2000/10/13 19:26:17  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include <typeinfo>
#include "StRichTofMuDstMaker.h"
#include "StChain.h"
#include "StEventTypes.h"
#include "StMessMgr.h"
#include "SystemOfUnits.h"
#include "StGlobals.hh"
#include "StEventScavenger.h"
#include "StMessMgr.h"
#include "StEventScavenger.h"
#include "StTofUtil/StTofGeometry.h"
#include "StThreeVectorD.hh"

#include "StPhysicalHelixD.hh"


#include "StStrangeMuDstMaker/StStrangeMuDstMaker.h"
#include "StStrangeMuDstMaker/StV0MuDst.hh"

#include "StEventUtilities/StuRefMult.hh"
#include "StRrsMaker/StGlobalCoordinate.h"
#include "StRrsMaker/StRichRawCoordinate.h"
#include "StRrsMaker/StRichCoordinateTransform.h"
#include "StRrsMaker/StRichLocalCoordinate.h"
#include "StRrsMaker/StRichMomentumTransform.h"
#include "StRrsMaker/StRichGeometryDb.h"

static size_t acceptedEvents = 0;
static size_t acceptedTracks = 0;

ClassImp(StRichTofMuDstMaker)

    StRichTofMuDstMaker::StRichTofMuDstMaker(const Char_t *name) : StMaker(name)
{/* noop */ }

Int_t StRichTofMuDstMaker::Init()
{
    mRejectStrobeEvents = true;
    mOuterTrackGeometry = true;
    mTrackAcceptZ_min =   -250*centimeter; // default Z-range: East Barrel
    mTrackAcceptZ_max =      0*centimeter;
    mTrackAcceptPhi_min = -1.25*radian;     // default phi-range: 3 trays
    mTrackAcceptPhi_max = -1.05*radian;
 // L3 projection
    mRichGlobalEdgeMin.setX(83.031);
    mRichGlobalEdgeMax.setX(157.511);

    mRichGlobalEdgeMin.setY(-229.452);
    mRichGlobalEdgeMax.setY(-186.452);

    mRichGlobalEdgeMin.setZ(-67.279);
    mRichGlobalEdgeMax.setZ(66.721);


 // 2001
    mRichNormalVectorToPadPlane.setX(-0.497234);
    mRichNormalVectorToPadPlane.setY(0.867616);
    mRichNormalVectorToPadPlane.setZ(-0.000349682682);

    mRichLocation.setX(119.896);
    mRichLocation.setY(-207.86);
    mRichLocation.setZ(-0.471031);

    mL3PCut = 2.5;
    mRichPathCut = 400.;
    cout <<"StRichTofMuDstMaker::Init() ==== settings ===" << endl;
    cout <<"RichLocation for L3 " << mRichLocation << endl;
    cout <<"RichNormal for L3 " << mRichNormalVectorToPadPlane << endl;
    cout <<"RichGlobalEdgeMin for L3 " << mRichGlobalEdgeMin << endl;
    cout <<"RichGlobalEdgeMax for L3 " << mRichGlobalEdgeMax << endl;
    cout <<"L3 P cut " << mL3PCut << endl;
    cout <<"L3 Path Cut " << mRichPathCut << endl;
    
    mPadPlaneCut = 2.0*centimeter;
    mRadiatorCut = 2.0*centimeter;
    mLambdaLastHitCut = 140.*centimeter;
    mLambdaPathCut = 500*centimeter;
    mLambdaPCut = 1.2;
    cout << "mPadPlaneCut " << mPadPlaneCut << endl;
    cout << "mRadiatorCut " << mRadiatorCut << endl;
    cout << "mLambdaLastHitCut " << mLambdaLastHitCut << endl;
    cout << "mLambdaPathCut " << mLambdaPathCut << endl;
    cout << "mLambdaPCut " << mLambdaPCut << endl;
    
    
    return StMaker::Init();
}
Int_t StRichTofMuDstMaker::InitRun(int runumber) 
{
    
    mRichGeometryDb = StRichGeometryDb::getDb();
    mRichTrans = StRichCoordinateTransform::getTransform(mRichGeometryDb);
// define RICH position
    double norm_x = mRichGeometryDb->normalVectorToPadPlane().x();
    double norm_y = mRichGeometryDb->normalVectorToPadPlane().y();
    double norm_z = mRichGeometryDb->normalVectorToPadPlane().z();

    StThreeVectorD richNormalTest(norm_x,norm_y,norm_z);
    richNormalTest.setMag(1.0);
    mRichNormal.setX(norm_x);
    mRichNormal.setY(norm_y);
    mRichNormal.setZ(norm_z);
 
// Pad plane
    StRichLocalCoordinate richPadLocal(0.,0.,mRichGeometryDb->anodeToPadSpacing());
    StGlobalCoordinate richPadGlobal;
    (*mRichTrans)(richPadLocal,richPadGlobal);
    
    mRichPad.setX(richPadGlobal.position().x());
    mRichPad.setY(richPadGlobal.position().y());
    mRichPad.setZ(richPadGlobal.position().z());
// Radiator
    StRichLocalCoordinate richRadLocal(0.0,
						   0.0,
						   mRichGeometryDb->proximityGap() +
						   mRichGeometryDb->quartzDimension().z() +
						   mRichGeometryDb->radiatorDimension().z());
    
    StGlobalCoordinate richRadGlobal;
    (*mRichTrans)(richRadLocal,richRadGlobal);
    
    mRichRadiator.setX(richRadGlobal.position().x());
    mRichRadiator.setY(richRadGlobal.position().y());
    mRichRadiator.setZ(richRadGlobal.position().z());
    cout << "Radiator " 
	 << mRichRadiator.x() << "," 
	 << mRichRadiator.y() << "," 
	 <<  mRichRadiator.z() << endl;
    cout << "Pad Plane " 
	 << mRichPad.x() << "," 
	 << mRichPad.y() << "," 
	 <<  mRichPad.z() << endl;
    cout << "Normal " 
	 << mRichNormal.x() << "," 
	 << mRichNormal.y() << "," 
	 <<  mRichNormal.z() << endl;
    

    mTofGeom = new StTofGeometry;
    mTofGeom->init(this);
    cout <<"StRichTofMuDstMaker::InitRun()  ==== settings === " << endl;
    cout <<"Reject strobe events      ... " << mRejectStrobeEvents << endl;
    cout <<"Use outer track geometry  ... " << mOuterTrackGeometry << endl;
    cout <<"Track Z-range   ... "
	<<mTrackAcceptZ_min <<" to " <<  mTrackAcceptZ_max << endl;
    cout <<"Track Phi-range ... "
	<<mTrackAcceptPhi_min <<" to " <<  mTrackAcceptPhi_max << endl;

    return StMaker::InitRun(runumber);
}

Int_t StRichTofMuDstMaker::Make()
{
    //
    //	Get StEvent
    //
    StEvent* event;
    event = (StEvent *) GetInputDS("StEvent");
    
    
    if (!event) {
        cout <<"StRichTofMuDstMaker::Make(): not a valid StEvent: " << endl;
	

	return kStOK;
    }
       

    if (!accept(event)) {
	cout << "StRichTofMuDstMaker::Make(): event not accepted." << endl;
	delete event;
	return kStOK;
    }
    // Remove rich collection if event is not accepted for RICH.
    if (!mEventAcceptedRich
	&&event->richCollection() 
	&& !(event->richCollection()->isZombie())) {
	cout << "event no good for RICH.  Removing rich." << endl;
	
	this->removeRich(event);
	
    }
    
    // Overwrite the stuff in event summary
    cout << "Overwriting numberOfGoodtracks in StEvent " << endl;
    
    event->summary()->setNumberOfGoodTracks(uncorrectedNumberOfPrimaries(*event));
    event->summary()->setNumberOfGoodTracks(positive,uncorrectedNumberOfPositivePrimaries(*event));
    event->summary()->setNumberOfGoodTracks(negative,uncorrectedNumberOfNegativePrimaries(*event));
    

    event->summary()->setNumberOfExoticTracks(-999);


    StEventScavenger::removeFtpcHitCollection(event);
    StEventScavenger::removeSvtHitCollection(event);
    StEventScavenger::removeSsdHitCollection(event);
    StEventScavenger::removeEmcCollection(event);
// Remove them all to save space
    StEventScavenger::removeTpcHitCollection(event);

    StEventScavenger::removeXiVertices(event);
    StEventScavenger::removeKinkVertices(event);

    unsigned int nL3 = this->removeL3Tracks(event);
    this->removeL3Hits(event);
    
    //================================================================
    //
    //  Prepare the miniDST by removing everything we don't like.
    //  The rest happens automatically.
    //
    //================================================================
 

    // Get the Lambdas 
    vector<StV0Vertex*> theLambdas = this->getLambdas(event);
    vector<unsigned int> uniqueGoodDaughters = this->removeV0sButLambdas(event,theLambdas);
    
    //
    //  Select only those track nodes  that have pid traits
    //

    UInt_t allTracks = 0;
    vector<StTrackNode*> badNodes;
    vector<StTrackNode*> goodNodes;

    
    unsigned int acceptedTof = 0;
    unsigned int acceptedRich = 0;
    unsigned int acceptedLambda = 0;
    
    StSPtrVecTrackNode& nodes = event->trackNodes();
    for (StSPtrVecTrackNodeIterator nodeIter = nodes.begin();
	 nodeIter != nodes.end(); ++nodeIter) {
	bool isGood = false;
	++allTracks;
	bool isAcceptedTof = false;
	bool isAcceptedRich = false;
	bool isAcceptedLambda = false;
	
	
	for (UInt_t ientry=0; 
	     ientry < (*nodeIter)->entries(); ++ientry) {
	    if (binary_search(uniqueGoodDaughters.begin(),uniqueGoodDaughters.end(),(*nodeIter)->track(ientry)->key())) {
		isGood = true;
		isAcceptedLambda = true;
	    }
	    
	    if (acceptTof((*nodeIter)->track(ientry)) ) {
		isGood = true;
		isAcceptedTof = true;
		
	    }
	    if (acceptRich((*nodeIter)->track(ientry)) ) {
		isAcceptedRich = true;
		isGood=true;
		
	    }

	}
	if (isAcceptedRich) ++acceptedRich;
	if (isAcceptedTof) ++acceptedTof;
	if (isAcceptedLambda) ++acceptedLambda;
	
	if (!isGood) {
	    badNodes.push_back((*nodeIter));
	}
	else {
	    goodNodes.push_back((*nodeIter));
	}
    }
    if (!goodNodes.size() && nL3 ==0) {
	cout << "StRichTofMuDstMaker::Make():  no good tracks.  Not deleting event." << endl;

	
    }
    if (acceptedRich==0 
	&&event->richCollection() 
	&& !(event->richCollection()->isZombie())) {
	cout << "0 accepted rich tracks.  Removing rich" << endl;
	this->removeRich(event);
    }
    


// remove tracks
    for (vector<StTrackNode*>::iterator nodeIter = badNodes.begin();
	 nodeIter != badNodes.end(); ++nodeIter) {

	for (UInt_t ientry=0; ientry < (*nodeIter)->entries(); ++ientry) {
	    StEventScavenger::remove((*nodeIter)->track(ientry));
	}
    }


    ++acceptedEvents;
    
    acceptedTracks += goodNodes.size();
    cout << "StRichTofMuDstMaker::Make(): " << goodNodes.size() << " total good tracks out of " << allTracks << endl;
    cout << "StRichTofMuDstMaker::Make(): " 
	 << acceptedTof << " TOF tracks, " 
	 << acceptedRich << " RICH tracks, " 
	 << acceptedLambda << " Lambda tracks" << endl;

    
    //
    //  That's it!
    //  The IO maker will take care of the rest.
    //
    return kStOK;
}

bool StRichTofMuDstMaker::accept(StEvent* event)
{

    // 1. must have a primary vertex
    if (!event->primaryVertex()){
	cout <<"StRichTofMuDstMaker::event not accepted: no primary vertex" << endl;
	return false;
    }
    // Get triggerActionWord
    unsigned int theTriggerActionWord = 0;
    if (event->l0Trigger()) {
	theTriggerActionWord = event->l0Trigger()->triggerActionWord();
    }
    
    

    mEventAcceptedTof = true;
    if (! (theTriggerActionWord & mTofMaskInActionWord)) {
	cout << "triggerActionWord: 0x" << hex << theTriggerActionWord 
				     << " does not contain tof 0x" 
				     << mTofMaskInActionWord 
				     << dec << endl;
	mEventAcceptedTof = false;
    }
    
    // 2. must have TOF collection and TofData()
    StTofCollection *theTof = event->tofCollection();
    if (!(theTof && theTof->dataPresent())){
//	cout <<"no TOF collection/data present";
	mEventAcceptedTof = false;
    }
    else {
	
    // 3. must be beam event (i.e. non-strobe)
	StSPtrVecTofData  &tofData = theTof->tofData();
	if (strobeEvent(tofData) && mRejectStrobeEvents) {
//	cout <<"event not accepted: strobed event";
	    mEventAcceptedTof = false;
	}
    }
    
    mEventAcceptedRich = true;
    if (! (theTriggerActionWord & mRichMaskInActionWord)) {
	cout<< "triggerActionWord: 0x"
	    << hex << theTriggerActionWord 
	    << " does not contain Rich 0x" 
	    << mRichMaskInActionWord 
	    << dec << endl;
	mEventAcceptedRich = false;
    }
    
// Also check if there is a rich collection
    if (event->detectorState(kRichId) && !(event->detectorState(kRichId)->good())) {
	cout << "Bad rich event: detectorState(kRichId)->good():" << event->detectorState(kRichId)->good() << " time " << event->time() << endl;
	
	mEventAcceptedRich = false;
    }
//    if (!(event->richCollection())) mEventAcceptedRich = false;
//    if (event->richCollection() && !(event->richCollection()->pixelsPresent())) mEventAcceptedRich = false;
    
    
    return true;
    
}

bool StRichTofMuDstMaker::acceptTof(StTrack* track)
{    
    if (!track) return false;
    if (track->flag() <=0) return false;
    
    // Check only if the tof accepts the event
    if (mEventAcceptedTof) {
	
	// 2. Ask if track must be "in range" of TOFp
	StPhysicalHelixD theHelix;
	if (mOuterTrackGeometry) theHelix = track->outerGeometry()->helix();
	else                     theHelix = track->geometry()->helix();
	pairD pairLength = theHelix.pathLength(mTofGeom->tofParam().r);
	StThreeVectorD firstPoint = theHelix.at(pairLength.first);
	StThreeVectorD secondPoint = theHelix.at(pairLength.second);
	
	// if either one of the points is w/i range => accept.
	bool firstPointInRange =  (pairLength.first>0) &&
	    (firstPoint.z()>mTrackAcceptZ_min)  &&
	    (firstPoint.z()<mTrackAcceptZ_max)  &&
	    (atan2(firstPoint.y(),firstPoint.x())>mTrackAcceptPhi_min) &&
	    (atan2(firstPoint.y(),firstPoint.x())<mTrackAcceptPhi_max);
	bool secondPointInRange = (pairLength.second>0) && 
	    (secondPoint.z()>mTrackAcceptZ_min)  &&
	    (secondPoint.z()<mTrackAcceptZ_max)  &&
	    (atan2(secondPoint.y(),secondPoint.x())>mTrackAcceptPhi_min) &&
	    (atan2(secondPoint.y(),secondPoint.x())<mTrackAcceptPhi_max);
	if (firstPointInRange || secondPointInRange) 
// Short circuit out of here if matches tof
	    
	    return true; 
    }
    return false;
}
bool StRichTofMuDstMaker::acceptRich(StTrack* track) 
{
    
    if (!mEventAcceptedRich) return false;
    
    if (!(track->pidTraits(kRichId).size())) return false;

    
    return true;
}

Int_t StRichTofMuDstMaker::Finish()
{
    cout << "**************************************************************************" << endl;
    cout << "*************************>> RichTofMuDST statistics <<*************************" << endl;
    cout << "**************************************************************************" << endl;
    cout << "===> total number of accepted event: " << acceptedEvents  << endl;
    cout << "===> total number of accepted tracks: " << acceptedTracks  << endl;
    cout << "**************************************************************************" << endl;
    return kStOK;
}
unsigned int StRichTofMuDstMaker::removeL3Tracks(StEvent* evt)
{
// Keep l3 tracks that project to rich

    unsigned int nL3Accepted = 0;
    if (evt && evt->l3Trigger()) {
	StSPtrVecTrackNode& theL3Tracks = 
	    evt->l3Trigger()->trackNodes();

	for (StSPtrVecTrackNodeIterator iter = theL3Tracks.begin();
	     iter != theL3Tracks.end(); ++iter) {
	    bool accepted = false;
	    for (UInt_t ientry=0; ientry<(*iter)->entries();++ientry) {
		if (this->acceptL3TrackInRich((*iter)->track(ientry))) {
		    accepted = true;
		}
	    }
	    if (!accepted) {
		(*iter)->makeZombie();
		(*iter)->track(0)->detectorInfo()->makeZombie();
		
	    }
	    else {
		++nL3Accepted;
	    }
	}

	cout <<"StRichTofMuDstMaker::Accepted " << nL3Accepted << " L3 Tracks " << endl;
	
    }
    return nL3Accepted;
    

}

void StRichTofMuDstMaker::removeL3Hits(StEvent* evt)
{

    if (evt && evt->l3Trigger() && evt->l3Trigger()->tpcHitCollection()) {
	    
	StTpcHitCollection* theHits = 
	    evt->l3Trigger()->tpcHitCollection();
	
	for (unsigned int n=0; n<theHits->numberOfSectors(); n++)
	    for (unsigned int m=0; m<theHits->sector(n)->numberOfPadrows(); m++)
		for (unsigned int h=0; h<theHits->sector(n)->padrow(m)->hits().size(); h++)
		    theHits->sector(n)->padrow(m)->hits()[h]->makeZombie();
	theHits->makeZombie();

    }

    
}






bool StRichTofMuDstMaker::acceptL3TrackInRich(StTrack *gt) 
{

    bool retval = false;
    if (!mEventAcceptedRich) return false;
    
    if (gt->geometry()->momentum().mag()<mL3PCut) return false;
    
    const StPhysicalHelixD &gh = gt->geometry()->helix();
    
    double padpath =  
	gh.pathLength(mRichLocation,mRichNormalVectorToPadPlane);
    
    if (padpath>0 && padpath < mRichPathCut) {
	StThreeVectorD diffmin = gh.at(padpath) - mRichGlobalEdgeMin;
	StThreeVectorD diffmax = mRichGlobalEdgeMax - gh.at(padpath);
	if (diffmin.x() >0 && diffmin.y()>0. && diffmin.z()>0 &&
	    diffmax.x() >0 && diffmax.y()>0. && diffmax.z()>0) {
	    retval = true;
	}
    }
    
    return retval;
}
bool StRichTofMuDstMaker::strobeEvent(StSPtrVecTofData tofData){
  // determine strobe event from pVPD TDC data

  const int strobeTdcMin = 1600;
  const int strobeTdcMax = 1650;

  int nStrobedPvpdTdcs=0;
  for(int i=42;i<48;i++)
    if((tofData[i]->tdc()>strobeTdcMin) &&(tofData[i]->tdc()<strobeTdcMax))
  	nStrobedPvpdTdcs++;
  
  if (nStrobedPvpdTdcs==6) return true;

  return false;
}

vector<StV0Vertex*> StRichTofMuDstMaker::getLambdas(StEvent* event) 
{

  vector <StV0Vertex*> ret;
  if (!mEventAcceptedRich) return ret;
  
    
    
  StStrangeMuDstMaker* strangeDst =
    (StStrangeMuDstMaker*) GetMaker("strangeMuDst");
    
      
  map<unsigned int,StTrack*> keyToTrack;
  StSPtrVecTrackNode& theNodes = event->trackNodes();
  for (StSPtrVecTrackNodeConstIterator nodeIter=theNodes.begin();
       nodeIter != theNodes.end(); ++nodeIter) {
    for (UInt_t ientry=0; ientry<(*nodeIter)->entries(global); ++ientry) {
      StTrack* theTrack = (*nodeIter)->track(global,ientry);
      if (theTrack) {
	keyToTrack[theTrack->key()] = theTrack;
      }
    }
  }

    
  if (!strangeDst) return ret;
    
  vector< pair<unsigned int, unsigned int> > lamDaughters;
    
  for( Int_t j=0; j<strangeDst->GetNV0(); j++ ) {
    StV0MuDst *v0m = strangeDst->GetV0(j);
	
    // Check if Lambda	
    if  (v0m &&
	 (v0m->dcaV0ToPrimVertex()<0.7)
	 &&
	 (v0m->decayLengthV0()>5.0) &&
	 (v0m->topologyMapPos().numberOfHits(kTpcId)>15) &&
	 (v0m->topologyMapNeg().numberOfHits(kTpcId)>15) &&
	 (v0m->dcaV0Daughters()<0.75) 
	 )
      {
	if (
	    (v0m->alphaV0()>0) &&
	    (v0m->dcaNegToPrimVertex()>2.85) && 
	    (v0m->massLambda() > 1.07) &&
	    (v0m->massLambda() < 1.18) 
	    )
	  {
	    StTrack* theTrack = keyToTrack[v0m->keyPos()];
		
	    if (this->acceptLambdaDaughter(theTrack)) {
	      lamDaughters.push_back(make_pair(v0m->keyPos(),v0m->keyNeg()));
	    }
		
	  }
	if (
	    (v0m->alphaV0()<0) &&
	    (v0m->dcaPosToPrimVertex()>2.85) &&
	    (v0m->massAntiLambda() > 1.07) &&
	    (v0m->massAntiLambda() < 1.18) 
	    )
	  {
	    StTrack* theTrack = keyToTrack[v0m->keyNeg()];
		
	    if (this->acceptLambdaDaughter(theTrack)) {
	      lamDaughters.push_back(make_pair(v0m->keyPos(),v0m->keyNeg()));
	    }
		
	  }
      }
  }
  // Now find the lambdas
  StSPtrVecV0Vertex& theVertices = event->v0Vertices();

  for (StSPtrVecV0VertexConstIterator vIter=theVertices.begin();
       vIter != theVertices.end(); ++vIter) {
    if (!(*vIter)) continue;
  	
    for (vector< pair<unsigned int, unsigned int> >::const_iterator mIter = lamDaughters.begin();
	 mIter != lamDaughters.end(); ++mIter) {
      StTrack* dPos = (*vIter)->daughter(positive);
      StTrack* dNeg = (*vIter)->daughter(negative);
	    
      if (dPos && dNeg && dPos->key() == (*mIter).first && dNeg->key() == (*mIter).second) 
	{
	  ret.push_back(*vIter);
	}
    }
  }
  sort(ret.begin(),ret.end());
    
  cout << "Found " << ret.size() << "Unique lambda V0s pointing to rich out of " 
       << theVertices.size() << " total" << endl;
    
  return ret;
}

vector<unsigned int> StRichTofMuDstMaker::removeV0sButLambdas(StEvent* event,vector<StV0Vertex*>& goodV0s) 
{
  // Make a vector of good daughters
  vector<unsigned int> goodDaughters;
    
  for (vector<StV0Vertex*>::const_iterator vIter = goodV0s.begin();
       vIter != goodV0s.end(); ++vIter) {
    for (UInt_t id=0; id< (*vIter)->numberOfDaughters(); ++id) {
      StTrack* theTrack = (*vIter)->daughter(id);
      if (theTrack) {
	goodDaughters.push_back(theTrack->key());
      }
    }
  }
  sort(goodDaughters.begin(),goodDaughters.end());
  vector<unsigned int> uniqueGoodDaughters;
  unique_copy(goodDaughters.begin(),goodDaughters.end(),back_inserter(uniqueGoodDaughters));

  // Now remove bad V0s
  unsigned int keptV0s = 0;
    
  StSPtrVecV0Vertex& theV0s = event->v0Vertices();
  for (StSPtrVecV0VertexIterator vIter = theV0s.begin();
       vIter != theV0s.end(); ++vIter) {
    if (!(*vIter)) continue;
	
    bool isGood = false;
	
    if ((binary_search(goodV0s.begin(),goodV0s.end(),(*vIter)))) {
      isGood = true;
    }
    if (isGood) {
      keptV0s++;
    }
    if (!isGood) {
      (*vIter)->makeZombie();
    }
  }
    
	    
 
  cout << "StRichTofMuDstMaker::Kept " << keptV0s << "V0s" << endl;
  return uniqueGoodDaughters;
  
}
bool StRichTofMuDstMaker::acceptLambdaDaughter(StTrack* track) 
{
        
    StTrack* gt = track;
    if (!gt) {
	return false;
    }

    if (gt->geometry()->momentum().mag()<mLambdaPCut) {
	return false;
    }


    if (gt->detectorInfo()->lastPoint().perp()<mLambdaLastHitCut) return false;
    if (gt->fitTraits().numberOfFitPoints() < mLambdaFitPointsCut) return false;
    
// No DCA cut
    if (gt->flag()<0) {
  	return false;
    }
    
// Projection copied from pid maker
    
    StPhysicalHelixD gh;
    
    if (gt->outerGeometry()) {
	gh = gt->outerGeometry()->helix();
    }
    else {
	gh = gt->geometry()->helix();
    }
    
    double padpath =  
	gh.pathLength(mRichPad,mRichNormal);
    double radpath =
	gh.pathLength(mRichRadiator,mRichNormal);
    
    if (radpath>0 && radpath<mLambdaPathCut) {
	StRichLocalCoordinate extrapolatedPosition;
	StRichLocalCoordinate impactPoint;
	
	{
	    
	
// Project to pad plane
	    double impactPoint_x = gh.x(padpath);
	    double impactPoint_y = gh.y(padpath);
	    double impactPoint_z = gh.z(padpath);
	    StGlobalCoordinate gb(impactPoint_x, impactPoint_y, impactPoint_z);
	    (*mRichTrans)(gb,extrapolatedPosition);
	    
	}
	{
	    
// Project to radiator
	    
		
	    double radimpactPoint_x = gh.x(radpath);
	    double radimpactPoint_y = gh.y(radpath);
	    double radimpactPoint_z = gh.z(radpath);
	    StGlobalCoordinate radgb(radimpactPoint_x, radimpactPoint_y, radimpactPoint_z);
	    (*mRichTrans)(radgb,impactPoint);
	}
	
	if ( fabs(extrapolatedPosition.position().x()) < (mRichGeometryDb->padPlaneDimension().x() - mPadPlaneCut) &&
	     fabs(extrapolatedPosition.position().y()) < (mRichGeometryDb->padPlaneDimension().y() - mPadPlaneCut) &&
	     fabs(extrapolatedPosition.position().x()) > (mPadPlaneCut) &&
	     fabs(extrapolatedPosition.position().y()) > (mPadPlaneCut) &&
	     fabs(impactPoint.position().x()) < (mRichGeometryDb->radiatorDimension().x() - mRadiatorCut) &&
	     fabs(impactPoint.position().y()) < (mRichGeometryDb->radiatorDimension().y() - mRadiatorCut) &&
		 fabs(impactPoint.position().x()) > (mRadiatorCut) &&
	     fabs(impactPoint.position().y()) > (mRadiatorCut)) {
	    
	    return true;
	}
    }
    

    return false;
}

void StRichTofMuDstMaker::removeRich(StEvent *event) 
{
    // Get Rid of rich collection

    StEventScavenger::removeRichCollection(event);

    // Get rid of rich pid traits
    StSPtrVecTrackNode &tracks = event->trackNodes();

    unsigned int nPidTraitsRemoved = 0;
    
    for (StSPtrVecTrackNodeIterator titer = tracks.begin();
	 titer != tracks.end(); ++titer) {
	for (unsigned int ientry = 0; ientry < (*titer)->entries(); ++ientry) {
	    StTrack *track = (*titer)->track(ientry);
	    
	    StPtrVecTrackPidTraits traits = track->pidTraits(kRichId);
	    for (StPtrVecTrackPidTraitsIterator piter = traits.begin();
		 piter != traits.end(); ++piter) {
		(*piter)->makeZombie();
		++nPidTraitsRemoved;
		
	    }
	}
    }
    cout << "Removed rich PID traits from " << nPidTraitsRemoved << " tracks." << endl;
    
}

