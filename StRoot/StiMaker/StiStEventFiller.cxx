/***************************************************************************
 *
 * $Id: StiStEventFiller.cxx,v 1.16 2002/08/19 19:33:00 pruneau Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Mar 2002
 ***************************************************************************
 *
 * $Log: StiStEventFiller.cxx,v $
 * Revision 1.16  2002/08/19 19:33:00  pruneau
 * eliminated cout when unnecessary, made helix member of the EventFiller
 *
 * Revision 1.15  2002/08/12 21:39:56  calderon
 * Introduced fillPidTraits, which uses the values obtained from
 * Andrews brand new dEdxCalculator to create two instances of an
 * StTrackPidTraits object and pass it to the track being filled.
 *
 * Revision 1.14  2002/08/12 15:29:21  andrewar
 * Added dedx calculators
 *
 * Revision 1.13  2002/06/28 23:30:56  calderon
 * Updated with changes debugging for number of primary tracks added.
 * Merged with Claude's latest changes, but restored the tabs, othewise
 * cvs diff will not give useful information: everything will be different.
 *
 * Revision 1.12  2002/06/26 23:05:31  pruneau
 * changed macro
 *
 * Revision 1.11  2002/06/25 15:09:16  pruneau
 * *** empty log message ***
 *
 * Revision 1.10  2002/06/18 18:08:34  pruneau
 * some cout statements removed/added
 *
 * Revision 1.9  2002/06/05 20:31:15  calderon
 * remove some redundant statements, the call to
 * StTrackNode::addTrack()
 * already calls
 * track->SetNode(this), so I don't need to do it again
 *
 * Revision 1.8  2002/05/29 19:14:45  calderon
 * Filling of primaries, in
 * StiStEventFiller::fillEventPrimaries()
 *
 * Revision 1.7  2002/04/16 19:46:44  pruneau
 * must catch exception
 *
 * Revision 1.6  2002/04/16 13:11:30  pruneau
 * *** empty log message ***
 *
 * Revision 1.5  2002/04/09 16:03:13  pruneau
 * Included explicit extension of tracks to the main vertex.
 *
 * Revision 1.4  2002/04/03 16:35:03  calderon
 * Check if primary vertex is available in StiStEventFiller::impactParameter(),
 * if not, return DBL_MAX;
 *
 * Revision 1.3  2002/03/28 04:29:49  calderon
 * First test version of Filler
 * Currently fills only global tracks with the following characteristics
 * -Flag is set to 101, as most current global tracks are.  This is not strictly correct, as
 *  this flag is supposed to mean a tpc only track, so really need to check if the track has
 *  svt hits and then set it to the appropriate flag (501 or 601).
 * -Encoded method is set with bits 15 and 1 (starting from bit 0).  Bit 1 means Kalman fit.
 *  Bit 15 is an as-yet unused track-finding bit, which Thomas said ITTF could grab.
 * -Impact Parameter calculation is done using StHelix and the primary vertex from StEvent
 * -length is set using getTrackLength, which might still need tweaking
 * -possible points is currently set from getMaxPointCount which returns the total, and it is not
 *  what we need for StEvent, so this needs to be modified
 * -inner geometry (using the innermostHitNode -> Ben's transformer -> StPhysicalHelix -> StHelixModel)
 * -outer geometry, needs inside-out pass to obtain good parameters at outermostHitNode
 * -fit traits, still missing the probability of chi2
 * -topology map, filled from StuFixTopoMap once StDetectorInfo is properly set
 *
 * This version prints out lots of messages for debugging, should be more quiet
 * when we make progress.
 *
 **************************************************************************/

//std
#include <iostream>
#include <algorithm>
#include <stdexcept>
using namespace std;

// SCL
#include "StPhysicalHelix.hh"
#include "StThreeVector.hh"
#include "StThreeVectorF.hh"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"

#include "StDedxMethod.h"

//StEvent
#include "StPrimaryVertex.h"
#include "StEventTypes.h"
#include "StDetectorId.h"

#include "StEventUtilities/StuFixTopoMap.cxx"
//Sti
#include "Sti/StiTrackContainer.h"
#include "Sti/StiKalmanTrack.h"
#include "Sti/StiGeometryTransform.h"
#include "Sti/StiDedxCalculator.h"

//StiMaker
#include "StiStEventFiller.h"

StiStEventFiller::StiStEventFiller() : mEvent(0), mTrackStore(0), mTrkNodeMap()
{
	//temp, make sure we're not constructing extra copies...
	//cout <<"StiStEventFiller::StiStEventFiller()"<<endl;
  dEdxTpcCalculator.setFractionUsed(.6);
  dEdxSvtCalculator.setFractionUsed(.6);
  dEdxTpcCalculator.setDetectorFilter(kTpcId);
  dEdxSvtCalculator.setDetectorFilter(kSvtId);


  //mResMaker.setLimits(-1.5,1.5,-1.5,1.5,-10,10,-10,10);
  //mResMaker.setDetector(kSvtId);
}

StiStEventFiller::~StiStEventFiller()
{
  cout <<"StiStEventFiller::~StiStEventFiller()"<<endl;
  //mResMaker.writeResiduals();
}

//Helper functor, gotta live some place else, just a temp. test of StiTrack::stHits() method
struct StreamStHit
{
    void operator()(const StHit* h) {
      //cout << "DetectorId: " << (unsigned long) h->detector();
	if (const StTpcHit* hit = dynamic_cast<const StTpcHit*>(h)) {
	    cout <<hit->position() << " Sector: " << hit->sector() << " Padrow: " << hit->padrow() << endl;
	}
	else if (const StSvtHit* hit = dynamic_cast<const StSvtHit*>(h)) {
	  //cout << hit->position() << " layer: " << hit->layer() << " ladder: " << hit->ladder()
	  // << " wafer: " << hit->wafer() << " barrel: " << hit->barrel() << endl;
	}
	else {	
	  //cout << hit->position() << endl;
	}
    }
};

/*! 
  Algorithm:
  Loop over all tracks in the StiTrackContainer, doing for each track:
  - Create a new global track and associated information (see below)
    and set its data members according to the StiTrack,
    can be done in a StGlobalTrack constructor
  - Hang the new track to the StTrackNode container in StEvent, this creates a new entry
    in the container, the global track is now owned by it.
    <p>
  In addition to the StGlobalTrack, we need to create the following objects (owned by it):
  StTrackTopologyMap
  StTrackFitTraits
  StTrackGeometry (2 are needed, one at first point, one at last point)
  (note: StHelixModel is implementation of the StTrackGeometry abstract class)
  
  The track also owns a container of PidTraits, this algorithm will not fill this container.
  
  And set up links to:
  StTrackDetectorInfo (owned by StEvent, StSPtrVecTrackDetectorInfo)
  StTrackNode         (owned by StEvent, StSPtrVecTrackNode)
  These links are
  track  -> detector info
  track <-> track node

  Skeleton of the algorithm:
  <code> \n
  StSPtrVecTrackNode& trNodeVec = mEvent->trackNodes(); \n
  StSPtrVecTrackDetectorInfo& detInfoVec = mEvent->trackDetectorInfo(); \n
  for (trackIterator trackIt = mTrackStore->begin(); trackIt != mTrackStore->end(); ++trackIt) { \n
     StiKalmanTrack* kTrack = (*trackIt).second; // the container is a <map>, need second entry of <pair> \n
\n
     StTrackDetectorInfo* detInfo = new StTrackDetectorInfo();\n
     fillDetectorInfo(detInfo,kTrack);\n
     detInfoVec.push_back(detInfo);\n
     \n
     StTrackNode* trackNode = new StTrackNode;\n
     trNodeVec.push_back(trackNode);\n
     \n
     StGlobalTrack* gTrack = new StGlobalTrack();\n
     fillGlobalTrack(gTrack,kTrack);\n
     \n
     // set up relationships between objects\n
     gTrack->setDetectorInfo(detInfo);\n
     gTrack->setNode(trackNode);\n
     trackNode->AddTrack(gTrack);\n
  }\n
  </code>
  The creation of the various objects needed by StGlobalTrack are taken care of in the methods:
  fillTopologyMap(), fillGeometry(), fillFitTraits(), which are called within fillGlobalTrack().
  
*/

StEvent* StiStEventFiller::fillEvent(StEvent* e, StiTrackContainer* t)
{
    if (e==0 || t==0) {
	cout <<"StiStEventFiller::fillEvent(). ERROR:\t"
	     <<"Null StEvent ("<<e<<") || StiTrackContainer ("<<t<<").  Exit"<<endl;
	return 0;
    }
    
    mEvent = e;
    mTrackStore = t;
    
    // loop
    StSPtrVecTrackNode& trNodeVec = mEvent->trackNodes(); 
    StSPtrVecTrackDetectorInfo& detInfoVec = mEvent->trackDetectorInfo(); 
    
    int errorCount=0;
    for (KalmanTrackMap::const_iterator trackIt = mTrackStore->begin(); trackIt!=mTrackStore->end();++trackIt) {
	const StiKalmanTrack* kTrack = (*trackIt).second;
	// Mike's test of track->stHits();
	//vector<StHit*> vec = kTrack->stHits();
	//cout <<" --- Hits for next track --- "<<endl;
	//for_each(vec.begin(), vec.end(), StreamStHit());
	//cout << "StiStEventFiller::fillEvent() - INFO - track: " << *kTrack << endl;
	// detector info
	StTrackDetectorInfo* detInfo = new StTrackDetectorInfo;
	fillDetectorInfo(detInfo,kTrack);
	
	// track node where the new StTrack will reside
	StTrackNode* trackNode = new StTrackNode;
	// actual filling of StTrack from StiTrack
	StGlobalTrack* gTrack = new StGlobalTrack;
	
	try {
	    fillTrack(gTrack,kTrack);
	    // filling successful,
	    // set up relationships between objects
	    detInfoVec.push_back(detInfo);
	    gTrack->setDetectorInfo(detInfo);
	    trackNode->addTrack(gTrack);
	    //cout<<"Track Node Entries: "<<trackNode->entries()<<endl;
	    trNodeVec.push_back(trackNode);
	    // reuse the utility to fill the topology map
	    // this has to be done at the end as it relies on
	    // having the proper track->detectorInfo() relationship
	    // and a valid StDetectorInfo object.
	    StuFixTopoMap(gTrack);
	    //cout<<"Tester: Event Track Node Entries: "<<trackNode->entries()<<endl;
	    mTrkNodeMap.insert(map<const StiKalmanTrack*,StTrackNode*>::value_type (kTrack,trNodeVec.back()) );
	    if (trackNode->entries(global)<1)
		cout << "StiStEventFiller::fillEvent() - ERROR - Track Node has no entries!! " << endl;
	}
	catch (runtime_error & rte ) {
	    if (++errorCount<5)
		cout << "StiStEventFiller::fillEvent() - WARNING - runtime exception filling track: "
		     << rte.what() << endl;
	    delete trackNode;
	    delete detInfo;
	    delete gTrack;
	}
	catch (...) {
	    cout << "StiStEventFiller::fillEvent() - WARNING - Unknown exception filling track."<<endl;
	    delete trackNode;
	    delete detInfo;
	    delete gTrack;
	}
    }
    if (errorCount>4)
	cout << "There were "<<errorCount<<"runtime_error while filling StEvent"<<endl;
    
    return mEvent;
}

StEvent* StiStEventFiller::fillEventPrimaries(StEvent* e, StiTrackContainer* t) {
    cout <<"StiStEventFiller::fillEventPrimaries()"<<endl;
    if (!mTrkNodeMap.size()) {
	cout <<"StiStEventFiller::fillEventPrimaries(). ERROR:\t"
	     << "Mapping between the StTrackNodes and the StiTracks is empty.  Exit." << endl;
	return 0;
    }
    if (e==0 || t==0) {
	cout <<"StiStEventFiller::fillEventPrimaries(). ERROR:\t"
	     <<"Null StEvent ("<<e<<") || StiTrackContainer ("<<t<<").  Exit"<<endl;
	return 0;
    }
    
    
    mEvent = e;
    mTrackStore = t;

    //Added residual maker...aar
    
    StPrimaryVertex*       vertex = mEvent->primaryVertex(0);
    StSPtrVecTrackDetectorInfo& detInfoVec = mEvent->trackDetectorInfo();

    //cout <<"Checking Vertex. "
    //     <<"Daughters: "<< vertex->numberOfDaughters()<<endl;
    if(!vertex)
      {
	cout <<"Failed to find a primary vertex."<<endl;
	return (StEvent*)NULL;
	  
      }
    //else
    //  {
	//cout << "original daughters " << vertex->daughters().size() << endl;
    //}
    
    int skippedCount=0;
    int fillTrackCount=0;
    // loop over StiKalmanTracks
    //cout << "Tracks in container " << mTrackStore->size() << endl;
    int mTrackN=0;
    for (KalmanTrackMap::const_iterator trackIt = mTrackStore->begin(); trackIt!=mTrackStore->end();++trackIt,++mTrackN) {
      //cout <<"Current track: "<< mTrackN<<endl;
	// get track and corresponding StTrackNode
	const StiKalmanTrack* kTrack = (*trackIt).second;
	//mResMaker.setTrackResiduals(kTrack);
	if (kTrack==0) {
	  //cout<< "kTrackLoop: kTrack pointer==0"<<endl;
	    continue;
	}
	map<const StiKalmanTrack*, StTrackNode*>::iterator itKtrack = mTrkNodeMap.find(kTrack);
	if (itKtrack == mTrkNodeMap.end()) {
	    if (++skippedCount<5)
		cout << "skipping track which was not entered in an StTrackNode" << endl;
	    continue;
	}
	//cout <<"Tester: Primary Track Map Node Entries: "<<(*itKtrack).second->entries()<<endl;
	if (kTrack->isPrimary()) {
	  //cout <<"Track is a primary."<<endl;
	    StTrackNode* currentTrackNode = (*itKtrack).second;
	    //cout <<"Got Current Node."<<endl;
	    //  cout<<"Total entries in track node: "<<currentTrackNode->entries()<<endl;
	    if (currentTrackNode->entries()>10)
	       {
		 cout <<"Track map gone pathalogic. Next track!"<<endl;
		 continue;
	       }
	    if (currentTrackNode->entries(global)<1) {
		cout << "skipping Node: this node should have a global track but doesn't" << endl;
		continue;
	    }
		  cout <<"Entries >1."<<endl;
	    // detector info
	    StTrackDetectorInfo* detInfo = new StTrackDetectorInfo;
	    fillDetectorInfo(detInfo,kTrack);
	    //cout <<"Filled Detector info."<<endl;

	    
	    // actual filling of StTrack from StiTrack
	    //cout <<"Creating new track"<<endl;
	    StPrimaryTrack* pTrack = new StPrimaryTrack;

	    //cout <<"Entering try-catch block"<<endl;
	    try	{
		fillTrackCount++;
		//cout << "trying to fill track " << fillTrackCount++ << endl;
		fillTrack(pTrack,kTrack);
		
		// set up relationships between objects
		detInfoVec.push_back(detInfo);
		pTrack->setDetectorInfo(detInfo);
		currentTrackNode->addTrack(pTrack);  // StTrackNode::addTrack() calls track->setNode(this);
		vertex->addDaughter(pTrack);
		
		StuFixTopoMap(pTrack);
	    }
	    catch (runtime_error & rte ) {
		cout << "StiStEventFiller::fillEventPrimaries() - runtime exception, filling track: "
		     << rte.what() << endl;
		delete detInfo;
		delete pTrack;
	    }
	    catch (...) {
		cout << "StiStEventFiller::fillEventPrimaries() - Unknown exception, filling track."<<endl;
		delete detInfo;
		delete pTrack;
	    }
	}//end if primary
    } // kalman track loop
    //cout << "# isPrimary()=true " << fillTrackCount << endl;
    //cout << "new vtx  daughters " << vertex->daughters().size() << endl;
    if (skippedCount>0)
	cout << "A total of "<<skippedCount<<" StiTracks were skipped"<<endl;
    mTrkNodeMap.clear();  // need to reset for the next event
    
    return mEvent;
}

void StiStEventFiller::fillDetectorInfo(StTrackDetectorInfo* detInfo, const StiTrack* track) {
    //cout << "StiStEventFiller::fillDetectorInfo() " << endl;
    // use the vector of StHits to fill the detector info
    // change: currently point and fit points are the same for StiTracks,
    // if this gets modified later in ITTF, this must be changed here
    // but maybe use track->getPointCount() later?
    
    //vector<StHit*> hitVec = track->stHits();
    vector<StMeasuredPoint*> hitVec = track->stHits();
    detInfo->setFirstPoint(hitVec.front()->position());
    detInfo->setLastPoint(hitVec.back()->position());
    detInfo->setNumberOfPoints(encodedStEventFitPoints(track));
    for (vector<StMeasuredPoint*>::iterator point = hitVec.begin(); point!=hitVec.end(); ++point) {
	StHit * hh = dynamic_cast<StHit*>(*point);
	if (hh)
	    detInfo->addHit(hh);
    }
    return;
}

void StiStEventFiller::fillGeometry(StTrack* gTrack, const StiTrack* track, bool outer)
{
    if (gTrack==0 || track==0) {
	cout << "StiStEventFiller::fillGeometry(). ERROR:\t"
	     << "Null StGlobalTrack or null StiTrack.  Exit" <<endl;
	return;
    }
    
    // fill a new instance of StTrackGeometry (i.e. StHelixModel) selecting
    // between inner and outermost point based on the third argument of the function call
    const StiKalmanTrack* kTrack = dynamic_cast<const StiKalmanTrack*>(track);
    if (!kTrack) {
	cout << "StiStEventFiller::fillGeometry(). ERROR:\t"
	     << "StiTrack can't be dynamic_cast'd to StiKalmanTrack.  Exit" <<endl;
	return;	
    }
    StiKalmanTrackNode* node;
    if (outer)
	node = kTrack->getOuterMostHitNode();
    else
	node = kTrack->getInnerMostHitNode();
    
    // good ol' Ben's transformation routine,
    // this fills StPhysicalHelix appropriately
    StiGeometryTransform* transformer = StiGeometryTransform::instance();
    // note, we have to create an instance of StPhysicalHelix
    // but the default constructor is protected, so we have to
    // call the constructor with dummy information.
    // the transformer will call the constructor with the right
    // values and then use the assignment operator to put them into
    // the instance that we pass to it.
    StThreeVector<double> dummyVec(-999,-999,-999);

    // transform node into "helix" (which is a Physical Helix)
    // and then create a HelixModel out of it.
    transformer->operator()(node, &helix);
    const StThreeVector<double> orig = helix.origin();
    StThreeVectorF origin(orig.x(),orig.y(),orig.z());
    
    double mom[3];
    node->getGlobalMomentum(mom);
    StThreeVectorF momF(mom[0],mom[1],mom[2]); 
    //cout <<"Global Momentum: "<<momF<<endl;

    double bField = -99999;
    // test
    // change: the magnetic field should NEVER be hardwired, this is merely a test to
    // make sure the chain works, must use something else ASAP!!!!
    if (mEvent->runInfo()!=0) 
      {
	bField = mEvent->runInfo()->magneticField();
	bField = bField*tesla*.1;
      }
    else bField =  0.5; 
    /*
      cout <<"Helix BField: "<<bField<<endl;
      cout <<"Helix Momentum: "<<helix.momentum(bField).x()
      <<helix.momentum(bField).y()
      <<helix.momentum(bField).z()
      <<endl;
    cout <<"Old phase: "<<helix.phase();
    */
    double newPhase = helix.phase() + helix.h()*halfpi;
    //cout <<"New phase: "<<newPhase<<endl;
    StTrackGeometry* geometry = new StHelixModel(helix.charge(bField),
				         	 newPhase,
						 helix.curvature(),
						 helix.dipAngle(),
						 origin, momF,
						 helix.h());
    //cout <<"Helix: "<<helix<<endl;
    
    if (outer)
      {
	gTrack->setOuterGeometry(geometry);

	//cout <<"Helix From Track: "<<gTrack->outerGeometry()->helix()<<endl;
      }
    else
      {
	gTrack->setGeometry(geometry);
	//cout <<"Helix From Track: "<<gTrack->geometry()->helix()<<endl;
      }

    
    return;
}

// void StiStEventFiller::fillTopologyMap(StTrack* gTrack, const StiTrack* track){
// 	cout << "StiStEventFiller::fillTopologyMap()" << endl;
//     int map1,map2;
//     map1 = map2 = 0;
//     // change: add code to set the bits appropriately here

//     StTrackTopologyMap topomap(map1,map2);
//     gTrack->setTopologyMap(topomap);
//     return;
// }

void StiStEventFiller::fillFitTraits(StTrack* gTrack, const StiTrack* track){
    // mass
    double massHyp = track->getMass();  // change: perhaps this mass is not set right?
    unsigned short geantIdPidHyp = 9999;
    if (.13< massHyp<.14) geantIdPidHyp = 9;
    
    unsigned short nFitPoints = encodedStEventFitPoints(track);
    
    
    // chi square and covariance matrix, plus other stuff from the
    // innermost track node
    const StiKalmanTrack* kTrack = dynamic_cast<const StiKalmanTrack*>(track);
    if (!kTrack) {
	cout << "StiStEventFiller::fillFitTraits(). ERROR:\t"
	     << "StiTrack can't be dynamic_cast'd to StiKalmanTrack.  Exit" <<endl;
	return;	
    }
    StiKalmanTrackNode* node = kTrack->getInnerMostHitNode();
    double alpha, xRef, x[5], covM[15], dEdxNode, chi2node;
    node->get(alpha,xRef,x,covM,dEdxNode,chi2node);
    //cout <<"GetAlpha: "<<alpha<<" Alpha: "<<node->fAlpha<<endl;
    float chi2[2];
    chi2[0] = chi2node; // change: perhaps use chi2node instead of track->getChi2()?
    chi2[1] = -9999; // change: here goes an actual probability, need to calculate?
    

    // @#$%^&
    // need to transform the covariant matrix from double's (Sti) to floats (StEvent)!

    float covMFloat[15];
    for (int ind = 0; ind<15; ++ind) covMFloat[ind] = static_cast<float>(covM[ind]);
    
    // setFitTraits uses assignment operator of StTrackFitTraits, which is the default one,
    // which does a memberwise copy.  Therefore, constructing a local instance of 
    // StTrackFitTraits is fine, as it will get properly copied.
    StTrackFitTraits fitTraits(geantIdPidHyp,nFitPoints,chi2,covMFloat);
    gTrack->setFitTraits(fitTraits); 
    return;
}

void StiStEventFiller::filldEdxInfo(StiDedxCalculator& dEdxCalculator, StTrack* gTrack, const StiTrack* track){
    double dEdx, errordEdx, nPoints;
    dEdx = errordEdx = nPoints = 9999;
    const StiKalmanTrack* kTrack = dynamic_cast<const StiKalmanTrack*>(track);
    if (kTrack) {
	dEdxCalculator.getDedx(kTrack, dEdx, errordEdx, nPoints);
    }
    
    StTrackPidTraits* pidTrait = new StDedxPidTraits(dEdxCalculator.whichDetId(),
						     static_cast<short>(kTruncatedMeanId),
						     static_cast<unsigned short>(nPoints),
						     static_cast<float>(dEdx),
						     static_cast<float>(errordEdx));
    gTrack->addPidTraits(pidTrait);
    return;
}
void StiStEventFiller::fillPidTraits(StTrack* gTrack, const StiTrack* track){

    // TPC
    filldEdxInfo(dEdxTpcCalculator,gTrack,track);

    // SVT
    filldEdxInfo(dEdxSvtCalculator,gTrack,track);

    return;
}
void StiStEventFiller::fillTrack(StTrack* gTrack, const StiTrack* track){
  
    //cout << "StiStEventFiller::fillTrack()" << endl;
    // data members from StTrack
    // flags http://www.star.bnl.gov/html/all_l/html/
    //  x=1 -> TPC only 
    // 	x=2 -> SVT only 
    // 	x=3 -> TPC + primary vertex 
    // 	x=4 -> SVT + primary vertex 
    // 	x=5 -> SVT+TPC 
    // 	x=6 -> SVT+TPC+primary vertex 
    // 	x=7 -> FTPC only 
    // 	x=8 -> FTPC+primary 
    
    if (gTrack->type()==global) {
	gTrack->setFlag(101); //change: make sure flag is ok
    }
    else if (gTrack->type()==primary) {
	gTrack->setFlag(301);
    }
    //     else {
    
    //     }
    
    // encoded method = 16 bits = 12 fitting and 4 finding, for the moment use:
    // kKalmanFitId
    // bit 15 for finding, (needs to be changed in StEvent).
    // change: make sure bits are ok, are the bits set up one in each position and nothing else?
    // this would mean that the encoded method is wasting space!
    // the problem is that in principle there might be combinations of finders for each tracking detector
    // but the integrated tracker will use only one for all detectors maybe
    // so need this bit pattern:
    // finding 100000000000     
    // fitting             0010 
    //            32768    +    2 = 32770;
    gTrack->setEncodedMethod(32770);
    
    gTrack->setImpactParameter(impactParameter(track));//gTrack->setImpactParamter(track->getDca(vertex)); // change: need to calculate impact parameter or use 
    gTrack->setLength(track->getTrackLength());
    // StiTracks don't return this in the format used by StEvent,
    // change: StiTracks need methods to calculate this by detector
    int maxPoints = track->getMaxPointCount();
    
    gTrack->setNumberOfPossiblePoints(static_cast<unsigned short>(maxPoints));
    fillGeometry(gTrack, track, false); // inner geometry
    fillGeometry(gTrack, track, true);  // outer geometry
    fillFitTraits(gTrack, track);
    fillPidTraits(gTrack, track);
    return;
}
unsigned short StiStEventFiller::encodedStEventFitPoints(const StiTrack* track) {
    // need to write the fit points in StEvent following the convention
    // 1*tpc + 1000*svt + 10000*ssd (Helen/Spiros Oct 29, 1999)
    //vector<StHit*> hitVec = track->stHits();
    vector<StMeasuredPoint*> hitVec = track->stHits();
    
    unsigned short nFitTpc, nFitSvt, nFitSsd; // maybe need ftpc (east, west), emc, rich, tof, later
    nFitTpc = nFitSvt = nFitSsd = 0;
    
    // loop here to get the hits in each detector
    // use StDetectorId's and switch
    
    for (vector<StMeasuredPoint*>::iterator point = hitVec.begin(); point!=hitVec.end();++point) {
	StHit * hit = dynamic_cast<StHit *>(*point);
	if (hit) {
	    StDetectorId detId = hit->detector();
	    switch (detId) {
	    case kTpcId:
		++nFitTpc;
		break;
	    case kSvtId:
		++nFitSvt;
		break;
	    case kSsdId:
		++nFitSsd;
		break;
	    default:
		cout << "StiStEventFiller::encodedStEventFitPoints()\t"
		     << "hit->detector() " << (unsigned long)hit->detector() << " not forseen in the logic" << endl;
	    }
	}
    }
    //        1*tpc + 1000*svt     + 10000*ssd       (Helen/Spiros Oct 29, 1999)
    return (nFitTpc + 1000*nFitSvt + 10000*nFitSsd);
    
}
float StiStEventFiller::impactParameter(const StiTrack* track) {
    if (!mEvent->primaryVertex()) {
	return DBL_MAX;
    }
    
    // get the innermost hit node
    const StiKalmanTrack* kTrack = dynamic_cast<const StiKalmanTrack*>(track);
    StiKalmanTrackNode*	node = kTrack->getInnerMostHitNode();
    // construct a Helix using Ben's Routines
    StiGeometryTransform* transformer = StiGeometryTransform::instance();
    StThreeVector<double> dummyVec(-999,-999,-999);
    //StPhysicalHelix* helix = new StPhysicalHelix(dummyVec,dummyVec,-100.,-100);
    transformer->operator()(node, &helix);
    // these next lines are just to keep prototypes right, Ben uses StPhysicalHelix<double>
    // but StEvent uses StThreeVectorF for persistency...
    const StThreeVectorF& vxF = mEvent->primaryVertex()->position();
    StThreeVector<double> vxD(vxF.x(),vxF.y(),vxF.z());
    //cout << "primary vertex " << vxD << endl;
    // return distance of closest approach to primary vertex
    //cout << "helix " << helix << endl;
    float dca = static_cast<float>(helix.distance(vxD));
    //cout << "dca " << dca << endl;
    return dca;
}
