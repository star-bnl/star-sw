/******************************************************************************
 *
 * $Id: StFtpcTrackToStEvent.cc,v 1.22 2013/07/23 13:38:46 didenko Exp $
 *
 * Author: Markus D. Oldenburg 
 * (changed version of StiStEventFiller by Manuel Calderon de la Barca Sanchez)
 ******************************************************************************
 *
 *
 **************************************************************************/

//std
#include "Stiostream.h"
#include <algorithm>
#include <stdexcept>
using namespace std;

// SCL
#include "StPhysicalHelix.hh"
#include "StThreeVector.hh"
#include "StThreeVectorF.hh"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StTrackDefinitions.h"
#include "StTrackMethod.h"
#include "StDedxMethod.h"

//StEvent
#include "StPrimaryVertex.h"
#include "StEventTypes.h"
#include "StDetectorId.h"
#include "StHelix.hh"

//StFtpcTrackMaker
#include "StFtpcPoint.hh"
#include "StFtpcTrack.hh"
#include "StFtpcTrackingParams.hh"
#include "TObjArray.h"

#include "StFtpcTrackToStEvent.hh"

StFtpcTrackToStEvent::StFtpcTrackToStEvent() : mEvent(0), mTrackStore(0), mTrkNodeMap() {
  // encoded method = 16 bits = 12 fitting and 4 finding, Refer
  // to StTrackMethod.h and StTrackDefinitions.h in pams/global/inc/
  // and StEvent/StEnumerations.h
  unsigned short bit = 1 << ftpcConformal;  // shifting the "1" exactly ftpcConformal places to the left
  mStiEncoded = kHelix2StepId + bit; // adding that to the proper fitting Id
}

StFtpcTrackToStEvent::~StFtpcTrackToStEvent()
{
  //LOG_INFO <<"StFtpcTrackToStEvent::~StFtpcTrackToStEvent()"<<endm;
}

/*! 
  Algorithm:
  Loop over all tracks in the TObjArray of StFtpcTracks, doing for each track:
  - Create a new global track and associated information (see below)
    and set its data members according to the StFtpcTrack,
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

  The creation of the various objects needed by StGlobalTrack are taken care of in the methods:
  FillTopologyMap(), FillGeometry(), FillFitTraits(), which are called within FillGlobalTrack().
  
*/

StEvent* StFtpcTrackToStEvent::FillEvent(StEvent* e, TObjArray* t) {
  
  if (e==0 || t==0) {
    LOG_WARN <<"StFtpcTrackToStEvent::FillEvent(). ERROR:"
	 <<"Null StEvent ("<<e<<") || TObjArray of tracks ("<<t<<").  Exit"<<endm;
    return 0;
  }

  mEvent = e;
  mTrackStore = t;
  mTrkNodeMap.clear();
  StSPtrVecTrackNode& trNodeVec = mEvent->trackNodes(); 
  StSPtrVecTrackDetectorInfo& detInfoVec = mEvent->trackDetectorInfo(); 
  int errorCount=0; 
  
  int fillTrackCount1=0;
  int fillTrackCount2=0;

  int currentTrackKey = GetHighestTrackKey(trNodeVec);

  for (Int_t trackIt = 0; trackIt < mTrackStore->GetEntriesFast();++trackIt) {
    
    StFtpcTrack* kTrack = (StFtpcTrack*)mTrackStore->At(trackIt);
    if (kTrack->GetP()<=1e-6) continue; 	//sanity check (VP)
    StTrackDetectorInfo* detInfo = new StTrackDetectorInfo;
    FillDetectorInfo(detInfo, kTrack, kTRUE);
    // track node where the new StTrack will reside
    StTrackNode* trackNode = new StTrackNode;
    // actual filling of StTrack from StFtpcTrack
    StGlobalTrack* gTrack = new StGlobalTrack;
    
    try {
      fillTrackCount1++;
      FillTrack(gTrack, kTrack);
      // filling successful, set up relationships between objects
      detInfoVec.push_back(detInfo);
      gTrack->setDetectorInfo(detInfo);	
      gTrack->setKey(++currentTrackKey);
      trackNode->addTrack(gTrack);
      trNodeVec.push_back(trackNode);
      FillTopologyMap(gTrack, kTrack);
      mTrkNodeMap.insert(map<StFtpcTrack*,StTrackNode*>::value_type (kTrack, trNodeVec.back()));
      if (trackNode->entries(global)<1) {
	LOG_WARN << "StFtpcTrackToStEvent::FillEvent() - ERROR - Track Node has no entries!" << endm;
      }
      
      fillTrackCount2++;
    }
    
    catch (runtime_error & rte ) {
      LOG_WARN << "StFtpcTrackToStEvent::FillEvent() -W- runtime-e filling track"<<rte.what() << endm;
      delete trackNode;
      delete detInfo;
      delete gTrack;
    }

    catch (...) {
      LOG_WARN << "StFtpcTrackToStEvent::FillEvent() - WARNING - Unknown exception filling track."<<endm;
      delete trackNode;
      delete detInfo;
      delete gTrack;
    }
  }
  
  if (errorCount>4) {
    LOG_WARN << "There were "<<errorCount<<"runtime_errors while filling StEvent"<<endm;
  }
  
  return mEvent;
}


StEvent* StFtpcTrackToStEvent::FillEventPrimaries(StEvent* e, TObjArray* t) {

  if (!mTrkNodeMap.size()) {
    LOG_WARN <<"StFtpcTrackToStEvent::FillEventPrimaries(): "
	 << "Mapping between the StTrackNodes and the StFtpcTracks is empty.  Exit." << endm;
    return 0;
  }
  
  if (e==0 || t==0) {
    LOG_WARN <<"StFtpcTrackToStEvent::FillEventPrimaries(): "
	 <<"Null StEvent ("<<e<<") || TObjArray of tracks ("<<t<<").  Exit"<<endm;
    return 0;
  }

  mEvent = e;
  mTrackStore = t;
  StPrimaryVertex* vertex = mEvent->primaryVertex(0);
  StSPtrVecTrackDetectorInfo& detInfoVec = mEvent->trackDetectorInfo();

  if(!vertex) {
    LOG_WARN <<"Failed to find a primary vertex. No primary FTPC tracks written to StEvent."<<endm;
    return (StEvent*)NULL;
  }
  
  int skippedCount=0;
  // loop over StFtpcTracks
  int mTrackN=0;
  StFtpcTrack* kTrack;

  int fillTrackCount1=0;
  int fillTrackCount2=0;

  for (Int_t trackIt = 0; trackIt<mTrackStore->GetEntriesFast();++trackIt,++mTrackN) {
    
    kTrack = (StFtpcTrack*)mTrackStore->At(trackIt);
    
    if (kTrack==0) {
      throw runtime_error("StFtpcTrackToStEvent::FillEventPrimaries() -F-  (StFtpcTrack*)mTrackStore->At(trackIt)");
    }
    
    map<StFtpcTrack*, StTrackNode*>::iterator itKtrack = mTrkNodeMap.find(kTrack);

    if (itKtrack == mTrkNodeMap.end()) {
      continue;
//      throw runtime_error("StiStEventFiller::fillEventPrimaries() -F- itKtrack == mTrkNodeMap.end()");
    }
    
    StTrackNode* currentTrackNode = (*itKtrack).second;
    StGlobalTrack* currentGlobalTrack = static_cast<StGlobalTrack*>(currentTrackNode->track(global));
    
    if (kTrack->ComesFromMainVertex()) {
      
      fillTrackCount1++;
      
      if (currentTrackNode->entries()>10) {
	throw runtime_error("StFtpcTrackToStEvent::FillEventPrimaries() -F- currentTrackNode->entries()>10");
      }
      
      if (currentTrackNode->entries(global)<1) {
	throw runtime_error("StFtpcTrackToStEvent::FillEventPrimaries() -F- currentTrackNode->entries(global)<1");
      }
	
      // detector info
      StTrackDetectorInfo* detInfo = new StTrackDetectorInfo;
      FillDetectorInfo(detInfo, kTrack, kFALSE);
      StPrimaryTrack* pTrack = new StPrimaryTrack;
      
      try {
	FillTrack(pTrack, kTrack);
	// set up relationships between objects
	detInfoVec.push_back(detInfo);
	pTrack->setDetectorInfo(detInfo);
	pTrack->setKey(currentGlobalTrack->key());
	currentTrackNode->addTrack(pTrack);  // StTrackNode::addTrack() calls track->setNode(this);
	vertex->addDaughter(pTrack);
	FillTopologyMap(pTrack, kTrack);
	fillTrackCount2++;
      }
	
      catch (runtime_error & rte ) {
	LOG_WARN << "StFtpcTrackToStEvent::FillEventPrimaries() - runtime exception, filling track: "
	     << rte.what() << endm;
	delete detInfo;
	delete pTrack;
      }
	
      catch (...) {
	LOG_WARN << "StFtpcTrackToStEvent::FillEventPrimaries() - Unknown exception, filling track."<<endm;
	delete detInfo;
	delete pTrack;
      }
    } //end if primary
  } // Ftpc track loop
  
  if (skippedCount>0) LOG_WARN << "StFtpcTrackToStEvent::FillEventPrimaries() -I- A total of "<<skippedCount<<" StFtpcTracks were skipped"<<endm;
  mTrkNodeMap.clear();  // need to reset for the next event
  return mEvent;
}


void StFtpcTrackToStEvent::FillDetectorInfo(StTrackDetectorInfo* detInfo, StFtpcTrack* track, Bool_t global) {

  TObjArray *hitVec = track->GetHits();
  
  detInfo->setFirstPoint(((StFtpcPoint*)hitVec->Last())->GetStFtpcHit()->position());
  detInfo->setLastPoint(((StFtpcPoint*)hitVec->First())->GetStFtpcHit()->position());
  detInfo->setNumberOfPoints(EncodedStEventFitPoints(track), track->GetDetectorId());

  for (Int_t iHit = hitVec->GetEntriesFast()-1; iHit >= 0; iHit--) {  // revert order
    StFtpcPoint *pt = (StFtpcPoint*)hitVec->At(iHit);
    StHit *hh = dynamic_cast<StHit*>(pt->GetStFtpcHit());
    if (hh) detInfo->addHit(hh, global);
  }
}


void StFtpcTrackToStEvent::FillGeometry(StTrack* gTrack, StFtpcTrack* track, bool outer) {
  
  if (!gTrack) {
    throw runtime_error("StFtpcTrackToStEvent::FillGeometry() -F- gTrack==0");
  }
  
  if (!track) {
    throw runtime_error("StFtpcTrackToStEvent::FillGeometry() -F- track==0");
  }

  TVector3 hit;
  StThreeVectorF origin(0., 0., 0.);

  if (outer) {
    hit = track->GetLastPointOnTrack();
  }
   
  else {
    hit = track->GetFirstPointOnTrack();
  }

  //  radius at start of track (cm) 
  Double_t r0 = TMath::Sqrt(hit.X()*hit.X() + hit.Y()*hit.Y());
  
  //  azimuthal angle at start of track (deg)
  Double_t phi0 = TMath::ATan2(hit.Y(), hit.X());
  
  //  z-coordinate at start of track 
  Double_t z0 = hit.Z();
  
  //  momentum angle at start 
  Double_t psi = TMath::ATan2(track->GetPy(), track->GetPx());
  if (psi < 0.0) { 
    psi += 2*TMath::Pi(); 
  }
  
  origin.setX(r0 * TMath::Cos(phi0));
  origin.setY(r0 * TMath::Sin(phi0));
  origin.setZ(z0);
  
  // making some checks.  Seems the curvature is infinity sometimes and
  // the origin is sometimes filled with nan's...
  
  if (!finite(origin.x()) ||
      !finite(origin.y()) ||
      !finite(origin.z()) || !finite(track->curvature())) {
    LOG_ERROR << "StFtpcTrackToStEvent::FillGeometry() Encountered non-finite numbers!!!! Bail out completely!!! " << endm;
    
    abort();
  }
  
  if (outer) {

    //  momentum angle at last point on track

    Double_t Ptotal = TMath::Sqrt(track->GetPt()*track->GetPt() + track->GetPz()*track->GetPz());
    Double_t psiLast = psi + track->h()*track->curvature()*track->GetTrackLength()*(track->GetPt()/Ptotal);
    if (psiLast < 0.0) { 
       psiLast += 2*TMath::Pi(); 
    }
     
     StThreeVectorF P(track->GetPt() * TMath::Cos(psiLast), track->GetPt() * TMath::Sin(psiLast), track->GetPz());
  
     StTrackGeometry* geometry = new StHelixModel(short(track->GetCharge()),
					       psiLast,
					       track->curvature(),
					       TMath::ATan2(track->GetPz(),(track->GetPt()+1.e-10)), // dip angle
					       origin, 
					       P, 
					       track->h());

     gTrack->setOuterGeometry(geometry);
  }
  
  else {

     StThreeVectorF P(track->GetPt() * TMath::Cos(psi), track->GetPt() * TMath::Sin(psi), track->GetPz());
  
     StTrackGeometry* geometry = new StHelixModel(short(track->GetCharge()),
					       psi,
					       track->curvature(),
					       TMath::ATan2(track->GetPz(),(track->GetPt()+1.e-10)), // dip angle
					       origin, 
					       P, 
					       track->h());

     gTrack->setGeometry(geometry);
  }
  
  return;
}


void StFtpcTrackToStEvent::FillFitTraits(StTrack* gTrack, StFtpcTrack* track){
  // mass
  // this makes no sense right now... double massHyp = track->getMass();  // change: perhaps this mass is not set right?
  unsigned short geantIdPidHyp = 9999;
  //if (.13< massHyp<.14) 
  geantIdPidHyp = 0;
  unsigned short nFitPoints = EncodedStEventFitPoints(track);
  // chi square and covariance matrix, plus other stuff from the
  // innermost track node
  float chi2[2];
  chi2[0] = track->GetChiSq()[0]/(nFitPoints-3.);
  chi2[1] = track->GetChiSq()[1]/(nFitPoints-2.);
    
  float covMFloat[15];
  for (Int_t i = 0; i < 15; i++) {
    covMFloat[i]  = 0.;
  }
    
  // setFitTraits uses assignment operator of StTrackFitTraits, which is the default one,
  // which does a memberwise copy.  Therefore, constructing a local instance of 
  // StTrackFitTraits is fine, as it will get properly copied.
  StTrackFitTraits fitTraits(geantIdPidHyp, nFitPoints, chi2, covMFloat);
  fitTraits.setNumberOfFitPoints(nFitPoints, track->GetDetectorId());  // The vertex is not added as a fit point anymore.
  fitTraits.setPrimaryVertexUsedInFit(gTrack->type() == primary);   // The fitTraits are flagged as primary or global.
  gTrack->setFitTraits(fitTraits); 
  return;
}


void StFtpcTrackToStEvent::FilldEdxInfo(StTrack* gTrack, StFtpcTrack* track) {
  
  double dEdx = track->GetdEdx();
  double errordEdx = 0.;
  double nPoints = track->GetNumdEdxHits();
  short  method;
  
  if (StFtpcTrackingParams::Instance()->IdMethod() == 0) {
    method = kTruncatedMeanId;
  }
  
  else if (StFtpcTrackingParams::Instance()->IdMethod() == 1) {
    method = kEnsembleTruncatedMeanId;
  }
  
  else {
    method = kUndefinedMethodId;
  }
  
  if(!finite(dEdx) || dEdx>9999) {
    dEdx = 9999;
    errordEdx = dEdx;
    nPoints = 0;
    LOG_WARN <<"StFtpcTrackToStEvent::Error: dEdx non-finite."<<endm;
  }
  
  else if(!finite(errordEdx)) {
    dEdx = 9999;
    errordEdx = dEdx;
    nPoints=0;
    LOG_WARN <<"StFtpcTrackToStEvent::Error: errordEdx non-finite."<<endm;
  }
  
  StTrackPidTraits* pidTrait = new StDedxPidTraits(track->GetDetectorId(),
						   static_cast<short>(method),
						   static_cast<unsigned short>(nPoints),
						   static_cast<float>(dEdx),
						   static_cast<float>(errordEdx));
  gTrack->addPidTraits(pidTrait);
  return;
}


void StFtpcTrackToStEvent::FillPidTraits(StTrack* gTrack, StFtpcTrack* track) {

  FilldEdxInfo(gTrack, track);
  
  return;
}


void StFtpcTrackToStEvent::FillTrack(StTrack* gTrack, StFtpcTrack* track) {
  
  if (gTrack->type()==global) {
    
    Int_t flag = 700;
    if (track->ComesFromMainVertex()) { // primary track candidate
      flag += 1;  // 701
    } // else 700
    
    if (TMath::Abs(track->GetCharge()) != 1.) {
      flag = -flag+20;
    }
    
    if (TMath::Abs(track->GetPt()) < 1.e-3) {
      flag = -799;
    }
    
    gTrack->setFlag(flag);
  }
  
  else if (gTrack->type()==primary) { // is 'primary candidate' automatically
    gTrack->setFlag(801);
  }

  gTrack->setEncodedMethod(mStiEncoded); 
  
  gTrack->setImpactParameter(ImpactParameter(track));
  gTrack->setLength(track->GetTrackLength());
  gTrack->setNumberOfPossiblePoints(static_cast<unsigned short>(track->GetNMax()), track->GetDetectorId());

  FillGeometry(gTrack, track, false); // inner geometry
  FillGeometry(gTrack, track, true);  // outer geometry
  FillFitTraits(gTrack, track);
  FillPidTraits(gTrack, track);

  return;
}

unsigned short StFtpcTrackToStEvent::EncodedStEventFitPoints(StFtpcTrack* track) {
  // need to write the fit points in StEvent following the convention
  // 1*tpc + 1000*svt + 10000*ssd (Helen/Spiros Oct 29, 1999)
  // FTPC fit point are not encoded. They use the plain number.
  
  return ((unsigned short)track->GetHits()->GetEntriesFast());    
}


float StFtpcTrackToStEvent::ImpactParameter(StFtpcTrack* track) {
  
  if (!mEvent->primaryVertex()) {
    return 3.e33;
  }
  
  else {
    return track->GetDca();
  }
}


void StFtpcTrackToStEvent::FillTopologyMap(StTrack *gTrack, StFtpcTrack* ftpcTrack) {
  unsigned long word0 = (gTrack->type()==primary) ? 1 : 0;
  unsigned long word1 = (1<<31); // FTPC track flag

  for (Int_t i = 0; i < ftpcTrack->GetHits()->GetEntriesFast(); i++){
    StFtpcPoint *pt = (StFtpcPoint*)ftpcTrack->GetHits()->At(i);
    word0 |= (1<<pt->GetPadRow());
  }
  
  StTrackTopologyMap newmap(word0, word1);
  gTrack->setTopologyMap(newmap);
  
  return;
}


int StFtpcTrackToStEvent::GetHighestTrackKey(StSPtrVecTrackNode& trNodeVec) {
  // get the highest used track key
  // This was necessary to introduce since the highest track key is not
  // allways the same as the size of the StSPtrVecTrackNode (tpt leaves holes
  // in there).

  StTrack *globTrack;
  Int_t highestKey = 0;
  Int_t currentKey = 0;

  for (unsigned int j = 0; j < trNodeVec.size(); j++) {
    globTrack = trNodeVec[j]->track(global);
      if (globTrack==0) continue;

      currentKey = globTrack->key();
      if (currentKey > highestKey) highestKey = currentKey;
  }
      
  return highestKey;
}
