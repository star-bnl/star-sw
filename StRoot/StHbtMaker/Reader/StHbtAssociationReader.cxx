#define HBT_B_FIELD 0.5*tesla
#define DIFF_CUT_OFF 1.

#include "StHbtMaker/Reader/StHbtAssociationReader.h"
#include "StChain.h"
#include "TOrdCollection.h"

#include "StEvent.h"
#include "StGlobalTrack.h"
#include "StTrackNode.h"
#include "StContainers.h"
#include "StPrimaryVertex.h"
#include "StVertex.h"
#include "StMeasuredPoint.h"
#include "StDedxPidTraits.h"
#include "StTrackPidTraits.h"
#include "StTrackGeometry.h"

#include "StParticleTypes.hh"
#include "StTpcDedxPidAlgorithm.h"

#include <typeinfo>
#include <cmath>

#include "SystemOfUnits.h"   // has "tesla" in it
#include "StHbtMaker/Infrastructure/StHbtTrackCollection.hh"
#include "StEventMaker/StEventMaker.h"
#include "StHbtMaker/Infrastructure/StHbtV0Collection.hh"
#include "StV0MiniDstMaker/StV0MiniDstMaker.h"  
#include "StV0MiniDstMaker/StV0MiniDst.hh"
#include "StAssociationMaker/StAssociationMaker.h"

#include <iostream.h>
#include <stdlib.h>
#include <string>
#include <vector>
//#include "TStyle.h"
//#include "TCanvas.h"
#include "StMcEventMaker/StMcEventMaker.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StThreeVector.hh"
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"

#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"

#include "g2t_event.h"
#include "g2t_ftp_hit.h"
#include "g2t_svt_hit.h"
#include "g2t_tpc_hit.h"
#include "g2t_track.h"
#include "g2t_vertex.h"

#include "tables/St_g2t_event_Table.h"
#include "tables/St_g2t_ftp_hit_Table.h"
#include "tables/St_g2t_svt_hit_Table.h"
#include "tables/St_g2t_tpc_hit_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "tables/St_g2t_vertex_Table.h"

// #include "StMcEvent/StMemoryInfo.hh"

//#include "StMcEvent/StMcEvent.hh"
//#include "StMcEvent/StMcTrack.hh"
//#include "StMcEvent/StMcTpcHit.hh"
//#include "StMcEvent/StMcFtpcHit.hh"
//#include "StMcEvent/StMcSvtHit.hh"
//#include "StMcEvent/StMcVertex.hh"

#include "StMcEvent.hh"
#include "StMcTrack.hh"
#include "StMcTpcHit.hh"
#include "StMcFtpcHit.hh"
#include "StMcSvtHit.hh"
#include "StMcVertex.hh"

#include "StAssociationMaker/StTrackPairInfo.hh"
#include "StParticleDefinition.hh"
#include "StPhysicalHelix.hh"
  

#ifndef ST_NO_NAMESPACES
using namespace std;
#endif

//static double vertexCut = .0000025; // 25 nm (lifetime of the pi0)
struct vertexFlag {
	      StMcVertex* vtx;
	      int primaryFlag; };

// Here one gives values to data members that need them.
// So far the only data members I have are drawinit and currentMcEvent.
// but might be useful later on.  Look at St_QA_Maker.cxx file





//__________________
StHbtAssociationReader::StHbtAssociationReader() {
  mDiffCurrent = new StHbt1DHisto("Diff_current", " (p_real - p_mc ) / p_real ",100,-1.,1.);
  mDiff        = new StHbt1DHisto("Diff",         " (p_real - p_mc ) / p_real ",100,-1.,1.);
  mDiffMean    = new StHbt1DHisto("Diff_mean",    " mean of   (p_real - p_mc ) / p_real ",100,-1.,1.);
  mDiffRMS     = new StHbt1DHisto("Diff_sigma",   " sigma of  (p_real - p_mc ) / p_real ",100,0.,1.);
  mDiffEvents  = new StHbt2DHisto("Diff_sigma",   " (p_real - p_mc ) / p_real vs eventNumber",100,0.,1.,40,0.,39.);
  eventNumber=0;
  mEventCut=0;
  mTrackCut=0;
  mReaderStatus = 0;  // "good"
  mV0=0;
}
//__________________
StHbtAssociationReader::~StHbtAssociationReader(){
  if (mEventCut) delete mEventCut;
  if (mTrackCut) delete mTrackCut;
}   
//__________________
StHbtString StHbtAssociationReader::Report(){
  StHbtString temp = "\n This is the StHbtAssociationReader\n";
  temp += "---> EventCuts in Reader: ";
  if (mEventCut) {
    temp += mEventCut->Report();
  }
  else {
    temp += "NONE";
  }
  temp += "\n---> TrackCuts in Reader: ";
  if (mTrackCut) {
    temp += mTrackCut->Report();
  }
  else {
    temp += "NONE";
  }
  temp += "\n";
  return temp;
}
//__________________
StHbtEvent* StHbtAssociationReader::ReturnHbtEvent(){
  cout << " **************************************************************************************" << endl;
  cout << " StHbtAssociationReader::ReturnHbtEvent() :  Seconds elapsed since last call : " << difftime( time(0), timeStamp ) << endl;
  cout << " **************************************************************************************" << endl;
  timeStamp = time(0);

  cout << "StHbtAssociationReader::ReturnHbtEvent" << endl;
  cout << "StHbtAssociationReader::ReturnHbtEvent" << endl;
  cout << "StHbtAssociationReader::ReturnHbtEvent" << endl;
  cout << "StHbtAssociationReader::ReturnHbtEvent" << endl;
  cout << "StHbtAssociationReader::ReturnHbtEvent" << endl;
  cout << "StHbtAssociationReader::ReturnHbtEvent" << endl;
  
  // ********************************
  // get pointer to eventMaker, event
  // ********************************
  StEvent* rEvent = 0;
  StEventMaker* tempMaker = (StEventMaker*) mTheEventMaker;
  rEvent = tempMaker->event();
  if (!rEvent){
    cout << "StHbtAssociationReader - No StEvent!!! " << endl;
    return 0;
  }
  cout << " StEvent " << endl;
  // **************************************
  // get pointer to mcEventMaker, Event *
  // **************************************
  StMcEvent* mEvent = 0;  //!
  StMcEventMaker* mTempMaker = (StMcEventMaker*) mTheMcEventMaker;
  //mEvent = ((StMcEventMaker*) (StMcEventMaker*) gStChain->Maker("MCEvent"))->currentMcEvent();
  mEvent = mTempMaker->currentMcEvent();
  if (!mEvent){
    cout << "StHbtAssociationReader - No StMcEvent!!! " << endl;
    return 0;
  }

  cout << " McEvent " << endl;
  // ****************************************
  // get pointer to associationMaker, mcEvent
  // ****************************************
  StAssociationMaker* assoc = 0;
  assoc = (StAssociationMaker*) mTheAssociationMaker;
  //assoc = (StAssociationMaker*) gStChain->Maker("Associations");
  if (!assoc){
    cout << "StHbtAssociationReader - No StAssociationMaker!!! " << endl;
    cout << "StHbtAssociationReader - assoc " << assoc <<endl;
    return 0;
  }
  rcTpcHitMapType* theHitMap = 0;
  theHitMap = assoc->rcTpcHitMap();
  rcTrackMapType* theTrackMap = 0;
  theTrackMap = assoc->rcTrackMap();
  if (!theTrackMap){
    cout << "StHbtAssociationReader - No trackMap!!! " << endl;
    cout << "StHbtAssociationReader - theTrackMap " << theTrackMap <<endl;
    return 0;
  }
  if (!theHitMap){
    cout << "StHbtAssociationReader - No tpcHitMap!!! " << endl;
    cout << "StHbtAssociationReader - theHitMap " << theHitMap <<endl;
    return 0;
  }

  // o.k. we got a StEvent and a McEvent --> create hbtEvent
  
  // ******************
  // Event properties
  // ******************
  // ******************************
  // cross check event <--> mcEvent
  // ******************************
  cout << " **********************" << endl;
  cout << " StHbtAssociationReader" << endl;
  cout << " **********************" << endl;
  unsigned long mRunNumber = mEvent->runNumber();
  unsigned long rRunNumber = rEvent->runId();
  cout << " DST run:    #" << rRunNumber << endl;
  cout << " MC  run:    #" << mRunNumber << endl;
  unsigned long rEventNumber = 0;
  unsigned long mEventNumber = mEvent->eventNumber();
  cout << " DST event:  #" << rEventNumber << endl;
  cout << " MC  event:  #" << mEventNumber << endl;
  int rMult = rEvent->trackNodes().size();
  int mMult = mEvent->tracks().size();
  cout << " DST mult:  #" << rMult << endl;
  cout << " MC  mult:  #" << mMult << endl;
  StHbtThreeVector rVertexPosition = rEvent->primaryVertex()->position();
  StHbtThreeVector mVertexPosition = mEvent->primaryVertex()->position();
  cout << " DST primary Vertex #" << rVertexPosition << endl;
  cout << " MC  primary Vertex #" << mVertexPosition << endl;
  
  cout << "StHbtAssociationReader::ReturnHbtEvent - We have " << rMult << " tracks to store - we skip tracks with nhits==0" << endl;
    
  
  double pathlength;
  StHbtThreeVector p;
  StHbtThreeVector mp;
  
  mDiffCurrent->Reset();

  // what the hell is this?  StHbtTrackCollection dummyTrackCollection;
  float diff=0;

  StHbtEvent* hbtEvent = new StHbtEvent;

  hbtEvent->SetEventNumber(rEventNumber);
  hbtEvent->SetCtbMult(0.);
  hbtEvent->SetZdcAdcEast(0.);
  hbtEvent->SetZdcAdcWest(0.);
  hbtEvent->SetNumberOfTpcHits(0.);
  hbtEvent->SetNumberOfTracks(rMult);
  hbtEvent->SetReactionPlane(0.);
  hbtEvent->SetReactionPlaneError(0.);
  hbtEvent->SetPrimVertPos(rVertexPosition); 

  // By now, all event-wise information has been extracted and stored in hbtEvent
  // see if it passes any front-loaded event cut
  if (mEventCut){
    if (!(mEventCut->Pass(hbtEvent))){    // event failed! - return null pointer (but leave Reader status flag as "good")
      delete hbtEvent;
      return 0;
    }
  }


  for (rcTrackMapIter tIter=theTrackMap->begin(); tIter!=theTrackMap->end(); ++tIter){
    //    cout << "Doing track number " << ++icount << endl;
    StGlobalTrack* rTrack = (*tIter).first;
    // do I really got a track
    if (!rTrack) {
      continue;
    }
    // check number points in tpc
    int nhits = rTrack->detectorInfo()->numberOfPoints(kTpcId);
    //cout << "nhits\t" << nhits << endl;
    if (nhits==0) {
      //cout << "No hits -- skipping track (because it crashes otherwise)" << endl;
      continue;
    }

    // *********************************
    // checking for associated mc tracks
    // *********************************
    int numberOfassociatedTracks =  theTrackMap->count(rTrack);
    if (numberOfassociatedTracks !=1) {
      //      cout << " number of associated mc tracks : " << theTrackMap->count(rTrack) << endl;
      //      cout << " skip this track" << endl;
      continue;
    }
    // get dedxPidTraits
    //cout << " number of pidTraits " << rTrack->pidTraits().size();
    //cout << " number of pidTraits for tpc: " << rTrack->pidTraits(kTpcId).size() << endl;
    StTrackPidTraits* trackPidTraits; 
    int iPidTraitsCounter=0;
    do {
      iPidTraitsCounter++;
      trackPidTraits = rTrack->pidTraits(kTpcId)[iPidTraitsCounter];
    } while (iPidTraitsCounter < (int)rTrack->pidTraits(kTpcId).size() && (!trackPidTraits) );
    if (!trackPidTraits) {
      //cout << " No dEdx information from Tpc- skipping track with " << nhits << " hits"<< endl;
      continue;
    }
    const StDedxPidTraits* dedxPidTraits = (const StDedxPidTraits*)trackPidTraits;
    //cout << " dE/dx = " << dedxPidTraits->mean() << endl;

    // get fitTraits
    StTrackFitTraits fitTraits = rTrack->fitTraits();
    //cout << " got fitTraits " << endl;
      
    // **********************
    // get associated mctrack
    // **********************
    StMcTrack* mTrack = (*tIter).second->partnerMcTrack();
    /*
      if ((*tIter).second->commonTpcHits()<10) 
      continue;
    */
    int geantId =  mTrack->geantId();
    //cout << " partnerMcTrack geant Id           : " << mTrack->geantId() << endl;
    //cout << " partnerMcTrack particleDefinition : " << *(mTrack->particleDefinition()) << endl;
    
    //    cout << "Now getting the pidTraits" << endl;
    //StTrackPidTraits pidTraitsTemp = rTrack->pidTraits();
    //cout << " Got it"<<endl;
    
    // ****************************************
    // check momenta of real track and mc track
    // ****************************************
    pathlength = rTrack->geometry()->helix().pathLength( rVertexPosition );
    //cout << "pathlength\t" << pathlength << endl;
    p = rTrack->geometry()->helix().momentumAt(pathlength,HBT_B_FIELD);
    mp = mTrack->momentum();
    //cout << "p: " << p << endl;
    //cout << "mp: " << mp << endl;
    diff =  (p.mag()-mp.mag()) / p.mag();
    
    if ( fabs(diff) > DIFF_CUT_OFF ) {
      //      cout << "StHbtAssociationReader::ReturnEvent() : momenta diff " << 100 * diff << " %   -- track skipped -- " << endl;
      continue;
    }
    
    mDiff->Fill(diff,1.);
    mDiffEvents->Fill(diff,eventNumber,1.);
    mDiffCurrent->Fill(diff,1.);
    
    //    cout << "Getting readty to instantiate new StHbtTrack " << endl;
    // o.k., god track found, fill it
    StHbtTrack* hbtTrack = new StHbtTrack;
    
    //cout << "StHbtTrack instantiated " << endl;
    
    hbtTrack->SetNHits(nhits);
    
    switch (geantId) {
    case 8:  // intentional fall-through
    case 9:  // gid=8,9 is pion
      hbtTrack->SetNSigmaPion(0.);
      hbtTrack->SetNSigmaKaon(-999.);
      hbtTrack->SetNSigmaProton(-999.);
      break;
    case 11:  // intentional fall-through
    case 12:  // gid=11,12 is kaon
      hbtTrack->SetNSigmaPion(999.0);
      hbtTrack->SetNSigmaKaon(0.);
      hbtTrack->SetNSigmaProton(-999.);
      break;
    case 14:  // intentional fall-through
    case 15:  // gid=14,15 is proton
      hbtTrack->SetNSigmaPion(999.);
      hbtTrack->SetNSigmaKaon(999.);
      hbtTrack->SetNSigmaProton(0.);
      break;
    default:
      hbtTrack->SetNSigmaPion(999.);
      hbtTrack->SetNSigmaKaon(999.);
      hbtTrack->SetNSigmaProton(999.);
      break;
    }
    
    
    float dEdx = dedxPidTraits->mean();
    //cout << "dEdx\t" << dEdx << endl; 
    hbtTrack->SetdEdx(dEdx);
    
    double pathlength = rTrack->geometry()->helix().pathLength(rVertexPosition);
    //cout << "pathlength\t" << pathlength << endl;
    StHbtThreeVector p = rTrack->geometry()->helix().momentumAt(pathlength,HBT_B_FIELD);
    //cout << "p: " << p << endl;
    hbtTrack->SetP(p);
    
    StHbtThreeVector  DCAxyz = rTrack->geometry()->helix().at(pathlength)-rVertexPosition;
    //cout << "DCA\t\t" << DCAxyz << " " << DCAxyz.perp() << endl;
    hbtTrack->SetDCAxy( DCAxyz.perp() );
    hbtTrack->SetDCAz(  DCAxyz.z()  );
    
    hbtTrack->SetChiSquaredXY( rTrack->fitTraits().chi2(0) );
    hbtTrack->SetChiSquaredZ( rTrack->fitTraits().chi2(1) ); 
    
    StPhysicalHelixD&  helix = rTrack->geometry()->helix();
    hbtTrack->SetHelix( helix );
    
    float pt = sqrt(p[0]*p[0]+p[1]*p[1]);
    //cout << "pt\t\t\t" << pt << endl;
    //hbtTrack->SetPt(pt);
    
    hbtTrack->SetPt(pt);
    
    int charge = ( rTrack->geometry()->charge() );
    //cout << "charge\t\t\t\t" << charge << endl;
    hbtTrack->SetCharge(charge);
    
    //cout << "pushing..." <<endl;
    // By now, all track-wise information has been extracted and stored in hbtTrack
    // see if it passes any front-loaded event cut
    if (mTrackCut){
      if (!(mTrackCut->Pass(hbtTrack))){                  // track failed - delete it and skip the push_back
	delete hbtTrack;
	continue;
      }
    }
    
    hbtEvent->TrackCollection()->push_back(hbtTrack);
  }
  
  
  hbtEvent->SetNumberOfGoodTracks(hbtEvent->TrackCollection()->size());
  
  cout << "StHbtAssociationReader::ReturnEvent() : mean of momenta diff (accepted tracks)= " << mDiffCurrent->GetMean() << endl;
  cout << "StHbtAssociationReader::ReturnEvent() : rms  of momenta diff (accepted tracks)= " << mDiffCurrent->GetRMS() << endl;
  mDiffMean->Fill(mDiffCurrent->GetMean(),1.);
  mDiffRMS->Fill(mDiffCurrent->GetRMS(),1.);
  cout << "DiffCurrent   (p_real - p_mc ) / p_real                " << mDiffCurrent << endl;
  cout << "DiffEvents    (p_real - p_mc ) / p_real vs eventNumber " << mDiffEvents << endl;
  cout << "Diff          (p_real - p_mc ) / p_real                " << mDiff << endl;
  cout << "DiffMean       mean  of (p_real - p_mc ) / p_real      " << mDiffMean << endl;
  cout << "DiffSigma      sigma of (p_real - p_mc ) / p_real      " << mDiffRMS << endl;
  
  cout << "StHbtAssociationReader::Finish" << endl;
  eventNumber++;
  return hbtEvent;
}

ClassImp(StHbtAssociationReader)

