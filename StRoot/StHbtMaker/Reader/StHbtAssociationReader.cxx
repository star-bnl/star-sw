// Cons tries to include even if there is a  '#ifdef #endif' around
// For that reson I had to change all '#include' statements into
// '//(notTheY2KBuf)#include' 
#undef McEventExists
#ifdef McEventExists


#define HBT_B_FIELD 0.5*tesla
#define DIFF_CUT_OFF 1.

//(notTheY2KBug)#include "StHbtMaker/Reader/StHbtAssociationReader.h"
//(notTheY2KBug)#include "StChain.h"
//(notTheY2KBug)#include "TOrdCollection.h"

//(notTheY2KBug)#include "StEvent.h"
//(notTheY2KBug)#include "StGlobalTrack.h"
//(notTheY2KBug)#include "StTpcDedxPid.h"
//(notTheY2KBug)#include "StDedxPid.h"

//(notTheY2KBug)#include "SystemOfUnits.h"   // has "tesla" in it
//(notTheY2KBug)#include "StHbtMaker/Infrastructure/StHbtTrackCollection.hh"
//(notTheY2KBug)#include "StEventMaker/StEventMaker.h"
//(notTheY2KBug)#include "StHbtMaker/Infrastructure/StHbtV0Collection.hh"
//(notTheY2KBug)#include "StV0MiniDstMaker/StV0MiniDstMaker.h"  
//(notTheY2KBug)#include "StV0MiniDstMaker/StV0MiniDst.hh"
//(notTheY2KBug)#include "StAssociationMaker/StAssociationMaker.h"

//(notTheY2KBug)#include <iostream.h>
//(notTheY2KBug)#include <stdlib.h>
//(notTheY2KBug)#include <string>
//(notTheY2KBug)#include <vector>
////(notTheY2KBug)#include "TStyle.h"
////(notTheY2KBug)#include "TCanvas.h"
//(notTheY2KBug)#include "StMcEventMaker/StMcEventMaker.h"
//(notTheY2KBug)#include "PhysicalConstants.h"
//(notTheY2KBug)#include "SystemOfUnits.h"
//(notTheY2KBug)#include "StThreeVector.hh"
//(notTheY2KBug)#include "StThreeVectorF.hh"
//(notTheY2KBug)#include "StThreeVectorD.hh"

//(notTheY2KBug)#include "StChain.h"
//(notTheY2KBug)#include "St_DataSet.h"
//(notTheY2KBug)#include "St_DataSetIter.h"

//(notTheY2KBug)#include "g2t_event.h"
//(notTheY2KBug)#include "g2t_ftp_hit.h"
//(notTheY2KBug)#include "g2t_svt_hit.h"
//(notTheY2KBug)#include "g2t_tpc_hit.h"
//(notTheY2KBug)#include "g2t_track.h"
//(notTheY2KBug)#include "g2t_vertex.h"

//(notTheY2KBug)#include "tables/St_g2t_event_Table.h"
//(notTheY2KBug)#include "tables/St_g2t_ftp_hit_Table.h"
//(notTheY2KBug)#include "tables/St_g2t_svt_hit_Table.h"
//(notTheY2KBug)#include "tables/St_g2t_tpc_hit_Table.h"
//(notTheY2KBug)#include "tables/St_g2t_track_Table.h"
//(notTheY2KBug)#include "tables/St_g2t_vertex_Table.h"

// //(notTheY2KBug)#include "StMcEvent/StMemoryInfo.hh"

////(notTheY2KBug)#include "StMcEvent/StMcEvent.hh"
////(notTheY2KBug)#include "StMcEvent/StMcTrack.hh"
////(notTheY2KBug)#include "StMcEvent/StMcTpcHit.hh"
////(notTheY2KBug)#include "StMcEvent/StMcFtpcHit.hh"
////(notTheY2KBug)#include "StMcEvent/StMcSvtHit.hh"
////(notTheY2KBug)#include "StMcEvent/StMcVertex.hh"

//(notTheY2KBug)#include "StMcEvent.hh"
//(notTheY2KBug)#include "StMcTrack.hh"
//(notTheY2KBug)#include "StMcTpcHit.hh"
//(notTheY2KBug)#include "StMcFtpcHit.hh"
//(notTheY2KBug)#include "StMcSvtHit.hh"
//(notTheY2KBug)#include "StMcVertex.hh"

//(notTheY2KBug)#include "StAssociationMaker/StTrackPairInfo.hh"
//(notTheY2KBug)#include "StParticleDefinition.hh"
//(notTheY2KBug)#include "StPhysicalHelix.hh"
  

#ifndef ST_NO_NAMESPACES
using namespace std;
#endif

static double vertexCut = .0000025; // 25 nm (lifetime of the pi0)
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
  tpcHitMapType* theHitMap = 0;
  theHitMap = assoc->tpcHitMap();
  trackMapType* theTrackMap = 0;
  theTrackMap = assoc->trackMap();
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
  unsigned long rRunNumber = rEvent->runNumber();
  cout << " DST run:    #" << rRunNumber << endl;
  cout << " MC  run:    #" << mRunNumber << endl;
  unsigned long rEventNumber = 0;
  unsigned long mEventNumber = mEvent->eventNumber();
  cout << " DST event:  #" << rEventNumber << endl;
  cout << " MC  event:  #" << mEventNumber << endl;
  int rMult = rEvent->trackCollection()->size();
  int mMult = mEvent->trackCollection()->size();
  cout << " DST mult:  #" << rMult << endl;
  cout << " MC  mult:  #" << mMult << endl;
  StHbtThreeVector rVertexPosition = rEvent->primaryVertex()->position();
  StHbtThreeVector mVertexPosition = mEvent->primaryVertex()->position();
  cout << " DST primary Vertex #" << rVertexPosition << endl;
  cout << " MC  primary Vertex #" << mVertexPosition << endl;
  
  cout << "StHbtAssociationReader::ReturnHbtEvent - We have " << rMult << " tracks to store - we skip tracks with nhits==0" << endl;
    
  
  //StMcTrack*     mTrack;
  //int numberOfassociatedTracks =  theTrackMap->count(rTrack);
  pair<trackMapIter,trackMapIter> trackBounds;
  StDedxPid* tpcDedxPid;
  double pathlength;
  StHbtThreeVector p;
  StHbtThreeVector mp;
  
  mDiffCurrent->Reset();

  // what the hell is this?  StHbtTrackCollection dummyTrackCollection;
  int icount=0;
  float diff=0;
  float diff_mean=0;

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

  for (StTrackIterator iter=rEvent->trackCollection()->begin();
       iter!=rEvent->trackCollection()->end();iter++){
    //    cout << "Doing track number " << ++icount << endl;
  StGlobalTrack* rTrack = *iter;
    int nhits = rTrack->numberOfTpcHits();
    //    cout << "nhits\t" << nhits << endl;
    if (nhits==0) {
      //      cout << "No hits -- skipping track (because it crashes otherwise)" << endl;
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



    // **********************
    // get associated mctrack
    // **********************
    trackBounds = theTrackMap->equal_range(rTrack);
    // equal-range return two iterators, 
    // first iterator points to the first matching entry theTrackMap 
    // second iterator points to the first not matching entry theTrackMap -- is this write 
    //cout << rTrack << " == " << (*trackBounds.first).first << "== " << (*trackBounds.second).first  << " ???" << endl;
    //                                         ^       ^
    //                   first of the pair ----+       +---- first element of the map
    StMcTrack* mTrack = (*trackBounds.first).second->partnerMcTrack();
    int geantId =  mTrack->geantId();
    //cout << " partnerMcTrack geant Id           : " << mTrack->geantId() << endl;
    //cout << " partnerMcTrack particleDefinition : " << *(mTrack->particleDefinition()) << endl;

    //    cout << "Now getting the pidTraits" << endl;
    //StTrackPidTraits pidTraitsTemp = rTrack->pidTraits();
    //cout << " Got it"<<endl;

    tpcDedxPid = rTrack->pidTraits().tpcDedxPid();
    if (!tpcDedxPid) {
      cout << "No dEdx information - skipping track with " << nhits << " hits"<<endl;
      continue;
    }

    // ****************************************
    // check momenta of real track and mc track
    // ****************************************
    pathlength = rTrack->helix().pathLength( rVertexPosition );
    //cout << "pathlength\t" << pathlength << endl;
    p = rTrack->helix().momentumAt(pathlength,HBT_B_FIELD);
    mp = (*trackBounds.first).second->partnerMcTrack()->momentum();
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


    float dEdx = rTrack->tpcDedx()->mean();
    //cout << "dEdx\t" << dEdx << endl; 
    hbtTrack->SetdEdx(dEdx);
    
    double pathlength = rTrack->helix().pathLength(rVertexPosition);
    //cout << "pathlength\t" << pathlength << endl;
    StHbtThreeVector p = rTrack->helix().momentumAt(pathlength,HBT_B_FIELD);
    //cout << "p: " << p << endl;
    hbtTrack->SetP(p);

    StHbtThreeVector  DCAxyz = rTrack->helix().at(pathlength)-rVertexPosition;
    //cout << "DCA\t\t" << DCAxyz << " " << DCAxyz.perp() << endl;
    hbtTrack->SetDCAxy( DCAxyz.perp() );
    hbtTrack->SetDCAz(  DCAxyz.z()  );

    hbtTrack->SetChiSquaredXY( rTrack->fitTraits().chiSquaredInXY() );
    hbtTrack->SetChiSquaredZ( rTrack->fitTraits().chiSquaredInPlaneZ() ); 

    StPhysicalHelixD&  helix = rTrack->helix();
    hbtTrack->SetHelix( helix );

    float pt = sqrt(p[0]*p[0]+p[1]*p[1]);
    //cout << "pt\t\t\t" << pt << endl;
    //hbtTrack->SetPt(pt);

    hbtTrack->SetPt(pt);

    int charge = ((rTrack->helix().charge(HBT_B_FIELD)>0) ? 1 : -1);
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

#endif // McEventExists
