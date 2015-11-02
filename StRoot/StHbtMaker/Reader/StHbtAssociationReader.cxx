#define DIFF_CUT_OFF 1.

#include "StHbtMaker/Reader/StHbtAssociationReader.h"
#include "StHbtMaker/Infrastructure/StHbtTrackCollection.hh"
#include "StHbtMaker/Infrastructure/StHbtV0Collection.hh"
#include "StEventUtilities/StuRefMult.hh"

#include "StHbtMaker/Infrastructure/StHbtEvent.hh"
#include "StHbtMaker/Base/StHbtEventCut.h"
#include "StHbtMaker/Base/StHbtTrackCut.h"
#include "StHbtMaker/Base/StHbtV0Cut.h"
#include "StHbtMaker/Base/StHbtKinkCut.h"

#include "StChain.h"
#include "TOrdCollection.h"

#include "StEventTypes.h"
#include "StParticleTypes.hh"
#include "SystemOfUnits.h"   // has "tesla" in it
#include "StTpcDedxPidAlgorithm.h"
#include <math.h>
// #include "StGlobalTrack.h"
// #include "StTrackNode.h"
// #include "StContainers.h"
// #include "StPrimaryVertex.h"
// #include "StVertex.h"
// #include "StMeasuredPoint.h"
// #include "StDedxPidTraits.h"
// #include "StTrackPidTraits.h"
// #include "StTrackGeometry.h"
// #include "StTrackDetectorInfo.h"



#include "StAssociationMaker/StAssociationMaker.h"

#include "StStrangeMuDstMaker/StStrangeEvMuDst.hh"
#include "StStrangeMuDstMaker/StV0MuDst.hh"

#include "StMcEventTypes.hh"
#include "StMcEvent.hh"
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

// #include "StMcEvent.hh"
// #include "StMcTrack.hh"
// #include "StMcTpcHit.hh"
// #include "StMcFtpcHit.hh"
// #include "StMcSvtHit.hh"
// #include "StMcVertex.hh"

#include "StAssociationMaker/StTrackPairInfo.hh"
#include "StParticleDefinition.hh"
#include "StPhysicalHelix.hh"
  

#ifndef ST_NO_NAMESPACES
  using namespace units;
#endif

struct vertexFlag {
	      StMcVertex* vtx;
	      int primaryFlag; };

//__________________
StHbtAssociationReader::StHbtAssociationReader() : mPerfectPID(true) {
  mDiffCurrent = new StHbt1DHisto("Diff_current", " (p_real - p_mc ) / p_real ",100,-1.,1.);
  mDiff        = new StHbt1DHisto("Diff",         " (p_real - p_mc ) / p_real ",100,-1.,1.);
  mDiffMean    = new StHbt1DHisto("Diff_mean",    " mean of   (p_real - p_mc ) / p_real ",100,-1.,1.);
  mDiffRMS     = new StHbt1DHisto("Diff_sigma",   " sigma of  (p_real - p_mc ) / p_real ",100,0.,1.);
  eventNumber=0;
  mEventCut=0;
  mTrackCut=0;
  mReaderStatus = 0;  // "good"
  mV0=0;
}
//__________________
StHbtAssociationReader::~StHbtAssociationReader(){
  cout << " StHbtAssociationReader::~StHbtAssociationReader() " << endl;
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
  // ********************************
  // get pointer to eventMaker, event
  // ********************************

  // the following lines commented out as per S&C request September 2005 - malisa
  //  StEvent* rEvent = 0;
  // StEventMaker* tempMaker = (StEventMaker*) mTheEventMaker;
  //  rEvent = tempMaker->event();
  // and they are replaced by the one following line
  StEvent* rEvent = (StEvent*) StMaker::GetChain()->GetDataSet("StEvent"); 
  // end changes malisa sept2005
  if (!rEvent){
    cout << "StHbtAssociationReader - No StEvent!!! " << endl;
    return 0;
  }
  cout << " StEvent " << endl;
  // **************************************
  // get pointer to mcEventMaker, Event *
  // **************************************
  StMcEvent* mEvent = 0;  //!


  // malisa sept2005 - similar change as above
  // StMcEventMaker* mTempMaker = (StMcEventMaker*) mTheMcEventMaker;
  // mEvent = mTempMaker->currentMcEvent();
  // 28sep2005 - mike lisa replaces the two lines above with the
  // following one as per http://www.star.bnl.gov/HyperNews-star/protected/get/starsoft/  
  mEvent = (StMcEvent*) StMaker::GetChain()->GetDataSet("StMcEvent");


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
  if (!theHitMap){
    cout << "StHbtAssociationReader - No tpcHitMap!!! " << endl;
    cout << "StHbtAssociationReader - theHitMap " << theHitMap <<endl;
    return 0;
  }
  rcTrackMapType* theTrackMap = 0;
  theTrackMap = assoc->rcTrackMap();
  if (!theTrackMap){
    cout << "StHbtAssociationReader - No trackMap!!! " << endl;
    cout << "StHbtAssociationReader - theTrackMap " << theTrackMap <<endl;
    return 0;
  }
  rcV0MapType* theV0Map = 0;
  theV0Map = assoc->rcV0Map();
  if (!theV0Map){
    cout << "StHbtAssociationReader - No v0Map!!! " << endl;
    cout << "StHbtAssociationReader - theV0Map " << theV0Map <<endl;
    return 0;
  }

  if ( !(rEvent->primaryVertex()) || // no pointer to prim vertex
       (mEvent->primaryVertex()->position().x() == mEvent->primaryVertex()->position().y() && 
	mEvent->primaryVertex()->position().y() == mEvent->primaryVertex()->position().z()     ) ||// x==y==z --> mark for bad events from embedding
       ::isnan(rEvent->primaryVertex()->position().x())  // vertex position not a number 
       ) { 
    cout << "StHbtAssociationReader - bad vertex !!! " << endl;
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
  if ( !rEvent->primaryVertex() ) return 0;
  if ( !mEvent->primaryVertex() ) return 0;
  StHbtThreeVector rVertexPosition = rEvent->primaryVertex()->position();
  StHbtThreeVector mVertexPosition = mEvent->primaryVertex()->position();
  cout << " DST primary Vertex #" << rVertexPosition << endl;
  cout << " MC  primary Vertex #" << mVertexPosition << endl;
  
  cout << "StHbtAssociationReader::ReturnHbtEvent - We have " << rMult << " tracks to store - we skip tracks with nhits==0" << endl;
    
  double pathlength;
  StHbtThreeVector p;
  StHbtThreeVector mp;
  
  mDiffCurrent->Reset();

  float diff=0;

  StHbtEvent* hbtEvent = new StHbtEvent;

  hbtEvent->SetEventNumber(rEventNumber);
  hbtEvent->SetUncorrectedNumberOfPositivePrimaries(uncorrectedNumberOfPositivePrimaries(*rEvent));
  hbtEvent->SetUncorrectedNumberOfNegativePrimaries(uncorrectedNumberOfNegativePrimaries(*rEvent));
  hbtEvent->SetCtbMult(0);
  hbtEvent->SetZdcAdcEast(0);
  hbtEvent->SetZdcAdcWest(0);
  hbtEvent->SetNumberOfTpcHits(0);
  hbtEvent->SetNumberOfTracks(rMult);
  hbtEvent->SetReactionPlane(0.);
  hbtEvent->SetReactionPlaneSubEventDifference(0.);
  hbtEvent->SetPrimVertPos(rVertexPosition); 

  // By now, all event-wise information has been extracted and stored in hbtEvent
  // see if it passes any front-loaded event cut
//   if (mEventCut){
//     if (!(mEventCut->Pass(hbtEvent))){    // event failed! - return null pointer (but leave Reader status flag as "good")
//       delete hbtEvent;
//       return 0;
//     }
//   }

  StTpcDedxPidAlgorithm* PidAlgorithm = new StTpcDedxPidAlgorithm();

  if (!PidAlgorithm) cout << " StStandardHbtEventReader::ReturnHbtEvent() - Whoa!! No PidAlgorithm!! " << endl;

  // the following just point to particle definitions in StEvent
  StElectron* Electron = StElectron::instance();
  StPionPlus* Pion = StPionPlus::instance();
  StKaonPlus* Kaon = StKaonPlus::instance();
  StProton* Proton = StProton::instance();

  {for (rcTrackMapIter tIter=theTrackMap->begin(); tIter!=theTrackMap->end(); ++tIter){
    //    cout << "Doing track number " << ++icount << endl;
    const StGlobalTrack* rTrack = (*tIter).first;
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

    // while getting the bestGuess, the pidAlgorithm (StTpcDedxPidAlgorithm) is set up.
    // pointers to track and pidTraits are set 
    StParticleDefinition* BestGuess = (StParticleDefinition*)rTrack->pidTraits(*PidAlgorithm);
    //    if (BestGuess) cout << "best guess for particle is " << BestGuess->name() << endl; //2dec9
    if (!BestGuess){
      continue;
    }

    // **********************
    // get associated mctrack
    // **********************
    const StMcTrack* mTrack = (*tIter).second->partnerMcTrack();

    // check Pdg Id of the StMcTrack and its mc-mother and mc-daughters
    int pdgCode = 0;
    int motherPdgCode = 0;
    int daughterPdgCode =0;
    int motherTrackId =0;
    if (CheckPdgIdLists()) {
      int check=0;
      if (!mTrack->particleDefinition()) {
	cout << " track has no particle definiton " << endl;
	continue;
      }
      pdgCode = mTrack->particleDefinition()->pdgEncoding();
      // this was just stupid... mal 14nov01      motherPdgCode = mTrack->parent(); // 0 if no start vertex 
      //      if (motherPdgCode) {
      if (mTrack->parent()) {
	motherPdgCode = mTrack->parent()->pdgId();
	motherTrackId = mTrack->parent()->key();
      }
      if (motherPdgCode) cout << " motherPdgCode :"  << motherPdgCode << endl;
      else cout << " mother has no pdgId "<< endl;  
      if ( mTrack->stopVertex() == 0 ) {
	check += CheckPdgIdList(pdgCode,motherPdgCode,0);
      }
      else {
	for (unsigned int iDaughter=0; iDaughter < mTrack->stopVertex()->daughters().size()-1; iDaughter++) {
	  daughterPdgCode = mTrack->stopVertex()->daughters()[iDaughter]->pdgId();
	  check += CheckPdgIdList(pdgCode,motherPdgCode,daughterPdgCode);
	}
      }
      if ( !(check) ) {
	continue;   // particle failed, continue with next track
      }
    }
    

    int geantId =  mTrack->geantId();


    //cout << " partnerMcTrack geant Id           : " << mTrack->geantId() << endl;
    //cout << " partnerMcTrack particleDefinition : " << *(mTrack->particleDefinition()) << endl;
    
    // ****************************************
    // check momenta of real track and mc track
    // ****************************************
    pathlength = rTrack->geometry()->helix().pathLength( rVertexPosition );
    //cout << "pathlength\t" << pathlength << endl;
    p = rTrack->geometry()->momentum();
    mp = mTrack->momentum();
    //cout << "p: " << p << endl;
    //cout << "mp: " << mp << endl;
    diff =  (p.mag()-mp.mag()) / p.mag();
    
    if ( fabs(diff) > DIFF_CUT_OFF ) {
      //      cout << "StHbtAssociationReader::ReturnEvent() : momenta diff " << 100 * diff << " %   -- track skipped -- " << endl;
      continue;
    }
    
    mDiff->Fill(diff,1.);
    mDiffCurrent->Fill(diff,1.);
    
    //    cout << "Getting readty to instantiate new StHbtTrack " << endl;
    // o.k., god track found, fill it
    StHbtTrack* hbtTrack = new StHbtTrack;
    
    //cout << "StHbtTrack instantiated " << endl;
    
    hbtTrack->SetTrackId((int)(rTrack->key()+motherTrackId*::pow(2,16)));

    hbtTrack->SetNHits(nhits);
    
    if (mPerfectPID) {
      switch (geantId) {
      case 2:  // intentional fall-through
      case 3:  // gid=2,3 is electron
	hbtTrack->SetNSigmaElectron(0.);
	hbtTrack->SetNSigmaPion(-999);
	hbtTrack->SetNSigmaKaon(-999.);
	hbtTrack->SetNSigmaProton(-999.);
	break;
      case 8:  // intentional fall-through
      case 9:  // gid=8,9 is pion
	hbtTrack->SetNSigmaElectron(999.);
	hbtTrack->SetNSigmaPion(0.);
	hbtTrack->SetNSigmaKaon(-999.);
	hbtTrack->SetNSigmaProton(-999.);
	break;
      case 11:  // intentional fall-through
      case 12:  // gid=11,12 is kaon
	hbtTrack->SetNSigmaElectron(999.);
	hbtTrack->SetNSigmaPion(999.0);
	hbtTrack->SetNSigmaKaon(0.);
	hbtTrack->SetNSigmaProton(-999.);
	break;
      case 14:  // intentional fall-through
      case 15:  // gid=14,15 is proton
	hbtTrack->SetNSigmaElectron(999.);
	hbtTrack->SetNSigmaPion(999.);
	hbtTrack->SetNSigmaKaon(999.);
	hbtTrack->SetNSigmaProton(0.);
	break;
      default:
	hbtTrack->SetNSigmaElectron(999.);
	hbtTrack->SetNSigmaPion(999.);
	hbtTrack->SetNSigmaKaon(999.);
	hbtTrack->SetNSigmaProton(999.);
	break;
      }
    }
    else {
      hbtTrack->SetNSigmaElectron(PidAlgorithm->numberOfSigma(Electron));
      hbtTrack->SetNSigmaPion(PidAlgorithm->numberOfSigma(Pion));
      hbtTrack->SetNSigmaKaon(PidAlgorithm->numberOfSigma(Kaon));
      hbtTrack->SetNSigmaProton(PidAlgorithm->numberOfSigma(Proton));
    }
    

    //cout << "dEdx\t" << PidAlgorithm->traits()->mean() << endl; 
    hbtTrack->SetdEdx(PidAlgorithm->traits()->mean());
      
    double pathlength = rTrack->geometry()->helix().pathLength(rVertexPosition);
    //cout << "pathlength\t" << pathlength << endl;
    StHbtThreeVector p = rTrack->geometry()->momentum();
    //cout << "p: " << p << endl;
    hbtTrack->SetP(p);
    
    StHbtThreeVector  DCAxyz = rTrack->geometry()->helix().at(pathlength)-rVertexPosition;
    //cout << "DCA\t\t" << DCAxyz << " " << DCAxyz.perp() << endl;
    hbtTrack->SetDCAxy( DCAxyz.perp() );
    hbtTrack->SetDCAz(  DCAxyz.z()  );
    
    hbtTrack->SetChiSquaredXY( rTrack->fitTraits().chi2(0) );
    hbtTrack->SetChiSquaredZ( rTrack->fitTraits().chi2(1) ); 
    
    StPhysicalHelixD  helix = rTrack->geometry()->helix();
    hbtTrack->SetHelix( helix );
    
    float pt = ::sqrt(p[0]*p[0]+p[1]*p[1]);
    //cout << "pt\t\t\t" << pt << endl;
    //hbtTrack->SetPt(pt);
    
    hbtTrack->SetPt(pt);
    
    int charge = ( rTrack->geometry()->charge() );
    //cout << "charge\t\t\t\t" << charge << endl;
    hbtTrack->SetCharge(charge);
    
    hbtTrack->SetTopologyMap( 0, rTrack->topologyMap().data(0) );
    hbtTrack->SetTopologyMap( 1, rTrack->topologyMap().data(1) );

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
  }}
  
  
  hbtEvent->SetNumberOfGoodTracks(hbtEvent->TrackCollection()->size());
  
  cout << " StHbtAssociationReader::ReturnEvent() : mean of momenta diff (accepted tracks)= " << mDiffCurrent->GetMean() << endl;
  cout << " StHbtAssociationReader::ReturnEvent() : rms  of momenta diff (accepted tracks)= " << mDiffCurrent->GetRMS() << endl;
  mDiffMean->Fill(mDiffCurrent->GetMean(),1.);
  mDiffRMS->Fill(mDiffCurrent->GetRMS(),1.);
  cout << " StHbtAssociationReader::ReturnEvent() : DiffCurrent (p_real - p_mc ) / p_real                " << mDiffCurrent << endl;
  cout << " StHbtAssociationReader::ReturnEvent() : Diff        (p_real - p_mc ) / p_real                " << mDiff << endl;
  cout << " StHbtAssociationReader::ReturnEvent() : DiffMean    mean  of (p_real - p_mc ) / p_real      " << mDiffMean << endl;
  cout << " StHbtAssociationReader::ReturnEvent() : DiffSigma   sigma of (p_real - p_mc ) / p_real      " << mDiffRMS << endl;
  

  //the fastes way to fill the StHbtV0 is to create the StStrangeEvMuDst with the StV0MuDst and the copy into StHbtV0
  StStrangeEvMuDst strangeEvMuDst(*rEvent);
  // loop over all the StV0Vertices
  {for (rcV0MapIter tIter=theV0Map->begin(); tIter!=theV0Map->end(); ++tIter){
    StV0Vertex* rV0Vertex = (StV0Vertex*) (*tIter).first; // got the V0 from reconstruction
    StV0MuDst v0MuDst(rV0Vertex,&strangeEvMuDst);         // create the StV0MuDst
    cout << " StHbtAssociationEventReader::ReturnHbtEvent() " << theV0Map->count(rV0Vertex) << " associated V0s " << endl;
    // loop over associated tracks // maybe later you want to check particle ids
    pair<rcV0MapIter, rcV0MapIter> boundsV0 = theV0Map->equal_range(rV0Vertex);
    for (rcV0MapIter v0Iter = boundsV0.first; v0Iter!= boundsV0.second; v0Iter++){ // loop over all associated V0s
    }
    StHbtV0* hbtV0 = new StHbtV0(v0MuDst);       // copy into StHbtV0;
    if (mV0Cut){                                 // check whether vo passes cuts
      if (!(mV0Cut->Pass(hbtV0))) {              // track failed - delete it and skip the push_back
	delete hbtV0;
	continue;
      }
    }
    hbtEvent->V0Collection()->push_back(hbtV0);  // good V0 fill in collection
  }}
  
  cout << " StHbtAssociationReader::ReturnHbtEvent() - " << hbtEvent->V0Collection()->size();
  cout << " V0s pushed in collection " << endl;

  eventNumber++;
  return hbtEvent;
}

ClassImp(StHbtAssociationReader)

