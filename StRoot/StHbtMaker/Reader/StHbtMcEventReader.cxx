// Cons tries to include even if there is a  '#ifdef #endif' around
// For that reson I had to change all '#include' statements into
// '//(notTheY2KBuf)#include' 
#undef McEventExists
#ifdef McEventExists

#undef TheWorldIsNice 
#define HBT_BFIELD 0.5*tesla
#define DIFF_CUT_OFF 1.

//(notTheY2KBug)#include "StHbtMaker/Reader/StHbtMcEventReader.h"
//(notTheY2KBug)#include "StChain.h"
//(notTheY2KBug)#include "TOrdCollection.h"

//(notTheY2KBug)#include "StEvent.h"
//(notTheY2KBug)#include "StGlobalTrack.h"
//(notTheY2KBug)#include "StTpcDedxPid.h"
//(notTheY2KBug)#include "StDedxPid.h"

//(notTheY2KBug)#include "SystemOfUnits.h"   // has "tesla" in it
//(notTheY2KBug)#include "StHbtMaker/Infrastructure/StHbtTrackCollection.hh"
//(notTheY2KBug)#include "StHbtMaker/Infrastructure/StHbtV0Collection.hh"
//(notTheY2KBug)#include "StV0MiniDstMaker/StV0MiniDstMaker.h"  
//(notTheY2KBug)#include "StV0MiniDstMaker/StV0MiniDst.hh"
//(notTheY2KBug)#include "StEventMaker/StEventMaker.h"
//(notTheY2KBug)#include "StMcEventMaker/StMcEventMaker.h"
////(notTheY2KBug)#include "StAssociationMaker/StAssociationMaker.h"

//(notTheY2KBug)#include <iostream.h>
//(notTheY2KBug)#include <stdlib.h>
//(notTheY2KBug)#include <string>
//(notTheY2KBug)#include <vector>
////(notTheY2KBug)#include "TStyle.h"
////(notTheY2KBug)#include "TCanvas.h"


//(notTheY2KBug)#include "PhysicalConstants.h"
//(notTheY2KBug)#include "SystemOfUnits.h"
//(notTheY2KBug)#include "StThreeVector.hh"

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

//(notTheY2KBug)#include "StMcEvent.hh"
//(notTheY2KBug)#include "StMcTrack.hh"
//(notTheY2KBug)#include "StMcTpcHit.hh"
//(notTheY2KBug)#include "StMcFtpcHit.hh"
//(notTheY2KBug)#include "StMcSvtHit.hh"
//(notTheY2KBug)#include "StMcVertex.hh"

////(notTheY2KBug)#include "StAssociationMaker/StTrackPairInfo.hh"
//(notTheY2KBug)#include "StParticleDefinition.hh"
//(notTheY2KBug)#include "StPhysicalHelix.hh"




#ifndef ST_NO_NAMESPACES
using namespace std;
#endif

struct vertexFlag {
	      StMcVertex* vtx;
	      int primaryFlag; };

// Here one gives values to data members that need them.
// So far the only data members I have are drawinit and currentMcEvent.
// but might be useful later on.  Look at St_QA_Maker.cxx file


double dedxMean(double mass, double momentum){
  double dedxMean;
  double tpcDedxGain = 0.174325e-06;
  double tpcDedxOffset = -2.71889; 
  double tpcDedxRise = 776.626;
  
  double gamma = sqrt(pow(momentum/mass,2)+1.);
  double beta = sqrt(1. - 1./pow(gamma,2));
  double rise = tpcDedxRise*pow(beta*gamma,2);      
  if ( beta > 0)
    dedxMean = tpcDedxGain/pow(beta,2) * (0.5*log(rise)-pow(beta,2)- tpcDedxOffset);
  else
    dedxMean = 1000.;
  return dedxMean;
}



//ClassImp(StHbtMcEventReader)


//__________________
StHbtMcEventReader::StHbtMcEventReader(){
  mEventCut=0;
  mTrackCut=0;
  mV0Cut=0;
  mReaderStatus = 0;  // "good"
  mV0=0;
}
//__________________
StHbtMcEventReader::~StHbtMcEventReader(){
  if (mEventCut) delete mEventCut;
  if (mTrackCut) delete mTrackCut;
  if (mV0Cut) delete mV0Cut;
}
//__________________
StHbtString StHbtMcEventReader::Report(){
  StHbtString temp = "\n This is the StHbtMcEventReader\n";
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
  temp += "\n---> V0Cuts in Reader: ";
  if (mV0Cut) {
    temp += mV0Cut->Report();
  }
  else {
    temp += "NONE";
  }
  temp += "\n";
  return temp;
}
//__________________
StHbtEvent* StHbtMcEventReader::ReturnHbtEvent(){
  cout << " **************************************************************************************" << endl;
  cout << " StHbtMcEventReader::ReturnHbtEvent() :  Seconds elapsed since last call : " << difftime( time(0), timeStamp ) << endl;
  cout << " **************************************************************************************" << endl;
  timeStamp = time(0);
  
  // **************************************
  // get pointer to mcEventMaker, Event *
  // **************************************
  StMcEvent* Event = 0;  //!
  StMcEventMaker* tempMaker = (StMcEventMaker*) mTheMcEventMaker;
  //mEvent = ((StMcEventMaker*) (StMcEventMaker*) gStChain->Maker("MCEvent"))->currentMcEvent();
  Event = tempMaker->currentMcEvent();
  if (!Event){
    cout << "StHbtMcEventReader - No StMcEvent!!! " << endl;
    return 0;
  }
  
  // ******************
  // Event properties
  // ******************
  unsigned long RunNumber = Event->runNumber();
  //cout << " MC : run: #" << RunNumber;
  unsigned long EventNumber = Event->eventNumber();
  //cout << " * event: #" << EventNumber;
  int Mult = Event->trackCollection()->size();
  //cout << " *  mult = " << Mult;

  //#ifdef TheWorldIsNice
  StHbtThreeVector VertexPosition = Event->primaryVertex()->position();  // using templates, everything is fine
  //#else
  //  StHbtThreeVector VertexPosition;
  //VertexPosition.setX( Event->primaryVertex()->position().x() );         // the none template version can not cast 
  //VertexPosition.setY( Event->primaryVertex()->position().y() );         // StThreeVectorD = StThreeVectorF
  //VertexPosition.setZ( Event->primaryVertex()->position().z() );         // isn't it sad ? yes, it is !
  //#endif
  //cout << " *  primary Vertex = " << VertexPosition << endl;
  // cout << "StHbtMcEventReader::ReturnHbtEvent - We have " << Mult << " tracks " << endl;
    
  StHbtEvent* hbtEvent = new StHbtEvent;

  hbtEvent->SetEventNumber(EventNumber);
  hbtEvent->SetCtbMult(0.);
  hbtEvent->SetZdcAdcEast(0.);
  hbtEvent->SetZdcAdcWest(0.);
  hbtEvent->SetNumberOfTpcHits(0.);
  hbtEvent->SetNumberOfTracks(Mult);
  hbtEvent->SetNumberOfGoodTracks(Mult);  // same for now
  hbtEvent->SetReactionPlane(0.);
  hbtEvent->SetReactionPlaneError(0.);
  hbtEvent->SetPrimVertPos(VertexPosition); 

  // By now, all event-wise information has been extracted and stored in hbtEvent
  // see if it passes any front-loaded event cut
  if (mEventCut){
    cout << " performing event cut " << endl;
    if (!(mEventCut->Pass(hbtEvent))){    // event failed! - return null pointer (but leave Reader status flag as "good")
      delete hbtEvent;
      return 0;
    }
  }

  
  //  StDedxPid* tpcDedxPid;
  //  float nsigpi;
  //  float nsigk;
  //  float nsigprot;
  //  double pathlength;
  StHbtThreeVector p;
  //  float DCA;
  //float Dedx;
  //float pt;
  //int charge;
  
  for (StMcTrackIterator iter=Event->trackCollection()->begin();
       iter!=Event->trackCollection()->end();iter++){
    StMcTrack*  track = *iter;

    int nTpcHits = track->tpcHits()->size();

    if (nTpcHits==0) {
      //cout << "No hits in TPC-- skipping track " << endl;
      continue;
    }
    
    //cout << "nTpcHits\t" << nTpcHits << endl;
    
    //cout << "Getting readty to instantiate new StHbtTrack " << endl;
    
    StHbtTrack* hbtTrack = new StHbtTrack;
    //cout << "StHbtTrack instantiated " << endl;

#ifdef TheWorldIsNice
    hbtTrack->SetP( track->momentum() );     // set momentum
#else
    StHbtThreeVector tmpP;
    tmpP.setX( track->momentum().x() );
    tmpP.setY( track->momentum().y() );
    tmpP.setZ( track->momentum().z() );
    hbtTrack->SetP( tmpP );     // set momentum
#endif
    //cout << " P     " << hbtTrack->P() << endl;

    hbtTrack->SetNHits( nTpcHits );          // hits in Tpc
    hbtTrack->SetNHitsPossible(nTpcHits );        // hits in Tpc
    //cout << " NHits " << hbtTrack->NHits() << endl;

    // **************************************************************************************
    // set NSigma's, derivation in dedx distribustion from being a poin, kaon, proton  [sigma]
    // **************************************************************************************
    
    int geantId = track->geantId();

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

    //cout << "Nsig pion,kaon,proton : " << hbtTrack->NSigmaPion() << " ";
    //cout << hbtTrack->NSigmaKaon() << " " << hbtTrack->NSigmaProton() << " Geant ID " << geantId << endl;
   

    hbtTrack->SetdEdx( dedxMean( track->particleDefinition()->mass(),  track->momentum().mag() ) ); // not in mike's
    //cout << " dedx " << hbtTrack->Dedx() << endl;

    hbtTrack->SetPt( track->momentum().perp() );   
    //cout << " Pt " << hbtTrack->Pt() << endl;

    hbtTrack->SetCharge( track->particleDefinition()->charge() ); 
    //cout << " choarge " << hbtTrack->Charge() << endl;
    
#ifdef TheWorldIsNice
    StPhysicalHelix helix = StPhysicalHelix( hbtTrack->P(), track->startVertex()->position(), HBT_BFIELD, hbtTrack->Charge() ); 
#else
    StHbtThreeVector tmpSV;
    tmpSV.setX( track->startVertex()->position().x() );
    tmpSV.setY( track->startVertex()->position().y() );
    tmpSV.setZ( track->startVertex()->position().z() );
    StPhysicalHelixD helix = StPhysicalHelixD( hbtTrack->P(), tmpSV, HBT_BFIELD, hbtTrack->Charge() ); 
#endif
    StHbtThreeVector dist= helix.distance(VertexPosition);

    hbtTrack->SetHelix(helix);

    hbtTrack->SetDCAxy( dist.perp() );     // in xy-plane
    hbtTrack->SetDCAz( dist.z() );         // in z direction

    hbtTrack->SetChiSquaredXY( 0.); 
    hbtTrack->SetChiSquaredZ( 0.); 


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
  cout << hbtEvent->TrackCollection()->size() << " tracks pushed to collection" << endl;

 //Pick up pointer v0 minidst maker
  StV0MiniDstMaker* v0Maker = (StV0MiniDstMaker *) mTheV0Maker;
  if( ! v0Maker ) {
    cout << "Not doing v0 stuff" << endl;
    return hbtEvent; 
  }
  //Get collection
  
  mCollection = v0Maker->GetCollection();
  int n_v0 =0;
  if( mCollection ){
    n_v0 = mCollection->GetSize();
    //Loop over all v0s in collection for this event
    
    for( int i=mV0; i<n_v0; i++){
      StV0MiniDst* v0FromMiniDst = (StV0MiniDst *) mCollection->At(i);
      v0FromMiniDst->UpdateV0();
      StHbtV0* hbtV0 = new StHbtV0;
      hbtV0->SetdecayLengthV0(v0FromMiniDst->decayLengthV0());
      hbtV0->SetdecayVertexV0(v0FromMiniDst->decayVertexV0());
      hbtV0->SetdcaV0Daughters(v0FromMiniDst->dcaV0Daughters());
      hbtV0->SetdcaV0ToPrimVertex(v0FromMiniDst->dcaV0ToPrimVertex());
      hbtV0->SetdcaPosToPrimVertex(v0FromMiniDst->dcaPosToPrimVertex());
      hbtV0->SetdcaNegToPrimVertex(v0FromMiniDst->dcaNegToPrimVertex());
      hbtV0->SetmomPos(v0FromMiniDst->momPos());
      hbtV0->SetmomNeg(v0FromMiniDst->momNeg());
      hbtV0->SettpcHitsPos(v0FromMiniDst->tpcHitsPos());
      hbtV0->SettpcHitsNeg(v0FromMiniDst->tpcHitsNeg());
      hbtV0->SetmomV0(v0FromMiniDst->momV0());
      hbtV0->SetalphaV0(v0FromMiniDst->alphaV0());
      hbtV0->SetptArmV0(v0FromMiniDst->ptArmV0());
      hbtV0->SeteLambda(v0FromMiniDst->eLambda());
      hbtV0->SeteK0Short(v0FromMiniDst->eK0Short());
      hbtV0->SetePosProton(v0FromMiniDst->ePosProton());
      hbtV0->SetePosPion(v0FromMiniDst->ePosPion());
      hbtV0->SeteNegPion(v0FromMiniDst->eNegPion());
      hbtV0->SeteNegProton(v0FromMiniDst->eNegProton());
      hbtV0->SetmassLambda(v0FromMiniDst->massLambda());
      hbtV0->SetmassAntiLambda(v0FromMiniDst->massAntiLambda());
      hbtV0->SetmassK0Short(v0FromMiniDst->massK0Short());
      hbtV0->SetrapLambda(v0FromMiniDst->rapLambda());
      hbtV0->SetrapK0Short(v0FromMiniDst->rapK0Short());
      hbtV0->SetcTauLambda(v0FromMiniDst->cTauLambda());
      hbtV0->SetcTauK0Short(v0FromMiniDst->cTauK0Short());
      hbtV0->SetptV0(v0FromMiniDst->ptV0());
      hbtV0->SetptotV0(v0FromMiniDst->ptotV0());
      hbtV0->SetptPos(v0FromMiniDst->ptPos());
      hbtV0->SetptotPos(v0FromMiniDst->ptotPos());
      hbtV0->SetptNeg(v0FromMiniDst->ptNeg());
      hbtV0->SetptotNeg(v0FromMiniDst->ptotNeg());
      
      hbtEvent->V0Collection()->push_back(hbtV0);
    }
    //Store total number of v0s in v0minidst so can start from there next time
    cout << "**** n_v0 = " << n_v0 << "**mV0"   << n_v0-mV0 << endl;        //  "       "
    mV0 =n_v0;
  }

  return hbtEvent;
}


#endif
