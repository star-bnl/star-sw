#define HBT_BFIELD 0.5*tesla
#define DIFF_CUT_OFF 1.

#include "StHbtMaker/Reader/StHbtMcEventReader.h"
#include "StChain.h"
#include "TOrdCollection.h"

// StEvent stuff
#include "StEventTypes.h"
#include "StEventMaker/StEventMaker.h"
// StMcEvent stuff
#include "StMcEventTypes.hh"
#include "StMcEventMaker/StMcEventMaker.h"

// c++ stuff
//#include <typeinfo>
#include <cmath>

// hbt stuff
#include "SystemOfUnits.h"   // has "tesla" in it
#include "StHbtMaker/Infrastructure/StHbtTrackCollection.hh"
#include "StHbtMaker/Infrastructure/StHbtV0Collection.hh"
// strangeness v0 stuff
#include "StV0MiniDstMaker/StV0MiniDstMaker.h"  
#include "StV0MiniDstMaker/StV0MiniDst.hh"

#include <iostream.h>
#include <stdlib.h>
#include <string>
#include <vector>

#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StThreeVector.hh"
#include "phys_constants.h"

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

#include "StMcEvent.hh"
#include "StMcTrack.hh"
#include "StMcTpcHit.hh"
#include "StMcFtpcHit.hh"
#include "StMcSvtHit.hh"
#include "StMcVertex.hh"

#include "StParticleDefinition.hh"
#include "StPhysicalHelix.hh"

#ifdef __ROOT__
ClassImp(StHbtMcEventReader)
#endif

#ifndef ST_NO_NAMESPACES
  using namespace units;
#endif

struct vertexFlag {
	      StMcVertex* vtx;
	      int primaryFlag; };

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
  StMcEvent* mcEvent = 0;  //!
  StMcEventMaker* tempMaker = (StMcEventMaker*) mTheMcEventMaker;
  mcEvent = tempMaker->currentMcEvent();
  if (!mcEvent){
    cout << "StHbtMcEventReader - No StMcEvent!!! " << endl;
    return 0;
  }
  
  // ******************
  // Event properties
  // ******************
  //unsigned long RunNumber = mcEvent->runNumber();
  //cout << " MC : run: #" << RunNumber;
  unsigned long EventNumber = mcEvent->eventNumber();
  //cout << " * event: #" << EventNumber;
  int Mult = mcEvent->tracks().size();
  //cout << " *  mult = " << Mult;
  //cout << "StHbtMcEventReader::ReturnHbtEvent - We have " << Mult << " tracks " << endl;

  StHbtThreeVector VertexPosition = mcEvent->primaryVertex()->position();  // using templates, everything is fine
  //cout << " *  primary Vertex = " << VertexPosition << endl;
    
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

  
  StHbtThreeVector p;
  
  for (StMcTrackIterator iter=mcEvent->tracks().begin(); iter!=mcEvent->tracks().end(); iter++){
    StMcTrack*  track = *iter;

    int nTpcHits = track->tpcHits().size();

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

    hbtTrack->SetNHits( nTpcHits );               // hits in Tpc
    hbtTrack->SetNHitsPossible(nTpcHits );        // hits in Tpc
    //cout << " NHits " << hbtTrack->NHits() << endl;

    // **************************************************************************************
    // set NSigma's, derivation in dedx distribustion from being a poin, kaon, proton  [sigma]
    // **************************************************************************************
    int geantPid = track->particleDefinition()->pdgEncoding();

    //cout << geantPid << " " << track->particleDefinition()->mass() << " " << track->particleDefinition()->charge() << endl;

    switch (geantPid) {
    case 211:  // intentional fall-through
    case -211:  // gid=211,-211 is pion
      hbtTrack->SetNSigmaPion(0.);
      hbtTrack->SetNSigmaKaon(-999.);
      hbtTrack->SetNSigmaProton(-999.);
      break;
    case 321:  // intentional fall-through
    case -321:  // gid=321,-321 is kaon
      hbtTrack->SetNSigmaPion(999.0);
      hbtTrack->SetNSigmaKaon(0.);
      hbtTrack->SetNSigmaProton(-999.);
      break;
    case 2212:  // intentional fall-through
    case -2212:  // gid=2212,-2212 is proton
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
    //cout << hbtTrack->NSigmaKaon() << " " << hbtTrack->NSigmaProton() << " PDG code " << geantPId << endl;
   
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

    hbtTrack->SetHelix(helix);


    double pathlength = helix.pathLength(VertexPosition);
    //cout << "pathlength\t" << pathlength << endl;
    StHbtThreeVector  DCAxyz = helix.at(pathlength)-VertexPosition;
    //cout << "DCA\t\t" << DCAxyz << " " << DCAxyz.perp() << endl;

    hbtTrack->SetDCAxy( DCAxyz.perp() );     // in xy-plane
    hbtTrack->SetDCAz( DCAxyz.z() );         // in z direction

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

  // ******************
  // fill v0 collection
  // ******************
  for (StMcVertexIterator vIter=mcEvent->vertices().begin(); vIter!=mcEvent->vertices().end(); vIter++){
    StMcVertex*  vertex = *vIter;
    int nDaughters = vertex->numberOfDaughters();
    long geantProcess = vertex->geantProcess();
    if ( nDaughters!=2) {
#ifdef HBTDEBUG
      cout << " geant Process : " << geantProcess  << endl;
      cout << " daughters : " << nDaughters  << endl;
#endif
      continue; // not a v0
    }

    //StMcTrack* parent = vertex->parent();
    StMcTrack* daughter1 = *(vertex->daughters().begin());
    StMcTrack* daughter2 = *(vertex->daughters().end()-1);
    //cout << parent << " " << daughter1 << " " << daughter2 << " " << endl;
    /*
      if (!(parent && daughter1 && daughter2)) {
      cout << " two daughters, but no parent " << endl;
      }
    */

    double daughter1Charge = daughter1->particleDefinition()->charge();
    double daughter2Charge = daughter2->particleDefinition()->charge();
    double parentCharge = daughter1Charge+daughter2Charge;
 
    if (!( parentCharge+daughter1Charge+daughter2Charge==0 && daughter1Charge*daughter2Charge<0 )) {
      continue;  // not a v0
    }
    
#ifdef HBTDEBUG
      cout << " got a v0 "  << endl;
      cout << " charge ";
      cout << daughter1->particleDefinition()->charge() << " ";
      cout << daughter2->particleDefinition()->charge() << " ";
      cout << endl;
      cout << " geantProcessId " << geantProcess << endl;
#endif    

    // fill the V0MiniDst structure
    StHbtV0* hbtv0 = new StHbtV0();
    hbtv0->SetdecayLengthV0( abs(vertex->position()-VertexPosition) );
    hbtv0->SetdecayVertexV0( vertex->position() );
    
    StMcTrack* pos;
    StMcTrack* neg;
    
    if ( daughter1Charge>0 ) {
      pos = daughter1;
      neg = daughter2;
    }
    else {
      pos = daughter2;
      neg = daughter1;
    }

    StPhysicalHelixD posHelix = StPhysicalHelixD( pos->momentum(), vertex->position(), HBT_BFIELD, pos->particleDefinition()->charge() ); 
    StPhysicalHelixD negHelix = StPhysicalHelixD( neg->momentum(), vertex->position(), HBT_BFIELD, neg->particleDefinition()->charge() ); 
    StPhysicalHelixD v0Helix = StPhysicalHelixD( pos->momentum()+neg->momentum(), vertex->position(), HBT_BFIELD, 0 );
 
    double posPathLength = posHelix.pathLength( vertex->position() );
    double negPathLength = negHelix.pathLength( vertex->position() );

    hbtv0->SetdcaV0Daughters( abs(posHelix.at(posPathLength)-negHelix.at(negPathLength))  );
    //cout << " dcaV0Daughters " << hbtv0->dcaV0Daughters() << endl;

    hbtv0->SetdcaV0ToPrimVertex( v0Helix.distance( VertexPosition ) );    // VertexPosition = prim vert pos
    //cout << " dcaV0ToPrimVertex " << hbtv0->dcaV0ToPrimVertex() << endl;

    hbtv0->SetdcaPosToPrimVertex( posHelix.distance( VertexPosition ) );    // VertexPosition = prim vert pos
    hbtv0->SetdcaNegToPrimVertex( negHelix.distance( VertexPosition ) );    // VertexPosition = prim vert pos
    
    hbtv0->SetmomPos( pos->momentum() );
    hbtv0->SetmomNeg( neg->momentum() );

    hbtv0->SettpcHitsPos( pos->tpcHits().size() );
    hbtv0->SettpcHitsNeg( neg->tpcHits().size() );

    StThreeVectorF v0P = pos->momentum()+neg->momentum();

    float eLambda=sqrt( pow((double)v0P.mag(),2.) + pow(M_LAMBDA,2.) );
    float rapLambda = 0.5*log( (eLambda+v0P.z()) / (eLambda-v0P.z()) );
    float tauLambda = M_LAMBDA*(hbtv0->decayLengthV0()) / sqrt( pow((double)v0P.mag(),2.) );
    hbtv0->SetrapLambda( rapLambda );
    hbtv0->SetcTauLambda( tauLambda );

    float eK0Short=sqrt( pow((double)v0P.mag(),2.) + pow(M_KAON_0_SHORT,2.) );
    float rapK0Short = 0.5*log( (eK0Short+v0P.z()) / (eK0Short-v0P.z()) );
    float tauK0Short = M_KAON_0_SHORT*(hbtv0->decayLengthV0()) / sqrt( pow((double)v0P.mag(),2.) );
    hbtv0->SetrapK0Short( rapK0Short );
    hbtv0->SetcTauK0Short( tauK0Short );

    /*
    hbtv0->SetidPos( pos->geantId() );
    hbtv0->SetidNeg( neg->geantId() );
    cout << pos->geantId() << " " << pos->geantId() << endl;
    */
    hbtv0->UpdateV0();

    // apply v0 cut
    if (mV0Cut){
      if (!(mV0Cut->Pass(hbtv0))){                  // track failed - delete it and skip the push_back
	delete hbtv0;
	continue;
      }
    }

    hbtEvent->V0Collection()->push_back(hbtv0);
  }
  //Store total number of v0s in v0minidst so can start from there next time
  cout << hbtEvent->V0Collection()->size() << " v0s pushed to collection" << endl;

  return hbtEvent;
}


