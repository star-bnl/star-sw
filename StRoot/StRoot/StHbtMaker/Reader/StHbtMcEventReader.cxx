#define HBT_BFIELD 0.25*tesla
#define DIFF_CUT_OFF 1.

#include "StHbtMaker/Reader/StHbtMcEventReader.h"
#include "StChain.h"

// StEvent stuff
#include "StEventTypes.h"
#include "StEventMaker/StEventMaker.h"
// StMcEvent stuff
#include "StMcEventTypes.hh"
#include "StMcEventMaker/StMcEventMaker.h"

// c++ stuff
//#include <typeinfo>
#include <math.h>

// hbt stuff
#include "SystemOfUnits.h"   // has "tesla" in it
#include "StHbtMaker/Infrastructure/StHbtTrackCollection.hh"
#include "StHbtMaker/Infrastructure/StHbtV0Collection.hh"

#include <Stiostream.h>
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
#include "StKaonZeroShort.hh"
#include "StLambda.hh"
#include "StAntiLambda.hh"
#include "StPhysicalHelix.hh"



#ifdef __ROOT__
ClassImp(StHbtMcEventReader)
#endif

#ifndef ST_NO_NAMESPACES
  using namespace units;
#endif

#define __2POWER16__ 65536

struct vertexFlag {
	      StMcVertex* vtx;
	      int primaryFlag; };

double dedxMean(double mass, double momentum){
  double dedxMean;
  double tpcDedxGain = 0.174325e-06;
  double tpcDedxOffset = -2.71889; 
  double tpcDedxRise = 776.626;
  
  double gamma = ::sqrt(::pow(momentum/mass,2)+1.);
  double beta = ::sqrt(1. - 1./::pow(gamma,2));
  double rise = tpcDedxRise*::pow(beta*gamma,2);      
  if ( beta > 0)
    dedxMean = tpcDedxGain/::pow(beta,2) * (0.5*::log(rise)-::pow(beta,2)- tpcDedxOffset);
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

  mMotherMinvYPt = new StHbt3DHisto("Mother_MinvYPt","Mother_MinvYPt",      100,0.,5., 80,-2.,2., 80,0.,4.);
  mMotherMinvYMt = new StHbt3DHisto("Mother_MinvYMt","Mother_MinvYMt",      100,0.,5., 80,-2.,2., 80,0.,4.);
  mMotherMinvEtaPt = new StHbt3DHisto("Mother_MinvEtaPt","Mother_MinvEtaPt",100,0.,5., 80,-2.,2., 80,0.,4.);
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

  // StMcEventMaker* mTempMaker = (StMcEventMaker*) mTheMcEventMaker;
  // mcEvent = mTempMaker->currentMcEvent();
  // 28sep2005 - mike lisa replaces the two lines above with the
  // following one as per http://www.star.bnl.gov/HyperNews-star/protected/get/starsoft/  
  mcEvent = (StMcEvent*) StMaker::GetChain()->GetDataSet("StMcEvent");

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

  if (VertexPosition.x() == VertexPosition.y() &&
      VertexPosition.y() == VertexPosition.z() ){//x==y==z --> mark for bad events from embedding
    cout << "StHbtMcEventReader - bad vertex !!! " << endl;
    return 0;
  }

    
  StHbtEvent* hbtEvent = new StHbtEvent;
  hbtEvent->SetEventNumber(EventNumber);
  hbtEvent->SetCtbMult(0);
  hbtEvent->SetZdcAdcEast(0);
  hbtEvent->SetZdcAdcWest(0);
  hbtEvent->SetNumberOfTpcHits(0);
  hbtEvent->SetNumberOfTracks(Mult);
  hbtEvent->SetNumberOfGoodTracks(Mult);  // same for now
  hbtEvent->SetReactionPlane(0.);
  hbtEvent->SetReactionPlaneSubEventDifference(0.);
  hbtEvent->SetPrimVertPos(VertexPosition); 

  // By now, all event-wise information has been extracted and stored in hbtEvent
  // see if it passes any front-loaded event cut
//   if (mEventCut){
//     cout << " performing event cut " << endl;
//     if (!(mEventCut->Pass(hbtEvent))){    // event failed! - return null pointer (but leave Reader status flag as "good")
//       delete hbtEvent;
//       return 0;
//     }
//   }

  StHbtThreeVector p;
  
  for (StMcTrackIterator iter=mcEvent->tracks().begin(); iter!=mcEvent->tracks().end(); iter++){
    StMcTrack*  track = *iter;

    // y-pt for mother particles
    if (track->particleDefinition()) {
      if (CheckPdgIdList(mAcceptedMothers,track->particleDefinition()->pdgEncoding())) {
	mMotherMinvYPt->Fill(track->fourMomentum().m(),track->rapidity(),track->pt());
	mMotherMinvYMt->Fill(track->fourMomentum().m(),track->rapidity(),track->fourMomentum().mt());
	mMotherMinvEtaPt->Fill(track->fourMomentum().m(),track->pseudoRapidity(),track->pt());
      }
    }


    // check Pdg Id of the StMcTrack and its mc-mother and mc-daughters
    int pdgCode = 0;
    int motherPdgCode = 0;
    int daughterPdgCode = 0;
    int motherTrackId = 0;
    if (CheckPdgIdLists()) {
      int check=0;
      if (!track->particleDefinition()) {
	cout << " track has no particle definiton " << endl;
	continue;
      }
      pdgCode = track->particleDefinition()->pdgEncoding();
      //this was just stupid... malisa 14nov01      motherPdgCode = track->parent(); // 0 if no start vertex 
      if (track->parent()) { 
	motherPdgCode = track->parent()->pdgId();
	motherTrackId = track->parent()->key();
      }
      if (motherPdgCode==333) cout << " phi  333" << endl;

      if ( track->stopVertex() == 0 ) {
	check += CheckPdgIdList(pdgCode,motherPdgCode,0);
	// 	    cout << " no stop vertex " << endl;
      }
      else {
	for (unsigned int iDaughter=0; iDaughter < track->stopVertex()->daughters().size()-1; iDaughter++) {
	  daughterPdgCode = track->stopVertex()->daughters()[iDaughter]->pdgId();
	  check += CheckPdgIdList(pdgCode,motherPdgCode,daughterPdgCode);
	  // 		cout << " daughterPdgCode " << daughterPdgCode;
	}
      }
      // 	cout << " check " << check << endl;
      if ( !(check) ) {
	continue;   // particle failed, continue with next track
      }
    }

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

    hbtTrack->SetTrackId(track->key()+motherTrackId*__2POWER16__);

    hbtTrack->SetNHits( nTpcHits );               // hits in Tpc
    hbtTrack->SetNHitsPossible(nTpcHits );        // hits in Tpc
    //cout << " NHits " << hbtTrack->NHits() << endl;

    // **************************************************************************************
    // set NSigma's, derivation in dedx distribustion from being a poin, kaon, proton  [sigma]
    // **************************************************************************************
    int geantPid = track->particleDefinition()->pdgEncoding();
    //cout << geantPid << " " << track->particleDefinition()->mass() << " " << track->particleDefinition()->charge() << endl;

    switch (geantPid) {
    case 11:  // intentional fall-through
    case -11:  // gid=211,-211 is pion
      hbtTrack->SetNSigmaElectron(0.);
      hbtTrack->SetNSigmaPion(-999.);
      hbtTrack->SetNSigmaKaon(-999.);
      hbtTrack->SetNSigmaProton(-999.);
      break;
    case 211:  // intentional fall-through
    case -211:  // gid=211,-211 is pion
      hbtTrack->SetNSigmaElectron(999.);
      hbtTrack->SetNSigmaPion(0.);
      hbtTrack->SetNSigmaKaon(-999.);
      hbtTrack->SetNSigmaProton(-999.);
      break;
    case 321:  // intentional fall-through
    case -321:  // gid=321,-321 is kaon
      hbtTrack->SetNSigmaElectron(999.);
      hbtTrack->SetNSigmaPion(999.0);
      hbtTrack->SetNSigmaKaon(0.);
      hbtTrack->SetNSigmaProton(-999.);
      break;
    case 2212:  // intentional fall-through
    case -2212:  // gid=2212,-2212 is proton
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

    //cout << "Nsig electron,pion,kaon,proton : " << hbtTrack->NSigmaElectron() << " ";
    //cout << hbtTrack->NSigmaPion() << " " hbtTrack->NSigmaKaon() << " " << hbtTrack->NSigmaProton() << " PDG code " << geantPId << endl;
   
    hbtTrack->SetdEdx( dedxMean( track->particleDefinition()->mass(),  track->momentum().mag() ) ); // not in mike's
    //cout << " dedx " << hbtTrack->Dedx() << endl;

    hbtTrack->SetPt( track->momentum().perp() );   
    //cout << " Pt " << hbtTrack->Pt() << endl;

    hbtTrack->SetCharge( (int)(track->particleDefinition()->charge()) ); 
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
  StKaonZeroShort* k0Short = StKaonZeroShort::instance();
  StLambda* lambda = StLambda::instance();
  StAntiLambda* antiLambda = StAntiLambda::instance();
  for (StMcVertexIterator vIter=mcEvent->vertices().begin(); vIter!=mcEvent->vertices().end(); vIter++){
    StMcVertex*  vertex = *vIter;
    StMcTrack* parent  = (StMcTrack*)vertex->parent();  // get parent
    if (parent) {
      if ( parent->particleDefinition() == k0Short ||
	   parent->particleDefinition() == lambda ||
	   parent->particleDefinition() == antiLambda &&
	   vertex->numberOfDaughters() == 2) {
#ifdef STHBTDEBUG 
	cout << " v0 Id : " << parent->particleDefinition()->name() << endl;
#endif
	StMcTrack* pos;
	StMcTrack* neg;
	if ( (*(vertex->daughters().begin()))->particleDefinition()->charge() > 0  ) {
	  pos = *(vertex->daughters().begin());  // positive daughter
	  neg = *(vertex->daughters().end()-1);  // negative daughter
	} 
	else if ( (*(vertex->daughters().begin()))->particleDefinition()->charge() < 0  ) {
	  neg = *(vertex->daughters().begin());  // negative daughter 
	  pos = *(vertex->daughters().end()-1);  // positive daughter
	}
	else continue;

	// fill the StHbtV0 structure
	StHbtV0* hbtv0 = new StHbtV0();
	hbtv0->SetdecayLengthV0( abs(vertex->position()-VertexPosition) );
	hbtv0->SetdecayVertexV0( vertex->position() );
	
	StPhysicalHelixD posHelix = StPhysicalHelixD( pos->momentum(), vertex->position(), 
						      HBT_BFIELD, pos->particleDefinition()->charge() ); 
	StPhysicalHelixD negHelix = StPhysicalHelixD( neg->momentum(), vertex->position(), 
						      HBT_BFIELD, neg->particleDefinition()->charge() ); 
	StPhysicalHelixD v0Helix = StPhysicalHelixD( pos->momentum()+neg->momentum(), vertex->position(), 
						     HBT_BFIELD, 0 );
 
	double posPathLength = posHelix.pathLength( vertex->position() );
	double negPathLength = negHelix.pathLength( vertex->position() );

	hbtv0->SetdcaV0Daughters( abs(posHelix.at(posPathLength)-negHelix.at(negPathLength))  );
	//cout << " dcaV0Daughters " << hbtv0->dcaV0Daughters() << endl;

	hbtv0->SetdcaV0ToPrimVertex( v0Helix.distance( VertexPosition ) );
	//cout << " dcaV0ToPrimVertex " << hbtv0->dcaV0ToPrimVertex() << endl;

	hbtv0->SetdcaPosToPrimVertex( posHelix.distance( VertexPosition ) );    // VertexPosition = prim vert pos
	hbtv0->SetdcaNegToPrimVertex( negHelix.distance( VertexPosition ) );    // VertexPosition = prim vert pos
    
	hbtv0->SetmomPos( pos->momentum() );
	hbtv0->SetmomNeg( neg->momentum() );

	hbtv0->SettpcHitsPos( pos->tpcHits().size() );
	hbtv0->SettpcHitsNeg( neg->tpcHits().size() );

	//hbtv0->SetTrackTopologyMapPos(0,pos->topologyMap().data(0));
	//hbtv0->SetTrackTopologyMapPos(1,pos->topologyMap().data(1));
	//hbtv0->SetTrackTopologyMapNeg(0,neg->topologyMap().data(0));
	//hbtv0->SetTrackTopologyMapNeg(1,neg->topologyMap().data(1));

	hbtv0->SetkeyPos(pos->key());
	hbtv0->SetkeyNeg(neg->key());

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
    }
  }
  cout << hbtEvent->V0Collection()->size() << " v0s pushed to collection" << endl;
  
  return hbtEvent;
}


