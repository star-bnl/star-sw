#undef TheWorldIsNice 
#define HBT_BFIELD 0.5*tesla
#define DIFF_CUT_OFF 1.

#include "StHbtMaker/Reader/StHbtMcEventReader.h"
#include "StChain.h"

// these StEvent files keep oscillating between ".hh" and ".h" files
// fortunately, they are used only here
//.h files
/*
  #include "StEvent.h"
  #include "StGlobalTrack.h"
  #include "StTpcDedxPid.h"
  #include "StDedxPid.h"
*/
#include "StEvent.h"
#include "StGlobalTrack.h"
#include "StTpcDedxPid.h"
#include "StDedxPid.h"

#include "SystemOfUnits.h"   // has "tesla" in it
#include "StHbtMaker/Infrastructure/StHbtTrackCollection.hh"
#include "StEventMaker/StEventMaker.h"
#include "StAssociationMaker/StAssociationMaker.h"

#include <iostream.h>
#include <stdlib.h>
#include <string>
#include <vector>
#include "TStyle.h"
#include "TCanvas.h"
#include "StMcEventMaker/StMcEventMaker.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StThreeVector.hh"

#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"

#include "tables/St_g2t_event_Table.h"
#include "tables/St_g2t_ftp_hit_Table.h"
#include "tables/St_g2t_svt_hit_Table.h"
#include "tables/St_g2t_tpc_hit_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "tables/St_g2t_vertex_Table.h"

// #include "StMemoryInfo.hh"

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



ClassImp(StHbtMcEventReader)


//__________________
StHbtMcEventReader::StHbtMcEventReader(){
  mReaderStatus = 0;
}
//__________________
//StHbtMcEventReader::~StHbtMcEventReader(){
//  /* no-op *//
//}
//__________________
StHbtString StHbtMcEventReader::Report(){
  StHbtString temp = "\n This is the StHbtMcEventReader - no Early Cuts applied\n";
  return temp;
}
//__________________
StHbtEvent* StHbtMcEventReader::ReturnHbtEvent(){
  cout << "StHbtMcEventReader::ReturnHbtEvent" << endl;
  
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
  cout << " ********************************************************************************" << endl;
  cout << " StHbtMcEventReader::ReturnHbtEvent() :  Seconds elapsed since last call : " << difftime( time(0), timeStamp ) << endl;
  cout << " ********************************************************************************" << endl;
  timeStamp = time(0);
  unsigned long RunNumber = Event->runNumber();
  cout << " MC : run: #" << RunNumber;
  unsigned long EventNumber = Event->eventNumber();
  cout << " * event: #" << EventNumber;
  int Mult = Event->trackCollection()->size();
  cout << " *  mult = " << Mult;
#ifdef TheWorldIsNice
  StHbtThreeVector VertexPosition = Event->primaryVertex()->position();  // using templates, everything is fine
#else
  StHbtThreeVector VertexPosition;
  VertexPosition.setX( Event->primaryVertex()->position().x() );         // the none template version can not cast 
  VertexPosition.setY( Event->primaryVertex()->position().y() );         // StThreeVectorD = StThreeVectorF
  VertexPosition.setZ( Event->primaryVertex()->position().z() );         // isn't it sad ? yes, it is !
#endif
  cout << " *  primary Vertex = " << VertexPosition << endl;
  
  // cout << "StHbtMcEventReader::ReturnHbtEvent - We have " << Mult << " tracks " << endl;
    
  StHbtEvent* hbtEvent = new StHbtEvent;

  hbtEvent->SetPrimVertPos(VertexPosition);
  
  StMcTrack*  track;
  int nTpcHits;
  int geantId;
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
    track = *iter;

    nTpcHits = track->tpcHits()->size();

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
    hbtTrack->SetNHitsPossible( 0. );        // hits in Tpc
    //cout << " NHits " << hbtTrack->NHits() << endl;

    // **************************************************************************************
    // set NSigma's, derivation in dedx distribustion from being a poin, kaon, proton  [sigma]
    // **************************************************************************************
    
    geantId = track->geantId();

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
    StPhysicalHelix* helix = new StPhysicalHelix( hbtTrack->P(), track->startVertex()->position(), HBT_BFIELD, hbtTrack->Charge() ); 
#else
    StHbtThreeVector tmpSV;
    tmpSV.setX( track->startVertex()->position().x() );
    tmpSV.setY( track->startVertex()->position().y() );
    tmpSV.setZ( track->startVertex()->position().z() );
    StPhysicalHelixD* helix = new StPhysicalHelixD( hbtTrack->P(), tmpSV, HBT_BFIELD, hbtTrack->Charge() ); 
#endif
    StHbtThreeVector dist= helix->distance(VertexPosition);
    hbtTrack->SetDCAxy( dist.perp() );     // in xy-plane
    hbtTrack->SetDCAz( dist.z() );         // in z direction

    hbtTrack->SetChiSquaredXY( 0.); 
    hbtTrack->SetChiSquaredZ( 0.); 

    hbtTrack->SetHelix(*helix);

    hbtEvent->TrackCollection()->push_back(hbtTrack);
  }
  
  hbtEvent->SetNumberOfTracks(hbtEvent->TrackCollection()->size() );
  hbtEvent->SetNumberOfGoodTracks(hbtEvent->TrackCollection()->size() );  // same for now
  return hbtEvent;
}
