// $Id: StPeCMaker.cxx,v 1.15 2001/02/14 18:34:44 yepes Exp $
// $Log: StPeCMaker.cxx,v $
// Revision 1.15  2001/02/14 18:34:44  yepes
// bug in deleting StEvent and the of of Make
//
// Revision 1.14  2001/02/13 17:54:41  yepes
// still problems on differnt platforms
//
// Revision 1.13  2001/02/13 16:33:58  yepes
// fix problem on Sun
//
// Revision 1.12  2001/02/12 21:15:55  yepes
// New version of StPeCMaker, lots of changes
//
// Revision 1.9  2000/04/21 19:09:49  nystrand
// Update StPeCPair class, new histograms
//
// Revision 1.8  2000/03/24 22:36:24  nystrand
// First version with StPeCEvent
//
// Revision 1.7  2000/01/20 23:03:08  nystrand
// First Version of StPeCMaker with new StEvent
//
// Revision 1.6  1999/09/24 01:23:19  fisyak
// Reduced Include Path
//
// Revision 1.5  1999/07/15 13:57:20  perev
// cleanup
//
// Revision 1.4  1999/06/27 22:45:29  fisyak
// Merge StRootEvent and StEvent
//
// Revision 1.3  1999/05/01 00:57:02  fisyak
// Change Clear function to defualt
//
// Revision 1.2  1999/04/08 16:37:15  nystrand
// MakeBranch,SetBranch removed
//
// Revision 1.1  1999/04/06 20:47:27  akio
// The first version
//
// Revision 1.0  1999/03/05 11:00:00  Nystrand
// initial version
//
//
///////////////////////////////////////////////////////////////////////////////
//
// StPeCMaker
//
// Description: 
//  Maker for Peripheral Collisions DST analysis
//  This version uses StPeCEvent
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Joakim Nystrand, LBNL
//
// History:
//
///////////////////////////////////////////////////////////////////////////////
#include "StPeCMaker.h"
#include "StEventTypes.h"
#include "StChain.h"
#include "Stypes.h"
#include "StMessMgr.h"
#include "TH1.h"
#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

#include "StTriggerDetectorCollection.h"
#include "StCtbTriggerDetector.h"
#include "StMwcTriggerDetector.h"
#include "StVpdTriggerDetector.h"
#include "StZdcTriggerDetector.h"
#include "StFtpcHitCollection.h"
#include "StFtpcPlaneHitCollection.h"
#include "StFtpcSectorHitCollection.h"

#include "StMcEventTypes.hh"
#include "StMcEventMaker/StMcEventMaker.h"
#include "StIOMaker/StIOMaker.h"



static const char rcsid[] = "$Id: StPeCMaker.cxx,v 1.15 2001/02/14 18:34:44 yepes Exp $";

ClassImp(StPeCMaker)

StPeCMaker::StPeCMaker(const Char_t *name) : StMaker(name) {
  infoLevel = 0 ;
  filter    = 0 ;
}

StPeCMaker::~StPeCMaker() {}



Int_t StPeCMaker::Init() {

  if ( infoLevel > 0 ) 
     cout<<"StPeCMaker: Initializing..."<<endl;
//
//  Set uDst output file
//
  TString uDstFileName("StPecMaker.uDst.root");    
  StIOMaker* pIOMaker = (StIOMaker*)GetMaker("IO");
  printf ( "pIOMaker %x \n", pIOMaker ) ;
  if ( pIOMaker) {
     uDstFileName = pIOMaker->GetFile() ;
     char* ccc = "/" ;
     Ssiz_t slashPosition = uDstFileName.Last(*ccc) ;
     if ( slashPosition != -1 &&
          slashPosition < uDstFileName.Length() ) 
	     uDstFileName.Remove(0,slashPosition+1);
  }

  TString  tDst("dst");
  TString  tEvt("event");
  TString  tuDst("uDst");
  uDstFileName.ReplaceAll(tDst,tuDst);
  uDstFileName.ReplaceAll(tEvt,tuDst);
  cout << "StPeCMaker: uDst output file: " << uDstFileName << endl;

  m_outfile   = new TFile( uDstFileName,"recreate");

  uDstTree = new TTree("uDst","Pcol uDst");
//  geantTree = new TTree("geant","Pcol geant Tree");

  // Instantiate StPeCEvent
  pevent  = new StPeCEvent();
  pevent->setInfoLevel(infoLevel);
  trigger = new StPeCTrigger() ;
  trigger->setInfoLevel ( infoLevel ) ;
  geant   = new StPeCGeant();
  //
  uDstTree->Branch ("Event","StPeCEvent",&pevent,64000,1);
  uDstTree->Branch ("Trigger","StPeCTrigger",&trigger,64000,1);
  uDstTree->Branch ("Geant","StPeCGeant",&geant,64000,1);

//geantTree->Branch ("AllGeant","StPeCGeant",&geant,64000,1);

  if ( infoLevel > 0 ) 
     cout<<"StPeCMaker: Initialization done!"<<endl;

  return StMaker::Init();
}


Int_t StPeCMaker::Make() {

  // Count all the events
  if ( infoLevel > 0 ) printf ( "StPeCMaker::Make: Start \n" ) ;

  StEvent* event = 0 ;
  event = (StEvent *) GetInputDS("StEvent");
  if (!event){
    cout<<"StPeCMaker: There was no StEvent!  Return."<<endl;
    return kStOK; 
  }
//
// Get StMcEvent
//
// StMcEvent *mEvent = 0;
// mEvent = ((StMcEventMaker *) GetMaker ("StMcEvent"))->currentMcEvent ();
// if (!mEvent) {
//    printf ( "!!!!!!!!!!!! no mEvent !!!!!!!!!!!!! \n" ) ;
// }

  TDataSet *geantBranch = 0 ;
  geantBranch = GetInputDS("geantBranch");
  if ( geantBranch ) {
//   if ( !geant2->fill ( geantBranch ) ) geantTree->Fill ( ) ;
     geant->fill ( geantBranch ) ;
  }

  // trigger simulations
  if ( infoLevel ) printf ( "StPeCMaker: trigger simulation \n" ) ;
  trigger->process(event);

  // Do this way since call to event->summary->numberOfTracks() crashes
  StSPtrVecTrackNode& tempn = event->trackNodes();
  Int_t NTracks=tempn.size();


  Int_t flag = kStOk ;

  if( NTracks > 10 ){
    cout<<"StPeCMaker: Number of good tracks: "<<NTracks<<endl;
    cout<<"Not a peripheral event (NTracks>15)"<<endl;
    flag = kStErr;
  }
  if( NTracks <= 0 ){
    cout<<"StPeCMaker: Event has no tracks!"<<endl;
    flag = kStErr;
  }

  // Fill StPeCEvent
  if ( flag == kStOk ) {
     if ( infoLevel ) printf ( "StPeCMaker: Fill StPeCEvent \n" ) ;
     pevent->fill ( event ) ;
     uDstTree->Fill();
//
//   Select only 4 prong candidates
//
     if      ( filter == 1 ) flag = Cuts       ( event, pevent ) ; 
     else if ( filter == 2 ) flag = Cuts4Prong ( event, pevent ) ; 
  }
   
  pevent->clear();
  geant->clear ( ) ;

  if ( filter ) return flag ;
  return kStOk ;
}


Int_t StPeCMaker::Cuts(StEvent *event, StPeCEvent *pevent){
  //get vertex info
  StPrimaryVertex* vtx = event->primaryVertex();
  if ( !vtx ) return kStErr;
   //
  if ( vtx->position().x() < -5. ) return kStErr ;
  if ( vtx->position().x() >  5. ) return kStErr ;
  if ( vtx->position().y() < -5. ) return kStErr ;
  if ( vtx->position().y() >  5. ) return kStErr ;
  if ( abs(vtx->position().z()) >  200. ) return kStErr ;
  // Select interesting events
  //
  if ( pevent->getNPriPairs() != 1 ) return kStErr ;
  StPeCPair *pair = pevent->getPriPair(0);
  if ( pair->getOpeningAngle()>3.0 ) return kStErr ;
  if ( pair->getSumCharge()  ) return kStErr ;
      
  return kStOK;
}



Int_t StPeCMaker::Cuts4Prong(StEvent *event, StPeCEvent *pevent){

  if ( pevent->getNTot     () != 4 ) return kStErr ;
  if ( fabs(pevent->getZVertex()) > 200 ) return kStErr ;
  if ( pevent->getNPriPairs() != 1 ) return kStErr ;
  if ( pevent->getNSecPairs() != 1 )return kStErr ; 
  if ( pevent->getPriPair(0)->getSumCharge() ) return kStErr ;
  if ( pevent->getSecPair(0)->getSumCharge() ) return kStErr ;
  if ( pevent->getPriPair(0)->getOpeningAngle() > 3 ) return kStErr ;
  if ( pevent->getSecPair(0)->getOpeningAngle() > 3 ) return kStErr ;
  return kStOK;
}

Int_t StPeCMaker::Finish() {
  m_outfile->Write();
  m_outfile->Close();
  StMaker::Finish();
  return kStOK;
}


