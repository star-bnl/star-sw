// $Id: StPeCLumiMaker.cxx,v 1.2 2002/03/20 17:42:14 meissner Exp $
// $Log: StPeCLumiMaker.cxx,v $
// Revision 1.2  2002/03/20 17:42:14  meissner
// buu fix //uDst->SetFormat
//
// Revision 1.1  2002/03/19 22:23:39  meissner
// New variables: zdc unatt., Trigger word, MC tree if Geant Branch, DCA  for primary pairs, all tracks for secondary pairs (Test)
//
//
// Revision 1.0  2001   Meissner
// initial version based on StPeCMaker
//
//
///////////////////////////////////////////////////////////////////////////////
//
// StPeCLumiMaker
//
// Description: 
// Small maker for Luminosity determination
// For each event some variables are written out (multiplicities, ZDC'x etc)
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Falk Meissner, LBNL, based on STPeCMaker
//
// History:
//
///////////////////////////////////////////////////////////////////////////////
#include "StPeCLumiMaker.h"
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



static const char rcsid[] = "$Id: StPeCLumiMaker.cxx,v 1.2 2002/03/20 17:42:14 meissner Exp $";

ClassImp(StPeCLumiMaker)

StPeCLumiMaker::StPeCLumiMaker(const Char_t *name) : StMaker(name) {
  //  infoLevel = 0 ;
  //filter    = 0 ;
}

StPeCLumiMaker::~StPeCLumiMaker() {}



Int_t StPeCLumiMaker::Init() {

  // if ( infoLevel > 0 ) 
     cout<<"StPeCLumiMaker: Initializing..."<<endl;
//
//  Set uDst output file
//
  TString uDstFileName("StPecMaker.uDst.root");    
  StIOMaker* pIOMaker = (StIOMaker*)GetMaker("IO");
  if ( pIOMaker) {
     uDstFileName = pIOMaker->GetFile() ;
     char* ccc = "/" ;
     Ssiz_t slashPosition = uDstFileName.Last(*ccc) ;
     if ( slashPosition != -1 &&
          slashPosition < uDstFileName.Length() ) 
	     uDstFileName.Remove(0,slashPosition+1);
  }

  cout << uDstFileName << endl;
  TString  tDst("dst");
  TString  tEvt("event");
  TString  tuDst("lumiDst");
  uDstFileName.ReplaceAll(tDst,tuDst);
  uDstFileName.ReplaceAll(tEvt,tuDst);
  cout << "StPeCLumiMaker: uDst output file: " << uDstFileName << endl;
  // Get runnumber from filename somewhow
  // filenumber= 

  m_outfile   = new TFile( uDstFileName,"recreate");
  // OLD not needed anymore
  // Get the standard root format to be independent of Star IO   
  // m_outfile->SetFormat(1);
  // m_outfile->SetCompressionLevel(1);
  
  
  uDstTree = new TTree("uDst","Pcol uDst",99);

  // Instantiate StPeCLumiEntry
  LumiEntry = new StPeCLumiEntry() ;
  uDstTree->Branch ("LumiEntry","StPeCLumiEntry",&LumiEntry,64000,1);
  
  //if ( infoLevel > 0 ) 
  cout<<"StPeCLumiMaker: Initialization done!"<<endl;
  
  return StMaker::Init();
}


Int_t StPeCLumiMaker::Make() {

  // Count all the events
  //if ( infoLevel > 0 ) printf ( "StPeCLumiMaker::Make: Start \n" ) ;

  StEvent* event = 0 ;
  event = (StEvent *) GetInputDS("StEvent");
  if (!event){
    cout<<"StPeCLumiMaker: There was no StEvent!  Return."<<endl;
    return kStOK; 
  }


  // Do this way since call to event->summary->numberOfTracks() crashes
  StSPtrVecTrackNode& tempn = event->trackNodes();
  Int_t NTracks=tempn.size();
  cout<<"StPeCLumiMaker: Number of  tracks: "<<NTracks<<endl;
  
  Int_t flag = kStOk ;

  if( NTracks > StPeCnMaxTracks ){
    cout<<"StPeCLumiMaker: Number of tracks: "<<NTracks<<endl;
    cout<<"Not a peripheral event (NTracks>15)"<<endl;
    flag = kStErr;
  }
  if( NTracks <= 0 ){
    cout<<"StPeCLumiMaker: Event has no tracks!"<<endl;
    flag = kStErr;
  }
  // put runnumber to fill function
  //   LumiEntry->fill(  event , filenumber);
  LumiEntry->fill(  event );
  // fill the tree 
  uDstTree->Fill();
  return kStOk ;
}

Int_t StPeCLumiMaker::Finish() {
  cout << "StPeCLumiMaker: Finish" << endl;
  m_outfile->Write();
  m_outfile->Close();
  StMaker::Finish();
  return kStOK;
}


