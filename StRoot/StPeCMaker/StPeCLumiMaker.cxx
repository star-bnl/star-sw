// $Id: StPeCLumiMaker.cxx,v 1.7 2007/04/28 17:56:33 perev Exp $
// $Log: StPeCLumiMaker.cxx,v $
// Revision 1.7  2007/04/28 17:56:33  perev
// Redundant StChain.h removed
//
// Revision 1.6  2003/02/01 18:50:18  yepes
// New Lumi version for MuDst
//
// Revision 1.5  2002/06/04 17:55:01  meissner
// filtering: filter all  UPC triggerwords
//
// Revision 1.3  2002/04/18 19:02:09  meissner
// Change Init to  InitRun
//
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



static const char rcsid[] = "$Id: StPeCLumiMaker.cxx,v 1.7 2007/04/28 17:56:33 perev Exp $";

ClassImp(StPeCLumiMaker)

StPeCLumiMaker::StPeCLumiMaker(const Char_t *name) : StMaker(name) {
  //  infoLevel = 0 ;
  //filter    = 0 ;
}

StPeCLumiMaker::~StPeCLumiMaker() {}

Int_t StPeCLumiMaker::Init() {
  cout << "StPeCLumiMaker::Init,  Do nothing!";
  return StMaker::Init();
}

Int_t StPeCLumiMaker::InitRun(Int_t runnr) {

  // if ( infoLevel > 0 ) 
  cout<<"StPeCLumiMaker: Initializing..run " << runnr <<endl;
//
//  Set uDst output file
//
  TString uDstFileName("StPeCMaker.tree.root");    
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
  TString  tEvtSel("evtsel");
  TString  tuDst("lumiDst");
  TString  tuDstSel("lumiDstSel");
  uDstFileName.ReplaceAll(tDst,tuDst);
  uDstFileName.ReplaceAll(tEvt,tuDst);
  uDstFileName.ReplaceAll(tEvtSel,tuDstSel);
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
  
  return StMaker::InitRun(runnr);
}


Int_t StPeCLumiMaker::Make() {

  // Count all the events
  //if ( infoLevel > 0 ) printf ( "StPeCLumiMaker::Make: Start \n" ) ;

  //Vladimir - first check if muDst is available, if not, see if StEvent is 

  if (muDst) {
    cout<<"StPeCLumiMaker: Reading muDst"<<endl;
    Int_t NTracks = muDst->globalTracks()->GetEntries();
    cout<<"StPeCLumiMaker: Number of tracks "<<NTracks<<endl;

    Int_t flag = kStOk ;
 
    if( NTracks > StPeCnMaxTracks ){
     cout<<"StPeCLumiMaker: Number of tracks: "<<NTracks<<endl;
     cout<<"Not a peripheral event (NTracks>15)"<<endl;
     flag = kStErr;
    }
    if( NTracks <= 1 ){
     cout<<"StPeCLumiMaker: Event has no tracks!"<<endl;
     flag = kStErr;
    }


    StL0Trigger &trig = muDst->event()->l0Trigger();
    int tw = trig.triggerWord();
    cout << "Trigger word " << tw << endl;
    if (tw == 0x3001 || tw==0x3002 || tw == 0x3011 || tw == 0x1001  ) {
     cout << "UPC trigger filter event"  << endl;
     flag= kStOk;
    }

    //fill LumiEntry
    LumiEntry->fill(muDst );
    // fill the tree 
    uDstTree->Fill();
    //return kStOk ;
    return flag ;

  }


  else {
   // look for StEvent
   StEvent* event = 0 ;
   event = (StEvent *) GetInputDS("StEvent");

   if (event){

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
    if( NTracks <= 1 ){
     cout<<"StPeCLumiMaker: Event has no tracks!"<<endl;
     flag = kStErr;
    }

    int tw = event->l0Trigger()->triggerWord();
    cout << "Trigger word " << tw << endl;
    if (tw == 0x3001 || tw==0x3002 || tw == 0x3011 || tw == 0x1001  ) {
     cout << "UPC trigger filter event"  << endl;
     flag= kStOk;
    }
    
    // put runnumber to fill function
    //   LumiEntry->fill(  event , filenumber);
    LumiEntry->fill(  event );
    // fill the tree 
    uDstTree->Fill();
    //return kStOk ;
    return flag ;
   }
   else {
    cout<<"StPeCLumiMaker: There was no StEvent!  Return."<<endl;
    return kStOK; 
   }
  }
}

Int_t StPeCLumiMaker::Finish() {
  cout << "StPeCLumiMaker: Finish" << endl;
  m_outfile->Write();
  m_outfile->Close();
  StMaker::Finish();
  return kStOK;
}


