//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowMaker.cxx,v 1.24 2000/05/12 22:42:04 snelling Exp $
//
// Authors: Raimond Snellings and Art Poskanzer, LBNL, Jun 1999
//
//////////////////////////////////////////////////////////////////////
//
// Description: Maker to fill StFlowEvent from StEvent
//
//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowMaker.cxx,v $
// Revision 1.24  2000/05/12 22:42:04  snelling
// Additions for persistency and minor fix
//
// Revision 1.23  2000/05/11 20:00:35  posk
// Preparation for micro and nano DSTs.
//
// Revision 1.22  2000/03/28 23:21:02  posk
// Allow multiple instances of the AnalysisMaker.
//
// Revision 1.21  2000/03/21 00:22:01  posk
// Added GetCVS and some print commands.
//
// Revision 1.20  2000/03/15 23:28:52  posk
// Added StFlowSelection.
//
// Revision 1.19  2000/03/07 17:50:57  snelling
// Added Nano DST
//
// Revision 1.18  2000/03/02 23:02:53  posk
// Changed extensions from .hh and .cc to .h and .cxx .
//
// Revision 1.17  2000/02/29 22:00:54  posk
// Made SetPhiWeight inline, changed ImpactPar to Dca, etc.
//
// Revision 1.16  2000/02/29 01:26:11  snelling
// removed static const int& nxxx = Flow::nxxx;
//
// Revision 1.14  2000/02/18 22:49:56  posk
// Added PID and centrality.
//
// Revision 1.13  2000/02/11 20:53:10  posk
// Commented out random_shuffle and cout formatting so as to work under CC5.
//
// Revision 1.9  1999/12/21 01:11:00  posk
// Added more quantities to StFlowEvent.
//
// Revision 1.8  1999/12/16 18:05:23  posk
// Fixed Linux compatability again.
//
// Revision 1.7  1999/12/15 22:01:27  posk
// Added StFlowConstants.hh
//
// Revision 1.6  1999/12/07 23:30:53  snelling
// Fixed Linux warnings
//
// Revision 1.5  1999/12/04 00:10:34  posk
// Works with the new StEvent
//
// Revision 1.4  1999/11/30 18:52:52  snelling
// First modification for the new StEvent
//
// Revision 1.3  1999/11/24 18:17:15  posk
// Put the methods which act on the data in with the data in StFlowEvent.
//
// Revision 1.2  1999/11/11 23:08:57  posk
// Rearrangement of files.
//
// Revision 1.1  1999/11/04 19:02:12  snelling
// First check in of StFlowMaker. It contains the common code from
// StFlowTagMaker and StFlowAnalysisMaker.
//
//////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <math.h>
#include "StFlowMaker.h"
#include "StFlowEvent.h"
#include "StFlowNanoEvent.h"
#include "StEvent.h"
#include "StEventTypes.h"
#include "StFlowCutEvent.h"
#include "StFlowCutTrack.h"
#include "StFlowSelection.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StThreeVector.hh"
#include "TFile.h"
#include "TTree.h"
#include "TBranch.h"
#include "StFlowConstants.h"
#include "StPionPlus.hh"
#include "StPionMinus.hh"
#include "StProton.hh"
#include "StTpcDedxPidAlgorithm.h"
#define PR(x) cout << "##### FlowMaker: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowMaker)

//-----------------------------------------------------------------------

StFlowMaker::StFlowMaker(const Char_t* name): 
  StMaker(name), mNanoFlowEventOn(kFALSE), 
  mFlowEventWrite(kFALSE), mFlowEventRead(kFALSE), pEvent(NULL) {
  pFlowSelect = new StFlowSelection();
}

StFlowMaker::StFlowMaker(const Char_t* name,
					 const StFlowSelection& flowSelect) :
  StMaker(name), mNanoFlowEventOn(kFALSE), 
  mFlowEventWrite(kFALSE), mFlowEventRead(kFALSE), pEvent(NULL) {
  pFlowSelect = new StFlowSelection(flowSelect); //copy constructor
}

//-----------------------------------------------------------------------

StFlowMaker::~StFlowMaker() {
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::Make() {

  // Get a pointer to StEvent
  if (!mFlowEventRead) {
    pEvent = (StEvent*)GetDataSet("StEvent");
    if (!pEvent) return kStOK; // If no event, we're done
  }

  // Delete previous FlowEvent
  if (pFlowEvent) delete pFlowEvent;    // it deletes pTrackCollection
  pFlowEvent = NULL;

  if (!mFlowEventRead) {
    // Check the event cuts and fill StFlowEvent
    if (StFlowCutEvent::CheckEvent(pEvent)) {
      // Instantiate a new StFlowEvent
      pFlowEvent = new StFlowEvent;
      if (!pFlowEvent) return kStOK;
      FillFlowEvent();
      if (mFlowEventWrite) pFlowMicroTree->Fill();  //fill the tree
      if (mNanoFlowEventOn) FillFlowNanoEvent();
    } else {
      Long_t eventID = pEvent->id();
      cout << "##### FlowMaker: event " << eventID << " cut" << endl;
    }
  } else {
    // Get a pointer to StFlowEvent
    //StFlowEvent* pFlowEvent = pFlowDST->ReadBuffer( , );
    if (!pFlowEvent) return kStOK; // If no event, we're done
    if (mNanoFlowEventOn) FillFlowNanoEvent();
  }

  //PrintInfo();
  UInt_t flowEventMult;
  if (!pFlowEvent) { flowEventMult = 0;}
  else { flowEventMult = pFlowEvent->TotalMult(); }
  PR(flowEventMult);

  return kStOK;
}

//-----------------------------------------------------------------------

void StFlowMaker::PrintInfo() {
  cout << "*************************************************************" << endl;
  cout << "$Id: StFlowMaker.cxx,v 1.24 2000/05/12 22:42:04 snelling Exp $" << endl;
  cout << "*************************************************************" << endl;
  if (Debug()) StMaker::PrintInfo();

}

//-----------------------------------------------------------------------

Int_t StFlowMaker::Init() {
  // Open PhiWgt file
  ReadPhiWgtFile();
  if (mNanoFlowEventOn) InitFlowNanoEvent();
  if (mFlowEventWrite) InitFlowEventWrite();
  if (mFlowEventRead)  InitFlowEventRead();

  return StMaker::Init();

}

//-----------------------------------------------------------------------

Int_t StFlowMaker::Finish() {
  // Print the cut lists
  cout << "#######################################################" << endl;
  cout << "##### FlowMaker: Cut Lists" << endl;
  StFlowCutEvent::PrintCutList();
  StFlowCutTrack::PrintCutList();
  pFlowEvent->PrintSelectionList();
  if (mNanoFlowEventOn) CloseFlowNanoEvent();
  if (mFlowEventWrite) CloseFlowEventWrite();
  if (mFlowEventRead)  CloseFlowEventRead();

  return StMaker::Finish();
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::ReadPhiWgtFile() {
  // Read the PhiWgt root file

  TDirectory* dirSave = gDirectory;
  TString* fileName = new TString("flowPhiWgt.hist.root");
  fileName->Prepend(pFlowSelect->Number());
  TFile* pPhiWgtFile = new TFile(fileName->Data(), "READ");
  if (!pPhiWgtFile->IsOpen()) {
    cout << "##### FlowMaker: No PhiWgt file. Will set weights = 1." << endl;
  }
  delete fileName;
  gDirectory = dirSave;

  // Fill mPhiWgt
  // for each selection and each harmonic
  for (int k = 0; k < Flow::nSels; k++) {
    char countSels[2];
    sprintf(countSels,"%d",k+1);
    for (int j = 0; j < Flow::nHars; j++) {
      char countHars[2];
      sprintf(countHars,"%d",j+1);
      TString* histTitle = new TString("Flow_Phi_Weight_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      if (pPhiWgtFile->IsOpen()) {
	TH1* phiWgtHist = (TH1*)pPhiWgtFile->Get(histTitle->Data());
	for (int n = 0; n < Flow::nPhiBins; n++) {
	  mPhiWgt[k][j][n] = (phiWgtHist) ? phiWgtHist->GetBinContent(n+1) : 1.;
	}
      } else {
	for (int n = 0; n < Flow::nPhiBins; n++) mPhiWgt[k][j][n] = 1.;
      }
      delete histTitle;
    }
  }

  // Close PhiWgt file
  if (pPhiWgtFile->IsOpen()) pPhiWgtFile->Close();

  return kStOK;
}

//-----------------------------------------------------------------------

void StFlowMaker::FillFlowEvent() {
  // Make StFlowEvent from StEvent

  // Fill PhiWgt array
  pFlowEvent->SetPhiWeight(mPhiWgt);

  // Get event id 
  Long_t eventID = pEvent->id(); 
  pFlowEvent->SetEventNumber(eventID);
  PR(eventID);

  // Get initial multiplicity before TrackCuts 
  UInt_t origMult = pEvent->primaryVertex(0)->numberOfDaughters(); 
  pFlowEvent->SetOrigMult(origMult);
  pFlowEvent->SetCentrality(origMult);
  PR(origMult);

  // Get primary vertex position
  const StThreeVectorF& vertex = pEvent->primaryVertex(0)->position();
  pFlowEvent->SetVertexPos(vertex);

  // loop over tracks in StEvent
  int goodTracks = 0;
  const StSPtrVecPrimaryTrack& tracks = pEvent->primaryVertex(0)->daughters();
  StSPtrVecPrimaryTrackIterator itr;
  StTpcDedxPidAlgorithm tpcDedxAlgo;
  Float_t nSigma;

  for (itr = tracks.begin(); itr != tracks.end(); itr++) {
    StPrimaryTrack* pTrack = *itr;
    if (pTrack && pTrack->flag() > 0 && StFlowCutTrack::CheckTrack(pTrack)) {
      // Instantiate new StFlowTrack
      StFlowTrack* pFlowTrack = new StFlowTrack;
      if (!pFlowTrack) return;
      StThreeVectorD p = pTrack->geometry()->momentum();
      pFlowTrack->SetPhi(p.phi());
      pFlowTrack->SetEta(p.pseudoRapidity());
      pFlowTrack->SetPt(p.perp());
      pFlowTrack->SetCharge(pTrack->geometry()->charge());
      pFlowTrack->SetDca(pTrack->impactParameter());
      pFlowTrack->SetChi2(pTrack->fitTraits().chi2());
      pFlowTrack->SetFitPts(pTrack->fitTraits().numberOfFitPoints());
      pFlowTrack->SetMaxPts(pTrack->numberOfPossiblePoints());
      pTrack->pidTraits(tpcDedxAlgo);       // initialize
      nSigma = (float)tpcDedxAlgo.numberOfSigma(StPionPlus::instance());
      pFlowTrack->SetPidPiPlus(nSigma);
      nSigma = (float)tpcDedxAlgo.numberOfSigma(StPionMinus::instance());
      pFlowTrack->SetPidPiMinus(nSigma);
      nSigma = (float)tpcDedxAlgo.numberOfSigma(StProton::instance());
      pFlowTrack->SetPidProton(nSigma);
      pFlowEvent->TrackCollection()->push_back(pFlowTrack);
      goodTracks++;
    }
  }
  
  // Check Eta Symmetry
  if (!StFlowCutEvent::CheckEtaSymmetry()) {  // if kFALSE delete this event
    delete pFlowEvent;
    pFlowEvent = NULL;
    return;
  }

  pFlowEvent->SetSelections();
  pFlowEvent->MakeSubEvents();
  pFlowEvent->SetPids();

}

//-----------------------------------------------------------------------

void StFlowMaker::InitFlowNanoEvent() {

  Int_t split  = 1;       // by default, split Event in sub branches

  pFlowNanoEvent = new StFlowNanoEvent();   
  // Create a new ROOT binary machine independent file.
  // Note that this file may contain any kind of ROOT objects, histograms,
  // pictures, graphics objects, detector geometries, tracks, events, etc..
  // This file is now becoming the current directory.
  pFlowNanoDST = new TFile("FlowNanoDST.root", "RECREATE", "Flow Nano DST file");
  Int_t comp   = 1;       // by default file is compressed
  if (pFlowNanoDST) pFlowNanoDST->SetCompressionLevel(comp);
  // Create a ROOT Tree and one superbranch
  pFlowNanoTree = new TTree("FlowNanoTree","Flow Nano Tree");
  if (pFlowNanoTree) {
    pFlowNanoTree->SetAutoSave(100000000);  // autosave when 100 Mbyte written
  }
  Int_t bufsize = 256000;
  if (split)  bufsize /= 4;
  if (pFlowNanoTree) {
    pFlowNanoTree->Branch("pFlowNanoEvent", "StFlowNanoEvent", &pFlowNanoEvent, bufsize,split);
  }

}

//-----------------------------------------------------------------------

void StFlowMaker::InitFlowEventWrite() {

  Int_t split  = 1;       // by default, split Event in sub branches

  // Create a new ROOT binary machine independent file.
  // Note that this file may contain any kind of ROOT objects, histograms,
  // pictures, graphics objects, detector geometries, tracks, events, etc..
  // This file is now becoming the current directory.
  pFlowDST = new TFile("FlowDST.root", "RECREATE", "Flow micro DST file");
  Int_t comp   = 1;       // by default file is compressed
  if (pFlowDST) pFlowDST->SetCompressionLevel(comp);
  // Create a ROOT Tree and one superbranch
  pFlowMicroTree = new TTree("FlowMicroTree","Flow Micro Tree");
  if (pFlowMicroTree) {
    pFlowMicroTree->SetAutoSave(100000000);  // autosave when 100 Mbyte written
  }
  Int_t bufsize = 256000;
  if (split)  bufsize /= 4;
  if (pFlowMicroTree) {
    pFlowMicroTree->Branch("pFlowEvent", "StFlowEvent", &pFlowEvent, bufsize, split);
  }
  PR("InitFlowEventWrite()");
}

//-----------------------------------------------------------------------

void StFlowMaker::InitFlowEventRead() {
  pFlowDST = new TFile("flowEvent.root", "READ");
  PR("InitFlowEventRead()");
}

//-----------------------------------------------------------------------

void StFlowMaker::FillFlowNanoEvent() {


  StFlowTrackIterator itr;
  StFlowTrackCollection* pFlowTracks = pFlowEvent->TrackCollection();

  // set event id, run number and date
  if (pFlowNanoEvent) {
    Long_t eventID = pEvent->id();
    pFlowNanoEvent->SetHeader(eventID, 200, 960312);
  }
  else {
    cout << "##### FlowMaker: Warning: No FlowNanoEvent" << endl;
  }

  for (itr = pFlowTracks->begin(); itr != pFlowTracks->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    Float_t Pt  = pFlowTrack->Pt(); 
    Float_t Phi = pFlowTrack->Phi();
    Float_t Eta = pFlowTrack->Eta();
    if (pFlowNanoEvent) {pFlowNanoEvent->AddTrack(Pt,Phi,Eta);}
    else {cout << "##### FlowMaker: Warning: No FlowNanoEvent" << endl;}
  }


  pFlowNanoTree->Fill();  //fill the tree
  pFlowNanoEvent->Clear();

}

//-----------------------------------------------------------------------

void StFlowMaker::CloseFlowNanoEvent() {

  if (pFlowNanoDST) {
    pFlowNanoDST->Write();
    pFlowNanoDST->Close();
  }

}

//-----------------------------------------------------------------------

void StFlowMaker::CloseFlowEventWrite() {

  if (pFlowDST->IsOpen()) {
    pFlowDST->Write();
    pFlowDST->Close();
  }
  PR("CloseFlowEventWrite()");
}

//-----------------------------------------------------------------------

void StFlowMaker::CloseFlowEventRead() {

  //if (pFlowDST->IsOpen()) pFlowDST->Close();
  PR("CloseFlowEventRead()");
}

//-----------------------------------------------------------------------
