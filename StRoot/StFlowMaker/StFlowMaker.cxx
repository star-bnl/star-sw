//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowMaker.cxx,v 1.27 2000/05/23 20:09:44 voloshin Exp $
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
// Revision 1.27  2000/05/23 20:09:44  voloshin
// added StFlowPicoEvent, persistent FlowEvent as plain root TTree
//
// Revision 1.26  2000/05/20 00:55:15  posk
// Condensed flownanoevent.root somewhat.
//
// Revision 1.25  2000/05/16 20:59:31  posk
// Voloshin's flownanoevent.root added.
//
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
#include "StFlowPicoEvent.h"
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
  StMaker(name), mNanoEventWrite(kFALSE), mNanoEventRead(kFALSE), 
  mPicoEventWrite(kFALSE), mPicoEventRead(kFALSE),
  mFlowEventWrite(kFALSE), mFlowEventRead(kFALSE), pEvent(NULL) {
  pFlowSelect = new StFlowSelection();
}

StFlowMaker::StFlowMaker(const Char_t* name,
					 const StFlowSelection& flowSelect) :
  StMaker(name), 
  mNanoEventWrite(kFALSE), mNanoEventRead(kFALSE), 
  mPicoEventWrite(kFALSE), mPicoEventRead(kFALSE), 
  mFlowEventWrite(kFALSE), mFlowEventRead(kFALSE), pEvent(NULL) {
  pFlowSelect = new StFlowSelection(flowSelect); //copy constructor
}

//-----------------------------------------------------------------------

StFlowMaker::~StFlowMaker() {
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::Make() {

  // Delete previous StFlowEvent
  if (pFlowEvent) delete pFlowEvent;    // it deletes pTrackCollection
  pFlowEvent = NULL;
    
  // Get a pointer to StEvent
  if (!mFlowEventRead && !mNanoEventRead && !mPicoEventRead) {
    pEvent = (StEvent*)GetDataSet("StEvent");
    if (!pEvent) return kStOK; // If no event, we're done
    
    // Check the event cuts and fill StFlowEvent
    if (StFlowCutEvent::CheckEvent(pEvent)) {
      // Instantiate a new StFlowEvent
      pFlowEvent = new StFlowEvent;
      if (!pFlowEvent) return kStOK;
      FillFlowEvent();
      if (!pFlowEvent) return kStOK;  // could have been deleted
      if (mNanoEventWrite) FillNanoEvent();
      if (mPicoEventWrite) FillPicoEvent();
      if (mFlowEventWrite) pFlowMicroTree->Fill();  // fill the tree
    } else {
      Long_t eventID = pEvent->id();
      cout << "##### FlowMaker: event " << eventID << " cut" << endl;
    }
  } else if (mFlowEventRead) {
    // Get a pointer to StFlowEvent
    if (!pFlowEvent) return kStOK; // If no event, we're done
    if (mNanoEventWrite) FillNanoEvent();
    if (mPicoEventWrite) FillPicoEvent();
  } else if (mNanoEventRead) {
    // Instantiate a new StFlowEvent
    pFlowEvent = new StFlowEvent;
    if (!pFlowEvent) return kStOK;
    if (!FillFromNanoDST(pFlowNanoEvent)) return kStEOF; // false if EOF
    if (mFlowEventWrite) pFlowMicroTree->Fill();  // fill the tree
  } else if (mPicoEventRead) {
    // Instantiate a new StFlowEvent
    pFlowEvent = new StFlowEvent;
    if (!pFlowEvent) return kStOK;
    if (!FillFromPicoDST(pFlowPicoEvent)) return kStEOF; // false if EOF
    if (mFlowEventWrite) pFlowMicroTree->Fill();  // fill the tree
  }

  //PrintInfo();
  UInt_t flowEventMult;
  if (!pFlowEvent) { flowEventMult = 0;}
  else { flowEventMult = pFlowEvent->FlowEventMult(); }
  PR(flowEventMult);

  return kStOK;
}

//-----------------------------------------------------------------------

void StFlowMaker::PrintInfo() {
  cout << "*************************************************************" << endl;
  cout << "$Id: StFlowMaker.cxx,v 1.27 2000/05/23 20:09:44 voloshin Exp $" << endl;
  cout << "*************************************************************" << endl;
  if (Debug()) StMaker::PrintInfo();

}

//-----------------------------------------------------------------------

Int_t StFlowMaker::Init() {
  // Open PhiWgt file
  ReadPhiWgtFile();

  if (mNanoEventWrite) InitNanoEventWrite();
  if (mNanoEventRead)  InitNanoEventRead();
  if (mPicoEventWrite) InitPicoEventWrite();
  if (mPicoEventRead)  InitPicoEventRead();
  if (mFlowEventWrite) InitFlowEventWrite();
  if (mFlowEventRead)  InitFlowEventRead();

  return StMaker::Init();
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::Finish() {
  // Print the cut lists
  cout << "#######################################################" << endl;
  cout << "##### FlowMaker: Cut Lists" << endl;
  if (!mFlowEventRead && !mNanoEventRead && !mPicoEventRead) {
    StFlowCutEvent::PrintCutList();
    StFlowCutTrack::PrintCutList();
  }
  pFlowEvent->PrintSelectionList();

  if (mNanoEventWrite) CloseNanoEventWrite();
  if (mNanoEventRead)  CloseNanoEventRead();
  if (mPicoEventWrite) ClosePicoEventWrite();
  if (mPicoEventRead)  ClosePicoEventRead();
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
  pFlowEvent->SetEventID((Int_t)(pEvent->id()));

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
      pFlowTrack->SetChi2((Float_t)(pTrack->fitTraits().chi2()));
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

  pFlowEvent->TrackCollection()->random_shuffle();
  pFlowEvent->SetSelections();
  pFlowEvent->MakeSubEvents();
  pFlowEvent->SetPids();

  // Print multiplicities
//   int j, k, n;

//   pFlowSelect->SetSubevent(-1);
//   for (j = 0; j < Flow::nHars; j++) {
//     pFlowSelect->SetHarmonic(j);
//     for (k = 0; k < Flow::nSels; k++) {
//       pFlowSelect->SetSelection(k);
//       cout << "j,k= " << j << k << " : " << pFlowEvent->Mult(pFlowSelect) << endl;
//     }
//   }

//   for (j = 0; j < Flow::nHars; j++) {
//     pFlowSelect->SetHarmonic(j);
//     for (k = 0; k <Flow:: nSels; k++) {
//       pFlowSelect->SetSelection(k);
//       for (n = 0; n < Flow::nSubs+1; n++) {
// 	pFlowSelect->SetSubevent(n);
// 	cout << "j,k,n= " << j << k << n << " : " << 
// 	  pFlowEvent->Mult(pFlowSelect) << endl;
//       }
//     }
//   }

}

//-----------------------------------------------------------------------

void StFlowMaker::FillNanoEvent() {

  if (!pFlowNanoEvent) {
    cout << "##### FlowMaker: Warning: No FlowNanoEvent" << endl;
  }

  pFlowNanoEvent->SetEventID(pFlowEvent->EventID());
  pFlowNanoEvent->SetOrigMult(pFlowEvent->OrigMult());
  pFlowNanoEvent->SetCentrality(pFlowEvent->Centrality());
  pFlowNanoEvent->SetVertexPos(pFlowEvent->VertexPos());
  
  StFlowTrackIterator itr;
  StFlowTrackCollection* pFlowTracks = pFlowEvent->TrackCollection();
  
  for (itr = pFlowTracks->begin(); itr != pFlowTracks->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    pFlowNanoEvent->AddTrack(pFlowTrack);
  }
  pFlowTree->Fill();  //fill the tree
  pFlowNanoEvent->Clear();  
  
}

//----------------------------------------------------------------------

void StFlowMaker::FillPicoEvent() {

  if (!pFlowPicoEvent) {
    cout << "##### FlowMaker: Warning: No FlowPicoEvent" << endl;
  }

  if (pFlowPicoEvent) {
    pFlowPicoEvent->SetEventID(pFlowEvent->EventID());
    pFlowPicoEvent->SetOrigMult(pFlowEvent->OrigMult());
    pFlowPicoEvent->SetCentrality(pFlowEvent->Centrality());
    pFlowPicoEvent->SetVertexPos(pFlowEvent->VertexPos().x(),
                                 pFlowEvent->VertexPos().y(),
                                 pFlowEvent->VertexPos().z());

    StFlowTrackIterator itr;
    StFlowTrackCollection* pFlowTracks = pFlowEvent->TrackCollection();

    TClonesArray &tracks = *pFlowPicoEvent->Tracks();
    Int_t nnt=0;
    for (itr = pFlowTracks->begin(); itr != pFlowTracks->end(); itr++) {
      StFlowTrack* pFlowTrack = *itr;
      new(tracks[nnt++])
               StFlowPicoTrack(pFlowTrack->Pt(), 
                               pFlowTrack->Eta(),
                               pFlowTrack->Phi(),
                               pFlowTrack->Charge(),
                               pFlowTrack->Dca(),
                               pFlowTrack->Chi2(),
                               pFlowTrack->FitPts(),
                               pFlowTrack->MaxPts(),
                               pFlowTrack->PidPiPlus(),
                               pFlowTrack->PidPiMinus(),
                               pFlowTrack->PidProton()    );
    }  
    pFlowPicoEvent->SetNtrack(nnt);
    
    pFlowTree->Fill();  //fill the tree
    pFlowPicoEvent->Clear();
  } else {
    cout << "##### FlowMaker: Warning: No FlowPicoEvent" << endl;
  }
  
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::FillFromNanoDST(const StFlowNanoEvent* pFlowNanoEvent) {
  // Make StFlowEvent from StFlowNanoEvent

  if (!pFlowTree->GetEntry(mNanoEventCounter++)) {
    cout << "##### FlowMaker: no more events" << endl; 
    return kFALSE; 
  }

  // Fill FlowEvent
  pFlowEvent->SetPhiWeight(mPhiWgt);
  pFlowEvent->SetEventID(pFlowNanoEvent->EventID());
  UInt_t origMult = pFlowNanoEvent->OrigMult();
  pFlowEvent->SetOrigMult(origMult);
  pFlowEvent->SetCentrality(origMult);
  pFlowEvent->SetVertexPos(pFlowNanoEvent->VertexPos());

  // Fill FlowTracks
  Int_t nTracks = (Int_t)pFlowNanoEvent->GetNtrack();
  TClonesArray &tracks = *pFlowNanoEvent->GetTracks();
  for (Int_t nt=0; nt<nTracks; nt++) {
      pFlowEvent->TrackCollection()->push_back((StFlowTrack*)tracks[nt]);
  }

  pFlowEvent->SetSelections();
  pFlowEvent->MakeSubEvents();
  pFlowEvent->SetPids();

  // Print multiplicities
//   int j, k, n;

//   pFlowSelect->SetSubevent(-1);
//   for (j = 0; j < Flow::nHars; j++) {
//     pFlowSelect->SetHarmonic(j);
//     for (k = 0; k < Flow::nSels; k++) {
//       pFlowSelect->SetSelection(k);
//       cout << "j,k= " << j << k << " : " << pFlowEvent->Mult(pFlowSelect) << endl;
//     }
//   }

//   for (j = 0; j < Flow::nHars; j++) {
//     pFlowSelect->SetHarmonic(j);
//     for (k = 0; k <Flow:: nSels; k++) {
//       pFlowSelect->SetSelection(k);
//       for (n = 0; n < Flow::nSubs+1; n++) {
// 	pFlowSelect->SetSubevent(n);
// 	cout << "j,k,n= " << j << k << n << " : " << 
// 	  pFlowEvent->Mult(pFlowSelect) << endl;
//       }
//     }
//   }

  return kTRUE;
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::FillFromPicoDST(const StFlowPicoEvent* pFlowPicoEvent) {
  // Make StFlowEvent from StFlowPicoEvent

  if (!pFlowTree->GetEntry(mPicoEventCounter++)) {
    cout << "##### FlowMaker: no more events" << endl; 
    return kFALSE; 
  }

  // Fill FlowEvent
  pFlowEvent->SetPhiWeight(mPhiWgt);
  pFlowEvent->SetEventID(pFlowPicoEvent->EventID());
  UInt_t origMult = pFlowPicoEvent->OrigMult();
  pFlowEvent->SetOrigMult(origMult);
  pFlowEvent->SetCentrality(origMult);
  pFlowEvent->SetVertexPos(StThreeVectorF( pFlowPicoEvent->VertexX(),
pFlowPicoEvent->VertexY(), pFlowPicoEvent->VertexZ()) );

  // Fill FlowTracks
  for (Int_t nt=0; nt<pFlowPicoEvent->GetNtrack(); nt++) {
    StFlowPicoTrack *ntrack = (StFlowPicoTrack*)pFlowPicoEvent->Tracks()
      ->UncheckedAt(nt);
      // Instantiate new StFlowTrack
      StFlowTrack* pFlowTrack = new StFlowTrack;
      if (!pFlowTrack) return kFALSE;
      pFlowTrack->SetPt(ntrack->Pt());
      pFlowTrack->SetPhi(ntrack->Phi());
      pFlowTrack->SetEta(ntrack->Eta());
      pFlowTrack->SetCharge(ntrack->Charge());
      pFlowTrack->SetDca(ntrack->Dca());
      pFlowTrack->SetChi2(ntrack->Chi2());
      pFlowTrack->SetFitPts(ntrack->FitPts());
      pFlowTrack->SetMaxPts(ntrack->MaxPts());
      pFlowTrack->SetPidPiPlus(ntrack->PidPiPlus());
      pFlowTrack->SetPidPiMinus(ntrack->PidPiMinus());
      pFlowTrack->SetPidProton(ntrack->PidProton());
      pFlowEvent->TrackCollection()->push_back(pFlowTrack);
  }

  return kTRUE;
}

//-----------------------------------------------------------------------

void StFlowMaker::InitNanoEventWrite() {

  Int_t split  = 1;       // by default split Event into sub branches
  Int_t comp   = 1;       // by default file is compressed
  Int_t bufsize = 256000;
  if (split) bufsize /= 4;

  pFlowNanoEvent = new StFlowNanoEvent();   

  // Open an output file
  Char_t* file = mNanoEventFileName;  
  pFlowNanoDST = new TFile(mNanoEventFileName,"RECREATE","Flow Nano DST file");
  if (pFlowNanoDST) {
    pFlowNanoDST->SetCompressionLevel(comp);
    cout << "##### FlowMaker: NanoEvents file = " << file << endl;
  } else {
    cout << "##### FlowMaker: Warning: no NanoEvents file = " << file << endl;
    return;
  }

  // Create a ROOT Tree and one superbranch
  pFlowTree = new TTree("FlowTree", "Flow Nano Tree");
  if (pFlowTree) {
    pFlowTree->SetAutoSave(100000000);  // autosave when 100 Mbyte written
    pFlowTree->Branch("pFlowNanoEvent", "StFlowNanoEvent", &pFlowNanoEvent,
		      bufsize, split);
  } else {
    cout << "##### FlowMaker: Warning: No FlowNanoTree" << endl;
  }
  
}

//-----------------------------------------------------------------------

void StFlowMaker::InitPicoEventWrite() {

  Int_t split  = 1;       // by default split Event into sub branches
  Int_t comp   = 1;       // by default file is compressed
  Int_t bufsize = 256000;
  if (split) bufsize /= 4;

  // creat a Picoevent and an output file
  pFlowPicoEvent = new StFlowPicoEvent();   
  pFlowPicoDST = new TFile("flowPicoevent.root","RECREATE","Flow Pico DST file");
  if (pFlowPicoDST) pFlowPicoDST->SetCompressionLevel(comp);

  // Create a ROOT Tree and one superbranch
  pFlowTree = new TTree("FlowTree", "Flow Pico Tree");
  if (pFlowTree) {
    pFlowTree->SetAutoSave(100000000);  // autosave when 100 Mbyte written
    pFlowTree->Branch("pFlowPicoEvent", "StFlowPicoEvent", &pFlowPicoEvent,
		      bufsize, split);
  } else {
    cout << "##### FlowMaker: Warning: No FlowPicoTree" << endl;
  }
  
}

//-----------------------------------------------------------------------

void StFlowMaker::InitNanoEventRead() {
  
  pFlowNanoEvent = new StFlowNanoEvent(); 
  
  // Open the input file
  Char_t* file = mNanoEventFileName;  
  pFlowNanoDST = new TFile(file);
  if (pFlowNanoDST) {
    cout << "##### FlowMaker: NanoEvents file = " << file << endl;
  } else {
    cout << "##### FlowMaker: Warning: no NanoEvents file = " << file << endl;
    return;
  }

  // Get the tree, the branch, and the entries
  pFlowTree = (TTree*)pFlowNanoDST->Get("FlowTree");
  if (pFlowTree) {
    TBranch* branch = pFlowTree->GetBranch("pFlowNanoEvent");
    branch->SetAddress(&pFlowNanoEvent);
    Int_t nEntries = (Int_t)pFlowTree->GetEntries(); 
    cout << "##### FlowMaker: events in nano-DST file = " << nEntries << endl;
  } else {
    cout << "##### FlowMaker: Warning: No FlowNanoTree" << endl;
  }

  mNanoEventCounter = 0;

  }

//-----------------------------------------------------------------------

void StFlowMaker::InitPicoEventRead() {
  
  pFlowPicoEvent = new StFlowPicoEvent(); 
  
  // Open the file
  Char_t* file = mPicoEventFileName;  
  //Char_t* file = "flowPicoevent.root";  
  cout << "##### FlowMaker: " << "PicoEvents file = " << file << endl;
  pFlowPicoDST = new TFile(file);

  // Get the tree, the branch, and the entries
  pFlowTree = (TTree*)pFlowPicoDST->Get("FlowTree");
  TBranch* branch = pFlowTree->GetBranch("pFlowPicoEvent");
  branch->SetAddress(&pFlowPicoEvent);
  Int_t nEntries = (Int_t)pFlowTree->GetEntries(); 
  cout << "##### FlowMaker: " << "events in Pico-DST file = " << nEntries << endl;

  mPicoEventCounter = 0;

  }

//-----------------------------------------------------------------------

void StFlowMaker::InitFlowEventWrite() {

  Int_t split  = 1;       // by default, split Event in sub branches
  Int_t comp   = 1;       // by default file is compressed
  Int_t bufsize = 256000;
  if (split)  bufsize /= 4;

  // Create a new ROOT binary machine independent file.
  // Note that this file may contain any kind of ROOT objects, histograms,
  // pictures, graphics objects, detector geometries, tracks, events, etc..
  // This file is now becoming the current directory.
  pFlowDST = new TFile("flowevent.root", "RECREATE", "Flow micro DST file");
  if (pFlowDST) {
    pFlowDST->SetCompressionLevel(comp);
  } else {
    cout << "##### FlowMaker: Warning: no FlowEvents file" << endl;
  }
  // Create a ROOT Tree and one superbranch
  pFlowMicroTree = new TTree("FlowMicroTree","Flow Micro Tree");
  if (pFlowMicroTree) {
    pFlowMicroTree->SetAutoSave(100000000);  // autosave when 100 Mbyte written
    pFlowMicroTree->Branch("pFlowEvent", "StFlowEvent", &pFlowEvent, bufsize, split);
  } else {
    cout << "##### FlowMaker: Warning: No FlowMicroTree" << endl;
  }

}

//-----------------------------------------------------------------------

void StFlowMaker::InitFlowEventRead() {
  pFlowDST = new TFile("flowevent.root", "READ");

}

//-----------------------------------------------------------------------

void StFlowMaker::CloseNanoEventWrite() {

  if (pFlowNanoDST->IsOpen()) {
    pFlowNanoDST->Write();
    pFlowNanoDST->Close();
  }

}

//-----------------------------------------------------------------------

void StFlowMaker::ClosePicoEventWrite() {

  if (pFlowPicoDST->IsOpen()) {
    pFlowPicoDST->Write();
    pFlowPicoDST->Close();
  }

}

//-----------------------------------------------------------------------

void StFlowMaker::CloseNanoEventRead() {

  if (pFlowNanoDST->IsOpen()) {
    pFlowNanoDST->Close();
  }

}

//-----------------------------------------------------------------------

void StFlowMaker::ClosePicoEventRead() {

  if (pFlowPicoDST->IsOpen()) {
    pFlowPicoDST->Close();
  }

}

//-----------------------------------------------------------------------

void StFlowMaker::CloseFlowEventWrite() {

  if (pFlowDST->IsOpen()) {
    pFlowDST->Write();
    pFlowDST->Close();
  }

}

//-----------------------------------------------------------------------

void StFlowMaker::CloseFlowEventRead() {

  //if (pFlowDST->IsOpen()) pFlowDST->Close();

}

//-----------------------------------------------------------------------
