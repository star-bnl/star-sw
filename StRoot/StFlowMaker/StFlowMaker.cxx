//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowMaker.cxx,v 1.45 2000/11/07 02:36:41 snelling Exp $
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
// Revision 1.45  2000/11/07 02:36:41  snelling
// Do not init prob pid when not used
//
// Revision 1.44  2000/11/02 18:19:09  fine
// protection against of the crash with traits.size() == 0 and pid == 0. May be one has to change the logic of this code
//
// Revision 1.43  2000/10/12 22:46:37  snelling
// Added support for the new pDST's and the probability pid method
//
// Revision 1.42  2000/09/15 22:51:30  posk
// Added pt weighting for event plane calcualtion.
//
// Revision 1.41  2000/09/11 17:24:07  snelling
// Put picoreader for different versions in seperate methods
//
// Revision 1.40  2000/09/05 16:11:33  snelling
// Added global DCA, electron and positron
//
// Revision 1.39  2000/08/31 18:58:23  posk
// For picoDST, added version number, runID, and multEta for centrality.
// Added centrality cut when reading picoDST.
// Added pt and eta selections for particles corr. wrt event plane.
//
// Revision 1.38  2000/08/26 21:37:02  snelling
// Removed flownanoevent, Added multiple input for pico, fixed IO bug
//
// Revision 1.37  2000/08/25 19:55:16  snelling
// Changed naming pico files (1 pico per dst)
//
// Revision 1.36  2000/08/12 20:22:20  posk
// Recalculate centrality in read from pico.
//
// Revision 1.35  2000/08/10 23:00:22  posk
// New centralities. pt and eta cuts.
//
// Revision 1.34  2000/08/09 21:38:23  snelling
// PID added
//
// Revision 1.33  2000/07/20 17:25:51  posk
// Fixed bug in readPico checkEvent.
//
// Revision 1.32  2000/07/14 23:49:03  snelling
// Changed to ConstIterator for new StEvent and removed comparison int uint
//
// Revision 1.31  2000/07/12 17:54:37  posk
// Added chi2 and dca cuts. Multiplied EtaSym by sqrt(mult).
// Apply cuts when reading picoevent file.
//
// Revision 1.30  2000/06/30 14:48:33  posk
// Using MessageMgr, changed Eta Symmetry cut.
//
// Revision 1.29  2000/06/01 18:26:36  posk
// Increased precision of Track integer data members.
//
// Revision 1.28  2000/05/26 21:29:28  posk
// Protected Track data members from overflow.
//
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
#include "StFlowPicoEvent.h"
#include "StFlowCutEvent.h"
#include "StFlowCutTrack.h"
#include "StFlowSelection.h"
#include "StFlowConstants.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StEvent.h"
#include "StEventTypes.h"
#include "StThreeVector.hh"
#include "StIOMaker/StIOMaker.h"
#include "TFile.h"
#include "TTree.h"
#include "TBranch.h"
#include "TChain.h"
#include "StPionPlus.hh"
#include "StPionMinus.hh"
#include "StProton.hh"
#include "StKaonMinus.hh"
#include "StKaonPlus.hh"
#include "StAntiProton.hh"
#include "StDeuteron.hh"
#include "StElectron.hh"
#include "StPositron.hh"
#include "StTpcDedxPidAlgorithm.h"
#include "StuProbabilityPidAlgorithm.h"
#include "StMessMgr.h"
//#include <algorithm>
#define PR(x) cout << "##### FlowMaker: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowMaker)

//-----------------------------------------------------------------------

StFlowMaker::StFlowMaker(const Char_t* name): 
  StMaker(name), 
  mPicoEventWrite(kFALSE), mPicoEventRead(kFALSE),
  mFlowEventWrite(kFALSE), mFlowEventRead(kFALSE), pEvent(NULL) {
  pFlowSelect = new StFlowSelection();
  SetPicoEventDir("./");
}

StFlowMaker::StFlowMaker(const Char_t* name,
			 const StFlowSelection& flowSelect) :
  StMaker(name), 
  mPicoEventWrite(kFALSE), mPicoEventRead(kFALSE), 
  mFlowEventWrite(kFALSE), mFlowEventRead(kFALSE), pEvent(NULL) {
  pFlowSelect = new StFlowSelection(flowSelect); //copy constructor
  SetPicoEventDir("./");
}

//-----------------------------------------------------------------------

StFlowMaker::~StFlowMaker() {
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::Make() {
  if (Debug()) gMessMgr->Info() << "FlowMaker: Make() " << endm;

  // Delete previous StFlowEvent
  if (pFlowEvent) delete pFlowEvent;
  pFlowEvent = NULL;

  // Get the input file name from the ioMaker
  if (!mPicoEventRead && !mFlowEventRead && pIOMaker) {
    mEventFileName = strrchr(pIOMaker->GetFile(),'/')+1;
    if (Debug()) { 
      gMessMgr->Info() << "FlowMaker: filename: " << mEventFileName << endm;
      gMessMgr->Info() << "FlowMaker: Old filename: " 
		       << mEventFileNameOld << endm;  
    }

    if (mEventFileName != mEventFileNameOld) { 
      if (Debug()) gMessMgr->Info() << "FlowMaker: New file opened " << endm;
      if (mPicoEventWrite && pPicoDST->IsOpen()) {
	pPicoDST->Write();
	pPicoDST->Close();
      }
      if (mPicoEventWrite) {
	if (pPicoEvent) delete pPicoEvent;
	if (pPicoDST) delete pPicoDST;
	pPicoEvent = NULL;
	pPicoDST = NULL;
	InitPicoEventWrite();
      }
      mEventFileNameOld = mEventFileName;
    }
  }

  // Get a pointer to StEvent
  if (!mFlowEventRead && !mPicoEventRead) {
    pEvent = (StEvent*)GetDataSet("StEvent");
    if (!pEvent) return kStOK; // If no event, we're done
    
    // Check the event cuts and fill StFlowEvent
    if (StFlowCutEvent::CheckEvent(pEvent)) {
      // Instantiate a new StFlowEvent
      pFlowEvent = new StFlowEvent;
      if (!pFlowEvent) return kStOK;
      FillFlowEvent();
      if (!pFlowEvent) return kStOK;  // could have been deleted
      if (mPicoEventWrite) FillPicoEvent();
      if (mFlowEventWrite) pFlowMicroTree->Fill();  // fill the tree
    } else {
      Long_t eventID = pEvent->id();
      gMessMgr->Info() << "##### FlowMaker: event " << eventID 
		       << " cut" << endm;
    }

  } else if (mFlowEventRead) {
    // Get a pointer to StFlowEvent
    if (!pFlowEvent) return kStOK; // If no event, we're done
    if (mPicoEventWrite) FillPicoEvent();
    
  } else if (mPicoEventRead) {
    // Instantiate a new StFlowEvent
    pFlowEvent = new StFlowEvent;
    if (!pFlowEvent) return kStOK;
    if (!FillFromPicoDST(pPicoEvent)) return kStEOF; // false if EOF
    if (!pFlowEvent) return kStOK; // could have been deleted
  }
  
  UInt_t flowEventMult;
  if (!pFlowEvent) { flowEventMult = 0; }
  else { flowEventMult = pFlowEvent->FlowEventMult(); }

  if (Debug()) StMaker::PrintInfo();
  
  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::Init() {
  if (Debug()) gMessMgr->Info() << "FlowMaker: Init()" << endm;

  // Open PhiWgt file
  ReadPhiWgtFile();

  Int_t kRETURN = kStOK;

  if (!mPicoEventRead && !mFlowEventRead) {
    // get input file name
    pIOMaker = (StIOMaker*)GetMaker("IO");
    if (pIOMaker) {
      mEventFileName = strrchr(pIOMaker->GetFile(),'/')+1;
      mEventFileNameOld = mEventFileName; 
    
      gMessMgr->Info() << "##### FlowMaker: truncated filename " 
		       <<  mEventFileName << endm;
    }
    // Init pid probability algorithm
    if (pFlowEvent->ProbPid()) {
      TString parameterfile = "nhitsBin_0_10_20_45_ptBin_0_Inf_dcaBin_0_2_50000_Amp.root";
      StuProbabilityPidAlgorithm::readParametersFromFile(parameterfile.Data());
    }
  }

  if (mPicoEventWrite) kRETURN += InitPicoEventWrite();
  if (mPicoEventRead)  kRETURN += InitPicoEventRead();
  if (mFlowEventWrite) kRETURN += InitFlowEventWrite();
  if (mFlowEventRead)  kRETURN += InitFlowEventRead();

  gMessMgr->SetLimit("##### FlowMaker", 5);
  gMessMgr->Info("##### FlowMaker: $Id: StFlowMaker.cxx,v 1.45 2000/11/07 02:36:41 snelling Exp $");
  if (kRETURN) gMessMgr->Info() << "##### FlowMaker: Init return = " << kRETURN << endm;

  return kRETURN;
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::InitRun() {
  if (Debug()) gMessMgr->Info() << "FlowMaker: InitRun()" << endm;

  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::Finish() {
  if (Debug()) gMessMgr->Info() << "FlowMaker: Finish()" << endm;

  // Print the cut lists
  cout << "#######################################################" << endl;
  cout << "##### FlowMaker: Cut Lists" << endl;
  if (!mFlowEventRead) {
    StFlowCutEvent::PrintCutList();
    StFlowCutTrack::PrintCutList();
  }
  pFlowEvent->PrintSelectionList();

  if (mPicoEventWrite && pPicoDST->IsOpen()) {
    pPicoDST->Write();
    pPicoDST->Close();
  }

  if (mFlowEventWrite && pFlowDST->IsOpen()) {
    pFlowDST->Write();
    pFlowDST->Close();
  }

  if (mFlowEventRead && pFlowDST->IsOpen()) { // pFlowDST->Close(); 
  }

  return StMaker::Finish();
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::ReadPhiWgtFile() {
  // Read the PhiWgt root file

  if (Debug()) gMessMgr->Info() << "FlowMaker: ReadPhiWgtFile()" << endm;

  TDirectory* dirSave = gDirectory;
  TString* fileName = new TString("flowPhiWgt.hist.root");
  fileName->Prepend(pFlowSelect->Number());
  TFile* pPhiWgtFile = new TFile(fileName->Data(), "READ");
  if (!pPhiWgtFile->IsOpen()) {
    gMessMgr->Info("##### FlowMaker: No PhiWgt file. Will set weights = 1.");
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

  if (Debug()) gMessMgr->Info() << "FlowMaker: FillFlowEvent()" << endm;

  // Fill PhiWgt array
  pFlowEvent->SetPhiWeight(mPhiWgt);

  // Get event id 
  pFlowEvent->SetEventID((Int_t)(pEvent->id()));
  pFlowEvent->SetRunID((Int_t)(pEvent->runId()));

  // Get primary vertex position
  const StThreeVectorF& vertex = pEvent->primaryVertex(0)->position();
  pFlowEvent->SetVertexPos(vertex);

  // include trigger (ZDC and CTB)
  Float_t ctb  = -1.;
  Float_t zdce = -1.;
  Float_t zdcw = -1.;
  StTriggerDetectorCollection *triggers = pEvent->triggerDetectorCollection();
  if (triggers)	{
    StCtbTriggerDetector &CTB = triggers->ctb();
    StZdcTriggerDetector &ZDC = triggers->zdc();
    // get CTB
    for (UInt_t slat=0; slat<CTB.numberOfSlats(); slat++) {
      for (UInt_t tray=0; tray<CTB.numberOfTrays();tray++) {
	ctb += CTB.mips(tray,slat,0);
      }
    }
    //get ZDCe and ZDCw        
    zdce = ZDC.adcSum(east);
    zdcw = ZDC.adcSum(west);
  } 
  pFlowEvent->SetCTB(ctb);
  pFlowEvent->SetZDCe(zdce);
  pFlowEvent->SetZDCw(zdcw);
  
  // Get initial multiplicity before TrackCuts 
  UInt_t origMult = pEvent->primaryVertex(0)->numberOfDaughters(); 
  pFlowEvent->SetOrigMult(origMult);
  PR(origMult);

  // define functor for pid probability algorithm
  StuProbabilityPidAlgorithm uPid(*pEvent);

  // loop over tracks in StEvent
  int goodTracks    = 0;
  int goodTracksEta = 0;
  StTpcDedxPidAlgorithm tpcDedxAlgo;
  Float_t nSigma;

  StSPtrVecTrackNode& trackNode = pEvent->trackNodes();

  for (unsigned int j=0; j < trackNode.size(); j++) {
    StGlobalTrack* gTrack = 
      static_cast<StGlobalTrack*>(trackNode[j]->track(global));
    StPrimaryTrack* pTrack = 
      static_cast<StPrimaryTrack*>(trackNode[j]->track(primary));

    // Tricking flowMaker to use global tracks 
    // StPrimaryTrack* pTrack = (StPrimaryTrack*)(trackNode[j]->track(global));

    if (pTrack && pTrack->flag() > 0) {
      StThreeVectorD p = pTrack->geometry()->momentum();
      StThreeVectorD g = gTrack->geometry()->momentum();
      // calculate the number of tracks with positive flag & |eta| < 0.75
      if (fabs(p.pseudoRapidity()) < 0.75) {
	goodTracksEta++;
      }
      if (StFlowCutTrack::CheckTrack(pTrack)) {
	// Instantiate new StFlowTrack
	StFlowTrack* pFlowTrack = new StFlowTrack;
	if (!pFlowTrack) return;
	pFlowTrack->SetPhi(p.phi());
	pFlowTrack->SetPhiGlobal(g.phi());
	pFlowTrack->SetEta(p.pseudoRapidity());
	pFlowTrack->SetEtaGlobal(g.pseudoRapidity());
	pFlowTrack->SetPt(p.perp());
	pFlowTrack->SetPtGlobal(g.perp());
	pFlowTrack->SetCharge(pTrack->geometry()->charge());
	pFlowTrack->SetDca(pTrack->impactParameter());
	pFlowTrack->SetDcaGlobal(gTrack->impactParameter());
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
	nSigma = (float)tpcDedxAlgo.numberOfSigma(StAntiProton::instance());
	pFlowTrack->SetPidAntiProton(nSigma);
	nSigma = (float)tpcDedxAlgo.numberOfSigma(StKaonMinus::instance());
	pFlowTrack->SetPidKaonMinus(nSigma);
	nSigma = (float)tpcDedxAlgo.numberOfSigma(StKaonPlus::instance());
	pFlowTrack->SetPidKaonPlus(nSigma);
	nSigma = (float)tpcDedxAlgo.numberOfSigma(StDeuteron::instance());
	pFlowTrack->SetPidDeuteron(nSigma);
	if (pTrack->geometry()->charge() < 0) {
	  pFlowTrack->SetPidAntiDeuteron(nSigma);
	}
	nSigma = (float)tpcDedxAlgo.numberOfSigma(StElectron::instance());
	pFlowTrack->SetPidElectron(nSigma);
	nSigma = (float)tpcDedxAlgo.numberOfSigma(StPositron::instance());
	pFlowTrack->SetPidPositron(nSigma);
	
	// dE/dx
        
  	StPtrVecTrackPidTraits traits = pTrack->pidTraits(kTpcId);
        unsigned int size = traits.size();
        if (size) {
	  StDedxPidTraits* pid;
	  for (unsigned int i = 0; i < traits.size(); i++) {
	    pid = dynamic_cast<StDedxPidTraits*>(traits[i]);
	    if (pid && pid->method() == kTruncatedMeanId) break;
	  }
	  assert(pid); pFlowTrack->SetDedx(pid->mean());
        }

	if (pFlowEvent->ProbPid()) {
	  // Probability pid
	  const StParticleDefinition* def = pTrack->pidTraits(uPid);
	  pFlowTrack->SetMostLikelihoodPID(uPid.mostLikelihoodParticleGeantID());
	  pFlowTrack->SetMostLikelihoodProb(uPid.mostLikelihoodProbability());
	  if (uPid.isExtrap()) pFlowTrack->SetExtrapTag(1); //mergin area. 
	  else pFlowTrack->SetExtrapTag(0); 
	}

	pFlowEvent->TrackCollection()->push_back(pFlowTrack);
	goodTracks++;
      }
    }
  }

  // Check Eta Symmetry
  if (!StFlowCutEvent::CheckEtaSymmetry(pEvent)) {  
    delete pFlowEvent;             //  delete this event
    pFlowEvent = NULL;
    return;
  }

  pFlowEvent->SetMultEta(goodTracksEta);
  pFlowEvent->SetCentrality(goodTracksEta);
  
  // For use with STL vector
  //   random_shuffle(pFlowEvent->TrackCollection()->begin(),
  // 		 pFlowEvent->TrackCollection()->end());

  pFlowEvent->TrackCollection()->random_shuffle();

  pFlowEvent->SetSelections();
  pFlowEvent->MakeSubEvents();
  pFlowEvent->SetPids();

}

//----------------------------------------------------------------------

void StFlowMaker::FillPicoEvent() {
  if (Debug()) gMessMgr->Info() << "FlowMaker: FillPicoEvent()" << endm;

  if (!pPicoEvent) {
    gMessMgr->Warning("##### FlowMaker: No FlowPicoEvent");
    return;
  }
  StFlowPicoTrack* pFlowPicoTrack = new StFlowPicoTrack();
  
  pPicoEvent->SetVersion(3);         // version 3 aihong pid
  pPicoEvent->SetEventID(pFlowEvent->EventID());
  pPicoEvent->SetRunID(pFlowEvent->RunID());
  pPicoEvent->SetOrigMult(pFlowEvent->OrigMult());
  pPicoEvent->SetMultEta(pFlowEvent->MultEta());
  pPicoEvent->SetCentrality(pFlowEvent->Centrality());
  pPicoEvent->SetVertexPos(pFlowEvent->VertexPos().x(),
			   pFlowEvent->VertexPos().y(),
			   pFlowEvent->VertexPos().z());
  pPicoEvent->SetCTB(pFlowEvent->CTB());
  pPicoEvent->SetZDCe(pFlowEvent->ZDCe());
  pPicoEvent->SetZDCw(pFlowEvent->ZDCw());
  
  StFlowTrackIterator itr;
  StFlowTrackCollection* pFlowTracks = pFlowEvent->TrackCollection();
  
  for (itr = pFlowTracks->begin(); itr != pFlowTracks->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    pFlowPicoTrack->SetPt(pFlowTrack->Pt());
    pFlowPicoTrack->SetPtGlobal(pFlowTrack->PtGlobal());
    pFlowPicoTrack->SetEta(pFlowTrack->Eta());
    pFlowPicoTrack->SetEtaGlobal(pFlowTrack->EtaGlobal());
    pFlowPicoTrack->SetDedx(pFlowTrack->Dedx());
    pFlowPicoTrack->SetPhi(pFlowTrack->Phi());
    pFlowPicoTrack->SetPhiGlobal(pFlowTrack->PhiGlobal());
    pFlowPicoTrack->SetCharge(pFlowTrack->Charge());
    pFlowPicoTrack->SetDca(pFlowTrack->Dca());
    pFlowPicoTrack->SetDcaGlobal(pFlowTrack->DcaGlobal());
    pFlowPicoTrack->SetChi2(pFlowTrack->Chi2());
    pFlowPicoTrack->SetFitPts(pFlowTrack->FitPts());
    pFlowPicoTrack->SetMaxPts(pFlowTrack->MaxPts());
    pFlowPicoTrack->SetMostLikelihoodPID(pFlowTrack->MostLikelihoodPID()); 
    pFlowPicoTrack->SetMostLikelihoodProb(pFlowTrack->MostLikelihoodProb());
    pFlowPicoTrack->SetExtrapTag(pFlowTrack->ExtrapTag());
    if (pFlowPicoTrack->Charge() > 0) {
      pFlowPicoTrack->SetPidPion(pFlowTrack->PidPiPlus());
      pFlowPicoTrack->SetPidProton(pFlowTrack->PidProton());
      pFlowPicoTrack->SetPidKaon(pFlowTrack->PidKaonPlus());
      pFlowPicoTrack->SetPidDeuteron(pFlowTrack->PidDeuteron());
      pFlowPicoTrack->SetPidElectron(pFlowTrack->PidPositron());
    }
    else {
      pFlowPicoTrack->SetPidPion(pFlowTrack->PidPiMinus());
      pFlowPicoTrack->SetPidProton(pFlowTrack->PidAntiProton());
      pFlowPicoTrack->SetPidKaon(pFlowTrack->PidKaonMinus());
      pFlowPicoTrack->SetPidDeuteron(pFlowTrack->PidAntiDeuteron());
      pFlowPicoTrack->SetPidElectron(pFlowTrack->PidElectron());
    }
    pPicoEvent->AddTrack(pFlowPicoTrack);
  }

  pFlowTree->Fill();  //fill the tree
  pPicoEvent->Clear();
  
  delete pFlowPicoTrack;  
}

//-----------------------------------------------------------------------

Bool_t StFlowMaker::FillFromPicoDST(StFlowPicoEvent* pPicoEvent) {
  // Make StFlowEvent from StFlowPicoEvent
  
  if (Debug()) gMessMgr->Info() << "FlowMaker: FillFromPicoDST()" << endm;

  if (!pPicoEvent || !pPicoChain->GetEntry(mPicoEventCounter++)) {
    cout << "##### FlowMaker: no more events" << endl; 
    return kFALSE; 
  }
  
  // Fill FlowEvent
  pFlowEvent->SetPhiWeight(mPhiWgt);

  if (!pPicoEvent->Version()) {
    FillFromPicoVersion0DST(pPicoEvent);
  }
  if (pPicoEvent->Version() == 1) {
    FillFromPicoVersion1DST(pPicoEvent);
  }
  if (pPicoEvent->Version() == 2) {
    FillFromPicoVersion2DST(pPicoEvent);
  }
  if (pPicoEvent->Version() == 3) {
    FillFromPicoVersion3DST(pPicoEvent);
  }
  
  // Check event cuts and Eta Symmetry
  if (!StFlowCutEvent::CheckEvent(pPicoEvent) ||
      !StFlowCutEvent::CheckEtaSymmetry(pPicoEvent)) {  
    Int_t eventID = pPicoEvent->EventID();
    gMessMgr->Info() << "##### FlowMaker: picoevent " << eventID 
		     << " cut" << endm;
    delete pFlowEvent;             // delete this event
    pFlowEvent = NULL;
    return kTRUE;
  }
  
  // For use with STL vector
  //   random_shuffle(pFlowEvent->TrackCollection()->begin(),
  // 		 pFlowEvent->TrackCollection()->end());
  
  pFlowEvent->TrackCollection()->random_shuffle();

  pFlowEvent->SetSelections();
  pFlowEvent->MakeSubEvents();
  pFlowEvent->SetPids();
  
  return kTRUE;
}

//-----------------------------------------------------------------------

Bool_t StFlowMaker::FillFromPicoVersion0DST(StFlowPicoEvent* pPicoEvent) {
  // Make StFlowEvent from StFlowPicoEvent
  
  if (Debug()) gMessMgr->Info() << "FlowMaker: FillFromPicoVersion0DST()" 
				<< endm;
  
  pFlowEvent->SetEventID(pPicoEvent->EventID());
  UInt_t origMult = pPicoEvent->OrigMult();
  pFlowEvent->SetOrigMult(origMult);
  PR(origMult);
  pFlowEvent->SetVertexPos(StThreeVectorF(pPicoEvent->VertexX(),
					  pPicoEvent->VertexY(),
					  pPicoEvent->VertexZ()) );
  pFlowEvent->SetCTB(pPicoEvent->CTB());
  pFlowEvent->SetZDCe(pPicoEvent->ZDCe());
  pFlowEvent->SetZDCw(pPicoEvent->ZDCw());
  
  int    goodTracks    = 0;
  UInt_t goodTracksEta = 0;
  // Fill FlowTracks
  for (Int_t nt=0; nt<pPicoEvent->GetNtrack(); nt++) {
    StFlowPicoTrack* pPicoTrack = (StFlowPicoTrack*)pPicoEvent->Tracks()
      ->UncheckedAt(nt);
    if (fabs(pPicoTrack->Eta()) < 0.75) {
      goodTracksEta++;
    }
    if (pPicoTrack && StFlowCutTrack::CheckTrack(pPicoTrack)) {
      // Instantiate new StFlowTrack
      StFlowTrack* pFlowTrack = new StFlowTrack;
      if (!pFlowTrack) return kFALSE;
      pFlowTrack->SetPt(pPicoTrack->Pt());
      pFlowTrack->SetPhi(pPicoTrack->Phi());
      pFlowTrack->SetEta(pPicoTrack->Eta());
      pFlowTrack->SetDedx(pPicoTrack->Dedx());
      pFlowTrack->SetCharge(pPicoTrack->Charge());
      pFlowTrack->SetDca(pPicoTrack->Dca()/10000.);
      pFlowTrack->SetChi2(pPicoTrack->Chi2()/10000.);
      pFlowTrack->SetFitPts(pPicoTrack->FitPts());
      pFlowTrack->SetMaxPts(pPicoTrack->MaxPts());
      
      pFlowEvent->TrackCollection()->push_back(pFlowTrack);
      goodTracks++;
    }
  }
  
  // Recreate centrality
  pFlowEvent->SetMultEta(goodTracksEta);
  pFlowEvent->SetCentrality(goodTracksEta);
  
  return kTRUE;
}

//-----------------------------------------------------------------------

Bool_t StFlowMaker::FillFromPicoVersion1DST(StFlowPicoEvent* pPicoEvent) {
  // Make StFlowEvent from StFlowPicoEvent
  
  if (Debug()) gMessMgr->Info() << "FlowMaker: FillFromPicoVersion1DST()" 
				<< endm;

  pFlowEvent->SetEventID(pPicoEvent->EventID());
  UInt_t origMult = pPicoEvent->OrigMult();
  pFlowEvent->SetOrigMult(origMult);
  PR(origMult);
  pFlowEvent->SetVertexPos(StThreeVectorF(pPicoEvent->VertexX(),
					  pPicoEvent->VertexY(),
					  pPicoEvent->VertexZ()) );
  pFlowEvent->SetMultEta(pPicoEvent->MultEta());
  pFlowEvent->SetCentrality(pPicoEvent->MultEta());
  pFlowEvent->SetRunID(pPicoEvent->RunID());
  pFlowEvent->SetCTB(pPicoEvent->CTB());
  pFlowEvent->SetZDCe(pPicoEvent->ZDCe());
  pFlowEvent->SetZDCw(pPicoEvent->ZDCw());
  
  int    goodTracks    = 0;
  // Fill FlowTracks
  for (Int_t nt=0; nt<pPicoEvent->GetNtrack(); nt++) {
    StFlowPicoTrack* pPicoTrack = (StFlowPicoTrack*)pPicoEvent->Tracks()
      ->UncheckedAt(nt);
    if (pPicoTrack && StFlowCutTrack::CheckTrack(pPicoTrack)) {
      // Instantiate new StFlowTrack
      StFlowTrack* pFlowTrack = new StFlowTrack;
      if (!pFlowTrack) return kFALSE;
      pFlowTrack->SetPt(pPicoTrack->Pt());
      pFlowTrack->SetPhi(pPicoTrack->Phi());
      pFlowTrack->SetEta(pPicoTrack->Eta());
      pFlowTrack->SetDedx(pPicoTrack->Dedx());
      pFlowTrack->SetCharge(pPicoTrack->Charge());
      pFlowTrack->SetDca(pPicoTrack->Dca());
      pFlowTrack->SetDcaGlobal(pPicoTrack->DcaGlobal());
      pFlowTrack->SetChi2(pPicoTrack->Chi2());
      pFlowTrack->SetFitPts(pPicoTrack->FitPts());
      pFlowTrack->SetMaxPts(pPicoTrack->MaxPts());
      if (pPicoTrack->Charge() < 0) {
	pFlowTrack->SetPidPiMinus(pPicoTrack->PidPion());
	pFlowTrack->SetPidAntiProton(pPicoTrack->PidProton());
	pFlowTrack->SetPidKaonMinus(pPicoTrack->PidKaon());
	pFlowTrack->SetPidAntiDeuteron(pPicoTrack->PidDeuteron());
	pFlowTrack->SetPidElectron(pPicoTrack->PidElectron());
      }
      else {
	pFlowTrack->SetPidPiPlus(pPicoTrack->PidPion());
	pFlowTrack->SetPidProton(pPicoTrack->PidProton());
	pFlowTrack->SetPidKaonPlus(pPicoTrack->PidKaon());
	pFlowTrack->SetPidDeuteron(pPicoTrack->PidDeuteron());
	pFlowTrack->SetPidPositron(pPicoTrack->PidElectron());
      }
      
      pFlowEvent->TrackCollection()->push_back(pFlowTrack);
      goodTracks++;
    }
  }
  
  return kTRUE;
}

//-----------------------------------------------------------------------

Bool_t StFlowMaker::FillFromPicoVersion2DST(StFlowPicoEvent* pPicoEvent) {
  // Make StFlowEvent from StFlowPicoEvent
  
  if (Debug()) gMessMgr->Info() << "FlowMaker: FillFromPicoVersion2DST()" 
				<< endm;

  pFlowEvent->SetEventID(pPicoEvent->EventID());
  UInt_t origMult = pPicoEvent->OrigMult();
  pFlowEvent->SetOrigMult(origMult);
  PR(origMult);
  pFlowEvent->SetVertexPos(StThreeVectorF(pPicoEvent->VertexX(),
					  pPicoEvent->VertexY(),
					  pPicoEvent->VertexZ()) );
  pFlowEvent->SetMultEta(pPicoEvent->MultEta());
  pFlowEvent->SetCentrality(pPicoEvent->MultEta());
  pFlowEvent->SetRunID(pPicoEvent->RunID());
  pFlowEvent->SetCTB(pPicoEvent->CTB());
  pFlowEvent->SetZDCe(pPicoEvent->ZDCe());
  pFlowEvent->SetZDCw(pPicoEvent->ZDCw());
  
  int    goodTracks    = 0;
  // Fill FlowTracks
  for (Int_t nt=0; nt<pPicoEvent->GetNtrack(); nt++) {
    StFlowPicoTrack* pPicoTrack = (StFlowPicoTrack*)pPicoEvent->Tracks()
      ->UncheckedAt(nt);
    if (pPicoTrack && StFlowCutTrack::CheckTrack(pPicoTrack)) {
      // Instantiate new StFlowTrack
      StFlowTrack* pFlowTrack = new StFlowTrack;
      if (!pFlowTrack) return kFALSE;
      pFlowTrack->SetPt(pPicoTrack->Pt());
      pFlowTrack->SetPtGlobal(pPicoTrack->PtGlobal());
      pFlowTrack->SetPhi(pPicoTrack->Phi());
      pFlowTrack->SetPhiGlobal(pPicoTrack->PhiGlobal());
      pFlowTrack->SetEta(pPicoTrack->Eta());
      pFlowTrack->SetEtaGlobal(pPicoTrack->EtaGlobal());
      pFlowTrack->SetDedx(pPicoTrack->Dedx());
      pFlowTrack->SetCharge(pPicoTrack->Charge());
      pFlowTrack->SetDca(pPicoTrack->Dca());
      pFlowTrack->SetDcaGlobal(pPicoTrack->DcaGlobal());
      pFlowTrack->SetChi2(pPicoTrack->Chi2());
      pFlowTrack->SetFitPts(pPicoTrack->FitPts());
      pFlowTrack->SetMaxPts(pPicoTrack->MaxPts());
      if (pPicoTrack->Charge() < 0) {
	pFlowTrack->SetPidPiMinus(pPicoTrack->PidPion());
	pFlowTrack->SetPidAntiProton(pPicoTrack->PidProton());
	pFlowTrack->SetPidKaonMinus(pPicoTrack->PidKaon());
	pFlowTrack->SetPidAntiDeuteron(pPicoTrack->PidDeuteron());
	pFlowTrack->SetPidElectron(pPicoTrack->PidElectron());
      }
      else {
	pFlowTrack->SetPidPiPlus(pPicoTrack->PidPion());
	pFlowTrack->SetPidProton(pPicoTrack->PidProton());
	pFlowTrack->SetPidKaonPlus(pPicoTrack->PidKaon());
	pFlowTrack->SetPidDeuteron(pPicoTrack->PidDeuteron());
	pFlowTrack->SetPidPositron(pPicoTrack->PidElectron());
      }
      
      pFlowEvent->TrackCollection()->push_back(pFlowTrack);
      goodTracks++;
    }
  }
  
  return kTRUE;
}

//-----------------------------------------------------------------------

Bool_t StFlowMaker::FillFromPicoVersion3DST(StFlowPicoEvent* pPicoEvent) {
  // Make StFlowEvent from StFlowPicoEvent
  
  if (Debug()) gMessMgr->Info() << "FlowMaker: FillFromPicoVersion3DST()" 
				<< endm;

  pFlowEvent->SetEventID(pPicoEvent->EventID());
  UInt_t origMult = pPicoEvent->OrigMult();
  pFlowEvent->SetOrigMult(origMult);
  PR(origMult);
  pFlowEvent->SetVertexPos(StThreeVectorF(pPicoEvent->VertexX(),
					  pPicoEvent->VertexY(),
					  pPicoEvent->VertexZ()) );
  pFlowEvent->SetMultEta(pPicoEvent->MultEta());
  pFlowEvent->SetCentrality(pPicoEvent->MultEta());
  pFlowEvent->SetRunID(pPicoEvent->RunID());
  pFlowEvent->SetCTB(pPicoEvent->CTB());
  pFlowEvent->SetZDCe(pPicoEvent->ZDCe());
  pFlowEvent->SetZDCw(pPicoEvent->ZDCw());
  
  int    goodTracks    = 0;
  // Fill FlowTracks
  for (Int_t nt=0; nt < pPicoEvent->GetNtrack(); nt++) {
    StFlowPicoTrack* pPicoTrack = (StFlowPicoTrack*)pPicoEvent->Tracks()
      ->UncheckedAt(nt);
    if (pPicoTrack && StFlowCutTrack::CheckTrack(pPicoTrack)) {
      // Instantiate new StFlowTrack
      StFlowTrack* pFlowTrack = new StFlowTrack;
      if (!pFlowTrack) return kFALSE;
      pFlowTrack->SetPt(pPicoTrack->Pt());
      pFlowTrack->SetPtGlobal(pPicoTrack->PtGlobal());
      pFlowTrack->SetPhi(pPicoTrack->Phi());
      pFlowTrack->SetPhiGlobal(pPicoTrack->PhiGlobal());
      pFlowTrack->SetEta(pPicoTrack->Eta());
      pFlowTrack->SetEtaGlobal(pPicoTrack->EtaGlobal());
      pFlowTrack->SetDedx(pPicoTrack->Dedx());
      pFlowTrack->SetCharge(pPicoTrack->Charge());
      pFlowTrack->SetDca(pPicoTrack->Dca());
      pFlowTrack->SetDcaGlobal(pPicoTrack->DcaGlobal());
      pFlowTrack->SetChi2(pPicoTrack->Chi2());
      pFlowTrack->SetFitPts(pPicoTrack->FitPts());
      pFlowTrack->SetMaxPts(pPicoTrack->MaxPts());
      pFlowTrack->SetMostLikelihoodPID(pPicoTrack->MostLikelihoodPID()); 
      pFlowTrack->SetMostLikelihoodProb(pPicoTrack->MostLikelihoodProb());
      pFlowTrack->SetExtrapTag(pPicoTrack->ExtrapTag());
      if (pPicoTrack->Charge() < 0) {
	pFlowTrack->SetPidPiMinus(pPicoTrack->PidPion());
	pFlowTrack->SetPidAntiProton(pPicoTrack->PidProton());
	pFlowTrack->SetPidKaonMinus(pPicoTrack->PidKaon());
	pFlowTrack->SetPidAntiDeuteron(pPicoTrack->PidDeuteron());
	pFlowTrack->SetPidElectron(pPicoTrack->PidElectron());
      }
      else {
	pFlowTrack->SetPidPiPlus(pPicoTrack->PidPion());
	pFlowTrack->SetPidProton(pPicoTrack->PidProton());
	pFlowTrack->SetPidKaonPlus(pPicoTrack->PidKaon());
	pFlowTrack->SetPidDeuteron(pPicoTrack->PidDeuteron());
	pFlowTrack->SetPidPositron(pPicoTrack->PidElectron());
      }

      pFlowEvent->TrackCollection()->push_back(pFlowTrack);
      goodTracks++;
    }
  }
  
  return kTRUE;
}

//-----------------------------------------------------------------------

void StFlowMaker::PrintSubeventMults() {
  if (Debug()) gMessMgr->Info() << "FlowMaker: PrintSubeventMults()" << endm;
  
  int j, k, n;
  
  pFlowSelect->SetSubevent(-1);
  for (j = 0; j < Flow::nHars; j++) {
    pFlowSelect->SetHarmonic(j);
    for (k = 0; k < Flow::nSels; k++) {
      pFlowSelect->SetSelection(k);
      cout << "j,k= " << j << k << " : " << pFlowEvent->Mult(pFlowSelect) 
	   << endl;
    }
  }
  
  for (j = 0; j < Flow::nHars; j++) {
    pFlowSelect->SetHarmonic(j);
    for (k = 0; k <Flow:: nSels; k++) {
      pFlowSelect->SetSelection(k);
      for (n = 0; n < Flow::nSubs+1; n++) {
	pFlowSelect->SetSubevent(n);
	cout << "j,k,n= " << j << k << n << " : " << 
	  pFlowEvent->Mult(pFlowSelect) << endl;
      }
    }
  }
  
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::InitPicoEventWrite() {
  if (Debug()) gMessMgr->Info() << "FlowMaker: InitPicoEventWrite()" << endm;
  
  Int_t split  = 1;       // by default split Event into sub branches
  Int_t comp   = 1;       // by default file is compressed
  Int_t bufsize = 256000;
  if (split) bufsize /= 4;
  
  // creat a Picoevent and an output file
  pPicoEvent = new StFlowPicoEvent();   

  TString* filestring = new TString(mPicoEventDir);
  filestring->Append(mEventFileName);
  filestring->Append(".flowpicoevent.root");
  pPicoDST = new TFile(filestring->Data(),"RECREATE","Flow Pico DST file");
  if (!pPicoDST) {
    cout << "##### FlowMaker: Warning: no PicoEvents file = " 
	 << filestring->Data() << endl;
    return kStFatal;
  }
  pPicoDST->SetFormat(1);
  pPicoDST->SetCompressionLevel(comp);
  gMessMgr->Info() << "##### FlowMaker: PicoEvents file = " 
		   << filestring->Data() << endm;
  
  // Create a ROOT Tree and one superbranch
  pFlowTree = new TTree("FlowTree", "Flow Pico Tree");
  if (!pFlowTree) {
    cout << "##### FlowMaker: Warning: No FlowPicoTree" << endl;
    return kStFatal;
  }
  
  pFlowTree->SetAutoSave(1000000);  // autosave when 1 Mbyte written
  pFlowTree->Branch("pPicoEvent", "StFlowPicoEvent", &pPicoEvent,
		    bufsize, split);

  delete filestring;
  
  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::InitPicoEventRead() {
  if (Debug()) gMessMgr->Info() << "FlowMaker: InitPicoEventRead()" << endm;
  
  pPicoEvent = new StFlowPicoEvent(); 
  pPicoChain = new TChain("FlowTree");
  
  for (Int_t ilist = 0;  ilist < pPicoFileList->GetNBundles(); ilist++) {
    pPicoFileList->GetNextBundle();
    if (Debug()) gMessMgr->Info() << " doFlowEvents -  input fileList = " 
				  << pPicoFileList->GetFileName(0) << endm;
    pPicoChain->Add(pPicoFileList->GetFileName(0));
  }
  
  pPicoChain->SetBranchAddress("pPicoEvent", &pPicoEvent);
  
  Int_t nEntries = (Int_t)pPicoChain->GetEntries(); 
  gMessMgr->Info() << "##### FlowMaker: events in Pico-DST file = "
		   << nEntries << endm;
  
  mPicoEventCounter = 0;
  
  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::InitFlowEventWrite() {
  if (Debug()) gMessMgr->Info() << "FlowMaker: InitFlowEventWrite()" << endm;
  
  Int_t split  = 1;       // by default, split Event in sub branches
  Int_t comp   = 1;       // by default file is compressed
  Int_t bufsize = 256000;
  if (split)  bufsize /= 4;
  
  // Create a new ROOT binary machine independent file.
  // Note that this file may contain any kind of ROOT objects, histograms,
  // pictures, graphics objects, detector geometries, tracks, events, etc..
  // This file is now becoming the current directory.
  
  pFlowDST = new TFile("flowevent.root", "RECREATE", "Flow micro DST file");
  if (!pFlowDST) {
    cout << "##### FlowMaker: Warning: no FlowEvents file" << endl;
    return kStFatal;
  }
  
  pFlowDST->SetCompressionLevel(comp);
  
  // Create a ROOT Tree and one superbranch
  pFlowMicroTree = new TTree("FlowMicroTree", "Flow Micro Tree");
  if (!pFlowMicroTree) {
    cout << "##### FlowMaker: Warning: No FlowMicroTree" << endl;
    return kStFatal;
  }

  pFlowMicroTree->SetAutoSave(1000000);  // autosave when 1 Mbyte written
  pFlowMicroTree->Branch("pFlowEvent", "StFlowEvent", &pFlowEvent, 
			 bufsize, split);
  
  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::InitFlowEventRead() {
  if (Debug()) gMessMgr->Info() << "FlowMaker: InitFlowEventRead()" << endm;
  
  pFlowDST = new TFile("flowevent.root", "READ");
  if (pFlowDST) return kStFatal;

  return kStOK;
}

//-----------------------------------------------------------------------
