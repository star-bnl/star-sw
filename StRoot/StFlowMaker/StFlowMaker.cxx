//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowMaker.cxx,v 1.88 2004/04/09 15:49:01 aihong Exp $
//
// Authors: Raimond Snellings and Art Poskanzer, LBNL, Jun 1999
//          FTPC added by Markus Oldenburg, MPI, Dec 2000
//          MuDst enabled by Kirill Filimonov, LBNL, Jun 2002
//
//////////////////////////////////////////////////////////////////////
//
// Description:
//    Maker to fill StFlowEvent from StEvent, picoevent, or microevent
//
//////////////////////////////////////////////////////////////////////

#include <Stiostream.h>
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
#include "TText.h"
#include "StuRefMult.hh"
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
#include "StHbtMaker/StHbtMaker.h"
#include "StHbtMaker/Infrastructure/StHbtTrack.hh"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#define PR(x) cout << "##### FlowMaker: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowMaker)

//-----------------------------------------------------------------------

StFlowMaker::StFlowMaker(const Char_t* name): 
  StMaker(name), 
  mPicoEventWrite(kFALSE), mPicoEventRead(kFALSE), mMuEventRead(kFALSE), pEvent(NULL) {
  pFlowSelect = new StFlowSelection();
  SetPicoEventDir("./");
  StMuTrack::Class()->IgnoreTObjectStreamer();
  StMuHelix::Class()->IgnoreTObjectStreamer();
}

StFlowMaker::StFlowMaker(const Char_t* name,
			 const StFlowSelection& flowSelect) :
  StMaker(name), 
  mPicoEventWrite(kFALSE), mPicoEventRead(kFALSE), mMuEventRead(kFALSE), pEvent(NULL) {
  pFlowSelect = new StFlowSelection(flowSelect); //copy constructor
  SetPicoEventDir("./");
  StMuTrack::Class()->IgnoreTObjectStreamer();
  StMuHelix::Class()->IgnoreTObjectStreamer();
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

  // Get the input file name from the IOMaker
  if (!mPicoEventRead && pIOMaker) {
    mEventFileName = strrchr(pIOMaker->GetFile(),'/')+1;
    if (Debug()) { 
      gMessMgr->Info() << "FlowMaker: filename: " << mEventFileName << endm;
      gMessMgr->Info() << "FlowMaker: Old filename: " 
		       << mEventFileNameOld << endm;  
    }

    if (mEventFileName != mEventFileNameOld) { 
      //if (Debug()) gMessMgr->Info() << "FlowMaker: New file opened " << endm;
      if (Debug()) gMessMgr->Info() << "##### FlowMaker: " <<  mEventFileName << endm;
      if (mPicoEventWrite) {
	if (pPicoDST->IsOpen()) {
	  pPicoDST->Write(0, TObject::kOverwrite);
	  pPicoDST->Close();
	}
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
  if (!mPicoEventRead && !mMuEventRead) {
    pEvent = dynamic_cast<StEvent*>(GetInputDS("StEvent"));
    if (!pEvent) {
      if (Debug()) { 
	gMessMgr->Info() << "FlowMaker: no StEvent " << endm;
      }
      return kStOK; // If no event, we're done
    }

    // Check the event cuts and fill StFlowEvent
    if (StFlowCutEvent::CheckEvent(pEvent)) {
      // Instantiate a new StFlowEvent
      pFlowEvent = new StFlowEvent;
      if (!pFlowEvent) return kStOK;
      FillFlowEvent();
      if (!pFlowEvent) return kStOK;  // could have been deleted
      if (mPicoEventWrite) FillPicoEvent();
    } else {
      Long_t eventID = pEvent->id();
      gMessMgr->Info() << "##### FlowMaker: event " << eventID 
		       << " cut" << endm;
      return kStOK; // to prevent seg. fault when no event survives
    }

  } else if (mPicoEventRead) {
    // Instantiate a new StFlowEvent
    pFlowEvent = new StFlowEvent;
    if (!pFlowEvent) return kStOK;
    if (!FillFromPicoDST(pPicoEvent)) return kStEOF; // false if EOF
    if (!pFlowEvent) return kStOK; // could have been deleted
  } else if (mMuEventRead) {
    pFlowEvent = new StFlowEvent;
    if (!pFlowEvent) return kStOK;
    if (!FillFromMuDST()) return kStEOF; // false if EOF
    if (!pFlowEvent) return kStOK; // could have been deleted
    mEventFileName = strrchr(pMuChain->GetFile()->GetName(),'/')+1;
    if (mEventFileName != mEventFileNameOld) { 
      //if (Debug()) gMessMgr->Info() << "FlowMaker: New file opened " << endm;
      if (Debug()) gMessMgr->Info() << "##### FlowMaker: " <<  mEventFileName << endm;
      if (mPicoEventWrite) {
	if (pPicoDST->IsOpen()) {
	  pPicoDST->Write(0, TObject::kOverwrite);
	  pPicoDST->Close();
	}
        if (pPicoDST) delete pPicoDST;
        pPicoDST = NULL;
        InitPicoEventWrite();
      }
      mEventFileNameOld = mEventFileName;
    }
    
    if (mPicoEventWrite) FillPicoEvent();
  }
  
  
  UInt_t flowEventMult;
  if (!pFlowEvent) { flowEventMult = 0; }
  else { flowEventMult = pFlowEvent->FlowEventMult(); }

  if (flowEventMult) { // to prevent seg. fault when no particles
    int runID = pFlowEvent->RunID();
    if (runID != mRunID) {
      double beamEnergy = pFlowEvent->CenterOfMassEnergy();
      double magneticField = pFlowEvent->MagneticField();
      short beamMassE   = pFlowEvent->BeamMassNumberEast();
      short beamMassW   = pFlowEvent->BeamMassNumberWest();
      gMessMgr->Info() << "##### FlowMaker: " << runID << ", " <<
	beamEnergy << " GeV/A " << beamMassE << "+" << beamMassW << 
	", B= " << magneticField << endm;
      mRunID = runID;
    }
  }

  if (Debug()) StMaker::PrintInfo();
  
  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::Init() {
  if (Debug()) gMessMgr->Info() << "FlowMaker: Init()" << endm;

  // Open PhiWgt file
  ReadPhiWgtFile();

  Int_t kRETURN = kStOK;

  if (!mPicoEventRead || !mMuEventRead) {
    pIOMaker = (StIOMaker*)GetMaker("IO");
    if (pIOMaker) {
      mEventFileName = "";
      mEventFileNameOld = mEventFileName;
    } 
  }
  StuProbabilityPidAlgorithm::readParametersFromFile("PIDTable.root");

  if (mPicoEventWrite) kRETURN += InitPicoEventWrite();
  if (mPicoEventRead)  kRETURN += InitPicoEventRead();
  if (mMuEventRead)    kRETURN += InitMuEventRead();

  gMessMgr->SetLimit("##### FlowMaker", 5);
  gMessMgr->Info("##### FlowMaker: $Id: StFlowMaker.cxx,v 1.88 2004/04/09 15:49:01 aihong Exp $");

  if (kRETURN) gMessMgr->Info() << "##### FlowMaker: Init return = " << kRETURN << endm;
  return kRETURN;
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::InitRun(int runID) {
  if (Debug()) gMessMgr->Info() << "FlowMaker: InitRun()" << endm;
  PR(runID);

  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::Finish() {
  if (Debug()) gMessMgr->Info() << "FlowMaker: Finish()" << endm;

  // Print message summary
  cout << endl;
  gMessMgr->Summary(3);
  cout << endl;

  // Print the selection object details
  pFlowSelect->PrintList();

  // Print the cut lists
  cout << "#######################################################" << endl;
  cout << "##### FlowMaker: Cut Lists" << endl;
  StFlowCutEvent::PrintCutList();
  StFlowCutTrack::PrintCutList();
  pFlowEvent->PrintSelectionList();

  if (mPicoEventWrite && pPicoDST->IsOpen()) {
    pPicoDST->Write(0, TObject::kOverwrite);
    pPicoDST->Close();
  }

  return StMaker::Finish();
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::ReadPhiWgtFile() {
  // Read the PhiWgt root file

  if (Debug()) gMessMgr->Info() << "FlowMaker: ReadPhiWgtFile()" << endm;

  TDirectory* dirSave = gDirectory;
  TFile* pPhiWgtFile = new TFile("flowPhiWgt.hist.root", "READ");
  if (!pPhiWgtFile->IsOpen()) {
    gMessMgr->Info("##### FlowMaker: No PhiWgt file. Will set weights = 1.");
  }
  gDirectory = dirSave;

  // Read text object
  if (pPhiWgtFile->IsOpen()) {
    TText* textInfo = dynamic_cast<TText*>(pPhiWgtFile->Get("info"));
    if (textInfo) {
      mFirstLastPhiWgt = kTRUE;
      gMessMgr->Info("##### FlowMaker: PhiWgt file uses z of first-last points");
      cout << "##### FlowMaker: PhiWgt file written with " ;
      textInfo->ls();
    } else {
      gMessMgr->Info("##### FlowMaker: PhiWgt file uses vertex z and eta");
    }
  }

  // Fill mPhiWgt for each selection and each harmonic
  for (int k = 0; k < Flow::nSels; k++) {
    char countSels[2];
    sprintf(countSels,"%d",k+1);
    for (int j = 0; j < Flow::nHars; j++) {
      char countHars[2];
      sprintf(countHars,"%d",j+1);
      // Tpc
      TString* histTitle = new TString("Flow_Phi_Weight_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      // Tpc (FarEast)
      TString* histTitleFarEast = new TString("Flow_Phi_Weight_FarEast_Sel");
      histTitleFarEast->Append(*countSels);
      histTitleFarEast->Append("_Har");
      histTitleFarEast->Append(*countHars);
      // Tpc (East)
      TString* histTitleEast = new TString("Flow_Phi_Weight_East_Sel");
      histTitleEast->Append(*countSels);
      histTitleEast->Append("_Har");
      histTitleEast->Append(*countHars);
      // Tpc (West)
      TString* histTitleWest = new TString("Flow_Phi_Weight_West_Sel");
      histTitleWest->Append(*countSels);
      histTitleWest->Append("_Har");
      histTitleWest->Append(*countHars);
      // Tpc (FarWest)
      TString* histTitleFarWest = new TString("Flow_Phi_Weight_FarWest_Sel");
      histTitleFarWest->Append(*countSels);
      histTitleFarWest->Append("_Har");
      histTitleFarWest->Append(*countHars);
      // Ftpc (FarEast)
      TString* histTitleFtpcFarEast = new TString("Flow_Phi_Weight_FtpcFarEast_Sel");
      histTitleFtpcFarEast->Append(*countSels);
      histTitleFtpcFarEast->Append("_Har");
      histTitleFtpcFarEast->Append(*countHars);
      // Ftpc (East)
      TString* histTitleFtpcEast = new TString("Flow_Phi_Weight_FtpcEast_Sel");
      histTitleFtpcEast->Append(*countSels);
      histTitleFtpcEast->Append("_Har");
      histTitleFtpcEast->Append(*countHars);
      // Ftpc (West)
      TString* histTitleFtpcWest = new TString("Flow_Phi_Weight_FtpcWest_Sel");
      histTitleFtpcWest->Append(*countSels);
      histTitleFtpcWest->Append("_Har");
      histTitleFtpcWest->Append(*countHars);
      // Ftpc (FarWest)
      TString* histTitleFtpcFarWest = new TString("Flow_Phi_Weight_FtpcFarWest_Sel");
      histTitleFtpcFarWest->Append(*countSels);
      histTitleFtpcFarWest->Append("_Har");
      histTitleFtpcFarWest->Append(*countHars);
      if (pPhiWgtFile->IsOpen()) {
	TH1* phiWgtHist = dynamic_cast<TH1*>(pPhiWgtFile->Get(histTitle->Data()));
	if (k==0 && j==0 && phiWgtHist) {
	  mOnePhiWgt = kTRUE;
	  gMessMgr->Info("##### FlowMaker: Using old type phi weight file.");
	}
	if (mOnePhiWgt) {
	  for (int n = 0; n < Flow::nPhiBins; n++) {
	    mPhiWgt[k][j][n] = (phiWgtHist) ? 
	      phiWgtHist->GetBinContent(n+1) : 1.;
	  }
	} else {
	  TH1* phiWgtHistFarEast = dynamic_cast<TH1*>(pPhiWgtFile->
						      Get(histTitleFarEast->Data()));
	  TH1* phiWgtHistEast = dynamic_cast<TH1*>(pPhiWgtFile->
						   Get(histTitleEast->Data()));
	  TH1* phiWgtHistWest = dynamic_cast<TH1*>(pPhiWgtFile->
						   Get(histTitleWest->Data()));
	  TH1* phiWgtHistFarWest = dynamic_cast<TH1*>(pPhiWgtFile->
						      Get(histTitleFarWest->Data()));
	  for (int n = 0; n < Flow::nPhiBins; n++) {
	    mPhiWgtFarEast[k][j][n] = (phiWgtHistFarEast) ? 
	      phiWgtHistFarEast->GetBinContent(n+1) : 1.;
	    mPhiWgtEast[k][j][n] = (phiWgtHistEast) ? 
	      phiWgtHistEast->GetBinContent(n+1) : 1.;
	    mPhiWgtWest[k][j][n] = (phiWgtHistWest) ? 
	      phiWgtHistWest->GetBinContent(n+1) : 1.;
	    mPhiWgtFarWest[k][j][n] = (phiWgtHistFarWest) ? 
	      phiWgtHistFarWest->GetBinContent(n+1) : 1.;
	  }
	}
	TH1* phiWgtHistFtpcFarEast = dynamic_cast<TH1*>(pPhiWgtFile->
							Get(histTitleFtpcFarEast->Data()));
	TH1* phiWgtHistFtpcEast = dynamic_cast<TH1*>(pPhiWgtFile->
						     Get(histTitleFtpcEast->Data()));
	TH1* phiWgtHistFtpcWest = dynamic_cast<TH1*>(pPhiWgtFile->
						     Get(histTitleFtpcWest->Data()));
	TH1* phiWgtHistFtpcFarWest = dynamic_cast<TH1*>(pPhiWgtFile->
							Get(histTitleFtpcFarWest->Data()));
	for (int n = 0; n < Flow::nPhiBinsFtpc; n++) {
	  mPhiWgtFtpcFarEast[k][j][n] = (phiWgtHistFtpcFarEast) ? 
	    phiWgtHistFtpcFarEast->GetBinContent(n+1) : 1.;
	  mPhiWgtFtpcEast[k][j][n] = (phiWgtHistFtpcEast) ? 
	    phiWgtHistFtpcEast->GetBinContent(n+1) : 1.;
	  mPhiWgtFtpcWest[k][j][n] = (phiWgtHistFtpcWest) ? 
	    phiWgtHistFtpcWest->GetBinContent(n+1) : 1.;
	  mPhiWgtFtpcFarWest[k][j][n] = (phiWgtHistFtpcFarWest) ? 
	    phiWgtHistFtpcFarWest->GetBinContent(n+1) : 1.;
	}
      } else {
	for (int n = 0; n < Flow::nPhiBins; n++) {
	  mPhiWgt[k][j][n]        = 1.;
	  mPhiWgtFarEast[k][j][n] = 1.;
	  mPhiWgtEast[k][j][n]    = 1.;
	  mPhiWgtWest[k][j][n]    = 1.;
	  mPhiWgtFarWest[k][j][n] = 1.;
	}
	for (int n = 0; n < Flow::nPhiBinsFtpc; n++) {
	  mPhiWgtFtpcFarEast[k][j][n] = 1.;
	  mPhiWgtFtpcWest[k][j][n] = 1.;
	  mPhiWgtFtpcEast[k][j][n] = 1.;
	  mPhiWgtFtpcFarWest[k][j][n] = 1.;
	}
      }
      delete histTitle;
      delete histTitleFarEast;
      delete histTitleEast;
      delete histTitleWest;
      delete histTitleFarWest;
      delete histTitleFtpcFarEast;
      delete histTitleFtpcEast;
      delete histTitleFtpcWest;
      delete histTitleFtpcFarWest;
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
  if (mOnePhiWgt) {
    pFlowEvent->SetOnePhiWgt();
    pFlowEvent->SetPhiWeight(mPhiWgt);
  } else {
    if (mFirstLastPhiWgt) pFlowEvent->SetFirstLastPhiWgt();
    pFlowEvent->SetPhiWeightFarEast(mPhiWgtFarEast);
    pFlowEvent->SetPhiWeightEast(mPhiWgtEast);
    pFlowEvent->SetPhiWeightWest(mPhiWgtWest);
    pFlowEvent->SetPhiWeightFarWest(mPhiWgtFarWest);
  }
  pFlowEvent->SetPhiWeightFtpcFarEast(mPhiWgtFtpcFarEast);
  pFlowEvent->SetPhiWeightFtpcEast(mPhiWgtFtpcEast);
  pFlowEvent->SetPhiWeightFtpcWest(mPhiWgtFtpcWest);
  pFlowEvent->SetPhiWeightFtpcFarWest(mPhiWgtFtpcFarWest);
  pFlowEvent->SetFirstLastPoints();

  // Get Trigger information
  StL0Trigger* pTrigger = pEvent->l0Trigger();
  if (pTrigger) {
    pFlowEvent->SetL0TriggerWord(pTrigger->triggerWord());
  }

  // Get event id 
  pFlowEvent->SetEventID((Int_t)(pEvent->id()));
  pFlowEvent->SetRunID((Int_t)(pEvent->runId()));

  if (pEvent->runInfo()) {
    pFlowEvent->SetCenterOfMassEnergy(pEvent->runInfo()->centerOfMassEnergy());
    pFlowEvent->SetMagneticField(pEvent->runInfo()->magneticField());
    pFlowEvent->SetBeamMassNumberEast(pEvent->runInfo()->beamMassNumber(east));
    pFlowEvent->SetBeamMassNumberWest(pEvent->runInfo()->beamMassNumber(west));
  } else {
    gMessMgr->Info() << "FlowMaker: no Run Info, reverting to year 1 settings "
		     << endm;
    pFlowEvent->SetCenterOfMassEnergy(130);
    pFlowEvent->SetMagneticField(4.98);
    pFlowEvent->SetBeamMassNumberEast(197);
    pFlowEvent->SetBeamMassNumberWest(197);
  }

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
    for (UInt_t slat = 0; slat < CTB.numberOfSlats(); slat++) {
      for (UInt_t tray = 0; tray < CTB.numberOfTrays(); tray++) {
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
  pFlowEvent->SetUncorrNegMult(uncorrectedNumberOfNegativePrimaries(*pEvent));
  pFlowEvent->SetUncorrPosMult(uncorrectedNumberOfPositivePrimaries(*pEvent));

  // define functor for pid probability algorithm
  StuProbabilityPidAlgorithm uPid(*pEvent);

  // loop over tracks in StEvent
  int goodTracks    = 0;
  int goodTracksEta = 0;
  StTpcDedxPidAlgorithm tpcDedxAlgo;
  Float_t nSigma;
  Float_t dcaSigned;

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
      StThreeVectorD g = gTrack->geometry()->momentum(); // p of global track
      // calculate the number of tracks with positive flag & |eta| < 0.75
      if (fabs(p.pseudoRapidity()) < 0.75) goodTracksEta++;
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

	dcaSigned = CalcDcaSigned(vertex,gTrack);
	pFlowTrack->SetDcaSigned(dcaSigned);
	pFlowTrack->SetDca(pTrack->impactParameter());
	pFlowTrack->SetDcaGlobal(gTrack->impactParameter());
	pFlowTrack->SetChi2((Float_t)(pTrack->fitTraits().chi2()));
	pFlowTrack->SetFitPts(pTrack->fitTraits().numberOfFitPoints());
	pFlowTrack->SetMaxPts(pTrack->numberOfPossiblePoints());

	Double_t pathLength = gTrack->geometry()->helix().pathLength(vertex);
	StThreeVectorD distance = gTrack->geometry()->helix().at(pathLength);
	pFlowTrack->SetDcaGlobal3(distance - vertex);

	pFlowTrack->SetTrackLength(pTrack->length());
	pFlowTrack->SetNhits(pTrack->detectorInfo()->numberOfPoints(kTpcId));
	pFlowTrack->SetZFirstPoint(pTrack->detectorInfo()->firstPoint().z());
	pFlowTrack->SetZLastPoint(pTrack->detectorInfo()->lastPoint().z());
	
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

	pFlowTrack->SetTopologyMap(pTrack->topologyMap());
	
	// dE/dx
	StPtrVecTrackPidTraits traits = pTrack->pidTraits(kTpcId);
        unsigned int size = traits.size();
        if (size) {
	  StDedxPidTraits* pid;
	  for (unsigned int i = 0; i < traits.size(); i++) {
	    pid = dynamic_cast<StDedxPidTraits*>(traits[i]);
	    if (pid && pid->method() == kTruncatedMeanId) break;
	  }
	  assert(pid); 
	  pFlowTrack->SetDedx(pid->mean());
	  pFlowTrack->SetNdedxPts(pid->numberOfPoints());
        }

	// Probability pid
	//const StParticleDefinition* def = pTrack->pidTraits(uPid);
	(void*)pTrack->pidTraits(uPid);
	pFlowTrack->SetMostLikelihoodPID(uPid.mostLikelihoodParticleGeantID());
	pFlowTrack->SetMostLikelihoodProb(uPid.mostLikelihoodProbability());
	if (uPid.isExtrap()) pFlowTrack->SetExtrapTag(1); //merging area. 
	else pFlowTrack->SetExtrapTag(0); 
	if (pTrack->geometry()->charge() < 0) {
	  pFlowTrack->SetElectronPositronProb(uPid.beingElectronProb());
	  pFlowTrack->SetPionPlusMinusProb(uPid.beingPionMinusProb());
	  pFlowTrack->SetKaonPlusMinusProb(uPid.beingKaonMinusProb());
	  pFlowTrack->SetProtonPbarProb(uPid.beingAntiProtonProb());
	} else if (pTrack->geometry()->charge() > 0) {
	  pFlowTrack->SetElectronPositronProb(uPid.beingPositronProb());
	  pFlowTrack->SetPionPlusMinusProb(uPid.beingPionPlusProb());
	  pFlowTrack->SetKaonPlusMinusProb(uPid.beingKaonPlusProb());
	  pFlowTrack->SetProtonPbarProb(uPid.beingProtonProb());
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

  if (pFlowEvent->CenterOfMassEnergy() == 0.) { // year=1
    pFlowEvent->SetMultEta(goodTracksEta);
  } else {
    UInt_t rawMult = pFlowEvent->UncorrNegMult() + pFlowEvent->UncorrPosMult();
    pFlowEvent->SetMultEta(rawMult);
  }

  pFlowEvent->SetCentrality();

  (pFlowEvent->ProbPid()) ? pFlowEvent->SetPidsProb() : 
    pFlowEvent->SetPidsDeviant();

  pFlowEvent->TrackCollection()->random_shuffle();

  pFlowEvent->SetSelections();
  (pFlowEvent->EtaSubs()) ? pFlowEvent->MakeEtaSubEvents() :
    pFlowEvent->MakeSubEvents();

}

//----------------------------------------------------------------------

void StFlowMaker::FillFlowEvent(StHbtEvent* hbtEvent) {
  // For use with HBT Maker. By Randy Wells.

  if (Debug()) gMessMgr->Info() << "FlowMaker: FillFlowEvent(HbtEvent)" << endm;

  // Delete previous StFlowEvent
  if (pFlowEvent) delete pFlowEvent;
  pFlowEvent = NULL;
  // Instantiate a new StFlowEvent
  pFlowEvent = new StFlowEvent;

  cout << "Inside FlowMaker::FillFlowEvent(HbtEvent)..." << endl;

  // set phiweights
  if (mOnePhiWgt) {
    pFlowEvent->SetOnePhiWgt();
    pFlowEvent->SetPhiWeight(mPhiWgt);
  } else {
    if (mFirstLastPhiWgt) pFlowEvent->SetFirstLastPhiWgt();
    pFlowEvent->SetPhiWeightFarEast(mPhiWgtFarEast);
    pFlowEvent->SetPhiWeightEast(mPhiWgtEast);
    pFlowEvent->SetPhiWeightWest(mPhiWgtWest);
    pFlowEvent->SetPhiWeightFarWest(mPhiWgtFarWest);
  }
  pFlowEvent->SetPhiWeightFtpcFarEast(mPhiWgtFtpcFarEast);
  pFlowEvent->SetPhiWeightFtpcEast(mPhiWgtFtpcEast);
  pFlowEvent->SetPhiWeightFtpcWest(mPhiWgtFtpcWest);
  pFlowEvent->SetPhiWeightFtpcFarWest(mPhiWgtFtpcFarWest);

  // Event ID and Run ID
  // ????????
  // Primary Vertex
  pFlowEvent->SetVertexPos( hbtEvent->PrimVertPos() );
  // Triggers
  pFlowEvent->SetCTB( hbtEvent->CtbMult() );
  pFlowEvent->SetZDCe( hbtEvent->ZdcAdcEast() );
  pFlowEvent->SetZDCw( hbtEvent->ZdcAdcWest() );
  // Get initial multiplicity before TrackCuts
  UInt_t origMult = hbtEvent->NumberOfTracks();
  pFlowEvent->SetOrigMult(origMult);
  PR(origMult);
  // Fill track info
  int goodTracks    = 0;
  int goodTracksEta = 0;
  StHbtTrack* pParticle;
  StHbtTrackIterator pIter;
  StHbtTrackIterator startLoop = hbtEvent->TrackCollection()->begin();
  StHbtTrackIterator endLoop   = hbtEvent->TrackCollection()->end();
  for (pIter=startLoop; pIter!=endLoop; pIter++){
    pParticle = *pIter;
    // Instantiate new StFlowTrack
    StFlowTrack* pFlowTrack = new StFlowTrack;
    if (!pFlowTrack) return;
    double px = pParticle->P().x();
    double py = pParticle->P().y();
    double phi = atan2(py,px);
    if (phi<0.0) phi+=twopi;
    pFlowTrack->SetPhi( phi );
    pFlowTrack->SetPhiGlobal( phi );
    double pz = pParticle->P().z();
    double pTotal = pParticle->P().mag();
    double eta = 0.5*::log( (1.0+pz/pTotal)/(1.0-pz/pTotal) );
    pFlowTrack->SetEta( eta );
    pFlowTrack->SetEtaGlobal( eta );
    pFlowTrack->SetPt( pParticle->Pt() );
    pFlowTrack->SetPtGlobal( pParticle->Pt() );
    pFlowTrack->SetCharge( int(pParticle->Charge()) );
    double dcaXY = pParticle->DCAxy();
    double dcaZ = pParticle->DCAz();
    double dca = ::sqrt( dcaXY*dcaXY + dcaZ*dcaZ );
    //pFlowTrack->SetDca( dca );
    pFlowTrack->SetDcaGlobal( dca );
    pFlowTrack->SetChi2( pParticle->ChiSquaredXY() );
    pFlowTrack->SetFitPts( pParticle->NHits() );
    pFlowTrack->SetMaxPts( pParticle->NHitsPossible() );
    // PID
    pFlowTrack->SetPidPiPlus( pParticle->NSigmaPion() );
    pFlowTrack->SetPidPiMinus( pParticle->NSigmaPion() );
    pFlowTrack->SetPidProton( pParticle->NSigmaProton() );
    pFlowTrack->SetPidAntiProton( pParticle->NSigmaProton() );
    pFlowTrack->SetPidKaonMinus( pParticle->NSigmaKaon() );
    pFlowTrack->SetPidKaonPlus( pParticle->NSigmaKaon() );
    pFlowTrack->SetPidDeuteron( 0.0 );
    pFlowTrack->SetPidAntiDeuteron( 0.0 );
    pFlowTrack->SetPidElectron( pParticle->NSigmaElectron() );
    pFlowTrack->SetPidPositron( pParticle->NSigmaElectron() );
    // dEdx
    if ( pParticle->NSigmaKaon() > 2.0 ) {
      if (pParticle->Charge() > 0 ) {
	pFlowTrack->SetMostLikelihoodPID(14); // proton
	pFlowTrack->SetMostLikelihoodProb( 0.99 ); // guaranteed
      }
      else {
	pFlowTrack->SetMostLikelihoodPID(15); // anti-proton
	pFlowTrack->SetMostLikelihoodProb( 0.99 ); // guaranteed
      }
    }
    if ( pParticle->NSigmaPion() > 2.0 ) {
      if (pParticle->Charge() > 0 ) {
	pFlowTrack->SetMostLikelihoodPID(11); // kaon
	pFlowTrack->SetMostLikelihoodProb( 0.99 ); // guaranteed
      }
      else {
	pFlowTrack->SetMostLikelihoodPID(12); // anti-kaon
	pFlowTrack->SetMostLikelihoodProb( 0.99 ); // guaranteed
      }
    }
    if ( pParticle->NSigmaPion() < -2.0 ) {
      if (pParticle->Charge() < 0 ) {
	pFlowTrack->SetMostLikelihoodPID(3); // electron
	pFlowTrack->SetMostLikelihoodProb( 0.99 ); // guaranteed
      }
      else {
	pFlowTrack->SetMostLikelihoodPID(2); // positron
	pFlowTrack->SetMostLikelihoodProb( 0.99 ); // guaranteed
      }
    }
    pFlowTrack->SetExtrapTag(0); // none are in the PID merging area
    pFlowEvent->TrackCollection()->push_back(pFlowTrack);
    goodTracks++;
  }

  pFlowEvent->SetMultEta(goodTracksEta);
  pFlowEvent->SetCentrality();
  pFlowEvent->TrackCollection()->random_shuffle();
  pFlowEvent->SetSelections();
  pFlowEvent->MakeSubEvents();
}

//----------------------------------------------------------------------

void StFlowMaker::FillPicoEvent() {
  // Make StFlowPicoEvent from StFlowEvent

  if (Debug()) gMessMgr->Info() << "FlowMaker: FillPicoEvent()" << endm;

  if (!pPicoEvent) {
    gMessMgr->Warning("##### FlowMaker: No FlowPicoEvent");
    return;
  }
  StFlowPicoTrack* pFlowPicoTrack = new StFlowPicoTrack();
  
  pPicoEvent->SetVersion(6);         // version 6 
  pPicoEvent->SetEventID(pFlowEvent->EventID());
  pPicoEvent->SetRunID(pFlowEvent->RunID());
  pPicoEvent->SetL0TriggerWord(pFlowEvent->L0TriggerWord());

  pPicoEvent->SetCenterOfMassEnergy(pFlowEvent->CenterOfMassEnergy());
  pPicoEvent->SetMagneticField(pFlowEvent->MagneticField());
  pPicoEvent->SetBeamMassNumberEast(pFlowEvent->BeamMassNumberEast());
  pPicoEvent->SetBeamMassNumberWest(pFlowEvent->BeamMassNumberWest());

  pPicoEvent->SetOrigMult(pFlowEvent->OrigMult());
  pPicoEvent->SetUncorrNegMult(pFlowEvent->UncorrNegMult());
  pPicoEvent->SetUncorrPosMult(pFlowEvent->UncorrPosMult());
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
    pFlowPicoTrack->SetDcaSigned(pFlowTrack->DcaSigned());
    pFlowPicoTrack->SetDca(pFlowTrack->Dca());
    pFlowPicoTrack->SetDcaGlobal(pFlowTrack->DcaGlobal());
    pFlowPicoTrack->SetZFirstPoint(pFlowTrack->ZFirstPoint());
    pFlowPicoTrack->SetZLastPoint(pFlowTrack->ZLastPoint());
    pFlowPicoTrack->SetChi2(pFlowTrack->Chi2());
    pFlowPicoTrack->SetFitPts(pFlowTrack->FitPts());
    pFlowPicoTrack->SetMaxPts(pFlowTrack->MaxPts());
    pFlowPicoTrack->SetNhits(pFlowTrack->Nhits());
    pFlowPicoTrack->SetNdedxPts(pFlowTrack->NdedxPts());
    pFlowPicoTrack->SetDcaGlobal3(pFlowTrack->DcaGlobal3().x(),
				  pFlowTrack->DcaGlobal3().y(),
				  pFlowTrack->DcaGlobal3().z());
    pFlowPicoTrack->SetTrackLength(pFlowTrack->TrackLength());
    pFlowPicoTrack->SetMostLikelihoodPID(pFlowTrack->MostLikelihoodPID()); 
    pFlowPicoTrack->SetMostLikelihoodProb(pFlowTrack->MostLikelihoodProb());
    pFlowPicoTrack->SetExtrapTag(pFlowTrack->ExtrapTag());
    pFlowPicoTrack->SetElectronPositronProb(pFlowTrack->ElectronPositronProb());
    pFlowPicoTrack->SetPionPlusMinusProb(pFlowTrack->PionPlusMinusProb());
    pFlowPicoTrack->SetKaonPlusMinusProb(pFlowTrack->KaonPlusMinusProb());
    pFlowPicoTrack->SetProtonPbarProb(pFlowTrack->ProtonPbarProb());
    if (pFlowPicoTrack->Charge() > 0) {
      pFlowPicoTrack->SetPidPion(pFlowTrack->PidPiPlus());
      pFlowPicoTrack->SetPidProton(pFlowTrack->PidProton());
      pFlowPicoTrack->SetPidKaon(pFlowTrack->PidKaonPlus());
      pFlowPicoTrack->SetPidDeuteron(pFlowTrack->PidDeuteron());
      pFlowPicoTrack->SetPidElectron(pFlowTrack->PidPositron());
    } else {
      pFlowPicoTrack->SetPidPion(pFlowTrack->PidPiMinus());
      pFlowPicoTrack->SetPidProton(pFlowTrack->PidAntiProton());
      pFlowPicoTrack->SetPidKaon(pFlowTrack->PidKaonMinus());
      pFlowPicoTrack->SetPidDeuteron(pFlowTrack->PidAntiDeuteron());
      pFlowPicoTrack->SetPidElectron(pFlowTrack->PidElectron());
    }
    pFlowPicoTrack->SetTopologyMap(pFlowTrack->TopologyMap().data(0),
				   pFlowTrack->TopologyMap().data(1));
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
  if (Debug()) gMessMgr->Info() << "FlowMaker: Pico Version: " <<  
		 pPicoEvent->Version() << endm;

  if (!pPicoEvent || !pPicoChain->GetEntry(mEventCounter++)) {
    cout << "##### FlowMaker: no more events" << endl; 
    return kFALSE; 
  }
  
  // Set phi weights
  if (mOnePhiWgt) {
    pFlowEvent->SetOnePhiWgt();
    pFlowEvent->SetPhiWeight(mPhiWgt);
  } else {
    if (mFirstLastPhiWgt) pFlowEvent->SetFirstLastPhiWgt();
    pFlowEvent->SetPhiWeightFarEast(mPhiWgtFarEast);
    pFlowEvent->SetPhiWeightEast(mPhiWgtEast);
    pFlowEvent->SetPhiWeightWest(mPhiWgtWest);
    pFlowEvent->SetPhiWeightFarWest(mPhiWgtFarWest);
  }
  pFlowEvent->SetPhiWeightFtpcFarEast(mPhiWgtFtpcFarEast);
  pFlowEvent->SetPhiWeightFtpcEast(mPhiWgtFtpcEast);
  pFlowEvent->SetPhiWeightFtpcWest(mPhiWgtFtpcWest);
  pFlowEvent->SetPhiWeightFtpcFarWest(mPhiWgtFtpcFarWest);

  // Recalculate MultEta for year=2 old pico tapes
  if (pPicoEvent->CenterOfMassEnergy()) {
    UInt_t rawMult = pPicoEvent->UncorrNegMult() + pPicoEvent->UncorrPosMult();
    pPicoEvent->SetMultEta(rawMult);
  }
  
  // Check event cuts including centrality
  if (!StFlowCutEvent::CheckEvent(pPicoEvent)) {  
    Int_t eventID = pPicoEvent->EventID();
    gMessMgr->Info() << "##### FlowMaker: picoevent " << eventID 
  		     << " cut" << endm;
    delete pFlowEvent;             // delete this event
    pFlowEvent = NULL;
    return kTRUE;
  }

  switch (pPicoEvent->Version()) {
  case 6: FillFromPicoVersion6DST(pPicoEvent);
    break;
  case 5: FillFromPicoVersion5DST(pPicoEvent);
    break;
  case 4: FillFromPicoVersion4DST(pPicoEvent);
    break;
//   case 3: FillFromPicoVersion3DST(pPicoEvent);
//     break;
//   case 2: FillFromPicoVersion2DST(pPicoEvent);
//     break;
//   case 1: FillFromPicoVersion1DST(pPicoEvent);
//     break;
//   case 0: FillFromPicoVersion0DST(pPicoEvent);
//     break;
  default:
    cout << "##### FlowMaker: Illegal pico file version" << endl;
    return kStFatal;
  }
  
  // Check Eta Symmetry
  if (!StFlowCutEvent::CheckEtaSymmetry(pPicoEvent)) {  
    Int_t eventID = pPicoEvent->EventID();
    gMessMgr->Info() << "##### FlowMaker: picoevent " << eventID 
		     << " cut" << endm;
    delete pFlowEvent;             // delete this event
    pFlowEvent = NULL;
    return kTRUE;
  }
  
  (pFlowEvent->ProbPid()) ? pFlowEvent->SetPidsProb() : 
    pFlowEvent->SetPidsDeviant();

  pFlowEvent->TrackCollection()->random_shuffle();

  pFlowEvent->SetSelections();
  (pFlowEvent->EtaSubs()) ? pFlowEvent->MakeEtaSubEvents() :
    pFlowEvent->MakeSubEvents();
    
  return kTRUE;
}

//-----------------------------------------------------------------------

Bool_t StFlowMaker::FillFromPicoVersion4DST(StFlowPicoEvent* pPicoEvent) {
  // Make StFlowEvent from StFlowPicoEvent
  
  if (Debug()) gMessMgr->Info() << "FlowMaker: FillFromPicoVersion4DST()" << endm;

  StuProbabilityPidAlgorithm uPid;

  pFlowEvent->SetEventID(pPicoEvent->EventID());
  UInt_t origMult = pPicoEvent->OrigMult();
  pFlowEvent->SetOrigMult(origMult);
  PR(origMult);
  pFlowEvent->SetUncorrNegMult(pPicoEvent->UncorrNegMult());
  pFlowEvent->SetUncorrPosMult(pPicoEvent->UncorrPosMult());
  pFlowEvent->SetVertexPos(StThreeVectorF(pPicoEvent->VertexX(),
					  pPicoEvent->VertexY(),
					  pPicoEvent->VertexZ()) );
  pFlowEvent->SetMultEta(pPicoEvent->MultEta());
  pFlowEvent->SetCentrality();
  pFlowEvent->SetRunID(pPicoEvent->RunID());
  pFlowEvent->SetL0TriggerWord(pPicoEvent->L0TriggerWord());

  pFlowEvent->SetCenterOfMassEnergy(pPicoEvent->CenterOfMassEnergy());
  pFlowEvent->SetBeamMassNumberEast(pPicoEvent->BeamMassNumberEast());
  pFlowEvent->SetBeamMassNumberWest(pPicoEvent->BeamMassNumberWest());

  pFlowEvent->SetCTB(pPicoEvent->CTB());
  pFlowEvent->SetZDCe(pPicoEvent->ZDCe());
  pFlowEvent->SetZDCw(pPicoEvent->ZDCw());
  
  int goodTracks = 0;
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
      pFlowTrack->SetDcaSigned(pPicoTrack->DcaSigned());
      pFlowTrack->SetDca(pPicoTrack->Dca());
      pFlowTrack->SetDcaGlobal(pPicoTrack->DcaGlobal());
      pFlowTrack->SetChi2(pPicoTrack->Chi2());
      pFlowTrack->SetFitPts(pPicoTrack->FitPts());
      pFlowTrack->SetMaxPts(pPicoTrack->MaxPts());
      pFlowTrack->SetNhits(pPicoTrack->Nhits());
      pFlowTrack->SetNdedxPts(pPicoTrack->NdedxPts());
      pFlowTrack->SetDcaGlobal3(StThreeVectorD(pPicoTrack->DcaGlobalX(),
					       pPicoTrack->DcaGlobalY(),
					       pPicoTrack->DcaGlobalZ()) );
      pFlowTrack->SetTrackLength(pPicoTrack->TrackLength());

      if (StuProbabilityPidAlgorithm::isPIDTableRead()) {

  uPid.processPIDAsFunction(uPid.getCentrality(pPicoEvent->UncorrNegMult()),
                    pPicoTrack->DcaGlobal(),
                    pPicoTrack->Charge(),
  fabs((pPicoTrack->Pt()/sqrt(1-(tanh(pPicoTrack->Eta())*tanh(pPicoTrack->Eta()))))/float(pPicoTrack->Charge())),
                    pPicoTrack->Eta(),
                    pPicoTrack->NdedxPts(),
                    pPicoTrack->Dedx() );

      pFlowTrack->SetMostLikelihoodPID(uPid.mostLikelihoodParticleGeantID());
      pFlowTrack->SetMostLikelihoodProb(uPid.mostLikelihoodProbability());   
      pFlowTrack->SetExtrapTag(int(uPid.isExtrap()));                        

      pFlowTrack->SetElectronPositronProb( (pPicoTrack->Charge()>0) ?   
                                           float(uPid.beingPositronProb()) :
                                           float(uPid.beingElectronProb()) );

      pFlowTrack->SetPionPlusMinusProb( (pPicoTrack->Charge()>0) ?  
                                        float(uPid.beingPionPlusProb()) :
                                        float(uPid.beingPionMinusProb()) );

      pFlowTrack->SetKaonPlusMinusProb( (pPicoTrack->Charge()>0) ? 
                                        float(uPid.beingKaonPlusProb()) :
                                        float(uPid.beingKaonMinusProb()) );

      pFlowTrack->SetProtonPbarProb( (pPicoTrack->Charge()>0) ? 
                                     float(uPid.beingProtonProb()) :
                                     float(uPid.beingAntiProtonProb()) );

      } else {
      pFlowTrack->SetMostLikelihoodPID(pPicoTrack->MostLikelihoodPID()); 
      pFlowTrack->SetMostLikelihoodProb(pPicoTrack->MostLikelihoodProb());
      pFlowTrack->SetExtrapTag(pPicoTrack->ExtrapTag());
      pFlowTrack->SetElectronPositronProb(pPicoTrack->ElectronPositronProb()); 
      pFlowTrack->SetPionPlusMinusProb(pPicoTrack->PionPlusMinusProb()); 
      pFlowTrack->SetKaonPlusMinusProb(pPicoTrack->KaonPlusMinusProb()); 
      pFlowTrack->SetProtonPbarProb(pPicoTrack->ProtonPbarProb()); 
      }

      if (pPicoTrack->Charge() < 0) {
	pFlowTrack->SetPidPiMinus(pPicoTrack->PidPion());
	pFlowTrack->SetPidAntiProton(pPicoTrack->PidProton());
	pFlowTrack->SetPidKaonMinus(pPicoTrack->PidKaon());
	pFlowTrack->SetPidAntiDeuteron(pPicoTrack->PidDeuteron());
	pFlowTrack->SetPidElectron(pPicoTrack->PidElectron());
      } else {
	pFlowTrack->SetPidPiPlus(pPicoTrack->PidPion());
	pFlowTrack->SetPidProton(pPicoTrack->PidProton());
	pFlowTrack->SetPidKaonPlus(pPicoTrack->PidKaon());
	pFlowTrack->SetPidDeuteron(pPicoTrack->PidDeuteron());
	pFlowTrack->SetPidPositron(pPicoTrack->PidElectron());
      }

      if (pPicoTrack->TopologyMap0() || pPicoTrack->TopologyMap1()) {
	// topology map found
	pFlowTrack->SetTopologyMap(StTrackTopologyMap(pPicoTrack->TopologyMap0(),
						      pPicoTrack->TopologyMap1()) );
      }

      pFlowEvent->TrackCollection()->push_back(pFlowTrack);
      goodTracks++;
    }
  }
  
  return kTRUE;
}

//-----------------------------------------------------------------------

Bool_t StFlowMaker::FillFromPicoVersion5DST(StFlowPicoEvent* pPicoEvent) {
  // Make StFlowEvent from StFlowPicoEvent
  
  if (Debug()) gMessMgr->Info() << "FlowMaker: FillFromPicoVersion5DST()" << endm;

  StuProbabilityPidAlgorithm uPid;

  pFlowEvent->SetEventID(pPicoEvent->EventID());
  UInt_t origMult = pPicoEvent->OrigMult();
  pFlowEvent->SetOrigMult(origMult);
  PR(origMult);
  pFlowEvent->SetUncorrNegMult(pPicoEvent->UncorrNegMult());
  pFlowEvent->SetUncorrPosMult(pPicoEvent->UncorrPosMult());
  pFlowEvent->SetVertexPos(StThreeVectorF(pPicoEvent->VertexX(),
					  pPicoEvent->VertexY(),
					  pPicoEvent->VertexZ()) );

  pFlowEvent->SetRunID(pPicoEvent->RunID());
  pFlowEvent->SetL0TriggerWord(pPicoEvent->L0TriggerWord());

  pFlowEvent->SetCenterOfMassEnergy(pPicoEvent->CenterOfMassEnergy());
  pFlowEvent->SetMagneticField(pPicoEvent->MagneticField());
  pFlowEvent->SetBeamMassNumberEast(pPicoEvent->BeamMassNumberEast());
  pFlowEvent->SetBeamMassNumberWest(pPicoEvent->BeamMassNumberWest());
  pFlowEvent->SetMultEta(pPicoEvent->MultEta());
  pFlowEvent->SetCentrality();

  pFlowEvent->SetCTB(pPicoEvent->CTB());
  pFlowEvent->SetZDCe(pPicoEvent->ZDCe());
  pFlowEvent->SetZDCw(pPicoEvent->ZDCw());
  
  int goodTracks = 0;
  // Fill FlowTracks
  for (Int_t nt=0; nt < pPicoEvent->GetNtrack(); nt++) {
    StFlowPicoTrack* pPicoTrack = (StFlowPicoTrack*)pPicoEvent->Tracks()
      ->UncheckedAt(nt);
    if (pPicoTrack &&  (pPicoTrack->Dca())!=(pPicoTrack->DcaGlobal())
	&& StFlowCutTrack::CheckTrack(pPicoTrack)) {
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
      pFlowTrack->SetDcaSigned(pPicoTrack->DcaSigned());
      pFlowTrack->SetDca(pPicoTrack->Dca());
      pFlowTrack->SetDcaGlobal(pPicoTrack->DcaGlobal());
      pFlowTrack->SetChi2(pPicoTrack->Chi2());
      pFlowTrack->SetFitPts(pPicoTrack->FitPts());
      pFlowTrack->SetMaxPts(pPicoTrack->MaxPts());
      pFlowTrack->SetNhits(pPicoTrack->Nhits());
      pFlowTrack->SetNdedxPts(pPicoTrack->NdedxPts());
      pFlowTrack->SetDcaGlobal3(StThreeVectorD(pPicoTrack->DcaGlobalX(),
					       pPicoTrack->DcaGlobalY(),
					       pPicoTrack->DcaGlobalZ()) );
      pFlowTrack->SetTrackLength(pPicoTrack->TrackLength());

      if (StuProbabilityPidAlgorithm::isPIDTableRead()) {

  uPid.processPIDAsFunction(uPid.getCentrality(pPicoEvent->UncorrNegMult()),
                    pPicoTrack->DcaGlobal(),
                    pPicoTrack->Charge(),
  fabs((pPicoTrack->Pt()/sqrt(1-(tanh(pPicoTrack->Eta())*tanh(pPicoTrack->Eta()))))/float(pPicoTrack->Charge())),
                    pPicoTrack->Eta(),
                    pPicoTrack->NdedxPts(),
                    pPicoTrack->Dedx() );

      pFlowTrack->SetMostLikelihoodPID(uPid.mostLikelihoodParticleGeantID());
      pFlowTrack->SetMostLikelihoodProb(uPid.mostLikelihoodProbability());   
      pFlowTrack->SetExtrapTag(int(uPid.isExtrap()));                        

      pFlowTrack->SetElectronPositronProb( (pPicoTrack->Charge()>0) ?   
                                           float(uPid.beingPositronProb()) :
                                           float(uPid.beingElectronProb()) );

      pFlowTrack->SetPionPlusMinusProb( (pPicoTrack->Charge()>0) ?  
                                        float(uPid.beingPionPlusProb()) :
                                        float(uPid.beingPionMinusProb()) );

      pFlowTrack->SetKaonPlusMinusProb( (pPicoTrack->Charge()>0) ? 
                                        float(uPid.beingKaonPlusProb()) :
                                        float(uPid.beingKaonMinusProb()) );

      pFlowTrack->SetProtonPbarProb( (pPicoTrack->Charge()>0) ? 
                                     float(uPid.beingProtonProb()) :
                                     float(uPid.beingAntiProtonProb()) );

      } else {
      pFlowTrack->SetMostLikelihoodPID(pPicoTrack->MostLikelihoodPID()); 
      pFlowTrack->SetMostLikelihoodProb(pPicoTrack->MostLikelihoodProb());
      pFlowTrack->SetExtrapTag(pPicoTrack->ExtrapTag());
      pFlowTrack->SetElectronPositronProb(pPicoTrack->ElectronPositronProb()); 
      pFlowTrack->SetPionPlusMinusProb(pPicoTrack->PionPlusMinusProb()); 
      pFlowTrack->SetKaonPlusMinusProb(pPicoTrack->KaonPlusMinusProb()); 
      pFlowTrack->SetProtonPbarProb(pPicoTrack->ProtonPbarProb()); 
      }

      if (pPicoTrack->Charge() < 0) {
	pFlowTrack->SetPidPiMinus(pPicoTrack->PidPion());
	pFlowTrack->SetPidAntiProton(pPicoTrack->PidProton());
	pFlowTrack->SetPidKaonMinus(pPicoTrack->PidKaon());
	pFlowTrack->SetPidAntiDeuteron(pPicoTrack->PidDeuteron());
	pFlowTrack->SetPidElectron(pPicoTrack->PidElectron());
      } else {
	pFlowTrack->SetPidPiPlus(pPicoTrack->PidPion());
	pFlowTrack->SetPidProton(pPicoTrack->PidProton());
	pFlowTrack->SetPidKaonPlus(pPicoTrack->PidKaon());
	pFlowTrack->SetPidDeuteron(pPicoTrack->PidDeuteron());
	pFlowTrack->SetPidPositron(pPicoTrack->PidElectron());
      }

      if (pPicoTrack->TopologyMap0() || pPicoTrack->TopologyMap1()) {
	// topology map found
	pFlowTrack->SetTopologyMap(StTrackTopologyMap(pPicoTrack->TopologyMap0(),
						      pPicoTrack->TopologyMap1()) );
      }

      pFlowEvent->TrackCollection()->push_back(pFlowTrack);
      goodTracks++;
    }
  }
  
  return kTRUE;
}

//-----------------------------------------------------------------------

Bool_t StFlowMaker::FillFromPicoVersion6DST(StFlowPicoEvent* pPicoEvent) {
  // Make StFlowEvent from StFlowPicoEvent

  StuProbabilityPidAlgorithm uPid;
  
  if (Debug()) gMessMgr->Info() << "FlowMaker: FillFromPicoVersion6DST()" << endm;

  pFlowEvent->SetFirstLastPoints();  
  pFlowEvent->SetEventID(pPicoEvent->EventID());
  UInt_t origMult = pPicoEvent->OrigMult();
  pFlowEvent->SetOrigMult(origMult);
  PR(origMult);
  pFlowEvent->SetUncorrNegMult(pPicoEvent->UncorrNegMult());
  pFlowEvent->SetUncorrPosMult(pPicoEvent->UncorrPosMult());
  pFlowEvent->SetVertexPos(StThreeVectorF(pPicoEvent->VertexX(),
					  pPicoEvent->VertexY(),
					  pPicoEvent->VertexZ()) );

  pFlowEvent->SetRunID(pPicoEvent->RunID());
  pFlowEvent->SetL0TriggerWord(pPicoEvent->L0TriggerWord());

  pFlowEvent->SetCenterOfMassEnergy(pPicoEvent->CenterOfMassEnergy());
  pFlowEvent->SetMagneticField(pPicoEvent->MagneticField());
  pFlowEvent->SetBeamMassNumberEast(pPicoEvent->BeamMassNumberEast());
  pFlowEvent->SetBeamMassNumberWest(pPicoEvent->BeamMassNumberWest());
  pFlowEvent->SetMultEta(pPicoEvent->MultEta());
  pFlowEvent->SetCentrality();

  pFlowEvent->SetCTB(pPicoEvent->CTB());
  pFlowEvent->SetZDCe(pPicoEvent->ZDCe());
  pFlowEvent->SetZDCw(pPicoEvent->ZDCw());
  
  int goodTracks = 0;
  // Fill FlowTracks
  for (Int_t nt=0; nt < pPicoEvent->GetNtrack(); nt++) {
    StFlowPicoTrack* pPicoTrack = (StFlowPicoTrack*)pPicoEvent->Tracks()
      ->UncheckedAt(nt);
    if (pPicoTrack &&  (pPicoTrack->Dca())!=(pPicoTrack->DcaGlobal())
	&& StFlowCutTrack::CheckTrack(pPicoTrack)) {
      // Instantiate new StFlowTrack
      StFlowTrack* pFlowTrack = new StFlowTrack;
      if (!pFlowTrack) return kFALSE;
      pFlowTrack->SetPt(pPicoTrack->Pt());
      pFlowTrack->SetPtGlobal(pPicoTrack->PtGlobal());
      pFlowTrack->SetPhi(pPicoTrack->Phi());
      pFlowTrack->SetPhiGlobal(pPicoTrack->PhiGlobal());
      pFlowTrack->SetEta(pPicoTrack->Eta());
      pFlowTrack->SetEtaGlobal(pPicoTrack->EtaGlobal());
      pFlowTrack->SetZFirstPoint(pPicoTrack->ZFirstPoint());
      pFlowTrack->SetZLastPoint(pPicoTrack->ZLastPoint());
      pFlowTrack->SetDedx(pPicoTrack->Dedx());
      pFlowTrack->SetCharge(pPicoTrack->Charge());
      pFlowTrack->SetDcaSigned(pPicoTrack->DcaSigned());
      pFlowTrack->SetDca(pPicoTrack->Dca());
      pFlowTrack->SetDcaGlobal(pPicoTrack->DcaGlobal());
      pFlowTrack->SetChi2(pPicoTrack->Chi2());
      pFlowTrack->SetFitPts(pPicoTrack->FitPts());
      pFlowTrack->SetMaxPts(pPicoTrack->MaxPts());
      pFlowTrack->SetNhits(pPicoTrack->Nhits());
      pFlowTrack->SetNdedxPts(pPicoTrack->NdedxPts());
      pFlowTrack->SetDcaGlobal3(StThreeVectorD(pPicoTrack->DcaGlobalX(),
					       pPicoTrack->DcaGlobalY(),
					       pPicoTrack->DcaGlobalZ()) );
      pFlowTrack->SetTrackLength(pPicoTrack->TrackLength());

      if (StuProbabilityPidAlgorithm::isPIDTableRead()) {

  uPid.processPIDAsFunction(uPid.getCentrality(pPicoEvent->UncorrNegMult()),
                    pPicoTrack->DcaGlobal(),
                    pPicoTrack->Charge(),
  fabs((pPicoTrack->Pt()/sqrt(1-(tanh(pPicoTrack->Eta())*tanh(pPicoTrack->Eta()))))/float(pPicoTrack->Charge())),
                    pPicoTrack->Eta(),
                    pPicoTrack->NdedxPts(),
                    pPicoTrack->Dedx() );

      pFlowTrack->SetMostLikelihoodPID(uPid.mostLikelihoodParticleGeantID());
      pFlowTrack->SetMostLikelihoodProb(uPid.mostLikelihoodProbability());   
      pFlowTrack->SetExtrapTag(int(uPid.isExtrap()));                        

      pFlowTrack->SetElectronPositronProb( (pPicoTrack->Charge()>0) ?   
                                           float(uPid.beingPositronProb()) :
                                           float(uPid.beingElectronProb()) );

      pFlowTrack->SetPionPlusMinusProb( (pPicoTrack->Charge()>0) ?  
                                        float(uPid.beingPionPlusProb()) :
                                        float(uPid.beingPionMinusProb()) );

      pFlowTrack->SetKaonPlusMinusProb( (pPicoTrack->Charge()>0) ? 
                                        float(uPid.beingKaonPlusProb()) :
                                        float(uPid.beingKaonMinusProb()) );

      pFlowTrack->SetProtonPbarProb( (pPicoTrack->Charge()>0) ? 
                                     float(uPid.beingProtonProb()) :
                                     float(uPid.beingAntiProtonProb()) );

      } else {
      pFlowTrack->SetMostLikelihoodPID(pPicoTrack->MostLikelihoodPID()); 
      pFlowTrack->SetMostLikelihoodProb(pPicoTrack->MostLikelihoodProb());
      pFlowTrack->SetExtrapTag(pPicoTrack->ExtrapTag());
      pFlowTrack->SetElectronPositronProb(pPicoTrack->ElectronPositronProb()); 
      pFlowTrack->SetPionPlusMinusProb(pPicoTrack->PionPlusMinusProb()); 
      pFlowTrack->SetKaonPlusMinusProb(pPicoTrack->KaonPlusMinusProb()); 
      pFlowTrack->SetProtonPbarProb(pPicoTrack->ProtonPbarProb()); 
      }

      if (pPicoTrack->Charge() < 0) {
	pFlowTrack->SetPidPiMinus(pPicoTrack->PidPion());
	pFlowTrack->SetPidAntiProton(pPicoTrack->PidProton());
	pFlowTrack->SetPidKaonMinus(pPicoTrack->PidKaon());
	pFlowTrack->SetPidAntiDeuteron(pPicoTrack->PidDeuteron());
	pFlowTrack->SetPidElectron(pPicoTrack->PidElectron());
      } else {
	pFlowTrack->SetPidPiPlus(pPicoTrack->PidPion());
	pFlowTrack->SetPidProton(pPicoTrack->PidProton());
	pFlowTrack->SetPidKaonPlus(pPicoTrack->PidKaon());
	pFlowTrack->SetPidDeuteron(pPicoTrack->PidDeuteron());
	pFlowTrack->SetPidPositron(pPicoTrack->PidElectron());
      }

      if (pPicoTrack->TopologyMap0() || pPicoTrack->TopologyMap1()) {
	// topology map found
	pFlowTrack->SetTopologyMap(StTrackTopologyMap(pPicoTrack->TopologyMap0(),
						      pPicoTrack->TopologyMap1()) );
      }

      pFlowEvent->TrackCollection()->push_back(pFlowTrack);
      goodTracks++;
    }
  }
  
  return kTRUE;
}

//-----------------------------------------------------------------------

Bool_t StFlowMaker::FillFromMuDST() {
  // Make StFlowEvent from StMuDST

  if (Debug()) gMessMgr->Info() << "FlowMaker: FillFromMuDST()" << endm;
  
  pMuEvents->Clear();
  pMuTracks->Clear();
  pMuGlobalTracks->Clear();
      
  if (!pMuChain->GetEntry(mEventCounter++)) {
    cout << "##### FlowMaker: no more events" << endl; 
    return kFALSE; 
  }

  pMuEvent=(StMuEvent*)pMuEvents->UncheckedAt(0);

  // Set phi weights
  if (mOnePhiWgt) {
    pFlowEvent->SetOnePhiWgt();
    pFlowEvent->SetPhiWeight(mPhiWgt);
  } else {
    if (mFirstLastPhiWgt) pFlowEvent->SetFirstLastPhiWgt();
    pFlowEvent->SetPhiWeightFarEast(mPhiWgtFarEast);
    pFlowEvent->SetPhiWeightEast(mPhiWgtEast);
    pFlowEvent->SetPhiWeightWest(mPhiWgtWest);
    pFlowEvent->SetPhiWeightFarWest(mPhiWgtFarWest);
  }
  pFlowEvent->SetPhiWeightFtpcFarEast(mPhiWgtFtpcFarEast);
  pFlowEvent->SetPhiWeightFtpcEast(mPhiWgtFtpcEast);
  pFlowEvent->SetPhiWeightFtpcWest(mPhiWgtFtpcWest);
  pFlowEvent->SetPhiWeightFtpcFarWest(mPhiWgtFtpcFarWest);
  pFlowEvent->SetFirstLastPoints();  
  
  // Check event cuts
  if (!StFlowCutEvent::CheckEvent(pMuEvent)) {  
    Int_t eventID = pMuEvent->eventId();
    gMessMgr->Info() << "##### FlowMaker: MuEvent " << eventID 
                     << " cut" << endm;
    delete pFlowEvent;             // delete this event
    pFlowEvent = NULL;
    return kTRUE;
  }

  FillFromMuVersion0DST();

  // Check Eta Symmetry
  if (!StFlowCutEvent::CheckEtaSymmetry(pMuEvent)) {  
    Int_t eventID = pMuEvent->eventId();
    gMessMgr->Info() << "##### FlowMaker: MuEvent " << eventID 
                     << " cut" << endm;
    delete pFlowEvent;             // delete this event
    pFlowEvent = NULL;
    return kTRUE;
  }
  
  (pFlowEvent->ProbPid()) ? pFlowEvent->SetPidsProb() : 
    pFlowEvent->SetPidsDeviant();

  pFlowEvent->TrackCollection()->random_shuffle();

  pFlowEvent->SetSelections();
  (pFlowEvent->EtaSubs()) ? pFlowEvent->MakeEtaSubEvents() :
    pFlowEvent->MakeSubEvents();
  
  return kTRUE;
}

//-----------------------------------------------------------------------

Bool_t StFlowMaker::FillFromMuVersion0DST() {
  // Make StFlowEvent from StMuEvent
  
  if (Debug()) gMessMgr->Info() << "FlowMaker: FillFromMuVersion0DST()" << endm;

  StuProbabilityPidAlgorithm uPid;

  pFlowEvent->SetEventID(pMuEvent->eventId());
  pFlowEvent->SetVertexPos(pMuEvent->primaryVertexPosition());
  pFlowEvent->SetRunID(pMuEvent->runId());
  pFlowEvent->SetL0TriggerWord(pMuEvent->l0Trigger().triggerWord());
  pFlowEvent->SetCenterOfMassEnergy(pMuEvent->runInfo().centerOfMassEnergy());
  pFlowEvent->SetMagneticField(pMuEvent->runInfo().magneticField());
  pFlowEvent->SetBeamMassNumberEast(pMuEvent->runInfo().beamMassNumber(east));
  pFlowEvent->SetBeamMassNumberWest(pMuEvent->runInfo().beamMassNumber(west));
  pFlowEvent->SetCTB(pMuEvent->ctbMultiplicity());
  pFlowEvent->SetZDCe(pMuEvent->zdcAdcAttentuatedSumEast());
  pFlowEvent->SetZDCw(pMuEvent->zdcAdcAttentuatedSumWest());
    UInt_t origMult = pMuEvent->eventSummary().numberOfGoodPrimaryTracks(); //???
  pFlowEvent->SetOrigMult(origMult);
  PR(origMult);
  pFlowEvent->SetUncorrNegMult(pMuEvent->refMultNeg());
  pFlowEvent->SetUncorrPosMult(pMuEvent->refMultPos());
  pFlowEvent->SetMultEta(pMuEvent->refMult()); 
  pFlowEvent->SetCentrality(); 
  
  int goodTracks = 0;
  // Fill FlowTracks
  for (Int_t nt=0; nt < pMuTracks->GetEntries(); nt++) {
    StMuTrack* pMuTrack = (StMuTrack*)pMuTracks->UncheckedAt(nt);
    if (pMuTrack && pMuTrack->flag()>0 && StFlowCutTrack::CheckTrack(pMuTrack)) {
      // Instantiate new StFlowTrack
      StFlowTrack* pFlowTrack = new StFlowTrack;
      if (!pFlowTrack) return kFALSE;
      pFlowTrack->SetPt(pMuTrack->pt());
      StMuTrack* pMuGlobalTrack = (StMuTrack*)pMuGlobalTracks->UncheckedAt(pMuTrack->index2Global());
      if(!pMuGlobalTrack) {
        gMessMgr->Info() << "FlowMaker: FillFromMuVersion0DST(): WARNING! primary track has no reference to global track" << endl;
        continue;
      }
      pFlowTrack->SetPtGlobal(pMuGlobalTrack->pt());
      pFlowTrack->SetPhi(pMuTrack->phi());
      pFlowTrack->SetPhiGlobal(pMuGlobalTrack->phi());
      pFlowTrack->SetEta(pMuTrack->eta());
      pFlowTrack->SetEtaGlobal(pMuGlobalTrack->eta());
      pFlowTrack->SetZFirstPoint(pMuTrack->firstPoint().z());
      pFlowTrack->SetZLastPoint(pMuTrack->lastPoint().z());
      pFlowTrack->SetDedx(pMuTrack->dEdx());
      pFlowTrack->SetCharge(pMuTrack->charge());
      pFlowTrack->SetDcaSigned(CalcDcaSigned(pMuEvent->primaryVertexPosition(),pMuTrack->helix()));
      pFlowTrack->SetDca(pMuTrack->dca().mag());
      pFlowTrack->SetDcaGlobal(pMuTrack->dcaGlobal().mag());
      pFlowTrack->SetChi2(pMuTrack->chi2xy()); 
      pFlowTrack->SetFitPts(pMuTrack->nHitsFit());
      pFlowTrack->SetMaxPts(pMuTrack->nHitsPoss()); 
      pFlowTrack->SetNhits(pMuTrack->nHits());
      pFlowTrack->SetNdedxPts(pMuTrack->nHitsDedx());
      pFlowTrack->SetDcaGlobal3(pMuTrack->dcaGlobal());
      pFlowTrack->SetTrackLength(pMuTrack->helix().pathLength(pMuEvent->primaryVertexPosition())); //???

      if (StuProbabilityPidAlgorithm::isPIDTableRead()) {

  uPid.processPIDAsFunction(uPid.getCentrality(pMuEvent->refMultNeg()),
		    pMuTrack->dcaGlobal().mag(),
		    pMuTrack->charge(),
                    fabs((pMuTrack->p().mag())/float(pMuTrack->charge())),
		    pMuTrack->eta(),
		    pMuTrack->nHitsDedx(),
		    pMuTrack->dEdx() );

      pFlowTrack->SetMostLikelihoodPID(uPid.mostLikelihoodParticleGeantID());
      pFlowTrack->SetMostLikelihoodProb(uPid.mostLikelihoodProbability());   
      pFlowTrack->SetExtrapTag(int(uPid.isExtrap()));                        

      pFlowTrack->SetElectronPositronProb( (pMuTrack->charge()>0) ?   
                                           float(uPid.beingPositronProb()) :
                                           float(uPid.beingElectronProb()) );

      pFlowTrack->SetPionPlusMinusProb( (pMuTrack->charge()>0) ?  
                                        float(uPid.beingPionPlusProb()) :
                                        float(uPid.beingPionMinusProb()) );

      pFlowTrack->SetKaonPlusMinusProb( (pMuTrack->charge()>0) ? 
                                        float(uPid.beingKaonPlusProb()) :
                                        float(uPid.beingKaonMinusProb()) );

      pFlowTrack->SetProtonPbarProb( (pMuTrack->charge()>0) ? 
                                     float(uPid.beingProtonProb()) :
                                     float(uPid.beingAntiProtonProb()) );

      } else {

      pFlowTrack->SetElectronPositronProb(pMuTrack->pidProbElectron()); 
      pFlowTrack->SetPionPlusMinusProb(pMuTrack->pidProbPion()); 
      pFlowTrack->SetKaonPlusMinusProb(pMuTrack->pidProbKaon()); 
      pFlowTrack->SetProtonPbarProb(pMuTrack->pidProbProton()); 
      pFlowTrack->SetProtonPbarProb(pMuTrack->pidProbProton()); 

      // pFlowTrack->SetExtrapTag(pPicoTrack->ExtrapTag());

          
      int mostLikelihoodPid=0;
      double mostLikelihoodPidProbability=0;
      if (pFlowTrack->ElectronPositronProb()>mostLikelihoodPidProbability) {
        mostLikelihoodPidProbability=pFlowTrack->ElectronPositronProb();
        if (pMuTrack->charge() < 0) 
          mostLikelihoodPid=3;
        else
          mostLikelihoodPid=2;
      }
      if (pFlowTrack->PionPlusMinusProb()>mostLikelihoodPidProbability) {
        mostLikelihoodPidProbability=pFlowTrack->PionPlusMinusProb();
        if (pMuTrack->charge() < 0) 
          mostLikelihoodPid=9;
        else
          mostLikelihoodPid=8;
      }
      if (pFlowTrack->KaonPlusMinusProb()>mostLikelihoodPidProbability) {
        mostLikelihoodPidProbability=pFlowTrack->KaonPlusMinusProb();
        if (pMuTrack->charge() < 0) 
          mostLikelihoodPid=12;
        else
          mostLikelihoodPid=11;
      }
      if (pFlowTrack->ProtonPbarProb()>mostLikelihoodPidProbability) {
        mostLikelihoodPidProbability=pFlowTrack->ProtonPbarProb();
        if (pMuTrack->charge() < 0) 
          mostLikelihoodPid=15;
        else
          mostLikelihoodPid=14;
      }
      
      pFlowTrack->SetMostLikelihoodPID(mostLikelihoodPid); 
      pFlowTrack->SetMostLikelihoodProb(mostLikelihoodPidProbability);

      }    


      if (pMuTrack->charge() < 0) {
        pFlowTrack->SetPidPiMinus(pMuTrack->nSigmaPion()); 
        pFlowTrack->SetPidAntiProton(pMuTrack->nSigmaProton());
        pFlowTrack->SetPidKaonMinus(pMuTrack->nSigmaKaon());
	pFlowTrack->SetPidAntiDeuteron( 999.0 );
        pFlowTrack->SetPidElectron(pMuTrack->nSigmaElectron());
      } else {
        pFlowTrack->SetPidPiPlus(pMuTrack->nSigmaPion()); 
        pFlowTrack->SetPidProton(pMuTrack->nSigmaProton()); 
        pFlowTrack->SetPidKaonPlus(pMuTrack->nSigmaKaon()); 
	pFlowTrack->SetPidDeuteron( 999.0 );
        pFlowTrack->SetPidPositron(pMuTrack->nSigmaElectron());
      }





      pFlowTrack->SetTopologyMap(pMuTrack->topologyMap());
    
      pFlowEvent->TrackCollection()->push_back(pFlowTrack);
      goodTracks++;
      
    }
  }
  
  return kTRUE;
}

//-----------------------------------------------------------------------

void StFlowMaker::PrintSubeventMults() {
  // Used for testing

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
  
  Int_t split   = 99;      // by default split Event into sub branches
  Int_t comp    = 1;       // by default file is compressed
  Int_t bufsize = 256000;
  if (split) bufsize /= 16;
  
  // creat a Picoevent and an output file
  pPicoEvent = new StFlowPicoEvent();   

  TString* filestring = new TString(mPicoEventDir);
  filestring->Append(mEventFileName);
  if (filestring->EndsWith(".event.root")) {
    int nameLength = filestring->Length();
    filestring->Remove(nameLength - 11);
  }
  if (filestring->EndsWith(".MuDst.root")) {
    int nameLength = filestring->Length();
    filestring->Remove(nameLength - 11);
  }
  //cout << filestring->Data() << endl;
  filestring->Append(".flowpicoevent.root");
  pPicoDST = new TFile(filestring->Data(),"RECREATE","Flow Pico DST file");
  if (!pPicoDST) {
    cout << "##### FlowMaker: Warning: no PicoEvents file = " 
	 << filestring->Data() << endl;
    return kStFatal;
  }
  pPicoDST->SetCompressionLevel(comp);
//   gMessMgr->Info() << "##### FlowMaker: PicoEvents file = " 
// 		   << filestring->Data() << endm;
  
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
  if (Debug()) gMessMgr->Info() << "##### FlowMaker: events in Pico-DST file = "
		   << nEntries << endm;
  
  mEventCounter = 0;
  
  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::InitMuEventRead() {

  if (Debug()) gMessMgr->Info() << "FlowMaker: InitMuEventRead()" << endm;
  
  pMuEvents = new TClonesArray("StMuEvent", 1); 
  pMuTracks = new TClonesArray("StMuTrack", 10000);
  pMuGlobalTracks = new TClonesArray("StMuTrack", 10000);

  pMuChain = new TChain("MuDst");
  
  for (Int_t ilist = 0;  ilist < pMuFileList->GetNBundles(); ilist++) {
    pMuFileList->GetNextBundle();

    //**************  this block is to remove files with # evts < 5
    // if there is only one event in a file, the job will crash
    TChain* pTempChain = new TChain("MuDst");
    pTempChain->Add(pMuFileList->GetFileName(0));
    if (((Int_t)pTempChain->GetEntries()) > 5 ) 
    pMuChain->Add(pMuFileList->GetFileName(0));
    if (pTempChain) {delete pTempChain; pTempChain=0;}
    //**************  end of the block   

//     if (Debug()) gMessMgr->Info() << " doFlowEvents -  input fileList = " 
// 				  << pMuFileList->GetFileName(0) << endm;

      
    pMuChain->SetBranchAddress("MuEvent", &pMuEvents);
    pMuChain->SetBranchAddress("PrimaryTracks", &pMuTracks);
    pMuChain->SetBranchAddress("GlobalTracks", &pMuGlobalTracks);

    pMuChain->SetBranchStatus("*",0);
    pMuChain->SetBranchStatus("MuEvent*",1);
    pMuChain->SetBranchStatus("PrimaryTracks*",1);
    pMuChain->SetBranchStatus("GlobalTracks.mPt",1);
    pMuChain->SetBranchStatus("GlobalTracks.mPhi",1);
    pMuChain->SetBranchStatus("GlobalTracks.mEta",1);

    Int_t nEntries = (Int_t)pMuChain->GetEntries(); 
//     if (Debug()) gMessMgr->Info() << "##### FlowMaker: events in Mu-DST chain = "
//                      << nEntries << endm;
    gMessMgr->Info() << "### ## FlowMaker: " << pMuFileList->GetFileName(0)
				  << " " << nEntries << " events" << endm;
    
  }
  
  mEventCounter = 0;
  
  return kStOK;
}

//-----------------------------------------------------------------------

Float_t StFlowMaker::CalcDcaSigned(const StThreeVectorF vertex, 
				   const StTrack* gTrack) {
  // find the distance between the center of the circle and vertex.
  // if the radius of curvature > distance, then call it positive
  // Bum Choi

  double xCenter = gTrack->geometry()->helix().xcenter();
  double yCenter = gTrack->geometry()->helix().ycenter();
  double radius = 1.0/gTrack->geometry()->helix().curvature();

  double dPosCenter = ::sqrt( (vertex.x() - xCenter) * (vertex.x() - xCenter) +
			    (vertex.y() - yCenter) * (vertex.y() - yCenter));

  return (Float_t)(radius - dPosCenter);
}

//-----------------------------------------------------------------------

Float_t StFlowMaker::CalcDcaSigned(const StThreeVectorF vertex, 
                                   const StPhysicalHelixD helix) {
  // find the distance between the center of the circle and vertex.
  // if the radius of curvature > distance, then call it positive
  // Bum Choi

  double xCenter = helix.xcenter();
  double yCenter = helix.ycenter();
  double radius = 1.0/helix.curvature();

  double dPosCenter = ::sqrt( (vertex.x() - xCenter) * (vertex.x() - xCenter) +
                            (vertex.y() - yCenter) * (vertex.y() - yCenter));

  return (Float_t)(radius - dPosCenter);
}

//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowMaker.cxx,v $
// Revision 1.88  2004/04/09 15:49:01  aihong
// make changes to support getting PID on fly for picodst and MuDst.
//
// Revision 1.87  2003/12/12 02:33:04  oldi
// Read from PicoDST version 4 enabled again (some simulations are in this format).
//
// Revision 1.86  2003/09/02 17:58:12  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.85  2003/08/26 21:08:28  posk
// Removed commented lines.
//
// Revision 1.84  2003/07/08 18:27:09  posk
// Skips pico tracks with dca = dcaGlobal.
//
// Revision 1.83  2003/05/06 20:38:03  posk
// Removed all but last two versions of pico file read.
//
// Revision 1.82  2003/04/01 00:27:08  posk
// Little q is now unweighted by pt or eta. Big Q is unaffected.
//
// Revision 1.81  2003/01/14 14:19:07  oldi
// Log of last commit changed to indicate the important introduction of the
// pMuTrack->flag() cut.
//
// Revision 1.80  2003/01/14 14:12:17  oldi
// Cut on pMuTrack->flag() introduced. No the results agree, 
// independently of the input format (*.event.root <-> *.MuDst.root).
// Possibility to exclude TPC tracks completely (= FTPC only).
//
// Revision 1.79  2003/01/13 20:03:16  aihong
// let it exclude MuDst files with events less than 5
//
// Revision 1.78  2003/01/10 16:42:17  oldi
// Several changes to comply with FTPC tracks:
// - Switch to include/exclude FTPC tracks introduced.
//   The same switch changes the range of the eta histograms.
// - Eta symmetry plots for FTPC tracks added and separated from TPC plots.
// - PhiWgts and related histograms for FTPC tracks split in FarEast, East,
//   West, FarWest (depending on vertex.z()).
// - Psi_Diff plots for 2 different selections and the first 2 harmonics added.
// - Cut to exclude mu-events with no primary vertex introduced.
//   (This is possible for UPC events and FTPC tracks.)
// - Global DCA cut for FTPC tracks added.
// - Global DCA cuts for event plane selection separated for TPC and FTPC tracks.
// - Charge cut for FTPC tracks added.
//
// Revision 1.77  2003/01/08 19:26:49  posk
// PhiWgt hists sorted on sign of z of first and last points.
// Version 6 of pico file.
//
// Revision 1.76  2002/06/10 22:51:00  posk
// pt and eta weighting now default.
// DcaGlobalPart default now 0 to 1 cm.
// Event cut order changed.
//
// Revision 1.75  2002/06/07 22:18:40  kirill
// Introduced MuDst reader
//
// Revision 1.74  2002/05/23 18:54:12  posk
// Moved centrality cuts into StFlowConstants
//
// Revision 1.73  2002/03/15 16:43:22  snelling
// Added a method to recalculate the centrality in StFlowPicoEvent
//
// Revision 1.72  2002/03/14 18:51:50  snelling
// Added new centralities
//
// Revision 1.71  2002/03/12 02:33:22  posk
// Now makes pico files in SL02c.
//
// Revision 1.70  2002/02/05 07:19:26  snelling
// Quick fix for problems with backward compatibility (changed ClassDef back)
//
// Revision 1.69  2002/02/01 23:06:34  snelling
// Added entries for header information in flowPico (not everthing is available yet)
//
// Revision 1.68  2002/01/14 23:39:34  posk
// Moved print commands to Finish().
//
// Revision 1.67  2002/01/11 19:08:43  posk
// Restored SetDca for backwards compatability.
//
// Revision 1.66  2002/01/07 23:32:01  posk
// Added return to prevent seg. fault when event skipped.
//
// Revision 1.65  2002/01/07 21:42:49  posk
// Protection for seg. fault when no particles.
//
// Revision 1.64  2001/12/18 19:22:15  posk
// "proton" and "antiproton" changed to "pr+" and "pr-".
// Compiles on Solaris.
//
// Revision 1.63  2001/12/11 21:33:55  posk
// Went from one to four sets of histograms for making the event plane isotropic.
// StFlowEvent::PhiWeight() has changed arguments and return value.
// The ptWgt saturates above 2 GeV/c.
//
// Revision 1.62  2001/11/09 21:10:45  posk
// Switched from CERNLIB to TMath. Little q is now normalized.
//
// Revision 1.61  2001/08/01 19:39:40  snelling
// Added the trigger word
//
// Revision 1.60  2001/07/27 20:33:40  snelling
// switched from StRun to StEvtHddr.
//
// Revision 1.59  2001/07/27 01:26:19  snelling
// Added and changed variables for picoEvent. Changed trackCut class to StTrack
//
// Revision 1.58  2001/07/24 22:29:17  snelling
// First attempt to get a standard root pico file again, added variables
//
// Revision 1.57  2001/07/02 20:19:12  posk
// Moved call to SetPids() above call to SetSelections().
//
// Revision 1.56  2001/06/06 13:02:58  rcwells
// Added SetPtWgt(Bool_t) function to StFlowEvent
//
// Revision 1.55  2001/06/04 18:57:05  rcwells
// Adding filling from HbtEvents
//
// Revision 1.54  2001/05/23 18:11:14  posk
// Removed SetPids().
//
// Revision 1.53  2001/05/22 20:17:34  posk
// Now can do pseudorapidity subevents.
//
// Revision 1.52  2001/04/25 17:46:33  perev
// HPcorrs
//
// Revision 1.51  2000/12/29 19:40:39  snelling
// Used the new calibration file for PID
//
// Revision 1.50  2000/12/12 20:22:05  posk
// Put log comments at end of files.
// Deleted persistent StFlowEvent (old micro DST).
//
// Revision 1.49  2000/12/10 02:01:13  oldi
// A new member (StTrackTopologyMap mTopology) was added to StFlowPicoTrack.
// The evaluation of either a track originates from the FTPC or not is
// unambiguous now. The evaluation itself is easily extendible for other
// detectors (e.g. SVT+TPC). Old flowpicoevent.root files are treated as if
// they contain TPC tracks only (backward compatibility).
//
// Revision 1.48  2000/12/08 17:03:38  oldi
// Phi weights for both FTPCs included.
//
// Revision 1.47  2000/12/06 15:38:46  oldi
// Including FTPC.
//
// Revision 1.46  2000/11/30 16:40:21  snelling
// Protection against loading probability pid caused it not to work anymore
// therefore protection removed again
//
// Revision 1.45  2000/11/07 02:36:41  snelling
// Do not init prob pid when not used
//
//////////////////////////////////////////////////////////////////////
