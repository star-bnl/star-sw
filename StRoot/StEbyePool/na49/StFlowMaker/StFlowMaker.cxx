//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowMaker.cxx,v 1.4 2001/11/06 17:05:30 posk Exp $
//
// Authors: Art Poskanzer, LBNL, and Alexander Wetzler, IKF, Dec 2000
//
//////////////////////////////////////////////////////////////////////
//
// Description: Maker to fill StFlowEvent from microDST
//
//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowMaker.cxx,v $
// Revision 1.4  2001/11/06 17:05:30  posk
// New 40 Gev centrality bins. Using only sin terms at 40 GeV.
//
// Revision 1.3  2001/08/17 22:10:26  posk
// Now also can do 40 GeV data.
//
// Revision 1.2  2001/03/06 17:41:33  posk
// Put CheckEvent before fill event.
//
// Revision 1.45  2000/11/07 02:36:41  snelling
//
//////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <math.h>
#include "StFlowMaker.h"
#include "StFlowEvent.h"
#include "StEbyeEvent.h"
#include "StFlowCutEvent.h"
#include "StFlowCutTrack.h"
#include "StFlowSelection.h"
#include "StFlowConstants.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StThreeVector.hh"
#include "StIOMaker/StIOMaker.h"
#include "TFile.h"
#include "TTree.h"
#include "TBranch.h"
#include "TChain.h"
#include "StMessMgr.h"
#include "TProfile2D.h"
#define PR(x) cout << "##### FlowMaker: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowMaker)

//-----------------------------------------------------------------------

StFlowMaker::StFlowMaker(const Char_t* name): 
  StMaker(name) {
  pFlowSelect = new StFlowSelection();
}

StFlowMaker::StFlowMaker(const Char_t* name,
			 const StFlowSelection& flowSelect) :
  StMaker(name) {
  pFlowSelect = new StFlowSelection(flowSelect); // copy constructor
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

  // Instantiate a new StFlowEvent
  pFlowEvent = new StFlowEvent;
  if (!pFlowEvent) return kStOK;
  if (!FillFromMicroDST(pMicroEvent)) return kStEOF; // false if EOF
  if (!pFlowEvent) return kStOK; // could have been deleted

  int runID = pFlowEvent->RunID();
  if (runID != mRunID) {
    PR(runID);
    gMessMgr->Info() << "##### FlowMaker: " << runID << " Run" << endm;
    mRunID = runID;
  }

  if (Debug()) StMaker::PrintInfo();
  
  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::Init() {
  if (Debug()) gMessMgr->Info() << "FlowMaker: Init()" << endm;
  gMessMgr->Info() << "FlowMaker: Init()" << endm;

  mRunID = -1;

  // Open flattening files
  ReadPhiWgtFile();
  ReadMeanSinCosFile();

  Int_t kRETURN = InitMicroEventRead();

  gMessMgr->SetLimit("##### FlowMaker", 5);
  if (kRETURN) gMessMgr->Info() << "##### FlowMaker: Init return =" 
				<< kRETURN << endm;

  return kRETURN;
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::InitRun(int runID) {
  if (Debug()) gMessMgr->Info() << "FlowMaker: InitRun()" << endm;

  //PR(runID);

  return StMaker::InitRun(runID);
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::InitMicroEventRead() {
  if (Debug()) gMessMgr->Info() << "FlowMaker: InitMicroEventRead()" << endm;
  
  pMicroEvent = new StEbyeEvent(); 
  pMicroChain = new TChain("ebyeTree");
  
  for (Int_t ilist = 0;  ilist < pMicroFileList->GetNBundles(); ilist++) {
    pMicroFileList->GetNextBundle();
    if (Debug()) gMessMgr->Info() << " doFlowEvents -  input fileList = " 
				  << pMicroFileList->GetFileName(0) << endm;
    pMicroChain->Add(pMicroFileList->GetFileName(0));
  }
  
  pMicroChain->SetBranchAddress("mDSTevent", &pMicroEvent);
  
  Int_t nEntries = (Int_t)pMicroChain->GetEntries(); 
  gMessMgr->Info() << "##### FlowMaker: events in Micro-DST file = "
		   << nEntries << endm;
  if (nEntries == 0) return kStErr;

  mMicroEventCounter = 0;
  
  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::Finish() {
  if (Debug()) gMessMgr->Info() << "FlowMaker: Finish()" << endm;

  // Print the cut lists
  cout << "#######################################################" << endl;
  cout << "##### FlowMaker: Cut Lists" << endl;
  StFlowCutEvent::PrintCutList();
  StFlowCutTrack::PrintCutList();
  pFlowEvent->PrintSelectionList();

  return StMaker::Finish();
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::ReadPhiWgtFile() {
  // Read the PhiWgt root file

  if (Debug()) gMessMgr->Info() << "FlowMaker: ReadPhiWgtFile()" << endm;

  TDirectory* dirSave = gDirectory;
  TString* fileName = new TString("flowPhiWgt.root");
  TFile* pPhiWgtFile = new TFile(fileName->Data(), "READ");
  if (!pPhiWgtFile->IsOpen()) {
    gMessMgr->Info("##### FlowMaker: PhiWgt file absent. Will set weights = 1.");
  } else {
    gMessMgr->Info("##### FlowMaker: Using Phi Weights");
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

Int_t StFlowMaker::ReadMeanSinCosFile() {
  // Read the recentering root file

  if (Debug()) gMessMgr->Info() << "FlowMaker: ReadMeanSinCosFile()" << endm;

  TDirectory* dirSave = gDirectory;
  TString* fileName = new TString("flowMeanSinCos.root");
  TFile* pMeanSinCosFile = new TFile(fileName->Data(), "READ");
  if (!pMeanSinCosFile->IsOpen()) {
    gMessMgr->Info("##### FlowMaker: MeanSinCos file absent. Will set shifts = 0.");
  } else {
    gMessMgr->Info("##### FlowMaker: Using Mean Sin/Cos shifts");
  }
  delete fileName;
  gDirectory = dirSave;
  
  // Fill mMeanSin and mMeanCos arrays
  // for each harmonic
  for (int j = 0; j < Flow::nHars; j++) {
    char countHars[2];
    sprintf(countHars,"%d",j+1);
    TString* histTitleCos = new TString("Flow_Mean_Cos2D_Sel1_Har");
    histTitleCos->Append(*countHars);
    TString* histTitleSin = new TString("Flow_Mean_Sin2D_Sel1_Har");
    histTitleSin->Append(*countHars);
    if (pMeanSinCosFile->IsOpen()) {
      TProfile2D* meanCosHist = (TProfile2D*)pMeanSinCosFile->
	Get(histTitleCos->Data());
      if (!meanCosHist) {
	gMessMgr->Info("##### FlowMaker: No histogram ", histTitleCos->Data());
      }
      TProfile2D* meanSinHist = (TProfile2D*)pMeanSinCosFile->
	Get(histTitleSin->Data());
      if (!meanSinHist) {
	gMessMgr->Info("##### FlowMaker: No histogram ", histTitleSin->Data());
      }
      for (int n = 0; n < Flow::nSinCosYBins; n++) {
	for (int o = 0; o < Flow::nSinCosPtBins; o++) {
	  mMeanCos[j][n][o] = (meanCosHist) ? meanCosHist->
	    GetCellContent(n+1,o+1) : 0.;
	  mMeanSin[j][n][o] = (meanSinHist) ? meanSinHist->
	    GetCellContent(n+1,o+1) : 0.;
	}
      }
    } else {
      for (int n = 0; n < Flow::nSinCosYBins; n++) {
	for (int o = 0; o < Flow::nSinCosPtBins; o++) {
	  mMeanCos[j][n][o] = 0.;
	  mMeanSin[j][n][o] = 0.;
	}
      }
    }

    delete histTitleCos;
    delete histTitleSin;
  }

  // Close MeanSinCos file
  if (pMeanSinCosFile->IsOpen()) pMeanSinCosFile->Close();

  return kStOK;
}

//-----------------------------------------------------------------------

Bool_t StFlowMaker::FillFromMicroDST(StEbyeEvent* pMicroEvent) {
  // Make StFlowEvent from StEbyeEvent
  if (Debug()) gMessMgr->Info() << "FlowMaker: FillFromMicroDST()" << endm;
  
  if (!pMicroEvent || !pMicroChain->GetEntry(mMicroEventCounter++)) {
    cout << "##### FlowMaker: no more events" << endl; 
    return kFALSE; 
  }

  // Fill FlowEvent
  pFlowEvent->SetPhiWeight(mPhiWgt);
  pFlowEvent->SetMeanCos(mMeanCos);
  pFlowEvent->SetMeanSin(mMeanSin);
  
  // Check event cuts
  if (!StFlowCutEvent::CheckEvent(pMicroEvent)) { 
    //gMessMgr->Info() << "##### FlowMaker: microevent cut" << endm;
    delete pFlowEvent;             // delete this event
    pFlowEvent = NULL;
    return kTRUE;
  }

  if (pMicroEvent->Version()==1) {
    FillFromMicroVer1(pMicroEvent);
  }
  
  // Check Eta Symmetry
  if (!StFlowCutEvent::CheckEtaSymmetry(pMicroEvent)) { 
    //gMessMgr->Info() << "##### FlowMaker: microevent cut" << endm;
    delete pFlowEvent;             // delete this event
    pFlowEvent = NULL;
    return kTRUE;
  }
  
  // Check event cuts again
  if (!StFlowCutEvent::CheckEvent(pFlowEvent)) { 
    gMessMgr->Info() << "##### FlowMaker: flowevent cut" << endm;
    delete pFlowEvent;             // delete this event
    pFlowEvent = NULL;
    return kTRUE;
  }

  if (!pFlowEvent->ProbPid()) {
    pFlowEvent->SetPids();
  } else {
    //SetPidsProb();
  }
  pFlowEvent->SetRapidities();

  if (!pFlowEvent->Stripes()) {
    pFlowEvent->TrackCollection()->random_shuffle();
    pFlowEvent->SetSelections();
    pFlowEvent->MakeSubEvents();
  } else {
    pFlowEvent->SetSelections();
    pFlowEvent->MakeStripedSubs();
  }
  
  //PrintSubeventMults();

  return kTRUE;
}

//-----------------------------------------------------------------------

Bool_t StFlowMaker::FillFromMicroVer1(StEbyeEvent* pMicroEvent) {
  // Make StFlowEvent from StEbyeEvent
  
  if (Debug()) gMessMgr->Info() << "FlowMaker: FillFromMicroVer1()" 
				<< endm;

  pFlowEvent->SetEventID(pMicroEvent->EventID());
  UInt_t origMult = (UInt_t) pMicroEvent->OrigMult();
  pFlowEvent->SetOrigMult(origMult);
  PR(origMult);
  pFlowEvent->SetVertexPos(StThreeVectorF(pMicroEvent->Vx(),
					  pMicroEvent->Vy(),
					  pMicroEvent->Vz()));
  pFlowEvent->SetEVeto(pMicroEvent->Eveto());
  pFlowEvent->SetRunID(pMicroEvent->RunID());
  pFlowEvent->SetCent(pMicroEvent->Centrality());


  // Fill FlowTracks
  int    goodTracks    = 0;
  for (Int_t nt=0; nt < pMicroEvent->OrigMult(); nt++) {
    StEbyeTrack* pMicroTrack = (StEbyeTrack*) pMicroEvent->Tracks()
      ->UncheckedAt(nt);
    if (pMicroTrack && StFlowCutTrack::CheckTrack(pMicroTrack)) {
      // Instantiate new StFlowTrack
      StFlowTrack* pFlowTrack = new StFlowTrack;
      if (!pFlowTrack) return kFALSE;
      float pt = pMicroTrack->Pt();
      pFlowTrack->SetPt(pt);
      pFlowTrack->SetPhi(pMicroTrack->Phi());
      pFlowTrack->SetEta(pMicroTrack->Eta());
      pFlowTrack->SetDedx(pMicroTrack->TmeanCharge()/1000.);
      pFlowTrack->SetDedxMain(pMicroTrack->TmeanChargeM()/1000.);
      pFlowTrack->SetCharge(pMicroTrack->Charge());
      pFlowTrack->SetDca(pMicroTrack->Dca());
      pFlowTrack->SetBx(pMicroTrack->Bx());
      pFlowTrack->SetBy(pMicroTrack->By());
      pFlowTrack->SetChi2(pMicroTrack->Chi2());
      pFlowTrack->SetFitPts(pMicroTrack->NFitPoints());
      pFlowTrack->SetMaxPts(pMicroTrack->NMaxPoints());
      float pz = pMicroTrack->Pz();
      pFlowTrack->SetP(sqrt(pt*pt + pz*pz));
      
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
