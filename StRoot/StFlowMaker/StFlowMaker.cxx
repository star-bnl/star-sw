//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowMaker.cxx,v 1.119 2010/09/30 19:30:28 posk Exp $
//
// Authors: Raimond Snellings and Art Poskanzer, LBNL, Jun 1999
//          FTPC added by Markus Oldenburg, MPI, Dec 2000
//          MuDst enabled by Kirill Filimonov, LBNL, Jun 2002
//          ZDCSMD added by Aihong Tang, Dec 2004
//////////////////////////////////////////////////////////////////////
//
// Description:
//    Maker to fill StFlowEvent from StEvent, picoevent, or muevent
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
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TProfile2D.h"
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
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "TClass.h"
#include "TDatime.h"
#include "StHbtMaker/StHbtMaker.h"
#include "StHbtMaker/Infrastructure/StHbtTrack.hh"
#define PR(x) cout << "##### FlowMaker: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowMaker)

//-----------------------------------------------------------------------

StFlowMaker::StFlowMaker(const Char_t* name): 
  StMaker(name), 
  mPicoEventWrite(kFALSE), mPicoEventRead(kFALSE), mMuEventRead(kFALSE),
  pEvent(NULL) {
  pFlowSelect = new StFlowSelection();
  SetPicoEventDir("./");
  StMuTrack::Class()->IgnoreTObjectStreamer();
  StMuHelix::Class()->IgnoreTObjectStreamer();
  pFlowEvent = NULL;
}

StFlowMaker::StFlowMaker(const Char_t* name,
			 const StFlowSelection& flowSelect) :
  StMaker(name), 
  mPicoEventWrite(kFALSE), mPicoEventRead(kFALSE), mMuEventRead(kFALSE),
  pEvent(NULL) {
  pFlowSelect = new StFlowSelection(flowSelect);
  SetPicoEventDir("./");
  StMuTrack::Class()->IgnoreTObjectStreamer();
  StMuHelix::Class()->IgnoreTObjectStreamer();
  pFlowEvent = NULL;
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
      gMessMgr->Info() << "FlowMaker: Old filename: "  << mEventFileNameOld << endm;  
    }

    // new file?
    if (mEventFileName != mEventFileNameOld) { 
      //gMessMgr->Info() << "##### FlowMaker: " <<  mEventFileName << endm;
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

  // Instantiate a new StFlowEvent
  pFlowEvent = new StFlowEvent;
  if (!pFlowEvent) return kStOK;

  // Get a pointer to the event read
  if (mPicoEventRead) {
    if (!FillFromPicoDST(pPicoEvent)) return kStEOF; // false if EOF
    if (!pFlowEvent) return kStOK; // could have been deleted
    
  } else if (mMuEventRead) {
    pMu = (StMuDst*)GetInputDS("MuDst");
    if (pMu) {
      pMuEvent = pMu->event();
      pMuTracks = pMu->primaryTracks();
      pMuGlobalTracks = pMu->globalTracks();
      if (!FillFromMuDST()) return kStEOF; // false if EOF
      if (!pFlowEvent) return kStOK; // could have been deleted
      if (mPicoEventWrite) FillPicoEvent();
    }
  } else {
    // StEvent
    pEvent = dynamic_cast<StEvent*>(GetInputDS("StEvent"));
    if (!pEvent) {
      if (Debug()) { gMessMgr->Info() << "FlowMaker: no StEvent " << endm; }
      return kStOK; // If no event, we're done
    }    
    // Check the event cuts and fill StFlowEvent
    if (StFlowCutEvent::CheckEvent(pEvent)) {
      FillFlowEvent();
      if (!pFlowEvent) return kStOK;  // could have been deleted
      if (mPicoEventWrite) FillPicoEvent();
    } else {
      Long_t eventID = pEvent->id();
      gMessMgr->Info() << "##### FlowMaker: event " << eventID 
		       << " cut" << endm;
      return kStOK; // to prevent seg. fault when event does not survive
    }
  }
  // if (Debug()) PrintSubeventMults();
  
  UInt_t flowEventMult;
  if (!pFlowEvent) { flowEventMult = 0; }
  else { flowEventMult = pFlowEvent->FlowEventMult(); }

  if (flowEventMult) { // to prevent seg. fault when no particles
    // new run?
    int runID = pFlowEvent->RunID();
    if (runID != mRunID) {
      double beamEnergy    = pFlowEvent->CenterOfMassEnergy();
      double magneticField = pFlowEvent->MagneticField();
      short beamMassE      = pFlowEvent->BeamMassNumberEast();
      short beamMassW      = pFlowEvent->BeamMassNumberWest();
      if (Debug()) gMessMgr->Info() << "##### FlowMaker: " << runID << ", " <<
	beamEnergy << " GeV/A " << beamMassE << "+" << beamMassW << 
	", B= " << magneticField << endm;
      mRunID = runID;
      if (pFlowEvent->UseZDCSMD()) {
	Float_t RawZDCSMD[2][2][8];
	Float_t ex,ey,wx,wy;
	for (int strip=1; strip<9; strip++) {
	  RawZDCSMD[0][1][strip-1] = (pFlowEvent->ZDCSMD(0,1,strip))*
	    Flow::zdcsmdGainFac[0][1][strip-1] + mZDCSMDPed[0][1][strip-1];
	  RawZDCSMD[0][0][strip-1] = (pFlowEvent->ZDCSMD(0,0,strip))*
	    Flow::zdcsmdGainFac[0][0][strip-1] + mZDCSMDPed[0][0][strip-1];
          RawZDCSMD[1][1][strip-1] = (pFlowEvent->ZDCSMD(1,1,strip))*
	    Flow::zdcsmdGainFac[1][1][strip-1] + mZDCSMDPed[1][1][strip-1];
          RawZDCSMD[1][0][strip-1] = (pFlowEvent->ZDCSMD(1,0,strip))*
	    Flow::zdcsmdGainFac[1][0][strip-1] + mZDCSMDPed[1][0][strip-1];
        }
	ReadZDCSMDFile();
	for (int strip=1; strip<9; strip++) {
	  ey = (RawZDCSMD[0][1][strip-1] - mZDCSMDPed[0][1][strip-1])/
	    Flow::zdcsmdGainFac[0][1][strip-1];
	  ex = (RawZDCSMD[0][0][strip-1] - mZDCSMDPed[0][0][strip-1])/
	    Flow::zdcsmdGainFac[0][0][strip-1];
	  wy = (RawZDCSMD[1][1][strip-1] - mZDCSMDPed[1][1][strip-1])/
	    Flow::zdcsmdGainFac[1][1][strip-1];
          wx = (RawZDCSMD[1][0][strip-1] - mZDCSMDPed[1][0][strip-1])/
	    Flow::zdcsmdGainFac[1][0][strip-1];
	  if (!mPicoEventRead) {
            pFlowEvent->SetZDCSMD(0,1,strip,ey);
            pFlowEvent->SetZDCSMD(0,0,strip,ex);
            pFlowEvent->SetZDCSMD(1,0,strip,wx);
            pFlowEvent->SetZDCSMD(1,1,strip,wy);
	  }
        }
	pFlowEvent->SetZDCSMD_BeamCenter(mZDCSMDCenterEx, mZDCSMDCenterEy,
					 mZDCSMDCenterWx, mZDCSMDCenterWy);
	if (mPicoEventWrite) 
  	  for (int i=0; i<2; i++) for (int j=0; j<2; j++) for (int k=1; k<9; k++)
        	pPicoEvent->SetZDCSMD(i,j,k,pFlowEvent->ZDCSMD(i,j,k));
      }//UseZDCSMD
    }
  }

  if (Debug()) StMaker::PrintInfo();
  
  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::Init() {

  // init message manager
  gMessMgr->MemoryOn();
  gMessMgr->SetLimit("##### FlowMaker", 5);
  gMessMgr->Info("##### FlowMaker: $Id: StFlowMaker.cxx,v 1.119 2010/09/30 19:30:28 posk Exp $");

  if (Debug()) gMessMgr->Info() << "FlowMaker: Init()" << endm;

  // Open PhiWgt or ReCent file
  ReadPhiWgtFile();
  ReadReCentFile();

  if (!mPicoEventRead || !mMuEventRead) { // StEvent
    pIOMaker = (StIOMaker*)GetMaker("IO");
    if (pIOMaker) {
      mEventFileName = "";
      mEventFileNameOld = mEventFileName;
    } 
  }
  StuProbabilityPidAlgorithm::readParametersFromFile("PIDTable.root");

  Int_t kRETURN = kStOK;
  if (mPicoEventWrite) kRETURN += InitPicoEventWrite();
  if (mPicoEventRead)  kRETURN += InitPicoEventRead();
  if (mMuEventRead)    kRETURN += InitMuEventRead();; 

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

  // Print the date of completion
  TDatime now;
  cout << "#######################################################" << endl;
  cout << "#####  Finish time: " << now.AsString() << endl;

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
  if (!pPhiWgtFile->IsOpen() || !mPhiWgtCalc) {
    gMessMgr->Info("##### FlowMaker: No PhiWgt, will set weights = 1.");
  }
  if (!pPhiWgtFile->IsOpen() && mPhiWgtCalc) {
    gMessMgr->Info("##### FlowMaker: Calculating PhiWgts");
  }
  gDirectory = dirSave;

  // Read text object
  if (pPhiWgtFile->IsOpen() && mPhiWgtCalc) {
    gMessMgr->Info("##### FlowMaker: PhiWgt flattening being done");
    TText* textInfo = dynamic_cast<TText*>(pPhiWgtFile->Get("info"));
    if (textInfo) {
      mFirstLastPhiWgt = kTRUE;
      gMessMgr->Info("##### FlowMaker: File uses z of first-last points");
      cout << "##### FlowMaker: PhiWgt file written with " ;
      textInfo->ls();
    } else {
      gMessMgr->Info("##### FlowMaker: File uses vertex z and eta");
    }
  }

  // Fill mPhiWgt for each selection, and odd and even harmonics
  for (int k = 0; k < Flow::nSels; k++) {
    for (int j = 0; j < 2; j++) {
      // Tpc (FarEast)
      TString* histTitleFarEast = new TString("Flow_Phi_Weight_FarEast_Sel");
      *histTitleFarEast += k+1;
      histTitleFarEast->Append("_Har");
      *histTitleFarEast += j+1;
      // Tpc (East)
      TString* histTitleEast = new TString("Flow_Phi_Weight_East_Sel");
      *histTitleEast += k+1;
      histTitleEast->Append("_Har");
      *histTitleEast += j+1;
      // Tpc (West)
      TString* histTitleWest = new TString("Flow_Phi_Weight_West_Sel");
      *histTitleWest += k+1;
      histTitleWest->Append("_Har");
      *histTitleWest += j+1;
      // Tpc (FarWest)
      TString* histTitleFarWest = new TString("Flow_Phi_Weight_FarWest_Sel");
      *histTitleFarWest += k+1;
      histTitleFarWest->Append("_Har");
      *histTitleFarWest += j+1;
      // Ftpc (FarEast)
      TString* histTitleFtpcFarEast = new TString("Flow_Phi_Weight_FtpcFarEast_Sel");
      *histTitleFtpcFarEast += k+1;
      histTitleFtpcFarEast->Append("_Har");
      *histTitleFtpcFarEast += j+1;
      // Ftpc (East)
      TString* histTitleFtpcEast = new TString("Flow_Phi_Weight_FtpcEast_Sel");
      *histTitleFtpcEast += k+1;
      histTitleFtpcEast->Append("_Har");
      *histTitleFtpcEast += j+1;
      // Ftpc (West)
      TString* histTitleFtpcWest = new TString("Flow_Phi_Weight_FtpcWest_Sel");
      *histTitleFtpcWest += k+1;
      histTitleFtpcWest->Append("_Har");
      *histTitleFtpcWest += j+1;
      // Ftpc (FarWest)
      TString* histTitleFtpcFarWest = new TString("Flow_Phi_Weight_FtpcFarWest_Sel");
      *histTitleFtpcFarWest += k+1;
      histTitleFtpcFarWest->Append("_Har");
      *histTitleFtpcFarWest += j+1;
      if (pPhiWgtFile->IsOpen() && mPhiWgtCalc) { //use phiWgt
	TH1* phiWgtHistFarEast = dynamic_cast<TH1*>(pPhiWgtFile->
						 Get(histTitleFarEast->Data()));
	TH1* phiWgtHistEast    = dynamic_cast<TH1*>(pPhiWgtFile->
						 Get(histTitleEast->Data()));
	TH1* phiWgtHistWest    = dynamic_cast<TH1*>(pPhiWgtFile->
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
	TH1* phiWgtHistFtpcFarEast = dynamic_cast<TH1*>(pPhiWgtFile->
					    Get(histTitleFtpcFarEast->Data()));
	TH1* phiWgtHistFtpcEast    = dynamic_cast<TH1*>(pPhiWgtFile->
					    Get(histTitleFtpcEast->Data()));
	TH1* phiWgtHistFtpcWest    = dynamic_cast<TH1*>(pPhiWgtFile->
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

      } else { //phiWgts = 1
	for (int n = 0; n < Flow::nPhiBins; n++) {
	  mPhiWgt[k][j][n]        = 1.;
	  mPhiWgtFarEast[k][j][n] = 1.;
	  mPhiWgtEast[k][j][n]    = 1.;
	  mPhiWgtWest[k][j][n]    = 1.;
	  mPhiWgtFarWest[k][j][n] = 1.;
	}
	
	for (int n = 0; n < Flow::nPhiBinsFtpc; n++) {
	  mPhiWgtFtpcFarEast[k][j][n] = 1.;
	  mPhiWgtFtpcWest[k][j][n]    = 1.;
	  mPhiWgtFtpcEast[k][j][n]    = 1.;
	  mPhiWgtFtpcFarWest[k][j][n] = 1.;
	}

      }

      delete histTitleFarEast;
      delete histTitleEast;
      delete histTitleWest;
      delete histTitleFarWest;
      delete histTitleFtpcFarEast;
      delete histTitleFtpcEast;
      delete histTitleFtpcWest;
      delete histTitleFtpcFarWest;
    }
  }//phiWgt

  // ZDCSMD psi Wgt
  if (pPhiWgtFile->IsOpen()) {
    TH1* mZDCSMDPsiWest = (TH1*)pPhiWgtFile->Get("Flow_ZDCSMDPsiWgtWest");
    TH1* mZDCSMDPsiEast = (TH1*)pPhiWgtFile->Get("Flow_ZDCSMDPsiWgtEast");
    TH1* mZDCSMDPsiFull = (TH1*)pPhiWgtFile->Get("Flow_ZDCSMDPsiWgtFull");
    if (mZDCSMDPsiWest && mZDCSMDPsiEast) { //Weight histograms exist
      Float_t mZDCSMDPsiWest_mean = (mZDCSMDPsiWest->Integral())/(mZDCSMDPsiWest->GetNbinsX());
      Float_t mZDCSMDPsiEast_mean = (mZDCSMDPsiEast->Integral())/(mZDCSMDPsiEast->GetNbinsX());
      for (int n = 0; n < Flow::zdcsmd_nPsiBins; n++) {
	mZDCSMD_PsiWgtWest[n] = ((mZDCSMDPsiWest->GetBinContent(n+1))>0.) ?
	  mZDCSMDPsiWest_mean/(mZDCSMDPsiWest->GetBinContent(n+1)):1.;
	mZDCSMD_PsiWgtEast[n] = ((mZDCSMDPsiEast->GetBinContent(n+1))>0.) ?
	  mZDCSMDPsiEast_mean/(mZDCSMDPsiEast->GetBinContent(n+1)):1.;
      } // zdcsmd_nPsiBins
      if (mZDCSMDPsiFull) {
        Float_t mZDCSMDPsiFull_mean = (mZDCSMDPsiFull->Integral())/(mZDCSMDPsiFull->GetNbinsX());
        for (int n = 0; n < Flow::zdcsmd_nPsiBins; n++) {
	  mZDCSMD_PsiWgtFull[n] = ((mZDCSMDPsiFull->GetBinContent(n+1))>0.) ?
	    mZDCSMDPsiFull_mean/(mZDCSMDPsiFull->GetBinContent(n+1)):1.;
        }
      } else{ // mZDCSMDPsiFull doesn't exist
	for (int n=0;n < Flow::zdcsmd_nPsiBins;n++) {
	  mZDCSMD_PsiWgtFull[n] = 1.;
	}
      }
    } else { // Weight histograms don't exist
      for (int n=0;n < Flow::zdcsmd_nPsiBins;n++) {
	mZDCSMD_PsiWgtWest[n] = 1.;
	mZDCSMD_PsiWgtEast[n] = 1.;
	mZDCSMD_PsiWgtFull[n] = 1.;
      } // zdcsmd_nPsiBins
    }
  } else { // no pPhiWgtFile
    for (int n=0;n < Flow::zdcsmd_nPsiBins;n++) {
      mZDCSMD_PsiWgtWest[n] = 1.;
      mZDCSMD_PsiWgtEast[n] = 1.;
      mZDCSMD_PsiWgtFull[n] = 1.;
    }
  }//ZDCSMD
 
  // Close PhiWgt file
  if (pPhiWgtFile->IsOpen()) pPhiWgtFile->Close();

  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::ReadReCentFile() {
  // Read the recentering flow.reCentAna.root file

  if (Debug()) gMessMgr->Info() << "FlowMaker: ReadReCentFile()" << endm;

  TDirectory* dirSave = gDirectory;
  TFile* pReCentFile = new TFile("flow.reCentAna.root", "READ");
  if (pReCentFile->IsOpen() && mReCentCalc) {
    gMessMgr->Info("##### FlowMaker: ReCentering being done.");
  }
  if (mReCentCalc) {
    gMessMgr->Info("##### FlowMaker: Calculating ReCentering parameters.");
  } else {
    gMessMgr->Info("##### FlowMaker: Will set ReCentering parameters = 0.");
  }
  gDirectory = dirSave;

  // Fill mReCentX,Y for each selection and harmonic
  if (pReCentFile->IsOpen() && mReCentCalc) {
    for (int k = 0; k < Flow::nSels; k++) {
      for (int j = 0; j < Flow::nHars; j++) {
	TString* histTitleTPC_x = new TString("FlowReCentX_Sel");
	*histTitleTPC_x += k+1;
	*histTitleTPC_x += "_Har";
	*histTitleTPC_x += j+1;
	TString* histTitleTPC_y = new TString("FlowReCentY_Sel");
	*histTitleTPC_y += k+1;
	*histTitleTPC_y += "_Har";
	*histTitleTPC_y += j+1;
	TProfile* histCentX = dynamic_cast<TProfile*>(pReCentFile->Get(histTitleTPC_x->Data()));
	TProfile* histCentY = dynamic_cast<TProfile*>(pReCentFile->Get(histTitleTPC_y->Data()));
	if (!histCentX || !histCentY) {
	  gMessMgr->Info("##### FlowMaker: No ReCent hists.");
	}
	for (int n = 0; n < 4; n++) {
	  mReCentX[k][j][n] = (histCentX) ? histCentX->GetBinContent(n+1) : 0.;
	  mReCentY[k][j][n] = (histCentY) ? histCentY->GetBinContent(n+1) : 0.;
	  //PR(mReCentX[k][j][n]);
	}
	delete histTitleTPC_x;
	delete histTitleTPC_y;
      }
    }
  } else {
    for (int k = 0; k < Flow::nSels; k++) {
      for (int j = 0; j < Flow::nHars; j++) {
	for (int n = 0; n < 4; n++) {
	  mReCentX[k][j][n] = 0.;
	  mReCentY[k][j][n] = 0.;
	}
      }
    }	
  } 

  // Close ReCent file
  if (pReCentFile->IsOpen()) pReCentFile->Close();

  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::ReadZDCSMDFile() {
  // Read the ZDCSMD constants root file

  if (Debug()) gMessMgr->Info() << "FlowMaker: ReadZDCSMDFile()" << endm;

  TDirectory* dirSave = gDirectory;
  TFile* pZDCSMDConstFile = new TFile("zdcsmdConstants.root", "READ");
  if (!pZDCSMDConstFile->IsOpen()) {
    gMessMgr->Info("##### FlowMaker: No ZDCSMD constant file. Will use default.");
  } else { gMessMgr->Info("##### FlowMaker: ZDCSMD constant file read."); }
  gDirectory = dirSave;
  int mRealRunID = mRunID;
  Double_t HistTest = 0.;
  if (pZDCSMDConstFile->IsOpen()) {
    TH2* mZDCSMDBeamCenter2D = (TH2D *)pZDCSMDConstFile->Get("ZDCSMDBeamCenter");
    while (HistTest < 1.) {
      HistTest = mZDCSMDBeamCenter2D->GetBinContent(1,mRealRunID-5050000);
      if (HistTest > 1.) break;
      mRealRunID -= 1;
      if (mRealRunID < 5050000) break;
    }
    if (mRealRunID < 5050000) {
      mZDCSMDCenterEx = Flow::zdcsmd_ex0;
      mZDCSMDCenterEy = Flow::zdcsmd_ey0;
      mZDCSMDCenterWx = Flow::zdcsmd_wx0;
      mZDCSMDCenterWy = Flow::zdcsmd_wy0;
    } else {
      mZDCSMDCenterEx = HistTest;
      mZDCSMDCenterEy = mZDCSMDBeamCenter2D->GetBinContent(2,mRealRunID-5050000);
      mZDCSMDCenterWx = mZDCSMDBeamCenter2D->GetBinContent(3,mRealRunID-5050000);
      mZDCSMDCenterWy = mZDCSMDBeamCenter2D->GetBinContent(4,mRealRunID-5050000);
    }
  } else {
    mZDCSMDCenterEx = Flow::zdcsmd_ex0;
    mZDCSMDCenterEy = Flow::zdcsmd_ey0;
    mZDCSMDCenterWx = Flow::zdcsmd_wx0;
    mZDCSMDCenterWy = Flow::zdcsmd_wy0;
  }
  if (pZDCSMDConstFile->IsOpen()) {
    mRealRunID = mRunID;
    HistTest = 0.;
    TH2* mZDCSMDPed2D = (TH2D*)pZDCSMDConstFile->Get("ZDCSMDPedestal");
    while(HistTest < .1) {
      HistTest = mZDCSMDPed2D->GetBinContent(1,mRealRunID-5050000);
      if (HistTest > .1) break;
      mRealRunID -= 1;
      if (mRealRunID < 5050000) break;
    }
    if (mRealRunID < 5050000) {
      for (int i=0;i<2;i++) {for (int j=0;j<2;j++){for (int k=0;k<8;k++) {
        mZDCSMDPed[i][j][k] = Flow::zdcsmdPedstal[i][j][k];
       }}} // for
    } else {
      int zdcsmd_map[2][2][8] = {
        { { 7, 6, 5, 4, 3, 2, 1, 11} ,
          { 0,15,14,13,12,8,10, 9} } ,
        { {23,22,21,20,19,18,17,(mRealRunID < 8083000)?24:26} ,
          {16,31,30,29,28,27,(mRealRunID < 8083000)?26:24,25} }
        };
      for (int i=0;i<2;i++) {for (int j=0;j<2;j++){for (int k=0;k<8;k++) {
	 mZDCSMDPed[i][j][k] = mZDCSMDPed2D->GetBinContent(zdcsmd_map[i][j][k]+1,
	 mRealRunID-5050000);
        }}} // for
    }
  } else {
    for (int i=0;i<2;i++) {for (int j=0;j<2;j++){for (int k=0;k<8;k++) {
	 mZDCSMDPed[i][j][k] = Flow::zdcsmdPedstal[i][j][k];
	 }}} // for
  }

  // Close ZDCSMD constants file
  if (pZDCSMDConstFile->IsOpen()) pZDCSMDConstFile->Close();

  return kStOK;
}

//-----------------------------------------------------------------------

void StFlowMaker::FillFlowEvent() {
  // Make StFlowEvent from StEvent

  if (Debug()) gMessMgr->Info() << "FlowMaker: FillFlowEvent()" << endm;

  // Fill PhiWgt array
  if (mFirstLastPhiWgt) pFlowEvent->SetFirstLastPhiWgt();
  pFlowEvent->SetPhiWeightFarEast(mPhiWgtFarEast);
  pFlowEvent->SetPhiWeightEast(mPhiWgtEast);
  pFlowEvent->SetPhiWeightWest(mPhiWgtWest);
  pFlowEvent->SetPhiWeightFarWest(mPhiWgtFarWest);
  pFlowEvent->SetPhiWeightFtpcFarEast(mPhiWgtFtpcFarEast);
  pFlowEvent->SetPhiWeightFtpcEast(mPhiWgtFtpcEast);
  pFlowEvent->SetPhiWeightFtpcWest(mPhiWgtFtpcWest);
  pFlowEvent->SetPhiWeightFtpcFarWest(mPhiWgtFtpcFarWest);
  pFlowEvent->SetFirstLastPoints();
  pFlowEvent->SetZDCSMD_PsiWeightWest(mZDCSMD_PsiWgtWest);
  pFlowEvent->SetZDCSMD_PsiWeightEast(mZDCSMD_PsiWgtEast);
  pFlowEvent->SetZDCSMD_PsiWeightFull(mZDCSMD_PsiWgtFull);
  pFlowEvent->SetZDCSMD_BeamCenter(mZDCSMDCenterEx,mZDCSMDCenterEy,mZDCSMDCenterWx,
				   mZDCSMDCenterWy);

  // fill ReCent array
  pFlowEvent->SetReCentX(mReCentX);
  pFlowEvent->SetReCentY(mReCentY);

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

  // Get Trigger information
  if (pEvent->runId() > 4000000) { // trigger collections
    pFlowEvent->SetL0TriggerWord(StFlowCutEvent::TriggersFound()); // triggerWord is obsolete
  } else {
    StL0Trigger* pTrigger = pEvent->l0Trigger();
    if (pTrigger) { pFlowEvent->SetL0TriggerWord(pTrigger->triggerWord()); }
  }

  // Get primary vertex position
  const StThreeVectorF& vertex = pEvent->primaryVertex(0)->position();
  pFlowEvent->SetVertexPos(vertex);

  // include trigger (ZDC and CTB)
  Float_t ctb  = -1.;
  Float_t zdce = -1.;
  Float_t zdcw = -1.;
  Float_t zdcsmdEastHorizontal = -1.;
  Float_t zdcsmdEastVertical   = -1.;
  Float_t zdcsmdWestHorizontal = -1.;
  Float_t zdcsmdWestVertical   = -1.;
  StTriggerDetectorCollection *triggers = pEvent->triggerDetectorCollection();
  if (triggers)	{
    StCtbTriggerDetector &CTB = triggers->ctb();
    StZdcTriggerDetector &ZDC = triggers->zdc();
    // Get CTB
    for (UInt_t slat = 0; slat < CTB.numberOfSlats(); slat++) {
      for (UInt_t tray = 0; tray < CTB.numberOfTrays(); tray++) {
	ctb += CTB.mips(tray,slat,0);
      }
    }
    // Get ZDCe and ZDCw        
    zdce = ZDC.adcSum(east);
    zdcw = ZDC.adcSum(west);
    // Get ZDCSMD pedstal-subtracted and gain-corrected
    for (int strip=1;strip<9;strip++) {
      if (ZDC.zdcSmd(east,1,strip)) {
	zdcsmdEastHorizontal = (ZDC.zdcSmd(east,1,strip)
       	-mZDCSMDPed[0][1][strip-1])/Flow::zdcsmdGainFac[0][1][strip-1];
	pFlowEvent->SetZDCSMD(0,1,strip,zdcsmdEastHorizontal);
      }
      if (ZDC.zdcSmd(east,0,strip)) {
	zdcsmdEastVertical = (ZDC.zdcSmd(east,0,strip)
	-mZDCSMDPed[0][0][strip-1])/Flow::zdcsmdGainFac[0][0][strip-1];
	pFlowEvent->SetZDCSMD(0,0,strip,zdcsmdEastVertical);
      }
      if (ZDC.zdcSmd(west,1,strip)) {
	zdcsmdWestHorizontal = (ZDC.zdcSmd(west,1,strip)
	-mZDCSMDPed[1][1][strip-1])/Flow::zdcsmdGainFac[1][1][strip-1];
	pFlowEvent->SetZDCSMD(1,1,strip,zdcsmdWestHorizontal);
      }
      if (ZDC.zdcSmd(west,0,strip)) {
	zdcsmdWestVertical = (ZDC.zdcSmd(west,0,strip)
	-mZDCSMDPed[1][0][strip-1])/Flow::zdcsmdGainFac[1][0][strip-1];
	pFlowEvent->SetZDCSMD(1,0,strip,zdcsmdWestVertical);
      }
    }
  } 
  pFlowEvent->SetCTB(ctb);
  pFlowEvent->SetZDCe(zdce);
  pFlowEvent->SetZDCw(zdcw);
  
  // Get initial multiplicity before TrackCuts 
  UInt_t origMult = pEvent->primaryVertex(0)->numberOfDaughters(); 
  pFlowEvent->SetOrigMult(origMult);
  //PR(origMult);
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
	pFlowTrack->SetTopologyMap(pTrack->topologyMap());

	pFlowTrack->SetNhits(pTrack->detectorInfo()->numberOfPoints());
	pFlowTrack->SetFitPts(pTrack->fitTraits().numberOfFitPoints() -
			      pTrack->fitTraits().numberOfFitPoints(kSvtId) -
			      pTrack->fitTraits().numberOfFitPoints(kSsdId) - 1); // remove additional vertex point
	pFlowTrack->SetMaxPts(pTrack->numberOfPossiblePoints() -
			      pTrack->numberOfPossiblePoints(kSvtId) -
			      pTrack->numberOfPossiblePoints(kSsdId) - 1); // remove additional vertex point
	
	Double_t pathLength = gTrack->geometry()->helix().pathLength(vertex);
	StThreeVectorD distance = gTrack->geometry()->helix().at(pathLength);
	pFlowTrack->SetDcaGlobal3(distance - vertex);

	pFlowTrack->SetTrackLength(pTrack->length());

	if (pTrack->topologyMap().hasHitInDetector(kFtpcEastId) || 
	    pTrack->topologyMap().hasHitInDetector(kFtpcWestId)) { // FTPC track: first and last point are within these detectors
	  pFlowTrack->SetZFirstPoint(pTrack->detectorInfo()->firstPoint().z());
	  pFlowTrack->SetZLastPoint(pTrack->detectorInfo()->lastPoint().z());
	}
	
	else if (pTrack->topologyMap().hasHitInDetector(kTpcId)) { // TPC track
	  Double_t innerFieldCageRadius = 46.107;
	  Double_t innerPadrowRadius = 60.0;
	  Double_t x, y;
	  
	  pFlowTrack->SetZLastPoint(pTrack->detectorInfo()->lastPoint().z()); 

	  x = pTrack->detectorInfo()->firstPoint().x();
	  y = pTrack->detectorInfo()->firstPoint().y();
	  if (TMath::Sqrt(x*x+y*y) >= innerFieldCageRadius) { // track starts in TPC
	    pFlowTrack->SetZFirstPoint(pTrack->detectorInfo()->firstPoint().z()); 
	  } else { // track starts before TPC: calculate first TPC point
	    pairD pathL = pTrack->geometry()->helix().pathLength(innerPadrowRadius);
	    
	    Double_t s = 0.;
	    Double_t s1 = pathL.first;
	    Double_t s2 = pathL.second;
	    
	    // Selects positive path length to project track forward along its helix relative to
	    // first point of track. The smaller solution is taken when both are positive.
	    
	    if (finite(s1) != 0 || finite(s2) != 0) { // track could be projected
	      if (s1 >= 0 && s2 >= 0) s = s1;
	      else if (s1 >= 0 && s2 < 0) s = s1;
	      else if (s1 < 0 && s2 >= 0) s = s2;
	      pFlowTrack->SetZFirstPoint(pTrack->geometry()->helix().z(s));
	    } else { // no projection possible
	      pFlowTrack->SetZFirstPoint(0.);
	    }
	  }
	}
	
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
	  StDedxPidTraits* pid = 0;
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

  // Get mult for centrality
  UInt_t rawMult = pFlowEvent->UncorrNegMult() + pFlowEvent->UncorrPosMult();
  pFlowEvent->SetMultEta(rawMult);

  // Set centrality
  pFlowEvent->SetCentrality();

  // Set PID method
  (pFlowEvent->ProbPid()) ? pFlowEvent->SetPidsProb() : 
    pFlowEvent->SetPidsDeviant();

  // Randomize tracks
  pFlowEvent->TrackCollection()->random_shuffle();

  // Set selections
  pFlowEvent->SetSelections();

  // Set subevent method
  (pFlowEvent->EtaSubs()) ? pFlowEvent->MakeEtaSubEvents() :
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
  
  pPicoEvent->SetVersion(7);         // version 7 
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
  
  for (int i=0; i<2; i++)
    for (int j=0; j<2; j++)
      for (int k=1; k<9; k++)
	pPicoEvent->SetZDCSMD(i,j,k,pFlowEvent->ZDCSMD(i,j,k));

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
    pFlowPicoTrack->SetFitPts(pFlowTrack->FitPts() + 1); // add additional vertex point
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
  if (mFirstLastPhiWgt) pFlowEvent->SetFirstLastPhiWgt();
  pFlowEvent->SetPhiWeightFarEast(mPhiWgtFarEast);
  pFlowEvent->SetPhiWeightEast(mPhiWgtEast);
  pFlowEvent->SetPhiWeightWest(mPhiWgtWest);
  pFlowEvent->SetPhiWeightFarWest(mPhiWgtFarWest);
  pFlowEvent->SetPhiWeightFtpcFarEast(mPhiWgtFtpcFarEast);
  pFlowEvent->SetPhiWeightFtpcEast(mPhiWgtFtpcEast);
  pFlowEvent->SetPhiWeightFtpcWest(mPhiWgtFtpcWest);
  pFlowEvent->SetPhiWeightFtpcFarWest(mPhiWgtFtpcFarWest);

  // fill ReCent array
  pFlowEvent->SetReCentX(mReCentX);
  pFlowEvent->SetReCentY(mReCentY);
  
  // Check event cuts including centrality
  if (!StFlowCutEvent::CheckEvent(pPicoEvent)) {  
    Int_t eventID = pPicoEvent->EventID();
    gMessMgr->Info() << "##### FlowMaker: picoevent " << eventID 
  		     << " cut" << endm;
    delete pFlowEvent;             // delete this event
    pFlowEvent = NULL;
    return kTRUE;
  }

//   PR(pPicoEvent->Version());
  switch (pPicoEvent->Version()) {
  case 7: FillFromPicoVersion7DST(pPicoEvent); break;
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

Bool_t StFlowMaker::FillFromPicoVersion7DST(StFlowPicoEvent* pPicoEvent) {
  // Finish making StFlowEvent from StFlowPicoEvent

  if (Debug()) gMessMgr->Info() << "FlowMaker: FillFromPicoVersion7DST()" << endm;

  StuProbabilityPidAlgorithm uPid;
  
  pFlowEvent->SetFirstLastPoints();  
  pFlowEvent->SetEventID(pPicoEvent->EventID());
  UInt_t origMult = pPicoEvent->OrigMult();
  pFlowEvent->SetOrigMult(origMult);
  //PR(origMult);
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

  for (int i=0; i<2; i++)
    for (int j=0; j<2; j++)
      for (int k=1; k<9; k++)
	pFlowEvent->SetZDCSMD(i,j,k,pPicoEvent->ZDCSMD(i,j,k));

  pFlowEvent->SetZDCSMD_PsiWeightWest(mZDCSMD_PsiWgtWest);
  pFlowEvent->SetZDCSMD_PsiWeightEast(mZDCSMD_PsiWgtEast);
  pFlowEvent->SetZDCSMD_PsiWeightFull(mZDCSMD_PsiWgtFull);
  pFlowEvent->SetZDCSMD_BeamCenter(mZDCSMDCenterEx,mZDCSMDCenterEy,mZDCSMDCenterWx,
				   mZDCSMDCenterWy);
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
      pFlowTrack->SetFitPts(pPicoTrack->FitPts() - 1); // remove additional vertex point
      pFlowTrack->SetMaxPts(pPicoTrack->MaxPts());
      pFlowTrack->SetNhits(pPicoTrack->Nhits());
      pFlowTrack->SetNdedxPts(pPicoTrack->NdedxPts());
      pFlowTrack->SetDcaGlobal3(StThreeVectorD(pPicoTrack->DcaGlobalX(),
					       pPicoTrack->DcaGlobalY(),
					       pPicoTrack->DcaGlobalZ()) );
      pFlowTrack->SetTrackLength(pPicoTrack->TrackLength());

      if (StFlowEvent::ProbPid() && StuProbabilityPidAlgorithm::isPIDTableRead()) {
	uPid.processPIDAsFunction(uPid.getCentrality(pPicoEvent->UncorrNegMult()),
		pPicoTrack->DcaGlobal(),
		pPicoTrack->Charge(),
		fabs((pPicoTrack->Pt()/sqrt(1-(tanh(pPicoTrack->Eta())
		*tanh(pPicoTrack->Eta()))))/float(pPicoTrack->Charge())),
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
      
  // Check event cuts
  if (!StFlowCutEvent::CheckEvent(pMu)) {  
    Int_t eventID = pMuEvent->eventId();
    gMessMgr->Info() << "##### FlowMaker: MuEvent " << eventID 
                     << " cut" << endm;
    delete pFlowEvent;             // delete this event
    pFlowEvent = NULL;
    return kTRUE;
  }

  // Set phi weights
  if (mFirstLastPhiWgt) pFlowEvent->SetFirstLastPhiWgt();
  pFlowEvent->SetPhiWeightFarEast(mPhiWgtFarEast);
  pFlowEvent->SetPhiWeightEast(mPhiWgtEast);
  pFlowEvent->SetPhiWeightWest(mPhiWgtWest);
  pFlowEvent->SetPhiWeightFarWest(mPhiWgtFarWest);
  pFlowEvent->SetPhiWeightFtpcFarEast(mPhiWgtFtpcFarEast);
  pFlowEvent->SetPhiWeightFtpcEast(mPhiWgtFtpcEast);
  pFlowEvent->SetPhiWeightFtpcWest(mPhiWgtFtpcWest);
  pFlowEvent->SetPhiWeightFtpcFarWest(mPhiWgtFtpcFarWest);
  pFlowEvent->SetFirstLastPoints();  
  pFlowEvent->SetZDCSMD_PsiWeightWest(mZDCSMD_PsiWgtWest);
  pFlowEvent->SetZDCSMD_PsiWeightEast(mZDCSMD_PsiWgtEast);
  pFlowEvent->SetZDCSMD_PsiWeightFull(mZDCSMD_PsiWgtFull);
  pFlowEvent->SetZDCSMD_BeamCenter(mZDCSMDCenterEx, mZDCSMDCenterEy,
				   mZDCSMDCenterWx, mZDCSMDCenterWy);

  // Fill ReCent array
  pFlowEvent->SetReCentX(mReCentX);
  pFlowEvent->SetReCentY(mReCentY);

  // Set centrality, etc.
  pFlowEvent->SetRunID(pMuEvent->runId());
  //cout << "######### FillFromMuEvent runID = " << pFlowEvent->RunID() << endl;
  pFlowEvent->SetCenterOfMassEnergy(pMuEvent->runInfo().centerOfMassEnergy());
  pFlowEvent->SetMagneticField(pMuEvent->runInfo().magneticField());
  pFlowEvent->SetUncorrNegMult(pMuEvent->refMultNeg());
  pFlowEvent->SetUncorrPosMult(pMuEvent->refMultPos());
  if (pMuEvent->runId() > 8000000) { // year 7
    pFlowEvent->SetMultEta(pMuEvent->grefmult());
  } else {
    pFlowEvent->SetMultEta(pMuEvent->refMult()); 
  }
  pFlowEvent->SetCentrality(); 
  pFlowEvent->SetEventID(pMuEvent->eventId());
  pFlowEvent->SetVertexPos(pMuEvent->primaryVertexPosition());
  pFlowEvent->SetL0TriggerWord(StFlowCutEvent::TriggersFound());
  pFlowEvent->SetBeamMassNumberEast(pMuEvent->runInfo().beamMassNumber(east));
  pFlowEvent->SetBeamMassNumberWest(pMuEvent->runInfo().beamMassNumber(west));

  StuProbabilityPidAlgorithm uPid;

  pFlowEvent->SetCTB(pMuEvent->ctbMultiplicity());
  pFlowEvent->SetZDCe(pMuEvent->zdcAdcAttentuatedSumEast());
  pFlowEvent->SetZDCw(pMuEvent->zdcAdcAttentuatedSumWest());

  // Get ZDCSMD pedstal-subtracted and gain-corrected
  Float_t zdcsmdEastHorizontal = -1.;
  Float_t zdcsmdEastVertical   = -1.;
  Float_t zdcsmdWestHorizontal = -1.;
  Float_t zdcsmdWestVertical   = -1.;
  StZdcTriggerDetector &ZDC = pMuEvent->zdcTriggerDetector();
  for (int strip=1; strip<9; strip++) {
    if (ZDC.zdcSmd(east,1,strip)) {
      zdcsmdEastHorizontal = (ZDC.zdcSmd(east,1,strip)
	       -mZDCSMDPed[0][1][strip-1])/Flow::zdcsmdGainFac[0][1][strip-1];
      pFlowEvent->SetZDCSMD(0,1,strip,zdcsmdEastHorizontal);
    }
    if (ZDC.zdcSmd(east,0,strip)) {
      zdcsmdEastVertical = (ZDC.zdcSmd(east,0,strip)
	        -mZDCSMDPed[0][0][strip-1])/Flow::zdcsmdGainFac[0][0][strip-1];
      pFlowEvent->SetZDCSMD(0,0,strip,zdcsmdEastVertical);
    }
    if (ZDC.zdcSmd(west,1,strip)) {
      zdcsmdWestHorizontal = (ZDC.zdcSmd(west,1,strip)
		-mZDCSMDPed[1][1][strip-1])/Flow::zdcsmdGainFac[1][1][strip-1];
      pFlowEvent->SetZDCSMD(1,1,strip,zdcsmdWestHorizontal);
    }
    if (ZDC.zdcSmd(west,0,strip)) {
      zdcsmdWestVertical = (ZDC.zdcSmd(west,0,strip)
		-mZDCSMDPed[1][0][strip-1])/Flow::zdcsmdGainFac[1][0][strip-1];
      pFlowEvent->SetZDCSMD(1,0,strip,zdcsmdWestVertical);
    }
  }

  // Tracks
  UInt_t origMult = pMuTracks->GetEntries();
  pFlowEvent->SetOrigMult(origMult);
  //PR(origMult);  
  int goodTracks = 0;
  // Fill FlowTracks
  for (Int_t nt=0; nt < (Int_t)origMult; nt++) {
    StMuTrack* pMuTrack = (StMuTrack*)pMuTracks->UncheckedAt(nt);
    if (pMuTrack && pMuTrack->flag()>0 && StFlowCutTrack::CheckTrack(pMuTrack)) {
      // Instantiate new StFlowTrack
      StFlowTrack* pFlowTrack = new StFlowTrack;
      if (!pFlowTrack) return kFALSE;
      pFlowTrack->SetPt(pMuTrack->pt());
      if (pMuTrack->index2Global()<0) {
        gMessMgr->Info() << "FlowMaker: FillFromMuVersion0DST(): WARNING! primary track has no reference to global track (index2Global < 0)" << endl;
        continue;
      }
      StMuTrack* pMuGlobalTrack = (StMuTrack*)pMuGlobalTracks->
	At(pMuTrack->index2Global());
      if (!pMuGlobalTrack) {
        gMessMgr->Info() << "FlowMaker: FillFromMuVersion0DST(): WARNING! primary track has no reference to global track (pMuGlobalTrack = 0)" << endl;
        continue;
      }
      pFlowTrack->Setid(pMuTrack->id()); // added 2/17/10
      pFlowTrack->SetPtGlobal(pMuGlobalTrack->pt());
      pFlowTrack->SetPhi(pMuTrack->phi());
      pFlowTrack->SetPhiGlobal(pMuGlobalTrack->phi());
      pFlowTrack->SetEta(pMuTrack->eta());
      pFlowTrack->SetEtaGlobal(pMuGlobalTrack->eta());
      pFlowTrack->SetDedx(pMuTrack->dEdx());
      pFlowTrack->SetCharge(pMuTrack->charge());
      pFlowTrack->SetDcaSigned(CalcDcaSigned(pMuEvent->primaryVertexPosition(),
					     pMuTrack->helix()));
      pFlowTrack->SetDca(pMuTrack->dca().mag());
      pFlowTrack->SetDcaGlobal(pMuTrack->dcaGlobal().mag());
      pFlowTrack->SetChi2(pMuTrack->chi2xy()); 
      pFlowTrack->SetTopologyMap(pMuTrack->topologyMap());
      
      pFlowTrack->SetNhits(pMuTrack->nHits());
      pFlowTrack->SetFitPts(pMuTrack->nHitsFit() - 
			    pMuTrack->nHitsFit(kSvtId) -
			    pMuTrack->nHitsFit(kSsdId) - 1); // remove additional vertex point
      pFlowTrack->SetMaxPts(pMuTrack->nHitsPoss() -
			    pMuTrack->nHitsPoss(kSvtId) -
			    pMuTrack->nHitsPoss(kSsdId) - 1); // remove additional vertex point 

      pFlowTrack->SetNdedxPts(pMuTrack->nHitsDedx());
      pFlowTrack->SetDcaGlobal3(pMuTrack->dcaGlobal());
      pFlowTrack->SetTrackLength(pMuTrack->helix().pathLength(pMuEvent->
		     primaryVertexPosition()));

      if (pFlowTrack->TopologyMap().hasHitInDetector(kFtpcEastId)
	  || pFlowTrack->TopologyMap().hasHitInDetector(kFtpcWestId)) { // FTPC track: first and last point are within these detectors
	pFlowTrack->SetZFirstPoint(pMuTrack->firstPoint().z());
	pFlowTrack->SetZLastPoint(pMuTrack->lastPoint().z());
      }      
      else if (pFlowTrack->TopologyMap().hasHitInDetector(kTpcId)) { // TPC track
	Double_t innerFieldCageRadius = 46.107;
	Double_t innerPadrowRadius = 60.0;
	Double_t x, y;
	pFlowTrack->SetZLastPoint(pMuTrack->lastPoint().z()); 
	
	x = pMuTrack->firstPoint().x();
	y = pMuTrack->firstPoint().y();
	if (TMath::Sqrt(x*x+y*y) >= innerFieldCageRadius) { // track starts in TPC
	  pFlowTrack->SetZFirstPoint(pMuTrack->firstPoint().z()); 
	} else { // track starts before TPC: calculate first TPC point
	  pairD pathL = pMuTrack->helix().pathLength(innerPadrowRadius);
	  
	  Double_t s = 0.;
	  Double_t s1 = pathL.first;
	  Double_t s2 = pathL.second;
	  
	  // Selects positive path length to project track forward along its helix relative to
	  // first point of track. The smaller solution is taken when both are positive.
	  if (finite(s1) != 0 || finite(s2) != 0) { // track could be projected
	    if (s1 >= 0 && s2 >= 0) s = s1;
	    else if (s1 >= 0 && s2 < 0) s = s1;
	    else if (s1 < 0 && s2 >= 0) s = s2;
	    pFlowTrack->SetZFirstPoint(pMuTrack->helix().z(s));
	  } else { // no projection possible
	    pFlowTrack->SetZFirstPoint(0.);
	  }
	}
      }
      
      if (StFlowEvent::ProbPid() && StuProbabilityPidAlgorithm::isPIDTableRead()) { 
	PR(StuProbabilityPidAlgorithm::isPIDTableRead());
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
	
      }//PID    
      
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

      pFlowEvent->TrackCollection()->push_back(pFlowTrack);
      goodTracks++;
      
    }
  }//tracks
  
  // Check Eta Symmetry
  if (!StFlowCutEvent::CheckEtaSymmetry(pMuEvent)) {  
    Int_t eventID = pMuEvent->eventId();
    gMessMgr->Info() << "##### FlowMaker: MuEvent " << eventID 
                     << " cut" << endm;
    delete pFlowEvent;             // delete this event
    pFlowEvent = NULL;
    return kTRUE;
  }
  
  // Set PID method
  (pFlowEvent->ProbPid()) ? pFlowEvent->SetPidsProb() : 
    pFlowEvent->SetPidsDeviant();

  // Randomize tracks
  pFlowEvent->TrackCollection()->random_shuffle();

  // Set selections
  pFlowEvent->SetSelections();

  // Set subevent method
  (pFlowEvent->RanSubs()) ? pFlowEvent->MakeSubEvents() :
    pFlowEvent->MakeEtaSubEvents();
  
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
      cout << "j,k= " << j << k << " : " << pFlowEvent->Mult(pFlowSelect) << endl;
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
  if (Debug()) gMessMgr->Info() << "##### FlowMaker: PicoEvents file = " 
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

    TFile dummyFile(pPicoFileList->GetFileName(0),"READ");

    if (!(dummyFile.IsOpen())) {
      gMessMgr->Info() <<pPicoFileList->GetFileName(0)<<" open failed ! not chained"<<endm;
      continue;   
    }

    if (dummyFile.IsZombie()) {
      gMessMgr->Info() <<"  sth. very wrong (overwritten, invalid) with "<<pPicoFileList->GetFileName(0)<<", not chained "<<endm;
      continue;   
    }

    if (dummyFile.TestBit(1024)) { 
      gMessMgr->Info() <<"  revocer procedure applied to "<<pPicoFileList->GetFileName(0)<<", maybe useful but still not chained for flow analyses"<<endm;
      continue;   
    }

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

// Int_t StFlowMaker::InitMuEventRead() {
//   // NOT USED

//   if (Debug()) gMessMgr->Info() << "FlowMaker: InitMuEventRead()" << endm;
  
//   pMuEvents = new TClonesArray("StMuEvent", 1); 
//   pMuTracks = new TClonesArray("StMuTrack", 10000);
//   pMuGlobalTracks = new TClonesArray("StMuTrack", 10000);

//   pMuChain = new TChain("MuDst");
  
//   for (Int_t ilist = 0;  ilist < pMuFileList->GetNBundles(); ilist++) {
//     pMuFileList->GetNextBundle();

// #if 0
//     TFile* dummyFile = TFile::Open(pMuFileList->GetFileName(0),"READ");

//     if (!dummyFile ||!(dummyFile->IsOpen())) {
//       gMessMgr->Info() <<pMuFileList->GetFileName(0)<<" open failed ! not chained"<<endm;
//       continue;   
//     }

//     if (dummyFile->IsZombie()) {
//       gMessMgr->Info() <<"  sth. very wrong (overwritten, invalid) with "<<pMuFileList->GetFileName(0)<<", not chained "<<endm;
//       continue;   
//     }
//     // this shoudl fix the memory leak and the code crash
//     delete dummyFile;
    
//     // this produced the misleading statement, because the file was open to READ and it can not be recovered.
//     if (dummyFile->TestBit(1024)) { 
//       gMessMgr->Info() <<"  revocer procedure applied to "<<pMuFileList->GetFileName(0)<<", maybe useful but still not chained for flow analyses"<<endm;
//       continue;   
//     }
// #endif


//     //**************  this block is to remove files with # evts < 5
//     // if there is only one event in a file, the job will crash
//     TChain* pTempChain = new TChain("MuDst");
//     pTempChain->Add(pMuFileList->GetFileName(0));
//     if (((Int_t)pTempChain->GetEntries()) > 5 ) 
//     pMuChain->Add(pMuFileList->GetFileName(0));
//     if (pTempChain) { delete pTempChain; pTempChain=0; }
//     //**************  end of the block   

// //     if (Debug()) gMessMgr->Info() << " doFlowEvents -  input fileList = " 
// // 				  << pMuFileList->GetFileName(0) << endm;

      
//     pMuChain->SetBranchAddress("MuEvent", &pMuEvents);
//     pMuChain->SetBranchAddress("PrimaryTracks", &pMuTracks);
//     pMuChain->SetBranchAddress("GlobalTracks", &pMuGlobalTracks);

//     pMuChain->SetBranchStatus("*",0);
//     pMuChain->SetBranchStatus("MuEvent*",1);
//     pMuChain->SetBranchStatus("PrimaryTracks*",1);
//     pMuChain->SetBranchStatus("GlobalTracks.mPt",1);
//     pMuChain->SetBranchStatus("GlobalTracks.mPhi",1);
//     pMuChain->SetBranchStatus("GlobalTracks.mEta",1);

//     Int_t nEntries = (Int_t)pMuChain->GetEntries(); 
//     // if (Debug()) gMessMgr->Info() << "##### FlowMaker: events in Mu-DST chain = "
//     // << nEntries << endm;
//     gMessMgr->Info() << "### ## FlowMaker: " << pMuFileList->GetFileName(0)
// 				  << " " << nEntries << " events" << endm;
    
//   }
  
//   mEventCounter = 0;
  
//   return kStOK;
// }

//-----------------------------------------------------------------------

Int_t StFlowMaker::InitMuEventRead() {

  if (Debug()) gMessMgr->Info() << "FlowMaker: InitMuEventRead()" << endm;
  
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
  if (mFirstLastPhiWgt) pFlowEvent->SetFirstLastPhiWgt();
  pFlowEvent->SetPhiWeightFarEast(mPhiWgtFarEast);
  pFlowEvent->SetPhiWeightEast(mPhiWgtEast);
  pFlowEvent->SetPhiWeightWest(mPhiWgtWest);
  pFlowEvent->SetPhiWeightFarWest(mPhiWgtFarWest);
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
  //PR(origMult);
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
    cout << "pParticle->NHits(), pParticle->NHitsPossible() in StFlowMaker::FillFlowEvent(StHbtEvent* hbtEvent) might be wrong!" << endl;
    cout << "(MuDst, StEvent were changed and nFitPts might be different from nHits.)" << endl;
    cout << "Whoever uses them, check it, please!" << endl;
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

//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowMaker.cxx,v $
// Revision 1.119  2010/09/30 19:30:28  posk
// Instead of reversing the weight for negative pseudrapidity for odd harmonics,
// it is now done only for the first harmonic.
// Recentering is now done for all harmonics.
//
// Revision 1.118  2010/03/08 16:52:51  posk
// Added StFlowDirectCumulantMaker written by Dhevan Gangadharan.
//
// Revision 1.117  2009/11/24 19:23:04  posk
// Added reCenter option to remove acceptance correlations instead of phiWgt.
//
// Revision 1.116  2009/08/04 23:00:29  posk
// Reads year 7 MuDsts.
//
// Revision 1.115  2009/07/28 16:11:52  posk
// Reinstalled hbt stuff.
//
// Revision 1.114  2009/07/24 20:23:32  posk
// Clean up: Removed John Wu's Grid Collector, reading any data before year4, and calculating event plane for hbt Maker. Kept only the most recent pico DST read.
//
// Revision 1.113  2007/07/12 19:35:49  fisyak
// Add includes for ROOT 5.16
//
// Revision 1.112  2007/03/26 20:36:04  aihong
// west ZDCSMD H7 (readout as 26 in the trigger array) was swapped with LED (readout as 24 in trigger array), to avoid abnormal pedestal in the electronic channel associated with 26 in the trigger array. The swap happened on 03/25/07
//
// Revision 1.111  2007/02/06 18:57:55  posk
// In Lee Yang Zeros method, introduced recentering of Q vector.
// Reactivated eta symmetry cut.
//
// Revision 1.110  2006/07/06 20:29:48  posk
// Changed the dynamic_cast of GetInputDS("MuDst") to a const cast.
//
// Revision 1.109  2006/07/06 16:56:02  posk
// Calculation of v1 for selection=2 is done with mixed harmonics.
//
// Revision 1.108  2006/05/11 20:14:36  fine
// Eliminate the memeory leak and code crash
//
// Revision 1.107  2006/02/22 19:25:35  posk
// Changes needed for the MuDst
// Stopped using eventSummary()
//
// Revision 1.106  2005/12/07 19:41:29  perev
// new TFile ==> TFile::Open
//
// Revision 1.105  2005/03/03 17:22:02  posk
// Initialized pFlowEvent in the constructors.
//
// Revision 1.104  2005/02/11 23:24:31  posk
// SetCentrality works for year4.
//
// Revision 1.103  2005/02/10 21:04:57  aihong
// test mProbPid of StFlowEvent before launch calculation pid on fly
//
// Revision 1.102  2005/02/10 17:39:40  posk
// Now also works with the Grid Collector.
//
// Revision 1.101  2005/02/08 20:57:36  psoren
// trigger and centrality selections were updated for all runs after run 4 to be compatible with trigger collections. Added TriggersFound() and GetFlowTriggerBitMap() functions.
//
// Revision 1.100  2004/12/22 15:15:16  aihong
// Read run-by-run beam shifts and SMD pedestal. Done by Gang
//
// Revision 1.99  2004/12/21 17:06:12  aihong
// check corrupted files for MuDst and picoDst
//
// Revision 1.98  2004/12/17 22:33:16  aihong
// add in full Psi weight for ZDC SMD and fix a few bugs, done by Gang
//
// Revision 1.97  2004/12/09 23:43:36  posk
// Minor changes in code formatting.
//
// Revision 1.96  2004/12/09 00:45:17  oldi
// Little fix to get rid if problems when primaries don't have globals assigned to them.
//
// Revision 1.95  2004/12/07 23:08:12  posk
// Only odd and even phiWgt hists. If the old phiWgt file contains more than
// two harmonics, only the first two are read. Now writes only the first two.
//
// Revision 1.94  2004/12/07 17:04:46  posk
// Eliminated the very old mOnePhiWgt, which used one phiWgt histogram for flttening
// instead of four.
//
// Revision 1.93  2004/11/11 18:14:56  posk
// Added a debug print statement.
//
// Revision 1.92  2004/08/24 20:24:34  oldi
// Minor modifications to avoid compiler warnings.
// Small bug fix (didn't affect anyone yet).
//
// Revision 1.91  2004/08/18 00:19:19  oldi
// Several changes were necessary to comply with latest changes of MuDsts and StEvent:
//
// nHits, nFitPoints, nMaxPoints
// -----------------------------
// From now on
//  - the fit points used in StFlowMaker are the fit points within the TPC xor FTPC (vertex excluded).
//  - the max. possible points used in StFlowMAker are the max. possible points within the TPC xor FTPC (vertex excluded).
//  - the number of points (nHits; not used for analyses so far) are the total number of points on a track, i. e.
//    TPC + SVT + SSD + FTPCeast + FTPCwest [reading from HBT event gives a warning, but it seems like nobody uses it anyhow].
// - The fit/max plot (used to be (fit-1)/max) was updated accordingly.
// - The default cuts for fit points were changed (only for the FTPC, since TPC doesn't set default cuts).
// - All these changes are backward compatible, as long as you change your cuts for the fit points by 1 (the vertex used to
//   be included and is not included anymore). In other words, your results won't depend on old or new MuDst, StEvent,
//   PicoDsts as long as you use the new flow software (together with the latest MuDst and StEvent software version).
// - For backward compatibility reasons the number of fit points which is written out to the flowpicoevent.root file
//   includes the vertex. It is subtracted internally while reading back the pico files. This is completely hidden from the
//   user.
//
// zFirstPoint
// -----------
// The positions of the first point of tracks which have points in the TPC can lie outside of the TPC (the tracks can start in
// the SVT or SSD now). In this case, the first point of the track is obtained by extrapolating the track helix to the inner
// radius of the TPC.
//
// Revision 1.90  2004/05/31 20:09:37  oldi
// PicoDst format changed (Version 7) to hold ZDC SMD information.
// Trigger cut modified to comply with TriggerCollections.
// Centrality definition for 62 GeV data introduced.
// Minor bug fixes.
//
// Revision 1.89  2004/05/05 21:13:44  aihong
// Gang's code for ZDC-SMD added
//
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
