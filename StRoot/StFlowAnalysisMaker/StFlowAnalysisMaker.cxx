////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowAnalysisMaker.cxx,v 1.9 1999/11/11 23:16:43 posk Exp $
//
// Author: Raimond Snellings and Art Poskanzer, LBNL, Aug 1999
// Description:  Maker to analyze Flow using the FlowTags
//
////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowAnalysisMaker.cxx,v $
// Revision 1.9  1999/11/11 23:16:43  posk
// Rearrangement of files.
//
// Revision 1.8  1999/11/05 00:02:02  posk
// Changed the flow vector, Q, to a TVector2.
//
// Revision 1.7  1999/10/05 16:54:07  posk
// Added getPhiWeight method for making the event plane isotropic.
//
// Revision 1.6  1999/09/24 01:23:06  fisyak
// Reduced Include Path
//
// Revision 1.5  1999/09/16 19:54:12  posk
// Added more histograms.
//
// Revision 1.4  1999/09/03 01:05:59  fisyak
// replace iostream/stdlib by iostream.h/stdlib.h
//
// Revision 1.3  1999/08/24 18:02:37  posk
// Calculates event plane resolution.
// Added macros for plotting histograms.
//
// Revision 1.2  1999/08/13 21:12:00  posk
// corrections and polishing
//
// Revision 1.1.1.1  1999/08/09 19:50:37  posk
// ver. 1.0
//
// Revision 1.0  1999/08/02 
//
//  
////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <math.h>
#include "StFlowAnalysisMaker.hh"
#include "../StFlowMaker/StFlowMaker.hh"
#include "../StFlowMaker/StFlowEvent.hh"
//#include "../StFlowMaker/StFlowTrackCollection.hh"
#include "../StFlowMaker/StFlowCutTrack.hh"
//#include "StFlowCutEvent.hh"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
//#include "StThreeVector.hh"
#include "TVector2.h"
//#include "StEvent.h"
//#include "StGlobalTrack.h"
//#include "StChain.h"
#include "TFile.h"
#include "TString.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TProfile.h"
#include "TOrdCollection.h"
#define PR(x) cout << (#x) << " = " << (x) << endl;
extern "C" float besi0_(const float&);

ClassImp(StFlowAnalysisMaker)

const Double_t bField = 0.5*tesla;

const Float_t phiMin    =    0.;
const Float_t phiMax    = twopi; 
const Float_t etaMin    =   -2.;
const Float_t etaMax    =    2.;
const Float_t ptMin     =    0.;
const Float_t ptMax     =    2.;
const Float_t psiMin    =    0.;
const Float_t psiMax    = twopi; 
const Float_t meanPtMin =    0.;
const Float_t meanPtMax =    1.;
const Float_t multMin   =    0.;
const Float_t multMax   = 2000.;
const Float_t qMin      =    0.;
const Float_t qMax      =    2.;
const Int_t nPhi3DBins  =    18;
const Int_t nPhiBins    =    60;
const Int_t nEtaBins    =    20;
const Int_t nPtBins     =    10;
const Int_t nPsiBins    =    36;
const Int_t nMeanPtBins =    50;
const Int_t nMultBins   =    50;
const Int_t n_qBins     =    50;

//-----------------------------------------------------------------------

StFlowAnalysisMaker::StFlowAnalysisMaker(const Char_t* name): StFlowMaker(name){
  // Make Flow histograms.
}

//-----------------------------------------------------------------------

StFlowAnalysisMaker::~StFlowAnalysisMaker() {
}

//-----------------------------------------------------------------------

Int_t StFlowAnalysisMaker::Make() {
  // Make histograms

  StFlowMaker* flowMaker = (StFlowMaker*)GetMaker("Flow");
  mFlowTag = flowMaker->TagPointer();
  mFlowEvent = flowMaker->FlowEventPointer();
  if (mFlowEvent && mFlowTag) {
    if (kStOK == Tags()) {    // get Tags and calculated quantities
      fillTagHistograms();    // fill histograms from Flow Tags
      fillFlowHistograms();   // fill flow histograms from particles
    }  
  } else {
    return kStOK;             // no StFlowEvent or no Tag pointer
  }
  
  PrintInfo();

  return kStOK;
}

//-----------------------------------------------------------------------

void StFlowAnalysisMaker::PrintInfo() {
  cout << "*************************************************************" << endl;
  cout << "$Id: StFlowAnalysisMaker.cxx,v 1.9 1999/11/11 23:16:43 posk Exp $"
       << endl;
  cout << "*************************************************************" << endl;
  if (Debug()) StMaker::PrintInfo();
}

//-----------------------------------------------------------------------

Int_t StFlowAnalysisMaker::Init() {
  // Book the histograms

  TString* histTitle;
  for (int i = 0; i < nSubs; i++) {
    char countSubs[2];
    sprintf(countSubs,"%d",i+1);

    // for sub-events
    for (int j = 0; j < nHars; j++) {
      char countHars[2];
      sprintf(countHars,"%d",j+1);
      float harN = (float)(j + 1);

      // event planes
      histTitle = new TString("Flow_Psi_Subs");
      histTitle->Append(*countSubs);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histSub[i].histSubHar[j].mHistPsiSubs = new TH1F(histTitle->Data(),
	histTitle->Data(), nPsiBins, psiMin, (psiMax / harN));
      histSub[i].histSubHar[j].mHistPsiSubs->SetXTitle
	("Event Plane Angle (rad)");
      histSub[i].histSubHar[j].mHistPsiSubs->SetYTitle("Counts");
      delete histTitle;
    }
  }

  for (int k = 0; k < nSubs/2; k++) {
    char countSubs[2];
    sprintf(countSubs,"%d",k+1);
    // for sub-event pairs

    // cos(n*delta_Psi)
    histTitle = new TString("Flow_prof_Cos_Event");
    histTitle->Append(*countSubs);
    histFull[k].mHistCos = new TProfile(histTitle->Data(), histTitle->Data(),
      nHars, 0.5, (float)(nHars) + 0.5, -1., 1., "");
    histFull[k].mHistCos->SetXTitle("Harmonic");
    histFull[k].mHistCos->SetYTitle("cos(n*delta_Psi)");
    delete histTitle;
    
    // resolution
    histTitle = new TString("Flow_Res_Event");
    histTitle->Append(*countSubs);
    histFull[k].mHistRes = new TH1F(histTitle->Data(), histTitle->Data(),
      nHars, 0.5, (float)(nHars) + 0.5);
    histFull[k].mHistRes->SetXTitle("Harmonic");
    histFull[k].mHistRes->SetYTitle("Resolution");
    delete histTitle;

    // for full events
    // EtaPtPhi
    histTitle = new TString("Flow_EtaPtPhi3D_Event");
    histTitle->Append(*countSubs);
    histFull[k].mHistEtaPtPhi3D = new TH3F(histTitle->Data(),
      histTitle->Data(), nEtaBins, etaMin, etaMax, nPtBins, ptMin, ptMax,
      nPhi3DBins, phiMin, phiMax);
    histFull[k].mHistEtaPtPhi3D->SetXTitle("Eta");
    histFull[k].mHistEtaPtPhi3D->SetYTitle("Pt");
    histFull[k].mHistEtaPtPhi3D->SetZTitle("Phi");
    delete histTitle;
    
    // for full events for each harmonic
    for (int j = 0; j < nHars; j++) {
      float harN  = (float)(j+1);
      char countHars[2];
      sprintf(countHars,"%d",j+1);

      // multiplicity
      histTitle = new TString("Flow_Mult_Event");
      histTitle->Append(*countSubs);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistMult = new TH1F(histTitle->Data(),
        histTitle->Data(), nMultBins, multMin, multMax);
      histFull[k].histFullHar[j].mHistMult->SetXTitle("Multiplicity");
      histFull[k].histFullHar[j].mHistMult->SetYTitle("Counts");
      delete histTitle;
      
      // Phi lab
      histTitle = new TString("Flow_Phi_Event");
      histTitle->Append(*countSubs);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhi = new TH1D(histTitle->Data(),
        histTitle->Data(), nPhiBins, phiMin, phiMax);
      histFull[k].histFullHar[j].mHistPhi->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histFullHar[j].mHistPhi->SetYTitle("Counts");
      delete histTitle;
      
      // PhiWgt
//       histTitle = new TString("Flow_Phi_Weight_Event");
//       histTitle->Append(*countSubs);
//       histTitle->Append("_Har");
//       histTitle->Append(*countHars);
//       if (phiWgtFile->IsOpen()) {
// 	TH1* phiWgtHist = (TH1*)phiWgtFile->Get(histTitle->Data());
// 	for (int n = 0; n < nPhiBins; n++) {
// 	  mPhiWgt[k][j][n] = (phiWgtHist) ? phiWgtHist->GetBinContent(n+1) : 1.;
// 	}
//       } else {
// 	for (int n = 0; n < nPhiBins; n++) {
// 	  mPhiWgt[k][j][n] = 1.;
// 	}
//       }
//       delete histTitle;

      // PhiWgt new
      histTitle = new TString("Flow_Phi_Weight_Event");
      histTitle->Append(*countSubs);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhiWgt =	new TH1D(histTitle->Data(),
        histTitle->Data(), nPhiBins, phiMin, phiMax);
      histFull[k].histFullHar[j].mHistPhiWgt->Sumw2();
      histFull[k].histFullHar[j].mHistPhiWgt->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histFullHar[j].mHistPhiWgt->SetYTitle("PhiWgt");
      delete histTitle;
      
      // Phi lab flattened
      histTitle = new TString("Flow_Phi_Flat_Event");
      histTitle->Append(*countSubs);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhiFlat =	new TH1D(histTitle->Data(),
        histTitle->Data(), nPhiBins, phiMin, phiMax);
      histFull[k].histFullHar[j].mHistPhiFlat->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histFullHar[j].mHistPhiFlat->SetYTitle("Counts");
      delete histTitle;
      
      // event plane
      histTitle = new TString("Flow_Psi_Event");
      histTitle->Append(*countSubs);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPsi = new TH1F(histTitle->Data(),
        histTitle->Data(), nPsiBins, psiMin, (psiMax / harN));
      histFull[k].histFullHar[j].mHistPsi->SetXTitle
	("Event Plane Angle (rad)");
      histFull[k].histFullHar[j].mHistPsi->SetYTitle("Counts");
      delete histTitle;
      
      // correlation of sub-event planes
      histTitle = new TString("Flow_Psi_Sub_Corr_Event");
      histTitle->Append(*countSubs);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPsiSubCorr = new TH1F(histTitle->Data(),
        histTitle->Data(), nPsiBins, psiMin, psiMax / harN);
      histFull[k].histFullHar[j].mHistPsiSubCorr->Sumw2();
      histFull[k].histFullHar[j].mHistPsiSubCorr->SetXTitle
	("Sub-Event Correlation (rad)");
      histFull[k].histFullHar[j].mHistPsiSubCorr->SetYTitle("Counts");
      delete histTitle;
      
      // correlation of sub-event planes of different harN
      histTitle = new TString("Flow_Psi_Sub_Corr_Diff_Event");
      histTitle->Append(*countSubs);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPsiSubCorrDiff = new 
        TH1F(histTitle->Data(), histTitle->Data(), nPsiBins, psiMin,
        psiMax / (harN+1.));
      histFull[k].histFullHar[j].mHistPsiSubCorrDiff->Sumw2();
      histFull[k].histFullHar[j].mHistPsiSubCorrDiff->SetXTitle
	("Sub-Event Correlation (rad)");
      histFull[k].histFullHar[j].mHistPsiSubCorrDiff->SetYTitle("Counts");
      delete histTitle;
      
      // q
      histTitle = new TString("Flow_q_Event");
      histTitle->Append(*countSubs);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHist_q = new TH1F(histTitle->Data(),
        histTitle->Data(), n_qBins, qMin, qMax);
      histFull[k].histFullHar[j].mHist_q->Sumw2();
      histFull[k].histFullHar[j].mHist_q->SetXTitle("q = |Q|/sqrt(Mult)");
      histFull[k].histFullHar[j].mHist_q->SetYTitle("Counts");
      delete histTitle;
      
      // <p_t>
      histTitle = new TString("Flow_MeanPt_Event");
      histTitle->Append(*countSubs);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistMeanPt = new TH1F(histTitle->Data(),
        histTitle->Data(), nMeanPtBins, meanPtMin, meanPtMax);
      histFull[k].histFullHar[j].mHistMeanPt->SetXTitle("Mean Pt (GeV)");
      histFull[k].histFullHar[j].mHistMeanPt->SetYTitle("Counts");
      delete histTitle;

      // particle-plane azimuthal correlation
      histTitle = new TString("Flow_Phi_Corr_Event");
      histTitle->Append(*countSubs);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhiCorr =	new TH1F(histTitle->Data(),
        histTitle->Data(), nPhiBins, phiMin, phiMax / harN);
      histFull[k].histFullHar[j].mHistPhiCorr->Sumw2();
      histFull[k].histFullHar[j].mHistPhiCorr->
	SetXTitle("Particle-Plane Correlation (rad)");
      histFull[k].histFullHar[j].mHistPhiCorr->SetYTitle("Counts");
      delete histTitle;

      // Sum of v
      histTitle = new TString("Flow_Sum_v2D_Event");
      histTitle->Append(*countSubs);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistSum_v2D =	new TH2D(histTitle->Data(),
        histTitle->Data(), nEtaBins, etaMin, etaMax, nPtBins, ptMin, ptMax);
      histFull[k].histFullHar[j].mHistSum_v2D->SetXTitle("Pseudorapidity");
      histFull[k].histFullHar[j].mHistSum_v2D->SetYTitle("Pt (GeV)");
      histFull[k].histFullHar[j].mHistSum_v2D->Sumw2();
      delete histTitle;

      // Yield
      histTitle = new TString("Flow_Yield2D_Event");
      histTitle->Append(*countSubs);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistYield2D =	new TH2D(histTitle->Data(),
        histTitle->Data(), nEtaBins, etaMin, etaMax, nPtBins, ptMin, ptMax);
      histFull[k].histFullHar[j].mHistYield2D->Sumw2();
      histFull[k].histFullHar[j].mHistYield2D->SetXTitle("Pseudorapidity");
      histFull[k].histFullHar[j].mHistYield2D->SetYTitle("Pt (GeV)");
      delete histTitle;

      // Flow observed
      histTitle = new TString("Flow_vObs2D_Event");
      histTitle->Append(*countSubs);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHist_vObs2D =	new TH2F(histTitle->Data(),
        histTitle->Data(), nEtaBins, etaMin, etaMax, nPtBins, ptMin, ptMax);
      histFull[k].histFullHar[j].mHist_vObs2D->Sumw2();
      histFull[k].histFullHar[j].mHist_vObs2D->SetXTitle("Pseudorapidity");
      histFull[k].histFullHar[j].mHist_vObs2D->SetYTitle("Pt (GeV)");
      delete histTitle;

      // Flow
      histTitle = new TString("Flow_v2D_Event");
      histTitle->Append(*countSubs);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHist_v2D = new TH2F(histTitle->Data(),
        histTitle->Data(), nEtaBins, etaMin, etaMax, nPtBins, ptMin, ptMax);
      histFull[k].histFullHar[j].mHist_v2D->Sumw2();
      histFull[k].histFullHar[j].mHist_v2D->SetXTitle("Pseudorapidity");
      histFull[k].histFullHar[j].mHist_v2D->SetYTitle("Pt (GeV)");
      delete histTitle;

      // Mean Eta in each bin
      histTitle = new TString("Flow_Bin_Eta_Event");
      histTitle->Append(*countSubs);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistBinEta = new TProfile(histTitle->Data(),
        histTitle->Data(), nEtaBins, etaMin, etaMax, etaMin, etaMax, "");
      histFull[k].histFullHar[j].mHistBinEta->SetXTitle("Pseudorapidity");
      histFull[k].histFullHar[j].mHistBinEta->SetYTitle("<Eta>");
      delete histTitle;

      // Mean Pt in each bin
      histTitle = new TString("Flow_Bin_Pt_Event");
      histTitle->Append(*countSubs);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistBinPt = new TProfile(histTitle->Data(),
        histTitle->Data(), nPtBins, ptMin, ptMax, ptMin, ptMax, "");
      histFull[k].histFullHar[j].mHistBinPt->SetXTitle("Pseudorapidity");
      histFull[k].histFullHar[j].mHistBinPt->SetYTitle("<Pt>");
      delete histTitle;
    }
  }
  return StMaker::Init();
}

//-----------------------------------------------------------------------

void StFlowAnalysisMaker::fillTagHistograms() {
  // Fill histograms from the tags
  // sub-event Psi_Subs
  for (int i = 0; i < nSubs; i++) {
    for (int j = 0; j < nHars; j++) {
      histSub[i].histSubHar[j].mHistPsiSubs->Fill(mPsiSub[i][j]);
    }
  }

  // full event Psi, PsiSubCorr, PsiSubCorrDiff, cos, N, q, <Pt>
  for (int k = 0; k < nSubs/2; k++) {
    for (int j = 0; j < nHars; j++) {
      float harN  = (float)(j+1);
      histFull[k].histFullHar[j].mHistPsi->Fill(mPsi[k][j]);
      float psiSubCorr = mPsiSub[2*k][j] - mPsiSub[2*k+1][j];
      histFull[k].mHistCos->Fill(harN,(float)cos(harN*psiSubCorr));    
      if (psiSubCorr < 0.) psiSubCorr += twopi / harN;
      histFull[k].histFullHar[j].mHistPsiSubCorr->Fill(psiSubCorr);
      if (j < nHars - 1) { // subevents of different harmonics
	int j1, j2;
	float psiSubCorrDiff;
	if (j==0) {
	  j1 = 1, j2 = 2;	
	} else if (j==1) {
	  j1 = 1, j2 = 3;	
	} else if (j==2) {
	  j1 = 2, j2 = 4;	
	}
	psiSubCorrDiff = fmod(mPsiSub[2*k][j1-1], twopi/(float)j2) - 
	  fmod(mPsiSub[2*k+1][j2-1], twopi/(float)j2);
	if (psiSubCorrDiff < 0.) psiSubCorrDiff += twopi/(float)j2;
	histFull[k].histFullHar[j].mHistPsiSubCorrDiff->
	  Fill(psiSubCorrDiff);
	psiSubCorrDiff = fmod(mPsiSub[2*k][j2-1], twopi/(float)j2) - 
	  fmod(mPsiSub[2*k+1][j1-1], twopi/(float)j2);
	if (psiSubCorrDiff < 0.) psiSubCorrDiff += twopi/(float)j2;
	histFull[k].histFullHar[j].mHistPsiSubCorrDiff->
	  Fill(psiSubCorrDiff);
      }
      histFull[k].histFullHar[j].mHistMult->Fill(mMul[k][j]);
      histFull[k].histFullHar[j].mHist_q->Fill(m_q[k][j]);
      if (mMul[k][j] > 0.) {
	histFull[k].histFullHar[j].mHistMeanPt->
	  Fill(mSumPt[k][j] / mMul[k][j]);
      }
    }
  }
}

//-----------------------------------------------------------------------

void StFlowAnalysisMaker::fillFlowHistograms() {
  // Fill histograms from the particles

  // Initialize Iterator, loop variables, and arrays
  StFlowTrackCollection* tracks = mFlowEvent->TrackCollection();
  StFlowTrackIterator itr;
  
  // Track loop
  for (itr = tracks->begin(); itr != tracks->end(); itr++) {
    StFlowTrack* mTrack = *itr;
    Float_t mPhi =  mTrack->Phi();
    if (mPhi < 0.) mPhi += twopi;
    Float_t mEta =  mTrack->Eta();
    Float_t mPt  =  mTrack->Pt();
    for (int k = 0; k < nSubs/2; k++) {
      histFull[k].mHistEtaPtPhi3D->Fill(mEta, mPt, mPhi);
      for (int j = 0; j < nHars; j++) {
	float harN  = (float)(j+1);
	Float_t psi_i = mQ[k][j].Phi()/harN;
	Double_t phiWgt = PhiWeight(mPhi, k, j);
	if (mEta < 0 && (j+1) % 2 == 1) phiWgt *= -1.;
	// Remove autocorrelations
	if (StFlowCutTrack::SelectTrack(mTrack, k, j)) {
	  TVector2 Q_i;
	  Q_i.Set(phiWgt * cos(mPhi * harN), phiWgt * sin(mPhi * harN));
	  TVector2 mQ_i = mQ[k][j] - Q_i;
	  psi_i = mQ_i.Phi() / harN;
// 	  cout << "k= " << k << " j= " << j << " Psi= " <<
// 	    mQ[k][j].Phi() / harN << "\t Psi_i= " << psi_i << endl;
	}
	histFull[k].histFullHar[j].mHistPhi->Fill(mPhi);
	histFull[k].histFullHar[j].mHistPhiFlat->Fill(mPhi, phiWgt);
	// 2D hists
	histFull[k].histFullHar[j].mHistYield2D->Fill(mEta, mPt);
	Float_t v = cos(harN*(mPhi - psi_i))/perCent;
	histFull[k].histFullHar[j].mHistSum_v2D->Fill(mEta, mPt, v);
	histFull[k].histFullHar[j].mHistBinEta->Fill(mEta, mEta);
	histFull[k].histFullHar[j].mHistBinPt->Fill(mPt, mPt);

	// Correlation of Phi with Psi
	if (mEta < 0 && (j+1) % 2 == 1) {
	  mPhi += pi; // backward particle and odd harmonic
	  if (mPhi > twopi) mPhi -= twopi;
	}
	float dPhi = mPhi - psi_i;
	if (dPhi < 0.) dPhi += twopi;
	histFull[k].histFullHar[j].mHistPhiCorr->
	  Fill(fmod(dPhi, twopi/harN));
      }
    }  
    // Finish loop
  }
}

//-----------------------------------------------------------------------

static Double_t qDist(double* q, double* par) {
  // Calculates the q distribution given the parameters v, mult, area

  Double_t expo = par[1]*par[0]*par[0]*perCent*perCent + q[0]*q[0];
  Double_t dNdq = par[2] * (2. * q[0] * exp(-expo) * 
    besi0_(2*q[0]*par[0]*perCent*sqrt(par[1])));

  return dNdq;
}

//-----------------------------------------------------------------------

Int_t StFlowAnalysisMaker::Finish() {
  // Calculates resolution and mean flow values
  // Fits q distribution and outputs phiWgt values

  // PhiWgt histogram collection
  TOrdCollection* phiWgtHistNames = new TOrdCollection(nSubs*nHars/2);

  // Calculate resolution = sqrt(2)*sqrt(mHistCos)
  Float_t cosPair[nSubs/2][nHars];
  Float_t cosPairErr[nSubs/2][nHars];
  for (int k = 0; k < nSubs/2; k++) {
    for (int j = 0; j < nHars; j++) {
      cosPair[k][j]    = histFull[k].mHistCos->GetBinContent(j+1);
      cosPairErr[k][j] = histFull[k].mHistCos->GetBinError(j+1);
      if (cosPair[k][j] > 0.) {
	mRes[k][j] = sqrt(2*cosPair[k][j]);
	mResErr[k][j] = cosPairErr[k][j] / mRes[k][j];
      } else {
	mRes[k][j] = 0.;
	mResErr[k][j] = 0.;
      }
      histFull[k].mHistRes->SetBinContent(j+1, mRes[k][j]);
      histFull[k].mHistRes->SetBinError(j+1, mResErr[k][j]);

      // Calculate vObs = Sum_v / Yield
      TH2D* histYield2DZero = new TH2D("ZeroError", "ZeroError", nEtaBins, 
       	etaMin, etaMax, nPtBins, ptMin, ptMax);
      histYield2DZero->Sumw2();
      histFull[k].histFullHar[j].mHistYield2D->Copy(*histYield2DZero);
      Double_t zero[nEtaBins+2][nPtBins+2] = {{0.}};
      histYield2DZero->SetError(&zero[0][0]);
      histFull[k].histFullHar[j].mHist_vObs2D->
 	Divide(histFull[k].histFullHar[j].mHistSum_v2D, histYield2DZero,1.,1.);

      // Calulate v = vObs / Resolution
      // The systematic error of the resolution is not folded in.
      cout << "# Resolution= " << mRes[k][j] << " +/- " << mResErr[k][j] << endl;
      histFull[k].histFullHar[j].mHist_v2D->
   	Divide(histFull[k].histFullHar[j].mHistSum_v2D, histYield2DZero,1.,1.);
      if (mRes[k][j] != 0.) {
	histFull[k].histFullHar[j].mHist_v2D->Scale(1. / mRes[k][j]);
      } else {
	cout << "### Resolution of the " << j+1 << "th harmonic was zero. ###"
	     << endl;
      }
      delete histYield2DZero;

      // Fit q distribution
      Float_t area = histFull[k].histFullHar[j].mHist_q->
	Integral() * qMax / (float)n_qBins; 
      Float_t mult = histFull[k].histFullHar[j].mHistMult->GetMean();
      TF1* func_q = new TF1("qDist", qDist, 0., qMax, 3); // fit q dist
      func_q->SetParNames("v", "mult", "area");
      func_q->SetParameters(1., mult, area); // initial values
      func_q->SetParLimits(1, 1, 1); // mult is fixed
      func_q->SetParLimits(2, 1, 1); // area is fixed
      histFull[k].histFullHar[j].mHist_q->Fit("qDist", "0");

      // Calculate PhiWgt
      Double_t mean = histFull[k].histFullHar[j].mHistPhi->Integral() 
	/ (double)nPhiBins;
      for (int i = 0; i < nPhiBins; i++) {
	histFull[k].histFullHar[j].mHistPhiWgt->SetBinContent(i+1, mean);
	histFull[k].histFullHar[j].mHistPhiWgt->SetBinError(i+1, 0.);
      }
      histFull[k].histFullHar[j].mHistPhiWgt->
	Divide(histFull[k].histFullHar[j].mHistPhi);
      phiWgtHistNames->AddLast(histFull[k].histFullHar[j].mHistPhiWgt);
    }
  }

  // Write all histograms
  TFile histFile("flow.hist.root", "RECREATE");
  GetHistList()->Write();
  histFile.Close();
  
  // Write PhiWgt histograms
  TFile phiWgtNewFile("flowPhiWgtNew.hist.root", "RECREATE");
  phiWgtHistNames->Write();
  phiWgtNewFile.Close();
  delete phiWgtHistNames;
  
  return StMaker::Finish();
}
