////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowAnalysisMaker.cxx,v 1.11 1999/12/04 00:15:39 posk Exp $
//
// Author: Raimond Snellings and Art Poskanzer, LBNL, Aug 1999
// Description:  Maker to analyze Flow using the FlowTags
//
////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowAnalysisMaker.cxx,v $
// Revision 1.11  1999/12/04 00:15:39  posk
// Works with StFlowEvent which works with the new StEvent
//
// Revision 1.10  1999/11/24 18:14:05  posk
// Now reads event quantities with StFlowEvent methods
//
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
#include "StMaker.h"
#include "StFlowAnalysisMaker.hh"
#include "../StFlowMaker/StFlowMaker.hh"
#include "../StFlowMaker/StFlowEvent.hh"
#include "../StFlowMaker/StFlowCutEvent.hh"
#include "../StFlowMaker/StFlowCutTrack.hh"
#include "../StFlowTagMaker/StFlowTagMaker.hh"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "TVector2.h"
#include "TFile.h"
#include "TString.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TProfile.h"
#include "TF1.h"
#include "TOrdCollection.h"
#define PR(x) cout << "##### FlowAnalysis: " << (#x) << " = " << (x) << endl;
extern "C" float besi0_(const float&);

ClassImp(StFlowAnalysisMaker)

const Float_t StFlowAnalysisMaker::phiMin    =    0.;
const Float_t StFlowAnalysisMaker::phiMax    = twopi; 
const Float_t StFlowAnalysisMaker::etaMin    =   -2.;
const Float_t StFlowAnalysisMaker::etaMax    =    2.;
const Float_t StFlowAnalysisMaker::ptMin     =    0.;
const Float_t StFlowAnalysisMaker::ptMax     =    2.;
const Float_t StFlowAnalysisMaker::psiMin    =    0.;
const Float_t StFlowAnalysisMaker::psiMax    = twopi; 
const Float_t StFlowAnalysisMaker::meanPtMin =    0.;
const Float_t StFlowAnalysisMaker::meanPtMax =    1.;
const Float_t StFlowAnalysisMaker::multMin   =    0.;
const Float_t StFlowAnalysisMaker::multMax   = 2000.;
const Float_t StFlowAnalysisMaker::qMin      =    0.;
const Float_t StFlowAnalysisMaker::qMax      =    2.;
const Int_t StFlowAnalysisMaker::nPhi3DBins  =    18;
//const Int_t StFlowAnalysisMaker::nPhiBins    =    60;
const Int_t StFlowAnalysisMaker::nEtaBins    =    20;
const Int_t StFlowAnalysisMaker::nPtBins     =    10;
const Int_t StFlowAnalysisMaker::nPsiBins    =    36;
const Int_t StFlowAnalysisMaker::nMeanPtBins =    50;
const Int_t StFlowAnalysisMaker::nMultBins   =    50;
const Int_t StFlowAnalysisMaker::n_qBins     =    50;

//-----------------------------------------------------------------------

StFlowAnalysisMaker::StFlowAnalysisMaker(const Char_t* name): StMaker(name){
  // Make Flow histograms.
}

//-----------------------------------------------------------------------

StFlowAnalysisMaker::~StFlowAnalysisMaker() {
}

//-----------------------------------------------------------------------

Int_t StFlowAnalysisMaker::Make() {
  // Make histograms

  StFlowTagMaker* pFlowTagMaker = 0;
  pFlowTag = 0;
  int fillFlag = 0;
  // Get a pointer to the flow tags
  pFlowTagMaker = (StFlowTagMaker*)GetMaker("FlowTag");
  if (pFlowTagMaker) pFlowTag = pFlowTagMaker->TagPointer();
  if (!pFlowTag) {
    cout << "$$$$$ null FlowTag pointer" << endl;
  } else if (kStOK == fillFromTags()) { // get event quantities
    fillEventHistograms();              // fill from Flow Tags
    fillFlag = 1;
  }

  // Get a pointer to StFlowEvent
  StFlowMaker* pFlowMaker = (StFlowMaker*)GetMaker("Flow");
  if (pFlowMaker) pFlowEvent = pFlowMaker->FlowEventPointer();
  if (!pFlowEvent) {
    cout << "$$$$$ null FlowEvent pointer" << endl;
    return kStOK;
  } else if (!fillFlag) {
    if (kStOK == fillFromFlowEvent()) { // get event quantities
      fillEventHistograms();            // fill from FlowEvent
      fillFlag = 1;
    }
  }
  if (fillFlag) fillParticleHistograms(); // fill particle histograms
   
  PrintInfo();
  
  return kStOK;
}

//-----------------------------------------------------------------------

void StFlowAnalysisMaker::PrintInfo() {
  cout << "*************************************************************" << endl;
  cout << "$Id: StFlowAnalysisMaker.cxx,v 1.11 1999/12/04 00:15:39 posk Exp $"
       << endl;
  cout << "*************************************************************" << endl;
  if (Debug()) StMaker::PrintInfo();
}

//-----------------------------------------------------------------------

Int_t StFlowAnalysisMaker::Init() {
  // Book the histograms

  // EtaPtPhi
  mHistEtaPtPhi3D = new TH3F("Flow_EtaPtPhi3D", "Flow_EtaPtPhi3D",
      nEtaBins, etaMin, etaMax, nPtBins, ptMin, ptMax, nPhi3DBins,
      phiMin, phiMax);
  mHistEtaPtPhi3D->SetXTitle("Eta");
  mHistEtaPtPhi3D->SetYTitle("Pt");
  mHistEtaPtPhi3D->SetZTitle("Phi");
    
  // Yield for all particles
  mHistYieldAll2D = new TH2D("Flow_YieldAll2D", "Flow_YieldAll2D",
    nEtaBins, etaMin, etaMax, nPtBins, ptMin, ptMax);
  mHistYieldAll2D->Sumw2();
  mHistYieldAll2D->SetXTitle("Pseudorapidity");
  mHistYieldAll2D->SetYTitle("Pt (GeV)");

  // Mean Eta in each bin
  mHistBinEta = new TProfile("Flow_Bin_Eta", "Flow_Bin_Eta",
    nEtaBins, etaMin, etaMax, etaMin, etaMax, "");
  mHistBinEta->SetXTitle("Pseudorapidity");
  mHistBinEta->SetYTitle("<Eta>");
  
  // Mean Pt in each bin
  mHistBinPt = new TProfile("Flow_Bin_Pt", "Flow_Bin_Pt",
    nPtBins, ptMin, ptMax, ptMin, ptMax, "");
  mHistBinPt->SetXTitle("Pt (GeV)");
  mHistBinPt->SetYTitle("<Pt> (GeV)");
  
  TString* histTitle;
  for (int i = 0; i < nSels + nSubs; i++) {
    char countSubs[2];
    sprintf(countSubs,"%d",i+1);

    // for sub-events
    for (int j = 0; j < nHars; j++) {
      char countHars[2];
      sprintf(countHars,"%d",j+1);
      float order = (float)(j + 1);

      // event planes
      histTitle = new TString("Flow_Psi_Subs");
      histTitle->Append(*countSubs);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histSub[i].histSubHar[j].mHistPsiSubs = new TH1F(histTitle->Data(),
	histTitle->Data(), nPsiBins, psiMin, (psiMax / order));
      histSub[i].histSubHar[j].mHistPsiSubs->SetXTitle
	("Event Plane Angle (rad)");
      histSub[i].histSubHar[j].mHistPsiSubs->SetYTitle("Counts");
      delete histTitle;
    }
  }

  for (int k = 0; k < nSels; k++) {
    char countSubs[2];
    sprintf(countSubs,"%d",k+1);
    // for sub-event pairs

    // cos(n*delta_Psi)
    histTitle = new TString("Flow_prof_Cos_Sel");
    histTitle->Append(*countSubs);
    histFull[k].mHistCos = new TProfile(histTitle->Data(), histTitle->Data(),
      nHars, 0.5, (float)(nHars) + 0.5, -1., 1., "");
    histFull[k].mHistCos->SetXTitle("Harmonic");
    histFull[k].mHistCos->SetYTitle("cos(n*delta_Psi)");
    delete histTitle;
    
    // resolution
    histTitle = new TString("Flow_Res_Sel");
    histTitle->Append(*countSubs);
    histFull[k].mHistRes = new TH1F(histTitle->Data(), histTitle->Data(),
      nHars, 0.5, (float)(nHars) + 0.5);
    histFull[k].mHistRes->SetXTitle("Harmonic");
    histFull[k].mHistRes->SetYTitle("Resolution");
    delete histTitle;

    // for each harmonic
    for (int j = 0; j < nHars; j++) {
      float order  = (float)(j+1);
      char countHars[2];
      sprintf(countHars,"%d",j+1);

      // multiplicity
      histTitle = new TString("Flow_Mult_Sel");
      histTitle->Append(*countSubs);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistMult = new TH1F(histTitle->Data(),
        histTitle->Data(), nMultBins, multMin, multMax);
      histFull[k].histFullHar[j].mHistMult->SetXTitle("Multiplicity");
      histFull[k].histFullHar[j].mHistMult->SetYTitle("Counts");
      delete histTitle;
      
      // Phi lab
      histTitle = new TString("Flow_Phi_Sel");
      histTitle->Append(*countSubs);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhi = new TH1D(histTitle->Data(),
        histTitle->Data(), nPhiBins, phiMin, phiMax);
      histFull[k].histFullHar[j].mHistPhi->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histFullHar[j].mHistPhi->SetYTitle("Counts");
      delete histTitle;
      
      // PhiWgt new
      histTitle = new TString("Flow_Phi_Weight_Sel");
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
      histTitle = new TString("Flow_Phi_Flat_Sel");
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
      histTitle = new TString("Flow_Psi_Sel");
      histTitle->Append(*countSubs);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPsi = new TH1F(histTitle->Data(),
        histTitle->Data(), nPsiBins, psiMin, psiMax / order);
      histFull[k].histFullHar[j].mHistPsi->SetXTitle
	("Event Plane Angle (rad)");
      histFull[k].histFullHar[j].mHistPsi->SetYTitle("Counts");
      delete histTitle;
      
      // correlation of sub-event planes
      histTitle = new TString("Flow_Psi_Sub_Corr_Sel");
      histTitle->Append(*countSubs);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPsiSubCorr = new TH1F(histTitle->Data(),
        histTitle->Data(), nPsiBins, psiMin, psiMax / order);
      histFull[k].histFullHar[j].mHistPsiSubCorr->Sumw2();
      histFull[k].histFullHar[j].mHistPsiSubCorr->SetXTitle
	("Sub-Event Correlation (rad)");
      histFull[k].histFullHar[j].mHistPsiSubCorr->SetYTitle("Counts");
      delete histTitle;
      
      // correlation of sub-event planes of different order
      histTitle = new TString("Flow_Psi_Sub_Corr_Diff_Sel");
      histTitle->Append(*countSubs);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPsiSubCorrDiff = new 
        TH1F(histTitle->Data(), histTitle->Data(), nPsiBins, psiMin,
        psiMax / (order+1.));
      histFull[k].histFullHar[j].mHistPsiSubCorrDiff->Sumw2();
      histFull[k].histFullHar[j].mHistPsiSubCorrDiff->SetXTitle
	("Sub-Event Correlation (rad)");
      histFull[k].histFullHar[j].mHistPsiSubCorrDiff->SetYTitle("Counts");
      delete histTitle;
      
      // q
      histTitle = new TString("Flow_q_Sel");
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
      histTitle = new TString("Flow_MeanPt_Sel");
      histTitle->Append(*countSubs);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistMeanPt = new TH1F(histTitle->Data(),
        histTitle->Data(), nMeanPtBins, meanPtMin, meanPtMax);
      histFull[k].histFullHar[j].mHistMeanPt->SetXTitle("Mean Pt (GeV)");
      histFull[k].histFullHar[j].mHistMeanPt->SetYTitle("Counts");
      delete histTitle;

      // particle-plane azimuthal correlation
      histTitle = new TString("Flow_Phi_Corr_Sel");
      histTitle->Append(*countSubs);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhiCorr =	new TH1F(histTitle->Data(),
        histTitle->Data(), nPhiBins, phiMin, phiMax / order);
      histFull[k].histFullHar[j].mHistPhiCorr->Sumw2();
      histFull[k].histFullHar[j].mHistPhiCorr->
	SetXTitle("Particle-Plane Correlation (rad)");
      histFull[k].histFullHar[j].mHistPhiCorr->SetYTitle("Counts");
      delete histTitle;

      // Sum of v
      histTitle = new TString("Flow_Sum_v2D_Sel");
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
      histTitle = new TString("Flow_Yield2D_Sel");
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
      histTitle = new TString("Flow_vObs2D_Sel");
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
      histTitle = new TString("Flow_v2D_Sel");
      histTitle->Append(*countSubs);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHist_v2D = new TH2F(histTitle->Data(),
        histTitle->Data(), nEtaBins, etaMin, etaMax, nPtBins, ptMin, ptMax);
      histFull[k].histFullHar[j].mHist_v2D->Sumw2();
      histFull[k].histFullHar[j].mHist_v2D->SetXTitle("Pseudorapidity");
      histFull[k].histFullHar[j].mHist_v2D->SetYTitle("Pt (GeV)");
      delete histTitle;

    }
  }
  return StMaker::Init();
}

//-----------------------------------------------------------------------

Int_t StFlowAnalysisMaker::fillFromTags() {
//   Get the flow tags and calculate the full event quantities
  for (int j = 0; j < nHars; j++) {
    float order  = (float)(j+1);
    // sub-event quantities
    mQSub[0][j].Set( pFlowTag->qxa[j], pFlowTag->qya[j] );
    mQSub[1][j].Set( pFlowTag->qxb[j], pFlowTag->qyb[j] );
    mQSub[2][j].Set( pFlowTag->qxc[j], pFlowTag->qyc[j] );
    mQSub[3][j].Set( pFlowTag->qxd[j], pFlowTag->qyd[j] );
    mMultSub[0][j]   = pFlowTag->na[j];
    mMultSub[1][j]   = pFlowTag->nb[j];
    mMultSub[2][j]   = pFlowTag->nc[j];
    mMultSub[3][j]   = pFlowTag->nd[j];
    mMeanPtSub[0][j] = pFlowTag->spta[j];
    mMeanPtSub[1][j] = pFlowTag->sptb[j];
    mMeanPtSub[2][j] = pFlowTag->sptc[j];
    mMeanPtSub[3][j] = pFlowTag->sptd[j];

    // calculate Psi
    for (int i = 0; i < nSels+nSubs; i++) {
      mPsiSub[i][j] = mQSub[i][j].Phi() / order;
    }

    // full event quantities
    for (int k = 0; k < nSels; k++) {
      mQ[k][j]      = mQSub[2*k][j] + mQSub[2*k+1][j];
      mPsi[k][j]    = mQ[k][j].Phi() / order;
      mMult[k][j]   = mMultSub[2*k][j] + mMultSub[2*k+1][j];
      m_q[k][j]     = (mMult[k][j] > 0.) ? mQ[k][j].Mod()/sqrt(mMult[k][j]) : 0.;
      mMeanPt[k][j] = (mMeanPtSub[2*k][j] + mMeanPtSub[2*k+1][j])/2.;
    }
  }

  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StFlowAnalysisMaker::fillFromFlowEvent() {
  // Get event quantities from StFlowEvent
  
   for (int k = 0; k < nSels; k++) {
    for (int j = 0; j < nHars; j++) {
      for (int n = 0; n < nSubs; n++) {
	int i = 2*k + n;
	// sub-event quantities
	mPsiSub[i][j] = pFlowEvent->Psi(j, k, n+1);
      }

      // full event quantities
      mQ[k][j]      = pFlowEvent->Q(j, k);
      mPsi[k][j]    = pFlowEvent->Psi(j, k);
      m_q[k][j]     = pFlowEvent->q(j, k);
      mMult[k][j]   = (float)(pFlowEvent->Mult(j, k));
      mMeanPt[k][j] = pFlowEvent->MeanPt(j, k);
    }
  }

  return kStOK;
}

//-----------------------------------------------------------------------

void StFlowAnalysisMaker::fillEventHistograms() {
  // Fill histograms with event quantities

  // sub-event Psi_Subs
  for (int i = 0; i < nSubs + nSels; i++) {
    for (int j = 0; j < nHars; j++) {
      histSub[i].histSubHar[j].mHistPsiSubs->Fill(mPsiSub[i][j]);
    }
  }

  // full event Psi, PsiSubCorr, PsiSubCorrDiff, cos, N, q, <Pt>
  for (int k = 0; k < nSels; k++) {
    for (int j = 0; j < nHars; j++) {
      float order  = (float)(j+1);
      histFull[k].histFullHar[j].mHistPsi->Fill(mPsi[k][j]);
      float psiSubCorr = mPsiSub[2*k][j] - mPsiSub[2*k+1][j];
      histFull[k].mHistCos->Fill(order, (float)cos(order * psiSubCorr));    
      if (psiSubCorr < 0.) psiSubCorr += twopi / order;
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
      histFull[k].histFullHar[j].mHistMult->Fill(mMult[k][j]);
      histFull[k].histFullHar[j].mHist_q->Fill(m_q[k][j]);
      if (mMult[k][j] > 0.) {
	histFull[k].histFullHar[j].mHistMeanPt->Fill(mMeanPt[k][j]);
      }
    }
  }
}

//-----------------------------------------------------------------------

void StFlowAnalysisMaker::fillParticleHistograms() {
  // Fill histograms from the particles

  // Initialize Iterator, loop variables, and arrays
  StFlowTrackCollection* pFlowTracks = pFlowEvent->TrackCollection();
  StFlowTrackIterator itr;
  
  // Track loop
  for (itr = pFlowTracks->begin(); itr != pFlowTracks->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    Float_t mPhi =  pFlowTrack->Phi();
    if (mPhi < 0.) mPhi += twopi;
    Float_t mEta =  pFlowTrack->Eta();
    Float_t mPt  =  pFlowTrack->Pt();

    // no selections
    mHistEtaPtPhi3D->Fill(mEta, mPt, mPhi);
    mHistYieldAll2D->Fill(mEta, mPt);
    mHistBinEta->Fill(mEta, mEta);
    mHistBinPt->Fill(mPt, mPt);

    for (int k = 0; k < nSels; k++) {
      for (int j = 0; j < nHars; j++) {
	float order  = (float)(j+1);
	Float_t psi_i = mQ[k][j].Phi() / order;
	//if (psi_i < 0.) psi_i += twopi / order;
	if (pFlowTrack->Select(j, k)) {
	  // Remove autocorrelations
	  TVector2 Q_i;
	  Double_t phiWgt = pFlowEvent->PhiWeight(mPhi, k, j);
	  histFull[k].histFullHar[j].mHistPhiFlat->Fill(mPhi, phiWgt);
	  if (mEta < 0 && (j+1) % 2 == 1) phiWgt *= -1.;
	  Q_i.Set(phiWgt * cos(mPhi * order), phiWgt * sin(mPhi * order));
	  TVector2 mQ_i = mQ[k][j] - Q_i;
	  psi_i = mQ_i.Phi() / order;
	  if (psi_i < 0.) psi_i += twopi / order;
	  // Fill histograms with selections
	  histFull[k].histFullHar[j].mHistPhi->Fill(mPhi);
	  histFull[k].histFullHar[j].mHistYield2D->Fill(mEta, mPt);
	}
 	//cout << "k= " << k << " j= " << j << " Psi= " <<
	//  mQ[k][j].Phi() / order << "\t Psi_i= " << psi_i << endl;

       	// Caculate v for all particles
	Float_t v = cos(order * (mPhi - psi_i))/perCent;
	histFull[k].histFullHar[j].mHistSum_v2D->Fill(mEta, mPt, v);

	// Correlation of Phi of all particles with Psi
	if (mEta < 0 && (j+1) % 2 == 1) {
	  mPhi += pi; // backward particle and odd harmonic
	  if (mPhi > twopi) mPhi -= twopi;
	}
	float dPhi = mPhi - psi_i;
	if (dPhi < 0.) dPhi += twopi;
	histFull[k].histFullHar[j].mHistPhiCorr->
	  Fill(fmod(dPhi, twopi / order));
      }
    }  
    // Finish track loop
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
  TOrdCollection* phiWgtHistNames = new TOrdCollection(nSels*nHars);

  // Yield with zero error
  TH2D* histYield2DZero = new TH2D("ZeroError", "ZeroError", nEtaBins, 
    etaMin, etaMax, nPtBins, ptMin, ptMax);
  histYield2DZero->Sumw2();
  mHistYieldAll2D->Copy(*histYield2DZero);
  Double_t zero[nEtaBins+2][nPtBins+2] = {{0.}};
  histYield2DZero->SetError(&zero[0][0]);

  // Calculate resolution = sqrt(2)*sqrt(mHistCos)
  Float_t cosPair[nSels][nHars];
  Float_t cosPairErr[nSels][nHars];
  for (int k = 0; k < nSels; k++) {
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
  delete histYield2DZero;

  // Write all histograms
  TFile histFile("flow.hist.root", "RECREATE");
  GetHistList()->Write();
  histFile.Close();
  
  // Write PhiWgt histograms
  TFile phiWgtNewFile("flowPhiWgtNew.hist.root", "RECREATE");
  phiWgtHistNames->Write();
  phiWgtNewFile.Close();
  delete phiWgtHistNames;

  // Print the cut list
  StFlowCutTrack::PrintCutList();
  StFlowCutEvent::PrintCutList();
  
  return StMaker::Finish();
}
