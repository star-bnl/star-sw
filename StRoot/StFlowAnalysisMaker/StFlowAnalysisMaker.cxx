/***************************************************************************
 *
 * $Id: StFlowAnalysisMaker.cxx,v 1.7 1999/10/05 16:54:07 posk Exp $
 *
 * Author: Raimond Snellings and Art Poskanzer, LBNL, Aug 1999
 * Description:  Maker to analyze Flow using the FlowTags
 *
 ***************************************************************************
 *
 * $Log: StFlowAnalysisMaker.cxx,v $
 * Revision 1.7  1999/10/05 16:54:07  posk
 * Added getPhiWeight method for making the event plane isotropic.
 *
 * Revision 1.6  1999/09/24 01:23:06  fisyak
 * Reduced Include Path
 *
 * Revision 1.5  1999/09/16 19:54:12  posk
 * Added more histograms.
 *
 * Revision 1.4  1999/09/03 01:05:59  fisyak
 * replace iostream/stdlib by iostream.h/stdlib.h
 *
 * Revision 1.3  1999/08/24 18:02:37  posk
 * Calculates event plane resolution.
 * Added macros for plotting histograms.
 *
 * Revision 1.2  1999/08/13 21:12:00  posk
 * corrections and polishing
 *
 * Revision 1.1.1.1  1999/08/09 19:50:37  posk
 * ver. 1.0
 *
 * Revision 1.0  1999/08/02 
 *
 *  
 **************************************************************************/
#include <iostream.h>
#include <stdlib.h>
#include <math.h>
#include "StFlowTagMaker/StFlowTagMaker.h"
#include "StFlowAnalysisMaker.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StThreeVector.hh"
#include "StLorentzVector.hh"
#include "StMaker.h"
#include "StEvent.h"
#include "StGlobalTrack.h"
#include "StChain.h"
//#include "TMath.h"
#include "TString.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TProfile.h"
#include "TOrdCollection.h"

#define PR(x) cout << (#x) << " = " << (x) << endl;
extern "C" float besi0_(const float&);
ClassImp(StFlowAnalysisMaker)

const Double_t    bField             = 0.5*tesla;
const Double_t    trackquality[3][2] = {{10, 0}, 
				       {  0, 0},
				       {  0, 0}}; 

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

StFlowAnalysisMaker::StFlowAnalysisMaker(const Char_t* name): StMaker(name){
  // Make Flow histograms.
  mFlowTag = 0;
  mEvent   = 0;
}

StFlowAnalysisMaker::~StFlowAnalysisMaker() {
}

Int_t StFlowAnalysisMaker::Make() {
  // Get the flow tags and make histograms

  // Get a pointer to the DST
  mEvent = (StEvent*)GetInputDS("StEvent");
  if (!mEvent) return kStOK; // If no event, we're done
  
  if (kStOK == getTags()) {  // get Tags and calculated quantities
    
    // fill histograms from Flow Tags
    makeTagHistograms();
    
    // fill flow histograms from particles
    makeFlowHistograms();
  }

  PrintInfo();

  return kStOK;
}

void StFlowAnalysisMaker::PrintInfo() {
  cout << "*************************************************************" << endl;
  cout << "$Id: StFlowAnalysisMaker.cxx,v 1.7 1999/10/05 16:54:07 posk Exp $"
       << endl;
  cout << "*************************************************************" << endl;
  if (Debug()) StMaker::PrintInfo();
}

Int_t StFlowAnalysisMaker::Init() {
  // Book the histograms
  // Open PhiWgt file
  TDirectory* dirSave = gDirectory;
  TFile* phiWgtFile = new TFile("flowPhiWgt.hist.root", "READ");
  if (!phiWgtFile->IsOpen()) {
    cout << "### Can't open PhiWgt file" << endl;
  }
  gDirectory = dirSave;

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
      histFull[k].histFullHar[j].mHistPhi = new TH1F(histTitle->Data(),
        histTitle->Data(), nPhiBins, phiMin, phiMax);
      histFull[k].histFullHar[j].mHistPhi->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histFullHar[j].mHistPhi->SetYTitle("Counts");
      delete histTitle;
      
      // PhiWgt
      histTitle = new TString("Flow_Phi_Weight_Event");
      histTitle->Append(*countSubs);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      if (phiWgtFile->IsOpen()) {
	TH1* phiWgtHist = (TH1*)phiWgtFile->Get(histTitle->Data());
	for (int n = 0; n < nPhiBins; n++) {
	  fPhiWgt[k][j][n] = (phiWgtHist) ? phiWgtHist->GetBinContent(n+1) : 1.;
	}
      } else {
	for (int n = 0; n < nPhiBins; n++) {
	  fPhiWgt[k][j][n] = 1.;
	}
      }
      delete histTitle;

      // PhiWgt new
      histTitle = new TString("Flow_Phi_Weight_Event");
      histTitle->Append(*countSubs);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhiWgt =	new TH1F(histTitle->Data(),
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
      histFull[k].histFullHar[j].mHistPhiFlat =	new TH1F(histTitle->Data(),
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
  if (phiWgtFile->IsOpen()) phiWgtFile->Close();
  return StMaker::Init();
}

Int_t StFlowAnalysisMaker::getTags() {
  // Get the flow tags and calculate some quantities

  float minMul = 10.; // minimum full event multiplicity
  
  // Get a pointer to the flow tags
  StFlowTagMaker* flowTagMaker = (StFlowTagMaker*)GetMaker("FlowTag");
  mFlowTag = flowTagMaker->tag();
  if (!mFlowTag) {
    cout << "$$$$$ zero FlowTag pointer" << endl;
    return kStErr;
  }

  for (int j = 0; j < nHars; j++) {
    float harN  = (float)(j+1);
    // sub-event quantities
    fQxSub[0][j]    = mFlowTag->qxa[j];
    fQxSub[1][j]    = mFlowTag->qxb[j];
    fQxSub[2][j]    = mFlowTag->qxc[j];
    fQxSub[3][j]    = mFlowTag->qxd[j];
    fQySub[0][j]    = mFlowTag->qya[j];
    fQySub[1][j]    = mFlowTag->qyb[j];
    fQySub[2][j]    = mFlowTag->qyc[j];
    fQySub[3][j]    = mFlowTag->qyd[j];
    fMulSub[0][j]   = mFlowTag->na[j];
    fMulSub[1][j]   = mFlowTag->nb[j];
    fMulSub[2][j]   = mFlowTag->nc[j];
    fMulSub[3][j]   = mFlowTag->nd[j];
    fSumPtSub[0][j] = mFlowTag->spta[j];
    fSumPtSub[1][j] = mFlowTag->sptb[j];
    fSumPtSub[2][j] = mFlowTag->sptc[j];
    fSumPtSub[3][j] = mFlowTag->sptd[j];
    // calculate Psi
    for (int i = 0; i < nSubs; i++) {
      fPsiSub[i][j] = atan2(fQySub[i][j], fQxSub[i][j]) / harN;
      if (fPsiSub[i][j] < 0.) fPsiSub[i][j] += twopi / harN;
    }

    // full event quantities
    for (int k = 0; k < nSubs/2; k++) {
      // add the sub-events
      fQx[k][j]   = fQxSub[2*k][j]    + fQxSub[2*k+1][j];
      fQy[k][j]   = fQySub[2*k][j]    + fQySub[2*k+1][j];
      fMul[k][j]  = fMulSub[2*k][j]   + fMulSub[2*k+1][j];
      fSumPt[k][j]= fSumPtSub[2*k][j] + fSumPtSub[2*k+1][j];

      // calculate
      fQ[k][j]    = sqrt(fQx[k][j]*fQx[k][j] + fQy[k][j]*fQy[k][j]);
      if (fMul[k][j] > 0.) {
	f_q[k][j]  = fQ[k][j] / sqrt(fMul[k][j]);
      } else {
	f_q[k][j]  = 0.;
      }
      fPsi[k][j]  = atan2(fQy[k][j],fQx[k][j]) / harN;
      if (fPsi[k][j] < 0.) fPsi[k][j] += twopi / harN;
    }
  }
  // Multiplicity cut
  return ((fMul[0][1] + fMul[1][1]) >= minMul) ? kStOK : kStErr;
}

Float_t StFlowAnalysisMaker::getPhiWeight(Float_t fPhi, Int_t eventN, Int_t harN) {
  int n = (fPhi/twopi)*nPhiBins;
  return fPhiWgt[eventN][harN][n];
}

void StFlowAnalysisMaker::makeTagHistograms() {
  // Fill histograms from the tags

  // sub-event Psi_Subs
  for (int i = 0; i < nSubs; i++) {
    for (int j = 0; j < nHars; j++) {
      histSub[i].histSubHar[j].mHistPsiSubs->Fill(fPsiSub[i][j]);
    }
  }

  // full event Psi, PsiSubCorr, PsiSubCorrDiff, cos, N, q, <Pt>
  for (int k = 0; k < nSubs/2; k++) {
    for (int j = 0; j < nHars; j++) {
      float harN  = (float)(j+1);
      histFull[k].histFullHar[j].mHistPsi->Fill(fPsi[k][j]);
      float psiSubCorr = fPsiSub[2*k][j] - fPsiSub[2*k+1][j];
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
	psiSubCorrDiff = fmod(fPsiSub[2*k][j1-1], twopi/(float)j2) - 
	  fmod(fPsiSub[2*k+1][j2-1], twopi/(float)j2);
	if (psiSubCorrDiff < 0.) psiSubCorrDiff += twopi/(float)j2;
	histFull[k].histFullHar[j].mHistPsiSubCorrDiff->
	  Fill(psiSubCorrDiff);
	psiSubCorrDiff = fmod(fPsiSub[2*k][j2-1], twopi/(float)j2) - 
	  fmod(fPsiSub[2*k+1][j1-1], twopi/(float)j2);
	if (psiSubCorrDiff < 0.) psiSubCorrDiff += twopi/(float)j2;
	histFull[k].histFullHar[j].mHistPsiSubCorrDiff->
	  Fill(psiSubCorrDiff);
      }
      histFull[k].histFullHar[j].mHistMult->Fill(fMul[k][j]);
      histFull[k].histFullHar[j].mHist_q->Fill(f_q[k][j]);
      if (fMul[k][j] > 0.) {
	histFull[k].histFullHar[j].mHistMeanPt->
	  Fill(fSumPt[k][j] / fMul[k][j]);
      }
    }
  }
}

void StFlowAnalysisMaker::makeFlowHistograms() {
  // Fill histograms from the particles

  // Initialize Iterator, loop variables, and arrays
  StTrackCollection* tracks = mEvent->trackCollection();
  StTrackIterator itr;
  long initialMultiplicity = tracks->size();
  
  // track loop
  long TrackCount = 0;
  int i;
  float dPhi;
  for (itr = tracks->begin(), i=0; itr != tracks->end(); itr++) {
    StGlobalTrack* gtrk = *itr;
    StTrackFitTraits& fitTraits = gtrk->fitTraits();
    Int_t nFitPoints = fitTraits.numberOfFitPoints();
    Int_t nMaxPoints = fitTraits.numberOfPossiblePoints();
    if (nFitPoints > (int)trackquality[0][0]) {
      StThreeVectorD p = gtrk->helix().momentum(bField); 
      Float_t fPhi =  p.phi();
      if (fPhi < 0.) fPhi += twopi;
      Float_t fEta =  p.pseudoRapidity();
      Float_t fPt  =  p.perp();
      for (int k = 0; k < nSubs/2; k++) {
	histFull[k].mHistEtaPtPhi3D->Fill(fEta, fPt, fPhi);
	for (int j = 0; j < nHars; j++) {
	  float harN  = (float)(j+1);
	  histFull[k].histFullHar[j].mHistPhi->Fill(fPhi);
	  // Remove autocorrelations
	  Float_t weight = getPhiWeight(fPhi, k, j);
	  histFull[k].histFullHar[j].mHistPhiFlat->Fill(fPhi, weight);
	  if (fEta < 0 && (j+1) % 2 == 1) weight *= -1.;
	  Float_t Qx_i = weight * cos(fPhi * harN);
	  Float_t Qy_i = weight * sin(fPhi * harN);
	  Float_t psi_i = atan2(fQy[k][j] - Qy_i, fQx[k][j] - Qx_i) / harN;
	  if (psi_i < 0.) psi_i += twopi / harN;
	  // cout << "k= " << k << " j= " << j << " Psi= " << fPsi[k][j] <<
	  //  "\t Psi_i= " << psi_i << endl;

	  // 2D hists
	  histFull[k].histFullHar[j].mHistYield2D->Fill(fEta, fPt);
	  Float_t v = cos(harN*(fPhi - psi_i))/perCent;
	  histFull[k].histFullHar[j].mHistSum_v2D->Fill(fEta, fPt, v);
	  histFull[k].histFullHar[j].mHistBinEta->Fill(fEta, fEta);
	  histFull[k].histFullHar[j].mHistBinPt->Fill(fPt, fPt);

	  // Correlation of Phi with Psi
	  if (fEta < 0 && (j+1) % 2 == 1) {
	    fPhi += pi; // backward particle and odd harmonic
	    if (fPhi > twopi) fPhi -= twopi;
	  }
	  dPhi = fPhi - psi_i;
	  if (dPhi < 0.) dPhi += twopi;
	  histFull[k].histFullHar[j].mHistPhiCorr->
	    Fill(fmod(dPhi, twopi/harN));
	}
      }
      
      // Finish loop
      TrackCount++;
      i++;
    }
  }
}

static double qDist(double* q, double* par) {
  // Calculates the q distribution given the parameters v, mult, area

  double expo = par[1]*par[0]*par[0]*perCent*perCent + q[0]*q[0];
  double dNdq = par[2] * (2. * q[0] * exp(-expo) * 
    besi0_(2*q[0]*par[0]*perCent*sqrt(par[1])));

  return dNdq;
}

Int_t StFlowAnalysisMaker::Finish() {
  // Calculates resolution and mean flow values
  // Fits q distribution and outputs phiWeight values

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
	fRes[k][j] = sqrt(2*cosPair[k][j]);
	fResErr[k][j] = cosPairErr[k][j] / fRes[k][j];
      } else {
	fRes[k][j] = 0.;
	fResErr[k][j] = 0.;
      }
      histFull[k].mHistRes->SetBinContent(j+1, fRes[k][j]);
      histFull[k].mHistRes->SetBinError(j+1, fResErr[k][j]);

      // Calculate vObs = Sum_v / Yield
      TH2D* histYield2DZero = new TH2D("ZeroError", "ZeroError", nEtaBins, 
       	etaMin, etaMax, nPtBins, ptMin, ptMax);
      histYield2DZero->Sumw2();
      histFull[k].histFullHar[j].mHistYield2D->Copy(*histYield2DZero);
      double zero[nEtaBins+2][nPtBins+2] = {{0.}};
      histYield2DZero->SetError(&zero[0][0]);
      histFull[k].histFullHar[j].mHist_vObs2D->
 	Divide(histFull[k].histFullHar[j].mHistSum_v2D, histYield2DZero,1.,1.);

      // Calulate v = vObs / Resolution
      // The systematic error of the resolution is not folded in.
      cout << "# Resolution= " << fRes[k][j] << " +/- " << fResErr[k][j] << endl;
      histFull[k].histFullHar[j].mHist_v2D->
   	Divide(histFull[k].histFullHar[j].mHistSum_v2D, histYield2DZero,1.,1.);
      if (fRes[k][j] != 0.) {
	histFull[k].histFullHar[j].mHist_v2D->Scale(1. / fRes[k][j]);
      } else {
	cout << "### Resolution of the " << j+1 << "th harmonic was zero. ###"
	     << endl;
      }
      delete histYield2DZero;

      // Fit q distribution
      float area = histFull[k].histFullHar[j].mHist_q->
	Integral() * qMax / (float)n_qBins; 
      float mult = histFull[k].histFullHar[j].mHistMult->GetMean();
      TF1* func_q = new TF1("qDist", qDist, 0., qMax, 3); // fit q dist
      func_q->SetParNames("v", "mult", "area");
      func_q->SetParameters(1., mult, area); // initial values
      func_q->SetParLimits(1, 1, 1); // mult is fixed
      func_q->SetParLimits(2, 1, 1); // area is fixed
      histFull[k].histFullHar[j].mHist_q->Fit("qDist", "0");

      // Calculate PhiWgt
      float mean = histFull[k].histFullHar[j].mHistPhi->
	Integral() / (float)nPhiBins;
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
  this->GetHistList()->Write();
  histFile.Close();
  
  // Write PhiWgt histograms
  TFile phiWgtNewFile("flowPhiWgtNew.hist.root", "RECREATE");
  phiWgtHistNames->Write();
  phiWgtNewFile.Close();
  delete phiWgtHistNames;
  
  return StMaker::Finish();
}

