/***************************************************************************
 *
 * $Id: StFlowAnalysisMaker.cxx,v 1.6 1999/09/24 01:23:06 fisyak Exp $
 *
 * Author: Raimond Snellings and Art Poskanzer, LBNL, Aug 1999
 * Description:  Maker to analyze Flow using the FlowTags
 *
 ***************************************************************************
 *
 * $Log: StFlowAnalysisMaker.cxx,v $
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
#include "TMath.h"
#include "TString.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TProfile.h"

extern "C" float besi0_(const float&);

ClassImp(StFlowAnalysisMaker)

const double    bField             = 0.5*tesla;
const double    trackquality[3][2] = {{10, 0}, 
				      { 0, 0},
				      { 0, 0}}; 

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
    
    // fill flow histograms
    makeFlowHistograms();
  }

  PrintInfo();

  return kStOK;
}

void StFlowAnalysisMaker::PrintInfo() {
  cout << "*************************************************************" << endl;
  cout << "$Id: StFlowAnalysisMaker.cxx,v 1.6 1999/09/24 01:23:06 fisyak Exp $"
       << endl;
  cout << "*************************************************************" << endl;
  if (Debug()) StMaker::PrintInfo();
}

Int_t StFlowAnalysisMaker::Init() {
  // Book the histograms

  const Float_t PhiMin    =    0.;
  const Float_t PhiMax    = twopi; 
  const Float_t EtaMin    =   -2.;
  const Float_t EtaMax    =    2.;
  const Float_t PtMin     =    0.;
  const Float_t PtMax     =    2.;
  const Float_t PsiMin    =    0.;
  const Float_t PsiMax    = twopi; 
  const Float_t MeanPtMin =    0.;
  const Float_t MeanPtMax =    1.;
  const Float_t MultMin   =    0.;
  const Float_t MultMax   = 2000.;
  const Float_t qMin      =    0.;
  const Float_t qMax      =    2.;
  const Int_t nPhi3DBins  =    18;
  const Int_t nPhiBins    =    45;
  const Int_t nEtaBins    =    20;
  const Int_t nPtBins     =    10;
  const Int_t nPsiBins    =    36;
  const Int_t nMeanPtBins =    50;
  const Int_t nMultBins   =    50;
  const Int_t n_qBins     =    50;

  TString* mHistTitle;
  for (int i = 0; i < nSubs; i++) {
    char mCountSubs[2];
    sprintf(mCountSubs,"%d",i);

    // for sub-events
    for (int j = 0; j < nHars; j++) {
      char mCountHars[2];
      sprintf(mCountHars,"%d",j);
      float harN = (float)(j + 1);

      // event planes
      mHistTitle = new TString("Flow_Psi_Subs");
      mHistTitle->Append(*mCountSubs + 1);
      mHistTitle->Append("_Har");
      mHistTitle->Append(*mCountHars + 1);
      histSub[i].histSubHar[j].mHistPsiSubs =
	new TH1F(mHistTitle->Data(), mHistTitle->Data(), nPsiBins, PsiMin,
		 (PsiMax / harN));
      histSub[i].histSubHar[j].mHistPsiSubs->SetXTitle
	("Event Plane Angle (rad)");
      histSub[i].histSubHar[j].mHistPsiSubs->SetYTitle("Counts");
      delete mHistTitle;
    }
  }

  for (int k = 0; k < nSubs/2; k++) {
    char mCountSubs[2];
    sprintf(mCountSubs,"%d",k);
    // for sub-event pairs

    // cos(n*delta_Psi)
    mHistTitle = new TString("Flow_prof_Cos_Event");
    mHistTitle->Append(*mCountSubs + 1);
    histFull[k].mHistCos =
      new TProfile(mHistTitle->Data(), mHistTitle->Data(), nHars, 0.5, 
		   (float)(nHars) + 0.5, -1., 1., "");
    histFull[k].mHistCos->SetXTitle("Harmonic");
    histFull[k].mHistCos->SetYTitle("cos(n*delta_Psi)");
    delete mHistTitle;
    
    // resolution
    mHistTitle = new TString("Flow_Res_Event");
    mHistTitle->Append(*mCountSubs + 1);
    histFull[k].mHistRes =
      new TH1F(mHistTitle->Data(), mHistTitle->Data(), nHars, 0.5, 
		   (float)(nHars) + 0.5);
    histFull[k].mHistRes->SetXTitle("Harmonic");
    histFull[k].mHistRes->SetYTitle("Resolution");
    delete mHistTitle;

    // for full events
    // EtaPtPhi
    mHistTitle = new TString("Flow_EtaPtPhi3D_Event");
    mHistTitle->Append(*mCountSubs + 1);
    histFull[k].mHistEtaPtPhi3D =
      new TH3F(mHistTitle->Data(), mHistTitle->Data(), nEtaBins, EtaMin,
	       EtaMax, nPtBins, PtMin, PtMax, nPhi3DBins, PhiMin, PhiMax);
    histFull[k].mHistEtaPtPhi3D->SetXTitle("Eta");
    histFull[k].mHistEtaPtPhi3D->SetYTitle("Pt");
    histFull[k].mHistEtaPtPhi3D->SetZTitle("Phi");
    delete mHistTitle;
    
    // for full events for each harmonic
    for (int j = 0; j < nHars; j++) {
      float harN  = (float)(j+1);
      char mCountHars[2];
      sprintf(mCountHars,"%d",j);

      // multiplicity
      mHistTitle = new TString("Flow_Mult_Event");
      mHistTitle->Append(*mCountSubs + 1);
      mHistTitle->Append("_Har");
      mHistTitle->Append(*mCountHars + 1);
      histFull[k].histFullHar[j].mHistMult =
	new TH1F(mHistTitle->Data(), mHistTitle->Data(), nMultBins, MultMin, 
		 MultMax);
      histFull[k].histFullHar[j].mHistMult->SetXTitle("Multiplicity");
      histFull[k].histFullHar[j].mHistMult->SetYTitle("Counts");
      delete mHistTitle;
      
      // Phi lab
      mHistTitle = new TString("Flow_Phi_Event");
      mHistTitle->Append(*mCountSubs + 1);
      mHistTitle->Append("_Har");
      mHistTitle->Append(*mCountHars + 1);
      histFull[k].histFullHar[j].mHistPhi =
	new TH1F(mHistTitle->Data(), mHistTitle->Data(), nPhiBins, PhiMin,
		 PhiMax);
      histFull[k].histFullHar[j].mHistPhi->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histFullHar[j].mHistPhi->SetYTitle("Counts");
      delete mHistTitle;
      
      // Phi lab flattened
      mHistTitle = new TString("Flow_Phi_Flat_Event");
      mHistTitle->Append(*mCountSubs + 1);
      mHistTitle->Append("_Har");
      mHistTitle->Append(*mCountHars + 1);
      histFull[k].histFullHar[j].mHistPhiFlat =
	new TH1F(mHistTitle->Data(), mHistTitle->Data(), nPhiBins, PhiMin,
		 PhiMax);
      histFull[k].histFullHar[j].mHistPhiFlat->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histFullHar[j].mHistPhiFlat->SetYTitle("Counts");
      delete mHistTitle;
      
      // event plane
      mHistTitle = new TString("Flow_Psi_Event");
      mHistTitle->Append(*mCountSubs + 1);
      mHistTitle->Append("_Har");
      mHistTitle->Append(*mCountHars + 1);
      histFull[k].histFullHar[j].mHistPsi =
	new TH1F(mHistTitle->Data(), mHistTitle->Data(), nPsiBins, PsiMin,
		 (PsiMax / harN));
      histFull[k].histFullHar[j].mHistPsi->SetXTitle
	("Event Plane Angle (rad)");
      histFull[k].histFullHar[j].mHistPsi->SetYTitle("Counts");
      delete mHistTitle;
      
      // correlation of sub-event planes
      mHistTitle = new TString("Flow_Psi_Sub_Corr_Event");
      mHistTitle->Append(*mCountSubs + 1);
      mHistTitle->Append("_Har");
      mHistTitle->Append(*mCountHars + 1);
      histFull[k].histFullHar[j].mHistPsiSubCorr =
	new TH1F(mHistTitle->Data(), mHistTitle->Data(), nPsiBins, PsiMin,
		 PsiMax / harN);
      histFull[k].histFullHar[j].mHistPsiSubCorr->Sumw2();
      histFull[k].histFullHar[j].mHistPsiSubCorr->SetXTitle
	("Sub-Event Correlation (rad)");
      histFull[k].histFullHar[j].mHistPsiSubCorr->SetYTitle("Counts");
      delete mHistTitle;
      
      // correlation of sub-event planes of different harN
      mHistTitle = new TString("Flow_Psi_Sub_Corr_Diff_Event");
      mHistTitle->Append(*mCountSubs + 1);
      mHistTitle->Append("_Har");
      mHistTitle->Append(*mCountHars + 1);
      histFull[k].histFullHar[j].mHistPsiSubCorrDiff =
	new TH1F(mHistTitle->Data(), mHistTitle->Data(), nPsiBins, PsiMin,
		 PsiMax / (harN+1.));
      histFull[k].histFullHar[j].mHistPsiSubCorrDiff->Sumw2();
      histFull[k].histFullHar[j].mHistPsiSubCorrDiff->SetXTitle
	("Sub-Event Correlation (rad)");
      histFull[k].histFullHar[j].mHistPsiSubCorrDiff->SetYTitle("Counts");
      delete mHistTitle;
      
      // q
      mHistTitle = new TString("Flow_q_Event");
      mHistTitle->Append(*mCountSubs + 1);
      mHistTitle->Append("_Har");
      mHistTitle->Append(*mCountHars + 1);
      histFull[k].histFullHar[j].mHist_q =
	new TH1F(mHistTitle->Data(), mHistTitle->Data(), n_qBins, qMin, qMax);
      histFull[k].histFullHar[j].mHist_q->Sumw2();
      histFull[k].histFullHar[j].mHist_q->SetXTitle("q = |Q|/sqrt Mul");
      histFull[k].histFullHar[j].mHist_q->SetYTitle("Counts");
      delete mHistTitle;
      
      // <p_t>
      mHistTitle = new TString("Flow_MeanPt_Event");
      mHistTitle->Append(*mCountSubs + 1);
      mHistTitle->Append("_Har");
      mHistTitle->Append(*mCountHars + 1);
      histFull[k].histFullHar[j].mHistMeanPt =
	new TH1F(mHistTitle->Data(), mHistTitle->Data(), nMeanPtBins, MeanPtMin,
		 MeanPtMax);
      histFull[k].histFullHar[j].mHistMeanPt->SetXTitle("Mean Pt (GeV)");
      histFull[k].histFullHar[j].mHistMeanPt->SetYTitle("Counts");
      delete mHistTitle;

      // particle-plane azimuthal correlation
      mHistTitle = new TString("Flow_phi_Corr_Event");
      mHistTitle->Append(*mCountSubs + 1);
      mHistTitle->Append("_Har");
      mHistTitle->Append(*mCountHars + 1);
      histFull[k].histFullHar[j].mHistPhiCorr =
	new TH1F(mHistTitle->Data(), mHistTitle->Data(), nPhiBins, PhiMin,
		 PhiMax / harN);
      histFull[k].histFullHar[j].mHistPhiCorr->Sumw2();
      histFull[k].histFullHar[j].mHistPhiCorr->
	SetXTitle("Particle-Plane Correlation (rad)");
      histFull[k].histFullHar[j].mHistPhiCorr->SetYTitle("Counts");
      delete mHistTitle;

      // Sum of v
      mHistTitle = new TString("Flow_Sum_v2D_Event");
      mHistTitle->Append(*mCountSubs + 1);
      mHistTitle->Append("_Har");
      mHistTitle->Append(*mCountHars + 1);
      histFull[k].histFullHar[j].mHistSum_v2D =
	new TH2D(mHistTitle->Data(), mHistTitle->Data(), nEtaBins, EtaMin,
		 EtaMax, nPtBins, PtMin, PtMax);
      histFull[k].histFullHar[j].mHistSum_v2D->SetXTitle("Pseudorapidity");
      histFull[k].histFullHar[j].mHistSum_v2D->SetYTitle("Pt (GeV)");
      histFull[k].histFullHar[j].mHistSum_v2D->Sumw2();
      delete mHistTitle;

      // Yield
      mHistTitle = new TString("Flow_Yield2D_Event");
      mHistTitle->Append(*mCountSubs + 1);
      mHistTitle->Append("_Har");
      mHistTitle->Append(*mCountHars + 1);
      histFull[k].histFullHar[j].mHistYield2D =
	new TH2D(mHistTitle->Data(), mHistTitle->Data(), nEtaBins, EtaMin,
		 EtaMax, nPtBins, PtMin, PtMax);
      histFull[k].histFullHar[j].mHistYield2D->Sumw2();
      histFull[k].histFullHar[j].mHistYield2D->SetXTitle("Pseudorapidity");
      histFull[k].histFullHar[j].mHistYield2D->SetYTitle("Pt (GeV)");
      delete mHistTitle;

      // Flow observed
      mHistTitle = new TString("Flow_vObs2D_Event");
      mHistTitle->Append(*mCountSubs + 1);
      mHistTitle->Append("_Har");
      mHistTitle->Append(*mCountHars + 1);
      histFull[k].histFullHar[j].mHist_vObs2D =
	new TH2F(mHistTitle->Data(), mHistTitle->Data(), nEtaBins, EtaMin,
		 EtaMax, nPtBins, PtMin, PtMax);
      histFull[k].histFullHar[j].mHist_vObs2D->Sumw2();
      histFull[k].histFullHar[j].mHist_vObs2D->SetXTitle("Pseudorapidity");
      histFull[k].histFullHar[j].mHist_vObs2D->SetYTitle("Pt (GeV)");
      delete mHistTitle;

      // Flow
      mHistTitle = new TString("Flow_v2D_Event");
      mHistTitle->Append(*mCountSubs + 1);
      mHistTitle->Append("_Har");
      mHistTitle->Append(*mCountHars + 1);
      histFull[k].histFullHar[j].mHist_v2D =
	new TH2F(mHistTitle->Data(), mHistTitle->Data(), nEtaBins, EtaMin,
		 EtaMax, nPtBins, PtMin, PtMax);
      histFull[k].histFullHar[j].mHist_v2D->Sumw2();
      histFull[k].histFullHar[j].mHist_v2D->SetXTitle("Pseudorapidity");
      histFull[k].histFullHar[j].mHist_v2D->SetYTitle("Pt (GeV)");
      delete mHistTitle;

      // Mean Eta in each bin
      mHistTitle = new TString("Flow_Bin_Eta_Event");
      mHistTitle->Append(*mCountSubs + 1);
      mHistTitle->Append("_Har");
      mHistTitle->Append(*mCountHars + 1);
      histFull[k].histFullHar[j].mHistBinEta =
	new TProfile(mHistTitle->Data(), mHistTitle->Data(), nEtaBins, EtaMin,
		 EtaMax, EtaMin, EtaMax, "");
      histFull[k].histFullHar[j].mHistBinEta->SetXTitle("Pseudorapidity");
      histFull[k].histFullHar[j].mHistBinEta->SetYTitle("<Eta>");
      delete mHistTitle;

      // Mean Pt in each bin
      mHistTitle = new TString("Flow_Bin_Pt_Event");
      mHistTitle->Append(*mCountSubs + 1);
      mHistTitle->Append("_Har");
      mHistTitle->Append(*mCountHars + 1);
      histFull[k].histFullHar[j].mHistBinPt =
	new TProfile(mHistTitle->Data(), mHistTitle->Data(), nPtBins, PtMin,
		 PtMax, PtMin, PtMax, "");
      histFull[k].histFullHar[j].mHistBinPt->SetXTitle("Pseudorapidity");
      histFull[k].histFullHar[j].mHistBinPt->SetYTitle("<Pt>");
      delete mHistTitle;
    }
  }
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
    mQxSub[0][j]    = mFlowTag->qxa[j];
    mQxSub[1][j]    = mFlowTag->qxb[j];
    mQxSub[2][j]    = mFlowTag->qxc[j];
    mQxSub[3][j]    = mFlowTag->qxd[j];
    mQySub[0][j]    = mFlowTag->qya[j];
    mQySub[1][j]    = mFlowTag->qyb[j];
    mQySub[2][j]    = mFlowTag->qyc[j];
    mQySub[3][j]    = mFlowTag->qyd[j];
    mMulSub[0][j]   = mFlowTag->na[j];
    mMulSub[1][j]   = mFlowTag->nb[j];
    mMulSub[2][j]   = mFlowTag->nc[j];
    mMulSub[3][j]   = mFlowTag->nd[j];
    mSumPtSub[0][j] = mFlowTag->spta[j];
    mSumPtSub[1][j] = mFlowTag->sptb[j];
    mSumPtSub[2][j] = mFlowTag->sptc[j];
    mSumPtSub[3][j] = mFlowTag->sptd[j];

    // calculate Psi
    for (int i = 0; i < nSubs; i++) {
      mPsiSub[i][j] = atan2(mQySub[i][j], mQxSub[i][j]) / harN;
      if (mPsiSub[i][j] < 0.) mPsiSub[i][j] += twopi / harN;
    }

    // full event quantities
    for (int k = 0; k < nSubs/2; k++) {
      // add the sub-events
      mQx[k][j]   = mQxSub[2*k][j]    + mQxSub[2*k+1][j];
      mQy[k][j]   = mQySub[2*k][j]    + mQySub[2*k+1][j];
      mMul[k][j]  = mMulSub[2*k][j]   + mMulSub[2*k+1][j];
      mSumPt[k][j]= mSumPtSub[2*k][j] + mSumPtSub[2*k+1][j];

      // calculate
      mQ[k][j]    = sqrt(mQx[k][j]*mQx[k][j] + mQy[k][j]*mQy[k][j]);
      if (mMul[k][j] > 0.) {
	m_q[k][j]  = mQ[k][j] / sqrt(mMul[k][j]);
      } else {
	m_q[k][j]  = 0.;
      }
      mPsi[k][j]  = atan2(mQy[k][j],mQx[k][j]) / harN;
      if (mPsi[k][j] < 0.) mPsi[k][j] += twopi / harN;
    }
  }
  // Multiplicity cut
  return ((mMul[0][1] + mMul[1][1]) >= minMul) ? kStOK : kStErr;
}

void StFlowAnalysisMaker::makeTagHistograms() {
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
      float PsiSubCorr = mPsiSub[2*k][j] - mPsiSub[2*k+1][j];
      histFull[k].mHistCos->Fill(harN,(float)cos(harN*PsiSubCorr));    
      if (PsiSubCorr < 0.) PsiSubCorr += twopi / harN;
      histFull[k].histFullHar[j].mHistPsiSubCorr->Fill(PsiSubCorr);
      if (j < nHars - 1) { // subevents of different harmonics
	int j1, j2;
	float PsiSubCorrDiff;
	if (j==0) {
	  j1 = 1, j2 = 2;	
	} else if (j==1) {
	  j1 = 1, j2 = 3;	
	} else if (j==2) {
	  j1 = 2, j2 = 4;	
	}
	PsiSubCorrDiff = fmod(mPsiSub[2*k][j1-1], twopi/(float)j2) - 
	    fmod(mPsiSub[2*k+1][j2-1], twopi/(float)j2);
	if (PsiSubCorrDiff < 0.) PsiSubCorrDiff += twopi/(float)j2;
	histFull[k].histFullHar[j].mHistPsiSubCorrDiff->
	  Fill(PsiSubCorrDiff);
	PsiSubCorrDiff = fmod(mPsiSub[2*k][j2-1], twopi/(float)j2) - 
	    fmod(mPsiSub[2*k+1][j1-1], twopi/(float)j2);
	if (PsiSubCorrDiff < 0.) PsiSubCorrDiff += twopi/(float)j2;
	histFull[k].histFullHar[j].mHistPsiSubCorrDiff->
	  Fill(PsiSubCorrDiff);
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

void StFlowAnalysisMaker::makeFlowHistograms() {
  // Fill histograms from the particles

  // Initialize Iterator, loop variables, and arrays
  StTrackCollection* tracks = mEvent->trackCollection();
  StTrackIterator itr;
  long initialMultiplicity = tracks->size();
  
  // track loop
  long TrackCount = 0;
  int i;
  float dphi;
  for (itr = tracks->begin(), i=0; itr != tracks->end(); itr++) {
    StGlobalTrack* gtrk = *itr;
    StTrackFitTraits& fitTraits = gtrk->fitTraits();
    Int_t nFitPoints = fitTraits.numberOfFitPoints();
    Int_t nMaxPoints = fitTraits.numberOfPossiblePoints();
    if (nFitPoints > (int)trackquality[0][0]) {
      StThreeVectorD p = gtrk->helix().momentum(bField); 
      Float_t Phi =  p.phi();
      if (Phi < 0.) Phi += twopi;
      Float_t Eta =  p.pseudoRapidity();
      Float_t Pt  =  p.perp();
      for (int k = 0; k < nSubs/2; k++) {
	histFull[k].mHistEtaPtPhi3D->Fill(Eta, Pt, Phi);
	for (int j = 0; j < nHars; j++) {
	  float harN  = (float)(j+1);
	  histFull[k].histFullHar[j].mHistPhi->Fill(Phi);
	  // Remove autocorrelations
	  float weight = 1.;
	  // if (PhiWeight) StFlowMaker::phiWeight(Phi, Eta, Pt, &weight);
	  histFull[k].histFullHar[j].mHistPhiFlat->Fill(Phi,weight);
	  if (Eta < 0 && (j+1) % 2 == 1) weight *= -1.;
	  Float_t Qx_i = weight * cos(Phi * harN);
	  Float_t Qy_i = weight * sin(Phi * harN);
	  Float_t Psi_i = atan2(mQy[k][j] - Qy_i, mQx[k][j] - Qx_i) / harN;
	  if (Psi_i < 0.) Psi_i += twopi / harN;
	  // cout << "k= " << k << " j= " << j << " Psi= " << Psi[k][j] <<
	  //  "\t Psi_i= " << Psi_i << endl;

	  // 2D hists
	  histFull[k].histFullHar[j].mHistYield2D->Fill(Eta, Pt);
	  Float_t v = 100.*cos(harN*(Phi - Psi_i));
	  histFull[k].histFullHar[j].mHistSum_v2D->Fill(Eta, Pt, v);
	  histFull[k].histFullHar[j].mHistBinEta->Fill(Eta, Eta);
	  histFull[k].histFullHar[j].mHistBinPt->Fill(Pt, Pt);

	  // Correlation of phi with Psi
	  if (Eta < 0 && (j+1) % 2 == 1) {
	    Phi += pi; // backward particle and odd harmonic
	    if (Phi > twopi) Phi -= twopi;
	  }
	  dphi = Phi - Psi_i;
	  if (dphi < 0.) dphi += twopi;
	  histFull[k].histFullHar[j].mHistPhiCorr->
	    Fill(fmod(dphi, twopi/harN));
	}
      }
      
      // Finish loop
      TrackCount++;
      i++;
    }
  }
}

static double qDist(double* q, double* par)
  // Calculates the q distribution given the parameters v, mult, area
{
  double expo = par[1]*par[0]*par[0] + q[0]*q[0];
  double dNdq = par[2] * (2. * q[0] * exp(-expo) * 
			  besi0_(2*q[0]*par[0]*sqrt(par[1])));
  return dNdq;
}

Int_t StFlowAnalysisMaker::Finish() {
  // Calculates resolution and mean flow values

  const Float_t qMax      =    2.;
  const Int_t n_qBins     =    50;
  
  // Calculate resolution = sqrt(2)*sqrt(mHistCos)
  Float_t cosPair[nSubs/2][nHars];
  Float_t cosPairErr[nSubs/2][nHars];
  for (int k = 0; k < nSubs/2; k++) {
    for (int j = 0; j < nHars; j++) {
      cosPair[k][j] = histFull[k].mHistCos->GetBinContent(j+1);
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
      const Int_t nEtaBins    =  20;
      const Int_t nPtBins     =  10;
      double zero[nEtaBins+2][nPtBins+2] = {{0.}};
      histFull[k].histFullHar[j].mHistYield2D->SetError(&zero[0][0]);
      histFull[k].histFullHar[j].mHist_vObs2D->
 	Divide(histFull[k].histFullHar[j].mHistSum_v2D,
 	       histFull[k].histFullHar[j].mHistYield2D,1.,1.);

      // Calulate v = vObs / Resolution
      // The systematic error of the resolution is not folded in.
      cout << "# Resolution= " << mRes[k][j] << " +/- " << mResErr[k][j] << endl;
      histFull[k].histFullHar[j].mHist_v2D->
   	Divide(histFull[k].histFullHar[j].mHistSum_v2D,
   	       histFull[k].histFullHar[j].mHistYield2D,1.,1.);
      if (mRes[k][j] != 0.) {
	histFull[k].histFullHar[j].mHist_v2D->Scale(1. / mRes[k][j]);
      } else {
	cout << "### Resolution of the " << j+1 << "th harmonic was zero. ###"
	     << endl;
      }

      // Fit q distribution
      float area = histFull[k].histFullHar[j].mHist_q->Integral() *  
	qMax / (float)n_qBins; 
      float mult = histFull[k].histFullHar[j].mHistMult->GetMean();
      cout << "Mult= " << mult << endl;
      TF1* func_q = new TF1("qDist", qDist, 0., qMax, 3); // fit q dist
      func_q->SetParNames("v", "mult", "area");
      func_q->SetParameters(0.01, mult, area); // initial values
      func_q->SetParLimits(1, 1, 1); // mult is fixed
      func_q->SetParLimits(2, 1, 1); // area is fixed
      histFull[k].histFullHar[j].mHist_q->Fit("qDist", "0");
    }
  }

  writeHistFile ();

  return StMaker::Finish();
}

Int_t StFlowAnalysisMaker::writeHistFile() {
  TString* mHistFileName = new TString("FlowHists.root");
  TFile histFile(mHistFileName->Data(), "RECREATE");

  this->GetHistList()->Write();

  histFile.Close();
  delete mHistFileName;
  return kStOK;
}
