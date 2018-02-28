////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowLeeYangZerosMaker.cxx,v 1.8 2018/02/26 23:10:59 smirnovd Exp $
//
// Authors: Markus Oldenberg and Art Poskanzer, LBNL
//          with advice from Jean-Yves Ollitrault and Nicolas Borghini
//
////////////////////////////////////////////////////////////////////////////
//
// Description:  Maker to analyze Flow using the LeeYangZeros method
//               Selection 1 (k=0) uses the Sum Generating Function
//               Selection 2 (k=1) uses the Product Generating Function
//               Equation numbers are from Big Paper (BP) Nucl. Phys. A 727, 373 (2003)
//               Practical Guide (PG) J. Phys. G: Nucl. Part. Phys. 30, S1213 (2004)
//               Directed Flow (DF) Nucl. Phys. A 742, 130 (2004)
//               The errors come from the variations with theta and batch job variations
//               This treats the acceptance variations with theta as statistical
//               Particles which would normally be correlated with the event plane are analyzed
//               The event plane selections are not used
//               The ZeroPass determines the recentering parameters
//
////////////////////////////////////////////////////////////////////////////

#include <Stiostream.h>
#include <stdlib.h>
#include <math.h>
#include "StMaker.h"
#include "StFlowLeeYangZerosMaker.h"
#include "StFlowMaker/StFlowMaker.h"
#include "StFlowMaker/StFlowEvent.h"
#include "StFlowMaker/StFlowConstants.h"
#include "StFlowMaker/StFlowSelection.h"
#include "StEnumerations.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "TVector2.h"
#include "TFile.h"
#include "TString.h"
#include "TH1.h"
#include "TProfile.h"
#include "TProfile2D.h"
#include "TOrdCollection.h"
#include "StMessMgr.h"
#include "TMath.h"
#include "TComplex.h"
//#include "StTimer.hh"
#define PR(x) cout << "##### FlowLYZ: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowLeeYangZerosMaker)

Bool_t  StFlowLeeYangZerosMaker::mV1Mixed = kTRUE;

//-----------------------------------------------------------------------

StFlowLeeYangZerosMaker::StFlowLeeYangZerosMaker(const Char_t* name): StMaker(name),
  MakerName(name) {
  pFlowSelect = new StFlowSelection();
  SetHistoRanges();
  SetPtRange_for_vEta(0., 0.);
  SetEtaRange_for_vPt(0., 0.);
}

StFlowLeeYangZerosMaker::StFlowLeeYangZerosMaker(const Char_t* name,
					 const StFlowSelection& flowSelect) :
  StMaker(name), MakerName(name) {
  pFlowSelect = new StFlowSelection(flowSelect);
  SetHistoRanges();
  SetPtRange_for_vEta(0., 0.);
  SetEtaRange_for_vPt(0., 0.);
}

//-----------------------------------------------------------------------

StFlowLeeYangZerosMaker::~StFlowLeeYangZerosMaker() {
}

//-----------------------------------------------------------------------

Int_t StFlowLeeYangZerosMaker::Make() {
  // Fill histograms

  // Get a pointer to StFlowEvent
  StFlowMaker* pFlowMaker = NULL;
  pFlowMaker = (StFlowMaker*)GetMaker("Flow");
  if (pFlowMaker) pFlowEvent = pFlowMaker->FlowEventPointer();
  if (pFlowEvent && pFlowSelect->Select(pFlowEvent)) {   // event selected
    if (FillFromFlowEvent()) {                           // fill event histograms
      if (!mFirstPass) { FillParticleHistograms(); }     // fill particle histograms
    } else {
      gMessMgr->Info("##### FlowLeeYangZero: Event Q = 0");
    }
  } else {
    gMessMgr->Info("##### FlowLeeYangZero: FlowEvent pointer null");
    return kStOK;
  }
    
  if (Debug()) StMaker::PrintInfo();
  
  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StFlowLeeYangZerosMaker::Init() {
  // Book histograms

//   timeEvent = StTimer::StTimer();
//   timePart = StTimer::StTimer();
//   timeFinish = StTimer::StTimer();
//   timeInit.start();

  float ptMaxPart = Flow::ptMaxPart;
  if (pFlowSelect->PtMaxPart()) {
    ptMaxPart = pFlowSelect->PtMaxPart();
  }

  mPtBinsPart = Flow::nPtBinsPart;
  if (pFlowSelect->PtBinsPart()) {
    mPtBinsPart = pFlowSelect->PtBinsPart();
  }
  xLabel = "Pseudorapidity";
  if (strlen(pFlowSelect->PidPart()) != 0) { xLabel = "Rapidity"; }

  StFlowMaker* pFlowMaker = NULL;
  pFlowMaker = (StFlowMaker*)GetMaker("Flow");
  Bool_t reCentCalc = pFlowMaker->ReCentCalc();

  const float multMin      =    0.;
  const float multMax      = 2000.;

  enum { nMultBins         = 200
  };

  TString* histTitle;
  mZeroPass  = kFALSE;
  mFirstPass = kFALSE;
  mV1Mixed   = kTRUE;
  
  // Make array of G(r) bins
  // Initial bin width is smaller than the mean bin width by the factor F
  // The increase in bin width per channel is eps
  Double_t rBinArray[Flow::nRBins+1];
  Double_t meanBinWidth = Flow::rMax / Flow::nRBins;
  //Double_t F = 1.; // for equal size bins
  Double_t F = 8.;
  Double_t initialBinWidth = meanBinWidth / F;
  Double_t eps = 2. * meanBinWidth * (1. - 1./F) / (double)(Flow::nRBins-1);
  rBinArray[0]= 0.;
//   PR(meanBinWidth);
//   PR(initialBinWidth);
//   PR(eps);
  Double_t binWidth = initialBinWidth;
  for (int rBin = 1; rBin < Flow::nRBins+1; rBin++) {
    rBinArray[rBin] = rBinArray[rBin-1] + binWidth;
//     cout << rBin << ": " << binWidth << ", " << rBinArray[rBin] << endl;
    binWidth += eps;
  }

  // Yield of particles
  mHistYieldPartPt = new TH1D("FlowLYZ_YieldPartPt", "FlowLYZ_YieldPartPt", mPtBinsPart,
			      Flow::ptMin, ptMaxPart);
  mHistYieldPartPt->SetXTitle("Pt (GeV/c)");

  mHistYieldPartEta = new TH1D("FlowLYZ_YieldPartEta", "FlowLYZ_YieldPartEta", mNEtaBins,
			       mEtaMin, mEtaMax);
  mHistYieldPartEta->SetXTitle((char*)xLabel.Data());

  // multiplicity
  mHistMult = new TH1F("FlowLYZ_Mult", "FlowLYZ_Mult", nMultBins, multMin, multMax);
  mHistMult->SetXTitle("Multiplicity");
  mHistMult->SetYTitle("Counts");

  mNEvents = 0;

  // neumerator for v1 mixed harmonics: DF Eq. 7
  for (int Ntheta = 0; Ntheta < Flow::nTheta; Ntheta++) {
    mV1neum[Ntheta](0., 0.);
  }

  // for each selection
  for (int k = 0; k < Flow::nSels; k++) {

    // V
    histTitle = new TString("FlowProLYZ_V_Sel");
    *histTitle += k+1;
    histFull[k].mHistPro_V = new TProfile(histTitle->Data(), histTitle->Data(),
      Flow::nHars, 0.5, (float)(Flow::nHars) + 0.5, -1000., 1000.);
    histFull[k].mHistPro_V->SetXTitle("Harmonic");
    histFull[k].mHistPro_V->SetYTitle("mean V_{n}");
    delete histTitle;
    
    // "v integrated" from r0
    histTitle = new TString("FlowProLYZ_vr0_Sel");
    *histTitle += k+1;
    histFull[k].mHistPro_vr0 = new TProfile(histTitle->Data(), histTitle->Data(),
      Flow::nHars, 0.5, (float)(Flow::nHars) + 0.5, -100., 100.);
    histFull[k].mHistPro_vr0->SetXTitle("Harmonic");
    histFull[k].mHistPro_vr0->SetYTitle("v from r0 (%)");
    delete histTitle;
    
    // v doubly integrated
    histTitle = new TString("FlowLYZ_v_Sel");
    *histTitle += k+1;
    histFull[k].mHist_v = new TH1F(histTitle->Data(), histTitle->Data(),
      Flow::nHars, 0.5, (float)(Flow::nHars) + 0.5);
    histFull[k].mHist_v->SetXTitle("Harmonic");
    histFull[k].mHist_v->SetYTitle("v (%)");
    delete histTitle;
    
    // for each harmonic
    for (int j = 0; j < Flow::nHars; j++) {
      mQ[k][j].Set(0.,0.);
      mQ2[k][j] = 0.;

      // Flow
      histTitle = new TString("FlowProLYZ_vEta_Sel");
      *histTitle += k+1;
      *histTitle += "_Har";
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHistPro_vEta = new TProfile(histTitle->Data(),
        histTitle->Data(), mNEtaBins, mEtaMin, mEtaMax, -1000., 1000.);
      histFull[k].histFullHar[j].mHistPro_vEta->SetXTitle((char*)xLabel.Data());
      histFull[k].histFullHar[j].mHistPro_vEta->SetYTitle("v (%)");
      delete histTitle;

      histTitle = new TString("FlowProLYZ_vPt_Sel");
      *histTitle += k+1;
      *histTitle += "_Har";
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHistPro_vPt = new TProfile(histTitle->Data(),
        histTitle->Data(), mPtBinsPart, Flow::ptMin, ptMaxPart, -1000., 1000.);
      histFull[k].histFullHar[j].mHistPro_vPt->SetXTitle("Pt (GeV/c)");
      histFull[k].histFullHar[j].mHistPro_vPt->SetYTitle("v (%)");
      delete histTitle;
 
      // for each theta
      for (Int_t Ntheta = 0; Ntheta < Flow::nTheta; Ntheta++) {

	// Gtheta
	histTitle = new TString("FlowLYZ_Gtheta");
	*histTitle += Ntheta;
	*histTitle += "_Sel";
	*histTitle += k+1;
	*histTitle += "_Har";
	*histTitle += j+1;
	histFull[k].histFullHar[j].histTheta[Ntheta].mHistGtheta = new TH1D(histTitle->Data(),
	  histTitle->Data(), Flow::nRBins, rBinArray);
	histFull[k].histFullHar[j].histTheta[Ntheta].mHistGtheta->SetXTitle("r");
	histFull[k].histFullHar[j].histTheta[Ntheta].mHistGtheta->SetYTitle("|G^{#theta}(ir)|^{2}");
	delete histTitle;

	// Re(Gtheta)
	histTitle = new TString("FlowReGtheta");
	*histTitle += Ntheta;
	*histTitle += "_Sel";
	*histTitle += k+1;
	*histTitle += "_Har";
	*histTitle += j+1;
	histFull[k].histFullHar[j].histTheta[Ntheta].mHistProReGtheta = new TProfile(histTitle->Data(),
	  histTitle->Data(), Flow::nRBins, rBinArray);
	histFull[k].histFullHar[j].histTheta[Ntheta].mHistProReGtheta->SetXTitle("r");
	histFull[k].histFullHar[j].histTheta[Ntheta].mHistProReGtheta->SetYTitle("Re(G^{#theta}(ir))");
	delete histTitle;

	// Im(Gtheta)
	histTitle = new TString("FlowImGtheta");
        *histTitle += Ntheta;
	*histTitle += "_Sel";
	*histTitle += k+1;
	*histTitle += "_Har";
	*histTitle += j+1;
	histFull[k].histFullHar[j].histTheta[Ntheta].mHistProImGtheta = new TProfile(histTitle->Data(),
	  histTitle->Data(), Flow::nRBins, rBinArray);
	histFull[k].histFullHar[j].histTheta[Ntheta].mHistProImGtheta->SetXTitle("r");
	histFull[k].histFullHar[j].histTheta[Ntheta].mHistProImGtheta->SetYTitle("Im(G^{#theta}(ir))");
	delete histTitle;

	// Re(Numerator) 2D
	histTitle = new TString("FlowReNumer2D");
	*histTitle += Ntheta;
	*histTitle += "_Sel";
	*histTitle += k+1;
	*histTitle += "_Har";
	*histTitle += j+1;
	histFull[k].histFullHar[j].histTheta[Ntheta].mHistReNumer2D = new TProfile2D(histTitle->Data(),
	  histTitle->Data(), 2, mEtaMin, mEtaMax, mPtBinsPart, Flow::ptMin, ptMaxPart);
	histFull[k].histFullHar[j].histTheta[Ntheta].mHistReNumer2D->SetXTitle((char*)xLabel.Data());
	histFull[k].histFullHar[j].histTheta[Ntheta].mHistReNumer2D->SetYTitle("Pt (GeV/c)");
	delete histTitle;
	
	// Re(Numerator)(eta)
	histTitle = new TString("FlowReNumerEta");
	*histTitle += Ntheta;
	*histTitle += "_Sel";
	*histTitle += k+1;
	*histTitle += "_Har";
	*histTitle += j+1;
	histFull[k].histFullHar[j].histTheta[Ntheta].mHistReNumerEta = new TProfile(histTitle->Data(),
	  histTitle->Data(), mNEtaBins, mEtaMin, mEtaMax);
	histFull[k].histFullHar[j].histTheta[Ntheta].mHistReNumerEta->SetXTitle((char*)xLabel.Data());
	histFull[k].histFullHar[j].histTheta[Ntheta].mHistReNumerEta->SetYTitle("v (%)");
	delete histTitle;
	
	// Im(Numerator) 2D
	histTitle = new TString("FlowImNumer2D");
	*histTitle += Ntheta;
	*histTitle += "_Sel";
	*histTitle += k+1;
	*histTitle += "_Har";
	*histTitle += j+1;
	histFull[k].histFullHar[j].histTheta[Ntheta].mHistImNumer2D = new TProfile2D(histTitle->Data(),
	  histTitle->Data(), 2, mEtaMin, mEtaMax, mPtBinsPart, Flow::ptMin, ptMaxPart);
	histFull[k].histFullHar[j].histTheta[Ntheta].mHistImNumer2D->SetXTitle((char*)xLabel.Data());
	histFull[k].histFullHar[j].histTheta[Ntheta].mHistImNumer2D->SetYTitle("Pt (GeV/c)");
	delete histTitle;
	
	// Im(Numerator)(eta)
	histTitle = new TString("FlowImNumerEta");
	*histTitle += Ntheta;
	*histTitle += "_Sel";
	*histTitle += k+1;
	*histTitle += "_Har";
	*histTitle += j+1;
	histFull[k].histFullHar[j].histTheta[Ntheta].mHistImNumerEta = new TProfile(histTitle->Data(),
	  histTitle->Data(), mNEtaBins, mEtaMin, mEtaMax);
	histFull[k].histFullHar[j].histTheta[Ntheta].mHistImNumerEta->SetXTitle((char*)xLabel.Data());
	histFull[k].histFullHar[j].histTheta[Ntheta].mHistImNumerEta->SetYTitle("v (%)");
	delete histTitle;
	
      } // theta

      // V
      TString *histTitleVtheta;
      histTitle = new TString("FlowProLYZ_Vtheta_Sel");
      *histTitle += k+1;
      *histTitle += "_Har";
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHistPro_Vtheta = new TProfile(histTitle->Data(),
        histTitle->Data(), Flow::nTheta, -0.5, Flow::nTheta-0.5, -1000., 1000.);
      histFull[k].histFullHar[j].mHistPro_Vtheta->SetXTitle("#theta");
      histFull[k].histFullHar[j].mHistPro_Vtheta->SetYTitle("V_{n}^{#theta}");
      histTitleVtheta = new TString(histTitle->Data());
      delete histTitle;

      // Re(Denom)
      histTitle = new TString("FlowReDenom_Sel");
      *histTitle += k+1;
      *histTitle += "_Har";
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHistReDenom = new TProfile(histTitle->Data(),
        histTitle->Data(), Flow::nTheta, -0.5, Flow::nTheta-0.5);
      histFull[k].histFullHar[j].mHistReDenom->SetXTitle("#theta");
      histFull[k].histFullHar[j].mHistReDenom->SetYTitle("Re(Q^{#theta}e^{ir_{0}^{#theta}Q^{#theta}})");
      delete histTitle;

      // Im(Denom)
      histTitle = new TString("FlowImDenom_Sel");
      *histTitle += k+1;
      *histTitle += "_Har";
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHistImDenom = new TProfile(histTitle->Data(),
        histTitle->Data(), Flow::nTheta, -0.5, Flow::nTheta-0.5);
      histFull[k].histFullHar[j].mHistImDenom->SetXTitle("#theta");
      histFull[k].histFullHar[j].mHistImDenom->SetYTitle("Im(Q^{#theta}e^{ir_{0}^{#theta}Q^{#theta}})");
      delete histTitle;
    
      // Recenter
      histTitle = new TString("FlowCentX_Sel");
      *histTitle += k+1;
      *histTitle += "_Har";
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHistCentX = new TProfile(histTitle->Data(),
        histTitle->Data(), 3, 0.5, 3.5);
      histFull[k].histFullHar[j].mHistCentX->SetXTitle("TPCE, TPCW, TPC");
      histFull[k].histFullHar[j].mHistCentX->SetYTitle("<cos n #phi>");
      delete histTitle;
    
      histTitle = new TString("FlowCentY_Sel");
      *histTitle += k+1;
      *histTitle += "_Har";
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHistCentY = new TProfile(histTitle->Data(),
        histTitle->Data(), 3, 0.5, 3.5);
      histFull[k].histFullHar[j].mHistCentY->SetXTitle("TPCE, TPCW, TPC");
      histFull[k].histFullHar[j].mHistCentY->SetYTitle("<sin n #phi>");
      delete histTitle;
    
      histTitle = new TString("FlowQCent_Sel");
      *histTitle += k+1;
      *histTitle += "_Har";
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHistQCent = new TProfile(histTitle->Data(),
        histTitle->Data(), 2, 0.5, 2.5);
      histFull[k].histFullHar[j].mHistQCent->SetXTitle("X, Y");
      histFull[k].histFullHar[j].mHistQCent->SetYTitle("<Q_{n}/M>");
      delete histTitle;
    
      // r0
      histTitle = new TString("FlowProLYZ_r0theta_Sel");
      *histTitle += k+1;
      *histTitle += "_Har";
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHistPro_r0theta = new TProfile(histTitle->Data(),
        histTitle->Data(), Flow::nTheta, -0.5, Flow::nTheta-0.5, 0., 2.);
      histFull[k].histFullHar[j].mHistPro_r0theta->SetXTitle("#theta");
      histFull[k].histFullHar[j].mHistPro_r0theta->SetYTitle("r_{0}^{#theta}");
      delete histTitle;

      if (j > 1) continue; // only for first two harmonics
      TString *histTitleForReadIn;
      histTitleForReadIn = new TString("FlowLYZ_r0theta_Sel");
      *histTitle += k+1;
      *histTitle += "_Har";
      *histTitle += j+1;
 
      // Zero pass for recenter paramerters?
      TFile fileZeroPass("flow.reCent.root","R");
      if (fileZeroPass.IsOpen() || !reCentCalc) { // not zero pass

	// Read the hists from the first pass file
	TFile fileFirstPass("flow.firstPassLYZ.root","R");
	if (fileFirstPass.IsOpen()) { // second pass
	  gMessMgr->Info("##### FlowLeeYangZero: Second Pass");
	  //fileFirstPass.ls();
	  TH1D* tempHist = 
	    dynamic_cast<TH1D*>(fileFirstPass.Get(histTitleForReadIn->Data()));
	  if (!tempHist) {
	    cout << "##### FlowLeeYangZeros: dynamic cast can't find " <<
	      histTitleForReadIn->Data() << endl;
	    return kFALSE;
	  } else if (tempHist->GetNbinsX() != Flow::nTheta) {
	    cout << "##### FlowLeeYangZeros: nTheta of 1st pass not equal to 2nd pass" <<
	      endl;
	    PR(tempHist->GetNbinsX());
	    PR(Flow::nTheta);
	    return kStFatal;
	  }   
	  delete  histTitleForReadIn;
	  
	  for (Int_t Ntheta = 0; Ntheta < Flow::nTheta; Ntheta++) {
	    mr0theta[k][j][Ntheta] = tempHist->GetBinContent(Ntheta+1);
	    mr0theta[k][j+2][Ntheta] = tempHist->GetBinContent(Ntheta+1); // for higher harmonics
	    //cout << k << " " << j << " " << Ntheta << " " << mr0theta[k][j][Ntheta] << endl;
	  }
	  fileFirstPass.Close();
	} else {	
	  gMessMgr->Info("##### FlowLeeYangZero: First Pass");
	  mFirstPass = kTRUE;
	}
      } else {	
	gMessMgr->Info("##### FlowLeeYangZero: Zero Pass");
	mZeroPass = kTRUE;
      }
    } // j
  } // k

  gMessMgr->SetLimit("##### FlowLeeYangZero", 5);
  gMessMgr->Info("##### FlowLeeYangZero: $Id: StFlowLeeYangZerosMaker.cxx,v 1.8 2018/02/26 23:10:59 smirnovd Exp $");

  return StMaker::Init();
}

//-----------------------------------------------------------------------

Bool_t StFlowLeeYangZerosMaker::FillFromFlowEvent() {
  // Get event quantities from StFlowEvent for all particles
//   timeEvent.start();

  // multiplicity
  mMult = (int)pFlowEvent->MultPart(pFlowSelect);
  mHistMult->Fill((float)mMult);

  mNEvents++; // increment number of events

  TVector2 Q, q;
  Float_t theta, theta1, order, r0;
  TComplex expo, denom, Gtheta;
  Double_t Qtheta, cosTheta12, mult;
  Int_t m;

  for (int k = 0; k < Flow::nSels; k++) {
    pFlowSelect->SetSelection(k);
    for (int j = 0; j < Flow::nHars; j++) {
      m = 1;
      pFlowSelect->SetHarmonic(j); // for integrated flow and denominator
      if (j==2) {
	m = 3;
	pFlowSelect->SetHarmonic(0);
      } else if (j==3) {
	m = 2;
	pFlowSelect->SetHarmonic(1);
      }
      order  = (double)((j+1)/m);

      if (mZeroPass) {
	// calculate recentering parameters
	q = pFlowEvent->ReCentPar(pFlowSelect,"TPCE");
	histFull[k].histFullHar[j].mHistCentX->Fill(1., q.X());
	histFull[k].histFullHar[j].mHistCentY->Fill(1., q.Y());
	q = pFlowEvent->ReCentPar(pFlowSelect,"TPCW");
	histFull[k].histFullHar[j].mHistCentX->Fill(2., q.X());
	histFull[k].histFullHar[j].mHistCentY->Fill(2., q.Y());
	q = pFlowEvent->ReCentPar(pFlowSelect,"TPC");
	histFull[k].histFullHar[j].mHistCentX->Fill(3., q.X());
	histFull[k].histFullHar[j].mHistCentY->Fill(3., q.Y());

	continue;;
      }

      // event Q
      Q = pFlowEvent->QPart(pFlowSelect);
      if (Q.Mod() == 0.) return kFALSE; // to eliminate Q=0
      mQ[k][j]  += Q;                   // for chi calculation
      mQ2[k][j] += Q.Mod2();
     
      if (!mZeroPass) {
	// test recentering of Q per particle
	mult = (double)(pFlowEvent->MultPart(pFlowSelect));
	histFull[k].histFullHar[j].mHistQCent->Fill(1., Q.X()/mult);
	histFull[k].histFullHar[j].mHistQCent->Fill(2., Q.Y()/mult);
      }

      // for each theta
      for (int Ntheta = 0; Ntheta < Flow::nTheta; Ntheta++) {
	theta = ((float)Ntheta / (float)Flow::nTheta) * TMath::Pi()/order;
	if (j<=1) {
	  mQtheta[k][j][Ntheta] = pFlowEvent->Qtheta(pFlowSelect, theta);
	} else {
	  mQtheta[k][j][Ntheta] = mQtheta[k][j-2][Ntheta];
	}
	Qtheta = mQtheta[k][j][Ntheta];

	if (mFirstPass && j<=1) {
	  // G for "integrated v"
	  for (int rBin = 1; rBin < Flow::nRBins; rBin++) {
	    Float_t r = histFull[k].histFullHar[j].histTheta[Ntheta].mHistGtheta->GetBinCenter(rBin);
	    if (!k) { // Sum G
	      expo(0., r * Qtheta);
	      Gtheta = TComplex::Exp(expo); // BP Eq. 6
	    } else if (!mV1Mixed || j) {  // Product G, skip for v1 mixed
	      Gtheta = pFlowEvent->Grtheta(pFlowSelect, r, theta);  // PG Eq. 3
	      if (Gtheta.Rho2() > 1000.) { break; } // stop when G gets too big
	    }
	    histFull[k].histFullHar[j].histTheta[Ntheta].mHistProReGtheta->Fill(r, Gtheta.Re());
	    histFull[k].histFullHar[j].histTheta[Ntheta].mHistProImGtheta->Fill(r, Gtheta.Im());
	  }
	} else if (!mFirstPass) {
	  // denominator for differential v, and "integrated" v1 mixed
	  r0 = mr0theta[k][j][Ntheta];
	  if (!k) {
	    expo(0., r0 * Qtheta);
	    denom = Qtheta * TComplex::Exp(expo); // BP Eq. 12
	  } else {
	    mGr0theta[k][j][Ntheta] = pFlowEvent->Grtheta(pFlowSelect, r0, theta);
	    denom = mGr0theta[k][j][Ntheta] *
	      pFlowEvent->Gder_r0theta(pFlowSelect, r0, theta);  // PG Eq. 9 & DF Eq. 5
	  }
	  histFull[k].histFullHar[j].mHistReDenom->Fill(Ntheta, denom.Re());
	  histFull[k].histFullHar[j].mHistImDenom->Fill(Ntheta, denom.Im());
	} // second pass
      } // theta
    } // j
  } // k
  
  // for v1 mixed harmonics: neumerator of DF Eq. 7
  // also stores G for use in DF Eq. 8
  if (!mFirstPass && mV1Mixed) {
    for (int Ntheta = 0; Ntheta < Flow::nTheta; Ntheta++) {
      theta = ((float)Ntheta / (float)Flow::nTheta) * TMath::Pi()/2.; // theta = theta2
      r0 = mr0theta[1][1][Ntheta];  // selection 2, harmonic 2
      TComplex theta1Term(0.,0.);
      for (int Ntheta1 = 0; Ntheta1 < Flow::nTheta1; Ntheta1++) {
	theta1 = ((float)Ntheta1 / (float)Flow::nTheta1) * 2. * TMath::Pi();
	cosTheta12 = cos(2.*(theta1 - theta));
	mGV1r0theta[Ntheta1][Ntheta] = pFlowEvent->GV1r0theta(pFlowSelect, r0, theta1, theta);
	theta1Term += (cosTheta12 * mGV1r0theta[Ntheta1][Ntheta]);
	//cout << Ntheta << ", " << Ntheta1 << ": " << cosTheta12 << " * " << mGV1r0theta[Ntheta1][Ntheta] << endl;
      }
      mV1neum[Ntheta] += (theta1Term / (double)Flow::nTheta1); // averaged over theta1
      //cout << Ntheta << " :" << mV1neum[Ntheta] << endl;
    }
  }

//   timeEvent.stop();
  return kTRUE;
}

//-----------------------------------------------------------------------

void StFlowLeeYangZerosMaker::FillParticleHistograms() {
  // Fill histograms from the particles on 2nd pass
//   timePart.start();

  // Initialize Iterator
  StFlowTrackCollection* pFlowTracks = pFlowEvent->TrackCollection();
  StFlowTrackIterator itr;
  
  Float_t theta, theta1, phi, eta, pt, m, r0;
  TVector2 reCent, reCent2, reCent1;
  Double_t order, wgt, wgt2, cosTerm, reCentTheta, reCentTheta2, reCentTheta1;
  TComplex expo, numer, cosTermComp;
  for (itr = pFlowTracks->begin(); itr != pFlowTracks->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;

    phi = pFlowTrack->Phi();
    if (phi < 0.) phi += twopi;
    eta = pFlowTrack->Eta();
    pt  = pFlowTrack->Pt();

    // Yield
    if (pFlowSelect->SelectPart(pFlowTrack)) {
      if (mPtRange_for_vEta[1] > mPtRange_for_vEta[0]) { // cut is used
	if (pt < mPtRange_for_vEta[1] && pt >= mPtRange_for_vEta[0]) { // check cut range
	  mHistYieldPartEta->Fill(eta);
	}
      } else { // cut is not used, fill in any case
	mHistYieldPartEta->Fill(eta);
      }
//       if (strlen(pFlowSelect->PidPart()) != 0) { // identified particles not implemented
// 	Float_t rapidity = pFlowTrack->Y();
// 	mHistYieldPartEta->Fill(rapidity);
//       } else {
// 	mHistYieldPartEta->Fill(eta);
//       }
      if (mEtaRange_for_vPt[1] > mEtaRange_for_vPt[0]) { // cut is used
	if (TMath::Abs(eta) < mEtaRange_for_vPt[1] && TMath::Abs(eta) >= mEtaRange_for_vPt[0]) { // check cut range
	  mHistYieldPartPt->Fill(pt);
	}
      } else { // cut is not used, fill in any case
	mHistYieldPartPt->Fill(pt);
      }
    }

    for (int k = 0; k < Flow::nSels; k++) {
      pFlowSelect->SetSelection(k);
      for (int j = 0; j < Flow::nHars; j++) {
	m = 1.; // float
	pFlowSelect->SetHarmonic(j);
	if (j==2) { m = 3.; } // for cos(m*n...)
	else if (j==3) {  m = 2.; }
	order  = (double)(j+1)/m; // for theta calc.

       	// Caculate numerator for all particles selected
	if (pFlowSelect->SelectPart(pFlowTrack)) {
	  reCent = pFlowEvent->ReCent(k, j, pFlowTrack);
	  for (int Ntheta = 0; Ntheta < Flow::nTheta; Ntheta++) {
	    theta = ((float)Ntheta / (float)Flow::nTheta) * TMath::Pi()/order;
	    reCentTheta = reCent.X() * cos(m*order*theta) + reCent.Y() * sin(m*order*theta);
	    r0 = mr0theta[k][j][Ntheta];
	    if (!k) {
	      expo(0., r0 * mQtheta[k][j][Ntheta]);
	      numer = (cos(m*order*(phi - theta)) - reCentTheta) * TComplex::Exp(expo); // BP Eq. 12
	    } else {
	      wgt = pFlowEvent->Weight(k, j, pFlowTrack);
	      if (!j && mV1Mixed) { // for v1 mixed harmonic differential flow
		theta = ((float)Ntheta / (float)Flow::nTheta) * TMath::Pi()/2.; // goes only to pi/2
		r0 = mr0theta[k][1][Ntheta]; // use r0 for 2nd harmonic
		wgt2 = pFlowEvent->Weight(k, 1, pFlowTrack); // weight for 2nd harmonic
		reCent2 = pFlowEvent->ReCent(k, 1, pFlowTrack);  // for 2nd harmonic
		reCentTheta2 = reCent2.X() * cos(2.*theta) + reCent2.Y() * sin(2.*theta);
		double Gim2 = r0 * wgt2 * (cos(2*(phi - theta)) - reCentTheta2);
		TComplex theta1Term(0.,0.);
		for (int Ntheta1 = 0; Ntheta1 < Flow::nTheta1; Ntheta1++) { // loop over theta1
		  theta1 = ((float)Ntheta1 / (float)Flow::nTheta1) * 2. * TMath::Pi();
		  reCentTheta1 = reCent.X() * cos(1.*theta1) + reCent.Y() * sin(1.*theta1); // for 1st harmonic
		  double Gim1 = r0 * Flow::epsV1 * wgt * (cos(phi - theta1) - reCentTheta1);
		  TComplex Gr0denom(1., Gim1+Gim2);
		  TComplex Gr0neum(0., r0 * Flow::epsV1 * (cos(phi - theta1)) - reCentTheta1);
		  TComplex Gder_r0theta = mGV1r0theta[Ntheta1][Ntheta] * Gr0neum / Gr0denom; // DF Eq. 8
		  Double_t cosTheta12 = cos(2.*(theta1 - theta));
		  theta1Term += (cosTheta12 * Gder_r0theta);
		}
		numer = theta1Term / (double)Flow::nTheta1; // DF Eq. 9 neumerator averaged over theta1
	      } else if (j==2 && mV1Mixed) { // 3rd harmonic not defined for mixed
	      	numer = 0.;
	      } else {
		cosTerm = cos(m*order*(phi - theta)) - reCentTheta;
		cosTermComp(1., r0*cosTerm);
		numer = mGr0theta[k][j][Ntheta] * cosTerm / cosTermComp;  // PG Eq. 9
	      }
	    }
	    	    
	    if (mPtRange_for_vEta[1] > mPtRange_for_vEta[0]) { // cut is used
	      if (pt < mPtRange_for_vEta[1] && pt >= mPtRange_for_vEta[0]) { // check cut range
		histFull[k].histFullHar[j].histTheta[Ntheta].mHistReNumerEta->Fill(eta, numer.Re());
		histFull[k].histFullHar[j].histTheta[Ntheta].mHistImNumerEta->Fill(eta, numer.Im());
	      }
	    }
	    else { // cut is not used, fill in any case
	      histFull[k].histFullHar[j].histTheta[Ntheta].mHistReNumerEta->Fill(eta, numer.Re());
	      histFull[k].histFullHar[j].histTheta[Ntheta].mHistImNumerEta->Fill(eta, numer.Im());
	    }
	    
	    if (mEtaRange_for_vPt[1] > mEtaRange_for_vPt[0]) { // cut is used
	      if (TMath::Abs(eta) < mEtaRange_for_vPt[1] && TMath::Abs(eta) >= mEtaRange_for_vPt[0]) { // check cut range
		histFull[k].histFullHar[j].histTheta[Ntheta].mHistReNumer2D->Fill(eta, pt, numer.Re());
		histFull[k].histFullHar[j].histTheta[Ntheta].mHistImNumer2D->Fill(eta, pt, numer.Im());
	      }
	    }
	    else { // cut is not used, fill in any case
	      histFull[k].histFullHar[j].histTheta[Ntheta].mHistReNumer2D->Fill(eta, pt, numer.Re());
	      histFull[k].histFullHar[j].histTheta[Ntheta].mHistImNumer2D->Fill(eta, pt, numer.Im());
	    }

	  } // theta
	} // select
      } // j
    } // k  
  } // track

//   timePart.stop();
}

//-----------------------------------------------------------------------

Int_t StFlowLeeYangZerosMaker::Finish() {
  // In the zero pass write out the recentering parameters
  // In the first pass, from Gtheta find the first minimum to get r0(theta) and write it out.
  // In the second pass calculate V(theta), average over theta, and then calculate v 
//   timeFinish.start();

  TOrdCollection* savedHistNames          = new TOrdCollection(Flow::nSels * Flow::nHars);
  TOrdCollection* savedHistFirstPassNames = new TOrdCollection(Flow::nSels * Flow::nTheta * 3);
  TOrdCollection* savedHistZeroPassNames  = new TOrdCollection(Flow::nSels * Flow::nHars * 2);
  TString* histTitle;

  cout << endl << "##### LeeYangZeros Maker:" << endl;

  Float_t reCentX, reCentY;
  if (mFirstPass) { cout << "Test of recentering of Q vector per particle:" << endl; }
  for (int k = 0; k < Flow::nSels; k++) {
    for (int j = 0; j < Flow::nHars; j++) {
      if (mZeroPass) {
	reCentX   = histFull[k].histFullHar[j].mHistCentX->GetBinContent(3);
	reCentY   = histFull[k].histFullHar[j].mHistCentY->GetBinContent(3);
	cout << setprecision(3) << "Sel = " << k+1 << ", Har = " << j+1 << " : reCent_x = " << reCentX
	     << ", reCent_y = " << reCentY << endl;
	reCentX   = histFull[k].histFullHar[j].mHistCentX->GetBinContent(1);
	reCentY   = histFull[k].histFullHar[j].mHistCentY->GetBinContent(1);
	cout << setprecision(3) << "Sel = " << k+1 << ", Har = " << j+1 << " : reCentE_x = " << reCentX
	     << ", reCentE_y = " << reCentY << endl;
	reCentX   = histFull[k].histFullHar[j].mHistCentX->GetBinContent(2);
	reCentY   = histFull[k].histFullHar[j].mHistCentY->GetBinContent(2);
	cout << setprecision(3) << "Sel = " << k+1 << ", Har = " << j+1 << " : reCentW_x = " << reCentX
	     << ", reCentW_y = " << reCentY << endl;
	savedHistZeroPassNames->AddLast(histFull[k].histFullHar[j].mHistCentX);
	savedHistZeroPassNames->AddLast(histFull[k].histFullHar[j].mHistCentY);
      } else if (mFirstPass) {
	reCentX   = histFull[k].histFullHar[j].mHistQCent->GetBinContent(1);
	reCentY   = histFull[k].histFullHar[j].mHistQCent->GetBinContent(2);
	cout << setprecision(3) << "Sel = " << k+1 << ", Har = " << j+1 << " : reCentedQ_x = " << reCentX
	     << ", reCentedQ_y = " << reCentY << endl;
	savedHistFirstPassNames->AddLast(histFull[k].histFullHar[j].mHistQCent);
      } else {
	savedHistNames->AddLast(histFull[k].histFullHar[j].mHistQCent);
      }
    }
  }
  cout << endl;
  if (mZeroPass) {
    TFile fileZeroPass("flow.reCent.root", "RECREATE");
    fileZeroPass.cd();
    savedHistZeroPassNames->Write();
    fileZeroPass.Close();
    delete savedHistZeroPassNames;
    delete pFlowSelect;
    
    return StMaker::Finish();
  }
  
  cout << "integrated flow: (errors just show variation with theta)" << endl;

  Float_t reG, imG, reNumer, imNumer, reDenom, imDenom, reDiv;
  TComplex Gtheta, denom, numer, div;
  TComplex i = TComplex::I();
  Float_t mult = mHistMult->GetMean();
  Float_t _v, vErr, Vtheta, V, V1SqTheta, V1Sq, yield, eta, pt;
  Double_t r0, yieldSum, vSum, err2Sum, Glast, G0, Gnext, GnextNext, v1DiffConst;
  Float_t Xlast, X0, Xnext, sigma2, chi;
  Float_t BesselRatio[3] = {1., 1.202, 2.69}; // is 2.69 correct?
  Int_t m, v2Sign;
  Bool_t etaPtNoCut;
  Double_t T, B, F;
  if (!mFirstPass) {
    Int_t etaBins = mHistYieldPartEta->GetNbinsX();
    T = mHistYieldPartEta->Integral(0, etaBins+1);
    B = mHistYieldPartEta->Integral(0, etaBins/2);
    F = mHistYieldPartEta->Integral(etaBins/2+1, etaBins+1);
    //cout << "bins= " << etaBins << ", B= " << B << ", F= " << F << ", T= " << T << endl;
  }

  int rBins = Flow::nRBins;
  for (int k = 0; k < Flow::nSels; k++) {
    for (int j = 0; j < Flow::nHars; j++) {
      bool oddHar = (j+1) % 2;
      m = 1; // int for index of BesselRatio and i^^(m-1)
      if (j==2) { m = 3; }
      else if (j==3) { m = 2; }
      if (mFirstPass && j<=1) {
	for (int Ntheta = 0; Ntheta < Flow::nTheta; Ntheta++) {
	  for (int rBin = 1; rBin <= rBins; rBin++) {
	    
	    // "Integrated flow"
	    // G, the generating function, as a function of r for each theta
	    reG = histFull[k].histFullHar[j].histTheta[Ntheta].mHistProReGtheta->GetBinContent(rBin);
	    imG = histFull[k].histFullHar[j].histTheta[Ntheta].mHistProImGtheta->GetBinContent(rBin);
	    Gtheta(reG, imG); // BP Eqs. 6 & A1
	    histFull[k].histFullHar[j].histTheta[Ntheta].mHistGtheta->SetBinContent(rBin, Gtheta.Rho2());
	  } // rBin
	  // Find first minimum of the square of the modulus of G for each theta
	  r0 = histFull[k].histFullHar[j].histTheta[Ntheta].mHistGtheta->GetXaxis()->
	    GetBinCenter(rBins); // default value if no minimum
	  for (int N = 2; N < rBins-1; N++) {
	    G0        = histFull[k].histFullHar[j].histTheta[Ntheta].mHistGtheta->GetBinContent(N);
	    Gnext     = histFull[k].histFullHar[j].histTheta[Ntheta].mHistGtheta->GetBinContent(N+1);
	    GnextNext = histFull[k].histFullHar[j].histTheta[Ntheta].mHistGtheta->GetBinContent(N+2);
	    
	    if (Gnext > G0 && GnextNext > G0) { // next two points are higher (footnote 3)
	      Glast     = histFull[k].histFullHar[j].histTheta[Ntheta].mHistGtheta->GetBinContent(N-1);
	      Xlast     = histFull[k].histFullHar[j].histTheta[Ntheta].mHistGtheta->GetBinCenter(N-1);
	      X0        = histFull[k].histFullHar[j].histTheta[Ntheta].mHistGtheta->GetBinCenter(N);
	      Xnext     = histFull[k].histFullHar[j].histTheta[Ntheta].mHistGtheta->GetBinCenter(N+1);
	      if (X0 < 0.005) break; // treat like no minimum
	      r0 = X0;
	      r0 -= ((X0 - Xlast)*(X0 - Xlast)*(G0 - Gnext) - (X0 - Xnext)*(X0 - Xnext)*(G0 - Glast)) /
		(2.*((X0 - Xlast)*(G0 - Gnext) - (X0 - Xnext)*(G0 - Glast))); // intopolated minimum
	      // from "Numerical Recepes in C", p. 402.
	      break;
	    } // if
	  } // N
	  histFull[k].histFullHar[j].mHistPro_r0theta->Fill(Ntheta, r0); // for first pass output
	  Vtheta = Flow::j01 / r0; // BP Eq. 9
	  histFull[k].histFullHar[j].mHistPro_Vtheta->Fill(Ntheta, Vtheta);
	  _v = (m==1) ? Vtheta / mult : 0.; // BP Eq. 5, v from r0 for each theta
	  histFull[k].mHistPro_vr0->Fill(j+1, _v/perCent);	  

	  savedHistFirstPassNames->AddLast(histFull[k].histFullHar[j].histTheta[Ntheta].mHistGtheta);

	  // Project the profile hists to 1D hists

	  histTitle = new TString("FlowReGtheta");
	  *histTitle += Ntheta;
	  *histTitle += "_Sel";
	  *histTitle += k+1;
	  *histTitle += "_Har";
	  *histTitle += j+1;
	  histFull[k].histFullHar[j].histTheta[Ntheta].mHistReGtheta =
	    histFull[k].histFullHar[j].histTheta[Ntheta].mHistProReGtheta->ProjectionX(histTitle->Data());
	  histFull[k].histFullHar[j].histTheta[Ntheta].mHistReGtheta->SetTitle(histTitle->Data());
	  histFull[k].histFullHar[j].histTheta[Ntheta].mHistReGtheta->SetXTitle("r");
	  histFull[k].histFullHar[j].histTheta[Ntheta].mHistReGtheta->SetYTitle("Re(G^{#theta}(ir))");
	  delete histTitle;
	  savedHistFirstPassNames->AddLast(histFull[k].histFullHar[j].histTheta[Ntheta].mHistReGtheta);

	  histTitle = new TString("FlowImGtheta");
	  *histTitle += Ntheta;
	  *histTitle += "_Sel";
	  *histTitle += k+1;
	  *histTitle += "_Har";
	  *histTitle += j+1;
	  histFull[k].histFullHar[j].histTheta[Ntheta].mHistImGtheta =
	    histFull[k].histFullHar[j].histTheta[Ntheta].mHistProImGtheta->ProjectionX(histTitle->Data());
	  histFull[k].histFullHar[j].histTheta[Ntheta].mHistImGtheta->SetTitle(histTitle->Data());
	  histFull[k].histFullHar[j].histTheta[Ntheta].mHistImGtheta->SetXTitle("r");
	  histFull[k].histFullHar[j].histTheta[Ntheta].mHistImGtheta->SetYTitle("Im(G^{#theta}(ir))");
	  delete histTitle;
	  savedHistFirstPassNames->AddLast(histFull[k].histFullHar[j].histTheta[Ntheta].mHistImGtheta);

	} // Ntheta

      } else if (!mFirstPass) { // second pass

	V1Sq = 0.;
	for (int Ntheta = 0; Ntheta < Flow::nTheta; Ntheta++) {
	  
	  if (k && !j && mV1Mixed) { // v1 "integrated" from mixed harmonics: selection 2, harmonic 1
	    mV1neum[Ntheta] /= mNEvents; // averaged over events
	    //PR(mV1neum[Ntheta]);
	    r0 = mr0theta[1][1][Ntheta];  // selection 2, harmonic 2
	    reDenom = histFull[k].histFullHar[1].mHistReDenom->GetBinContent(Ntheta+1);
	    imDenom = histFull[k].histFullHar[1].mHistImDenom->GetBinContent(Ntheta+1);
	    denom(reDenom,imDenom); // selection 2, harmonic 2
	    div = mV1neum[Ntheta] / denom;
	    reDiv = div.Re();
	    V1SqTheta = -8. * Flow::j01 / (Flow::epsV1*Flow::epsV1) / TMath::Power(r0,3.) * reDiv; // DF Eq. 7 for each theta2
	    V1Sq += V1SqTheta;
	    if (!std::isnan(V1SqTheta) && V1SqTheta != 0.) {
	      Vtheta = TMath::Sqrt(fabs(V1SqTheta)); // absolute values of negatives
	    }
	  } else {
	    Vtheta = Flow::j01 / mr0theta[k][j][Ntheta]; // BP Eq. 9
	  }
	  histFull[k].mHistPro_V->Fill(j+1, Vtheta);
	  _v = (m==1) ? Vtheta / mult : 0.; // BP Eq. 5, v from r0 for each theta
	  histFull[k].mHistPro_vr0->Fill(j+1, _v/perCent); // gives error for vr0

	} // Ntheta

	if (k && !j && mV1Mixed) { // v1 from mixed harmonics: selection 2, harmonic 1
	  V1Sq /= Flow::nTheta;	  
	  v2Sign = (int)(V1Sq / fabs(V1Sq));
	  cout << "The sign of v2 is " << v2Sign << endl;
	  //float Vhist = histFull[k].mHistPro_V->GetBinContent(j+1);
	  V = TMath::Sqrt(fabs(V1Sq));
	  //cout << "pro hist, V1Sq: " << Vhist << ", " << V << endl;
	  v1DiffConst = -4. * Flow::j01 * (double)v2Sign / V / (Flow::epsV1*Flow::epsV1) /
	    TMath::Power(r0,3.);
	}

	// Differential flow

	for (int Ntheta = 0; Ntheta < Flow::nTheta; Ntheta++) {

	  if (k && !j && mV1Mixed) { // v1 mixed harmonics, use denom for 2nd harmonic
	    reDenom = histFull[k].histFullHar[1].mHistReDenom->GetBinContent(Ntheta+1);
	    imDenom = histFull[k].histFullHar[1].mHistImDenom->GetBinContent(Ntheta+1);
	    denom(reDenom,imDenom);
	  } else {
	    reDenom = histFull[k].histFullHar[j].mHistReDenom->GetBinContent(Ntheta+1);
	    imDenom = histFull[k].histFullHar[j].mHistImDenom->GetBinContent(Ntheta+1);
	    denom(reDenom,imDenom);
	    denom *= TComplex::Power(i, m-1);
	    Vtheta = Flow::j01 / mr0theta[k][j][Ntheta]; // BP Eq. 9
	  }
	  
	  // v(eta)
	  int etaMax = histFull[k].histFullHar[j].histTheta[Ntheta].mHistReNumerEta->GetNbinsX();
	  for (int binEta = 1; binEta <= etaMax; binEta++) {
	    eta = histFull[k].histFullHar[j].histTheta[Ntheta].mHistReNumerEta->GetXaxis()->
	      GetBinCenter(binEta);
	    
	    reNumer = histFull[k].histFullHar[j].histTheta[Ntheta].mHistReNumerEta->
	      GetBinContent(binEta);
	    imNumer = histFull[k].histFullHar[j].histTheta[Ntheta].mHistImNumerEta->
	      GetBinContent(binEta);
	    numer(reNumer,imNumer); 
	    
	    reDiv = (numer / denom).Re();
	    if (!std::isnan(reDiv) && reDiv != 0.) {
	      if (k && !j && mV1Mixed) { // v1 from mixed harmonics: selection 2, harmonic 1
		_v = v1DiffConst * reDiv /perCent; // DF Eq. 9
	      } else {
		_v = BesselRatio[m-1] * reDiv * Vtheta /perCent; // BP Eq. 12
	      }
	      histFull[k].histFullHar[j].mHistPro_vEta->Fill(eta, _v);   
	      //cout << k << " " << j << " " << Ntheta << " " << binEta << " " << eta << " " << _v << endl;
	    }
	  }	

	  // v(pt)
	  Float_t v[2];
	  int etaMax2D = histFull[k].histFullHar[j].histTheta[Ntheta].mHistReNumer2D->
	    GetNbinsX(); // 2
	  for (int binPt = 1; binPt <= mPtBinsPart; binPt++) {
	    pt = histFull[k].histFullHar[j].histTheta[Ntheta].mHistReNumer2D->GetYaxis()->
	      GetBinCenter(binPt);	  
	    for (int binEta = 1; binEta <= etaMax2D; binEta++) {
	      eta = histFull[k].histFullHar[j].histTheta[Ntheta].mHistReNumer2D->GetXaxis()->
		GetBinCenter(binEta);
	      
	      reNumer = histFull[k].histFullHar[j].histTheta[Ntheta].mHistReNumer2D->
		GetBinContent(binEta, binPt);
	      imNumer = histFull[k].histFullHar[j].histTheta[Ntheta].mHistImNumer2D->
		GetBinContent(binEta, binPt);
	      numer(reNumer,imNumer); 
	      
	      reDiv = (numer / denom).Re();
	      if (k && !j && mV1Mixed) { // v1 from mixed harmonics: selection 2, harmonic 1
		v[binEta-1] = v1DiffConst * reDiv /perCent; // DF Eq. 9
	      } else {
		v[binEta-1] = BesselRatio[m-1] * reDiv * Vtheta /perCent; // BP Eq. 12
	      }
	    }
	    if (!std::isnan(reDiv) && reDiv != 0.) {
	      if (oddHar) {
		_v = (F * v[1] - B * v[0]) / T; // weighted forward minus back
	      } else { 
		_v = (B * v[0] + F * v[1]) / T; // weighted mean of forward and back
	      }
	      histFull[k].histFullHar[j].mHistPro_vPt->Fill(pt, _v);
	    }   
	  }

	} // Ntheta

      } // second pass
      
      if (j <=1) {
	// sigma2 and chi
	mQ[k][j]      /= (float)mNEvents;
	mQ2[k][j]     /= (float)mNEvents;

	V = histFull[k].mHistPro_V->GetBinContent(j+1);
	sigma2 = mQ2[k][j] - TMath::Power(mQ[k][j].X(), 2.) - TMath::Power(mQ[k][j].Y(), 2.)
	  - TMath::Power(V, 2.); // BP Eq. 62
	//cout << mQ2[k][j] << ", " << mQ[k][j].X() << ", " << mQ[k][j].Y() << ", " << V << endl;
	chi = V / TMath::Sqrt(sigma2); // BP Eq. 59
	
	// output v from r0, and chi
	_v = histFull[k].mHistPro_vr0->GetBinContent(j+1);
	vErr = histFull[k].mHistPro_vr0->GetBinError(j+1); // from the spread with theta
	if (k && !j && mV1Mixed) {
	  if (!mFirstPass) {
	    cout << setprecision(3) << "Sel = " << k+1 << ": v" << j+1 << " from r0 = (" << _v <<
	      " +/- " << vErr << ") %"<< " from mixed harmonics" << endl;
	  }
	} else if (mFirstPass) {
	  cout << setprecision(3) << "Sel = " << k+1 << ": v" << j+1 << " from r0 = (" << _v <<
	    " +/- " << vErr << ") %" << endl;
	} else {
	  cout << setprecision(3) << "Sel = " << k+1 << ": v" << j+1 << " from r0 = (" << _v <<
	    " +/- " << vErr << ") %  chiJYO = " << chi << endl;
	}
      }

      // Project the profile hists to 1D hists

      if (mFirstPass && j<=1) {

	histTitle = new TString("FlowLYZ_r0theta_Sel");
	*histTitle += k+1;
	*histTitle += "_Har";
	*histTitle += j+1;
	histFull[k].histFullHar[j].mHist_r0theta = 
	  histFull[k].histFullHar[j].mHistPro_r0theta->ProjectionX(histTitle->Data());
	histFull[k].histFullHar[j].mHist_r0theta->SetTitle(histTitle->Data());
	histFull[k].histFullHar[j].mHist_r0theta->SetXTitle("#theta");
	histFull[k].histFullHar[j].mHist_r0theta->SetYTitle("r_{0}^{#theta}");
	delete histTitle;
	savedHistFirstPassNames->AddLast(histFull[k].histFullHar[j].mHist_r0theta);

	histTitle = new TString("FlowLYZ_Vtheta_Sel");
	*histTitle += k+1;
	*histTitle += "_Har";
	*histTitle += j+1;
	histFull[k].histFullHar[j].mHist_Vtheta = 
	  histFull[k].histFullHar[j].mHistPro_Vtheta->ProjectionX(histTitle->Data());
	histFull[k].histFullHar[j].mHist_Vtheta->SetTitle(histTitle->Data());
	histFull[k].histFullHar[j].mHist_Vtheta->SetXTitle("#theta");
	histFull[k].histFullHar[j].mHist_Vtheta->SetYTitle("V_{n}^{#theta}");
	delete histTitle;
	savedHistFirstPassNames->AddLast(histFull[k].histFullHar[j].mHist_Vtheta);

      } else if (!mFirstPass) { // second pass
			
	histTitle = new TString("FlowLYZ_vEta_Sel");
	*histTitle += k+1;
	*histTitle += "_Har";
	*histTitle += j+1;
	histFull[k].histFullHar[j].mHist_vEta = 
	  histFull[k].histFullHar[j].mHistPro_vEta->ProjectionX(histTitle->Data());
	histFull[k].histFullHar[j].mHist_vEta->SetTitle(histTitle->Data());
	histFull[k].histFullHar[j].mHist_vEta->SetXTitle((char*)xLabel.Data());
	histFull[k].histFullHar[j].mHist_vEta->SetYTitle("v (%)");
	delete histTitle;
	savedHistNames->AddLast(histFull[k].histFullHar[j].mHist_vEta);

	histTitle = new TString("FlowLYZ_vPt_Sel");
	*histTitle += k+1;
	*histTitle += "_Har";
	*histTitle += j+1;
	histFull[k].histFullHar[j].mHist_vPt = 
	  histFull[k].histFullHar[j].mHistPro_vPt->ProjectionX(histTitle->Data());
	histFull[k].histFullHar[j].mHist_vPt->SetTitle(histTitle->Data());
	histFull[k].histFullHar[j].mHist_vPt->SetXTitle("Pt (GeV/c)");
	histFull[k].histFullHar[j].mHist_vPt->SetYTitle("v (%)");
	delete histTitle;
	savedHistNames->AddLast(histFull[k].histFullHar[j].mHist_vPt);

	// Doubly integrated v with cuts
	
	// from v(eta)
	yieldSum = 0.;
	vSum     = 0.;
	err2Sum  = 0.;
	for (int bin=1; bin<=mNEtaBins; bin++) {
	  etaPtNoCut = kTRUE;
	  eta = histFull[k].histFullHar[j].mHistPro_vEta->GetBinCenter(bin);
	  if (mEtaRange_for_vPt[1] > mEtaRange_for_vPt[0] && 
	      (TMath::Abs(eta) < mEtaRange_for_vPt[0] || 
	       TMath::Abs(eta) >= mEtaRange_for_vPt[1])) {
	    etaPtNoCut = kFALSE;
	  }
	  _v = histFull[k].histFullHar[j].mHistPro_vEta->GetBinContent(bin);
	  if (_v != 0. && etaPtNoCut) {
	    yield    = mHistYieldPartEta->GetBinContent(bin);
	    yieldSum += yield;
	    if (oddHar && eta < 0.) { _v *= -1.; }
	    vSum     += yield * _v;
	    vErr     = histFull[k].histFullHar[j].mHistPro_vEta->GetBinError(bin);
	    err2Sum  += yield*vErr * yield*vErr; 
	  }
	}
	if (yieldSum) {
	  _v = vSum / yieldSum;
	  vErr = TMath::Sqrt(err2Sum) / yieldSum;
	  cout << setprecision(3) << "Sel = " << k+1 << ": v" << j+1 << " from eta= (" << _v <<
	    " +/- " << vErr << ") %" << endl;
	}
	
	// from v(pt)
	yieldSum = 0.;
	vSum     = 0.;
	err2Sum  = 0.;
	for (int bin=1; bin<=mPtBinsPart; bin++) {
	  etaPtNoCut = kTRUE;
	  pt = histFull[k].histFullHar[j].mHistPro_vPt->GetBinCenter(bin);
	  if (mPtRange_for_vEta[1] > mPtRange_for_vEta[0] && 
	      (pt < mPtRange_for_vEta[0] || pt >= mPtRange_for_vEta[1])) {
	    etaPtNoCut = kFALSE;
	  }
	  _v = histFull[k].histFullHar[j].mHistPro_vPt->GetBinContent(bin);
	  if (_v != 0. && etaPtNoCut) {
	    yield    = mHistYieldPartPt->GetBinContent(bin);
	    yieldSum += yield;
	    vSum     += yield * _v;
	    vErr     = histFull[k].histFullHar[j].mHistPro_vPt->GetBinError(bin);
	    err2Sum  += yield*vErr * yield*vErr; 
	  }
	}
	if (yieldSum) {
	  _v = vSum / yieldSum;
	  vErr = TMath::Sqrt(err2Sum) / yieldSum;
	  histFull[k].mHist_v->SetBinContent(j+1, _v);
	  histFull[k].mHist_v->SetBinError(j+1, vErr);
	  cout << setprecision(3) << "Sel = " << k+1 << ": v" << j+1 << " from pt = (" << _v <<
	    " +/- " << vErr << ") %" << endl;
	}
	
      } // second pass

    } // j

    histTitle = new TString("FlowLYZ_vr0_Sel");
    *histTitle += k+1;
    histFull[k].mHist_vr0 = 
      histFull[k].mHistPro_vr0->ProjectionX(histTitle->Data());
    histFull[k].mHist_vr0->SetTitle(histTitle->Data());
    histFull[k].mHist_vr0->SetXTitle("Harmonic");
    histFull[k].mHist_vr0->SetYTitle("v from r_{0} (%)");
    delete histTitle;
    savedHistFirstPassNames->AddLast(histFull[k].mHist_vr0);
    savedHistNames->AddLast(histFull[k].mHist_vr0);

    if (!mFirstPass) {
      // Project the profile hists to 1D hists
      
      histTitle = new TString("FlowLYZ_V_Sel");
      *histTitle += k+1;
      histFull[k].mHist_V = 
	histFull[k].mHistPro_V->ProjectionX(histTitle->Data());
      histFull[k].mHist_V->SetTitle(histTitle->Data());
      histFull[k].mHist_V->SetXTitle("Harmonic");
      histFull[k].mHist_V->SetYTitle("mean V_{n}");
      delete histTitle;
      savedHistNames->AddLast(histFull[k].mHist_V);

      savedHistNames->AddLast(histFull[k].mHist_v);
    }

  } // k

    // Write histograms
  // first and second pass outputs are combined in doFlowEvents.C
  TFile fileFirstPassNew("flow.firstPassLYZNew.root", "RECREATE");
  TFile histFile("flow.LeeYangZeros.root", "RECREATE");
  if (mFirstPass) {
    savedHistFirstPassNames->AddLast(mHistMult);
    fileFirstPassNew.cd();
    savedHistFirstPassNames->Write();
    histFile.cd();
    savedHistFirstPassNames->Write();
  } else {
    histFile.cd();
    savedHistNames->AddLast(mHistMult);
    savedHistNames->AddLast(mHistYieldPartPt);
    savedHistNames->AddLast(mHistYieldPartEta);
    savedHistNames->Write();
  }
  fileFirstPassNew.Close(); // on second pass it is empty
  histFile.Close();
  
  delete savedHistNames;
  delete savedHistFirstPassNames;
  delete pFlowSelect;

//   timeFinish.stop();
//   cout << endl << "###### time Event =  " << timeEvent.elapsedTime() << " sec" << endl;
//   cout << "###### time Part =   " << timePart.elapsedTime() << " sec" << endl;
//   cout << "###### time Finish = " << timeFinish.elapsedTime() << " sec" << endl;

  return StMaker::Finish();
}

//-----------------------------------------------------------------------

void StFlowLeeYangZerosMaker::SetHistoRanges(Bool_t ftpc_included) {

    if (ftpc_included) {
	  mEtaMin = Flow::etaMin;
	  mEtaMax = Flow::etaMax;
	mNEtaBins = Flow::nEtaBins;
    }
    else {
	  mEtaMin = Flow::etaMinTpcOnly;
 	  mEtaMax = Flow::etaMaxTpcOnly;
	mNEtaBins = Flow::nEtaBinsTpcOnly;
    }

    return;
}

//------------------------------------------------------------------------

inline void StFlowLeeYangZerosMaker::SetPtRange_for_vEta(Float_t lo, Float_t hi) {

  // Sets the pt range for the v(eta) histograms.

  mPtRange_for_vEta[0] = lo;
  mPtRange_for_vEta[1] = hi;

  return;
}

//------------------------------------------------------------------------

inline void StFlowLeeYangZerosMaker::SetEtaRange_for_vPt(Float_t lo, Float_t hi) {
  
  // Sets the |eta| range for the v(pt) histograms.

  mEtaRange_for_vPt[0] = lo;
  mEtaRange_for_vPt[1] = hi;

  return;
}

//------------------------------------------------------------------------
