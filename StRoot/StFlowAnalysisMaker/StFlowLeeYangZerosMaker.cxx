////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowLeeYangZerosMaker.cxx,v 1.2 2006/03/22 21:55:28 posk Exp $
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
//               Directed Flow (DF) Nucl. Phys. A 742, 130  (nucl-th/0404087)
//               The errors come from the variations with theta and batch job variations
//               This treats the acceptance variations with theta as statistical
//               Particles which would normally be correlated with the event plane are analyzed
//               The event plane selections are not used
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
#include "StTimer.hh"
#define PR(x) cout << "##### FlowLYZ: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowLeeYangZerosMaker)

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

  const float multMin      =    0.;
  const float multMax      = 2000.;

  enum { nMultBins         = 200
  };

  TString* histTitle;
  mFirstPass = kFALSE;
  
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
        histTitle->Data(), mNEtaBins, mEtaMin, mEtaMax, -100., 100.);
      histFull[k].histFullHar[j].mHistPro_vEta->SetXTitle((char*)xLabel.Data());
      histFull[k].histFullHar[j].mHistPro_vEta->SetYTitle("v (%)");
      delete histTitle;

      histTitle = new TString("FlowProLYZ_vPt_Sel");
      *histTitle += k+1;
      *histTitle += "_Har";
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHistPro_vPt = new TProfile(histTitle->Data(),
        histTitle->Data(), mPtBinsPart, Flow::ptMin, ptMaxPart, -100., 100.);
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
    } // j
  } // k

  gMessMgr->SetLimit("##### FlowLeeYangZero", 5);
  gMessMgr->Info("##### FlowLeeYangZero: $Id: StFlowLeeYangZerosMaker.cxx,v 1.2 2006/03/22 21:55:28 posk Exp $");

  return StMaker::Init();
}

//-----------------------------------------------------------------------

Bool_t StFlowLeeYangZerosMaker::FillFromFlowEvent() {
  // Get event quantities from StFlowEvent for all particles
  //timeEvent.start();

  // multiplicity
  mMult = (int)pFlowEvent->MultPart(pFlowSelect);
  mHistMult->Fill((float)mMult);

  mNEvents++; // increment number of events

  TVector2 Q;
  Float_t theta, order, r0;
  TComplex expo, denom, Gtheta;
  TComplex i = TComplex::I();
  Double_t Qtheta;
  Int_t m;

  for (int k = 0; k < Flow::nSels; k++) {
    pFlowSelect->SetSelection(k);
    for (int j = 0; j < Flow::nHars; j++) {
      m = 1; // int for i^(m-1)
      pFlowSelect->SetHarmonic(j); // for integrated flow and denominator
      if (j==2) {
	m = 3;
	pFlowSelect->SetHarmonic(0);
      } else if (j==3) {
	m = 2;
	pFlowSelect->SetHarmonic(1);
      }
      order  = (double)((j+1)/m);

      // event Q
      Q = pFlowEvent->QPart(pFlowSelect);
      if (Q.Mod() == 0.) return kFALSE; // to eliminate Q=0
      mQ[k][j]  += Q;                   // for chi calculation
      mQ2[k][j] += Q.Mod2();
     
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
	    } else {  // Product G
	      Gtheta = pFlowEvent->Grtheta(pFlowSelect, r, theta);  // PG Eq. 3
	      if (Gtheta.Rho2() > 1000.) { break; } // stop when G gets too big
	    }
	    histFull[k].histFullHar[j].histTheta[Ntheta].mHistProReGtheta->Fill(r, Gtheta.Re());
	    histFull[k].histFullHar[j].histTheta[Ntheta].mHistProImGtheta->Fill(r, Gtheta.Im());
	  }
	} else if (!mFirstPass) {
	  // denominator for differential v
	  r0 = mr0theta[k][j][Ntheta];
	  if (!k) {
	    expo(0., r0 * Qtheta);
	    denom = TComplex::Power(i, m-1) * Qtheta * TComplex::Exp(expo); // BP Eq. 12
	  } else {
	    mGr0theta[k][j][Ntheta] = pFlowEvent->Grtheta(pFlowSelect, r0, theta);
	    denom = TComplex::Power(i, m-1) * mGr0theta[k][j][Ntheta] *
	      pFlowEvent->Gder_r0theta(pFlowSelect, r0, theta);  // PG Eq. 9
	  }
	  histFull[k].histFullHar[j].mHistReDenom->Fill(Ntheta, denom.Re());
	  histFull[k].histFullHar[j].mHistImDenom->Fill(Ntheta, denom.Im());
	} // second pass
      } // theta
    } // j
  } // k
  
  //timeEvent.stop();
  return kTRUE;
}

//-----------------------------------------------------------------------

void StFlowLeeYangZerosMaker::FillParticleHistograms() {
  // Fill histograms from the particles on 2nd pass
  //timePart.start();

  // Initialize Iterator
  StFlowTrackCollection* pFlowTracks = pFlowEvent->TrackCollection();
  StFlowTrackIterator itr;
  
  Float_t theta, phi, eta, pt, m, r0;
  Double_t order, wgt;
  TComplex expo, numer, cosTerm, cosTermComp;
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
	  for (int Ntheta = 0; Ntheta < Flow::nTheta; Ntheta++) {
	    theta = ((float)Ntheta / (float)Flow::nTheta) * TMath::Pi()/order;
	    r0 = mr0theta[k][j][Ntheta];
	    if (!k) {
	      expo(0., r0 * mQtheta[k][j][Ntheta]);
	      numer = cos(m*order*(phi - theta)) * TComplex::Exp(expo); // BP Eq. 12
	    } else {
	      cosTerm(cos(m*order*(phi - theta)), 0.);
	      wgt = pFlowEvent->Weight(k, j, pFlowTrack);
	      cosTermComp(1., r0*wgt*cos(order*(phi - theta)));
	      numer = mGr0theta[k][j][Ntheta] * cosTerm / cosTermComp;  // PG Eq. 9
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

  //timePart.stop();
}

//-----------------------------------------------------------------------

Int_t StFlowLeeYangZerosMaker::Finish() {
  // In the first pass, from Gtheta find the first minimum to get r0(theta).
  // In the second pass calculate V(theta), average over theta, and then calculate v 
  //timeFinish.start();

  TOrdCollection* savedHistNames          = new TOrdCollection(Flow::nSels * Flow::nHars);
  TOrdCollection* savedHistFirstPassNames = new TOrdCollection(Flow::nSels * 2 * Flow::nTheta);
  TString* histTitle;

  cout << endl << "##### LeeYangZeros Maker:" << endl;
  cout << "integrated flow: (errors just show variation with theta)" << endl;

  Float_t reG, imG, reNumer, imNumer, reDenom, imDenom, reDiv;
  TComplex Gtheta, denom, numer, div;
  Float_t mult = mHistMult->GetMean();
  Float_t _v, vErr, Vtheta, V, yield, eta, pt;
  Double_t r0, yieldSum, vSum, err2Sum, Glast, G0, Gnext, GnextNext;
  Float_t Xlast, X0, Xnext, sigma2, chi;
  Float_t BesselRatio[3] = {1., 1.202, 2.69}; // is 2.69 correct?
  Int_t m;
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
      m = 1; // int for index of BesselRatio
      if (j==2) { m = 3; }
      else if (j==3) { m = 2; }
      for (int Ntheta = 0; Ntheta < Flow::nTheta; Ntheta++) {
	if (mFirstPass && j<=1) {
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
	    
	    if (Gnext > G0 && GnextNext > G0) { // lowest point, footnote 3
	      Glast     = histFull[k].histFullHar[j].histTheta[Ntheta].mHistGtheta->GetBinContent(N-1);
	      Xlast     = histFull[k].histFullHar[j].histTheta[Ntheta].mHistGtheta->GetBinCenter(N-1);
	      X0        = histFull[k].histFullHar[j].histTheta[Ntheta].mHistGtheta->GetBinCenter(N);
	      Xnext     = histFull[k].histFullHar[j].histTheta[Ntheta].mHistGtheta->GetBinCenter(N+1);
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
	  //histFull[k].mHistPro_V->Fill(j+1, Vtheta);

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

	} else if (!mFirstPass) { // second pass

	  Vtheta = Flow::j01 / mr0theta[k][j][Ntheta]; // BP Eq. 9
	  histFull[k].mHistPro_V->Fill(j+1, Vtheta);

	  // Differential flow
	  reDenom = histFull[k].histFullHar[j].mHistReDenom->GetBinContent(Ntheta+1);
	  imDenom = histFull[k].histFullHar[j].mHistImDenom->GetBinContent(Ntheta+1);
	  denom(reDenom,imDenom); 
	  
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
	    
	    div = numer / denom;
	    reDiv = BesselRatio[m-1] * div.Re();
	    if (!isnan(reDiv) && reDiv != 0.) {
	      _v = reDiv * Vtheta /perCent; // BP Eq. 12
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
	      
	      div = numer / denom;
	      reDiv = BesselRatio[m-1] * div.Re();
	      v[binEta-1] = reDiv * Vtheta /perCent; // BP Eq. 12
	    }
	    if (!isnan(reDiv) && reDiv != 0.) {
	      if (oddHar) {
		_v = (F * v[1] - B * v[0]) / T; // weighted forward minus back
	      } else { 
		_v = (B * v[0] + F * v[1]) / T; // weighted mean of forward and back
	      }
	      histFull[k].histFullHar[j].mHistPro_vPt->Fill(pt, _v);
	    }   
	  }
	} // second pass

	// v from r0 for each theta
	_v = (m==1) ? Vtheta / mult : 0.; // BP Eq. 5
	histFull[k].mHistPro_vr0->Fill(j+1, _v/perCent);
	
      } // Ntheta
      
      // sigma2 and chi
      if (j <=1) {
	mQ[k][j]  /= (float)mNEvents;
	mQ2[k][j] /= (float)mNEvents;
	V = histFull[k].mHistPro_V->GetBinContent(j+1);
	sigma2 = mQ2[k][j] - TMath::Power(mQ[k][j].X(), 2.) - TMath::Power(mQ[k][j].Y(), 2.)
	  - TMath::Power(V, 2.); // BP Eq. 62
	//cout << mQ2[k][j] << ", " << mQ[k][j].X() << ", " << mQ[k][j].Y() << ", " << V << endl;
	chi = V / TMath::Sqrt(sigma2); // BP Eq. 59
	
	// output v from r0, and chi
	_v = histFull[k].mHistPro_vr0->GetBinContent(j+1);
	vErr = histFull[k].mHistPro_vr0->GetBinError(j+1); // from the spread with theta
	cout  << setprecision(3) << "Sel = " << k+1 << ": v" << j+1 << " from r0 = (" << _v <<
	  " +/- " << vErr << ") %  chiJYO = " << chi << endl;
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
