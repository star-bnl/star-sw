////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowAnalysisMaker.cxx,v 1.6 2002/01/16 18:13:32 posk Exp $
//
// Authors: Art Poskanzer, LBNL, and Alexander Wetzler, IKF, Dec 2000
//
////////////////////////////////////////////////////////////////////////////
//
// Description:  Maker to analyze Flow using StFlowEvent
//
////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowAnalysisMaker.cxx,v $
// Revision 1.6  2002/01/16 18:13:32  posk
// Moved fitting q to plot.C.
//
// Revision 1.5  2001/11/06 17:55:23  posk
// Only sin terms at 40 GeV.
//
// Revision 1.4  2001/08/17 22:03:23  posk
// Now can also do 40 GeV data.
//
// Revision 1.3  2001/05/14 22:53:37  posk
// Can select PID for event plane particles. Protons not used for 1st harmonic
// event plane.
//
// Revision 1.2  2001/03/16 22:42:00  posk
// Removed pt weighting for odd harmonics.
//
// Revision 1.1  2001/02/22 23:34:11  posk
// NA49 version of STAR flow software.
//
// Revision 1.45  2000/10/12 21:01:30  posk
//
////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <math.h>
#include "StMaker.h"
#include "StFlowAnalysisMaker.h"
#include "StFlowMaker/StFlowMaker.h"
#include "StFlowMaker/StFlowEvent.h"
#include "StFlowMaker/StFlowConstants.h"
#include "StFlowMaker/StFlowSelection.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "TVector2.h"
#include "TFile.h"
#include "TString.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TProfile.h"
#include "TProfile2D.h"
#include "TF1.h"
#include "TOrdCollection.h"
#include "StMessMgr.h"
#include "TRandom.h"
#define PR(x) cout << "##### FlowAnalysis: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowAnalysisMaker)

Bool_t  StFlowAnalysisMaker::mV21 = kFALSE;

//-----------------------------------------------------------------------

StFlowAnalysisMaker::StFlowAnalysisMaker(const Char_t* name): StMaker(name),
  MakerName(name) {
  pFlowSelect = new StFlowSelection();
}

StFlowAnalysisMaker::StFlowAnalysisMaker(const Char_t* name,
					 const StFlowSelection& flowSelect) :
  StMaker(name), MakerName(name) {
  pFlowSelect = new StFlowSelection(flowSelect); //copy constructor
}

//-----------------------------------------------------------------------

StFlowAnalysisMaker::~StFlowAnalysisMaker() {
}

//-----------------------------------------------------------------------

Int_t StFlowAnalysisMaker::Make() {
  // Fill histograms
  
  // Get a pointer to StFlowEvent
  StFlowMaker* pFlowMaker = NULL;
  pFlowMaker = (StFlowMaker*)GetMaker("Flow");
  if (pFlowMaker) pFlowEvent = pFlowMaker->FlowEventPointer();
  if (pFlowEvent) {
    if (pFlowSelect->Select(pFlowEvent)) {   // event selected

      //change to elliminate psi == 0 events
      if (FillFromFlowEvent()) {             // get event quantities
	FillEventHistograms();               // fill event histograms
	FillParticleHistograms(); 
      } else {
	gMessMgr->Info("##### FlowAnalysis: Event psi = 0");
      }
    }
    if (Debug()) StMaker::PrintInfo();
  } else {
    //gMessMgr->Info("##### FlowAnalysis: FlowEvent pointer null");
    return kStOK;
  }
 
  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StFlowAnalysisMaker::Init() {
  // Book histograms
  
  float ptMaxPart = 2.;
  if (pFlowSelect->PtMaxPart()) {
    ptMaxPart = pFlowSelect->PtMaxPart();
  }
  
  const float yMin            =    1.;
  const float yMax            =    7.;
  const float ptMin           =    0.;
  const float ptMax           =    2.;
  const float chargeMin       =  -2.5;
  const float chargeMax       =   2.5; 
  const float bxMin           =   -5.;
  const float bxMax           =    5.; 
  const float byMin           =   -1.;
  const float byMax           =    1.; 
  const float chi2Min         =    0.;
  const float chi2Max         =   12.; 
  const float fitPtsMin       =    0.;
  const float fitPtsMax       =  200.; 
  const float maxPtsMin       =    0.;
  const float maxPtsMax       =  200.; 
  const float fitOverMaxMin   =    0.;
  const float fitOverMaxMax   =   1.2; 
  const float origMultMin     =    0.;
  const float origMultMax     = 1500.; 
  const float totalMultMin    =    0.;
  const float totalMultMax    = 1500.; 
  const float corrMultMin     =    0.;
  const float corrMultMax     = 1500.; 
  const float multOverOrigMin =    0.;
  const float multOverOrigMax =    1.; 
  const float vertexZMin      = -581.;
  const float vertexZMax      = -577.; 
  const float vertexXYMin     =   -1.;
  const float vertexXYMax     =    1.; 
  const float etaSymMin       =    0.; 
  const float etaSymMax       =    1.; 
  const float phiMin          =    0.;
  const float phiMax          = twopi; 
  const float psiMin          =    0.;
  const float psiMax          = twopi; 
  const float multMin         =    0.;
  const float multMax         = 1500.;
  const float sumPt2Min       =    0.;
  const float sumPt2Max       =  200.;
  const float ptYMin          =    0.;
  const float ptYMax          =   10.;
  const float qMin            =    0.;
  const float eVetoMin        =    0.;
  const float eVetoMax        = 40000.;
  const float centMin         =   0.5;
  const float centMax         =   6.5;
  const float sinCosPtMin     =    0.;
  const float dedxMin         =    0.;
  const float dedxMax         =    2.;
  const float pMin            =   -2.;
  const float pMax            =    5.;

  enum { nYBins            = 60,
	 nPtBins           = 40,
	 nChargeBins       = 50,
	 nBxBins           = 50,
	 nByBins           = 40,
	 nChi2Bins         = 60,
	 nFitPtsBins       = 50,
	 nMaxPtsBins       = 50,
	 nFitOverMaxBins   = 60,
	 nOrigMultBins     = 60,
	 nTotalMultBins    = 60,
	 nMultOverOrigBins = 50,
	 nMultPartBins     = 40,
	 nVertexZBins      = 80,
	 nVertexXYBins     = 50,
	 nEtaSymBins       = 50,
	 nPhi3DBins        = 18,
	 nPsiBins          = 36,
	 nMultBins         = 60,
	 nSumPt2Bins      = 100,
	 nPtYBins         = 100,
	 nPidBins          = 50,
         nEVetoBins        = 50,
         nCentBins         =  6,
	 nDedxBins        = 200,
	 nMomenBins       = 140};

  // Charge
  mHistCharge = new TH1F("Flow_Charge", "Flow_Charge",
			 nChargeBins, chargeMin, chargeMax);
  mHistCharge->SetXTitle("Charge");
  mHistCharge->SetYTitle("Counts");
    
  // Distance of closest x approach
  mHistBx = new TH1F("Flow_Bx", "Flow_Bx",
      nBxBins, bxMin, bxMax);
  mHistBx->SetXTitle("Track x impact par. (cm)");
  mHistBx->SetYTitle("Counts");
    
  // Distance of closest y approach
  mHistBy = new TH1F("Flow_By", "Flow_By",
      nByBins, byMin, byMax);
  mHistBy->SetXTitle("Track y impact par. (cm)");
  mHistBy->SetYTitle("Counts");
    
  // Chi2
  mHistChi2 = new TH1F("Flow_Chi2", "Flow_Chi2",
      nChi2Bins, chi2Min, chi2Max);
  mHistChi2->SetXTitle("Chi square per df");
  mHistChi2->SetYTitle("Counts");
    
  // FitPts
  mHistFitPts = new TH1F("Flow_FitPts", "Flow_FitPts",
      nFitPtsBins, fitPtsMin, fitPtsMax);
  mHistFitPts->SetXTitle("Fit Points");
  mHistFitPts->SetYTitle("Counts");
    
  // MaxPts
  mHistMaxPts = new TH1F("Flow_MaxPts", "Flow_MaxPts",
      nMaxPtsBins, maxPtsMin, maxPtsMax);
  mHistMaxPts->SetXTitle("Max Points");
  mHistMaxPts->SetYTitle("Counts");
    
  // FitOverMax
  mHistFitOverMax = new TH1F("Flow_FitOverMax", "Flow_FitOverMax",
      nFitOverMaxBins, fitOverMaxMin, fitOverMaxMax);
  mHistFitOverMax->SetXTitle("Fit Points / Max Points");
  mHistFitOverMax->SetYTitle("Counts");
    
  // OrigMult
  mHistOrigMult = new TH1F("Flow_OrigMult", "Flow_OrigMult",
      nOrigMultBins, origMultMin, origMultMax);
  mHistOrigMult->SetXTitle("Original Mult");
  mHistOrigMult->SetYTitle("Counts");

  // Mult
  mHistMult = new TH1F("Flow_Mult", "Flow_Mult",
      nTotalMultBins, totalMultMin, totalMultMax);
  mHistMult->SetXTitle("Mult");
  mHistMult->SetYTitle("Counts");
    
  // MultOverOrig
  mHistMultOverOrig = new TH1F("Flow_MultOverOrig", "Flow_MultOverOrig",
      nMultOverOrigBins, multOverOrigMin, multOverOrigMax);
  mHistMultOverOrig->SetXTitle("Mult / Orig. Mult");
  mHistMultOverOrig->SetYTitle("Counts");
    
  // Mult correlated with the event planes
  mHistMultPart = new TH1F("Flow_MultPart", "Flow_MultPart",
      nMultPartBins, corrMultMin, corrMultMax);
  mHistMultPart->SetXTitle("Mult of Correlated Particles");
  mHistMultPart->SetYTitle("Counts");
    
  // VertexZ
  mHistVertexZ = new TH1F("Flow_VertexZ", "Flow_VertexZ",
      nVertexZBins, vertexZMin, vertexZMax);
  mHistVertexZ->SetXTitle("Vertex Z (cm)");
  mHistVertexZ->SetYTitle("Counts");
    
  // VertexXY
  mHistVertexXY2D = new TH2F("Flow_VertexXY2D", "Flow_VertexXY2D",
			     nVertexXYBins, vertexXYMin, vertexXYMax,
			     nVertexXYBins, vertexXYMin, vertexXYMax);
  mHistVertexXY2D->SetXTitle("Vertex X (cm)");
  mHistVertexXY2D->SetYTitle("Vertex Y (cm)");
    
  // EtaSym
  mHistEtaSym = new TH1F("Flow_EtaSym", "Flow_EtaSym",
      nEtaSymBins, etaSymMin, etaSymMax);
  mHistEtaSym->SetXTitle("Eta Symmetry Ratio");
  mHistEtaSym->SetYTitle("Counts");
    
  // YPtPhi
  mHistYPtPhi3D = new TH3F("Flow_YPtPhi3D", "Flow_YPtPhi3D",
      nYBins, yMin, yMax, nPtBins, ptMin, ptMax, nPhi3DBins,
      phiMin, phiMax);
  mHistYPtPhi3D->SetXTitle("Rapidity");
  mHistYPtPhi3D->SetYTitle("Pt (GeV)");
  mHistYPtPhi3D->SetZTitle("Phi (rad)");
    
  // Yield for all particles
  mHistYieldAll2D = new TH2D("Flow_YieldAll2D", "Flow_YieldAll2D",
    nYBins, yMin, yMax, nPtBins, ptMin, ptMax);
  mHistYieldAll2D->Sumw2();
  mHistYieldAll2D->SetXTitle("Rapidity");
  mHistYieldAll2D->SetYTitle("Pt (GeV)");

  // Yield for particles correlated with the event plane
  mHistYieldPart2D = new TH2D("Flow_YieldPart2D", "Flow_YieldPart2D",
    nYBins, yMin, yMax, nPtBins, ptMin, ptMaxPart);
  mHistYieldPart2D->Sumw2();
  mHistYieldPart2D->SetXTitle("Rapidity");
  mHistYieldPart2D->SetYTitle("Pt (GeV)");

  // Mean Y in each bin
  mHistBinY = new TProfile("Flow_Bin_Y", "Flow_Bin_Y",
    nYBins, yMin, yMax, yMin, yMax, "");
  mHistBinY->SetXTitle("Rapidity");
  mHistBinY->SetYTitle("<Y>");
  
  // Mean Pt in each bin
  mHistBinPt = new TProfile("Flow_Bin_Pt", "Flow_Bin_Pt",
    nPtBins, ptMin, ptMaxPart, ptMin, ptMaxPart, "");
  mHistBinPt->SetXTitle("Pt (GeV)");
  mHistBinPt->SetYTitle("<Pt> (GeV)");
  
  // cos(n*phiLab)
  mHistCosPhi = new TProfile("Flow_CosPhiLab", "Flow_CosPhiLab",
    Flow::nHars, 0.5, (float)(Flow::nHars) + 0.5, -100., 100., "");
  mHistCosPhi->SetXTitle("Harmonic");
  mHistCosPhi->SetYTitle("<cos(n*PhiLab)> (%)");
    
  // PID multiplicities selected
  mHistPidMult = new TProfile("Flow_PidMult", "Flow_PidMult",
			      7, 0.5, 7.5, 0., 10000., "");
  mHistPidMult->SetXTitle("All, Pi+, Pi-, Proton, Pbar, e-, e+");
  mHistPidMult->SetYTitle("Multiplicity");
    
  // E Veto
  mHistEVeto = new TH1F("Flow_EVeto", "Flow_EVeto",
		       nEVetoBins, eVetoMin, eVetoMax);
  mHistEVeto->SetXTitle("EVeto");
  mHistEVeto->SetYTitle("Counts");
    
  // Centrality
  mHistCent = new TH1F("Flow_Cent", "Flow_Cent",
		       nCentBins, centMin, centMax);
  mHistCent->SetXTitle("Centrality Bin");
  mHistCent->SetYTitle("Counts");
    
  // MeanDedx positive charged particles
  mHistMeanDedx2Dpos = new TH2F("Flow_MeanDedx2D_Pos", "Flow_MeanDedx2D_Pos",
				nMomenBins, pMin, pMax,
				nDedxBins, dedxMin, dedxMax);
  mHistMeanDedx2Dpos->SetXTitle("log(momentum) (GeV)");
  mHistMeanDedx2Dpos->SetYTitle("mean dEdx");

  // MeanDedx negative charged particles
  mHistMeanDedx2Dneg = new TH2F("Flow_MeanDedx2D_Neg", "Flow_MeanDedx2D_Neg",
				nMomenBins, pMin, pMax,
				nDedxBins, dedxMin, dedxMax);
  mHistMeanDedx2Dneg->SetXTitle("log(momentum) (GeV)");
  mHistMeanDedx2Dneg->SetYTitle("mean dEdx");
  
  // MeanDedx PiPlus
  mHistMeanDedxPiPlus2D = new TH2F("Flow_MeanDedxPiPlus2D", 
				   "Flow_MeanDedxPiPlus2D",
				   nMomenBins, pMin, pMax,
				   nDedxBins, dedxMin, dedxMax);
  mHistMeanDedxPiPlus2D->SetXTitle("log(momentum) (GeV)");
  mHistMeanDedxPiPlus2D->SetYTitle("mean dEdx");
  
  // MeanDedxPiMinus
  mHistMeanDedxPiMinus2D = new TH2F("Flow_MeanDedxPiMinus2D", 
				    "Flow_MeanDedxPiMinus2D",
				    nMomenBins, pMin, pMax,
				    nDedxBins, dedxMin, dedxMax);
  mHistMeanDedxPiMinus2D->SetXTitle("log(momentum) (GeV)");
  mHistMeanDedxPiMinus2D->SetYTitle("mean dEdx");

  // MeanDedxProton
  mHistMeanDedxProton2D = new TH2F("Flow_MeanDedxProton2D", 
				   "Flow_MeanDedxProton2D",
				   nMomenBins, pMin, pMax,
				   nDedxBins, dedxMin, dedxMax);
  mHistMeanDedxProton2D->SetXTitle("log(momentum) (GeV)");
  mHistMeanDedxProton2D->SetYTitle("mean dEdx");
  
  // MeanDedxPbar
  mHistMeanDedxPbar2D = new TH2F("Flow_MeanDedxPbar2D", 
				 "Flow_MeanDedxPbar2D",
				 nMomenBins, pMin, pMax,
				 nDedxBins, dedxMin, dedxMax);
  mHistMeanDedxPbar2D->SetXTitle("log(momentum) (GeV)");
  mHistMeanDedxPbar2D->SetYTitle("mean dEdx");
  
  // MeanDedxElectron
  mHistMeanDedxElectron2D = new TH2F("Flow_MeanDedxElectron2D", 
				     "Flow_MeanDedxElectron2D",
				     nMomenBins, pMin, pMax,
				     nDedxBins, dedxMin, dedxMax);
  mHistMeanDedxElectron2D->SetXTitle("log(momentum) (GeV)");
  mHistMeanDedxElectron2D->SetYTitle("mean dEdx");
  
  // MeanDedxPositron
  mHistMeanDedxPositron2D = new TH2F("Flow_MeanDedxPositron2D", 
				     "Flow_MeanDedxPositron2D",
				     nMomenBins, pMin, pMax,
				     nDedxBins, dedxMin, dedxMax);
  mHistMeanDedxPositron2D->SetXTitle("log(momentum) (GeV)");
  mHistMeanDedxPositron2D->SetYTitle("mean dEdx");

  TString* histTitle;
  for (int i = 0; i < Flow::nSels * Flow::nSubs; i++) {
    char countSubs[2];
    sprintf(countSubs,"%d",i+1);

    // for sub-events
    for (int j = 0; j < Flow::nHars; j++) {
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

  for (int k = 0; k < Flow::nSels; k++) {
    char countSels[2];
    sprintf(countSels,"%d",k+1);

    // for each selection

    // cos(n*delta_Psi)
    histTitle = new TString("Flow_Cos_Sel");
    histTitle->Append(*countSels);
    histFull[k].mHistCos = new TProfile(histTitle->Data(), histTitle->Data(),
      Flow::nHars, 0.5, (float)(Flow::nHars) + 0.5, -1., 1., "");
    histFull[k].mHistCos->SetXTitle("Harmonic");
    histFull[k].mHistCos->SetYTitle("<cos(n*delta_Psi)>");
    delete histTitle;
    
    // resolution
    histTitle = new TString("Flow_Res_Sel");
    histTitle->Append(*countSels);
    histFull[k].mHistRes = new TH1F(histTitle->Data(), histTitle->Data(),
      Flow::nHars, 0.5, (float)(Flow::nHars) + 0.5);
    histFull[k].mHistRes->SetXTitle("Harmonic");
    histFull[k].mHistRes->SetYTitle("Resolution");
    delete histTitle;

    // vObs
    histTitle = new TString("Flow_vObs_Sel");
    histTitle->Append(*countSels);
    histFull[k].mHist_vObs = new TProfile(histTitle->Data(), histTitle->Data(),
      Flow::nHars, 0.5, (float)(Flow::nHars) + 0.5, -100., 100., "");
    histFull[k].mHist_vObs->SetXTitle("Harmonic");
    histFull[k].mHist_vObs->SetYTitle("vObs (%)");
    delete histTitle;
    
    // for each harmonic
    for (int j = 0; j < Flow::nHars; j++) {
      float order  = (float)(j+1);
      char countHars[2];
      sprintf(countHars,"%d",j+1);

      // multiplicity
      histTitle = new TString("Flow_Mul_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistMult = new TH1F(histTitle->Data(),
        histTitle->Data(), nMultBins, multMin, multMax);
      histFull[k].histFullHar[j].mHistMult->SetXTitle("Multiplicity");
      histFull[k].histFullHar[j].mHistMult->SetYTitle("Counts");
      delete histTitle;
      
      // Phi lab
      histTitle = new TString("Flow_Phi_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhi = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
      histFull[k].histFullHar[j].mHistPhi->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histFullHar[j].mHistPhi->SetYTitle("Counts");
      delete histTitle;
      
      // PhiWgt new
      histTitle = new TString("Flow_Phi_Weight_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhiWgt =	new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
      histFull[k].histFullHar[j].mHistPhiWgt->Sumw2();
      histFull[k].histFullHar[j].mHistPhiWgt->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histFullHar[j].mHistPhiWgt->SetYTitle("PhiWgt");
      delete histTitle;
      
      // Phi lab flattened
      histTitle = new TString("Flow_Phi_Flat_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhiFlat =	new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
      histFull[k].histFullHar[j].mHistPhiFlat->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histFullHar[j].mHistPhiFlat->SetYTitle("Counts");
      delete histTitle;
      
      // event plane
      histTitle = new TString("Flow_Psi_Sel");
      histTitle->Append(*countSels);
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
      histTitle->Append(*countSels);
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
      histTitle->Append(*countSels);
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
      
      // sum pt squared
      histTitle = new TString("Flow_SumPt2_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistSumPt2 = new TH1F(histTitle->Data(),
        histTitle->Data(), nSumPt2Bins, sumPt2Min, sumPt2Max);
      histFull[k].histFullHar[j].mHistSumPt2->SetXTitle("Sum Pt^2 ((GeV/c)^2");
      histFull[k].histFullHar[j].mHistSumPt2->SetYTitle("Counts");
      delete histTitle;
      
      // pt weighted with y
      histTitle = new TString("Flow_PtY_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPtY = new TH1F(histTitle->Data(),
        histTitle->Data(), nPtYBins, ptYMin, ptYMax);
      histFull[k].histFullHar[j].mHistPtY->SetXTitle("Pt*Y (GeV/c)");
      histFull[k].histFullHar[j].mHistPtY->SetYTitle("Counts");
      delete histTitle;
      
      // q
      histTitle = new TString("Flow_q_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHist_q = new TH1F(histTitle->Data(),
        histTitle->Data(), Flow::n_qBins, qMin, Flow::qMax);
      histFull[k].histFullHar[j].mHist_q->Sumw2();
      histFull[k].histFullHar[j].mHist_q->SetXTitle("q = |Q|/sqrt(Mult)");
      histFull[k].histFullHar[j].mHist_q->SetYTitle("Counts");
      delete histTitle;
      
     // particle-plane azimuthal correlation
      histTitle = new TString("Flow_Phi_Corr_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhiCorr =	new TH1F(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBins, phiMin, phiMax / order);
      histFull[k].histFullHar[j].mHistPhiCorr->Sumw2();
      histFull[k].histFullHar[j].mHistPhiCorr->
	SetXTitle("Particle-Plane Correlation (rad)");
      histFull[k].histFullHar[j].mHistPhiCorr->SetYTitle("Counts");
      delete histTitle;

      // Yield
      histTitle = new TString("Flow_Yield2D_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistYield2D =	new TH2D(histTitle->Data(),
        histTitle->Data(), 
        Flow::nSinCosYBins, Flow::sinCosYMin, Flow::sinCosYMax, 
        Flow::nSinCosPtBins, sinCosPtMin, Flow::sinCosPtMax);
      histFull[k].histFullHar[j].mHistYield2D->Sumw2();
      histFull[k].histFullHar[j].mHistYield2D->SetXTitle("Rapidity");
      histFull[k].histFullHar[j].mHistYield2D->SetYTitle("Pt (GeV)");
      delete histTitle;

      // Flow observed
      histTitle = new TString("Flow_vObs2D_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHist_vObs2D =	new TProfile2D(histTitle->Data(),
        histTitle->Data(), nYBins, yMin, yMax, nPtBins, ptMin, ptMaxPart,
							       -100., 100., "");
      histFull[k].histFullHar[j].mHist_vObs2D->SetXTitle("Rapidity");
      histFull[k].histFullHar[j].mHist_vObs2D->SetYTitle("Pt (GeV)");
      delete histTitle;

      // Flow observed profiles
      histTitle = new TString("Flow_vObsY_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHist_vObsY = new TProfile(histTitle->Data(),
        histTitle->Data(), nYBins, yMin, yMax, -100., 100., "");
      histFull[k].histFullHar[j].mHist_vObsY->SetXTitle("Rapidity");
      histFull[k].histFullHar[j].mHist_vObsY->SetYTitle("v (%)");
      delete histTitle;

      histTitle = new TString("Flow_vObsPt_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHist_vObsPt = new TProfile(histTitle->Data(),
        histTitle->Data(), nPtBins, ptMin, ptMaxPart, -100., 100., "");
      histFull[k].histFullHar[j].mHist_vObsPt->SetXTitle("Pt (GeV)");
      histFull[k].histFullHar[j].mHist_vObsPt->SetYTitle("v (%)");
      delete histTitle;

      // mean cos
      histTitle = new TString("Flow_Mean_Cos2D_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistMeanCos = new TProfile2D(histTitle->Data(),
	histTitle->Data(), Flow::nSinCosYBins, Flow::sinCosYMin, Flow::sinCosYMax, 
        Flow::nSinCosPtBins, sinCosPtMin, Flow::sinCosPtMax, "");
      histFull[k].histFullHar[j].mHistMeanCos->SetXTitle("Rapidity");
      histFull[k].histFullHar[j].mHistMeanCos->SetYTitle("Pt (GeV)");
      delete histTitle;

      // mean sin
      histTitle = new TString("Flow_Mean_Sin2D_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistMeanSin = new TProfile2D(histTitle->Data(),
        histTitle->Data(), Flow::nSinCosYBins, Flow::sinCosYMin, Flow::sinCosYMax, 
        Flow::nSinCosPtBins, sinCosPtMin, Flow::sinCosPtMax, "");
      histFull[k].histFullHar[j].mHistMeanSin->SetXTitle("Rapidity");
      histFull[k].histFullHar[j].mHistMeanSin->SetYTitle("Pt (GeV)");
      delete histTitle;

      // mean cos after shifting
      histTitle = new TString("Flow_Mean_Cos_Flat2D_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistMeanCosFlat = 
	new TProfile2D(histTitle->Data(),
	histTitle->Data(), Flow::nSinCosYBins, Flow::sinCosYMin, Flow::sinCosYMax, 
        Flow::nSinCosPtBins, sinCosPtMin, Flow::sinCosPtMax, "");
      histFull[k].histFullHar[j].mHistMeanCosFlat->SetXTitle("Rapidity");
      histFull[k].histFullHar[j].mHistMeanCosFlat->SetYTitle("Pt (GeV)");
      delete histTitle;

      // mean sin after shifting
      histTitle = new TString("Flow_Mean_Sin_Flat2D_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistMeanSinFlat =
	new TProfile2D(histTitle->Data(),
        histTitle->Data(), Flow::nSinCosYBins, Flow::sinCosYMin, Flow::sinCosYMax, 
        Flow::nSinCosPtBins, sinCosPtMin, Flow::sinCosPtMax, "");
      histFull[k].histFullHar[j].mHistMeanSinFlat->SetXTitle("Rapidity");
      histFull[k].histFullHar[j].mHistMeanSinFlat->SetYTitle("Pt (GeV)");
      delete histTitle;

    }
  }

  gMessMgr->SetLimit("##### FlowAnalysis", 2);

  return StMaker::Init();
}

//-----------------------------------------------------------------------

bool StFlowAnalysisMaker::FillFromFlowEvent() {
  // Get event quantities from StFlowEvent
  
  for (int k = 0; k < Flow::nSels; k++) {
    pFlowSelect->SetSelection(k);
    for (int j = 0; j < Flow::nHars; j++) {
      pFlowSelect->SetHarmonic(j);
      for (int n = 0; n < Flow::nSubs; n++) {
	pFlowSelect->SetSubevent(n);
	int i = Flow::nSels*k + n;
	// sub-event quantities
	mPsiSub[i][j] = pFlowEvent->Psi(pFlowSelect);
	if ( mPsiSub[i][j] == 0 ) return kFALSE; //to elliminate psi0
      }

      pFlowSelect->SetSubevent(-1);
      // full event quantities
      mQ[k][j]      = pFlowEvent->Q(pFlowSelect);
      mPsi[k][j]    = pFlowEvent->Psi(pFlowSelect);
      m_q[k][j]     = pFlowEvent->q(pFlowSelect);
      mMult[k][j]   = pFlowEvent->Mult(pFlowSelect);
      mSumPt2[k][j] = pFlowEvent->SumPt2(pFlowSelect);
      if ( mPsi[k][j] == 0 ) return kFALSE; //to elliminate psi0
    }
  }
  return kTRUE;
}

//-----------------------------------------------------------------------

void StFlowAnalysisMaker::FillEventHistograms() {
  // Fill histograms with event quantities

  // no selections: OrigMult, Centrality, Mult, MultOverOrig, VertexZ, VertexXY
  int origMult = pFlowEvent->OrigMult();
  mHistOrigMult->Fill((float)origMult);
  mHistEVeto->Fill(pFlowEvent->EVeto());
  int cent = pFlowEvent->Centrality();
  mHistCent->Fill((float)cent);
  int totalMult = pFlowEvent->TrackCollection()->size();
  mHistMult->Fill((float)totalMult);
  if (origMult) mHistMultOverOrig->Fill((float)totalMult / (float)origMult);
  StThreeVectorF vertex = pFlowEvent->VertexPos();
  mHistVertexZ->Fill(vertex.z());
  mHistVertexXY2D->Fill(vertex.x(), vertex.y());

  // sub-event Psi_Subs
  for (int i = 0; i < Flow::nSubs * Flow::nSels; i++) {
    for (int j = 0; j < Flow::nHars; j++) {
      histSub[i].histSubHar[j].mHistPsiSubs->Fill(mPsiSub[i][j]);
    }
  }

  // full event Psi, PsiSubCorr, PsiSubCorrDiff, cos, mult, pt2, q
  for (int k = 0; k < Flow::nSels; k++) {
    for (int j = 0; j < Flow::nHars; j++) {
      float order  = (float)(j+1);
      histFull[k].histFullHar[j].mHistPsi->Fill(mPsi[k][j]);
      if (mPsiSub[Flow::nSels*k][j] != 0. && mPsiSub[Flow::nSels*k+1][j] != 0.) {
	float psiSubCorr = mPsiSub[Flow::nSels*k][j] - 
	  mPsiSub[Flow::nSels*k+1][j];
	histFull[k].mHistCos->Fill(order, (float)cos(order * psiSubCorr));    
	if (psiSubCorr < 0.) psiSubCorr += twopi / order;
	histFull[k].histFullHar[j].mHistPsiSubCorr->Fill(psiSubCorr);
      }
      if (j < Flow::nHars - 1) { // subevents of different harmonics
	int j1, j2;
	float psiSubCorrDiff;
	if (j==0) {
	  j1 = 1, j2 = 2;	
	} else if (j==1) {
	  j1 = 1, j2 = 3;	
	} else if (j==2) {
	  j1 = 2, j2 = 4;	
	}
	psiSubCorrDiff = fmod((double)mPsiSub[Flow::nSels*k][j1-1],
			      twopi/(double)j2) - 
	  fmod((double)mPsiSub[Flow::nSels*k+1][j2-1], twopi/(double)j2);
	if (psiSubCorrDiff < 0.) psiSubCorrDiff += twopi/(float)j2;
	histFull[k].histFullHar[j].mHistPsiSubCorrDiff->
	  Fill(psiSubCorrDiff);
	psiSubCorrDiff = fmod((double)mPsiSub[Flow::nSels*k][j2-1],
			      twopi/(double)j2) - 
	  fmod((double)mPsiSub[Flow::nSels*k+1][j1-1], twopi/(double)j2);
	if (psiSubCorrDiff < 0.) psiSubCorrDiff += twopi/(float)j2;
	histFull[k].histFullHar[j].mHistPsiSubCorrDiff->
	  Fill(psiSubCorrDiff);
      }
      histFull[k].histFullHar[j].mHistMult->Fill((float)mMult[k][j]);
      histFull[k].histFullHar[j].mHistSumPt2->Fill(mSumPt2[k][j]);
      histFull[k].histFullHar[j].mHist_q->Fill(m_q[k][j]);
    }
  }

}

//-----------------------------------------------------------------------

void StFlowAnalysisMaker::FillParticleHistograms() {
  // Fill histograms from the particles

  float corrMultN  = 0.;
  float etaSymPosN = 0.;
  float etaSymNegN = 0.;
  float piPlusN    = 0.;
  float piMinusN   = 0.;
  float protonN    = 0.;
  float pbarN      = 0.;
  float electronN  = 0.;
  float positronN  = 0.;

  TRandom* rand = new TRandom();

  // Initialize Iterator
  StFlowTrackCollection* pFlowTracks = pFlowEvent->TrackCollection();
  StFlowTrackIterator itr;
  
  for (itr = pFlowTracks->begin(); itr != pFlowTracks->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    float phi       = pFlowTrack->Phi();
    if (phi < 0.) phi += twopi;
    float rapidity  = pFlowTrack->Y();
    float pt        = pFlowTrack->Pt();
    float eta       = pFlowTrack->Eta();
    int   charge    = pFlowTrack->Charge();
    float bx        = pFlowTrack->Bx();
    float by        = pFlowTrack->By();
    float chi2      = pFlowTrack->Chi2();
    int   fitPts    = pFlowTrack->FitPts();
    int   maxPts    = pFlowTrack->MaxPts();
    Char_t pid[10];
    strcpy(pid, pFlowTrack->Pid());
    float p         = pFlowTrack->P();
    float dEdx      = pFlowTrack->Dedx();

    // For PID multiplicites
    if (strcmp(pid, "pi+")    == 0)  piPlusN++;
    if (strcmp(pid, "pi-")    == 0)  piMinusN++;
    if (strcmp(pid, "proton") == 0)  protonN++;
    if (strcmp(pid, "pbar")   == 0)  pbarN++;
    if (strcmp(pid, "e-")     == 0)  electronN++;
    if (strcmp(pid, "e+")     == 0)  positronN++;

    // no selections: Charge, bx, by, Chi2, FitPts, MaxPts, FitOverMax
    mHistCharge->Fill((float)charge);
    mHistBx->Fill(bx);
    mHistBy->Fill(by);
    mHistChi2->Fill(chi2);
    mHistFitPts->Fill((float)fitPts);
    mHistMaxPts->Fill((float)maxPts);
    if (maxPts) mHistFitOverMax->Fill((float)fitPts/(float)maxPts);

    // PID
    if (charge == 1) {
      mHistMeanDedx2Dpos->Fill(log(p), dEdx);

      if (strcmp(pid, "pi+") == 0) {
	mHistMeanDedxPiPlus2D->Fill(log(p), dEdx);
      }
      if (strcmp(pid, "proton") == 0) {
	mHistMeanDedxProton2D->Fill(log(p), dEdx);
      }
      if (strcmp(pid, "e+") == 0) {
	mHistMeanDedxPositron2D->Fill(log(p), dEdx);
      }
    } else if (charge == -1) {
      mHistMeanDedx2Dneg->Fill(log(p), dEdx);

      if (strcmp(pid, "pi-") == 0) {
	mHistMeanDedxPiMinus2D->Fill(log(p), dEdx);
      }
      if (strcmp(pid, "pbar") == 0) {
	mHistMeanDedxPbar2D->Fill(log(p), dEdx);
      }
      if (strcmp(pid, "e-") == 0) {
	mHistMeanDedxElectron2D->Fill(log(p), dEdx);
      }
    }

    // Yield3D, Yield2D, YieldPart2D, BinY, BinPt
    mHistYPtPhi3D->Fill(rapidity, pt, phi);
    mHistYieldAll2D->Fill(rapidity, pt);
    if (pFlowSelect->SelectPart(pFlowTrack)) {
      mHistYieldPart2D->Fill(rapidity, pt);
      mHistBinY->Fill(rapidity, rapidity);
      mHistBinPt->Fill(pt, pt);
    }

    // cos(n*phiLab)
    for (int j = 0; j < Flow::nHars; j++) {
      bool oddHar = (j+1) % 2;
      double order  = (double)(j+1);
      float cosLab   = cos(order * phi)/perCent;
      if (rapidity < Flow::yCM && oddHar) cosLab *= -1;
      mHistCosPhi->Fill(order, cosLab);
    }

    // For Eta Symmetry
    if (eta > Flow::yCM) { etaSymPosN++; }
    else { etaSymNegN++; }

    // Loop over the selections and harmonics
    for (int k = 0; k < Flow::nSels; k++) {
      pFlowSelect->SetSelection(k);
      for (int j = 0; j < Flow::nHars; j++) {
	pFlowSelect->SetHarmonic(j);
	bool oddHar = (j+1) % 2;
	double order  = (double)(j+1);
	// Set the event plane angle
	float psi_i;
	if (!pFlowEvent->Stripes()) {
	  if (mV21 && j==1) {
	    // 1st harmonics eventplane used for v2 calculation
	    psi_i = mPsi[k][j-1];
	    if (psi_i > twopi/2.) psi_i -= twopi/2.;
	  } else {
	    psi_i = mPsi[k][j];
	  }
	} else { // for particles with the other subevent
	  int i = Flow::nSels*k;
	  if (pFlowTrack->Select(j,k,0)) {
	    psi_i = mPsiSub[i+1][j];
	  } else if (pFlowTrack->Select(j,k,1)) {
	    psi_i = mPsiSub[i][j];
	  } else {
	    double ran = rand->Rndm(123);
	    if (ran > 0.5) {
	      psi_i = mPsiSub[i+1][j];
	    } else {
	      psi_i = mPsiSub[i][j];
	    }
	  }
	  // if 1st harmonics eventplane is used for v2 calculation
	  if (mV21 && j==1) {
	    int i = Flow::nSels*k;
	    if (pFlowTrack->Select(j-1,k,0)) {
	      psi_i = mPsiSub[i+1][j-1];
	    } else if (pFlowTrack->Select(j-1,k,1)) {
	      psi_i = mPsiSub[i][j-1];
	    } else {
	      double ran = rand->Rndm(123);
	      if (ran > 0.5) {
		psi_i = mPsiSub[i+1][j-1];
	      } else {
		psi_i = mPsiSub[i][j-1];
	      }
	    }
	    if (psi_i > twopi/2.) psi_i -= twopi/2.;
	  }
	}

	// Remove autocorrelations
	if (pFlowSelect->Select(pFlowTrack)) {
	  histFull[k].histFullHar[j].mHistPhi->Fill(phi);
	  histFull[k].histFullHar[j].mHistYield2D->Fill(rapidity, pt);
	  histFull[k].histFullHar[j].mHistPtY->Fill(pt*rapidity);
	  double phiWgt = pFlowEvent->PhiWeight(phi, k, j);
	  histFull[k].histFullHar[j].mHistPhiFlat->Fill(phi, phiWgt);
	  if (!pFlowEvent->Stripes()) {
	    if (rapidity < Flow::yCM && oddHar) phiWgt *= -1.;
	    if (pFlowEvent->PtWgt() && !oddHar) phiWgt *= pt;
	    if (pFlowEvent->YWgt() && oddHar)   phiWgt *= fabs(rapidity -
							       Flow::yCM);
	    double meanCos = pFlowEvent->MeanCos(rapidity, pt, j);
	    double meanSin = pFlowEvent->MeanSin(rapidity, pt, j);
	    TVector2 Q_i;
	    Q_i.Set(phiWgt * (cos(phi * order) - meanCos),
		    phiWgt * (sin(phi * order) - meanSin));
	    TVector2 mQ_i = mQ[k][j] - Q_i;
	    psi_i = mQ_i.Phi() / order;
	    if (psi_i < 0.) psi_i += twopi / order;
	  }
	  
	  // Remove autocorrelation for 2nd  harmonic when 1st harmonic 
	  // event plane is used for analysis
	  if (!pFlowEvent->Stripes() && mV21 && j==1) {
	    pFlowSelect->SetHarmonic(j-1);
	    if (pFlowSelect->Select(pFlowTrack)) {
	      double phiWgt = pFlowEvent->PhiWeight(phi, k, j-1);
	      if (rapidity < Flow::yCM && oddHar) phiWgt *= -1.;
	      if (pFlowEvent->PtWgt() && !oddHar) phiWgt *= pt;
	      if (pFlowEvent->YWgt() && oddHar)   phiWgt *= fabs(rapidity -
								 Flow::yCM);
	      double meanCos = pFlowEvent->MeanCos(rapidity, pt, j-1);
	      double meanSin = pFlowEvent->MeanSin(rapidity, pt, j-1);
	      TVector2 Q_i;
	      Q_i.Set(phiWgt * (cos(phi * (order-1)) - meanCos),
		      phiWgt * (sin(phi * (order-1)) - meanSin));
	      TVector2 mQ_i = mQ[k][j-1] - Q_i;
	      psi_i = mQ_i.Phi() / (order-1);
	      if (psi_i < 0.) psi_i += twopi / (order-1);
	      if (psi_i > pi) psi_i -= pi;
	    }	    
	  }
	}

       	// Caculate v for all particles selected for correlation analysis
	if (pFlowSelect->SelectPart(pFlowTrack)) {
	  corrMultN++;
	  float v = 0.;
	  if(order == 2 && pFlowEvent->SinOnly())
	    v = (sin(order*phi) * sin(order*psi_i))/perCent;
	  else	  
	    v = cos(order * (phi - psi_i))/perCent;
	  //float vFlip = v;
	  //if (rapidity < Flow::yCM && oddHar) vFlip *= -1;
	  histFull[k].histFullHar[j].mHist_vObs2D->Fill(rapidity, pt, v);
	  histFull[k].histFullHar[j].mHist_vObsY-> Fill(rapidity, v);
	  //histFull[k].histFullHar[j].mHist_vObsPt->Fill(pt, vFlip);
	  //histFull[k].mHist_vObs->Fill(order, vFlip);
	  if (rapidity > Flow::yCM) { // forward hemisphere only
	    histFull[k].histFullHar[j].mHist_vObsPt->Fill(pt, v);
	    histFull[k].mHist_vObs->Fill(order, v);
	  }      
	  // Correlation of Phi of all particles with Psi
	  float phi_i = phi;
	  if (rapidity < Flow::yCM && oddHar) {
	    phi_i += pi; // backward particle and odd harmonic
	    if (phi_i > twopi) phi_i -= twopi;
	  }
	  float dPhi = phi_i - psi_i;
	  if (dPhi < 0.) dPhi += twopi;
	  histFull[k].histFullHar[j].mHistPhiCorr->
	    Fill(fmod((double)dPhi, twopi / order));
	}

	// fill mean sin/cos tabels
	if (pFlowSelect->Select(pFlowTrack)) {
	  histFull[k].histFullHar[j].mHistMeanCos->Fill(rapidity, pt, 
							cos(order * phi));
	  histFull[k].histFullHar[j].mHistMeanSin->Fill(rapidity, pt, 
							sin(order * phi));
	  // fill mean sin/cos after shifting
	  double meanCos = pFlowEvent->MeanCos(rapidity, pt, j);
	  double meanSin = pFlowEvent->MeanSin(rapidity, pt, j);
	  histFull[k].histFullHar[j].mHistMeanCosFlat->Fill(rapidity, pt, 
				cos(order * phi) - meanCos);
	  histFull[k].histFullHar[j].mHistMeanSinFlat->Fill(rapidity, pt, 
				sin(order * phi) - meanSin);
	  
	  //for testing
	  //  histFull[k].histFullHar[j].mHistMeanCosFlat->Fill(rapidity, pt,
	  // 							    meanCos);
	  //  histFull[k].histFullHar[j].mHistMeanSinFlat->Fill(rapidity, pt, 
	  // 							    meanSin);
	}
      }
    }  
  }

  // EtaSym
  float etaSym = (etaSymPosN - etaSymNegN) / (etaSymPosN + etaSymNegN);
  mHistEtaSym->Fill(etaSym);
  
  // PID multiplicities
  float totalMult = (float)pFlowEvent->TrackCollection()->size();
  mHistPidMult->Fill(1., totalMult);
  mHistPidMult->Fill(2., piPlusN);
  mHistPidMult->Fill(3., piMinusN);
  mHistPidMult->Fill(4., protonN);
  mHistPidMult->Fill(5., pbarN);
  mHistPidMult->Fill(6., electronN);
  mHistPidMult->Fill(7., positronN);

  // Multiplicity of particles correlated with the event planes
  corrMultN = corrMultN / (float)(Flow::nHars * Flow::nSels);
  mHistMultPart->Fill(corrMultN);

  delete rand;

}

//-----------------------------------------------------------------------

static Double_t resEventPlane(double chi) {
  // Calculates the event plane resolution as a function of chi

  double con = 0.626657;                   // sqrt(pi/2)/2
  double arg = chi * chi / 4.;

  Double_t res = con * chi * exp(-arg) * (TMath::BesselI0(arg) + 
					  TMath::BesselI1(arg)); 

  return res;
}

//-----------------------------------------------------------------------

static Double_t resEventPlaneK2(double chi) {
  // Calculates the event plane resolution as a function of chi
  //  for the case k=2, elliptic flow relative to the 1st har. event plane.

  double con = 0.626657;                   // sqrt(pi/2)/2
  double arg = chi * chi / 4.;

  double besselOneHalf = sqrt(arg/halfpi) * sinh(arg)/arg;
  double besselThreeHalfs = sqrt(arg/halfpi) * (cosh(arg)/arg - sinh(arg)/(arg*arg));
  Double_t res = con * chi * exp(-arg) * (besselOneHalf + besselThreeHalfs); 

  return res;
}

//-----------------------------------------------------------------------

static Double_t chi(double res) {
  // Calculates chi from the event plane resolution

  double chi   = 2.0;
  double delta = 1.0;

  for (int i = 0; i < 15; i++) {
    chi = (resEventPlane(chi) < res) ? chi + delta : chi - delta;
    delta = delta / 2.;
  }

  return chi;
}

//-----------------------------------------------------------------------

Int_t StFlowAnalysisMaker::Finish() {
  // Calculates resolution and flow values
  // Outputs phiWgt and meanSinCos values
  TString* histTitle;

  // Flattening histogram collections
  TOrdCollection* phiWgtHistNames = new TOrdCollection(Flow::nSels*Flow::nHars);
  TOrdCollection* meanSinCosHistNames = new TOrdCollection(2*Flow::nHars);

  // Calculate resolution from sqrt(mHistCos)
  double cosPair[Flow::nSels][Flow::nHars];
  double cosPairErr[Flow::nSels][Flow::nHars];
  double content;
  double error;
  double totalError;
  for (int k = 0; k < Flow::nSels; k++) {
    char countSels[2];
    sprintf(countSels,"%d",k+1);
    // Create the 1D v histogram
    histTitle = new TString("Flow_v_Sel");
    histTitle->Append(*countSels);
    histFull[k].mHist_v = 
      histFull[k].mHist_vObs->ProjectionX(histTitle->Data());
    histFull[k].mHist_v->SetTitle(histTitle->Data());
    histFull[k].mHist_v->SetXTitle("Harmonic");
    histFull[k].mHist_v->SetYTitle("v (%)");
    delete histTitle;
    AddHist(histFull[k].mHist_v);

    for (int j = 0; j < Flow::nHars; j++) {
      char countHars[2];
      sprintf(countHars,"%d",j+1);
      cosPair[k][j]    = histFull[k].mHistCos->GetBinContent(j+1);
      cosPairErr[k][j] = histFull[k].mHistCos->GetBinError(j+1);
      if (!pFlowEvent->Stripes()) {
	if (cosPair[k][j] > 0.92) {      // resolution saturates
	  mRes[k][j]    = 0.99;
	  mResErr[k][j] = 0.007;
	} else if (cosPair[k][j] > 0.) {
	  double deltaResSub = 0.005;  // differential for the error propergation
	  double resSub = sqrt(cosPair[k][j]);
	  double resSubErr = cosPairErr[k][j] / (2. * resSub);
	  double chiSub = chi(resSub);
	  double chiSubDelta = chi(resSub + deltaResSub);
	  mRes[k][j] = resEventPlane(sqrt(2.) * chiSub); // full event plane res.
	  double mResDelta = resEventPlane(sqrt(2.) * chiSubDelta);
	  mResErr[k][j] = resSubErr * fabs((double)mRes[k][j] - mResDelta) 
	    / deltaResSub;
	} else {
	  mRes[k][j]    = 0.;     // subevent correlation must be positive
	  mResErr[k][j] = 0.;
	}
	if (j==0) {               // k=2 resolution
	  if (cosPair[k][j] > 0.) {
	    double deltaResSub = 0.005;  // differential for the error propergation
	    double resSub = sqrt(cosPair[k][j]);
	    double resSubErr = cosPairErr[k][j] / (2. * resSub);
	    double chiSub = chi(resSub);
	    double chiSubDelta = chi(resSub + deltaResSub);
	    mResK2[k] = resEventPlaneK2(sqrt(2.) * chiSub); // full event plane res.
	    double mResDelta = resEventPlaneK2(sqrt(2.) * chiSubDelta);
	    mResK2Err[k] = resSubErr * fabs((double)mResK2[k] - mResDelta) 
	      / deltaResSub;
	  } else {
	    mResK2[k]    = 0.;     // subevent correlation must be positive
	    mResK2Err[k] = 0.;
	  }
	}
      } else { // sub res only
	if (cosPair[k][j] > 0.) {
	  double resSub = sqrt(cosPair[k][j]);
	  double resSubErr = cosPairErr[k][j] / (2. * resSub);
	  mRes[k][j]    = resSub;
	  mResErr[k][j] = resSubErr;
	} else {
	  mRes[k][j]    = 0.;     // subevent correlation must be positive
	  mResErr[k][j] = 0.;
	}
      }

      // Create the v 2D histogram
      histTitle = new TString("Flow_v2D_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHist_v2D = 
	histFull[k].histFullHar[j].mHist_vObs2D->ProjectionXY(histTitle->Data());
      histFull[k].histFullHar[j].mHist_v2D->SetTitle(histTitle->Data());
      histFull[k].histFullHar[j].mHist_v2D->SetXTitle("Rapidity");
      histFull[k].histFullHar[j].mHist_v2D->SetYTitle("Pt (GeV)");
      histFull[k].histFullHar[j].mHist_v2D->SetZTitle("v (%)");
      delete histTitle;
      AddHist(histFull[k].histFullHar[j].mHist_v2D);

      // Create the v 1D histograms
      histTitle = new TString("Flow_vY_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHist_vY = 
	histFull[k].histFullHar[j].mHist_vObsY->ProjectionX(histTitle->Data());
      histFull[k].histFullHar[j].mHist_vY->SetTitle(histTitle->Data());
      histFull[k].histFullHar[j].mHist_vY->SetXTitle("Rapidity");
      histFull[k].histFullHar[j].mHist_vY->SetYTitle("v (%)");
      delete histTitle;
      AddHist(histFull[k].histFullHar[j].mHist_vY);

      TString* histTitle = new TString("Flow_vPt_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHist_vPt = 
	histFull[k].histFullHar[j].mHist_vObsPt->ProjectionX(histTitle->Data());
      histFull[k].histFullHar[j].mHist_vPt->SetTitle(histTitle->Data());
      histFull[k].histFullHar[j].mHist_vPt->SetXTitle("Pt (GeV)");
      histFull[k].histFullHar[j].mHist_vPt->SetYTitle("v (%)");
      delete histTitle;
      AddHist(histFull[k].histFullHar[j].mHist_vPt);

      // Calulate v = vObs / Resolution
      if (j==1) {
	cout << "##### Resolution of the 2nd harmonic = " << 
	  mRes[k][j] << " +/- " << mResErr[k][j] << endl;
	if (mResK2[k] != 0.) {
	  cout << "##### Resolution of the 2nd harmonic k=2 = " << 
	    mResK2[k] << " +/- " << mResK2Err[k] << endl;
	} else {
	  cout << "##### Resolution of the 2nd harmonic k=2 was zero." << endl;
	}
	if (mV21) {
	  mRes[k][j]    = mResK2[k];
	  mResErr[k][j] = mResK2Err[k];
	  cout<<"##### v2 relative to 1st har. event plane."<<endl;
	}
      }
      histFull[k].mHistRes->SetBinContent(j+1, mRes[k][j]);
      histFull[k].mHistRes->SetBinError(j+1, mResErr[k][j]);

      if (mRes[k][j] != 0.) {
	cout << "##### Resolution of the " << j+1 << "th harmonic = " << 
	  mRes[k][j] << " +/- " << mResErr[k][j] << endl;
	// The systematic error of the resolution is not folded in.
	histFull[k].histFullHar[j].mHist_v2D->Scale(1. / mRes[k][j]);
	histFull[k].histFullHar[j].mHist_vY->Scale(1. / mRes[k][j]);
	histFull[k].histFullHar[j].mHist_vPt->Scale(1. / mRes[k][j]);
	// correction for using sin correlation terms only
	if (j == 1 && pFlowEvent->SinOnly()) {
	  histFull[k].histFullHar[j].mHist_v2D->Scale(1.41);
	  histFull[k].histFullHar[j].mHist_vY->Scale(1.41);
	  histFull[k].histFullHar[j].mHist_vPt->Scale(1.41);
	}	  
	content = histFull[k].mHist_v->GetBinContent(j+1);
	content /=  mRes[k][j];
	histFull[k].mHist_v->SetBinContent(j+1, content);
	// The systematic error of the resolution is folded in.
	error = histFull[k].mHist_v->GetBinError(j+1);
	error /= mRes[k][j];
	totalError = fabs(content) * sqrt((error/content)*(error/content) +
	       (mResErr[k][j]/mRes[k][j])*(mResErr[k][j]/mRes[k][j]));
	histFull[k].mHist_v->SetBinError(j+1, totalError);
	cout << "##### v" << j+1 << "= (" << content << " +/- " << error << 
	  " +/- " << totalError << "(with syst.)) %" << endl;
      } else {
	cout << "##### Resolution of the " << j+1 << "th harmonic was zero."
	     << endl;
	histFull[k].histFullHar[j].mHist_v2D->Reset();
	histFull[k].histFullHar[j].mHist_vY ->Reset();
	histFull[k].histFullHar[j].mHist_vPt->Reset();
	histFull[k].mHist_v->SetBinContent(j+1, 0.);
	histFull[k].mHist_v->SetBinError(j+1, 0.);
      }

      // Calculate PhiWgt
      double mean = histFull[k].histFullHar[j].mHistPhi->Integral() 
	/ (double)Flow::nPhiBins;
      for (int i = 0; i < Flow::nPhiBins; i++) {
	histFull[k].histFullHar[j].mHistPhiWgt->SetBinContent(i+1, mean);
	histFull[k].histFullHar[j].mHistPhiWgt->SetBinError(i+1, 0.);
      }
      histFull[k].histFullHar[j].mHistPhiWgt->
	Divide(histFull[k].histFullHar[j].mHistPhi);
      phiWgtHistNames->AddLast(histFull[k].histFullHar[j].mHistPhiWgt);
      if (k == 0) {
	meanSinCosHistNames->AddLast(histFull[k].histFullHar[j].mHistMeanCos);
	meanSinCosHistNames->AddLast(histFull[k].histFullHar[j].mHistMeanSin);
      }
    }
  }
  //GetHistList()->ls();

  // Write all histograms
  TString* fileName = new TString("flow.hist.root");
  TFile histFile(fileName->Data(), "RECREATE");
  //histFile.SetFormat(1);
  GetHistList()->Write();
  histFile.Close();
  delete fileName;
  
  // Write PhiWgt histograms
  fileName = new TString("flowPhiWgtNew.root");
  TFile phiWgtNewFile(fileName->Data(), "RECREATE");
  phiWgtHistNames->Write();
  phiWgtNewFile.Close();
  delete fileName;
  delete phiWgtHistNames;

  // Write mean sin/cos histograms
  fileName = new TString("flowMeanSinCosNew.root");
  TFile meanSinCosNewFile(fileName->Data(), "RECREATE");
  meanSinCosHistNames->Write();
  meanSinCosNewFile.Close();
  delete fileName;
  delete meanSinCosHistNames;

  // Print the selection object details
  pFlowSelect->PrintList();

  delete pFlowSelect;

  cout << endl;
  gMessMgr->Summary(3);
  cout << endl;

  return StMaker::Finish();
}
