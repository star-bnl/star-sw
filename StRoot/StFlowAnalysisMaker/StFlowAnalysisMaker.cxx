////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowAnalysisMaker.cxx,v 1.25 2000/03/21 00:24:43 posk Exp $
//
// Authors: Raimond Snellings and Art Poskanzer, LBNL, Aug 1999
//
////////////////////////////////////////////////////////////////////////////
//
// Description:  Maker to analyze Flow using the FlowTags and/or StFlowEvent
//
////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowAnalysisMaker.cxx,v $
// Revision 1.25  2000/03/21 00:24:43  posk
// Added GetCVS and changed some plot names.
//
// Revision 1.24  2000/03/15 23:32:03  posk
// Added StFlowSelection.
//
// Revision 1.23  2000/03/02 22:55:32  posk
// Changed header file extensions from .hh to .h .
//
// Revision 1.22  2000/02/29 21:55:12  posk
// Removed static const int& statements.
//
// Revision 1.21  2000/02/18 23:44:52  posk
// Added PID and centrality.
//
// Revision 1.20  2000/02/10 01:47:30  snelling
// Make changes for HP compiler
//
// Revision 1.19  2000/02/04 16:26:41  posk
// Added correct calculation of event plane resolution for large flow.
//
// Revision 1.18  2000/01/27 00:04:29  posk
// Corrected error in pt plots.
//
// Revision 1.17  2000/01/24 23:02:11  posk
// Merged updates
//
// Revision 1.16  2000/01/14 02:09:24  snelling
// Fixed small typo (,)
//
// Revision 1.15  2000/01/14 01:35:52  snelling
// changed include path ../FlowMaker/ to FlowMaker/
//
// Revision 1.14  2000/01/14 01:13:34  snelling
// modified spt (sum pt) to mpt (mean pt) because FlowTag changed
//
// Revision 1.13  2000/01/13 21:50:22  posk
// Updates and corrections.
//
// Revision 1.12  1999/12/21 01:19:26  posk
// Added more histograms.
//
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
//
// Revision 1.0  1999/08/02 
//
//  
////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <math.h>
#include "StMaker.h"
#include "StFlowAnalysisMaker.h"
#include "StFlowMaker/StFlowMaker.h"
#include "StFlowMaker/StFlowEvent.h"
#include "StFlowTagMaker/StFlowTagMaker.h"
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
#include "TF1.h"
#include "TOrdCollection.h"
#define PR(x) cout << "##### FlowAnalysis: " << (#x) << " = " << (x) << endl;
extern "C" float besi0_(const float&);
extern "C" float besi1_(const float&);

ClassImp(StFlowAnalysisMaker)

const Float_t StFlowAnalysisMaker::etaMin = -2.;
const Float_t StFlowAnalysisMaker::etaMax =  2.;
const Float_t StFlowAnalysisMaker::ptMin  =  0.;
const Float_t StFlowAnalysisMaker::ptMax  =  2.;
const Float_t StFlowAnalysisMaker::qMax   =  5.;

enum { nEtaBins = 20,
       nPtBins  = 10,
       n_qBins  = 50};
// nPhiBins is in StFlowConstants.h

//-----------------------------------------------------------------------

StFlowAnalysisMaker::StFlowAnalysisMaker(const Char_t* name): StMaker(name),
  MakerName(name) {
  pFlowSelect = new StFlowSelection();
}

StFlowAnalysisMaker::StFlowAnalysisMaker(const Char_t* name,
					 const StFlowSelection& flowSelect) :
  StMaker(name),
  MakerName(name) {
  pFlowSelect = new StFlowSelection(flowSelect); //copy constructor
}

//-----------------------------------------------------------------------

StFlowAnalysisMaker::~StFlowAnalysisMaker() {
}

//-----------------------------------------------------------------------

Int_t StFlowAnalysisMaker::Make() {
  // Make histograms

  // Get a pointer to the flow tags
  StFlowTagMaker* pFlowTagMaker = NULL;
  pFlowTag = NULL;
  pFlowTagMaker = (StFlowTagMaker*)GetMaker("FlowTag");
  if (pFlowTagMaker) pFlowTag = pFlowTagMaker->TagPointer();

  // Get a pointer to StFlowEvent
  StFlowMaker* pFlowMaker = NULL;
  pFlowMaker = (StFlowMaker*)GetMaker("Flow");
  if (pFlowMaker) pFlowEvent = pFlowMaker->FlowEventPointer();

  if (pFlowSelect->Select(pFlowEvent)) {     // event selected

    // Event quantities
    if (pFlowTag) {
      FillFromTags();                        // get event quantities
      FillEventHistograms();                 // fill from Flow Tags
    } else if (pFlowEvent) {
      cout << "$$$$$ null FlowTag pointer" << endl;
      FillFromFlowEvent();                   // get event quantities
      FillEventHistograms();                 // fill from FlowEvent
    } else {
      cout << "$$$$$ null FlowEvent and FlowTag pointers" << endl;
      return kStOK;
    }
    
    // Particle quantities
    if (pFlowEvent) FillParticleHistograms(); // fill particle histograms
    
    PrintInfo();
    
  }
  
  // Clean up
  if (pFlowEvent) {
    delete pFlowEvent;    // it deletes pTrackCollection;
  }

  return kStOK;
}

//-----------------------------------------------------------------------

void StFlowAnalysisMaker::PrintInfo() {
  cout << "*************************************************************" << endl;
  cout << "$Id: StFlowAnalysisMaker.cxx,v 1.25 2000/03/21 00:24:43 posk Exp $"
       << endl;
  cout << "*************************************************************" << endl;
  if (Debug()) StMaker::PrintInfo();
}

//-----------------------------------------------------------------------

Int_t StFlowAnalysisMaker::Init() {
  // Book histograms

  const float chargeMin       =   -3.;
  const float chargeMax       =    3.; 
  const float dcaMin          =    0.;
  const float dcaMax          =   0.3; 
  const float chi2Min         =    0.;
  const float chi2Max         =    3.; 
  const float fitPtsMin       =    0.;
  const float fitPtsMax       =   60.; 
  const float maxPtsMin       =    0.;
  const float maxPtsMax       =   60.; 
  const float fitOverMaxMin   =    0.;
  const float fitOverMaxMax   =    2.; 
  const float origMultMin     =    0.;
  const float origMultMax     = 4000.; 
  const float totalMultMin    =    0.;
  const float totalMultMax    = 2000.; 
  const float corrMultMin     =    0.;
  const float corrMultMax     = 2000.; 
  const float multOverOrigMin =    0.;
  const float multOverOrigMax =    1.; 
  const float vertexZMin      = -100.;
  const float vertexZMax      =  100.; 
  const float vertexXYMin     =   -1.;
  const float vertexXYMax     =    1.; 
  const float etaSymMin       =  -0.2; 
  const float etaSymMax       =   0.2; 
  const float phiMin          =    0.;
  const float phiMax          = twopi; 
  const float psiMin          =    0.;
  const float psiMax          = twopi; 
  const float meanPtMin       =    0.;
  const float meanPtMax       =    1.;
  const float multMin         =    0.;
  const float multMax         = 2000.;
  const float qMin            =    0.;
  const float pidMin          =  -10.;
  const float pidMax          =   10.;
  const float centMin         =   0.5;
  const float centMax         =   7.5;

  enum { nChargeBins       = 50,
	 nDcaBins          = 50,
	 nChi2Bins         = 50,
	 nFitPtsBins       = 60,
	 nMaxPtsBins       = 60,
	 nFitOverMaxBins   = 50,
	 nOrigMultBins     = 50,
	 nTotalMultBins    = 50,
	 nMultOverOrigBins = 50,
	 nCorrMultBins     = 50,
	 nVertexZBins      = 50,
	 nVertexXYBins     = 50,
	 nEtaSymBins       = 50,
	 nPhi3DBins        = 18,
	 nPsiBins          = 36,
	 nMultBins         = 50,
	 nMeanPtBins       = 50,
	 nPidBins          = 50,
         nCentBins         =  7 };
  
  // Charge
  mHistCharge = new TH1F("Flow_Charge", "Flow_Charge",
      nChargeBins, chargeMin, chargeMax);
  mHistCharge->SetXTitle("Charge");
  mHistCharge->SetYTitle("Counts");
    
  // Distance of closest approach
  mHistDca = new TH1F("Flow_Dca", "Flow_Dca",
      nDcaBins, dcaMin, dcaMax);
  mHistDca->SetXTitle("Track dca to Vertex (cm)");
  mHistDca->SetYTitle("Counts");
    
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
  mHistCorrMult = new TH1F("Flow_CorrMult", "Flow_CorrMult",
      nCorrMultBins, corrMultMin, corrMultMax);
  mHistCorrMult->SetXTitle("Mult of Correlated Particles");
  mHistCorrMult->SetYTitle("Counts");
    
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
    
  // EtaPtPhi
  mHistEtaPtPhi3D = new TH3F("Flow_EtaPtPhi3D", "Flow_EtaPtPhi3D",
      nEtaBins, etaMin, etaMax, nPtBins, ptMin, ptMax, nPhi3DBins,
      phiMin, phiMax);
  mHistEtaPtPhi3D->SetXTitle("Eta");
  mHistEtaPtPhi3D->SetYTitle("Pt (GeV)");
  mHistEtaPtPhi3D->SetZTitle("Phi (rad)");
    
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
  
  // cos(n*phiLab)
  mHistCosPhi = new TProfile("Flow_CosPhiLab", "Flow_CosPhiLab",
    Flow::nHars, 0.5, (float)(Flow::nHars) + 0.5, -100., 100., "");
  mHistCosPhi->SetXTitle("Harmonic");
  mHistCosPhi->SetYTitle("<cos(n*PhiLab)> (%)");
    
  // PID pi+
  mHistPidPiPlus = new TH1F("Flow_PidPiPlus", "Flow_PidPiPlus",
      nPidBins, pidMin, pidMax);
  mHistPidPiPlus->SetXTitle("(PID - Mean) / Resolution");
  mHistPidPiPlus->SetYTitle("Counts");
    
  // PID pi-
  mHistPidPiMinus = new TH1F("Flow_PidPiMinus", "Flow_PidPiMinus",
      nPidBins, pidMin, pidMax);
  mHistPidPiMinus->SetXTitle("(PID - Mean) / Resolution");
  mHistPidPiMinus->SetYTitle("Counts");
    
  // PID proton
  mHistPidProton = new TH1F("Flow_PidProton", "Flow_PidProton",
      nPidBins, pidMin, pidMax);
  mHistPidProton->SetXTitle("(PID - Mean) / Resolution");
  mHistPidProton->SetYTitle("Counts");
    
  // PID multiplicities
  mHistPidMult = new TProfile("Flow_PidMult", "Flow_PidMult",
    4, 0.5, 4.5, 0., 10000., "");
  mHistPidMult->SetXTitle("All, Pi+, Pi-, Proton");
  mHistPidMult->SetYTitle("Multiplicity");
    
  // Centrality
  mHistCent = new TH1F("Flow_Cent", "Flow_Cent",
      nCentBins, centMin, centMax);
  mHistCent->SetXTitle("Centrality Bin");
  mHistCent->SetYTitle("Counts");
    
  TString* histTitle;
  for (int i = 0; i < Flow::nSels + Flow::nSubs; i++) {
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

    // for sub-event pairs

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

    // for each harmonic
    for (int j = 0; j < Flow::nHars; j++) {
      float order  = (float)(j+1);
      char countHars[2];
      sprintf(countHars,"%d",j+1);

      // multiplicity
      histTitle = new TString("Flow_Mult_Sel");
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
      
      // q
      histTitle = new TString("Flow_q_Sel");
      histTitle->Append(*countSels);
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
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistMeanPt = new TH1F(histTitle->Data(),
        histTitle->Data(), nMeanPtBins, meanPtMin, meanPtMax);
      histFull[k].histFullHar[j].mHistMeanPt->SetXTitle("Mean Pt (GeV)");
      histFull[k].histFullHar[j].mHistMeanPt->SetYTitle("Counts");
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

      // Sum of v
      histTitle = new TString("Flow_Sum_v2D_Sel");
      histTitle->Append(*countSels);
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
      histTitle->Append(*countSels);
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
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHist_vObs2D =	new TH2F(histTitle->Data(),
        histTitle->Data(), nEtaBins, etaMin, etaMax, nPtBins, ptMin, ptMax);
      histFull[k].histFullHar[j].mHist_vObs2D->Sumw2();
      histFull[k].histFullHar[j].mHist_vObs2D->SetXTitle("Pseudorapidity");
      histFull[k].histFullHar[j].mHist_vObs2D->SetYTitle("Pt (GeV)");
      delete histTitle;

      // Flow observed projections
      histTitle = new TString("Flow_vObsEta_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHist_vObsEta = new TProfile(histTitle->Data(),
        histTitle->Data(), 2*nEtaBins, etaMin, etaMax, -100., 100., "");
      histFull[k].histFullHar[j].mHist_vObsEta->SetXTitle("Pseudorapidity");
      histFull[k].histFullHar[j].mHist_vObsEta->SetYTitle("Flow (%)");
      delete histTitle;

      histTitle = new TString("Flow_vObsPt_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHist_vObsPt = new TProfile(histTitle->Data(),
        histTitle->Data(), 2*nPtBins, ptMin, ptMax, -100., 100., "");
      histFull[k].histFullHar[j].mHist_vObsPt->SetXTitle("Pt (GeV)");
      histFull[k].histFullHar[j].mHist_vObsPt->SetYTitle("Flow (%)");
      delete histTitle;

    }
  }

  return StMaker::Init();
}

//-----------------------------------------------------------------------

void StFlowAnalysisMaker::FillFromTags() {
//   Get the flow tags and calculate the full event quantities

  for (int j = 0; j < Flow::nHars; j++) {
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
    mMeanPtSub[0][j] = pFlowTag->mpta[j];
    mMeanPtSub[1][j] = pFlowTag->mptb[j];
    mMeanPtSub[2][j] = pFlowTag->mptc[j];
    mMeanPtSub[3][j] = pFlowTag->mptd[j];

    // calculate Psi
    for (int i = 0; i < Flow::nSels + Flow::nSubs; i++) {
      mPsiSub[i][j] = mQSub[i][j].Phi() / order;
    }

    // full event quantities
    for (int k = 0; k < Flow::nSels; k++) {
      mQ[k][j]      = mQSub[2*k][j] + mQSub[2*k+1][j];
      mPsi[k][j]    = mQ[k][j].Phi() / order;
      mMult[k][j]   = mMultSub[2*k][j] + mMultSub[2*k+1][j];
      m_q[k][j]     = (mMult[k][j] > 0) ? mQ[k][j].Mod()/sqrt((double)mMult[k][j])
	: 0.;
      mMeanPt[k][j] = (mMeanPtSub[2*k][j] + mMeanPtSub[2*k+1][j])/2.;
    }
  }

}

//-----------------------------------------------------------------------

void StFlowAnalysisMaker::FillFromFlowEvent() {
  // Get event quantities from StFlowEvent
  
  for (int k = 0; k < Flow::nSels; k++) {
    pFlowSelect->SetSelection(k);
    for (int j = 0; j < Flow::nHars; j++) {
      pFlowSelect->SetHarmonic(j);
      for (int n = 0; n < Flow::nSubs; n++) {
	pFlowSelect->SetSubevent(n);
	int i = 2*k + n;
	// sub-event quantities
	mPsiSub[i][j] = pFlowEvent->Psi(pFlowSelect);
      }

      pFlowSelect->SetSubevent(-1);
      // full event quantities
      mQ[k][j]      = pFlowEvent->Q(pFlowSelect);
      mPsi[k][j]    = pFlowEvent->Psi(pFlowSelect);
      m_q[k][j]     = pFlowEvent->q(pFlowSelect);
      mMult[k][j]   = pFlowEvent->Mult(pFlowSelect);
      mMeanPt[k][j] = pFlowEvent->MeanPt(pFlowSelect);
    }
  }

}

//-----------------------------------------------------------------------

void StFlowAnalysisMaker::FillEventHistograms() {
  // Fill histograms with event quantities

  // no selections: OrigMult, Centrality, Mult, MultOverOrig, VertexZ, VertexXY
  int origMult = pFlowEvent->OrigMult();
  mHistOrigMult->Fill((float)origMult);
  int cent = pFlowEvent->Centrality();
  mHistCent->Fill((float)cent);
  int totalMult = pFlowEvent->TrackCollection()->size();
  mHistMult->Fill((float)totalMult);
  if (origMult) mHistMultOverOrig->Fill((float)totalMult / (float)origMult);

  StThreeVectorF vertex = pFlowEvent->VertexPos();
  mHistVertexZ->Fill(vertex.z());
  mHistVertexXY2D->Fill(vertex.x(), vertex.y());

  // sub-event Psi_Subs
  for (int i = 0; i < Flow::nSubs + Flow::nSels; i++) {
    for (int j = 0; j < Flow::nHars; j++) {
      histSub[i].histSubHar[j].mHistPsiSubs->Fill(mPsiSub[i][j]);
    }
  }

  // full event Psi, PsiSubCorr, PsiSubCorrDiff, cos, mult, q, <Pt>
  for (int k = 0; k < Flow::nSels; k++) {
    for (int j = 0; j < Flow::nHars; j++) {
      float order  = (float)(j+1);
      histFull[k].histFullHar[j].mHistPsi->Fill(mPsi[k][j]);
      float psiSubCorr = mPsiSub[2*k][j] - mPsiSub[2*k+1][j];
      histFull[k].mHistCos->Fill(order, (float)cos(order * psiSubCorr));    
      if (psiSubCorr < 0.) psiSubCorr += twopi / order;
      histFull[k].histFullHar[j].mHistPsiSubCorr->Fill(psiSubCorr);
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
	psiSubCorrDiff = fmod((double)mPsiSub[2*k][j1-1], twopi/(double)j2) - 
	  fmod((double)mPsiSub[2*k+1][j2-1], twopi/(double)j2);
	if (psiSubCorrDiff < 0.) psiSubCorrDiff += twopi/(float)j2;
	histFull[k].histFullHar[j].mHistPsiSubCorrDiff->
	  Fill(psiSubCorrDiff);
	psiSubCorrDiff = fmod((double)mPsiSub[2*k][j2-1], twopi/(double)j2) - 
	  fmod((double)mPsiSub[2*k+1][j1-1], twopi/(double)j2);
	if (psiSubCorrDiff < 0.) psiSubCorrDiff += twopi/(float)j2;
	histFull[k].histFullHar[j].mHistPsiSubCorrDiff->
	  Fill(psiSubCorrDiff);
      }
      histFull[k].histFullHar[j].mHistMult->Fill((float)mMult[k][j]);
      histFull[k].histFullHar[j].mHist_q->Fill(m_q[k][j]);
      if (mMult[k][j] > 0) {
	histFull[k].histFullHar[j].mHistMeanPt->Fill(mMeanPt[k][j]);
      }
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

  // Initialize Iterator
  StFlowTrackCollection* pFlowTracks = pFlowEvent->TrackCollection();
  StFlowTrackIterator itr;
  
  for (itr = pFlowTracks->begin(); itr != pFlowTracks->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    float phi    = pFlowTrack->Phi();
    if (phi < 0.) phi += twopi;
    float eta    = pFlowTrack->Eta();
    float pt     = pFlowTrack->Pt();
    int   charge = pFlowTrack->Charge();
    float dca    = pFlowTrack->Dca();
    float chi2   = pFlowTrack->Chi2();
    int   fitPts = pFlowTrack->FitPts();
    int   maxPts = pFlowTrack->MaxPts();

    // no selections: Charge, Dca, Chi2, FitPts, MaxPts, FitOverMax, PID
    mHistCharge->Fill((float)charge);
    mHistDca->Fill(dca);
    mHistChi2->Fill(chi2);
    mHistFitPts->Fill((float)fitPts);
    mHistMaxPts->Fill((float)maxPts);
    if (maxPts) mHistFitOverMax->Fill((float)fitPts/(float)maxPts);
    if (charge == 1) {
      float piPlus  = pFlowTrack->PidPiPlus();
      mHistPidPiPlus->Fill(piPlus);
      float proton  = pFlowTrack->PidProton();
      mHistPidProton->Fill(proton);
    } else if (charge == -1) {
      float piMinus = pFlowTrack->PidPiMinus();
      mHistPidPiMinus->Fill(piMinus);
    }

    // Yield3D, Yield2D, BinEta, BinPt
    mHistEtaPtPhi3D->Fill(eta, pt, phi);
    mHistYieldAll2D->Fill(eta, pt);
    mHistBinEta->Fill(eta, eta);
    mHistBinPt->Fill(pt, pt);

    // cos(n*phiLab)
    for (int j = 0; j < Flow::nHars; j++) {
      float order = (float)(j+1);
      float vIn   = cos((double)order * phi)/perCent;
      if (eta < 0 && (j+1) % 2 == 1) vIn *= -1;
      mHistCosPhi->Fill(order, vIn);
    }

    // For Eta symmetry
    if (eta > 0.) { etaSymPosN++; }
    else { etaSymNegN++; }

    // For PID multiplicites
    Char_t pid[10];
    strcpy(pid, pFlowTrack->Pid());
    if (strcmp(pid, "pi+") == 0)    piPlusN++;
    if (strcmp(pid, "pi-") == 0)    piMinusN++;
    if (strcmp(pid, "proton") == 0) protonN++;

    for (int k = 0; k < Flow::nSels; k++) {
      pFlowSelect->SetSelection(k);
      for (int j = 0; j < Flow::nHars; j++) {
	pFlowSelect->SetHarmonic(j);
	double order  = (double)(j+1);
	float psi_i = mQ[k][j].Phi() / order;
	//if (psi_i < 0.) psi_i += twopi / order;
	if (pFlowSelect->Select(pFlowTrack)) {
	  // Remove autocorrelations
	  TVector2 Q_i;
	  double phiWgt = pFlowEvent->PhiWeight(phi, k, j);
	  histFull[k].histFullHar[j].mHistPhiFlat->Fill(phi, phiWgt);
	  if (eta < 0 && (j+1) % 2 == 1) phiWgt *= -1.;
	  Q_i.Set(phiWgt * cos(phi * order), phiWgt * sin(phi * order));
	  TVector2 mQ_i = mQ[k][j] - Q_i;
	  psi_i = mQ_i.Phi() / order;
	  if (psi_i < 0.) psi_i += twopi / order;
	  // Fill histograms with selections
	  histFull[k].histFullHar[j].mHistPhi->Fill(phi);
	  histFull[k].histFullHar[j].mHistYield2D->Fill(eta, pt);
	}

       	// Caculate v for all particles selected for correlation analysis
	if (pFlowSelect->SelectPart(pFlowTrack)) {
	  corrMultN++;
	  float v = cos(order * (phi - psi_i))/perCent;
	  float vFlip = v;
	  if (eta < 0 && (j+1) % 2 == 1) vFlip *= -1;
	  histFull[k].histFullHar[j].mHistSum_v2D-> Fill(eta, pt, v);
	  histFull[k].histFullHar[j].mHist_vObsEta->Fill(eta, v);
	  histFull[k].histFullHar[j].mHist_vObsPt-> Fill(pt, vFlip);
	  
	  // Correlation of Phi of all particles with Psi
	  float phi_i = phi;
	  if (eta < 0 && (j+1) % 2 == 1) {
	    phi_i += pi; // backward particle and odd harmonic
	    if (phi_i > twopi) phi_i -= twopi;
	  }
	  float dPhi = phi_i - psi_i;
	  if (dPhi < 0.) dPhi += twopi;
	  histFull[k].histFullHar[j].mHistPhiCorr->
	    Fill(fmod((double)dPhi, twopi / order));
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

  // Multiplicity of particles correlated with the event planes
  corrMultN = corrMultN / (float)(Flow::nHars * Flow::nSels);
  mHistCorrMult->Fill(corrMultN);

}

//-----------------------------------------------------------------------

static Double_t qDist(double* q, double* par) {
  // Calculates the q distribution given the parameters v, mult, area

  double expo = par[1]*par[0]*par[0]*perCent*perCent + q[0]*q[0];
  Double_t dNdq = par[2] * (2. * q[0] * exp(-expo) * 
    (double)besi0_(2.*q[0]*par[0]*perCent*sqrt(par[1])));

  return dNdq;
}

//-----------------------------------------------------------------------

static Double_t resEventPlane(double chi) {
  // Calculates the event plane resolution as a function of chi

  double con = 0.626657;                   // sqrt(pi/2)/2
  double arg = chi * chi / 4.;
  float farg = (float)arg;

  Double_t res = con * chi * exp(-arg) * (double)(besi0_(farg) + besi1_(farg)); 

  return res;
}

//-----------------------------------------------------------------------

static Double_t chi(double res) {
  // Calculates chi from the event plane resolution

  double chi = 2.0;
  double delta = 1.0;

  for (int i = 0; i < 15; i++) {
    chi = (resEventPlane(chi) < res) ? chi + delta : chi - delta;
    delta = delta / 2.;
  }

  return chi;
}

//-----------------------------------------------------------------------

Int_t StFlowAnalysisMaker::Finish() {
  // Calculates resolution and mean flow values
  // Fits q distribution and outputs phiWgt values

  TString* histTitle;

  // PhiWgt histogram collection
  TOrdCollection* phiWgtHistNames = new TOrdCollection(Flow::nSels*Flow::nHars);

  // Yield with zero error
  TH2D* histYield2DZero = new TH2D("ZeroError", "ZeroError", nEtaBins, 
    etaMin, etaMax, nPtBins, ptMin, ptMax);
  histYield2DZero->Sumw2();
  mHistYieldAll2D->Copy(*histYield2DZero);
  double zero[nEtaBins+2][nPtBins+2] = {{0.}};
  histYield2DZero->SetError(&zero[0][0]);

  // Calculate resolution from sqrt(mHistCos)
  double cosPair[Flow::nSels][Flow::nHars];
  double cosPairErr[Flow::nSels][Flow::nHars];
  for (int k = 0; k < Flow::nSels; k++) {
    char countSels[2];
    sprintf(countSels,"%d",k+1);
    for (int j = 0; j < Flow::nHars; j++) {
      char countHars[2];
      sprintf(countHars,"%d",j+1);
      cosPair[k][j]    = histFull[k].mHistCos->GetBinContent(j+1);
      cosPairErr[k][j] = histFull[k].mHistCos->GetBinError(j+1);
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
      histFull[k].mHistRes->SetBinContent(j+1, mRes[k][j]);
      histFull[k].mHistRes->SetBinError(j+1, mResErr[k][j]);

      // Calculate vObs = Sum_v / Yield
      histFull[k].histFullHar[j].mHist_vObs2D->
 	Divide(histFull[k].histFullHar[j].mHistSum_v2D, histYield2DZero,1.,1.);

      // Clone the _vObs histograms to make the _v histograms
      histFull[k].histFullHar[j].mHist_v2D = 
	(TH2F*)histFull[k].histFullHar[j].mHist_vObs2D->Clone();
      histTitle = new TString("Flow_v2D_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHist_v2D->SetName(histTitle->Data());
      histFull[k].histFullHar[j].mHist_v2D->SetTitle(histTitle->Data());
      delete histTitle;
      AddHist(histFull[k].histFullHar[j].mHist_v2D);

      histFull[k].histFullHar[j].mHist_vEta =
	(TProfile*)histFull[k].histFullHar[j].mHist_vObsEta->Clone();
      histTitle = new TString("Flow_vEta_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHist_vEta->SetName(histTitle->Data());
      histFull[k].histFullHar[j].mHist_vEta->SetTitle(histTitle->Data());
      delete histTitle;
      AddHist(histFull[k].histFullHar[j].mHist_vEta);

      histFull[k].histFullHar[j].mHist_vPt =
	(TProfile*)histFull[k].histFullHar[j].mHist_vObsPt->Clone();
      TString* histTitle = new TString("Flow_vPt_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHist_vPt->SetName(histTitle->Data());
      histFull[k].histFullHar[j].mHist_vPt->SetTitle(histTitle->Data());
      delete histTitle;
      AddHist(histFull[k].histFullHar[j].mHist_vPt);

      // Calulate v = vObs / Resolution
      // The systematic error of the resolution is not folded in.
      cout << "# Resolution= " << mRes[k][j] << " +/- " << mResErr[k][j] << endl;

      if (mRes[k][j] != 0.) {
	histFull[k].histFullHar[j].mHist_v2D-> Scale(1. / mRes[k][j]);
	histFull[k].histFullHar[j].mHist_vEta->Scale(1. / mRes[k][j]);
	histFull[k].histFullHar[j].mHist_vPt-> Scale(1. / mRes[k][j]);
      } else {
	cout << "##### Resolution of the " << j+1 << "th harmonic was zero."
	     << endl;
      }

      // Fit q distribution
      float area = histFull[k].histFullHar[j].mHist_q->
	Integral() * qMax / (float)n_qBins; 
      float mult = histFull[k].histFullHar[j].mHistMult->GetMean();
      TF1* func_q = new TF1("qDist", qDist, 0., qMax, 3); // fit q dist
      func_q->SetParNames("v", "mult", "area");
      float qMean = histFull[k].histFullHar[j].mHist_q->GetMean();
      //float v2N = (qMean > 1.) ? qMean - 1. : 0.;
      //float vGuess = 100. * sqrt(v2N / mult);
      float vGuess = (qMean > 2.) ? 8. : 1.;
      func_q->SetParameters(vGuess, mult, area); // initial values
      func_q->SetParLimits(1, 1, 1);             // mult is fixed
      func_q->SetParLimits(2, 1, 1);             // area is fixed
      histFull[k].histFullHar[j].mHist_q->Fit("qDist", "0");
      delete func_q;

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

  // Note the selection object used
  cout << "########################################################" << endl;
  cout << "##### The selection number was " << pFlowSelect->Number() << endl;
  cout << "##### Centrality was " << pFlowSelect->Centrality() << endl;
  cout << "##### Particles used for the event plane were " << 
    pFlowSelect->Pid() << endl;
  cout << "##### Particles correlated with the event plane were " << 
    pFlowSelect->PidPart() << endl;
  cout << "########################################################" << endl;
  delete pFlowSelect;

  return StMaker::Finish();
}
