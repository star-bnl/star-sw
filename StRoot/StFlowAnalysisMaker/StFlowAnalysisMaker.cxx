////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowAnalysisMaker.cxx,v 1.13 2000/01/13 21:50:22 posk Exp $
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
#include "StFlowAnalysisMaker.hh"
#include "../StFlowMaker/StFlowMaker.hh"
#include "../StFlowMaker/StFlowEvent.hh"
#include "../StFlowTagMaker/StFlowTagMaker.hh"
#include "../StFlowMaker/StFlowConstants.hh"
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

const Float_t StFlowAnalysisMaker::etaMin    =   -2.;
const Float_t StFlowAnalysisMaker::etaMax    =    2.;
const Float_t StFlowAnalysisMaker::ptMin     =    0.;
const Float_t StFlowAnalysisMaker::ptMax     =    2.;
const Float_t StFlowAnalysisMaker::qMax      =    2.;

enum { nEtaBins    = 20,
       nPtBins     = 10,
       n_qBins     = 50,};
// nPhiBins is in StFlowConstants.h

//-----------------------------------------------------------------------

StFlowAnalysisMaker::StFlowAnalysisMaker(const Char_t* name): StMaker(name),
  MakerName(name) {
}

//-----------------------------------------------------------------------

StFlowAnalysisMaker::~StFlowAnalysisMaker() {
}

//-----------------------------------------------------------------------

Int_t StFlowAnalysisMaker::Make() {
  // Make histograms

  // Get a pointer to the flow tags
  StFlowTagMaker* pFlowTagMaker = 0;
  pFlowTag = 0;
  pFlowTagMaker = (StFlowTagMaker*)GetMaker("FlowTag");
  if (pFlowTagMaker) pFlowTag = pFlowTagMaker->TagPointer();

  // Get a pointer to StFlowEvent
  StFlowMaker* pFlowMaker = 0;
  pFlowMaker = (StFlowMaker*)GetMaker("Flow");
  if (pFlowMaker) pFlowEvent = pFlowMaker->FlowEventPointer();

  // Event quantities
  if (pFlowTag) {
    fillFromTags();                        // get event quantities
    fillEventHistograms();                 // fill from Flow Tags
  } else if (pFlowEvent) {
    cout << "$$$$$ null FlowTag pointer" << endl;
    fillFromFlowEvent();                   // get event quantities
    fillEventHistograms();                 // fill from FlowEvent
  } else {
    cout << "$$$$$ null FlowEvent and FlowTag pointers" << endl;
    return kStOK;
  }

  // Particle quantities
  if (pFlowEvent) fillParticleHistograms(); // fill particle histograms
   
  PrintInfo();

  // Clean up
  if (pFlowEvent) {
    delete pFlowEvent;    // it deletes pTrackCollection;
  }

  return kStOK;
}

//-----------------------------------------------------------------------

void StFlowAnalysisMaker::PrintInfo() {
  cout << "*************************************************************" << endl;
  cout << "$Id: StFlowAnalysisMaker.cxx,v 1.13 2000/01/13 21:50:22 posk Exp $"
       << endl;
  cout << "*************************************************************" << endl;
  if (Debug()) StMaker::PrintInfo();
}

//-----------------------------------------------------------------------

Int_t StFlowAnalysisMaker::Init() {
  // Book histograms

  static const int& nHars    = Flow::nHars;
  static const int& nSels    = Flow::nSels;
  static const int& nSubs    = Flow::nSubs;
  static const int& nPhiBins = Flow::nPhiBins;

  const float chargeMin       =   -3.;
  const float chargeMax       =    3.; 
  const float dcaMin          =    0.;
  const float dcaMax          =    1.; 
  const float chi2Min         =    0.;
  const float chi2Max         =    3.; 
  const float fitPtsMin       =    0.;
  const float fitPtsMax       =  100.; 
  const float maxPtsMin       =    0.;
  const float maxPtsMax       =  100.; 
  const float fitOverMaxMin   =    0.;
  const float fitOverMaxMax   =    3.; 
  const float origMultMin     =    0.;
  const float origMultMax     = 2500.; 
  const float totalMultMin    =    0.;
  const float totalMultMax    = 1500.; 
  const float multOverOrigMin =    0.;
  const float multOverOrigMax =    1.; 
  const float vertexZMin      = -100.;
  const float vertexZMax      =  100.; 
  const float vertexXYMin     =  -1.;
  const float vertexXYMax     =   1.; 
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

  enum { nChargeBins       = 50,
	 nDcaBins          = 50,
	 nChi2Bins         = 50,
	 nFitPtsBins       = 50,
	 nMaxPtsBins       = 50,
	 nFitOverMaxBins   = 50,
	 nOrigMultBins     = 50,
	 nTotalMultBins    = 50,
	 nMultOverOrigBins = 50,
	 nVertexZBins      = 50,
	 nVertexXYBins     = 50,
	 nEtaSymBins       = 50,
	 nPhi3DBins        = 18,
	 nPsiBins          = 36,
	 nMultBins         = 50,
	 nMeanPtBins       = 50 };
  
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
    char countSels[2];
    sprintf(countSels,"%d",k+1);
    // for sub-event pairs

    // cos(n*delta_Psi)
    histTitle = new TString("Flow_prof_Cos_Sel");
    histTitle->Append(*countSels);
    histFull[k].mHistCos = new TProfile(histTitle->Data(), histTitle->Data(),
      nHars, 0.5, (float)(nHars) + 0.5, -1., 1., "");
    histFull[k].mHistCos->SetXTitle("Harmonic");
    histFull[k].mHistCos->SetYTitle("cos(n*delta_Psi)");
    delete histTitle;
    
    // resolution
    histTitle = new TString("Flow_Res_Sel");
    histTitle->Append(*countSels);
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
        histTitle->Data(), nPhiBins, phiMin, phiMax);
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
        histTitle->Data(), nPhiBins, phiMin, phiMax);
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
        histTitle->Data(), nPhiBins, phiMin, phiMax);
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
        histTitle->Data(), nPhiBins, phiMin, phiMax / order);
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

      // Flow
      histTitle = new TString("Flow_v2D_Sel");
      histTitle->Append(*countSels);
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

void StFlowAnalysisMaker::fillFromTags() {
//   Get the flow tags and calculate the full event quantities

  static const int& nHars = Flow::nHars;
  static const int& nSels = Flow::nSels;
  static const int& nSubs = Flow::nSubs;

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
      m_q[k][j]     = (mMult[k][j] > 0) ? mQ[k][j].Mod()/sqrt((float)mMult[k][j])
	: 0.;
      mMeanPt[k][j] = (mMeanPtSub[2*k][j] + mMeanPtSub[2*k+1][j])/2.;
    }
  }

}

//-----------------------------------------------------------------------

void StFlowAnalysisMaker::fillFromFlowEvent() {
  // Get event quantities from StFlowEvent

  static const int& nHars = Flow::nHars;
  static const int& nSels = Flow::nSels;
  static const int& nSubs = Flow::nSubs;
  
   for (int k = 0; k < nSels; k++) {
    for (int j = 0; j < nHars; j++) {
      for (int n = 0; n < nSubs; n++) {
	int i = 2*k + n;
	// sub-event quantities
	mPsiSub[i][j] = pFlowEvent->Psi(j, k, n);
      }

      // full event quantities
      mQ[k][j]      = pFlowEvent->Q(j, k);
      mPsi[k][j]    = pFlowEvent->Psi(j, k);
      m_q[k][j]     = pFlowEvent->q(j, k);
      mMult[k][j]   = pFlowEvent->Mult(j, k);
      mMeanPt[k][j] = pFlowEvent->MeanPt(j, k);
    }
  }

}

//-----------------------------------------------------------------------

void StFlowAnalysisMaker::fillEventHistograms() {
  // Fill histograms with event quantities

  static const int& nHars = Flow::nHars;
  static const int& nSels = Flow::nSels;
  static const int& nSubs = Flow::nSubs;

  // no selections: OrigMult, Mult, MultOverOrig, VertexZ, VertexXY
  int origMult = pFlowEvent->OrigMult();
  mHistOrigMult->Fill((float)origMult);
  int totalMult = pFlowEvent->TrackCollection()->size();
  mHistMult->Fill((float)totalMult);
  if (origMult) mHistMultOverOrig->Fill((float)totalMult / (float)origMult);

  StThreeVectorF vertex = pFlowEvent->VertexPos();
  mHistVertexZ->Fill(vertex.z());
  mHistVertexXY2D->Fill(vertex.x(), vertex.y());

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
      histFull[k].histFullHar[j].mHistMult->Fill((float)mMult[k][j]);
      histFull[k].histFullHar[j].mHist_q->Fill(m_q[k][j]);
      if (mMult[k][j] > 0) {
	histFull[k].histFullHar[j].mHistMeanPt->Fill(mMeanPt[k][j]);
      }
    }
  }

}

//-----------------------------------------------------------------------

void StFlowAnalysisMaker::fillParticleHistograms() {
  // Fill histograms from the particles

  static const int& nHars = Flow::nHars;
  static const int& nSels = Flow::nSels;

  float etaSymPosN = 0.;
  float etaSymNegN = 0.;

  // Initialize Iterator
  StFlowTrackCollection* pFlowTracks = pFlowEvent->TrackCollection();
  StFlowTrackIterator itr;
  
  for (itr = pFlowTracks->begin(); itr != pFlowTracks->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    float phi       = pFlowTrack->Phi();
    if (phi < 0.) phi += twopi;
    float eta       = pFlowTrack->Eta();
    float pt        = pFlowTrack->Pt();
    int   charge    = pFlowTrack->Charge();
    float dca       = pFlowTrack->ImpactPar();
    float chi2      = pFlowTrack->Chi2();
    int fitPts      = pFlowTrack->FitPts();
    int maxPts      = pFlowTrack->MaxPts();

    // no selections: Charge, Dca, Chi2, FitPts, MaxPts, FitOverMax
    mHistCharge->Fill((float)charge);
    mHistDca->Fill(dca);
    mHistChi2->Fill(chi2);
    mHistFitPts->Fill((float)fitPts);
    mHistMaxPts->Fill((float)maxPts);
    if (maxPts) mHistFitOverMax->Fill((float)fitPts/(float)maxPts);

    // Yield3D, Yield2D, BinEta, BinPt
    mHistEtaPtPhi3D->Fill(eta, pt, phi);
    mHistYieldAll2D->Fill(eta, pt);
    mHistBinEta->Fill(eta, eta);
    mHistBinPt->Fill(pt, pt);

    //For Eta symmetry
    if (eta > 0.) { etaSymPosN++; }
    else { etaSymNegN++; }

    for (int k = 0; k < nSels; k++) {
      for (int j = 0; j < nHars; j++) {
	float order  = (float)(j+1);
	float psi_i = mQ[k][j].Phi() / order;
	//if (psi_i < 0.) psi_i += twopi / order;
	if (pFlowTrack->Select(j, k)) {
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
 	//cout << "k= " << k << " j= " << j << " Psi= " <<
	//  mQ[k][j].Phi() / order << "\t Psi_i= " << psi_i << endl;

       	// Caculate v for all particles
	float v = cos(order * (phi - psi_i))/perCent;
	histFull[k].histFullHar[j].mHistSum_v2D->Fill(eta, pt, v);

	// Correlation of Phi of all particles with Psi
	float phi_i = phi;
	if (eta < 0 && (j+1) % 2 == 1) {
	  phi_i += pi; // backward particle and odd harmonic
	  if (phi_i > twopi) phi_i -= twopi;
	}
	float dPhi = phi_i - psi_i;
	if (dPhi < 0.) dPhi += twopi;
	histFull[k].histFullHar[j].mHistPhiCorr->
	  Fill(fmod(dPhi, twopi / order));
      }
    }  
  }

  // EtaSym
  float etaSym = (etaSymPosN - etaSymNegN) / (etaSymPosN + etaSymNegN);
  mHistEtaSym->Fill(etaSym);

}

//-----------------------------------------------------------------------

static Double_t qDist(double* q, double* par) {
  // Calculates the q distribution given the parameters v, mult, area

  double expo = par[1]*par[0]*par[0]*perCent*perCent + q[0]*q[0];
  Double_t dNdq = par[2] * (2. * q[0] * exp(-expo) * 
    besi0_(2*q[0]*par[0]*perCent*sqrt(par[1])));

  return dNdq;
}

//-----------------------------------------------------------------------

Int_t StFlowAnalysisMaker::Finish() {
  // Calculates resolution and mean flow values
  // Fits q distribution and outputs phiWgt values

  static const int& nHars    = Flow::nHars;
  static const int& nSels    = Flow::nSels;
  static const int& nPhiBins = Flow::nPhiBins;

  // PhiWgt histogram collection
  TOrdCollection* phiWgtHistNames = new TOrdCollection(nSels*nHars);

  // Yield with zero error
  TH2D* histYield2DZero = new TH2D("ZeroError", "ZeroError", nEtaBins, 
    etaMin, etaMax, nPtBins, ptMin, ptMax);
  histYield2DZero->Sumw2();
  mHistYieldAll2D->Copy(*histYield2DZero);
  double zero[nEtaBins+2][nPtBins+2] = {{0.}};
  histYield2DZero->SetError(&zero[0][0]);

  // Calculate resolution = sqrt(2)*sqrt(mHistCos)
  float cosPair[Flow::nSels][Flow::nHars];
  float cosPairErr[Flow::nSels][Flow::nHars];
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
	cout << "##### Resolution of the " << j+1 << "th harmonic was zero."
	     << endl;
      }

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
      double mean = histFull[k].histFullHar[j].mHistPhi->Integral() 
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

  return StMaker::Finish();
}
