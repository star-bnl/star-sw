////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowAnalysisMaker.cxx,v 1.42 2000/09/16 22:23:04 snelling Exp $
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
// Revision 1.42  2000/09/16 22:23:04  snelling
// Auto magically switch to rapidity when identified particles are used
//
// Revision 1.41  2000/09/15 22:52:53  posk
// Added Pt weighting for event plane calculation.
//
// Revision 1.40  2000/09/12 01:31:00  snelling
// Added pid histograms for e- e+ and dbar
//
// Revision 1.39  2000/09/07 17:02:45  snelling
// Made the hist file standard root compatible
//
// Revision 1.38  2000/09/05 16:12:12  snelling
// Added the new particles to the pid histogram
//
// Revision 1.37  2000/08/31 18:50:29  posk
// Added plotCen.C to plot from a series of files with different centralities.
//
// Revision 1.36  2000/08/12 20:20:13  posk
// More centrality bins.
//
// Revision 1.35  2000/08/09 21:38:59  snelling
// Added monitor histograms
//
// Revision 1.34  2000/08/01 21:51:18  posk
// Added doubly integrated v.
//
// Revision 1.33  2000/07/12 17:49:37  posk
// Changed EtaSym plots.
//
// Revision 1.32  2000/06/30 14:51:18  posk
// Using MessageMgr. Added graph for Eta Symmetry vs. Vertex Z.
//
// Revision 1.31  2000/06/01 18:29:56  posk
// When resolution=0 reset histograms.
//
// Revision 1.30  2000/05/26 21:25:20  posk
// Use TProfile2D class and profile projection methods.
// Correction needed for >2 subevents.
//
// Revision 1.28  2000/05/03 16:38:33  posk
// Compatable with ROOT 2.24/02.
//
// Revision 1.27  2000/04/13 22:34:13  posk
// Resolution correction is now made.
//
// Revision 1.26  2000/03/28 23:25:36  posk
// Allow multiple instances.
//
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
// Revision 1.15  2000/01/14 01:35:52  snelling
// changed include path ../FlowMaker/ to FlowMaker/
//
// Revision 1.14  2000/01/14 01:13:34  snelling
// modified spt (sum pt) to mpt (mean pt) because FlowTag changed
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
#include "StFlowMaker/StFlowCutTrack.h"
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
#define PR(x) cout << "##### FlowAnalysis: " << (#x) << " = " << (x) << endl;
extern "C" float besi0_(const float&);
extern "C" float besi1_(const float&);

ClassImp(StFlowAnalysisMaker)

const Float_t StFlowAnalysisMaker::qMax   =  3.5;
enum { n_qBins  = 50 };
// nPhiBins is in StFlowConstants.h

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
  // Make histograms

  // Get a pointer to the flow tags
  StFlowTagMaker* pFlowTagMaker = NULL;
  pFlowTag = NULL;
  TString* makerName = new TString("FlowTag");
  makerName->Append(pFlowSelect->Number());
  pFlowTagMaker = (StFlowTagMaker*)GetMaker(makerName->Data());
  delete makerName;
  if (pFlowTagMaker) pFlowTag = pFlowTagMaker->TagPointer();

  // Get a pointer to StFlowEvent
  StFlowMaker* pFlowMaker = NULL;
  makerName = new TString("Flow");
  makerName->Append(pFlowSelect->Number());
  pFlowMaker = (StFlowMaker*)GetMaker(makerName->Data());
  delete makerName;
  if (pFlowMaker) pFlowEvent = pFlowMaker->FlowEventPointer();
  if (pFlowEvent && pFlowSelect->Select(pFlowEvent)) {     // event selected

    // Event quantities
    if (pFlowTag) {
      FillFromTags();                        // get event quantities
      FillEventHistograms();                 // fill from Flow Tags
    } else if (pFlowEvent) {
      gMessMgr->Info("##### FlowAnalysis: FlowTag pointer null");
      FillFromFlowEvent();                   // get event quantities
      FillEventHistograms();                 // fill from FlowEvent
    } else {
      gMessMgr->Info("##### FlowAnalysis: FlowEvent and FlowTag pointers null");
      return kStOK;
    }
    // Particle quantities
    if (pFlowEvent) FillParticleHistograms(); // fill particle histograms
    
    if (Debug()) StMaker::PrintInfo();
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
  xLabel = "Pseudorapidity";
  if (strlen(pFlowSelect->PidPart()) != 0) { xLabel = "Rapidity"; }

  const float etaMin          =  -1.5;
  const float etaMax          =   1.5;
  const float ptMin           =    0.;
  const float ptMax           =    2.;
  const float chargeMin       =  -2.5;
  const float chargeMax       =   2.5; 
  const float dcaMin          =    0.;
  const float dcaMax          =   1.2; 
  const float chi2Min         =    0.;
  const float chi2Max         =    5.; 
  const float fitPtsMin       =    0.;
  const float fitPtsMax       =   60.; 
  const float maxPtsMin       =    0.;
  const float maxPtsMax       =   60.; 
  const float fitOverMaxMin   =    0.;
  const float fitOverMaxMax   =   1.2; 
  const float origMultMin     =    0.;
  const float origMultMax     = 2000.; 
  const float MultEtaMin      =    0.;
  const float MultEtaMax      = 1000.; 
  const float totalMultMin    =    0.;
  const float totalMultMax    = 2000.; 
  const float corrMultMin     =    0.;
  const float corrMultMax     = 2000.; 
  const float multOverOrigMin =    0.;
  const float multOverOrigMax =    1.; 
  const float vertexZMin      = -150.;
  const float vertexZMax      =  150.; 
  const float vertexXYMin     =   -1.;
  const float vertexXYMax     =    1.; 
  const float etaSymZMin      =  -0.5; 
  const float etaSymZMax      =   0.5; 
  const float etaSymMin       =   -5.; 
  const float etaSymMax       =    5.; 
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
  const float centMin         =  -0.5;
  const float centMax         =   9.5;

  enum { nEtaBins          = 30,
	 nPtBins           = 40,
	 nChargeBins       = 50,
	 nDcaBins          = 60,
	 nChi2Bins         = 50,
	 nFitPtsBins       = 60,
	 nMaxPtsBins       = 60,
	 nFitOverMaxBins   = 40,
	 nOrigMultBins     = 40,
	 nMultEtaBins      = 50,
	 nTotalMultBins    = 40,
	 nMultOverOrigBins = 50,
	 nCorrMultBins     = 40,
	 nVertexZBins      = 60,
	 nVertexXYBins     = 50,
	 nEtaSymBins       = 50,
	 nPhi3DBins        = 18,
	 nPsiBins          = 36,
	 nMultBins         = 40,
	 nMeanPtBins       = 50,
	 nPidBins          = 50,
         nCentBins         = 10 };
  
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

  // MultEta
  mHistMultEta = new TH1F("Flow_MultEta", "Flow_MultEta",
      nMultEtaBins, MultEtaMin, MultEtaMax);
  mHistMultEta->SetXTitle("Mult |eta| < 0.75");
  mHistMultEta->SetYTitle("Counts");
    
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
    
  // EtaSym vs. Vertex Z
  mHistEtaSymVerZ2D = new TH2F("Flow_EtaSymVerZ2D", "Flow_EtaSymVerZ2D",
    nVertexZBins, vertexZMin, vertexZMax, nEtaSymBins, etaSymZMin, etaSymZMax);
  mHistEtaSymVerZ2D->SetXTitle("Vertex Z (cm)");
  mHistEtaSymVerZ2D->SetYTitle("Eta Symmetry");

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
  mHistYieldAll2D->SetXTitle((char*)xLabel.Data());
  mHistYieldAll2D->SetYTitle("Pt (GeV)");

  // Mean Eta in each bin
  mHistBinEta = new TProfile("Flow_Bin_Eta", "Flow_Bin_Eta",
    nEtaBins, etaMin, etaMax, etaMin, etaMax, "");
  mHistBinEta->SetXTitle((char*)xLabel.Data());
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

  // PID anti proton
  mHistPidAntiProton = new TH1F("Flow_PidAntiProton", "Flow_PidAntiProton",
				nPidBins, pidMin, pidMax);
  mHistPidAntiProton->SetXTitle("(PID - Mean) / Resolution");
  mHistPidAntiProton->SetYTitle("Counts");

  // PID Kplus
  mHistPidKplus = new TH1F("Flow_PidKplus", "Flow_PidKplus",
			   nPidBins, pidMin, pidMax);
  mHistPidKplus->SetXTitle("(PID - Mean) / Resolution");
  mHistPidKplus->SetYTitle("Counts");

  // PID Kminus
  mHistPidKminus = new TH1F("Flow_PidKminus", "Flow_PidKminus",
			    nPidBins, pidMin, pidMax);
  mHistPidKminus->SetXTitle("(PID - Mean) / Resolution");
  mHistPidKminus->SetYTitle("Counts");

  // PID deuteron
  mHistPidDeuteron = new TH1F("Flow_PidDeuteron", "Flow_PidDeuteron",
			      nPidBins, pidMin, pidMax);
  mHistPidDeuteron->SetXTitle("(PID - Mean) / Resolution");
  mHistPidDeuteron->SetYTitle("Counts");

  // PID anti deuteron
  mHistPidAntiDeuteron = new TH1F("Flow_PidAntiDeuteron", 
				  "Flow_PidAntiDeuteron",
				  nPidBins, pidMin, pidMax);
  mHistPidAntiDeuteron->SetXTitle("(PID - Mean) / Resolution");
  mHistPidAntiDeuteron->SetYTitle("Counts");

  // PID electron
  mHistPidElectron = new TH1F("Flow_PidElectron", "Flow_PidElectron",
			      nPidBins, pidMin, pidMax);
  mHistPidElectron->SetXTitle("(PID - Mean) / Resolution");
  mHistPidElectron->SetYTitle("Counts");

  // PID positron
  mHistPidPositron = new TH1F("Flow_PidPositron", "Flow_PidPositron",
			      nPidBins, pidMin, pidMax);
  mHistPidPositron->SetXTitle("(PID - Mean) / Resolution");
  mHistPidPositron->SetYTitle("Counts");

  // PID pi+ selected
  mHistPidPiPlusSel = new TH1F("Flow_PidPiPlusSel", 
			       "Flow_PidPiPlusSel",
			       nPidBins, pidMin, pidMax);
  mHistPidPiPlusSel->SetXTitle("(PID - Mean) / Resolution");
  mHistPidPiPlusSel->SetYTitle("Counts");
    
  // PID pi- selected
  mHistPidPiMinusSel = new TH1F("Flow_PidPiMinusSel", 
				"Flow_PidPiMinusSel",
				nPidBins, pidMin, pidMax);
  mHistPidPiMinusSel->SetXTitle("(PID - Mean) / Resolution");
  mHistPidPiMinusSel->SetYTitle("Counts");
    
  // PID proton selected
  mHistPidProtonSel = new TH1F("Flow_PidProtonSel", 
			       "Flow_PidProtonSel",
			       nPidBins, pidMin, pidMax);
  mHistPidProtonSel->SetXTitle("(PID - Mean) / Resolution");
  mHistPidProtonSel->SetYTitle("Counts");

  // PID anti proton selected
  mHistPidAntiProtonSel = new TH1F("Flow_PidAntiProtonSel", 
				   "Flow_PidAntiProtonSel",
				   nPidBins, pidMin, pidMax);
  mHistPidAntiProtonSel->SetXTitle("(PID - Mean) / Resolution");
  mHistPidAntiProtonSel->SetYTitle("Counts");

  // PID Kplus selected
  mHistPidKplusSel = new TH1F("Flow_PidKplusSel", 
			      "Flow_PidKplusSel",
			      nPidBins, pidMin, pidMax);
  mHistPidKplusSel->SetXTitle("(PID - Mean) / Resolution");
  mHistPidKplusSel->SetYTitle("Counts");

  // PID Kminus selected
  mHistPidKminusSel = new TH1F("Flow_PidKminusSel", 
			       "Flow_PidKminusSel",
			       nPidBins, pidMin, pidMax);
  mHistPidKminusSel->SetXTitle("(PID - Mean) / Resolution");
  mHistPidKminusSel->SetYTitle("Counts");

  // PID deuteron selected
  mHistPidDeuteronSel = new TH1F("Flow_PidDeuteronSel", 
				 "Flow_PidDeuteronSel",
				 nPidBins, pidMin, pidMax);
  mHistPidDeuteronSel->SetXTitle("(PID - Mean) / Resolution");
  mHistPidDeuteronSel->SetYTitle("Counts");

  // PID anti deuteron selected
  mHistPidAntiDeuteronSel = new TH1F("Flow_PidAntiDeuteronSel", 
				     "Flow_PidAntiDeuteronSel",
				     nPidBins, pidMin, pidMax);
  mHistPidAntiDeuteronSel->SetXTitle("(PID - Mean) / Resolution");
  mHistPidAntiDeuteronSel->SetYTitle("Counts");

  // PID electron selected
  mHistPidElectronSel = new TH1F("Flow_PidElectronSel", 
				 "Flow_PidElectronSel",
				 nPidBins, pidMin, pidMax);
  mHistPidElectronSel->SetXTitle("(PID - Mean) / Resolution");
  mHistPidElectronSel->SetYTitle("Counts");

  // PID positron selected
  mHistPidPositronSel = new TH1F("Flow_PidPositronSel", 
				 "Flow_PidPositronSel",
				 nPidBins, pidMin, pidMax);
  mHistPidPositronSel->SetXTitle("(PID - Mean) / Resolution");
  mHistPidPositronSel->SetYTitle("Counts");

  // PID multiplicities selected
  mHistPidMult = new TProfile("Flow_PidMult", "Flow_PidMult",
			      11, 0.5, 11.5, 0., 10000., "");
  mHistPidMult->SetXTitle("All, Pi+, Pi-, Proton, Pbar, K+, K-, d, dbar, e-, e+");
  mHistPidMult->SetYTitle("Multiplicity");
    
  // Centrality
  mHistCent = new TH1F("Flow_Cent", "Flow_Cent",
		       nCentBins, centMin, centMax);
  mHistCent->SetXTitle("Centrality Bin");
  mHistCent->SetYTitle("Counts");
    
  // CTBversusZDC
  mHistCTBversusZDC = new TH2F("Flow_CTBversusZDC", "Flow_CTBversusZDC",
			       300, 0, 300,
			       250, 0, 25000);
  mHistCTBversusZDC->SetXTitle("ZDC sum");
  mHistCTBversusZDC->SetYTitle("CTB sum");

  // MeanDedx
  mHistMeanDedx = new TH2F("Flow_MeanDedx", "Flow_MeanDedx",
			   400, -5, 5,
			   1000, 0, 0.00005);
  mHistMeanDedx->SetXTitle("momentum/Z (GeV)");
  mHistMeanDedx->SetYTitle("mean dEdx");

  // MeanDedx PiPlus
  mHistMeanDedxPiPlus = new TH2F("Flow_MeanDedxPiPlus", 
				 "Flow_MeanDedxPiPlus",
				 400, -5, 5,
				 1000, 0, 0.00005);
  mHistMeanDedxPiPlus->SetXTitle("momentum/Z (GeV)");
  mHistMeanDedxPiPlus->SetYTitle("mean dEdx");

  // MeanDedxPiMinus
  mHistMeanDedxPiMinus = new TH2F("Flow_MeanDedxPiMinus", 
				  "Flow_MeanDedxPiMinus",
				  400, -5, 5,
				  1000, 0, 0.00005);
  mHistMeanDedxPiMinus->SetXTitle("momentum/Z (GeV)");
  mHistMeanDedxPiMinus->SetYTitle("mean dEdx");

  // MeanDedxProton
  mHistMeanDedxProton = new TH2F("Flow_MeanDedxProton", 
				 "Flow_MeanDedxProton",
				 400, -5, 5,
				 1000, 0, 0.00005);
  mHistMeanDedxProton->SetXTitle("momentum/Z (GeV)");
  mHistMeanDedxProton->SetYTitle("mean dEdx");

  // MeanDedxPbar
  mHistMeanDedxPbar = new TH2F("Flow_MeanDedxPbar", 
			       "Flow_MeanDedxPbar",
			       400, -5, 5,
			       1000, 0, 0.00005);
  mHistMeanDedxPbar->SetXTitle("momentum/Z (GeV)");
  mHistMeanDedxPbar->SetYTitle("mean dEdx");

  // MeanDedxKplus
  mHistMeanDedxKplus = new TH2F("Flow_MeanDedxKplus", 
				"Flow_MeanDedxKplus",
				400, -5, 5,
				1000, 0, 0.00005);
  mHistMeanDedxKplus->SetXTitle("momentum/Z (GeV)");
  mHistMeanDedxKplus->SetYTitle("mean dEdx");

  // MeanDedxKminus
  mHistMeanDedxKminus = new TH2F("Flow_MeanDedxKminus", "Flow_MeanDedxKminus",
				 400, -5, 5,
				 1000, 0, 0.00005);
  mHistMeanDedxKminus->SetXTitle("momentum/Z (GeV)");
  mHistMeanDedxKminus->SetYTitle("mean dEdx");

  // MeanDedxDeuteron
  mHistMeanDedxDeuteron = new TH2F("Flow_MeanDedxDeuteron", 
				   "Flow_MeanDedxDeuteron",
				   400, -5, 5,
				   1000, 0, 0.00005);
  mHistMeanDedxDeuteron->SetXTitle("momentum/Z (GeV)");
  mHistMeanDedxDeuteron->SetYTitle("mean dEdx");

  // MeanDedxAntiDeuteron
  mHistMeanDedxAntiDeuteron = new TH2F("Flow_MeanDedxAntiDeuteron", 
				       "Flow_MeanDedxAntiDeuteron",
				       400, -5, 5,
				       1000, 0, 0.00005);
  mHistMeanDedxAntiDeuteron->SetXTitle("momentum/Z (GeV)");
  mHistMeanDedxAntiDeuteron->SetYTitle("mean dEdx");

  // MeanDedxElectron
  mHistMeanDedxElectron = new TH2F("Flow_MeanDedxElectron", 
				   "Flow_MeanDedxElectron",
				   400, -5, 5,
				   1000, 0, 0.00005);
  mHistMeanDedxElectron->SetXTitle("momentum/Z (GeV)");
  mHistMeanDedxElectron->SetYTitle("mean dEdx");

  // MeanDedxPositron
  mHistMeanDedxPositron = new TH2F("Flow_MeanDedxPositron", 
				   "Flow_MeanDedxPositron",
				   400, -5, 5,
				   1000, 0, 0.00005);
  mHistMeanDedxPositron->SetXTitle("momentum/Z (GeV)");
  mHistMeanDedxPositron->SetYTitle("mean dEdx");

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

      // Yield
      histTitle = new TString("Flow_Yield2D_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistYield2D =	new TH2D(histTitle->Data(),
        histTitle->Data(), nEtaBins, etaMin, etaMax, nPtBins, ptMin, ptMax);
      histFull[k].histFullHar[j].mHistYield2D->Sumw2();
      histFull[k].histFullHar[j].mHistYield2D->SetXTitle((char*)xLabel.Data());
      histFull[k].histFullHar[j].mHistYield2D->SetYTitle("Pt (GeV)");
      delete histTitle;

      // Flow observed
      histTitle = new TString("Flow_vObs2D_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHist_vObs2D =	new TProfile2D(histTitle->Data(),
        histTitle->Data(), nEtaBins, etaMin, etaMax, nPtBins, ptMin, ptMaxPart,
							       -100., 100., "");
      histFull[k].histFullHar[j].mHist_vObs2D->SetXTitle((char*)xLabel.Data());
      histFull[k].histFullHar[j].mHist_vObs2D->SetYTitle("Pt (GeV)");
      delete histTitle;

      // Flow observed profiles
      histTitle = new TString("Flow_vObsEta_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHist_vObsEta = new TProfile(histTitle->Data(),
        histTitle->Data(), nEtaBins, etaMin, etaMax, -100., 100., "");
      histFull[k].histFullHar[j].mHist_vObsEta->SetXTitle((char*)xLabel.Data());
      histFull[k].histFullHar[j].mHist_vObsEta->SetYTitle("v (%)");
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

    }
  }

  gMessMgr->SetLimit("##### FlowAnalysis", 2);
  gMessMgr->Info("##### FlowAnalysis: $Id: StFlowAnalysisMaker.cxx,v 1.42 2000/09/16 22:23:04 snelling Exp $");

  return StMaker::Init();
}

//-----------------------------------------------------------------------

void StFlowAnalysisMaker::FillFromTags() {
//   Get the flow tags and calculate the full event quantities

  int nSels = 2, nHars = 6, nSubs = 2;

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
    mMeanPtSub[0][j] = pFlowTag->mpta[j];
    mMeanPtSub[1][j] = pFlowTag->mptb[j];
    mMeanPtSub[2][j] = pFlowTag->mptc[j];
    mMeanPtSub[3][j] = pFlowTag->mptd[j];

    // calculate Psi
    for (int i = 0; i < nSels * nSubs; i++) {
      mPsiSub[i][j] = mQSub[i][j].Phi() / order;
    }

    // full event quantities
    for (int k = 0; k < nSels; k++) {
      mQ[k][j]      = mQSub[nSels*k][j] + mQSub[nSels*k+1][j];
      mPsi[k][j]    = mQ[k][j].Phi() / order;
      mMult[k][j]   = mMultSub[nSels*k][j] + mMultSub[nSels*k+1][j];
      m_q[k][j]     = (mMult[k][j] > 0) ? mQ[k][j].Mod()/sqrt((double)mMult[k][j])
	: 0.;
      mMeanPt[k][j] = (mMeanPtSub[nSels*k][j] + mMeanPtSub[nSels*k+1][j])/2.;
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
	int i = Flow::nSels*k + n;
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
  mHistMultEta->Fill((float)pFlowEvent->MultEta());
  int cent = pFlowEvent->Centrality();
  mHistCent->Fill((float)cent);
  int totalMult = pFlowEvent->TrackCollection()->size();
  mHistMult->Fill((float)totalMult);
  if (origMult) mHistMultOverOrig->Fill((float)totalMult / (float)origMult);

  StThreeVectorF vertex = pFlowEvent->VertexPos();
  mHistVertexZ->Fill(vertex.z());
  mHistVertexXY2D->Fill(vertex.x(), vertex.y());

  mHistCTBversusZDC->Fill(pFlowEvent->ZDCe() + pFlowEvent->ZDCe(), 
			                       pFlowEvent->CTB());

  // sub-event Psi_Subs
  for (int i = 0; i < Flow::nSubs * Flow::nSels; i++) {
    for (int j = 0; j < Flow::nHars; j++) {
      histSub[i].histSubHar[j].mHistPsiSubs->Fill(mPsiSub[i][j]);
    }
  }

  // full event Psi, PsiSubCorr, PsiSubCorrDiff, cos, mult, q, <Pt>
  for (int k = 0; k < Flow::nSels; k++) {
    for (int j = 0; j < Flow::nHars; j++) {
      float order  = (float)(j+1);
      histFull[k].histFullHar[j].mHistPsi->Fill(mPsi[k][j]);
      float psiSubCorr = mPsiSub[Flow::nSels*k][j] - mPsiSub[Flow::nSels*k+1][j];
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
  float pbarN      = 0.;
  float kMinusN    = 0.;
  float kPlusN     = 0.;
  float deuteronN  = 0.;
  float dbarN      = 0.;
  float electronN  = 0.;
  float positronN  = 0.;

  // Initialize Iterator
  StFlowTrackCollection* pFlowTracks = pFlowEvent->TrackCollection();
  StFlowTrackIterator itr;
  
  for (itr = pFlowTracks->begin(); itr != pFlowTracks->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    float phi    = pFlowTrack->Phi();
    if (phi < 0.) phi += twopi;
    float eta    = pFlowTrack->Eta();
    if (strlen(pFlowSelect->PidPart()) != 0) { eta = pFlowTrack->Y(); }
    float pt     = pFlowTrack->Pt();
    int   charge = pFlowTrack->Charge();
    float dca    = pFlowTrack->Dca();
    float chi2   = pFlowTrack->Chi2();
    int   fitPts = pFlowTrack->FitPts();
    int   maxPts = pFlowTrack->MaxPts();
    Char_t pid[10];
    strcpy(pid, pFlowTrack->Pid());
    float totalp = pFlowTrack->P();

    // For PID multiplicites
    if (strcmp(pid, "pi+")    == 0)  piPlusN++;
    if (strcmp(pid, "pi-")    == 0)  piMinusN++;
    if (strcmp(pid, "proton") == 0)  protonN++;
    if (strcmp(pid, "pbar")   == 0)  pbarN++;
    if (strcmp(pid, "k+")     == 0)  kPlusN++;
    if (strcmp(pid, "k-")     == 0)  kMinusN++;
    if (strcmp(pid, "d")      == 0)  deuteronN++;
    if (strcmp(pid, "dbar")   == 0)  dbarN++;
    if (strcmp(pid, "e-")     == 0)  electronN++;
    if (strcmp(pid, "e+")     == 0)  positronN++;

    // no selections: Charge, Dca, Chi2, FitPts, MaxPts, FitOverMax, PID
    mHistCharge->Fill((float)charge);
    mHistDca->Fill(dca);
    mHistChi2->Fill(chi2);
    mHistFitPts->Fill((float)fitPts);
    mHistMaxPts->Fill((float)maxPts);
    if (maxPts) mHistFitOverMax->Fill((float)fitPts/(float)maxPts);

    mHistMeanDedx->Fill(totalp/charge,pFlowTrack->Dedx());

    if (charge == 1) {
      float piPlus = pFlowTrack->PidPiPlus();
      mHistPidPiPlus->Fill(piPlus);
      if (strcmp(pid, "pi+") == 0) {
	mHistMeanDedxPiPlus->Fill(totalp/charge,pFlowTrack->Dedx());
	mHistPidPiPlusSel->Fill(piPlus);
      }
      float kplus  = pFlowTrack->PidKaonPlus();
      mHistPidKplus->Fill(kplus);
      if (strcmp(pid, "k+") == 0) {
	mHistMeanDedxKplus->Fill(totalp/charge,pFlowTrack->Dedx());
	mHistPidKplusSel->Fill(kplus);
      }
      float proton  = pFlowTrack->PidProton();
      mHistPidProton->Fill(proton);
      if (strcmp(pid, "proton") == 0) {
	mHistMeanDedxProton->Fill(totalp/charge,pFlowTrack->Dedx());
	mHistPidProtonSel->Fill(proton);
      }
      float deuteron  = pFlowTrack->PidDeuteron();
      mHistPidDeuteron->Fill(deuteron);
      if (strcmp(pid, "d") == 0) {
	mHistMeanDedxDeuteron->Fill(totalp/charge,pFlowTrack->Dedx());
	mHistPidDeuteronSel->Fill(deuteron);
      }
      float positron  = pFlowTrack->PidPositron();
      mHistPidPositron->Fill(positron);
      if (strcmp(pid, "e+") == 0) {
	mHistMeanDedxPositron->Fill(totalp/charge,pFlowTrack->Dedx());
	mHistPidPositronSel->Fill(positron);
      }
    } else if (charge == -1) {
      float piMinus = pFlowTrack->PidPiMinus();
      mHistPidPiMinus->Fill(piMinus);
      if (strcmp(pid, "pi-") == 0) {
	mHistMeanDedxPiMinus->Fill(totalp/charge,pFlowTrack->Dedx());
	mHistPidPiMinusSel->Fill(piMinus);
      }
      float kminus  = pFlowTrack->PidKaonMinus();
      mHistPidKminus->Fill(kminus);
      if (strcmp(pid, "k-") == 0) {
	mHistMeanDedxKminus->Fill(totalp/charge,pFlowTrack->Dedx());
	mHistPidKminusSel->Fill(kminus);
      }
      float antiproton  = pFlowTrack->PidAntiProton();
      mHistPidAntiProton->Fill(antiproton);
      if (strcmp(pid, "pbar") == 0) {
	mHistMeanDedxPbar->Fill(totalp/charge,pFlowTrack->Dedx());
	mHistPidAntiProtonSel->Fill(antiproton);
      }
      float antideuteron  = pFlowTrack->PidAntiDeuteron();
      mHistPidAntiDeuteron->Fill(antideuteron);
      if (strcmp(pid, "dbar") == 0) {
	mHistMeanDedxAntiDeuteron->Fill(totalp/charge,pFlowTrack->Dedx());
	mHistPidAntiDeuteronSel->Fill(antideuteron);
      }
      float electron  = pFlowTrack->PidElectron();
      mHistPidElectron->Fill(electron);
      if (strcmp(pid, "e-") == 0) {
	mHistMeanDedxElectron->Fill(totalp/charge,pFlowTrack->Dedx());
	mHistPidElectronSel->Fill(electron);
      }
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

    for (int k = 0; k < Flow::nSels; k++) {
      pFlowSelect->SetSelection(k);
      for (int j = 0; j < Flow::nHars; j++) {
	pFlowSelect->SetHarmonic(j);
	double order  = (double)(j+1);
	float psi_i = mPsi[k][j];
// 	int i = Flow::nSels*k;
// 	// for particles with the opposite subevent
// 	float psi_i;
// 	if (eta > 0.) {
// 	  //float psi_i = mQSub[i+1][j].Phi() / order;
// 	  psi_i = mPsiSub[i+1][j];
// 	} else {
// 	  //psi_i = mQSub[i][j].Phi() / order;
// 	  psi_i = mPsiSub[i][j];
// 	}
	if (pFlowSelect->Select(pFlowTrack)) {
	  // Remove autocorrelations
	  TVector2 Q_i;
	  double phiWgt = pFlowEvent->PhiWeight(phi, k, j);
	  histFull[k].histFullHar[j].mHistPhiFlat->Fill(phi, phiWgt);
	  // for no autocorrelations remove, comment out the next 5 lines
	  if (eta < 0 && (j+1) % 2 == 1) phiWgt *= -1.;
	  if (pFlowEvent->PtWgt()) {
	    phiWgt *= pt;
	  }
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
	  histFull[k].histFullHar[j].mHist_vObs2D-> Fill(eta, pt, v);
	  histFull[k].histFullHar[j].mHist_vObsEta->Fill(eta, v);
	  histFull[k].histFullHar[j].mHist_vObsPt-> Fill(pt, vFlip);
	  histFull[k].mHist_vObs->Fill(order, vFlip);
	  
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

  StThreeVectorF vertex = pFlowEvent->VertexPos();
  Float_t vertexZ = vertex.z();
  mHistEtaSymVerZ2D->Fill(vertexZ , etaSym);

  float etaSymZSlope = 0.003;
  etaSym += (etaSymZSlope * vertexZ); // corrected for acceptance
  etaSym *= sqrt((double)(etaSymPosN + etaSymNegN)); // corrected for statistics
  mHistEtaSym->Fill(etaSym);

  // PID multiplicities
  float totalMult = (float)pFlowEvent->TrackCollection()->size();
  mHistPidMult->Fill(1., totalMult);
  mHistPidMult->Fill(2., piPlusN);
  mHistPidMult->Fill(3., piMinusN);
  mHistPidMult->Fill(4., protonN);
  mHistPidMult->Fill(5., pbarN);
  mHistPidMult->Fill(6., kPlusN);
  mHistPidMult->Fill(7., kMinusN);
  mHistPidMult->Fill(8., deuteronN);
  mHistPidMult->Fill(9., dbarN);
  mHistPidMult->Fill(10., electronN);
  mHistPidMult->Fill(11., positronN);

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

  // Calculate resolution from sqrt(mHistCos)
  double cosPair[Flow::nSels][Flow::nHars];
  double cosPairErr[Flow::nSels][Flow::nHars];
  double content;
  double error;
  for (int k = 0; k < Flow::nSels; k++) {
    char countSels[2];
    sprintf(countSels,"%d",k+1);
    // Creat the 1D v histogram
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
      // for sub res only
//       if (cosPair[k][j] > 0.) {
// 	double resSub = sqrt(cosPair[k][j]);
// 	double resSubErr = cosPairErr[k][j] / (2. * resSub);
// 	mRes[k][j]    = resSub;
// 	mResErr[k][j] = resSubErr;
      } else {
	mRes[k][j]    = 0.;     // subevent correlation must be positive
	mResErr[k][j] = 0.;
      }
      histFull[k].mHistRes->SetBinContent(j+1, mRes[k][j]);
      histFull[k].mHistRes->SetBinError(j+1, mResErr[k][j]);

	// Creat the v 2D histogram
      histTitle = new TString("Flow_v2D_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHist_v2D = 
	histFull[k].histFullHar[j].mHist_vObs2D->ProjectionXY(histTitle->Data());
      histFull[k].histFullHar[j].mHist_v2D->SetTitle(histTitle->Data());
      histFull[k].histFullHar[j].mHist_v2D->SetXTitle((char*)xLabel.Data());
      histFull[k].histFullHar[j].mHist_v2D->SetYTitle("Pt (GeV)");
      histFull[k].histFullHar[j].mHist_v2D->SetZTitle("v (%)");
      delete histTitle;
      AddHist(histFull[k].histFullHar[j].mHist_v2D);

      // Creat the 1D v histograms
      histTitle = new TString("Flow_vEta_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHist_vEta = 
	histFull[k].histFullHar[j].mHist_vObsEta->ProjectionX(histTitle->Data());
      histFull[k].histFullHar[j].mHist_vEta->SetTitle(histTitle->Data());
      histFull[k].histFullHar[j].mHist_vEta->SetXTitle((char*)xLabel.Data());
      histFull[k].histFullHar[j].mHist_vEta->SetYTitle("v (%)");
      delete histTitle;
      AddHist(histFull[k].histFullHar[j].mHist_vEta);

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
      // The systematic error of the resolution is not folded in.
      if (mRes[k][j] != 0.) {
	cout << "##### Resolution= " << mRes[k][j] << " +/- " << mResErr[k][j] 
	     << endl;
	histFull[k].histFullHar[j].mHist_v2D-> Scale(1. / mRes[k][j]);
	histFull[k].histFullHar[j].mHist_vEta->Scale(1. / mRes[k][j]);
	histFull[k].histFullHar[j].mHist_vPt ->Scale(1. / mRes[k][j]);
	content = histFull[k].mHist_v->GetBinContent(j+1);
	histFull[k].mHist_v->SetBinContent(j+1, content / mRes[k][j]);
	error = histFull[k].mHist_v->GetBinError(j+1);
	histFull[k].mHist_v->SetBinError(j+1, error / mRes[k][j]);
	cout << "##### v= (" << content/mRes[k][j] << " +/- " << error/mRes[k][j] 
	     << ") %" << endl;
      } else {
	cout << "##### Resolution of the " << j+1 << "th harmonic was zero."
	     << endl;
	histFull[k].histFullHar[j].mHist_v2D-> Reset();
	histFull[k].histFullHar[j].mHist_vEta->Reset();
	histFull[k].histFullHar[j].mHist_vPt ->Reset();
	histFull[k].mHist_v->SetBinContent(j+1, 0.);
	histFull[k].mHist_v->SetBinError(j+1, 0.);
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
  //GetHistList()->ls();

  // Write all histograms
  TString* fileName = new TString("flow.hist.root");
  fileName->Prepend(pFlowSelect->Number());
  TFile histFile(fileName->Data(), "RECREATE");
  histFile.SetFormat(1);
  GetHistList()->Write();
  histFile.Close();
  delete fileName;
  
  // Write PhiWgt histograms
  fileName = new TString("flowPhiWgtNew.hist.root");
  fileName->Prepend(pFlowSelect->Number());
  TFile phiWgtNewFile(fileName->Data(), "RECREATE");
  phiWgtHistNames->Write();
  phiWgtNewFile.Close();
  delete fileName;
  delete phiWgtHistNames;

  // Print the selection object details
  pFlowSelect->PrintList();

  delete pFlowSelect;

  cout << endl;
  gMessMgr->Summary(3);
  cout << endl;

  return StMaker::Finish();
}
