////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowAnalysisMaker.cxx,v 1.59 2001/12/18 19:27:06 posk Exp $
//
// Authors: Raimond Snellings and Art Poskanzer, LBNL, Aug 1999
//          FTPC added by Markus Oldenburg, MPI, Dec 2000
//
////////////////////////////////////////////////////////////////////////////
//
// Description:  Maker to analyze Flow using the FlowTags and/or StFlowEvent
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
#include "StEnumerations.h"
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
#include "TMath.h"
#define PR(x) cout << "##### FlowAnalysis: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowAnalysisMaker)

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
  pFlowTagMaker = (StFlowTagMaker*)GetMaker("FlowTag");
  if (pFlowTagMaker) pFlowTag = pFlowTagMaker->TagPointer();

  // Get a pointer to StFlowEvent
  StFlowMaker* pFlowMaker = NULL;
  pFlowMaker = (StFlowMaker*)GetMaker("Flow");
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

  float ptMaxPart = Flow::ptMaxPart;
  if (pFlowSelect->PtMaxPart()) {
    ptMaxPart = pFlowSelect->PtMaxPart();
  }
  xLabel = "Pseudorapidity";
  if (strlen(pFlowSelect->PidPart()) != 0) { xLabel = "Rapidity"; }

  const float triggerMin      =    0.;
  const float triggerMax      =   10.;
  const float chargeMin       =  -2.5;
  const float chargeMax       =   2.5; 
  const float dcaMin          =    0.;
  const float dcaMax          =   1.2; 
  const float glDcaMax        =   3.6; 
  const float chi2Min         =    0.;
  const float chi2Max         =    5.; 
  const float fitPtsMinTpc    =  -0.5;
  const float fitPtsMaxTpc    =  60.5; 
  const float maxPtsMinTpc    =  -0.5;
  const float maxPtsMaxTpc    =  60.5; 
  const float fitPtsMinFtpc   =  -0.5;
  const float fitPtsMaxFtpc   =  13.5; 
  const float maxPtsMinFtpc   =  -0.5;
  const float maxPtsMaxFtpc   =  13.5; 
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
  const float multMin         =    0.;
  const float multMax         = 2000.;
  const float qMin            =    0.;
  const float pidMin          =  -10.;
  const float pidMax          =   10.;
  const float centMin         =  -0.5;
  const float centMax         =   9.5;
  const float pMin            =  -2.5;
  const float pMax            =   1.5;
  const float dEdxMax       = 0.00004;
  const float qMax             =  3.5;

  enum { nTriggerBins      = 11,
	 nChargeBins       = 50,
	 nDcaBins          = 60,
	 nChi2Bins         = 50,
	 nFitPtsBinsTpc    = 61,
	 nFitPtsBinsFtpc   = 14,
	 nMaxPtsBinsTpc    = 61,
	 nMaxPtsBinsFtpc   = 14,
	 nFitOverMaxBins   = 40,
	 nOrigMultBins     = 40,
	 nMultEtaBins      = 50,
	 nTotalMultBins    = 40,
	 nMultOverOrigBins = 50,
	 nMultPartBins     = 40,
	 nVertexZBins      = 60,
	 nVertexXYBins     = 50,
	 nEtaSymBins       = 50,
	 nPhi3DBins        = 18,
	 nPsiBins          = 36,
	 nMultBins         = 40,
	 nPidBins          = 50,
         nCentBins         = 10,
	 nDedxBins        = 200,
	 nMomenBins       = 200,
	 n_qBins          = 50
  };
  
  // Trigger
  mHistTrigger = new TH1F("Flow_Trigger", "Flow_Trigger",
      nTriggerBins, triggerMin, triggerMax);
  mHistTrigger->SetXTitle("Trigger: 1 minbias, 2 central, 3 laser, 10 other");
  mHistTrigger->SetYTitle("Counts");

  // Charge
  mHistCharge = new TH1F("Flow_Charge", "Flow_Charge",
      nChargeBins, chargeMin, chargeMax);
  mHistCharge->SetXTitle("Charge");
  mHistCharge->SetYTitle("Counts");
    

  // Distance of closest approach
  // Ftpc
  mHistDcaFtpc = new TH1F("Flow_Dca_Ftpc", "Flow_Dca_Ftpc",
      nDcaBins, dcaMin, dcaMax);
  mHistDcaFtpc->SetXTitle("Track dca to Vertex (cm)");
  mHistDcaFtpc->SetYTitle("Counts");
    

  // Distance of closest approach for global tracks
  // Tpc
  mHistDcaGlobalTpc = new TH1F("Flow_DcaGlobal_Tpc", "Flow_DcaGlobal_Tpc",
      nDcaBins, dcaMin, glDcaMax);
  mHistDcaGlobalTpc->SetXTitle("Global Track dca (cm)");
  mHistDcaGlobalTpc->SetYTitle("Counts");

  // Ftpc
  mHistDcaGlobalFtpc = new TH1F("Flow_DcaGlobal_Ftpc", "Flow_DcaGlobal_Ftpc",
      nDcaBins, dcaMin, glDcaMax);
  mHistDcaGlobalFtpc->SetXTitle("Global Track dca (cm)");
  mHistDcaGlobalFtpc->SetYTitle("Counts");

    
  // Chi2
  // Tpc
  mHistChi2Tpc = new TH1F("Flow_Chi2_Tpc", "Flow_Chi2_Tpc",
      nChi2Bins, chi2Min, chi2Max);
  mHistChi2Tpc->SetXTitle("Chi square per df");
  mHistChi2Tpc->SetYTitle("Counts");
  
  // Ftpc
  mHistChi2Ftpc = new TH1F("Flow_Chi2_Ftpc", "Flow_Chi2_Ftpc",
      nChi2Bins, chi2Min, chi2Max);
  mHistChi2Ftpc->SetXTitle("Chi square per df");
  mHistChi2Ftpc->SetYTitle("Counts");
  
  
  // FitPts
  // Tpc
  mHistFitPtsTpc = new TH1F("Flow_FitPts_Tpc", "Flow_FitPts_Tpc",
      nFitPtsBinsTpc, fitPtsMinTpc, fitPtsMaxTpc);
  mHistFitPtsTpc->SetXTitle("Fit Points");
  mHistFitPtsTpc->SetYTitle("Counts");

  // Ftpc
  mHistFitPtsFtpc = new TH1F("Flow_FitPts_Ftpc", "Flow_FitPts_Ftpc",
      nFitPtsBinsFtpc, fitPtsMinFtpc, fitPtsMaxFtpc);
  mHistFitPtsFtpc->SetXTitle("Fit Points");
  mHistFitPtsFtpc->SetYTitle("Counts");
    

  // MaxPts
  // Tpc
  mHistMaxPtsTpc = new TH1F("Flow_MaxPts_Tpc ", "Flow_MaxPts_Tpc ",
      nMaxPtsBinsTpc , maxPtsMinTpc , maxPtsMaxTpc );
  mHistMaxPtsTpc ->SetXTitle("Max Points");
  mHistMaxPtsTpc ->SetYTitle("Counts");
  
  // Ftpc
  mHistMaxPtsFtpc = new TH1F("Flow_MaxPts_Ftpc", "Flow_MaxPts_Ftpc",
      nMaxPtsBinsFtpc, maxPtsMinFtpc, maxPtsMaxFtpc);
  mHistMaxPtsFtpc->SetXTitle("Max Points");
  mHistMaxPtsFtpc->SetYTitle("Counts");
    

  // FitOverMax
  // Tpc
  mHistFitOverMaxTpc = new TH1F("Flow_FitOverMax_Tpc", "Flow_FitOverMax_Tpc",
      nFitOverMaxBins, fitOverMaxMin, fitOverMaxMax);
  mHistFitOverMaxTpc->SetXTitle("Fit Points / Max Points");
  mHistFitOverMaxTpc->SetYTitle("Counts");
    
  // Ftpc
  mHistFitOverMaxFtpc = new TH1F("Flow_FitOverMax_Ftpc", "Flow_FitOverMax_Ftpc",
      nFitOverMaxBins, fitOverMaxMin, fitOverMaxMax);
  mHistFitOverMaxFtpc->SetXTitle("Fit Points / Max Points");
  mHistFitOverMaxFtpc->SetYTitle("Counts");
    

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
      Flow::nEtaBins, Flow::etaMin, Flow::etaMax, Flow::nPtBins, Flow::ptMin, 
			     Flow::ptMax, nPhi3DBins, phiMin, phiMax);
  mHistEtaPtPhi3D->SetXTitle("Eta");
  mHistEtaPtPhi3D->SetYTitle("Pt (GeV)");
  mHistEtaPtPhi3D->SetZTitle("Phi (rad)");
    
  // Yield for all particles
  mHistYieldAll2D = new TH2D("Flow_YieldAll2D", "Flow_YieldAll2D",
    Flow::nEtaBins, Flow::etaMin, Flow::etaMax, Flow::nPtBins, Flow::ptMin, 
			     Flow::ptMax);
  mHistYieldAll2D->Sumw2();
  mHistYieldAll2D->SetXTitle("Pseudorapidty");
  mHistYieldAll2D->SetYTitle("Pt (GeV)");

  // Yield for particles correlated with the event plane
  mHistYieldPart2D = new TH2D("Flow_YieldPart2D", "Flow_YieldPart2D",
    Flow::nEtaBins, Flow::etaMin, Flow::etaMax, Flow::nPtBins, Flow::ptMin, 
			      ptMaxPart);
  mHistYieldPart2D->Sumw2();
  mHistYieldPart2D->SetXTitle((char*)xLabel.Data());
  mHistYieldPart2D->SetYTitle("Pt (GeV)");

  // Mean Eta in each bin
  mHistBinEta = new TProfile("Flow_Bin_Eta", "Flow_Bin_Eta",
    Flow::nEtaBins, Flow::etaMin, Flow::etaMax, Flow::etaMin, Flow::etaMax, "");
  mHistBinEta->SetXTitle((char*)xLabel.Data());
  mHistBinEta->SetYTitle("<Eta>");
  
  // Mean Pt in each bin
  mHistBinPt = new TProfile("Flow_Bin_Pt", "Flow_Bin_Pt",
    Flow::nPtBins, Flow::ptMin, ptMaxPart, Flow::ptMin, ptMaxPart, "");
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
  mHistPidPiPlusPart = new TH1F("Flow_PidPiPlusPart", 
			       "Flow_PidPiPlusPart",
			       nPidBins, pidMin, pidMax);
  mHistPidPiPlusPart->SetXTitle("(PID - Mean) / Resolution");
  mHistPidPiPlusPart->SetYTitle("Counts");
    
  // PID pi- selected
  mHistPidPiMinusPart = new TH1F("Flow_PidPiMinusPart", 
				"Flow_PidPiMinusPart",
				nPidBins, pidMin, pidMax);
  mHistPidPiMinusPart->SetXTitle("(PID - Mean) / Resolution");
  mHistPidPiMinusPart->SetYTitle("Counts");
    
  // PID proton selected
  mHistPidProtonPart = new TH1F("Flow_PidProtonPart", 
			       "Flow_PidProtonPart",
			       nPidBins, pidMin, pidMax);
  mHistPidProtonPart->SetXTitle("(PID - Mean) / Resolution");
  mHistPidProtonPart->SetYTitle("Counts");

  // PID anti proton selected
  mHistPidAntiProtonPart = new TH1F("Flow_PidAntiProtonPart", 
				   "Flow_PidAntiProtonPart",
				   nPidBins, pidMin, pidMax);
  mHistPidAntiProtonPart->SetXTitle("(PID - Mean) / Resolution");
  mHistPidAntiProtonPart->SetYTitle("Counts");

  // PID Kplus selected
  mHistPidKplusPart = new TH1F("Flow_PidKplusPart", 
			      "Flow_PidKplusPart",
			      nPidBins, pidMin, pidMax);
  mHistPidKplusPart->SetXTitle("(PID - Mean) / Resolution");
  mHistPidKplusPart->SetYTitle("Counts");

  // PID Kminus selected
  mHistPidKminusPart = new TH1F("Flow_PidKminusPart", 
			       "Flow_PidKminusPart",
			       nPidBins, pidMin, pidMax);
  mHistPidKminusPart->SetXTitle("(PID - Mean) / Resolution");
  mHistPidKminusPart->SetYTitle("Counts");

  // PID deuteron selected
  mHistPidDeuteronPart = new TH1F("Flow_PidDeuteronPart", 
				 "Flow_PidDeuteronPart",
				 nPidBins, pidMin, pidMax);
  mHistPidDeuteronPart->SetXTitle("(PID - Mean) / Resolution");
  mHistPidDeuteronPart->SetYTitle("Counts");

  // PID anti deuteron selected
  mHistPidAntiDeuteronPart = new TH1F("Flow_PidAntiDeuteronPart", 
				     "Flow_PidAntiDeuteronPart",
				     nPidBins, pidMin, pidMax);
  mHistPidAntiDeuteronPart->SetXTitle("(PID - Mean) / Resolution");
  mHistPidAntiDeuteronPart->SetYTitle("Counts");

  // PID electron selected
  mHistPidElectronPart = new TH1F("Flow_PidElectronPart", 
				 "Flow_PidElectronPart",
				 nPidBins, pidMin, pidMax);
  mHistPidElectronPart->SetXTitle("(PID - Mean) / Resolution");
  mHistPidElectronPart->SetYTitle("Counts");

  // PID positron selected
  mHistPidPositronPart = new TH1F("Flow_PidPositronPart", 
				 "Flow_PidPositronPart",
				 nPidBins, pidMin, pidMax);
  mHistPidPositronPart->SetXTitle("(PID - Mean) / Resolution");
  mHistPidPositronPart->SetYTitle("Counts");

  // PID multiplicities selected
  mHistPidMult = new TProfile("Flow_PidMult", "Flow_PidMult",
			      11, 0.5, 11.5, 0., 10000., "");
  mHistPidMult->SetXTitle("all, pi+, pi-, pr+, pr-, K+, K-, d+, d-, e-, e+");
  mHistPidMult->SetYTitle("Multiplicity");
    
  // Centrality
  mHistCent = new TH1F("Flow_Cent", "Flow_Cent",
		       nCentBins, centMin, centMax);
  mHistCent->SetXTitle("Centrality Bin");
  mHistCent->SetYTitle("Counts");
    
  // CTB versus ZDC
  mHistCTBvsZDC2D = new TH2F("Flow_CTBvsZDC2D", "Flow_CTBvsZDC2D",
			       125, 0, 250,
			       125, 0, 25000);
  mHistCTBvsZDC2D->SetXTitle("ZDC sum");
  mHistCTBvsZDC2D->SetYTitle("CTB sum");

  // MeanDedxPos
  mHistMeanDedxPos2D = new TH2F("Flow_MeanDedxPos2D",
				"Flow_MeanDedxPos2D",
				nMomenBins, pMin, pMax,
				nDedxBins, 0, dEdxMax);
  mHistMeanDedxPos2D->SetXTitle("log(momentum) (GeV)");
  mHistMeanDedxPos2D->SetYTitle("mean dEdx");
  
  // MeanDedxNeg
  mHistMeanDedxNeg2D = new TH2F("Flow_MeanDedxNeg2D",
				"Flow_MeanDedxNeg2D",
				nMomenBins, pMin, pMax,
			   nDedxBins, 0, dEdxMax);
  mHistMeanDedxNeg2D->SetXTitle("log(momentum) (GeV)");
  mHistMeanDedxNeg2D->SetYTitle("mean dEdx");
  
  // MeanDedx PiPlus
  mHistMeanDedxPiPlus2D = new TH2F("Flow_MeanDedxPiPlus2D", 
				   "Flow_MeanDedxPiPlus2D",
				   nMomenBins, pMin, pMax,
				   nDedxBins, 0, dEdxMax);
  mHistMeanDedxPiPlus2D->SetXTitle("log(momentum) (GeV)");
  mHistMeanDedxPiPlus2D->SetYTitle("mean dEdx");
  
  // MeanDedxPiMinus
  mHistMeanDedxPiMinus2D = new TH2F("Flow_MeanDedxPiMinus2D", 
				    "Flow_MeanDedxPiMinus2D",
				    nMomenBins, pMin, pMax,
				    nDedxBins, 0, dEdxMax);
  mHistMeanDedxPiMinus2D->SetXTitle("log(momentum) (GeV)");
  mHistMeanDedxPiMinus2D->SetYTitle("mean dEdx");
  
  // MeanDedxProton
  mHistMeanDedxProton2D = new TH2F("Flow_MeanDedxProton2D", 
				   "Flow_MeanDedxProton2D",
				   nMomenBins, pMin, pMax,
				   nDedxBins, 0, dEdxMax);
  mHistMeanDedxProton2D->SetXTitle("log(momentum) (GeV)");
  mHistMeanDedxProton2D->SetYTitle("mean dEdx");
  
  // MeanDedxPbar
  mHistMeanDedxPbar2D = new TH2F("Flow_MeanDedxPbar2D", 
				 "Flow_MeanDedxPbar2D",
				 nMomenBins, pMin, pMax,
				 nDedxBins, 0, dEdxMax);
  mHistMeanDedxPbar2D->SetXTitle("log(momentum) (GeV)");
  mHistMeanDedxPbar2D->SetYTitle("mean dEdx");
  
  // MeanDedxKplus
  mHistMeanDedxKplus2D = new TH2F("Flow_MeanDedxKplus2D", 
				  "Flow_MeanDedxKplus2D",
				  nMomenBins, pMin, pMax,
				  nDedxBins, 0, dEdxMax);
  mHistMeanDedxKplus2D->SetXTitle("log(momentum) (GeV)");
  mHistMeanDedxKplus2D->SetYTitle("mean dEdx");
  
  // MeanDedxKminus
  mHistMeanDedxKminus2D = new TH2F("Flow_MeanDedxKminus2D", 
				   "Flow_MeanDedxKminus2D",
				   nMomenBins, pMin, pMax,
				   nDedxBins, 0, dEdxMax);
  mHistMeanDedxKminus2D->SetXTitle("log(momentum) (GeV)");
  mHistMeanDedxKminus2D->SetYTitle("mean dEdx");
  
  // MeanDedxDeuteron
  mHistMeanDedxDeuteron2D = new TH2F("Flow_MeanDedxDeuteron2D", 
				     "Flow_MeanDedxDeuteron2D",
				     nMomenBins, pMin, pMax,
				     nDedxBins, 0, dEdxMax);
  mHistMeanDedxDeuteron2D->SetXTitle("log(momentum) (GeV)");
  mHistMeanDedxDeuteron2D->SetYTitle("mean dEdx");

  // MeanDedxAntiDeuteron
  mHistMeanDedxAntiDeuteron2D = new TH2F("Flow_MeanDedxAntiDeuteron2D", 
					 "Flow_MeanDedxAntiDeuteron2D",
					 nMomenBins, pMin, pMax,
					 nDedxBins, 0, dEdxMax);
  mHistMeanDedxAntiDeuteron2D->SetXTitle("log(momentum) (GeV)");
  mHistMeanDedxAntiDeuteron2D->SetYTitle("mean dEdx");
  
  // MeanDedxElectron
  mHistMeanDedxElectron2D = new TH2F("Flow_MeanDedxElectron2D", 
				     "Flow_MeanDedxElectron2D",
				     nMomenBins, pMin, pMax,
				     nDedxBins, 0, dEdxMax);
  mHistMeanDedxElectron2D->SetXTitle("log(momentum) (GeV)");
  mHistMeanDedxElectron2D->SetYTitle("mean dEdx");
  
  // MeanDedxPositron
  mHistMeanDedxPositron2D = new TH2F("Flow_MeanDedxPositron2D", 
				     "Flow_MeanDedxPositron2D",
				     nMomenBins, pMin, pMax,
				     nDedxBins, 0, dEdxMax);
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
      // Tpc (FarEast)
      histTitle = new TString("Flow_Phi_FarEast_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhiFarEast = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
      histFull[k].histFullHar[j].mHistPhiFarEast->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histFullHar[j].mHistPhiFarEast->SetYTitle("Counts");
      delete histTitle;
      
      // Tpc (East)
      histTitle = new TString("Flow_Phi_East_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhiEast = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
      histFull[k].histFullHar[j].mHistPhiEast->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histFullHar[j].mHistPhiEast->SetYTitle("Counts");
      delete histTitle;
      
      // Tpc (West)
      histTitle = new TString("Flow_Phi_West_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhiWest = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
      histFull[k].histFullHar[j].mHistPhiWest->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histFullHar[j].mHistPhiWest->SetYTitle("Counts");
      delete histTitle;
      
      // Tpc (FarWest)
      histTitle = new TString("Flow_Phi_FarWest_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhiFarWest = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
      histFull[k].histFullHar[j].mHistPhiFarWest->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histFullHar[j].mHistPhiFarWest->SetYTitle("Counts");
      delete histTitle;
      
      // Ftpc (East)
      histTitle = new TString("Flow_Phi_FtpcEast_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhiFtpcEast = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBinsFtpc, phiMin, phiMax);
      histFull[k].histFullHar[j].mHistPhiFtpcEast->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histFullHar[j].mHistPhiFtpcEast->SetYTitle("Counts");
      delete histTitle;
      
      // Ftpc (West)
      histTitle = new TString("Flow_Phi_FtpcWest_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhiFtpcWest = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBinsFtpc, phiMin, phiMax);
      histFull[k].histFullHar[j].mHistPhiFtpcWest->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histFullHar[j].mHistPhiFtpcEast->SetYTitle("Counts");
      delete histTitle;
      

      // PhiWgt new
      // Tpc (FarEast)
      histTitle = new TString("Flow_Phi_Weight_FarEast_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhiWgtFarEast = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
      histFull[k].histFullHar[j].mHistPhiWgtFarEast->Sumw2();
      histFull[k].histFullHar[j].mHistPhiWgtFarEast->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histFullHar[j].mHistPhiWgtFarEast->SetYTitle("PhiWgt");
      delete histTitle;
      
      // Tpc (East)
      histTitle = new TString("Flow_Phi_Weight_East_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhiWgtEast = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
      histFull[k].histFullHar[j].mHistPhiWgtEast->Sumw2();
      histFull[k].histFullHar[j].mHistPhiWgtEast->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histFullHar[j].mHistPhiWgtEast->SetYTitle("PhiWgt");
      delete histTitle;
      
      // Tpc (West)
      histTitle = new TString("Flow_Phi_Weight_West_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhiWgtWest = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
      histFull[k].histFullHar[j].mHistPhiWgtWest->Sumw2();
      histFull[k].histFullHar[j].mHistPhiWgtWest->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histFullHar[j].mHistPhiWgtWest->SetYTitle("PhiWgt");
      delete histTitle;
      
      // Tpc (FarWest)
      histTitle = new TString("Flow_Phi_Weight_FarWest_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhiWgtFarWest = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
      histFull[k].histFullHar[j].mHistPhiWgtFarWest->Sumw2();
      histFull[k].histFullHar[j].mHistPhiWgtFarWest->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histFullHar[j].mHistPhiWgtFarWest->SetYTitle("PhiWgt");
      delete histTitle;
      
      // Ftpc (East)
      histTitle = new TString("Flow_Phi_Weight_FtpcEast_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhiWgtFtpcEast = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBinsFtpc, phiMin, phiMax);
      histFull[k].histFullHar[j].mHistPhiWgtFtpcEast->Sumw2();
      histFull[k].histFullHar[j].mHistPhiWgtFtpcEast->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histFullHar[j].mHistPhiWgtFtpcEast->SetYTitle("PhiWgt");
      delete histTitle;
      
      // Ftpc (West)
      histTitle = new TString("Flow_Phi_Weight_FtpcWest_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhiWgtFtpcWest = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBinsFtpc, phiMin, phiMax);
      histFull[k].histFullHar[j].mHistPhiWgtFtpcWest->Sumw2();
      histFull[k].histFullHar[j].mHistPhiWgtFtpcWest->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histFullHar[j].mHistPhiWgtFtpcWest->SetYTitle("PhiWgt");
      delete histTitle;
      

      // Phi lab flattened
      // Tpc (FarEast)
      histTitle = new TString("Flow_Phi_Flat_FarEast_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhiFlatFarEast = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
      histFull[k].histFullHar[j].mHistPhiFlatFarEast->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histFullHar[j].mHistPhiFlatFarEast->SetYTitle("Counts");
      delete histTitle;

      // Tpc (East)
      histTitle = new TString("Flow_Phi_Flat_East_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhiFlatEast = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
      histFull[k].histFullHar[j].mHistPhiFlatEast->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histFullHar[j].mHistPhiFlatEast->SetYTitle("Counts");
      delete histTitle;

      // Tpc (West)
      histTitle = new TString("Flow_Phi_Flat_West_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhiFlatWest = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
      histFull[k].histFullHar[j].mHistPhiFlatWest->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histFullHar[j].mHistPhiFlatWest->SetYTitle("Counts");
      delete histTitle;

      // Tpc (FarWest)
      histTitle = new TString("Flow_Phi_Flat_FarWest_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhiFlatFarWest = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
      histFull[k].histFullHar[j].mHistPhiFlatFarWest->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histFullHar[j].mHistPhiFlatFarWest->SetYTitle("Counts");
      delete histTitle;

      // Ftpc (East)
      histTitle = new TString("Flow_Phi_Flat_FtpcEast_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhiFlatFtpcEast =	new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBinsFtpc, phiMin, phiMax);
      histFull[k].histFullHar[j].mHistPhiFlatFtpcEast->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histFullHar[j].mHistPhiFlatFtpcEast->SetYTitle("Counts");
      delete histTitle;
      
      // Ftpc (West)
      histTitle = new TString("Flow_Phi_Flat_FtpcWest_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhiFlatFtpcWest =	new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBinsFtpc, phiMin, phiMax);
      histFull[k].histFullHar[j].mHistPhiFlatFtpcWest->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histFullHar[j].mHistPhiFlatFtpcWest->SetYTitle("Counts");
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

      // Yield(eta,pt)
      histTitle = new TString("Flow_Yield2D_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistYield2D =	new TH2D(histTitle->Data(),
        histTitle->Data(), Flow::nEtaBins, Flow::etaMin, Flow::etaMax,
			   Flow::nPtBins, Flow::ptMin, Flow::ptMax);
      histFull[k].histFullHar[j].mHistYield2D->Sumw2();
      histFull[k].histFullHar[j].mHistYield2D->SetXTitle("Pseudorapidty");
      histFull[k].histFullHar[j].mHistYield2D->SetYTitle("Pt (GeV)");
      delete histTitle;

      // Yield(eta,phi)
      histTitle = new TString("Flow_EtaPhi2D_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistEtaPhi2D = new TH2D(histTitle->Data(),
        histTitle->Data(), Flow::nEtaBins, Flow::etaMin, Flow::etaMax,
			   Flow::nPhiBins, phiMin, phiMax);
      histFull[k].histFullHar[j].mHistEtaPhi2D->Sumw2();
      histFull[k].histFullHar[j].mHistEtaPhi2D->SetXTitle("Pseudorapidty");
      histFull[k].histFullHar[j].mHistEtaPhi2D->SetYTitle("Azimuthal Angle (rad)");
      delete histTitle;

      // Flow observed
      histTitle = new TString("Flow_vObs2D_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHist_vObs2D =	new TProfile2D(histTitle->Data(),
        histTitle->Data(), Flow::nEtaBins, Flow::etaMin, Flow::etaMax, Flow::nPtBins, 
		 Flow::ptMin, ptMaxPart, -100., 100., "");
      histFull[k].histFullHar[j].mHist_vObs2D->SetXTitle((char*)xLabel.Data());
      histFull[k].histFullHar[j].mHist_vObs2D->SetYTitle("Pt (GeV)");
      delete histTitle;

      // Flow observed profiles
      histTitle = new TString("Flow_vObsEta_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHist_vObsEta = new TProfile(histTitle->Data(),
        histTitle->Data(), Flow::nEtaBins, Flow::etaMin, Flow::etaMax, 
							      -100., 100., "");
      histFull[k].histFullHar[j].mHist_vObsEta->SetXTitle((char*)xLabel.Data());
      histFull[k].histFullHar[j].mHist_vObsEta->SetYTitle("v (%)");
      delete histTitle;

      histTitle = new TString("Flow_vObsPt_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHist_vObsPt = new TProfile(histTitle->Data(),
        histTitle->Data(), Flow::nPtBins, Flow::ptMin, ptMaxPart, -100., 100., "");
      histFull[k].histFullHar[j].mHist_vObsPt->SetXTitle("Pt (GeV)");
      histFull[k].histFullHar[j].mHist_vObsPt->SetYTitle("v (%)");
      delete histTitle;

    }
  }

  gMessMgr->SetLimit("##### FlowAnalysis", 2);
  gMessMgr->Info("##### FlowAnalysis: $Id: StFlowAnalysisMaker.cxx,v 1.59 2001/12/18 19:27:06 posk Exp $");

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
    mMultSub[0][j] = pFlowTag->na[j];
    mMultSub[1][j] = pFlowTag->nb[j];
    mMultSub[2][j] = pFlowTag->nc[j];
    mMultSub[3][j] = pFlowTag->nd[j];

    // calculate Psi
    for (int i = 0; i < nSels * nSubs; i++) {
      mPsiSub[i][j] = mQSub[i][j].Phi() / order;
    }

    // full event quantities
    for (int k = 0; k < nSels; k++) {
      mQ[k][j]    = mQSub[nSels*k][j] + mQSub[nSels*k+1][j];
      mPsi[k][j]  = mQ[k][j].Phi() / order;
      mMult[k][j] = mMultSub[nSels*k][j] + mMultSub[nSels*k+1][j];
      m_q[k][j]   = (mMult[k][j] > 0) ? mQ[k][j].Mod()/sqrt((double)mMult[k][j])
	: 0.;
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
      mQ[k][j]    = pFlowEvent->Q(pFlowSelect);
      mPsi[k][j]  = pFlowEvent->Psi(pFlowSelect);
      m_q[k][j]   = pFlowEvent->q(pFlowSelect);
      mMult[k][j] = pFlowEvent->Mult(pFlowSelect);
    }
  }

}

//-----------------------------------------------------------------------

void StFlowAnalysisMaker::FillEventHistograms() {
  // Fill histograms with event quantities

  unsigned int triggerWord = pFlowEvent->L0TriggerWord();
  float trigger;
  switch (triggerWord) {
  case 4096:  trigger = 1.;  break; // minbias
  case 4352:  trigger = 2.;  break; // central
  case 61952: trigger = 3.;  break; // laser
  default:    trigger = 10.; break; // no clue
  }
  mHistTrigger->Fill(trigger);

  // no selections: OrigMult, Centrality, Mult, MultOverOrig, VertexZ, VertexXY
  int origMult  = pFlowEvent->OrigMult();
  mHistOrigMult ->Fill((float)origMult);
  mHistMultEta  ->Fill((float)pFlowEvent->MultEta());
  int cent      = pFlowEvent->Centrality();
  mHistCent     ->Fill((float)cent);
  int totalMult = pFlowEvent->TrackCollection()->size();
  mHistMult     ->Fill((float)totalMult);
  if (origMult) mHistMultOverOrig->Fill((float)totalMult / (float)origMult);

  StThreeVectorF vertex = pFlowEvent->VertexPos();
  mHistVertexZ   ->Fill(vertex.z());
  mHistVertexXY2D->Fill(vertex.x(), vertex.y());

  mHistCTBvsZDC2D->Fill(pFlowEvent->ZDCe() + pFlowEvent->ZDCe(), 
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
      
      if (mPsiSub[Flow::nSels*k][j] != 0. && mPsiSub[Flow::nSels*k+1][j] != 0.) {
	float psiSubCorr = mPsiSub[Flow::nSels*k][j] - mPsiSub[Flow::nSels*k+1][j];
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

    float phi       = pFlowTrack->Phi();
    if (phi < 0.) phi += twopi;
    float eta       = pFlowTrack->Eta();
    float pt        = pFlowTrack->Pt();
    int   charge    = pFlowTrack->Charge();
    float dca       = pFlowTrack->Dca();
    float dcaGlobal = pFlowTrack->DcaGlobal();
    float chi2      = pFlowTrack->Chi2();
    int   fitPts    = pFlowTrack->FitPts();
    int   maxPts    = pFlowTrack->MaxPts();
    Char_t pid[10];
    strcpy(pid, pFlowTrack->Pid());
    float totalp    = pFlowTrack->P();
    float logp      = log(totalp);
    float dEdx      = pFlowTrack->Dedx();

    // no selections: Charge, Dca, Chi2, FitPts, MaxPts, FitOverMax, PID
    mHistCharge->Fill((float)charge);

    // distinguish between Tpc and Ftpc
    if (pFlowTrack->TopologyMap().numberOfHits(kFtpcEastId) ||
	pFlowTrack->TopologyMap().numberOfHits(kFtpcWestId)) {
      mHistDcaFtpc      ->Fill(dca);
      mHistDcaGlobalFtpc->Fill(dcaGlobal);
      mHistChi2Ftpc     ->Fill(chi2);
      mHistFitPtsFtpc   ->Fill((float)fitPts);
      mHistMaxPtsFtpc   ->Fill((float)maxPts);
      if (maxPts) mHistFitOverMaxFtpc->Fill((float)fitPts/(float)maxPts);
    }
    
    else { // Tpc track or otherwise!!!
      
      // For PID multiplicites
      if (strcmp(pid, "pi+")  == 0)  piPlusN++;
      if (strcmp(pid, "pi-")  == 0)  piMinusN++;
      if (strcmp(pid, "pr+")  == 0)  protonN++;
      if (strcmp(pid, "pr-")  == 0)  pbarN++;
      if (strcmp(pid, "k+")   == 0)  kPlusN++;
      if (strcmp(pid, "k-")   == 0)  kMinusN++;
      if (strcmp(pid, "d+")   == 0)  deuteronN++;
      if (strcmp(pid, "d-")   == 0)  dbarN++;
      if (strcmp(pid, "e-")   == 0)  electronN++;
      if (strcmp(pid, "e+")   == 0)  positronN++;
      
      mHistDcaGlobalTpc->Fill(dcaGlobal);
      mHistChi2Tpc     ->Fill(chi2);
      mHistFitPtsTpc   ->Fill((float)fitPts);
      mHistMaxPtsTpc   ->Fill((float)maxPts);
      if (maxPts) mHistFitOverMaxTpc->Fill((float)fitPts/(float)maxPts);
      
      if (charge == 1) {
	mHistMeanDedxPos2D->Fill(logp, dEdx);
	float piPlus = pFlowTrack->PidPiPlus();
	mHistPidPiPlus->Fill(piPlus);
	if (strcmp(pid, "pi+") == 0) {
	  mHistMeanDedxPiPlus2D->Fill(logp, dEdx);
	  mHistPidPiPlusPart->Fill(piPlus);
	}
	float kplus  = pFlowTrack->PidKaonPlus();
	mHistPidKplus->Fill(kplus);
	if (strcmp(pid, "k+") == 0) {
	  mHistMeanDedxKplus2D->Fill(logp, dEdx);
	  mHistPidKplusPart->Fill(kplus);
	}
	float proton  = pFlowTrack->PidProton();
	mHistPidProton->Fill(proton);
	if (strcmp(pid, "pr+") == 0) {
	  mHistMeanDedxProton2D->Fill(logp, dEdx);
	  mHistPidProtonPart->Fill(proton);
	}
	float deuteron  = pFlowTrack->PidDeuteron();
	mHistPidDeuteron->Fill(deuteron);
	if (strcmp(pid, "d+") == 0) {
	  mHistMeanDedxDeuteron2D->Fill(logp, dEdx);
	  mHistPidDeuteronPart->Fill(deuteron);
	}
	float positron  = pFlowTrack->PidPositron();
	mHistPidPositron->Fill(positron);
	if (strcmp(pid, "e+") == 0) {
	  mHistMeanDedxPositron2D->Fill(logp, dEdx);
	  mHistPidPositronPart->Fill(positron);
	  }
      } else if (charge == -1) {
	mHistMeanDedxNeg2D->Fill(logp, dEdx);
	float piMinus = pFlowTrack->PidPiMinus();
	mHistPidPiMinus->Fill(piMinus);
	if (strcmp(pid, "pi-") == 0) {
	  mHistMeanDedxPiMinus2D->Fill(logp, dEdx);
	  mHistPidPiMinusPart->Fill(piMinus);
	}
	float kminus  = pFlowTrack->PidKaonMinus();
	mHistPidKminus->Fill(kminus);
	if (strcmp(pid, "k-") == 0) {
	  mHistMeanDedxKminus2D->Fill(logp, dEdx);
	  mHistPidKminusPart->Fill(kminus);
	}
	float antiproton  = pFlowTrack->PidAntiProton();
	mHistPidAntiProton->Fill(antiproton);
	if (strcmp(pid, "pr-") == 0) {
	  mHistMeanDedxPbar2D->Fill(logp, dEdx);
	  mHistPidAntiProtonPart->Fill(antiproton);
	}
	float antideuteron  = pFlowTrack->PidAntiDeuteron();
	mHistPidAntiDeuteron->Fill(antideuteron);
	if (strcmp(pid, "d-") == 0) {
	  mHistMeanDedxAntiDeuteron2D->Fill(logp, dEdx);
	  mHistPidAntiDeuteronPart->Fill(antideuteron);
	}
	float electron  = pFlowTrack->PidElectron();
	mHistPidElectron->Fill(electron);
	if (strcmp(pid, "e-") == 0) {
	  mHistMeanDedxElectron2D->Fill(logp, dEdx);
	  mHistPidElectronPart->Fill(electron);
	}
      }
    }
    
    // Yield3D, Yield2D, BinEta, BinPt
    mHistEtaPtPhi3D->Fill(eta, pt, phi);
    mHistYieldAll2D->Fill(eta, pt);
    if (pFlowSelect->SelectPart(pFlowTrack)) {
      if (strlen(pFlowSelect->PidPart()) != 0) { 
	float rapidity = pFlowTrack->Y();
	mHistBinEta->Fill(rapidity, rapidity);
	mHistYieldPart2D->Fill(rapidity, pt);
      } else {
	mHistBinEta->Fill(eta, eta);
	mHistYieldPart2D->Fill(eta, pt);
      }
      mHistBinPt->Fill(pt, pt);
    }

    // cos(n*phiLab)
    for (int j = 0; j < Flow::nHars; j++) {
      bool oddHar = (j+1) % 2;
      float order = (float)(j+1);
      float vIn   = cos((double)order * phi)/perCent;
      if (eta < 0 && oddHar) vIn *= -1;
      mHistCosPhi->Fill(order, vIn);
    }

    // For Eta symmetry
    (eta > 0.) ? etaSymPosN++ : etaSymNegN++;

    Float_t vertexZ = pFlowEvent->VertexPos().z();
    for (int k = 0; k < Flow::nSels; k++) {
      pFlowSelect->SetSelection(k);
      for (int j = 0; j < Flow::nHars; j++) {
	bool oddHar = (j+1) % 2;
	pFlowSelect->SetHarmonic(j);
	double order  = (double)(j+1);
	float psi_i;
	if (pFlowEvent->EtaSubs()) { // particles with the opposite subevent
	  int i = Flow::nSels*k;
	  psi_i = (eta > 0.) ? mPsiSub[i+1][j] : mPsiSub[i][j];
	} else {
	  psi_i = mPsi[k][j];
	}
	if (pFlowSelect->Select(pFlowTrack)) {
	  // Get detID
	  StDetectorId detId;
	  Bool_t kTpcFarEast = kFALSE;
	  Bool_t kTpcEast    = kFALSE;
	  Bool_t kTpcWest    = kFALSE;
	  Bool_t kTpcFarWest = kFALSE;
	  if (pFlowTrack->TopologyMap().numberOfHits(kTpcId) || 
	      (pFlowTrack->TopologyMap().data(0) == 0 && 
	       pFlowTrack->TopologyMap().data(1) == 0)) {
	    // Tpc track, or TopologyMap not available
	    detId = kTpcId;
	    // Set TpcEast and West
	    if (eta > 0. && vertexZ > 0.) {
	      kTpcFarWest = kTRUE;
	    } else if (eta > 0. && vertexZ < 0.) {
	      kTpcWest = kTRUE;
	    } else if (eta < 0. && vertexZ > 0.) {
	      kTpcEast = kTRUE;
	    } else {
	      kTpcFarEast = kTRUE;
	    }
	  } else if (pFlowTrack->TopologyMap().numberOfHits(kFtpcEastId)) {
	    detId = kFtpcEastId;
	  } else if (pFlowTrack->TopologyMap().numberOfHits(kFtpcWestId)) {
	    detId = kFtpcWestId;
	  } else {
	    detId = kUnknownId;
	  }

	  // Fill histograms with selections
	  if (detId == kFtpcEastId) {
	    histFull[k].histFullHar[j].mHistPhiFtpcEast->Fill(phi);
	  } else if (detId == kFtpcWestId) {
	    histFull[k].histFullHar[j].mHistPhiFtpcWest->Fill(phi);
	  } else if (kTpcFarEast){
	    histFull[k].histFullHar[j].mHistPhiFarEast->Fill(phi);
	  } else if (kTpcEast){
	    histFull[k].histFullHar[j].mHistPhiEast->Fill(phi);
	  } else if (kTpcWest){
	    histFull[k].histFullHar[j].mHistPhiWest->Fill(phi);
	  } else if (kTpcFarWest){
	    histFull[k].histFullHar[j].mHistPhiFarWest->Fill(phi);
	  }
	  histFull[k].histFullHar[j].mHistYield2D->Fill(eta, pt);
	  histFull[k].histFullHar[j].mHistEtaPhi2D->Fill(eta, phi);

	  // Get phiWgt
	  double phiWgt = pFlowEvent->PhiWeight(k, j, pFlowTrack);

	  double phiWgtRaw = phiWgt;
	  if (pFlowEvent->PtWgt()) { // remove pt weighting
	    phiWgtRaw /= (pt < 2.) ? pt : 2.;
	  }
	  if (oddHar && eta < 0.) phiWgtRaw /= -1.;
	  if (detId == kFtpcEastId) {
	    histFull[k].histFullHar[j].mHistPhiFlatFtpcEast->Fill(phi, phiWgtRaw);
	  } else if (detId == kFtpcWestId) {
	    histFull[k].histFullHar[j].mHistPhiFlatFtpcWest->Fill(phi, phiWgtRaw);
	  } else if (kTpcFarEast) {
	    histFull[k].histFullHar[j].mHistPhiFlatFarEast->Fill(phi, phiWgtRaw);
	  } else if (kTpcEast) {
	    histFull[k].histFullHar[j].mHistPhiFlatEast->Fill(phi, phiWgtRaw);
	  } else if (kTpcWest) {
	    histFull[k].histFullHar[j].mHistPhiFlatWest->Fill(phi, phiWgtRaw);
	  } else if (kTpcFarWest) {
	    histFull[k].histFullHar[j].mHistPhiFlatFarWest->Fill(phi, phiWgtRaw);
	  }

	  // Remove autocorrelations
	  TVector2 Q_i;
	  if (!pFlowEvent->EtaSubs()) {
	    Q_i.Set(phiWgt * cos(phi * order), phiWgt * sin(phi * order));
	    TVector2 mQ_i = mQ[k][j] - Q_i;
	    psi_i = mQ_i.Phi() / order;
	    if (psi_i < 0.) psi_i += twopi / order;
	  }
	}

       	// Caculate v for all particles selected for correlation analysis
	if (pFlowSelect->SelectPart(pFlowTrack)) {
	  corrMultN++;
	  float v = cos(order * (phi - psi_i))/perCent;
	  float vFlip = v;
	  if (eta < 0 && oddHar) vFlip *= -1;
	  if (strlen(pFlowSelect->PidPart()) != 0) { 
	    float rapidity = pFlowTrack->Y();
	    histFull[k].histFullHar[j].mHist_vObs2D-> Fill(rapidity, pt, v);
	    histFull[k].histFullHar[j].mHist_vObsEta->Fill(rapidity, v);
	  } else {
	    histFull[k].histFullHar[j].mHist_vObs2D-> Fill(eta, pt, v);
	    histFull[k].histFullHar[j].mHist_vObsEta->Fill(eta, v);
	  }
	  histFull[k].histFullHar[j].mHist_vObsPt-> Fill(pt, vFlip);
	  histFull[k].mHist_vObs->Fill(order, vFlip);
	  
	  // Correlation of Phi of all particles with Psi
	  float phi_i = phi;
	  if (eta < 0 && oddHar) {
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
  etaSym += (etaSymZSlope * vertexZ);                // corrected for acceptance
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
  mHistMultPart->Fill(corrMultN);

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

static Double_t chi(double res) {
  // Calculates chi from the event plane resolution

  double chi   = 2.0;
  double delta = 1.0;

  for (int i = 0; i < 15; i++) {
    chi   = (resEventPlane(chi) < res) ? chi + delta : chi - delta;
    delta = delta / 2.;
  }

  return chi;
}

//-----------------------------------------------------------------------

Int_t StFlowAnalysisMaker::Finish() {
  // Calculates resolution and mean flow values
  // Outputs phiWgt values
  TString* histTitle;

  // PhiWgt histogram collection
  TOrdCollection* phiWgtHistNames = new TOrdCollection(Flow::nSels*Flow::nHars);

  // Calculate resolution from sqrt(mHistCos)
  double cosPair[Flow::nSels][Flow::nHars];
  double cosPairErr[Flow::nSels][Flow::nHars];
  double content;
  double error;
  double totalError;
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
      if (pFlowEvent->EtaSubs()) { // sub res only
	if (cosPair[k][j] > 0.) {
	  double resSub = sqrt(cosPair[k][j]);
	  double resSubErr = cosPairErr[k][j] / (2. * resSub);
	  mRes[k][j]    = resSub;
	  mResErr[k][j] = resSubErr;
	} else {
	  mRes[k][j]    = 0.;       // subevent correlation must be positive
	  mResErr[k][j] = 0.;
	}
      } else {
	if (cosPair[k][j] > 0.92) { // resolution saturates
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
      if (mRes[k][j]) {
	cout << "##### Resolution of the " << j+1 << "th harmonic = " << 
	  mRes[k][j] << " +/- " << mResErr[k][j] 
	     << endl;
	// The systematic error of the resolution is not folded in.
	histFull[k].histFullHar[j].mHist_v2D-> Scale(1. / mRes[k][j]);
	histFull[k].histFullHar[j].mHist_vEta->Scale(1. / mRes[k][j]);
	histFull[k].histFullHar[j].mHist_vPt ->Scale(1. / mRes[k][j]);
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
	histFull[k].histFullHar[j].mHist_v2D-> Reset();
	histFull[k].histFullHar[j].mHist_vEta->Reset();
	histFull[k].histFullHar[j].mHist_vPt ->Reset();
	histFull[k].mHist_v->SetBinContent(j+1, 0.);
	histFull[k].mHist_v->SetBinError(j+1, 0.);
      }

      // Calculate PhiWgt
      double meanFarEast = histFull[k].histFullHar[j].mHistPhiFarEast->Integral() 
	/ (double)Flow::nPhiBins;
      double meanEast = histFull[k].histFullHar[j].mHistPhiEast->Integral() 
	/ (double)Flow::nPhiBins;
      double meanWest = histFull[k].histFullHar[j].mHistPhiWest->Integral() 
	/ (double)Flow::nPhiBins;
      double meanFarWest = histFull[k].histFullHar[j].mHistPhiFarWest->Integral() 
	/ (double)Flow::nPhiBins;
      double meanFtpcEast = histFull[k].histFullHar[j].mHistPhiFtpcEast->Integral() 
	/ (double)Flow::nPhiBinsFtpc;
      double meanFtpcWest = histFull[k].histFullHar[j].mHistPhiFtpcWest->Integral() 
	/ (double)Flow::nPhiBinsFtpc;
      for (int i = 0; i < Flow::nPhiBins; i++) {
	// Tpc
	histFull[k].histFullHar[j].mHistPhiWgtFarEast->SetBinContent(i+1,meanFarEast);
	histFull[k].histFullHar[j].mHistPhiWgtFarEast->SetBinError(i+1, 0.);
	histFull[k].histFullHar[j].mHistPhiWgtEast->SetBinContent(i+1, meanEast);
	histFull[k].histFullHar[j].mHistPhiWgtEast->SetBinError(i+1, 0.);
	histFull[k].histFullHar[j].mHistPhiWgtWest->SetBinContent(i+1, meanWest);
	histFull[k].histFullHar[j].mHistPhiWgtWest->SetBinError(i+1, 0.);
	histFull[k].histFullHar[j].mHistPhiWgtFarWest->SetBinContent(i+1,meanFarWest);
	histFull[k].histFullHar[j].mHistPhiWgtFarWest->SetBinError(i+1, 0.);
      }
      for (int i = 0; i < Flow::nPhiBinsFtpc; i++) {
	// Ftpc
	histFull[k].histFullHar[j].mHistPhiWgtFtpcEast->SetBinContent(i+1, meanFtpcEast);
	histFull[k].histFullHar[j].mHistPhiWgtFtpcEast->SetBinError(i+1, 0.);
	histFull[k].histFullHar[j].mHistPhiWgtFtpcWest->SetBinContent(i+1, meanFtpcWest);
	histFull[k].histFullHar[j].mHistPhiWgtFtpcWest->SetBinError(i+1, 0.);
      }
      // Tpc
      histFull[k].histFullHar[j].mHistPhiWgtFarEast->
	Divide(histFull[k].histFullHar[j].mHistPhiFarEast);
      phiWgtHistNames->AddLast(histFull[k].histFullHar[j].mHistPhiWgtFarEast);
      histFull[k].histFullHar[j].mHistPhiWgtEast->
	Divide(histFull[k].histFullHar[j].mHistPhiEast);
      phiWgtHistNames->AddLast(histFull[k].histFullHar[j].mHistPhiWgtEast);
      histFull[k].histFullHar[j].mHistPhiWgtWest->
	Divide(histFull[k].histFullHar[j].mHistPhiWest);
      phiWgtHistNames->AddLast(histFull[k].histFullHar[j].mHistPhiWgtWest);
      histFull[k].histFullHar[j].mHistPhiWgtFarWest->
	Divide(histFull[k].histFullHar[j].mHistPhiFarWest);
      phiWgtHistNames->AddLast(histFull[k].histFullHar[j].mHistPhiWgtFarWest);
      // Ftpc
      histFull[k].histFullHar[j].mHistPhiWgtFtpcEast->
	Divide(histFull[k].histFullHar[j].mHistPhiFtpcEast);
      phiWgtHistNames->AddLast(histFull[k].histFullHar[j].mHistPhiWgtFtpcEast);
      histFull[k].histFullHar[j].mHistPhiWgtFtpcWest->
	Divide(histFull[k].histFullHar[j].mHistPhiFtpcWest);
      phiWgtHistNames->AddLast(histFull[k].histFullHar[j].mHistPhiWgtFtpcWest);
    }
  }
  //GetHistList()->ls();

  // Write all histograms
  TFile histFile("flow.hist.root", "RECREATE");
  GetHistList()->Write();
  histFile.Close();
  
  // Write PhiWgt histograms
  TFile phiWgtNewFile("flowPhiWgtNew.hist.root", "RECREATE");
  phiWgtHistNames->Write();
  phiWgtNewFile.Close();
  delete phiWgtHistNames;

  // Print the selection object details
  pFlowSelect->PrintList();

  delete pFlowSelect;

  cout << endl;
  gMessMgr->Summary(3);
  cout << endl;

  return StMaker::Finish();
}

////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowAnalysisMaker.cxx,v $
// Revision 1.59  2001/12/18 19:27:06  posk
// "proton" and "antiproton" replaced by "pr+" and "pr-".
//
// Revision 1.58  2001/12/11 22:03:41  posk
// Four sets of phiWgt histograms.
// StFlowMaker StFlowEvent::PhiWeight() changes.
// Cumulant histogram names changed.
//
// Revision 1.57  2001/11/13 22:47:17  posk
// Documentation updated. Fit to q function moved to macro.
//
// Revision 1.56  2001/11/10 01:09:05  posk
// Moved some constants into StFlowConstants.
//
// Revision 1.55  2001/11/09 21:14:33  posk
// Switched from CERNLIB to TMath. Using global dca instead of dca.
//
// Revision 1.54  2001/08/02 20:42:01  snelling
// removed hist title conflict
//
// Revision 1.53  2001/08/02 17:41:49  snelling
// Added trigger histogram
//
// Revision 1.52  2001/05/22 20:10:55  posk
// Changed dEdx graphs.
//
// Revision 1.51  2001/04/25 17:43:24  perev
// HPcorrs
//
// Revision 1.50  2001/04/03 17:46:06  oldi
// Bug fix that excluded FTPC tracks from the determination of the reaction plane.
//
// Revision 1.49  2000/12/12 15:01:10  posk
// Put log comments at end of file.
//
// Revision 1.48  2000/12/10 02:02:01  oldi
// A new member (StTrackTopologyMap mTopology) was added to StFlowPicoTrack.
// The evaluation of either a track originates from the FTPC or not is
// unambiguous now. The evaluation itself is easily extendible for other
// detectors (e.g. SVT+TPC). Old flowpicoevent.root files are treated as if
// they contain TPC tracks only (backward compatibility).
//
// Revision 1.47  2000/12/08 17:04:09  oldi
// Phi weights for both FTPCs included.
//
// Revision 1.43  2000/09/22 22:01:38  posk
// Doubly integrated v now contains resolution error.
//
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
// Revision 1.15  2000/01/14 01:35:52  snelling
// changed include path ../FlowMaker/ to FlowMaker/
//
// Revision 1.14  2000/01/14 01:13:34  snelling
// modified spt (sum pt) to mpt (mean pt) because FlowTag changed
//
// Revision 1.11  1999/12/04 00:15:39  posk
// Works with StFlowEvent which works with the new StEvent
//
// Revision 1.10  1999/11/24 18:14:05  posk
// Now reads event quantities with StFlowEvent methods
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
////////////////////////////////////////////////////////////////////////////
