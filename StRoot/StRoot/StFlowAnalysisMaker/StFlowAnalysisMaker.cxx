////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowAnalysisMaker.cxx,v 1.103 2011/07/25 15:54:42 posk Exp $
//
// Authors: Raimond Snellings and Art Poskanzer, LBNL, Aug 1999
//          FTPC added by Markus Oldenburg, MPI, Dec 2000
//
////////////////////////////////////////////////////////////////////////////
//
// Description:  Maker to analyze Flow using StFlowEvent
//
////////////////////////////////////////////////////////////////////////////

#include <Stiostream.h>
#include <stdlib.h>
#include <math.h>
#include "StMaker.h"
#include "StFlowAnalysisMaker.h"
#include "StFlowMaker/StFlowMaker.h"
#include "StFlowMaker/StFlowEvent.h"
#include "StFlowMaker/StFlowConstants.h"
#include "StFlowMaker/StFlowSelection.h"
#include "StFlowMaker/StFlowCutEvent.h"
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
#include "TOrdCollection.h"
#include "StMessMgr.h"
#include "TMath.h"
#include "TText.h"
#include "TF1.h"
#define PR(x) cout << "##### FlowAnalysis: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowAnalysisMaker)

//-----------------------------------------------------------------------

StFlowAnalysisMaker::StFlowAnalysisMaker(const Char_t* name): StMaker(name),
  MakerName(name) {
  pFlowSelect = new StFlowSelection();
  SetHistoRanges();
  SetPtRange_for_vEta(0., 0.);
  SetEtaRange_for_vPt(0., 0.);
}

StFlowAnalysisMaker::StFlowAnalysisMaker(const Char_t* name,
					 const StFlowSelection& flowSelect) :
  StMaker(name), MakerName(name) {
  pFlowSelect = new StFlowSelection(flowSelect); //copy constructor
  SetHistoRanges();
  SetPtRange_for_vEta(0., 0.);
  SetEtaRange_for_vPt(0., 0.);
}

//-----------------------------------------------------------------------

StFlowAnalysisMaker::~StFlowAnalysisMaker() {
}

//-----------------------------------------------------------------------

Int_t StFlowAnalysisMaker::Make() {
  // Make histograms

  // Get another pointer to StFlowEvent
  pFlowMaker = NULL;
  pFlowMaker = (StFlowMaker*)GetMaker("Flow");
  if (pFlowMaker) pFlowEvent = pFlowMaker->FlowEventPointer();
  if (pFlowEvent && pFlowSelect->Select(pFlowEvent)) {     // event selected
    if (FillFromFlowEvent()) {               // get event quantities
      FillEventHistograms();                 // fill from FlowEvent
      FillParticleHistograms();              // fill particle histograms
    } else {
      gMessMgr->Info("##### FlowAnalysis: Event psi = 0");
    }
  } else {
    gMessMgr->Info("##### FlowAnalysis: FlowEvent pointer null");
    return kStOK;
  }// selected events
    
  if (Debug()) StMaker::PrintInfo();
  
  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StFlowAnalysisMaker::Init() {
  // Book histograms

  StFlowMaker* pFlowMaker = NULL;
  pFlowMaker = (StFlowMaker*)GetMaker("Flow");
  Bool_t reCentCalc = pFlowMaker->ReCentCalc();

  float ptMaxPart = Flow::ptMaxPart;
  if (pFlowSelect->PtMaxPart()) {
    ptMaxPart = pFlowSelect->PtMaxPart();
  }
  int nPtBinsPart = Flow::nPtBinsPart;
  if (pFlowSelect->PtBinsPart()) {
    nPtBinsPart = pFlowSelect->PtBinsPart();
  }
  xLabel = "Pseudorapidity";
  if (strlen(pFlowSelect->PidPart()) != 0) { xLabel = "Rapidity"; }

  const float triggerMin      =  -0.5;
  const float triggerMax      =  10.5;
  const float chargeMin       =  -2.5;
  const float chargeMax       =   2.5; 
  const float dcaMin          =    0.;
  const float dcaMax          =   0.3;
  const float glDcaMax        =   3.6; 
  const float chi2Min         =    0.;
  const float chi2Max         =    5.; 
  const float fitPtsMinTpc    =  -0.5;
  const float fitPtsMaxTpc    =  60.5; 
  const float maxPtsMinTpc    =  -0.5;
  const float maxPtsMaxTpc    =  60.5; 
  const float fitPtsMinFtpc   =  -0.5;
  const float fitPtsMaxFtpc   =  12.5; 
  const float maxPtsMinFtpc   =  -0.5;
  const float maxPtsMaxFtpc   =  12.5; 
  const float fitOverMaxMin   =    0.;
  const float fitOverMaxMax   =   1.2; 
  const float origMultMin     =    0.;
  const float origMultMax     = 3000.; 
  const float MultEtaMin      =    0.;
  const float MultEtaMax      = 1000.; 
  const float totalMultMin    =    0.;
  const float totalMultMax    = 2000.; 
  const float corrMultMin     =    0.;
  const float corrMultMax     = 2000.; 
  const float multOverOrigMin =    0.;
  const float multOverOrigMax =    1.; 
  const float vertexZMin      =-100.5;
  const float vertexZMax      = 100.5; 
  const float vertexXYMin     =   -1.;
  const float vertexXYMax     =    1.; 
  const float QXYMin          =  -0.5;
  const float QXYMax          =   0.5; 
  const float etaSymZMin      = -1.15; 
  const float etaSymZMax      =  1.15; 
  const float etaSymMin       =   -6.; 
  const float etaSymMax       =    6.; 
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
	 nFitPtsBinsFtpc   = 13,
	 nMaxPtsBinsTpc    = 61,
	 nMaxPtsBinsFtpc   = 13,
	 nFitOverMaxBins   = 40,
	 nOrigMultBins     = 60,
	 nMultEtaBins      = 50,
	 nTotalMultBins    = 40,
	 nMultOverOrigBins = 50,
	 nMultPartBins     = 40,
	 nVertexZBins      = 51,
	 nVertexXYBins     = 50,
	 nQXYBins          = 50,
	 nEtaSymBins       = 45,
	 nPhi3DBins        = 18,
	 nPsiBins          = 36,
	 nMultBins         = 40,
	 nPidBins          = 50,
         nCentBins         = 10,
	 nDedxBins        = 200,
	 nMomenBins       = 200,
	 n_qBins          =  50
  };

  // Trigger
  mHistTrigger = new TH1F("Flow_Trigger", "Flow_Trigger",
      nTriggerBins, triggerMin, triggerMax);
  mHistTrigger->SetXTitle("Trig: 0 mb+cen, 1 mb, 2 central, 3 laser, 10 other");
  mHistTrigger->SetYTitle("Counts");

  // Charge
  // Ftpc
  mHistChargeFtpc = new TH1F("Flow_Charge_Ftpc", "Flow_Charge_Ftpc",
      nChargeBins, chargeMin, chargeMax);
  mHistChargeFtpc->SetXTitle("Charge");
  mHistChargeFtpc->SetYTitle("Counts");
    
  // Distance of closest approach
  // Tpc
  mHistDcaTpc = new TH1F("Flow_Dca_Tpc", "Flow_Dca_Tpc",
      nDcaBins, dcaMin, dcaMax);
  mHistDcaTpc->SetXTitle("Track dca to Vertex (cm)");
  mHistDcaTpc->SetYTitle("Counts");

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
  mHistMultEta->SetXTitle("Mult for Centrality");
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
    
  // EtaSym vs. Vertex Z Tpc
  mHistEtaSymVerZ2DTpc = new TH2F("Flow_EtaSymVerZ2D_Tpc", "Flow_EtaSymVerZ2D_Tpc",
    nVertexZBins, vertexZMin, vertexZMax, nEtaSymBins, etaSymZMin, etaSymZMax);
  mHistEtaSymVerZ2DTpc->SetXTitle("Vertex Z (cm)");
  mHistEtaSymVerZ2DTpc->SetYTitle("Eta Symmetry");

  // EtaSym vs. Vertex Z Ftpc
  mHistEtaSymVerZ2DFtpc = new TH2F("Flow_EtaSymVerZ2D_Ftpc", "Flow_EtaSymVerZ2D_Ftpc",
    nVertexZBins, vertexZMin, vertexZMax, nEtaSymBins, etaSymZMin, etaSymZMax);
  mHistEtaSymVerZ2DFtpc->SetXTitle("Vertex Z (cm)");
  mHistEtaSymVerZ2DFtpc->SetYTitle("Eta Symmetry");

  // EtaSym Tpc
  mHistEtaSymTpc = new TH1F("Flow_EtaSym_Tpc", "Flow_EtaSym_Tpc",
      nEtaSymBins, etaSymMin, etaSymMax);
  mHistEtaSymTpc->SetXTitle("Eta Symmetry Ratio TPC");
  mHistEtaSymTpc->SetYTitle("Counts");
    
  // EtaSym Ftpc
  mHistEtaSymFtpc = new TH1F("Flow_EtaSym_Ftpc", "Flow_EtaSym_Ftpc",
      nEtaSymBins, etaSymMin, etaSymMax);
  mHistEtaSymFtpc->SetXTitle("Eta Symmetry Ratio FTPC");
  mHistEtaSymFtpc->SetYTitle("Counts");
    
  // EtaPtPhi
  mHistEtaPtPhi3D = new TH3F("Flow_EtaPtPhi3D", "Flow_EtaPtPhi3D",
      mNEtaBins, mEtaMin, mEtaMax, Flow::nPtBins, Flow::ptMin, 
			     Flow::ptMax, nPhi3DBins, phiMin, phiMax);
  mHistEtaPtPhi3D->SetXTitle("Eta");
  mHistEtaPtPhi3D->SetYTitle("Pt (GeV/c)");
  mHistEtaPtPhi3D->SetZTitle("Phi (rad)");
    
  // Yield for all particles
  mHistYieldAll2D = new TH2D("Flow_YieldAll2D", "Flow_YieldAll2D",
    mNEtaBins, mEtaMin, mEtaMax, Flow::nPtBins, Flow::ptMin, 
			     Flow::ptMax);
  mHistYieldAll2D->Sumw2();
  mHistYieldAll2D->SetXTitle("Pseudorapidty");
  mHistYieldAll2D->SetYTitle("Pt (GeV/c)");

  // Yield for particles correlated with the event plane
  mHistYieldPart2D = new TH2D("Flow_YieldPart2D", "Flow_YieldPart2D",
    mNEtaBins, mEtaMin, mEtaMax, nPtBinsPart, Flow::ptMin, 
			      ptMaxPart);
  mHistYieldPart2D->Sumw2();
  mHistYieldPart2D->SetXTitle((char*)xLabel.Data());
  mHistYieldPart2D->SetYTitle("Pt (GeV/c)");

  // Mean Eta in each bin
  mHistBinEta = new TProfile("Flow_Bin_Eta", "Flow_Bin_Eta",
    mNEtaBins, mEtaMin, mEtaMax, mEtaMin, mEtaMax, "");
  mHistBinEta->SetXTitle((char*)xLabel.Data());
  mHistBinEta->SetYTitle("<Eta>");
  
  // Mean Pt in each bin
  mHistBinPt = new TProfile("Flow_Bin_Pt", "Flow_Bin_Pt",
    nPtBinsPart, Flow::ptMin, ptMaxPart, Flow::ptMin, ptMaxPart, "");
  mHistBinPt->SetXTitle("Pt (GeV/c)");
  mHistBinPt->SetYTitle("<Pt> (GeV/c)");
      
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
			      13, 0.5, 13.5, 0., 10000., "");
  mHistPidMult->SetXTitle("all, h+, h-, pi+, pi-, pr+, pr-, K+, K-, d+, d-, e-, e+");
  mHistPidMult->SetYTitle("Multiplicity");
    
  // Centrality
  mHistCent = new TH1F("Flow_Cent", "Flow_Cent",
		       nCentBins, centMin, centMax);
  mHistCent->SetXTitle("Centrality Bin");
  mHistCent->SetYTitle("Counts");
    
  // CTB versus ZDC
  mHistCTBvsZDC2D = new TH2F("Flow_CTBvsZDC2D", "Flow_CTBvsZDC2D",
			       125, 0, 500,
			       125, 0, 40000);
  mHistCTBvsZDC2D->SetXTitle("ZDC sum");
  mHistCTBvsZDC2D->SetYTitle("CTB sum");

  // MeanDedxPos
  mHistMeanDedxPos2D = new TH2F("Flow_MeanDedxPos2D",
				"Flow_MeanDedxPos2D",
				nMomenBins, pMin, pMax,
				nDedxBins, 0, dEdxMax);
  mHistMeanDedxPos2D->SetXTitle("log(momentum) (GeV/c)");
  mHistMeanDedxPos2D->SetYTitle("mean dEdx");
  
  // MeanDedxNeg
  mHistMeanDedxNeg2D = new TH2F("Flow_MeanDedxNeg2D",
				"Flow_MeanDedxNeg2D",
				nMomenBins, pMin, pMax,
				nDedxBins, 0, dEdxMax);
  mHistMeanDedxNeg2D->SetXTitle("log(momentum) (GeV/c)");
  mHistMeanDedxNeg2D->SetYTitle("mean dEdx");
  
  // MeanDedx PiPlus
  mHistMeanDedxPiPlus2D = new TH2F("Flow_MeanDedxPiPlus2D", 
				   "Flow_MeanDedxPiPlus2D",
				   nMomenBins, pMin, pMax,
				   nDedxBins, 0, dEdxMax);
  mHistMeanDedxPiPlus2D->SetXTitle("log(momentum) (GeV/c)");
  mHistMeanDedxPiPlus2D->SetYTitle("mean dEdx");
  
  // MeanDedxPiMinus
  mHistMeanDedxPiMinus2D = new TH2F("Flow_MeanDedxPiMinus2D", 
				    "Flow_MeanDedxPiMinus2D",
				    nMomenBins, pMin, pMax,
				    nDedxBins, 0, dEdxMax);
  mHistMeanDedxPiMinus2D->SetXTitle("log(momentum) (GeV/c)");
  mHistMeanDedxPiMinus2D->SetYTitle("mean dEdx");
  
  // MeanDedxProton
  mHistMeanDedxProton2D = new TH2F("Flow_MeanDedxProton2D", 
				   "Flow_MeanDedxProton2D",
				   nMomenBins, pMin, pMax,
				   nDedxBins, 0, dEdxMax);
  mHistMeanDedxProton2D->SetXTitle("log(momentum) (GeV/c)");
  mHistMeanDedxProton2D->SetYTitle("mean dEdx");
  
  // MeanDedxPbar
  mHistMeanDedxPbar2D = new TH2F("Flow_MeanDedxPbar2D", 
				 "Flow_MeanDedxPbar2D",
				 nMomenBins, pMin, pMax,
				 nDedxBins, 0, dEdxMax);
  mHistMeanDedxPbar2D->SetXTitle("log(momentum) (GeV/c)");
  mHistMeanDedxPbar2D->SetYTitle("mean dEdx");
  
  // MeanDedxKplus
  mHistMeanDedxKplus2D = new TH2F("Flow_MeanDedxKplus2D", 
				  "Flow_MeanDedxKplus2D",
				  nMomenBins, pMin, pMax,
				  nDedxBins, 0, dEdxMax);
  mHistMeanDedxKplus2D->SetXTitle("log(momentum) (GeV/c)");
  mHistMeanDedxKplus2D->SetYTitle("mean dEdx");
  
  // MeanDedxKminus
  mHistMeanDedxKminus2D = new TH2F("Flow_MeanDedxKminus2D", 
				   "Flow_MeanDedxKminus2D",
				   nMomenBins, pMin, pMax,
				   nDedxBins, 0, dEdxMax);
  mHistMeanDedxKminus2D->SetXTitle("log(momentum) (GeV/c)");
  mHistMeanDedxKminus2D->SetYTitle("mean dEdx");
  
  // MeanDedxDeuteron
  mHistMeanDedxDeuteron2D = new TH2F("Flow_MeanDedxDeuteron2D", 
				     "Flow_MeanDedxDeuteron2D",
				     nMomenBins, pMin, pMax,
				     nDedxBins, 0, dEdxMax);
  mHistMeanDedxDeuteron2D->SetXTitle("log(momentum) (GeV/c)");
  mHistMeanDedxDeuteron2D->SetYTitle("mean dEdx");

  // MeanDedxAntiDeuteron
  mHistMeanDedxAntiDeuteron2D = new TH2F("Flow_MeanDedxAntiDeuteron2D", 
					 "Flow_MeanDedxAntiDeuteron2D",
					 nMomenBins, pMin, pMax,
					 nDedxBins, 0, dEdxMax);
  mHistMeanDedxAntiDeuteron2D->SetXTitle("log(momentum) (GeV/c)");
  mHistMeanDedxAntiDeuteron2D->SetYTitle("mean dEdx");
  
  // MeanDedxElectron
  mHistMeanDedxElectron2D = new TH2F("Flow_MeanDedxElectron2D", 
				     "Flow_MeanDedxElectron2D",
				     nMomenBins, pMin, pMax,
				     nDedxBins, 0, dEdxMax);
  mHistMeanDedxElectron2D->SetXTitle("log(momentum) (GeV/c)");
  mHistMeanDedxElectron2D->SetYTitle("mean dEdx");
  
  // MeanDedxPositron
  mHistMeanDedxPositron2D = new TH2F("Flow_MeanDedxPositron2D", 
				     "Flow_MeanDedxPositron2D",
				     nMomenBins, pMin, pMax,
				     nDedxBins, 0, dEdxMax);
  mHistMeanDedxPositron2D->SetXTitle("log(momentum) (GeV/c)");
  mHistMeanDedxPositron2D->SetYTitle("mean dEdx");
  
  // ZDCSMD test
  mZDC_SMD_west_vert = new TH1F("Flow_ZDC_SMD_west_vert","Flow_ZDC_SMD_west_vert",7,0.5,7.5);
  mZDC_SMD_east_vert = new TH1F("Flow_ZDC_SMD_east_vert","Flow_ZDC_SMD_east_vert",7,0.5,7.5);
  mZDC_SMD_west_hori = new TH1F("Flow_ZDC_SMD_west_hori","Flow_ZDC_SMD_west_hori",8,0.5,8.5);
  mZDC_SMD_east_hori = new TH1F("Flow_ZDC_SMD_east_hori","Flow_ZDC_SMD_east_hori",8,0.5,8.5);
  mHistZDCSMDPsiWgtEast = new TH1D("Flow_ZDCSMDPsiWgtEast","Flow_ZDCSMDPsiWgtEast",
				   Flow::zdcsmd_nPsiBins,-twopi/2.,twopi/2.);
  mHistZDCSMDPsiWgtWest = new TH1D("Flow_ZDCSMDPsiWgtWest","Flow_ZDCSMDPsiWgtWest",
				   Flow::zdcsmd_nPsiBins,-twopi/2.,twopi/2.);
  mHistZDCSMDPsiWgtTest = new TH1D("Flow_ZDCSMDPsiWgtTest","Flow_ZDCSMDPsiWgtTest",
                                   Flow::zdcsmd_nPsiBins,0.,twopi);
  mHistZDCSMDPsiWgtFull = new TH1D("Flow_ZDCSMDPsiWgtFull","Flow_ZDCSMDPsiWgtFull",
                                   Flow::zdcsmd_nPsiBins,0.,twopi);
  mHistZDCSMDPsiCorTest = new TH1D("Flow_ZDCSMDPsiCorTest","Flow_ZDCSMDPsiCorTest",
                                   Flow::zdcsmd_nPsiBins,-twopi/2.,twopi/2.);
  mHistZDCSMDPsiCorFull = new TH1D("Flow_ZDCSMDPsiCorFull","Flow_ZDCSMDPsiWgtFull",
                                   Flow::zdcsmd_nPsiBins,-twopi/2.,twopi/2.);
  TString* histTitle;
  for (int i = 0; i < Flow::nSels * Flow::nSubs; i++) {

    // for sub-events
    for (int j = 0; j < Flow::nHars; j++) {
      float order = (float)(j + 1);

      // event planes
      histTitle = new TString("Flow_Psi_Subs");
      *histTitle += i+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histSub[i].histSubHar[j].mHistPsiSubs = new TH1F(histTitle->Data(),
	histTitle->Data(), nPsiBins, psiMin, (psiMax / order));
      histSub[i].histSubHar[j].mHistPsiSubs->Sumw2();
      histSub[i].histSubHar[j].mHistPsiSubs->SetXTitle
	("Event Plane Angle (rad)");
      histSub[i].histSubHar[j].mHistPsiSubs->SetYTitle("Counts");
      delete histTitle;
    }
  }

  for (int k = 0; k < Flow::nSels; k++) {

    // for each selection

    // cos(n*delta_Psi)
    histTitle = new TString("Flow_Cos_Sel");
    *histTitle += k+1;
    histFull[k].mHistCos = new TProfile(histTitle->Data(), histTitle->Data(),
      Flow::nHars, 0.5, (float)(Flow::nHars) + 0.5, -1., 1., "");
    histFull[k].mHistCos->SetXTitle("Harmonic");
    histFull[k].mHistCos->SetYTitle("<cos(n*delta_Psi)>");
    delete histTitle;
    
    // resolution
    histTitle = new TString("Flow_Res_Sel");
    *histTitle += k+1;
    histFull[k].mHistRes = new TH1F(histTitle->Data(), histTitle->Data(),
      Flow::nHars, 0.5, (float)(Flow::nHars) + 0.5);
    histFull[k].mHistRes->SetXTitle("Harmonic");
    histFull[k].mHistRes->SetYTitle("Resolution");
    delete histTitle;

    // vObs
    histTitle = new TString("Flow_vObs_Sel");
    *histTitle += k+1;
    histFull[k].mHist_vObs = new TProfile(histTitle->Data(), histTitle->Data(),
      Flow::nHars, 0.5, (float)(Flow::nHars) + 0.5, -1000., 1000., "");
    histFull[k].mHist_vObs->SetXTitle("Harmonic");
    histFull[k].mHist_vObs->SetYTitle("vObs (%)");
    delete histTitle;
    
    // for each harmonic
    for (int j = 0; j < Flow::nHars; j++) {
      float order  = (float)(j+1);

      // multiplicity
      histTitle = new TString("Flow_Mul_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHistMult = new TH1F(histTitle->Data(),
        histTitle->Data(), nMultBins, multMin, multMax);
      histFull[k].histFullHar[j].mHistMult->SetXTitle("Multiplicity");
      histFull[k].histFullHar[j].mHistMult->SetYTitle("Counts");
      delete histTitle;

      // event plane
      histTitle = new TString("Flow_Psi_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHistPsi = new TH1F(histTitle->Data(),
        histTitle->Data(), nPsiBins, psiMin, psiMax / order);
      histFull[k].histFullHar[j].mHistPsi->Sumw2();
      histFull[k].histFullHar[j].mHistPsi->SetXTitle
	("Event Plane Angle (rad)");
      histFull[k].histFullHar[j].mHistPsi->SetYTitle("Counts");
      delete histTitle;
      
      // phi lab
      histTitle = new TString("Flow_PhiLab_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHistPhiLab = new TH1F(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
      histFull[k].histFullHar[j].mHistPhiLab->SetXTitle("Particle Lab Angle (rad)");
      histFull[k].histFullHar[j].mHistPhiLab->SetYTitle("Counts");
      histFull[k].histFullHar[j].mHistPhiLab->Sumw2(); // for scale
      delete histTitle;
      
      // Recenter
      histTitle = new TString("FlowReCentX_Sel");
      *histTitle += k+1;
      *histTitle += "_Har";
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHistReCentX = new TProfile(histTitle->Data(),
        histTitle->Data(), 3, 0.5, 3.5);
      histFull[k].histFullHar[j].mHistReCentX->SetXTitle("FTPCE, FTPCW, TPCE, TPCW");
      histFull[k].histFullHar[j].mHistReCentX->SetYTitle("<cos n #phi>");
      delete histTitle;
    
      histTitle = new TString("FlowReCentY_Sel");
      *histTitle += k+1;
      *histTitle += "_Har";
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHistReCentY = new TProfile(histTitle->Data(),
        histTitle->Data(), 3, 0.5, 3.5);
      histFull[k].histFullHar[j].mHistReCentY->SetXTitle("FTPCE, FTPCW, TPCE, TPCW");
      histFull[k].histFullHar[j].mHistReCentY->SetYTitle("<sin n #phi>");
      delete histTitle;
    
      // Recentered, for printing, not plotting
       histTitle = new TString("FlowQreCent_Sel");
      *histTitle += k+1;
      *histTitle += "_Har";
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHistQreCent = new TProfile(histTitle->Data(),
        histTitle->Data(), 2, 0.5, 2.5);
      histFull[k].histFullHar[j].mHistQreCent->SetXTitle("X, Y");
      histFull[k].histFullHar[j].mHistQreCent->SetYTitle("<Q_{n}/M>");

      // QXY
      histTitle = new TString("Flow_QXY2D_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHistQXY2D = new TH2D(histTitle->Data(), histTitle->Data(),
			    nQXYBins, QXYMin, QXYMax, nQXYBins, QXYMin, QXYMax);
      histFull[k].histFullHar[j].mHistQXY2D->SetXTitle("Q_X/M");
      histFull[k].histFullHar[j].mHistQXY2D->SetYTitle("Q_Y/M");
      delete histTitle;
    
      // QSubXY for k=0 subevents
      histTitle = new TString("Flow_QFTPCSubXY2D_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHistQFTPCSubXY2D = new TH2D(histTitle->Data(), histTitle->Data(),
			    nQXYBins, QXYMin*2., QXYMax*2., nQXYBins, QXYMin*2., QXYMax*2.);
      histFull[k].histFullHar[j].mHistQFTPCSubXY2D->SetXTitle("QSub_X/M");
      histFull[k].histFullHar[j].mHistQFTPCSubXY2D->SetYTitle("QSub_Y/M");
      delete histTitle;
    
      // QSubXY for k=1 subevents
      histTitle = new TString("Flow_QTPCSubXY2D_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHistQTPCSubXY2D = new TH2D(histTitle->Data(), histTitle->Data(),
			    nQXYBins, QXYMin*2., QXYMax*2., nQXYBins, QXYMin*2., QXYMax*2.);
      histFull[k].histFullHar[j].mHistQTPCSubXY2D->SetXTitle("QSub_X/M");
      histFull[k].histFullHar[j].mHistQTPCSubXY2D->SetYTitle("QSub_Y/M");
      delete histTitle;
 
      // event plane difference of two selections
      histTitle = new TString("Flow_Psi_Diff_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      
      if (k == 0 ) {
	Int_t my_order = 1;
	if (j == 1) {
	  my_order = 2;
	}
	histFull[k].histFullHar[j].mHistPsi_Diff = new TH1F(histTitle->Data(),
	     histTitle->Data(), nPsiBins, -psiMax/my_order/2., psiMax/my_order/2.);
      } else {
	histFull[k].histFullHar[j].mHistPsi_Diff = new TH1F(histTitle->Data(),	
		          histTitle->Data(), nPsiBins, -psiMax/2., psiMax/2.);
      }
      
      if (k == 0) {
	if (j == 0) {
	  histFull[k].histFullHar[j].mHistPsi_Diff->SetXTitle
	    ("#Psi_{1,Sel1} - #Psi_{1,Sel2}(rad)");
	} else if (j == 1) {
	  histFull[k].histFullHar[j].mHistPsi_Diff->SetXTitle
	    ("#Psi_{2,Sel1} - #Psi_{2,Sel2}(rad)");
	}
      } else if (k == 1) {
	if (j == 0) {
	  histFull[k].histFullHar[j].mHistPsi_Diff->SetXTitle
	    ("#Psi_{1,Sel1} - #Psi_{2,Sel2}(rad)");
	} else if (j == 1) {
	  histFull[k].histFullHar[j].mHistPsi_Diff->SetXTitle
	    ("#Psi_{1,Sel1} - #Psi_{2,Sel1}(rad)");
	}
      }

      histFull[k].histFullHar[j].mHistPsi_Diff->SetYTitle("Counts");
      delete histTitle;

      // correlation of sub-event planes
      histTitle = new TString("Flow_Psi_Sub_Corr_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHistPsiSubCorr = new TH1F(histTitle->Data(),
        histTitle->Data(), nPsiBins, psiMin, psiMax / order);
      histFull[k].histFullHar[j].mHistPsiSubCorr->Sumw2();
      histFull[k].histFullHar[j].mHistPsiSubCorr->SetXTitle
	("Sub-Event Correlation (rad)");
      histFull[k].histFullHar[j].mHistPsiSubCorr->SetYTitle("Counts");
      delete histTitle;
      
      // correlation of sub-event planes of different order
      histTitle = new TString("Flow_Psi_Sub_Corr_Diff_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
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
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHist_q = new TH1F(histTitle->Data(),
        histTitle->Data(), n_qBins, qMin, qMax);
      histFull[k].histFullHar[j].mHist_q->Sumw2();
      histFull[k].histFullHar[j].mHist_q->SetXTitle("q = |Q|/sqrt(Mult)");
      histFull[k].histFullHar[j].mHist_q->SetYTitle("Counts");
      delete histTitle;
      
      // particle-plane azimuthal correlation
      histTitle = new TString("Flow_Phi_Corr_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHistPhiCorr =	new TH1F(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBins, phiMin, phiMax / order);
      histFull[k].histFullHar[j].mHistPhiCorr->Sumw2();
      histFull[k].histFullHar[j].mHistPhiCorr->
	SetXTitle("Particle-Plane Correlation (rad)");
      histFull[k].histFullHar[j].mHistPhiCorr->SetYTitle("Counts");
      delete histTitle;

      // Yield(eta,pt)
      histTitle = new TString("Flow_Yield2D_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHistYield2D =	new TH2D(histTitle->Data(),
        histTitle->Data(), mNEtaBins, mEtaMin, mEtaMax,
			   Flow::nPtBins, Flow::ptMin, Flow::ptMax);
      histFull[k].histFullHar[j].mHistYield2D->Sumw2();
      histFull[k].histFullHar[j].mHistYield2D->SetXTitle("Pseudorapidty");
      histFull[k].histFullHar[j].mHistYield2D->SetYTitle("Pt (GeV/c)");
      delete histTitle;

      // Flow observed
      histTitle = new TString("Flow_vObs2D_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHist_vObs2D =	new TProfile2D(histTitle->Data(),
        histTitle->Data(), mNEtaBins, mEtaMin, mEtaMax, nPtBinsPart, 
		 Flow::ptMin, ptMaxPart, -1000., 1000., "");
      histFull[k].histFullHar[j].mHist_vObs2D->SetXTitle((char*)xLabel.Data());
      histFull[k].histFullHar[j].mHist_vObs2D->SetYTitle("Pt (GeV/c)");
      delete histTitle;

      // Flow observed profiles
      histTitle = new TString("Flow_vObsEta_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHist_vObsEta = new TProfile(histTitle->Data(),
        histTitle->Data(), mNEtaBins, mEtaMin, mEtaMax, -1000., 1000., "");
      histFull[k].histFullHar[j].mHist_vObsEta->SetXTitle((char*)xLabel.Data());
      histFull[k].histFullHar[j].mHist_vObsEta->SetYTitle("v (%)");
      delete histTitle;

      histTitle = new TString("Flow_vObsPt_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHist_vObsPt = new TProfile(histTitle->Data(),
        histTitle->Data(), nPtBinsPart, Flow::ptMin, ptMaxPart, -1000., 1000., "");
      histFull[k].histFullHar[j].mHist_vObsPt->SetXTitle("Pt (GeV/c)");
      histFull[k].histFullHar[j].mHist_vObsPt->SetYTitle("v (%)");
      delete histTitle;
    }

    // for two harmonics
    for (int j = 0; j < 2; j++) {

      // Phi lab
      // Tpc (FarEast)
      histTitle = new TString("Flow_Phi_FarEast_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histTwoHar[j].mHistPhiFarEast = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
      histFull[k].histTwoHar[j].mHistPhiFarEast->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histTwoHar[j].mHistPhiFarEast->SetYTitle("Counts");
      delete histTitle;
      
      // Tpc (East)
      histTitle = new TString("Flow_Phi_East_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histTwoHar[j].mHistPhiEast = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
      histFull[k].histTwoHar[j].mHistPhiEast->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histTwoHar[j].mHistPhiEast->SetYTitle("Counts");
      delete histTitle;
      
      // Tpc (West)
      histTitle = new TString("Flow_Phi_West_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histTwoHar[j].mHistPhiWest = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
      histFull[k].histTwoHar[j].mHistPhiWest->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histTwoHar[j].mHistPhiWest->SetYTitle("Counts");
      delete histTitle;
      
      // Tpc (FarWest)
      histTitle = new TString("Flow_Phi_FarWest_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histTwoHar[j].mHistPhiFarWest = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
      histFull[k].histTwoHar[j].mHistPhiFarWest->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histTwoHar[j].mHistPhiFarWest->SetYTitle("Counts");
      delete histTitle;
      
      // Ftpc (FarEast)
      histTitle = new TString("Flow_Phi_FtpcFarEast_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histTwoHar[j].mHistPhiFtpcFarEast = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBinsFtpc, phiMin, phiMax);
      histFull[k].histTwoHar[j].mHistPhiFtpcFarEast->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histTwoHar[j].mHistPhiFtpcFarEast->SetYTitle("Counts");
      delete histTitle;
      
      // Ftpc (East)
      histTitle = new TString("Flow_Phi_FtpcEast_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histTwoHar[j].mHistPhiFtpcEast = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBinsFtpc, phiMin, phiMax);
      histFull[k].histTwoHar[j].mHistPhiFtpcEast->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histTwoHar[j].mHistPhiFtpcEast->SetYTitle("Counts");
      delete histTitle;
      
      // Ftpc (West)
      histTitle = new TString("Flow_Phi_FtpcWest_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histTwoHar[j].mHistPhiFtpcWest = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBinsFtpc, phiMin, phiMax);
      histFull[k].histTwoHar[j].mHistPhiFtpcWest->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histTwoHar[j].mHistPhiFtpcWest->SetYTitle("Counts");
      delete histTitle;
      
      // Ftpc (FarWest)
      histTitle = new TString("Flow_Phi_FtpcFarWest_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histTwoHar[j].mHistPhiFtpcFarWest = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBinsFtpc, phiMin, phiMax);
      histFull[k].histTwoHar[j].mHistPhiFtpcFarWest->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histTwoHar[j].mHistPhiFtpcFarWest->SetYTitle("Counts");
      delete histTitle;
      
      // PhiWgt
      // Tpc (FarEast)
      histTitle = new TString("Flow_Phi_Weight_FarEast_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histTwoHar[j].mHistPhiWgtFarEast = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
      histFull[k].histTwoHar[j].mHistPhiWgtFarEast->Sumw2();
      histFull[k].histTwoHar[j].mHistPhiWgtFarEast->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histTwoHar[j].mHistPhiWgtFarEast->SetYTitle("PhiWgt");
      delete histTitle;
      
      // Tpc (East)
      histTitle = new TString("Flow_Phi_Weight_East_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histTwoHar[j].mHistPhiWgtEast = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
      histFull[k].histTwoHar[j].mHistPhiWgtEast->Sumw2();
      histFull[k].histTwoHar[j].mHistPhiWgtEast->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histTwoHar[j].mHistPhiWgtEast->SetYTitle("PhiWgt");
      delete histTitle;
      
      // Tpc (West)
      histTitle = new TString("Flow_Phi_Weight_West_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histTwoHar[j].mHistPhiWgtWest = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
      histFull[k].histTwoHar[j].mHistPhiWgtWest->Sumw2();
      histFull[k].histTwoHar[j].mHistPhiWgtWest->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histTwoHar[j].mHistPhiWgtWest->SetYTitle("PhiWgt");
      delete histTitle;
      
      // Tpc (FarWest)
      histTitle = new TString("Flow_Phi_Weight_FarWest_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histTwoHar[j].mHistPhiWgtFarWest = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
      histFull[k].histTwoHar[j].mHistPhiWgtFarWest->Sumw2();
      histFull[k].histTwoHar[j].mHistPhiWgtFarWest->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histTwoHar[j].mHistPhiWgtFarWest->SetYTitle("PhiWgt");
      delete histTitle;
      
      // Ftpc (FarEast)
      histTitle = new TString("Flow_Phi_Weight_FtpcFarEast_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histTwoHar[j].mHistPhiWgtFtpcFarEast = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBinsFtpc, phiMin, phiMax);
      histFull[k].histTwoHar[j].mHistPhiWgtFtpcFarEast->Sumw2();
      histFull[k].histTwoHar[j].mHistPhiWgtFtpcFarEast->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histTwoHar[j].mHistPhiWgtFtpcFarEast->SetYTitle("PhiWgt");
      delete histTitle;
      
      // Ftpc (East)
      histTitle = new TString("Flow_Phi_Weight_FtpcEast_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histTwoHar[j].mHistPhiWgtFtpcEast = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBinsFtpc, phiMin, phiMax);
      histFull[k].histTwoHar[j].mHistPhiWgtFtpcEast->Sumw2();
      histFull[k].histTwoHar[j].mHistPhiWgtFtpcEast->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histTwoHar[j].mHistPhiWgtFtpcEast->SetYTitle("PhiWgt");
      delete histTitle;
      
      // Ftpc (West)
      histTitle = new TString("Flow_Phi_Weight_FtpcWest_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histTwoHar[j].mHistPhiWgtFtpcWest = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBinsFtpc, phiMin, phiMax);
      histFull[k].histTwoHar[j].mHistPhiWgtFtpcWest->Sumw2();
      histFull[k].histTwoHar[j].mHistPhiWgtFtpcWest->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histTwoHar[j].mHistPhiWgtFtpcWest->SetYTitle("PhiWgt");
      delete histTitle;
      
      // Ftpc (FarWest)
      histTitle = new TString("Flow_Phi_Weight_FtpcFarWest_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histTwoHar[j].mHistPhiWgtFtpcFarWest = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBinsFtpc, phiMin, phiMax);
      histFull[k].histTwoHar[j].mHistPhiWgtFtpcFarWest->Sumw2();
      histFull[k].histTwoHar[j].mHistPhiWgtFtpcFarWest->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histTwoHar[j].mHistPhiWgtFtpcFarWest->SetYTitle("PhiWgt");
      delete histTitle;


      // Phi lab flattened
      // Tpc (FarEast)
      histTitle = new TString("Flow_Phi_Flat_FarEast_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histTwoHar[j].mHistPhiFlatFarEast = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
      histFull[k].histTwoHar[j].mHistPhiFlatFarEast->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histTwoHar[j].mHistPhiFlatFarEast->SetYTitle("Counts");
      delete histTitle;

      // Tpc (East)
      histTitle = new TString("Flow_Phi_Flat_East_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histTwoHar[j].mHistPhiFlatEast = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
      histFull[k].histTwoHar[j].mHistPhiFlatEast->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histTwoHar[j].mHistPhiFlatEast->SetYTitle("Counts");
      delete histTitle;

      // Tpc (West)
      histTitle = new TString("Flow_Phi_Flat_West_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histTwoHar[j].mHistPhiFlatWest = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
      histFull[k].histTwoHar[j].mHistPhiFlatWest->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histTwoHar[j].mHistPhiFlatWest->SetYTitle("Counts");
      delete histTitle;

      // Tpc (FarWest)
      histTitle = new TString("Flow_Phi_Flat_FarWest_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histTwoHar[j].mHistPhiFlatFarWest = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
      histFull[k].histTwoHar[j].mHistPhiFlatFarWest->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histTwoHar[j].mHistPhiFlatFarWest->SetYTitle("Counts");
      delete histTitle;

      // Ftpc (FarEast)
      histTitle = new TString("Flow_Phi_Flat_FtpcFarEast_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histTwoHar[j].mHistPhiFlatFtpcFarEast = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBinsFtpc, phiMin, phiMax);
      histFull[k].histTwoHar[j].mHistPhiFlatFtpcFarEast->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histTwoHar[j].mHistPhiFlatFtpcFarEast->SetYTitle("Counts");
      delete histTitle;     

      // Ftpc (East)
      histTitle = new TString("Flow_Phi_Flat_FtpcEast_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histTwoHar[j].mHistPhiFlatFtpcEast = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBinsFtpc, phiMin, phiMax);
      histFull[k].histTwoHar[j].mHistPhiFlatFtpcEast->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histTwoHar[j].mHistPhiFlatFtpcEast->SetYTitle("Counts");
      delete histTitle;
      
      // Ftpc (West)
      histTitle = new TString("Flow_Phi_Flat_FtpcWest_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histTwoHar[j].mHistPhiFlatFtpcWest = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBinsFtpc, phiMin, phiMax);
      histFull[k].histTwoHar[j].mHistPhiFlatFtpcWest->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histTwoHar[j].mHistPhiFlatFtpcWest->SetYTitle("Counts");
      delete histTitle;
      
      // Ftpc (FarWest)
      histTitle = new TString("Flow_Phi_Flat_FtpcFarWest_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histTwoHar[j].mHistPhiFlatFtpcFarWest = new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBinsFtpc, phiMin, phiMax);
      histFull[k].histTwoHar[j].mHistPhiFlatFtpcFarWest->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histTwoHar[j].mHistPhiFlatFtpcFarWest->SetYTitle("Counts");
      delete histTitle;
   
    }
  }

  // Calculate recenter paramerters?
  TFile fileReCent("flow.reCentAna.root","R");
  if (reCentCalc) {
    gMessMgr->Info("##### FlowAnalysis: Calc reCent Pars");
    mCalcReCentPars = kTRUE;
  } else {
    mCalcReCentPars = kFALSE;
  }

  gMessMgr->SetLimit("##### FlowAnalysis", 2);
  gMessMgr->Info("##### FlowAnalysis: $Id: StFlowAnalysisMaker.cxx,v 1.103 2011/07/25 15:54:42 posk Exp $");

  return StMaker::Init();
}

//-----------------------------------------------------------------------

Bool_t StFlowAnalysisMaker::FillFromFlowEvent() {
  // Get event quantities from StFlowEvent

  Bool_t kRETURN;

  for (int k = 0; k < Flow::nSels; k++) {
    pFlowSelect->SetSelection(k);
    for (int j = 0; j < Flow::nHars; j++) {
      pFlowSelect->SetHarmonic(j);
      for (int n = 0; n < Flow::nSubs; n++) {
	pFlowSelect->SetSubevent(n);
	int i = Flow::nSels*k + n;
	// sub-event quantities already recentered
	mQSub[i][j] = pFlowEvent->Q(pFlowSelect);
	mPsiSub[i][j] = pFlowEvent->Psi(pFlowSelect);
 	mMultSub[i][j] = pFlowEvent->Mult(pFlowSelect);
	if (mMultSub[i][j] < 3.) kRETURN = kFALSE; // to eliminate multSub < 3
	if (mQSub[i][j].Mod()==0.) kRETURN = kFALSE; // to eliminate psiSub=0
      }//subs
      
      pFlowSelect->SetSubevent(-1);
      // full event quantities already recentered
      mQ[k][j]    = pFlowEvent->Q(pFlowSelect);
      mPsi[k][j]  = pFlowEvent->Psi(pFlowSelect);
      m_q[k][j]   = pFlowEvent->q(pFlowSelect);
      mMult[k][j] = pFlowEvent->Mult(pFlowSelect);
      if (mQ[k][j].Mod()==0.) kRETURN = kFALSE; // to eliminate psi=0	
    }//full
  }
  mZDCSMD_e_PsiWgt = pFlowEvent->ZDCSMD_PsiWgtEast();
  mZDCSMD_w_PsiWgt = pFlowEvent->ZDCSMD_PsiWgtWest();
  mZDCSMD_f_PsiWgt = (pFlowEvent->UseZDCSMD()) ? pFlowEvent->ZDCSMD_PsiWgtFull():1.;
  mFlowWeight  	   = (pFlowEvent->UseZDCSMD()) ? mZDCSMD_e_PsiWgt*mZDCSMD_w_PsiWgt*mZDCSMD_f_PsiWgt:1.;

  return kRETURN;
}

//-----------------------------------------------------------------------

void StFlowAnalysisMaker::FillEventHistograms() {
  // Fill histograms with event quantities

  // trigger
  unsigned int triggers =  StFlowCutEvent::TriggersFound();
  mHistTrigger->Fill(triggers);

  // no selections: OrigMult, MultEta, Centrality, TotalMult, PartMult, MultOverOrig, VertexZ, VertexXY
  int origMult  = pFlowEvent->OrigMult();
  mHistOrigMult ->Fill((float)origMult);
  mHistMultEta  ->Fill((float)pFlowEvent->MultEta());
  int cent      = pFlowEvent->Centrality();
  mHistCent     ->Fill((float)cent);
  int totalMult = pFlowEvent->TrackCollection()->size();
  mHistMult     ->Fill((float)totalMult);
  UInt_t partMult = pFlowEvent->MultPart(pFlowSelect);
  mHistMultPart ->Fill((float)partMult);
  if (origMult) mHistMultOverOrig->Fill((float)totalMult / (float)origMult);

  StThreeVectorF vertex = pFlowEvent->VertexPos();
  mHistVertexZ   ->Fill(vertex.z());
  mHistVertexXY2D->Fill(vertex.x(), vertex.y());

  mHistCTBvsZDC2D->Fill(pFlowEvent->ZDCe() + pFlowEvent->ZDCw(), pFlowEvent->CTB());

  //ZDCSMD test
  for(int strip=1; strip<9; strip++) {
    mZDC_SMD_west_hori->Fill(strip,pFlowEvent->ZDCSMD(1,1,strip));
    mZDC_SMD_east_hori->Fill(strip,pFlowEvent->ZDCSMD(0,1,strip));
    if(strip==8) continue;
    mZDC_SMD_west_vert->Fill(strip,pFlowEvent->ZDCSMD(1,0,strip));
    mZDC_SMD_east_vert->Fill(strip,pFlowEvent->ZDCSMD(0,0,strip));
  }
  mHistZDCSMDPsiWgtEast->Fill(pFlowEvent->ZDCSMD_PsiEst());
  mHistZDCSMDPsiWgtWest->Fill(pFlowEvent->ZDCSMD_PsiWst());
  mHistZDCSMDPsiWgtTest->Fill(mPsi[0][0]);
  mHistZDCSMDPsiWgtFull->Fill(mPsi[0][0],mFlowWeight/mZDCSMD_f_PsiWgt);
  mHistZDCSMDPsiCorTest->Fill(pFlowEvent->ZDCSMD_PsiCorr());
  mHistZDCSMDPsiCorFull->Fill(pFlowEvent->ZDCSMD_PsiCorr(),mFlowWeight/mZDCSMD_f_PsiWgt);

  // sub-event Psi_Subs
  for (int k = 0; k < Flow::nSels; k++) {
    for (int j = 0; j < Flow::nHars; j++) {
      for (int n = 0; n < Flow::nSubs; n++) {
	int i = Flow::nSels*k + n;
	if(mPsiSub[i][j]) { histSub[i].histSubHar[j].mHistPsiSubs->Fill(mPsiSub[i][j]); }
	if (mQSub[i][j].Mod() && mMultSub[i][j]) {
	  if (k==0) { // FTPC
	    double QSubx = mQSub[i][j].X() / (double)mMultSub[i][j];
	    double QSuby = mQSub[i][j].Y() / (double)mMultSub[i][j];
	    histFull[n].histFullHar[j].mHistQFTPCSubXY2D->Fill(QSubx,QSuby);
	  } else if (k==1) { // TPC
	    double QSubx = mQSub[i][j].X() / (double)mMultSub[i][j];
	    double QSuby = mQSub[i][j].Y() / (double)mMultSub[i][j];
 	    histFull[n].histFullHar[j].mHistQTPCSubXY2D->Fill(QSubx,QSuby);
	  }
	}
      }
    }
  }

  // full event Psi, PsiSubCorr, PsiSubCorrDiff, cos, mult, q, reCent
  for (int k = 0; k < Flow::nSels; k++) {
    pFlowSelect->SetSelection(k);
    for (int j = 0; j < Flow::nHars; j++) {
      pFlowSelect->SetHarmonic(j);
      float order  = (float)(j+1);

      if (mPsi[k][j]) {
	histFull[k].histFullHar[j].mHistPsi->Fill(mPsi[k][j]);
	//histFull[k].histFullHar[j].mHistPsi->Fill(mPsi[k][j],mQ[k][j].Mod());
      }
      if (mQ[k][j].Mod() && mMult[k][j]) {
	double Qx = mQ[k][j].X() / (double)mMult[k][j];
	double Qy = mQ[k][j].Y() / (double)mMult[k][j];
	histFull[k].histFullHar[j].mHistQXY2D->Fill(Qx,Qy);
      }
      if (mPsi[0][j] && mPsi[1][j]) {
	if (k < 2 && j < 2) {
	  if (k == 0) { 
	    float psi1 = mPsi[0][j];
	    float psi2 = mPsi[1][j];	  
	    float diff = psi1 - psi2;	  
	    if (diff < -twopi/2./(j+1)) {
	      diff += twopi/(j+1);
	    } else if (diff > +twopi/2./(j+1)) {
	      diff -= twopi/(j+1);
	    }	  
	    histFull[k].histFullHar[j].mHistPsi_Diff->Fill(diff);
	  } else if (k == 1) {
	    float psi1 = 0.;
	    float psi2 = 0.;
	    if (j == 0) {
	      psi1 = mPsi[0][0];
	      psi2 = mPsi[1][1];
	    }  else if (j == 1) {
	      psi1 = mPsi[0][0];
	      psi2 = mPsi[0][1];
	    }
	    float diff = psi1 - psi2;
	    diff = (TMath::Abs(diff) > twopi/2.) ? ((diff > 0.) ? -(twopi - diff) :
						    -(diff + twopi)) : diff;
	    histFull[k].histFullHar[j].mHistPsi_Diff->Fill(diff);
	  }      
	}
      }
      
      if (mPsiSub[Flow::nSels*k][j] != 0. && mPsiSub[Flow::nSels*k+1][j] != 0.) {
	float psiSubCorr;
	if(pFlowEvent->UseZDCSMD()) {
	  psiSubCorr = pFlowEvent->ZDCSMD_PsiCorr();
	} 
	else if (mV1Ep1Ep2 == kFALSE || order != 1) { // normal case
	  psiSubCorr = mPsiSub[Flow::nSels*k][j] - mPsiSub[Flow::nSels*k+1][j];
	}
	else { // mV1Ep1Ep2 == kTRUE && order == 1
	  psiSubCorr = mPsiSub[Flow::nSels*k][0] + mPsiSub[Flow::nSels*k+1][0] - 2.*mPsi[k][1];
	}
	histFull[k].mHistCos->Fill(order, (float)cos(order * psiSubCorr),mFlowWeight/mZDCSMD_f_PsiWgt);
	if (psiSubCorr < 0.) psiSubCorr += twopi / order;
	if (psiSubCorr > twopi / order) psiSubCorr -= twopi / order; // for v1Ep1Ep2 which gives -twopi < psiSubCorr < 2.*twopi
	histFull[k].histFullHar[j].mHistPsiSubCorr->Fill(psiSubCorr,mFlowWeight/mZDCSMD_f_PsiWgt);
      }

      if (j < Flow::nHars - 1) { // subevents of different harmonics
	int j1 = 0, j2 = 0;
	float psiSubCorrDiff;
	if (j==0) {
	  j1 = 1, j2 = 2;	
	} else if (j==1) {
	  j1 = 1, j2 = 3;	
	} else if (j==2) {
	  j1 = 2, j2 = 4;	
	}
	psiSubCorrDiff = fmod((double)mPsiSub[Flow::nSels*k][j1-1],
	   twopi/(double)j2) - fmod((double)mPsiSub[Flow::nSels*k+1][j2-1],
				    twopi/(double)j2);
	if (psiSubCorrDiff < 0.) psiSubCorrDiff += twopi/(float)j2;
	if (psiSubCorrDiff) { histFull[k].histFullHar[j].mHistPsiSubCorrDiff->Fill(psiSubCorrDiff); }
	psiSubCorrDiff = fmod((double)mPsiSub[Flow::nSels*k][j2-1],
	   twopi/(double)j2) - fmod((double)mPsiSub[Flow::nSels*k+1][j1-1],
				    twopi/(double)j2);
	if (psiSubCorrDiff < 0.) psiSubCorrDiff += twopi/(float)j2;
	if (psiSubCorrDiff) { histFull[k].histFullHar[j].mHistPsiSubCorrDiff->Fill(psiSubCorrDiff); }
      }

      histFull[k].histFullHar[j].mHistMult->Fill((float)mMult[k][j]);
      if (m_q[k][j]) { histFull[k].histFullHar[j].mHist_q->Fill(m_q[k][j]); }

      if (mCalcReCentPars) {
	// calculate recentering parameters, fill 4 bins
	TVector2 qReCent;
	qReCent = pFlowEvent->ReCentEPPar(pFlowSelect,"FTPCE");
	if (qReCent.X()) histFull[k].histFullHar[j].mHistReCentX->Fill(1., qReCent.X());
	if (qReCent.Y()) histFull[k].histFullHar[j].mHistReCentY->Fill(1., qReCent.Y());
	qReCent = pFlowEvent->ReCentEPPar(pFlowSelect,"FTPCW");
	if (qReCent.X()) histFull[k].histFullHar[j].mHistReCentX->Fill(2., qReCent.X());
	if (qReCent.Y()) histFull[k].histFullHar[j].mHistReCentY->Fill(2., qReCent.Y());
	qReCent = pFlowEvent->ReCentEPPar(pFlowSelect,"TPCE");
	if (qReCent.X()) histFull[k].histFullHar[j].mHistReCentX->Fill(3., qReCent.X());
	if (qReCent.Y()) histFull[k].histFullHar[j].mHistReCentY->Fill(3., qReCent.Y());
	qReCent = pFlowEvent->ReCentEPPar(pFlowSelect,"TPCW");
	if (qReCent.X()) histFull[k].histFullHar[j].mHistReCentX->Fill(4., qReCent.X());
	if (qReCent.Y()) histFull[k].histFullHar[j].mHistReCentY->Fill(4., qReCent.Y());
      }
    }
  }
}

//-----------------------------------------------------------------------

void StFlowAnalysisMaker::FillParticleHistograms() {
  // Fill histograms from the particles

  float etaSymPosTpcN  = 0.;
  float etaSymNegTpcN  = 0.;
  float etaSymPosFtpcN = 0.;
  float etaSymNegFtpcN = 0.;
  float hPlusN         = 0.;
  float hMinusN        = 0.;
  float piPlusN        = 0.;
  float piMinusN       = 0.;
  float protonN        = 0.;
  float pbarN          = 0.;
  float kMinusN        = 0.;
  float kPlusN         = 0.;
  float deuteronN      = 0.;
  float dbarN          = 0.;
  float electronN      = 0.;
  float positronN      = 0.;

  // Initialize Iterator
  StFlowTrackCollection* pFlowTracks = pFlowEvent->TrackCollection();
  StFlowTrackIterator itr;

  for (itr = pFlowTracks->begin(); itr != pFlowTracks->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;

    float phi         = pFlowTrack->Phi();
    if (phi < 0.) phi += twopi;
    float eta         = pFlowTrack->Eta();
    float zFirstPoint = 0.;
    float zLastPoint  = 0.;
    if (pFlowEvent->FirstLastPoints()) {
      zFirstPoint = pFlowTrack->ZFirstPoint();
      zLastPoint  = pFlowTrack->ZLastPoint();
    }
    float pt          = pFlowTrack->Pt();
    int   charge      = pFlowTrack->Charge();
    float dca         = pFlowTrack->Dca();
    float dcaGlobal   = pFlowTrack->DcaGlobal();
    float chi2        = pFlowTrack->Chi2();
    int   fitPts      = pFlowTrack->FitPts();
    int   maxPts      = pFlowTrack->MaxPts();
    Char_t pid[10];
    strcpy(pid, pFlowTrack->Pid());
    float totalp    = pFlowTrack->P();
    float logp      = ::log(totalp);
    float dEdx      = pFlowTrack->Dedx();
    StTrackTopologyMap map = pFlowTrack->TopologyMap();

    // no selections: Charge, Dca, DcaGlobal, Chi2, FitPts, MaxPts, FitOverMax, PID

    // distinguish between Tpc and Ftpc
    if (map.trackFtpcEast() || map.trackFtpcWest()) {
      mHistChargeFtpc   ->Fill((float)charge);
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
      
      mHistDcaTpc      ->Fill(dca);
      mHistDcaGlobalTpc->Fill(dcaGlobal);
      mHistChi2Tpc     ->Fill(chi2);
      mHistFitPtsTpc   ->Fill((float)fitPts);
      mHistMaxPtsTpc   ->Fill((float)maxPts);
      if (maxPts) mHistFitOverMaxTpc->Fill((float)fitPts/(float)maxPts);
      
      if (charge == 1) {
	hPlusN++;
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
	hMinusN++;
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
    }//all tracks
    
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
    }//particles correlated with the EP

    // For Eta symmetry
    // Tpc
    if (map.hasHitInDetector(kTpcId)) {
      (eta > 0.) ? etaSymPosTpcN++ : etaSymNegTpcN++;
    }
    // Ftpc
    else if (map.trackFtpcEast() || map.trackFtpcWest()) {
      (eta > 0.) ? etaSymPosFtpcN++ : etaSymNegFtpcN++;
    }

    TVector2 Q, reCent;
    Double_t mult;
    // Get the angle of the Event Plane
    for (int k = 0; k < Flow::nSels; k++) {
      pFlowSelect->SetSelection(k);
      for (int j = 0; j < Flow::nHars; j++) {
	bool oddHar = (j+1) % 2;
	pFlowSelect->SetHarmonic(j);
	double order  = (double)(j+1);
	double orderEP  = order;
	float psi_i = 0., psi_2 = 0.;
	if (pFlowEvent->EtaSubs()) { // particles with the other subevent
	  int i = Flow::nSels*k;
	  psi_i = (eta > 0.) ? mPsiSub[i+1][j] : mPsiSub[i][j];
	} else if (pFlowEvent->RanSubs()) { // particles with the other subevent
	  int i = Flow::nSels*k;
	  if (pFlowTrack->Select(j,k,0)) {
	    psi_i =  mPsiSub[i+1][j];
	  } else if (pFlowTrack->Select(j,k,1)) {
	    psi_i =  mPsiSub[i][j];
	  } else { // neither
	    int r = (eta > 0.) ? 1 : 0;
	    psi_i =  mPsiSub[i+r][j]; // random
	  }
	} else if (order > 3. && !oddHar) { // 4, 6, etc
	  psi_i = mPsi[k][1];  // 2nd harmomic event plane
	  if (psi_i > twopi/order) psi_i -= twopi/order; // ???
	  if (psi_i > twopi/order) psi_i -= twopi/order;
	} else {
	  psi_i = mPsi[k][j];
	} // full EP

	if (pFlowSelect->Select(pFlowTrack)) { // particles used for the EP

	  histFull[k].histFullHar[j].mHistYield2D->Fill(eta, pt);

	  // Get phiWgt and reCent from files
	  double phiWgt = pFlowEvent->PhiWeight(k, j, pFlowTrack);
	  TVector2 reCent = pFlowEvent->ReCentEP(k, j, pFlowTrack);

	  if (pFlowMaker->PhiWgtCalc() && j < 2) { // only first two harmonics for phiWgt

	    // Get detID
	    StDetectorId detId;
	    Bool_t kTpcFarEast  = kFALSE;
	    Bool_t kTpcEast     = kFALSE;
	    Bool_t kTpcWest     = kFALSE;
	    Bool_t kTpcFarWest  = kFALSE;
	    Bool_t kFtpcFarEast = kFALSE;
	    Bool_t kFtpcEast    = kFALSE;
	    Bool_t kFtpcWest    = kFALSE;
	    Bool_t kFtpcFarWest = kFALSE;
	    if (map.hasHitInDetector(kTpcId) || (map.data(0) == 0 && map.data(1) == 0)) {
	      // Tpc track, or TopologyMap not available
	      detId = kTpcId;
	      // Set TpcEast and West
	      if (pFlowEvent->FirstLastPoints()) {
		if (zFirstPoint > 0. && zLastPoint > 0.) {
		  kTpcFarWest = kTRUE;
		} else if (zFirstPoint > 0. && zLastPoint < 0.) {
		  kTpcWest = kTRUE;
		} else if (zFirstPoint < 0. && zLastPoint > 0.) {
		  kTpcEast = kTRUE;
		} else {
		  kTpcFarEast = kTRUE;
		}
	      } else {
		Float_t vertexZ = pFlowEvent->VertexPos().z();
		if (eta > 0. && vertexZ > 0.) {
		  kTpcFarWest = kTRUE;
		} else if (eta > 0. && vertexZ < 0.) {
		  kTpcWest = kTRUE;
		} else if (eta < 0. && vertexZ > 0.) {
		  kTpcEast = kTRUE;
		} else {
		  kTpcFarEast = kTRUE;
		}
	      }
	    } else if (map.trackFtpcEast()) { // FTPC track
	      detId = kFtpcEastId;  // eta < 0.
	      Float_t vertexZ = pFlowEvent->VertexPos().z();
	      if (vertexZ > 0.) {
		kFtpcEast = kTRUE;
	      } else { // vertexZ < 0.
		kFtpcFarEast = kTRUE;
	      }
	    } else if (map.trackFtpcWest()) {
	      detId = kFtpcWestId; // eta > 0.
	      Float_t vertexZ = pFlowEvent->VertexPos().z();
	      if (vertexZ > 0.) {
		kFtpcFarWest = kTRUE;
	      } else { // vertexZ > 0.
		kFtpcWest = kTRUE;
	      }
	    } else {
	      detId = kUnknownId;
	    }
	    
	    // Calculate weights for filling histograms
	    float wt = 1.;
	    if (pFlowEvent->PtWgt()) { // pt wgt
	      wt *= (pt < pFlowEvent->PtWgtSaturation()) ? pt : 
		pFlowEvent->PtWgtSaturation();  // pt weighting going constant
	    }
	    float etaAbs = fabs(eta); // etaWgt, eta > 1.
	    //if (pFlowEvent->EtaWgt() && oddHar && etaAbs > 1.) { wt *= etaAbs; }
	    if (pFlowEvent->EtaWgt() && j==0 && etaAbs > 1.) { wt *= etaAbs; }
	    
	    // Fill histograms with selections
	    if (kFtpcFarEast) {
	      histFull[k].histTwoHar[j].mHistPhiFtpcFarEast->Fill(phi,wt);
	    } else if (kFtpcEast) {
	      histFull[k].histTwoHar[j].mHistPhiFtpcEast->Fill(phi,wt);
	    } else if (kFtpcWest) {
	      histFull[k].histTwoHar[j].mHistPhiFtpcWest->Fill(phi,wt);
	    } else if (kFtpcFarWest) {
	      histFull[k].histTwoHar[j].mHistPhiFtpcFarWest->Fill(phi,wt);
	    } else if (kTpcFarEast){
	      histFull[k].histTwoHar[j].mHistPhiFarEast->Fill(phi,wt);
	    } else if (kTpcEast){
	      histFull[k].histTwoHar[j].mHistPhiEast->Fill(phi,wt);
	    } else if (kTpcWest){
	      histFull[k].histTwoHar[j].mHistPhiWest->Fill(phi,wt);
	    } else if (kTpcFarWest){
	      histFull[k].histTwoHar[j].mHistPhiFarWest->Fill(phi,wt);
	    }
	    
	    // Which flat hist?
	    if (pFlowEvent->FirstLastPoints() && !pFlowEvent->FirstLastPhiWgt()) {
	      kTpcFarEast = kFALSE;
	      kTpcEast    = kFALSE;
	      kTpcWest    = kFALSE;
	      kTpcFarWest = kFALSE;
	      Float_t vertexZ = pFlowEvent->VertexPos().z();
	      if (eta > 0. && vertexZ > 0.) {
		kTpcFarWest = kTRUE;
	      } else if (eta > 0. && vertexZ < 0.) {
		kTpcWest = kTRUE;
	      } else if (eta < 0. && vertexZ > 0.) {
		kTpcEast = kTRUE;
	      } else {
		kTpcFarEast = kTRUE;
	      }
	    }
	    
	    //if (oddHar && eta < 0.) phiWgt /= -1.; // only for flat hists
	    if (j==0 && eta < 0.) phiWgt /= -1.; // only for flat hists
	    // Fill Flat histograms
	    if (kFtpcFarEast) {
	      histFull[k].histTwoHar[j].mHistPhiFlatFtpcFarEast->Fill(phi, phiWgt);
	    } else if (kFtpcEast) {
	      histFull[k].histTwoHar[j].mHistPhiFlatFtpcEast->Fill(phi, phiWgt);
	    } else if (kFtpcWest) {
	      histFull[k].histTwoHar[j].mHistPhiFlatFtpcWest->Fill(phi, phiWgt);
	    } else if (kFtpcFarWest) {
	      histFull[k].histTwoHar[j].mHistPhiFlatFtpcFarWest->Fill(phi, phiWgt);
	    } else if (kTpcFarEast) {
	      histFull[k].histTwoHar[j].mHistPhiFlatFarEast->Fill(phi, phiWgt);
	    } else if (kTpcEast) {
	      histFull[k].histTwoHar[j].mHistPhiFlatEast->Fill(phi, phiWgt);
	    } else if (kTpcWest) {
	      histFull[k].histTwoHar[j].mHistPhiFlatWest->Fill(phi, phiWgt);
	    } else if (kTpcFarWest) {
	      histFull[k].histTwoHar[j].mHistPhiFlatFarWest->Fill(phi, phiWgt);
	    }
	    if (j==0 && eta < 0.) phiWgt *= -1.; // restore value	    
	  }	  

	  // Remove autocorrelations with full EP
	  if (!pFlowEvent->EtaSubs() && !pFlowEvent->RanSubs()) {
	    TVector2 Q_i;
	    if (order > 3. && !oddHar) { // 2nd harmonic event plane
	      orderEP = 2;
	    }
	    double Qx = phiWgt * cos(orderEP * phi) - reCent.X();
	    double Qy = phiWgt * sin(orderEP * phi) - reCent.Y();
	    Q_i.Set(Qx, Qy);
	    TVector2 mQ_i = mQ[k][j] - Q_i;
	    psi_i = mQ_i.Phi() / orderEP;
	    if (psi_i < 0.) psi_i += twopi / orderEP;
	  }
	}//particles used for the EP
	  
	// Remove autocorrelations of the 2nd order 'particles' which were used for v1{EP1,EP2}.
	if (mV1Ep1Ep2 == kTRUE && order == 1) {
	  StFlowSelection usedForPsi2 = *pFlowSelect;
	  usedForPsi2.SetHarmonic(1);
	  usedForPsi2.SetSubevent(-1);
	  if (usedForPsi2.Select(pFlowTrack)) {
	    TVector2 Q_i;
	    double phiWgt = pFlowEvent->PhiWeight(k, 1, pFlowTrack);
	    TVector2 reCent = pFlowEvent->ReCentEP(k, 1, pFlowTrack);
	    double Qx = phiWgt * cos(2 * phi) - reCent.X();
	    double Qy = phiWgt * sin(2 * phi) - reCent.Y();
	    Q_i.Set(Qx, Qy);
	    TVector2 mQ_i = mQ[k][1] - Q_i;
	    psi_2 = mQ_i.Phi() / 2.;
	    if (psi_2 < 0.) psi_2 += twopi / 2.;	  
	  }
	  else { // particle was not used for Psi2 
	    psi_2 = mPsi[k][1];
	  }
	}

	// test recentering of Q per particle
	mult = (double)(pFlowEvent->Mult(pFlowSelect));
	if (mult > 0.) {
	  histFull[k].histFullHar[j].mHistQreCent->Fill(1., mQ[k][j].X()/mult);
	  histFull[k].histFullHar[j].mHistQreCent->Fill(2., mQ[k][j].Y()/mult);
	}

      	// Calculate v for all particles selected for correlation analysis
	if (pFlowSelect->SelectPart(pFlowTrack)) { // particles correlated with the EP
	  float v;
	  if (pFlowEvent->UseZDCSMD()) {
	    v = cos(order *(phi-mQ[k][1].Phi()))/perCent;
	  }
	  else if (mV1Ep1Ep2 == kFALSE || order != 1) {
	    v = cos(order * (phi - psi_i))/perCent; // normal method
	  }
	  else { // mV1Ep1Ep2 == kTRUE && order == 1
	    v = cos(phi + psi_i - 2.*psi_2)/perCent;
	  }
	  float vFlip = v;
	  if (eta < 0 && j==0) vFlip *= -1; // for 1st harmonic only
	  if (strlen(pFlowSelect->PidPart()) != 0) { // pid, fill rapidity 
	    float rapidity = pFlowTrack->Y();
	    histFull[k].histFullHar[j].mHist_vObs2D->Fill(rapidity, pt, v,mFlowWeight);
	    
	    if (mPtRange_for_vEta[1] > mPtRange_for_vEta[0]) { // cut is used
	      if (pt < mPtRange_for_vEta[1] && pt >= mPtRange_for_vEta[0]) {
		// check cut range, fill if in range
		histFull[k].histFullHar[j].mHist_vObsEta->Fill(rapidity, v,mFlowWeight);
	      }
	    }
	    else { // cut is not used, fill in any case
	      histFull[k].histFullHar[j].mHist_vObsEta->Fill(rapidity, v, mFlowWeight);
	    }

	  } else { // no pid, fill eta
	    histFull[k].histFullHar[j].mHist_vObs2D->Fill(eta, pt, v,mFlowWeight);
	   
	    if (mPtRange_for_vEta[1] > mPtRange_for_vEta[0]) { // cut is used
	      if (pt < mPtRange_for_vEta[1] && pt >= mPtRange_for_vEta[0]) {
		// check cut range, fill if in range
		histFull[k].histFullHar[j].mHist_vObsEta->Fill(eta, v, mFlowWeight);
	      }
	    }
	    else { // cut is not used, fill in any case
	      histFull[k].histFullHar[j].mHist_vObsEta->Fill(eta, v, mFlowWeight);
	    }
	  }

	  if (mEtaRange_for_vPt[1] > mEtaRange_for_vPt[0]) { // cut is used
	    if (TMath::Abs(eta) < mEtaRange_for_vPt[1] && TMath::Abs(eta) >= mEtaRange_for_vPt[0]) {
	      // check cut range, fill if in range
	      histFull[k].histFullHar[j].mHist_vObsPt->Fill(pt, vFlip, mFlowWeight);
	    }
	  }
	  else { // cut is not used, fill in any case
	    histFull[k].histFullHar[j].mHist_vObsPt->Fill(pt, vFlip,mFlowWeight);
	  }

	  // v_
	  Bool_t etaPtNoCut = kTRUE;
	  if (mPtRange_for_vEta[1] > mPtRange_for_vEta[0] && 
	      (pt < mPtRange_for_vEta[0] || pt >= mPtRange_for_vEta[1])) {
	    etaPtNoCut = kFALSE;
	  }
	  if (mEtaRange_for_vPt[1] > mEtaRange_for_vPt[0] && 
	      (TMath::Abs(eta) < mEtaRange_for_vPt[0] || 
	       TMath::Abs(eta) >= mEtaRange_for_vPt[1])) {
	    etaPtNoCut = kFALSE;
	  }
	  if (etaPtNoCut) histFull[k].mHist_vObs->Fill(order, vFlip,mFlowWeight);
	  
	  // PhiLab and Correlation of Phi of all particles with Psi
	  if (mPsi[k][j]) {
	    histFull[k].histFullHar[j].mHistPhiLab->Fill(phi);
	    histFull[k].histFullHar[j].mHistPhiLab->Fill(phi);
	    float phi_i = phi;
	    if (eta < 0 && j==0) {
	      phi_i += pi; // backward particle and 1st harmonic
	      if (phi_i > twopi) phi_i -= twopi;
	    }
	    float dPhi = phi_i - psi_i;
	    if (dPhi < 0.) dPhi += twopi;
	    histFull[k].histFullHar[j].mHistPhiCorr->
	      Fill(fmod((double)dPhi, twopi / order));
	  }
	}//particles correlated with the EP
      }
    }  
  }

  // EtaSym
  float etaSymTpc  = (etaSymPosTpcN  - etaSymNegTpcN)  / (etaSymPosTpcN  + etaSymNegTpcN);
  float etaSymFtpc = (etaSymPosFtpcN - etaSymNegFtpcN) / (etaSymPosFtpcN + etaSymNegFtpcN);

  StThreeVectorF vertex = pFlowEvent->VertexPos();
  Float_t vertexZ = vertex.z();
  mHistEtaSymVerZ2DTpc ->Fill(vertexZ , etaSymTpc);
  mHistEtaSymVerZ2DFtpc->Fill(vertexZ , etaSymFtpc);
  
  // Tpc
  float etaSymZInterceptTpc = 0.00023;  // new values introduced for 200 GeV
  float etaSymZSlopeTpc     = -0.00394; // data based on full statistics
  etaSymTpc -= (etaSymZInterceptTpc + etaSymZSlopeTpc * vertexZ); // corrected for acceptance
  etaSymTpc *= ::sqrt((etaSymPosTpcN + etaSymNegTpcN)); // corrected for statistics
  mHistEtaSymTpc->Fill(etaSymTpc);

  // Ftpc
  float etaSymZInterceptFtpc = -0.0077; // values for the FTPC based on 200 GeV data with
  float etaSymZSlopeFtpc = 0.0020;      // all sectors and 'bad runs' (323-325) excluded
  etaSymFtpc -= (etaSymZInterceptFtpc + etaSymZSlopeFtpc * vertexZ); // corrected for acceptance
  etaSymFtpc *= ::sqrt((etaSymPosFtpcN + etaSymNegFtpcN));  // corrected for statistics
  mHistEtaSymFtpc->Fill(etaSymFtpc);

  // PID multiplicities
  float totalMult = (float)pFlowEvent->TrackCollection()->size();
  mHistPidMult->Fill(1.,  totalMult);
  mHistPidMult->Fill(2.,  hPlusN);
  mHistPidMult->Fill(3.,  hMinusN);
  mHistPidMult->Fill(4.,  piPlusN);
  mHistPidMult->Fill(5.,  piMinusN);
  mHistPidMult->Fill(6.,  protonN);
  mHistPidMult->Fill(7.,  pbarN);
  mHistPidMult->Fill(8.,  kPlusN);
  mHistPidMult->Fill(9.,  kMinusN);
  mHistPidMult->Fill(10., deuteronN);
  mHistPidMult->Fill(11., dbarN);
  mHistPidMult->Fill(12., electronN);
  mHistPidMult->Fill(13., positronN);

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
  //  for the case k=2.

  double con = 0.626657;                   // sqrt(pi/2)/2
  double arg = chi * chi / 4.;

  double besselOneHalf    = ::sqrt(arg/halfpi) * sinh(arg)/arg;
  double besselThreeHalfs = ::sqrt(arg/halfpi) * (cosh(arg)/arg - sinh(arg)/(arg*arg));
  Double_t res = con * chi * exp(-arg) * (besselOneHalf + besselThreeHalfs); 

  return res;
}

//-----------------------------------------------------------------------

static Double_t resEventPlaneK3(double chi) {
  // Calculates the event plane resolution as a function of chi
  //  for the case k=3.

  double con = 0.626657;                   // sqrt(pi/2)/2
  double arg = chi * chi / 4.;

  Double_t res = con * chi * exp(-arg) * (TMath::BesselI1(arg) +
					  TMath::BesselI(2, arg)); 

  return res;
}

//-----------------------------------------------------------------------

static Double_t resEventPlaneK4(double chi) {
  // Calculates the event plane resolution as a function of chi
  //  for the case k=4.

  double con = 0.626657;                   // sqrt(pi/2)/2
  double arg = chi * chi / 4.;

  double besselOneHalf    = ::sqrt(arg/halfpi) * sinh(arg)/arg;
  double besselThreeHalfs = ::sqrt(arg/halfpi) * (cosh(arg)/arg - sinh(arg)/(arg*arg));
  double besselFiveHalfs  = besselOneHalf - 3*besselThreeHalfs/arg;

  Double_t res = con * chi * exp(-arg) * (besselThreeHalfs + besselFiveHalfs); 

  return res;
}

//-----------------------------------------------------------------------

Double_t chi(double res) {
  // Calculates chi from the event plane resolution

  double chi = 2.0;
  double delta = 1.0;

  for (int i = 0; i < 20; i++) {
      while(resEventPlane(chi) < res) {chi += delta;}
      delta = delta / 2.;
      while(resEventPlane(chi) > res) {chi -= delta;}
      delta = delta / 2.;
  }

  return chi;
}

//-----------------------------------------------------------------------

Int_t StFlowAnalysisMaker::Finish() {
  // Calculates resolution and mean flow values
  // Outputs phiWgt and reCent values

  cout << endl << "##### Analysis Maker:" << endl;
  TString* histTitle;

  // PhiWgt histogram collection
  TOrdCollection* phiWgtHistNames = new TOrdCollection(Flow::nSels*Flow::nHars+3);

  // If mCalcReCentPars write out the recentering parameters and print the recentered values
  TOrdCollection* savedHistReCentNames  = new TOrdCollection(Flow::nSels * Flow::nHars * 2);
  if (mCalcReCentPars) {
    Float_t reCentX, reCentY;
    cout << "ReCentered Q vector per particle:" << endl;
    for (int k = 0; k < Flow::nSels; k++) {
      for (int j = 0; j < Flow::nHars; j++) {
	savedHistReCentNames->AddLast(histFull[k].histFullHar[j].mHistReCentX);
	savedHistReCentNames->AddLast(histFull[k].histFullHar[j].mHistReCentY);
	reCentX   = histFull[k].histFullHar[j].mHistQreCent->GetBinContent(1);
	reCentY   = histFull[k].histFullHar[j].mHistQreCent->GetBinContent(2);
	cout << setprecision(3) << "Sel = " << k+1 << ", Har = " << j+1 << " : reCentedQ_x = "
	     << reCentX << ",\t reCentedQ_y = " << reCentY << endl;
      }
    }
  }
  cout << endl;

  // Calculate resolution from sqrt(mHistCos)
  double cosPair[Flow::nSels][Flow::nHars];
  double cosPairErr[Flow::nSels][Flow::nHars];
  double content;
  double error;
  double totalError;
  for (int k = 0; k < Flow::nSels; k++) {
    // Create the 1D v histogram
    histTitle = new TString("Flow_v_Sel");
    *histTitle += k+1;
    histFull[k].mHist_v = 
      histFull[k].mHist_vObs->ProjectionX(histTitle->Data());
    histFull[k].mHist_v->SetTitle(histTitle->Data());
    histFull[k].mHist_v->SetXTitle("Harmonic");
    histFull[k].mHist_v->SetYTitle("v (%)");
    delete histTitle;
    AddHist(histFull[k].mHist_v);

    for (int j = 0; j < Flow::nHars; j++) {
      double order  = (double)(j+1);
      cosPair[k][j]    = histFull[k].mHistCos->GetBinContent(j+1);
      cosPairErr[k][j] = histFull[k].mHistCos->GetBinError(j+1);

      if (pFlowEvent->UseZDCSMD()) { // ZDCSMD used to determine RP resolution
	double ZDCSMD_deltaResSub = 0.005,ZDCSMD_mResDelta=0.;
	double ZDCSMD_resSub = (histFull[k].mHistCos->GetBinContent(1)>0.) ? 
	  ::sqrt(histFull[k].mHistCos->GetBinContent(1)) : 0.;
	double ZDCSMD_resSubErr = (histFull[k].mHistCos->GetBinContent(1)>0.) ? 
	  histFull[k].mHistCos->GetBinError(1)/(2.*ZDCSMD_resSub) : 0.;
	double ZDCSMD_chiSub = chi(ZDCSMD_resSub);
	double ZDCSMD_chiSubDelta = chi((ZDCSMD_resSub+ZDCSMD_deltaResSub));
	if (j==0) {
	  mRes[k][j] = resEventPlane(::sqrt(2.) * ZDCSMD_chiSub);
	  ZDCSMD_mResDelta = resEventPlane(::sqrt(2.) * ZDCSMD_chiSubDelta);
	}
	if (j==1) {
	  mRes[k][j] = resEventPlaneK2(::sqrt(2.) * ZDCSMD_chiSub);
	  ZDCSMD_mResDelta = resEventPlaneK2(::sqrt(2.) * ZDCSMD_chiSubDelta);
	}
	if (j==2) {
	  mRes[k][j] = resEventPlaneK3(::sqrt(2.) * ZDCSMD_chiSub);
	  ZDCSMD_mResDelta = resEventPlaneK3(::sqrt(2.) * ZDCSMD_chiSubDelta);
	}
	if (j==3) {
	  mRes[k][j] = resEventPlaneK4(::sqrt(2.) * ZDCSMD_chiSub);
	  ZDCSMD_mResDelta = resEventPlaneK4(::sqrt(2.) * ZDCSMD_chiSubDelta);
	}
	mResErr[k][j] = ZDCSMD_resSubErr * fabs ((double)mRes[k][j]
		- ZDCSMD_mResDelta) / ZDCSMD_deltaResSub;
	
      }//UseZDCSMD
      else {
	if (cosPair[k][j] > 0.) {
	  double resSub, resSubErr;
	  double res2, res2error;
	  if (mV1Ep1Ep2 == kTRUE && order == 1) { // mixed harmonics
	    // calculate resolution of second order event plane first
	    if (histFull[k].mHistCos->GetBinContent(2) > 0.) {
	      if (histFull[k].mHistCos->GetBinContent(2) > 0.92) { // resolution saturates
		res2 = 0.99;
		res2error = 0.007;
	      } else {
		double deltaRes2Sub = 0.005;  // differential for the error propagation
		double res2Sub = ::sqrt(histFull[k].mHistCos->GetBinContent(2));
		double res2SubErr = histFull[k].mHistCos->GetBinError(2) / (2. * res2Sub);
		double chiSub2 = chi(res2Sub);
		double chiSub2Delta = chi(res2Sub + deltaRes2Sub);
		res2 = resEventPlane(::sqrt(2.) * chiSub2); // full event plane res.
		double mRes2Delta = resEventPlane(::sqrt(2.) * chiSub2Delta);
		res2error = res2SubErr * fabs((double)res2 - mRes2Delta) 
		  / deltaRes2Sub;
	      }
	    } else { // neg. corr.
	      res2 = 0.;
	      res2error = 0.;
	    }
	    // now put everything together with first order event plane
	    mRes[k][j]    = ::sqrt(cosPair[k][0]*res2);
	    mResErr[k][j] = 1./(2.*mRes[k][j])*::sqrt(res2*res2*cosPairErr[k][0]*cosPairErr[k][0]
	    + cosPair[k][0]*cosPair[k][0]*res2error*res2error); // Gaussian error propagation
	    if (!pFlowEvent->EtaSubs()) {
	      // correct to full event plane resolution
	      // 1st order res for k=1 is small and linear
	      mRes[k][j] *= ::sqrt(2.);
	      mResErr[k][j] *= ::sqrt(2.);
	    }
	  } else if (pFlowEvent->EtaSubs() || pFlowEvent->RanSubs()) { // sub res only
	    resSub = ::sqrt(cosPair[k][j]);
	    resSubErr = cosPairErr[k][j] / (2. * resSub);
	    mRes[k][j]    = resSub;
	    mResErr[k][j] = resSubErr;
	  } else if (order==4. || order==6.|| order==8.) { // 2nd harmonic event plane
	    double deltaResSub = 0.005;  // differential for the error propagation
	    double resSub = ::sqrt(cosPair[k][1]);
	    double resSubErr = cosPairErr[k][1] / (2. * resSub);
	    double chiSub = chi(resSub);
	    double chiSubDelta = chi(resSub + deltaResSub);
	    double mResDelta;
	    if (order==4.) {
	      mRes[k][j] = resEventPlaneK2(::sqrt(2.) * chiSub); // full event plane res.
	      mResDelta = resEventPlaneK2(::sqrt(2.) * chiSubDelta);
	    } else if (order==6.) {
	      mRes[k][j] = resEventPlaneK3(::sqrt(2.) * chiSub); // full event plane res.
	      mResDelta = resEventPlaneK3(::sqrt(2.) * chiSubDelta);
	    } else {
	      mRes[k][j] = resEventPlaneK4(::sqrt(2.) * chiSub); // full event plane res.
	      mResDelta = resEventPlaneK4(::sqrt(2.) * chiSubDelta);
	    }
	    mResErr[k][j] = resSubErr * fabs((double)mRes[k][j] - mResDelta) / deltaResSub;
	  } else { //normal case
	    if (cosPair[k][j] > 0.92) { // resolution saturates
	      mRes[k][j]    = 0.99;
	      mResErr[k][j] = 0.007;
	    } else {
	      double deltaResSub = 0.005;  // differential for the error propagation
	      double resSub = ::sqrt(cosPair[k][j]);
	      double resSubErr = cosPairErr[k][j] / (2. * resSub);
	      double chiSub = chi(resSub);
	      double chiSubDelta = chi(resSub + deltaResSub);
	      mRes[k][j] = resEventPlane(::sqrt(2.) * chiSub); // full event plane res.
	      double mResDelta = resEventPlane(::sqrt(2.) * chiSubDelta);
	      mResErr[k][j] = resSubErr * fabs((double)mRes[k][j] - mResDelta) 
		/ deltaResSub;
	    }
	  }
	} else {      // subevent correlation must be positive
	  mRes[k][j]    = 0.;
	  mResErr[k][j] = 0.;
	}
      }//else :standard way if(!pFlowEvent->UseZDCSMD())
      histFull[k].mHistRes->SetBinContent(j+1, mRes[k][j]);
      histFull[k].mHistRes->SetBinError(j+1, mResErr[k][j]);

	// Create the v 2D histogram
      histTitle = new TString("Flow_v2D_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHist_v2D = 
	histFull[k].histFullHar[j].mHist_vObs2D->ProjectionXY(histTitle->Data());
      histFull[k].histFullHar[j].mHist_v2D->SetTitle(histTitle->Data());
      histFull[k].histFullHar[j].mHist_v2D->SetXTitle((char*)xLabel.Data());
      histFull[k].histFullHar[j].mHist_v2D->SetYTitle("Pt (GeV/c)");
      histFull[k].histFullHar[j].mHist_v2D->SetZTitle("v (%)");
      delete histTitle;
      AddHist(histFull[k].histFullHar[j].mHist_v2D);

      // Create the 1D v histograms
      histTitle = new TString("Flow_vEta_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHist_vEta = 
	histFull[k].histFullHar[j].mHist_vObsEta->ProjectionX(histTitle->Data());
      histFull[k].histFullHar[j].mHist_vEta->SetTitle(histTitle->Data());
      histFull[k].histFullHar[j].mHist_vEta->SetXTitle((char*)xLabel.Data());
      histFull[k].histFullHar[j].mHist_vEta->SetYTitle("v (%)");
      delete histTitle;
      AddHist(histFull[k].histFullHar[j].mHist_vEta);

      TString* histTitle = new TString("Flow_vPt_Sel");
      *histTitle += k+1;
      histTitle->Append("_Har");
      *histTitle += j+1;
      histFull[k].histFullHar[j].mHist_vPt = 
	histFull[k].histFullHar[j].mHist_vObsPt->ProjectionX(histTitle->Data());
      histFull[k].histFullHar[j].mHist_vPt->SetTitle(histTitle->Data());
      histFull[k].histFullHar[j].mHist_vPt->SetXTitle("Pt (GeV/c)");
      histFull[k].histFullHar[j].mHist_vPt->SetYTitle("v (%)");
      delete histTitle;
      AddHist(histFull[k].histFullHar[j].mHist_vPt);

      // Calulate v = vObs / Resolution
      if (mRes[k][j]) {
	cout << endl << "##### Resolution of the " << j+1 << "th harmonic = " << 
	  mRes[k][j] << " +/- " << mResErr[k][j] << endl;
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
	totalError = fabs(content) * ::sqrt((error/content)*(error/content) +
	       (mResErr[k][j]/mRes[k][j])*(mResErr[k][j]/mRes[k][j]));
	histFull[k].mHist_v->SetBinError(j+1, totalError);

	// Calculate non-flatness correction
	TF1* funcCosSin = new TF1("funcCosSin",
		   "[3]*(1.+[0]*2./100.*cos([2]*x)+[1]*2./100.*sin([2]*x))", 0., twopi);
	//funcCosSin->SetParNames("100*cos", "100*sin", "har");
	funcCosSin->SetParameters(0, 0, j+1); // initial values
	funcCosSin->SetParLimits(2, 1, 1); // har is fixed
	histFull[k].histFullHar[j].mHistPhiLab->Fit("funcCosSin","Q,N");
	Double_t cosParLab = funcCosSin->GetParameter(0);
	Double_t sinParLab = funcCosSin->GetParameter(1);
	histFull[k].histFullHar[j].mHistPsi->Fit("funcCosSin","Q,N");
	Double_t cosParEP = funcCosSin->GetParameter(0);
	Double_t sinParEP = funcCosSin->GetParameter(1);
	cout << "100*cosLab = " << cosParLab << ", 100*sinLab = " << sinParLab << endl;
	cout << "100*cosEP = " << cosParEP << ", 100*sinEP = " << sinParEP << endl;
	delete funcCosSin;
	float nonflat = (cosParEP*cosParLab + sinParEP*sinParLab)/mRes[k][j]/100.;
	cout << "nonflat = " << nonflat << endl;

	cout << "##### v" << j+1 << "= (" << content << " +/- " << error << ")%" << endl;
	cout << "##### v" << j+1 << "= (" << content - nonflat << " (with nonflat corr.) +/- "
	     << totalError << " (with syst.) )%" << endl;
	histFull[k].mHist_v->SetBinContent(j+1, content - nonflat);


      } else {
	cout << "##### Resolution of the " << j+1 << "th harmonic was zero."
	     << endl;
	histFull[k].histFullHar[j].mHist_v2D-> Reset();
	histFull[k].histFullHar[j].mHist_vEta->Reset();
	histFull[k].histFullHar[j].mHist_vPt ->Reset();
	histFull[k].mHist_v->SetBinContent(j+1, 0.);
	histFull[k].mHist_v->SetBinError(j+1, 0.);
      }//v

      // Calculate PhiWgt
      if (pFlowMaker->PhiWgtCalc()) {
	if (j < 2) {
	  double meanFarEast = histFull[k].histTwoHar[j].mHistPhiFarEast->Integral() 
	    / (double)Flow::nPhiBins;
	  double meanEast = histFull[k].histTwoHar[j].mHistPhiEast->Integral() 
	    / (double)Flow::nPhiBins;
	  double meanWest = histFull[k].histTwoHar[j].mHistPhiWest->Integral() 
	    / (double)Flow::nPhiBins;
	  double meanFarWest = histFull[k].histTwoHar[j].mHistPhiFarWest->Integral() 
	    / (double)Flow::nPhiBins;
	  double meanFtpcFarEast = histFull[k].histTwoHar[j].mHistPhiFtpcFarEast->Integral() 
	    / (double)Flow::nPhiBinsFtpc;
	  double meanFtpcEast = histFull[k].histTwoHar[j].mHistPhiFtpcEast->Integral() 
	    / (double)Flow::nPhiBinsFtpc;
	  double meanFtpcWest = histFull[k].histTwoHar[j].mHistPhiFtpcWest->Integral() 
	    / (double)Flow::nPhiBinsFtpc;
	  double meanFtpcFarWest = histFull[k].histTwoHar[j].mHistPhiFtpcFarWest->Integral() 
	    / (double)Flow::nPhiBinsFtpc;
	  
	  // Tpc
	  for (int i = 0; i < Flow::nPhiBins; i++) {
	    histFull[k].histTwoHar[j].mHistPhiWgtFarEast->SetBinContent(i+1,meanFarEast);
	    histFull[k].histTwoHar[j].mHistPhiWgtFarEast->SetBinError(i+1, 0.);
	    histFull[k].histTwoHar[j].mHistPhiWgtEast   ->SetBinContent(i+1, meanEast);
	    histFull[k].histTwoHar[j].mHistPhiWgtEast   ->SetBinError(i+1, 0.);
	    histFull[k].histTwoHar[j].mHistPhiWgtWest   ->SetBinContent(i+1, meanWest);
	    histFull[k].histTwoHar[j].mHistPhiWgtWest   ->SetBinError(i+1, 0.);
	    histFull[k].histTwoHar[j].mHistPhiWgtFarWest->SetBinContent(i+1,meanFarWest);
	    histFull[k].histTwoHar[j].mHistPhiWgtFarWest->SetBinError(i+1, 0.);
	  }
	  
	  // Ftpc
	  for (int i = 0; i < Flow::nPhiBinsFtpc; i++) {
	    histFull[k].histTwoHar[j].mHistPhiWgtFtpcFarEast->SetBinContent(i+1,meanFtpcFarEast);
	    histFull[k].histTwoHar[j].mHistPhiWgtFtpcFarEast->SetBinError(i+1, 0.);
	    histFull[k].histTwoHar[j].mHistPhiWgtFtpcEast   ->SetBinContent(i+1,meanFtpcEast);
	    histFull[k].histTwoHar[j].mHistPhiWgtFtpcEast   ->SetBinError(i+1, 0.);
	    histFull[k].histTwoHar[j].mHistPhiWgtFtpcWest   ->SetBinContent(i+1,meanFtpcWest);
	    histFull[k].histTwoHar[j].mHistPhiWgtFtpcWest   ->SetBinError(i+1, 0.);
	    histFull[k].histTwoHar[j].mHistPhiWgtFtpcFarWest->SetBinContent(i+1,meanFtpcFarWest);
	    histFull[k].histTwoHar[j].mHistPhiWgtFtpcFarWest->SetBinError(i+1, 0.);
	  }
	  
	  // Tpc
	  histFull[k].histTwoHar[j].mHistPhiWgtFarEast->
	    Divide(histFull[k].histTwoHar[j].mHistPhiFarEast);
	  phiWgtHistNames->AddLast(histFull[k].histTwoHar[j].mHistPhiWgtFarEast);
	  histFull[k].histTwoHar[j].mHistPhiWgtEast->
	    Divide(histFull[k].histTwoHar[j].mHistPhiEast);
	  phiWgtHistNames->AddLast(histFull[k].histTwoHar[j].mHistPhiWgtEast);
	  histFull[k].histTwoHar[j].mHistPhiWgtWest->
	    Divide(histFull[k].histTwoHar[j].mHistPhiWest);
	  phiWgtHistNames->AddLast(histFull[k].histTwoHar[j].mHistPhiWgtWest);
	  histFull[k].histTwoHar[j].mHistPhiWgtFarWest->
	    Divide(histFull[k].histTwoHar[j].mHistPhiFarWest);
	  phiWgtHistNames->AddLast(histFull[k].histTwoHar[j].mHistPhiWgtFarWest);
	  
	  // Ftpc
	  histFull[k].histTwoHar[j].mHistPhiWgtFtpcFarEast->
	    Divide(histFull[k].histTwoHar[j].mHistPhiFtpcFarEast);
	  phiWgtHistNames->AddLast(histFull[k].histTwoHar[j].mHistPhiWgtFtpcFarEast);
	  histFull[k].histTwoHar[j].mHistPhiWgtFtpcEast->
	    Divide(histFull[k].histTwoHar[j].mHistPhiFtpcEast);
	  phiWgtHistNames->AddLast(histFull[k].histTwoHar[j].mHistPhiWgtFtpcEast);
	  histFull[k].histTwoHar[j].mHistPhiWgtFtpcWest->
	    Divide(histFull[k].histTwoHar[j].mHistPhiFtpcWest);
	  phiWgtHistNames->AddLast(histFull[k].histTwoHar[j].mHistPhiWgtFtpcWest);
	  histFull[k].histTwoHar[j].mHistPhiWgtFtpcFarWest->
	    Divide(histFull[k].histTwoHar[j].mHistPhiFtpcFarWest);
	  phiWgtHistNames->AddLast(histFull[k].histTwoHar[j].mHistPhiWgtFtpcFarWest);
	}
      }//phiWgt
    }
  }
  phiWgtHistNames->AddLast(mHistZDCSMDPsiWgtEast);
  phiWgtHistNames->AddLast(mHistZDCSMDPsiWgtWest);
  TFile* pPhiWgtFile = new TFile("flowPhiWgt.hist.root", "READ");
  if (pPhiWgtFile->IsOpen())
    { phiWgtHistNames->AddLast(mHistZDCSMDPsiWgtFull); }

  // Write all histograms
  TFile histFile("flow.hist.root", "RECREATE");
  //GetHistList()->ls();
  GetHistList()->Write();
  histFile.Close();
  
  // Write PhiWgt histograms preceded by documenting text
  if (pFlowMaker->PhiWgtCalc()) {
    TFile phiWgtNewFile("flowPhiWgtNew.hist.root", "RECREATE");
    TText* textInfo = 0;
    if (pFlowEvent->FirstLastPoints()) {
      char chInfo[400];
      sprintf(chInfo, "%s%d%s%d%s", " pt weight= ", pFlowEvent->PtWgt(),
	      ", eta weight= ", pFlowEvent->EtaWgt(), "\n");
      textInfo = new TText(0,0,chInfo);
      textInfo->Write("info");
    }
    phiWgtNewFile.cd();
    phiWgtHistNames->Write();
    phiWgtNewFile.Close();
    if (pFlowEvent->FirstLastPoints()) delete textInfo;
  }
  delete phiWgtHistNames;

  // Write reCent values
  if (mCalcReCentPars) {
    TFile fileReCent("flow.reCentAnaNew.root", "RECREATE");
    fileReCent.cd();
    savedHistReCentNames->Write();
    fileReCent.Close();
  }
  delete savedHistReCentNames;
  
  delete pFlowSelect;

  return StMaker::Finish();
}

//-----------------------------------------------------------------------

void StFlowAnalysisMaker::SetHistoRanges(Bool_t ftpc_included) {

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

void StFlowAnalysisMaker::SetPtRange_for_vEta(Float_t lo, Float_t hi) {

  // Sets the pt range for the v(eta) histograms.

  mPtRange_for_vEta[0] = lo;
  mPtRange_for_vEta[1] = hi;

  return;
}

//------------------------------------------------------------------------

void StFlowAnalysisMaker::SetEtaRange_for_vPt(Float_t lo, Float_t hi) {
  
  // Sets the |eta| range for the v(pt) histograms.

  mEtaRange_for_vPt[0] = lo;
  mEtaRange_for_vPt[1] = hi;

  return;
}

//------------------------------------------------------------------------

void StFlowAnalysisMaker::SetV1Ep1Ep2(Bool_t v1Ep1Ep2) {
  
  // Switches the v_1{EP1,EP2} calculation on/off.

  mV1Ep1Ep2 = v1Ep1Ep2;

  return;
}


////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowAnalysisMaker.cxx,v $
// Revision 1.103  2011/07/25 15:54:42  posk
// Added correction for non-flatness of event plane.
//
// Revision 1.102  2011/03/10 18:56:20  posk
// Added histogram for laboratory azimuthal distribution of particles.
//
// Revision 1.101  2010/09/30 19:28:09  posk
// Instead of reversing the weight for negative pseudrapidity for odd harmonics,
// it is now done only for the first harmonic.
// Recentering is now done for all harmonics.
//
// Revision 1.100  2010/02/15 12:01:58  canson
// Changed mHistCTBvsZDC2D from filling with ZDC_e + ZDC_e to filling with ZDC_e + ZDC_w
//
// Revision 1.99  2009/11/24 19:29:11  posk
// Added reCenter to remove acceptance correlations as an option instead of phiWgt.
//
// Revision 1.98  2007/07/13 22:18:29  posk
// Method chi() revised by Wang Gang: "With this modification,
// it will give good results not only for realistic resolutions, but also
// for res=1 and res=0."
//
// Revision 1.97  2007/02/06 19:00:39  posk
// In Lee Yang Zeros method, introduced recentering of Q vector.
// Reactivated eta symmetry cut.
//
// Revision 1.96  2006/07/10 21:03:48  posk
// For profile histograms of v, changed the limits to -1000, 1000.
//
// Revision 1.95  2006/02/22 19:36:21  posk
// Minor updates.
//
// Revision 1.94  2005/08/26 19:00:15  posk
// plot style back to bold
//
// Revision 1.93  2005/08/05 20:13:35  posk
// Improved first guess for qDist fit.
//
// Revision 1.92  2005/02/11 23:17:14  posk
// Fixed trigger histogram.
//
// Revision 1.91  2005/02/08 22:37:53  posk
// Fixed trigger histogram for year=4.
//
// Revision 1.90  2004/12/20 19:41:24  aihong
// crashes when run without ZDCSMD. bug fixed
//
// Revision 1.89  2004/12/17 22:33:35  aihong
// add in full Psi weight for ZDC SMD and fix a few bugs, done by Gang
//
// Revision 1.88  2004/12/09 23:47:05  posk
// Minor changes in code formatting.
// Added hist for TPC primary dca to AnalysisMaker.
//
// Revision 1.87  2004/12/07 23:10:19  posk
// Only odd and even phiWgt hists. If the old phiWgt file contains more than
// two harmonics, only the first two are read. Now writes only the first two.
//
// Revision 1.86  2004/08/24 20:22:36  oldi
// Minor modifications to avoid compiler warnings.
//
// Revision 1.85  2004/08/18 00:18:59  oldi
// Several changes were necessary to comply with latest changes of MuDsts and StEvent:
//
// nHits, nFitPoints, nMaxPoints
// -----------------------------
// From now on
//  - the fit points used in StFlowMaker are the fit points within the TPC xor FTPC (vertex excluded).
//  - the max. possible points used in StFlowMAker are the max. possible points within the TPC xor FTPC (vertex excluded).
//  - the number of points (nHits; not used for analyses so far) are the total number of points on a track, i. e.
//    TPC + SVT + SSD + FTPCeast + FTPCwest [reading from HBT event gives a warning, but it seems like nobody uses it anyhow].
// - The fit/max plot (used to be (fit-1)/max) was updated accordingly.
// - The default cuts for fit points were changed (only for the FTPC, since TPC doesn't set default cuts).
// - All these changes are backward compatible, as long as you change your cuts for the fit points by 1 (the vertex used to
//   be included and is not included anymore). In other words, your results won't depend on old or new MuDst, StEvent,
//   PicoDsts as long as you use the new flow software (together with the latest MuDst and StEvent software version).
// - For backward compatibility reasons the number of fit points which is written out to the flowpicoevent.root file
//   includes the vertex. It is subtracted internally while reading back the pico files. This is completely hidden from the
//   user.
//
// zFirstPoint
// -----------
// The positions of the first point of tracks which have points in the TPC can lie outside of the TPC (the tracks can start in
// the SVT or SSD now). In this case, the first point of the track is obtained by extrapolating the track helix to the inner
// radius of the TPC.
//
// Revision 1.84  2004/05/31 20:09:22  oldi
// PicoDst format changed (Version 7) to hold ZDC SMD information.
// Trigger cut modified to comply with TriggerCollections.
// Centrality definition for 62 GeV data introduced.
// Minor bug fixes.
//
// Revision 1.83  2004/05/05 21:13:47  aihong
// Gang's code for ZDC-SMD added
//
// Revision 1.82  2004/03/11 18:00:03  posk
// Added Random Subs analysis method.
//
// Revision 1.81  2003/12/12 02:34:40  oldi
// Removal of some major bugs in the v1{EP1,EP2} method.
//
// Revision 1.80  2003/12/09 01:40:15  oldi
// Removed 'inline' of some functions to cope with new compiler.
//
// Revision 1.79  2003/11/14 20:00:40  oldi
// Implementation of v1{EP1,EP2}. This method is set to be the default for v1 now!
// Minor code clean-ups.
//
// Revision 1.78  2003/09/02 17:58:10  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.77  2003/08/26 21:10:10  posk
// Calculates v8 if nHars=8.
//
// Revision 1.76  2003/08/06 20:54:06  oldi
// Introduction of possibility to exclude pt ranges for v(eta) and eta regions
// for v(pt) histograms. Default behavior stays the same (all available tracks
// are included in v(pt) and v(eta)).
//
// Revision 1.75  2003/07/30 22:08:25  oldi
// Several code fixes for EtaSym plots introduced (esp. the acceptance correction
// is done now for 200 GeV data and for the FTPCs as well).
// PtWgtSaturation parameter introduced.
//
// Revision 1.74  2003/07/07 21:58:16  posk
// Made units of momentum GeV/c instead of GeV.
//
// Revision 1.73  2003/06/27 21:25:41  posk
// v4 and v6 are with repect to the 2nd harmonic event plane.
//
// Revision 1.72  2003/05/16 20:44:46  posk
// First commit of StFlowPhiWgtMaker
//
// Revision 1.71  2003/05/06 21:33:04  posk
// Removed some histograms.
//
// Revision 1.70  2003/05/06 18:38:05  posk
// Removed StFlowTagMaker.
//
// Revision 1.69  2003/01/16 16:02:27  posk
// Some plotting changes.
//
// Revision 1.68  2003/01/10 16:40:16  oldi
// Several changes to comply with FTPC tracks:
// - Switch to include/exclude FTPC tracks introduced.
//   The same switch changes the range of the eta histograms.
// - Eta symmetry plots for FTPC tracks added and separated from TPC plots.
// - PhiWgts and related histograms for FTPC tracks split in FarEast, East,
//   West, FarWest (depending on vertex.z()).
// - Psi_Diff plots for 2 different selections and the first 2 harmonics added.
// - Cut to exclude mu-events with no primary vertex introduced.
//   (This is possible for UPC events and FTPC tracks.)
// - Global DCA cut for FTPC tracks added.
// - Global DCA cuts for event plane selection separated for TPC and FTPC tracks.
// - Charge cut for FTPC tracks added.
//
// Revision 1.67  2003/01/08 19:28:07  posk
// PhiWgt hists sorted on sign of z of first and last points.
//
// Revision 1.66  2002/10/28 19:45:52  posk
// Eliminate events with Psi=0.
//
// Revision 1.65  2002/06/15 23:03:56  posk
// Changed Fit/Max histogram to (Fit - 1)/Max.
//
// Revision 1.64  2002/05/23 18:57:08  posk
// changed label on MultHist histogram
//
// Revision 1.63  2002/05/21 18:42:15  posk
// Kirill's correction to minBias.C for bins with one count.
//
// Revision 1.62  2002/02/13 22:31:27  posk
// Pt Weight now also weights Phi Weight. Added Eta Weught, default=FALSE.
//
// Revision 1.61  2002/01/31 01:09:10  posk
// *** empty log message ***
//
// Revision 1.60  2002/01/14 23:42:21  posk
// Renamed ScalerProd histograms. Moved print commands to FlowMaker::Finish().
//
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
// replace iostream/stdlib by Stiostream.h/stdlib.h
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
