///////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowCumulantMaker.cxx,v 1.1 2001/11/02 04:47:42 aihong Exp $
//
// Authors:  Aihong Tang, Kent State U. Oct 2001
//           Frame adopted from Art and Raimond's StFlowAnalysisMaker.
///////////////////////////////////////////////////////////////////////////////
//
// Description:  Maker to analyze Flow using the latest cumulant method.
//                refer to Phy. Rev. C63 (2001) 054906 (new new method)
//                and      Phy. Rev. C62 (2000) 034902 (old new method)
//            all Eq. number tag are from new method paper if not specified.
///////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <math.h>
#include "float.h"
#include "StMaker.h"
#include "StFlowCumulantMaker.h"
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
#define PR(x) cout << "##### FlowCumulantAnalysis: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowCumulantMaker)

//-----------------------------------------------------------------------

StFlowCumulantMaker::StFlowCumulantMaker(const Char_t* name): StMaker(name),
  MakerName(name) {
  pFlowSelect = new StFlowSelection();
  mOldMethod=kFALSE;
}

StFlowCumulantMaker::StFlowCumulantMaker(const Char_t* name,
					 const StFlowSelection& flowSelect) :
  StMaker(name), MakerName(name) {
  pFlowSelect = new StFlowSelection(flowSelect); //copy constructor
  mOldMethod=kFALSE;
}

//-----------------------------------------------------------------------

StFlowCumulantMaker::~StFlowCumulantMaker() {
}

//-----------------------------------------------------------------------

Int_t StFlowCumulantMaker::Make() {
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
      gMessMgr->Info("##### FlowCumulantAnalysis: FlowTag pointer null");
      FillFromFlowEvent();                   // get event quantities
      FillEventHistograms();                 // fill from FlowEvent
    } else {
      gMessMgr->Info("##### FlowCumulantAnalysis: FlowEvent and FlowTag pointers null");
      return kStOK;
    }
    // Particle quantities
    if (pFlowEvent) FillParticleHistograms(); // fill particle histograms
    
    if (Debug()) StMaker::PrintInfo();
  }
  
  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StFlowCumulantMaker::Init() {
  // Book histograms

  float ptMaxPart = 2.;
  if (pFlowSelect->PtMaxPart()) {
    ptMaxPart = pFlowSelect->PtMaxPart();
  }
  xLabel = "Pseudorapidity";
  if (strlen(pFlowSelect->PidPart()) != 0) { xLabel = "Rapidity"; }

  // Commit with these values.
  const float triggerMin      =    0.;
  const float triggerMax      =   10.;
  const float etaMin          =  -4.5;
  const float etaMax          =   4.5;
  const float ptMin           =    0.;
  //  const float ptMax           =    2.;
  const float ptMax           =    8.;
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
  const float multMin         =    0.;
  const float multMax         = 2000.;
  const float pidMin          =  -10.;
  const float pidMax          =   10.;
  const float centMin         =  -0.5;
  const float centMax         =   9.5;
  const float pMin            =  -2.5;
  const float pMax            =   1.5;
  const float dEdxMax       = 0.00004;


  enum { // commit with this value
	 nTriggerBins      = 11,
	 nEtaBins          = 90,
         //nEtaBins          = 30,
	 //	 nPtBins           = 40,
	 nPtBins           = 40,
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
	 nMomenBins       = 200
  };
  

  if (mOldMethod) r0=0.06;
   else  r0=1.5; //  this number should be small, but it could bring numerical 
                 //error if it is too small.

   m_M=1;  //  if m_M =2, what measured is v2,v4, v6... etc. for harmonic 1,2,3
           //  m_M=2 is not working. do not know why at the moment.
 

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
      nEtaBins, etaMin, etaMax, nPtBins, ptMin, ptMax, nPhi3DBins,
      phiMin, phiMax);
  mHistEtaPtPhi3D->SetXTitle("Eta");
  mHistEtaPtPhi3D->SetYTitle("Pt (GeV)");
  mHistEtaPtPhi3D->SetZTitle("Phi (rad)");
    
  // Yield for all particles
  mHistYieldAll2D = new TH2D("Flow_YieldAll2D", "Flow_YieldAll2D",
    nEtaBins, etaMin, etaMax, nPtBins, ptMin, ptMax);
  mHistYieldAll2D->Sumw2();
  mHistYieldAll2D->SetXTitle("Pseudorapidty");
  mHistYieldAll2D->SetYTitle("Pt (GeV)");

  // Yield for particles correlated with the event plane
  mHistYieldPart2D = new TH2D("Flow_YieldPart2D", "Flow_YieldPart2D",
    nEtaBins, etaMin, etaMax, nPtBins, ptMin, ptMaxPart);
  mHistYieldPart2D->Sumw2();
  mHistYieldPart2D->SetXTitle((char*)xLabel.Data());
  mHistYieldPart2D->SetYTitle("Pt (GeV)");

  // Mean Eta in each bin
  mHistBinEta = new TProfile("Flow_Bin_Eta", "Flow_Bin_Eta",
    nEtaBins, etaMin, etaMax, etaMin, etaMax, "");
  mHistBinEta->SetXTitle((char*)xLabel.Data());
  mHistBinEta->SetYTitle("<Eta>");
  
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
  mHistPidMult->SetXTitle("All, Pi+, Pi-, Proton, Pbar, K+, K-, d, dbar, e-, e+");
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


  for (int k = 0; k < Flow::nSels; k++) {
    char countSels[2];
    sprintf(countSels,"%d",k+1);

    // for each selection

      histFull[k].mHistCumulant
           = new TProfile*[Flow::nCumulantDifferentialOrders];


     for (int ddd=0; ddd<Flow::nCumulantDifferentialOrders; ddd++){
      char theCumulantOrder[2]; //if >10, need to use char*
        sprintf(theCumulantOrder,"%d",(ddd+1)*2);
      histTitle = new TString("Flow_Cumulant_Order"); 
      histTitle->Append(*theCumulantOrder);           
      histTitle->Append("_Sel");                      
      histTitle->Append(*countSels);          
      histFull[k].mHistCumulant[ddd] =  
          new TProfile(histTitle->Data(), histTitle->Data(), Flow::nHars, 0.5, (float)(Flow::nHars) + 0.5, -1.*FLT_MAX, FLT_MAX, "");
      histFull[k].mHistCumulant[ddd]->SetXTitle("harmonic");
      delete histTitle;
     }



    
    // for each harmonic
    for (int j = 0; j < Flow::nHars; j++) {

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
      // Tpc
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
      
      // Ftpc (east)
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
      
      // Ftpc (west)
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
      // Tpc
      histTitle = new TString("Flow_Phi_Weight_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhiWgt =  new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBins, phiMin, phiMax);
      histFull[k].histFullHar[j].mHistPhiWgt->Sumw2();
      histFull[k].histFullHar[j].mHistPhiWgt->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histFullHar[j].mHistPhiWgt->SetYTitle("PhiWgt");
      delete histTitle;
      
      // Ftpc (east)
      histTitle = new TString("Flow_Phi_Weight_FtpcEast_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhiWgtFtpcEast =  new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBinsFtpc, phiMin, phiMax);
      histFull[k].histFullHar[j].mHistPhiWgtFtpcEast->Sumw2();
      histFull[k].histFullHar[j].mHistPhiWgtFtpcEast->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histFullHar[j].mHistPhiWgtFtpcEast->SetYTitle("PhiWgt");
      delete histTitle;
      
      // Ftpc (west)
      histTitle = new TString("Flow_Phi_Weight_FtpcWest_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistPhiWgtFtpcWest =  new TH1D(histTitle->Data(),
        histTitle->Data(), Flow::nPhiBinsFtpc, phiMin, phiMax);
      histFull[k].histFullHar[j].mHistPhiWgtFtpcWest->Sumw2();
      histFull[k].histFullHar[j].mHistPhiWgtFtpcWest->SetXTitle
	("Azimuthal Angles (rad)");
      histFull[k].histFullHar[j].mHistPhiWgtFtpcWest->SetYTitle("PhiWgt");
      delete histTitle;
      

      // Phi lab flattened
      // Tpc
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

      // Ftpc (east)
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
      
      // Ftpc (west)
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
      
      // Yield
      histTitle = new TString("Flow_Yield2D_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistYield2D =	new TH2D(histTitle->Data(),
        histTitle->Data(), nEtaBins, etaMin, etaMax, nPtBins, ptMin, ptMax);
      histFull[k].histFullHar[j].mHistYield2D->Sumw2();
      histFull[k].histFullHar[j].mHistYield2D->SetXTitle("Pseudorapidty");
      histFull[k].histFullHar[j].mHistYield2D->SetYTitle("Pt (GeV)");
      delete histTitle;

    //cumulant 
    //            ****  for differential flow   ****

    // cumulant        dp/n{k}      
    //p is the harmonic with which we want to look at differential flow 
    //n is the harmonic of integrated flow whith which differential flow
    //is measured with respect to. p = either n or 2n.

    //if m_M=1, k=2    dn/n{2}  : cumulant from 2-part corr.
    //if m_M=1, k=4    dn/n{4}  : cumulant from 4-part corr.
    //if m_M=2, k=2    d2n/n{3} : cumulant from 3-part corr., mixed harmonic
    //if m_M=2, k=4    d2n/n{5} : cumulant from 5-part corr., mixed harmonic
    //where {2},{3} corresponds to theCumulantOrder=1 below.
    //{4},{5} corresponds to theCumulantOrder=2 below.

      histFull[k].histFullHar[j].mHistCumulant2D  =  
          new TProfile2D*[Flow::nCumulantDifferentialOrders];
      histFull[k].histFullHar[j].mHistCumulantEta = 
          new TProfile*[Flow::nCumulantDifferentialOrders];
      histFull[k].histFullHar[j].mHistCumulantPt  = 
          new TProfile*[Flow::nCumulantDifferentialOrders];


     for (int ddd=0; ddd<Flow::nCumulantDifferentialOrders; ddd++){

      char theCumulantOrder[2]; //if >10, need to use char*

        sprintf(theCumulantOrder,"%d",(ddd+1)*2);

      histTitle = new TString("Flow_Cumulant2D_Order");
      histTitle->Append(*theCumulantOrder);
      histTitle->Append("_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistCumulant2D[ddd] =	
        new TProfile2D(histTitle->Data(),histTitle->Data(), nEtaBins, etaMin, etaMax, nPtBins, ptMin, ptMaxPart, -1.*FLT_MAX, FLT_MAX, "");
      histFull[k].histFullHar[j].mHistCumulant2D[ddd]->SetXTitle((char*)xLabel.Data());
      histFull[k].histFullHar[j].mHistCumulant2D[ddd]->SetYTitle("Pt (GeV)");
      delete histTitle;


      histTitle = new TString("Flow_CumulantEta_Order");
      histTitle->Append(*theCumulantOrder);
      histTitle->Append("_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistCumulantEta[ddd] =  
            new TProfile(histTitle->Data(),histTitle->Data(), nEtaBins, etaMin, etaMax, -1.*FLT_MAX, FLT_MAX, "");
      histFull[k].histFullHar[j].mHistCumulantEta[ddd]->SetXTitle((char*)xLabel.Data());
      delete histTitle;

      histTitle = new TString("Flow_CumulantPt_Order");
      histTitle->Append(*theCumulantOrder);
      histTitle->Append("_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistCumulantPt[ddd] =  
          new TProfile(histTitle->Data(), histTitle->Data(), nPtBins, ptMin, ptMax, -1.*FLT_MAX, FLT_MAX, "");
      histFull[k].histFullHar[j].mHistCumulantPt[ddd]->SetXTitle("Pt (GeV/c)");
      delete histTitle;



     }


      histFull[k].histFullHar[j].mCumuDifferentialG0Denominator2D = 
          new TProfile2D*[Flow::nCumulantDifferentialOrders*Flow::nCumulantDifferential_qMax];
      histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorEta = 
          new TProfile*[Flow::nCumulantDifferentialOrders*Flow::nCumulantDifferential_qMax];
      histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorPt = 
          new TProfile*[Flow::nCumulantDifferentialOrders*Flow::nCumulantDifferential_qMax];



      for (int ifcn =0; ifcn <  Flow::nCumulantDifferentialOrders*Flow::nCumulantDifferential_qMax; ifcn++){ 

	TString* histTitleDifferentialDenomPt; //for read in

	int cumulantIndex = (ifcn)/(Flow::nCumulantDifferential_qMax)+1; //like 1,2,3. (for cumulant order 2,4,6) not begin with 0. which is "p" in Eq. (B1) in the paper.
        int qIndex        = (ifcn)%(Flow::nCumulantDifferential_qMax); //like 0,1,..._qMax-1  begin with 0. which is "q" in Eq. (B3) in the paper.

	char theCumulantOrderChar[2];
        char qIndexOrderChar[2]; //if >10, need to use char*

	sprintf(theCumulantOrderChar,"%d",cumulantIndex*2); 
        sprintf(qIndexOrderChar,"%d",qIndex);

      histTitle = new TString("Flow_");
      histTitle->Append("CumulantDenom2D_Order");
      histTitle->Append(*theCumulantOrderChar);
      histTitle->Append("_GenFcnqIdx");
      histTitle->Append(*qIndexOrderChar);
      histTitle->Append("_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mCumuDifferentialG0Denominator2D[ifcn] =
            new TProfile2D(histTitle->Data(),histTitle->Data(), nEtaBins, etaMin, etaMax, nPtBins, ptMin, ptMaxPart,-1.*FLT_MAX, FLT_MAX, "");
      histFull[k].histFullHar[j].mCumuDifferentialG0Denominator2D[ifcn]->SetXTitle((char*)xLabel.Data());
      histFull[k].histFullHar[j].mCumuDifferentialG0Denominator2D[ifcn]->SetYTitle("Pt (GeV)");
      delete histTitle;

      histTitle = new TString("Flow_");
      histTitle->Append("CumulantDenomEta_Order");
      histTitle->Append(*theCumulantOrderChar);
      histTitle->Append("_GenFcnqIdx");
      histTitle->Append(*qIndexOrderChar);
      histTitle->Append("_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorEta[ifcn] =
           new TProfile(histTitle->Data(),histTitle->Data(), nEtaBins, etaMin, etaMax, -1.*FLT_MAX, FLT_MAX, "");
      histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorEta[ifcn]->SetXTitle((char*)xLabel.Data());
      delete histTitle;

      histTitle = new TString("Flow_");
      histTitle->Append("CumulantDenomPt_Order");
      histTitle->Append(*theCumulantOrderChar);
      histTitle->Append("_GenFcnqIdx");
      histTitle->Append(*qIndexOrderChar);
      histTitle->Append("_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorPt[ifcn] = 
            new TProfile(histTitle->Data(),histTitle->Data(), nPtBins, ptMin, ptMax, -1.*FLT_MAX, FLT_MAX, "");
      histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorPt[ifcn]->SetXTitle("Pt (GeV/c)");
      histTitleDifferentialDenomPt = new TString(histTitle->Data());
      delete histTitle;

      TFile f("denominator.root","R"); //do not move this and f.close() around

      if (f.IsOpen()) {

      f.cd();

      TProfile* tempDenomPtProfile = 
         (TProfile *)f.Get(histTitleDifferentialDenomPt->Data());
      delete  histTitleDifferentialDenomPt;     

      double tempDenomIntegral =0.;
      double tempDenomInteNoZeroBins =0.;

      for (int theBin =0; theBin<tempDenomPtProfile->GetNbinsX(); theBin++){

        if (tempDenomPtProfile->GetBinContent(theBin)){
          tempDenomIntegral +=(tempDenomPtProfile->GetBinContent(theBin))*
                               (tempDenomPtProfile->GetBinEntries(theBin));
          tempDenomInteNoZeroBins +=tempDenomPtProfile->GetBinEntries(theBin);

        }
      } //could use TProfile::GetMean(2), but it seems there is bug in ROOT.

      histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorRead[ifcn]
        = tempDenomIntegral/tempDenomInteNoZeroBins;


      f.Close();

      } else {
	cout<<"denominator.root is not present, assumming this run is just for producing denominator.root. that means cumulant flow result in flow.hist.root is nonsense for this run."<<endl;

  histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorRead[ifcn]=1.; 
      }

      Double_t theTempPhi =  twopi*((Double_t)(qIndex))/((Double_t)(Flow::nCumulantDifferential_qMax)); 
      Double_t theRz = r0*sqrt(cumulantIndex);
      histFull[k].histFullHar[j].mDifferentialXz[ifcn]= theRz*cos(theTempPhi);
      histFull[k].histFullHar[j].mDifferentialYz[ifcn]= theRz*sin(theTempPhi);

      }




      //      for integrated flow stuff  

      for (int ifcn =0; ifcn <  Flow::nCumulantIntegratedOrders*Flow::nCumulantIntegrated_qMax; ifcn++){   

	int cumulantIndex = (ifcn)/(Flow::nCumulantIntegrated_qMax)+1; //like 1,2,3.  not begin with 0. which is "p" in Eq. (B1) in the paper.
        int qIndex        = (ifcn)%(Flow::nCumulantIntegrated_qMax); //like 0,1,...5  begin with 0. just lie Eq. (B3) in the paper.


      Double_t theTempPhi =  twopi*((Double_t)(qIndex))/((Double_t)(Flow::nCumulantIntegrated_qMax)); 
      Double_t theRz = r0*sqrt(cumulantIndex);

      histFull[k].histFullHar[j].mIntegratedXz[ifcn]= theRz*cos(theTempPhi);
      histFull[k].histFullHar[j].mIntegratedYz[ifcn]= theRz*sin(theTempPhi);

      histFull[k].histFullHar[j].mCumuIntegratedG0[ifcn] = 0.;

      }

      histFull[k].histFullHar[j].mMultSum=0.;
      histFull[k].histFullHar[j].mWgtMultSum_q4=0.;
      histFull[k].histFullHar[j].mWgtMultSum_q6=0.;


    }
  }

  gMessMgr->SetLimit("##### FlowCumulantAnalysis", 2);
  gMessMgr->Info("##### FlowCumulantAnalysis: $Id: StFlowCumulantMaker.cxx,v 1.1 2001/11/02 04:47:42 aihong Exp $");

  return StMaker::Init();
}

//-----------------------------------------------------------------------

void StFlowCumulantMaker::FillFromTags() {
//   Get the flow tags and calculate the full event quantities

  int nSels = 2, nHars = 6;

  for (int j = 0; j < nHars; j++) {

    mMultSub[0][j] = pFlowTag->na[j];
    mMultSub[1][j] = pFlowTag->nb[j];
    mMultSub[2][j] = pFlowTag->nc[j];
    mMultSub[3][j] = pFlowTag->nd[j];

    // full event quantities
    for (int k = 0; k < nSels; k++) {
      mMult[k][j] = mMultSub[nSels*k][j] + mMultSub[nSels*k+1][j];
    }
  }

}

//-----------------------------------------------------------------------

void StFlowCumulantMaker::FillFromFlowEvent() {
  // Get event quantities from StFlowEvent
  for (int k = 0; k < Flow::nSels; k++) {
    pFlowSelect->SetSelection(k);
    for (int j = 0; j < Flow::nHars; j++) {
      pFlowSelect->SetHarmonic(j);

      pFlowSelect->SetSubevent(-1);
      // full event quantities
      mMult[k][j]       = pFlowEvent->Mult(pFlowSelect);
      mWgtMult_q4[k][j] = pFlowEvent->WgtMult_q4(pFlowSelect);              
      mWgtMult_q6[k][j] = pFlowEvent->WgtMult_q6(pFlowSelect);              

    }
  }
}

//-----------------------------------------------------------------------

void StFlowCumulantMaker::FillEventHistograms() {
  // Fill histograms with event quantities

  unsigned int triggerWord = pFlowEvent->L0TriggerWord();
  float trigger;
  switch (triggerWord) {
  case 4096: trigger = 1.; break; // minbias
  case 4352: trigger = 2.; break; // central
  case 61952: trigger = 3.; break; // laser
  default: trigger = 10.; break; // no clue
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

    for (int k = 0; k < Flow::nSels; k++) {
      pFlowSelect->SetSelection(k);
      for (int j = 0; j < Flow::nHars; j++) {
	pFlowSelect->SetHarmonic(j);

      histFull[k].histFullHar[j].mHistMult->Fill((float)mMult[k][j]);
      histFull[k].histFullHar[j].mMultSum +=float(mMult[k][j]);
      histFull[k].histFullHar[j].mWgtMultSum_q4 +=mWgtMult_q4[k][j];
      histFull[k].histFullHar[j].mWgtMultSum_q6 +=mWgtMult_q6[k][j];

      histFull[k].histFullHar[j].mNEvent++;

      for (int ifcn =0; ifcn <  Flow::nCumulantIntegratedOrders*Flow::nCumulantIntegrated_qMax; ifcn++){   
     
	if (mOldMethod)
      histFull[k].histFullHar[j].mCumuIntegratedG0[ifcn] += 
	pFlowEvent->G_Old( pFlowSelect,  
                       histFull[k].histFullHar[j].mIntegratedXz[ifcn], 
                       histFull[k].histFullHar[j].mIntegratedYz[ifcn] );
        else
      histFull[k].histFullHar[j].mCumuIntegratedG0[ifcn] += 
	pFlowEvent->G_New( pFlowSelect,  
                       histFull[k].histFullHar[j].mIntegratedXz[ifcn], 
                       histFull[k].histFullHar[j].mIntegratedYz[ifcn] );
      }
     }
    }

}

//-----------------------------------------------------------------------

void StFlowCumulantMaker::FillParticleHistograms() {
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
  


//In the view of a good coding, the following block should be placed inside
//the track loop, It is put here to reduce run time.
//If sb. change the pFlowSelect to select tracks for v-part, 
// he/she has to change pFlowSelect in this block for the consistency. 

  double* theEvtCrossTerm[Flow::nSels][Flow::nHars];
  double  theSqrtOfSumWgtSqr[Flow::nSels][Flow::nHars];


   for (int k = 0; k < Flow::nSels; k++) 
     for (int j = 0; j < Flow::nHars; j++) 
       theEvtCrossTerm[k][j] = 
 new double[Flow::nCumulantDifferentialOrders*Flow::nCumulantDifferential_qMax];

   for (int k = 0; k < Flow::nSels; k++) 
     for (int j = 0; j < Flow::nHars; j++) {
        pFlowSelect->SetSelection(k);
	pFlowSelect->SetHarmonic(j);

	theSqrtOfSumWgtSqr[k][j] = pFlowEvent->SumWeightSquare(pFlowSelect);

	    if (theSqrtOfSumWgtSqr[k][j]<0) 
            theSqrtOfSumWgtSqr[k][j]=double(mMult[k][j]);

	theSqrtOfSumWgtSqr[k][j]=sqrt( theSqrtOfSumWgtSqr[k][j] );   
        


       for (int ifcn =0; ifcn<Flow::nCumulantDifferentialOrders*Flow::nCumulantDifferential_qMax; ifcn++){

	theEvtCrossTerm[k][j][ifcn]= (mOldMethod) ?
        (pFlowEvent->G_Old( pFlowSelect,
                       histFull[k].histFullHar[j].mDifferentialXz[ifcn],
		       histFull[k].histFullHar[j].mDifferentialYz[ifcn] )) :
        (pFlowEvent->G_New( pFlowSelect,
                       histFull[k].histFullHar[j].mDifferentialXz[ifcn],
                       histFull[k].histFullHar[j].mDifferentialYz[ifcn] )) ;

       } 
     }
//////////////////end of the block




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
      
      mHistDcaTpc      ->Fill(dca);
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
	if (strcmp(pid, "proton") == 0) {
	  mHistMeanDedxProton2D->Fill(logp, dEdx);
	  mHistPidProtonPart->Fill(proton);
	}
	float deuteron  = pFlowTrack->PidDeuteron();
	mHistPidDeuteron->Fill(deuteron);
	if (strcmp(pid, "d") == 0) {
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
	if (strcmp(pid, "pbar") == 0) {
	  mHistMeanDedxPbar2D->Fill(logp, dEdx);
	  mHistPidAntiProtonPart->Fill(antiproton);
	}
	float antideuteron  = pFlowTrack->PidAntiDeuteron();
	mHistPidAntiDeuteron->Fill(antideuteron);
	if (strcmp(pid, "dbar") == 0) {
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

    for (int k = 0; k < Flow::nSels; k++) {
      pFlowSelect->SetSelection(k);
       double cumuTemp[Flow::nCumulantDifferentialOrders];
       double cumuTempFlip[Flow::nCumulantDifferentialOrders];
       double order;
       double phiWgt;
      for (int j = 0; j < Flow::nHars; j++) {
	bool oddHar = (j+1) % 2;
	pFlowSelect->SetHarmonic(j);
	      order  = (double)(j+1);

	if (pFlowSelect->Select(pFlowTrack)) {
	  // Get detID
	  StDetectorId detId;
	  if (pFlowTrack->TopologyMap().numberOfHits(kTpcId) || 
	      (pFlowTrack->TopologyMap().data(0) == 0 && 
	       pFlowTrack->TopologyMap().data(1) == 0)) {
	    // Tpc track, or TopologyMap not available
	    detId = kTpcId;
	  } else if (pFlowTrack->TopologyMap().numberOfHits(kFtpcEastId)) {
	    detId = kFtpcEastId;
	  } else if (pFlowTrack->TopologyMap().numberOfHits(kFtpcWestId)) {
	    detId = kFtpcWestId;
	  } else {
	    detId = kUnknownId;
	  }

	  // Get phiWgt
	  phiWgt = pFlowEvent->PhiWeight(phi, k, j, 
						pFlowTrack->TopologyMap());
	  if (detId == kFtpcEastId) {
	    histFull[k].histFullHar[j].mHistPhiFlatFtpcEast->Fill(phi, phiWgt);
	  } else if (detId == kFtpcWestId) {
	    histFull[k].histFullHar[j].mHistPhiFlatFtpcWest->Fill(phi, phiWgt);
	  } else { // (detId == kTpcId) or otherwise !!!
	    histFull[k].histFullHar[j].mHistPhiFlat->Fill(phi, phiWgt);
	  }


	  if (!pFlowEvent->EtaSubs()) {
	    if (eta < 0 && oddHar) phiWgt *= -1.;
	    if (pFlowEvent->PtWgt()) phiWgt *= pt;
	  }
	  
	  // Fill histograms with selections
	  if (detId == kFtpcEastId) {
	    histFull[k].histFullHar[j].mHistPhiFtpcEast->Fill(phi);
	  } else if (detId == kFtpcWestId) {
	    histFull[k].histFullHar[j].mHistPhiFtpcWest->Fill(phi);
	  } else { // (detId == kTpcId) or otherwise !!!
	    histFull[k].histFullHar[j].mHistPhi->Fill(phi);
	  }
	  histFull[k].histFullHar[j].mHistYield2D->Fill(eta, pt);
	}

       	// Caculate v for all particles selected for correlation analysis
	if (pFlowSelect->SelectPart(pFlowTrack)) {
	  corrMultN++;

	  float YOrEta = 
             (strlen(pFlowSelect->PidPart()) != 0) ?  pFlowTrack->Y() : eta;

	  double Dp[Flow::nCumulantDifferentialOrders]; // the Dp in (D6)
	    for (int ifcn=0; ifcn<Flow::nCumulantDifferentialOrders; ifcn++)
              Dp[ifcn]=0.;


       for (int ifcn =0; ifcn <  Flow::nCumulantDifferentialOrders*Flow::nCumulantDifferential_qMax; ifcn++){   

	int theCumulantOrder = (ifcn)/(Flow::nCumulantDifferential_qMax)+1; //like 1,2.  not begin with 0. which is "p" in (B6)
        int qIndex        = (ifcn)%(Flow::nCumulantDifferential_qMax); //like 0,1,...5  begin with 0. which is "q" in (B6)

        double theCoeff=pow(r0*sqrt(theCumulantOrder),m_M)/float(Flow::nCumulantDifferential_qMax); //first term in (B7)
        double theCosTerm=cos(twopi*float(qIndex)*float(m_M)/float(Flow::nCumulantDifferential_qMax)); //cos() in (B7)
        double theSinTerm=sin(twopi*float(qIndex)*float(m_M)/float(Flow::nCumulantDifferential_qMax)); //sin() in (B7)

	double theCrossterm = 
	  theEvtCrossTerm[k][j][ifcn]; //Gn(Zp,q) in (B6)


          if ( (pFlowSelect->SelectPart(pFlowTrack)) && 
               (pFlowSelect->Select(pFlowTrack)) ) { //remove auto term.

            if (mOldMethod)            
	    theCrossterm /= exp( (phiWgt/theSqrtOfSumWgtSqr[k][j]) *(2.*histFull[k].histFullHar[j].mDifferentialXz[ifcn]*cos(phi * order) + 2. *histFull[k].histFullHar[j].mDifferentialYz[ifcn]*sin(phi * order) ) );
            else
	    theCrossterm /= (1. + (phiWgt/mMult[k][j]) *(2.*histFull[k].histFullHar[j].mDifferentialXz[ifcn]*cos(phi * order) + 2. *histFull[k].histFullHar[j].mDifferentialYz[ifcn]*sin(phi * order) ) ); //the argument in the last paragraph in page 9. 

	  }

       double theXpq = ( theCrossterm*cos(float(m_M) * order * (phi)) )/ //(B6)
      histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorRead[ifcn]; 
       double theYpq = ( theCrossterm*sin(float(m_M) * order * (phi)) )/ //(B6)
      histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorRead[ifcn];



     Dp[theCumulantOrder-1] 
        += theCoeff*(theCosTerm*theXpq+theSinTerm*theYpq);//(B7)

     //for writting out denomXpqYpq, which is the denominator in (B6)
      histFull[k].histFullHar[j].mCumuDifferentialG0Denominator2D[ifcn]->
       Fill(YOrEta, pt, theCrossterm ); 
      histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorPt[ifcn]->
       Fill(pt, theCrossterm ); 
      histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorEta[ifcn]->
	Fill(YOrEta, theCrossterm );  
       }

       if (m_M==1){
         cumuTemp[0]=((2.*Dp[1-1])-(0.5*Dp[2-1]))/(r0*r0); //(B9)
         cumuTemp[1]=((-2.*Dp[1-1])+Dp[2-1])/(r0*r0*r0*r0);
       }else if (m_M==2){
         cumuTemp[0]=((4.*Dp[1-1])-(0.5*Dp[2-1]))/(r0*r0*r0*r0); //(B10)
         cumuTemp[1]=((-6.*Dp[1-1])+(1.5*Dp[2-1]))/(r0*r0*r0*r0*r0*r0);
       }

       cumuTempFlip[0]=cumuTemp[0];
       cumuTempFlip[1]=cumuTemp[1];
      if (eta < 0 && oddHar) {cumuTempFlip[0] *=-1.; cumuTempFlip[1] *=-1.;}

       for (int uuu=0; uuu<Flow::nCumulantDifferentialOrders; uuu++) {
      histFull[k].histFullHar[j].mHistCumulant2D[uuu]->
       Fill(YOrEta, pt,cumuTemp[uuu] ); 
      histFull[k].histFullHar[j].mHistCumulantEta[uuu]->
       Fill(YOrEta, cumuTemp[uuu] ); 
      histFull[k].histFullHar[j].mHistCumulantPt[uuu]->
       Fill(pt, cumuTempFlip[uuu] ); 
        }	  

       for (int uuu=0; uuu<Flow::nCumulantDifferentialOrders; uuu++)
      histFull[k].mHistCumulant[uuu]->Fill(order, cumuTempFlip[uuu] );

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

Int_t StFlowCumulantMaker::Finish() {
  // Calculates resolution and mean flow values
  // Fits q distribution and outputs phiWgt values
  TString* histTitle;

  // PhiWgt histogram collection
  TOrdCollection* phiWgtHistNames = new TOrdCollection(Flow::nSels*Flow::nHars);
  TOrdCollection* XpqYpqDenomNames = new TOrdCollection(Flow::nSels*Flow::nHars); 


  for (int k = 0; k < Flow::nSels; k++) {
    char countSels[2];
    sprintf(countSels,"%d",k+1);

    cout << "##### selection "<<k+1<<"  #### "<<endl;

      //integrated flow from cumulant
       histFull[k].mHist_v
           = new TH1D*[Flow::nCumulantDifferentialOrders];

      for (int ddd =0; ddd<Flow::nCumulantDifferentialOrders; ddd++){

      char theCumulantOrder[2]; //if >10, need to use char*
      sprintf(theCumulantOrder,"%d",(ddd+1)*2);

      histTitle = new TString("Flow_v_cumulantOrder");
      histTitle->Append(*theCumulantOrder);
      histTitle->Append("_Sel");
      histTitle->Append(*countSels);
       histFull[k].mHist_v[ddd] = 
                new  TH1D(*(histFull[k].mHistCumulant[ddd]->ProjectionX(histTitle->Data(),"e")));
       histFull[k].mHist_v[ddd]->SetTitle(histTitle->Data());
       histFull[k].mHist_v[ddd]->SetXTitle("harmonic");
       histFull[k].mHist_v[ddd]->SetYTitle("v (%)");
      delete histTitle;
    AddHist(histFull[k].mHist_v[ddd]);

   }

      double  meanIntegratedV[Flow::nHars];     //V**1
      double  meanIntegratedV2[Flow::nHars];    //V**2
      double  meanIntegratedV3[Flow::nHars];    //V**3
      double  meanIntegratedV4[Flow::nHars];    //V**4
      double  cumulantIntegrated1[Flow::nHars]; //outside of harmonic loop
      double  cumulantIntegrated2[Flow::nHars];
      double  cumulantIntegrated3[Flow::nHars];
      double  q2[Flow::nHars]; //for old method. <Q>**2 in (74) old paper.
      double  q4[Flow::nHars];
      double  q6[Flow::nHars];



   for (int j=0; j< Flow::nHars; j++) {
      meanIntegratedV[j]=0.;
      meanIntegratedV2[j]=0.;
      meanIntegratedV3[j]=0.;
      meanIntegratedV4[j]=0.;
      cumulantIntegrated1[j]=0.;
      cumulantIntegrated2[j]=0.;
      cumulantIntegrated3[j]=0.;
   }

    for (int j = 0; j < Flow::nHars; j++) {
      char countHars[2];
      sprintf(countHars,"%d",j+1);

      //for cumulant method
      Double_t mAvMult = //average multiplicity
     float(histFull[k].histFullHar[j].mMultSum)/
      (float(histFull[k].histFullHar[j].mNEvent));

      Double_t mAvWgtMult_q4 = //for getting q4 with wgt
     float(histFull[k].histFullHar[j].mWgtMultSum_q4)/
      (float(histFull[k].histFullHar[j].mNEvent));

      Double_t mAvWgtMult_q6 = //for getting q6 with wgt
     float(histFull[k].histFullHar[j].mWgtMultSum_q6)/
      (float(histFull[k].histFullHar[j].mNEvent));




      Double_t CpIntegrated[Flow::nCumulantIntegratedOrders]; //Cp in (B4)

      for (int ifcn =0; ifcn < Flow::nCumulantIntegratedOrders; ifcn ++)
	CpIntegrated[ifcn]=0.;
      for (int ifcn =0; ifcn <  Flow::nCumulantIntegratedOrders*Flow::nCumulantIntegrated_qMax; ifcn++){   
	int theCumulantOrder = (ifcn)/(Flow::nCumulantIntegrated_qMax)+1; //like 1,2,3.  not begin with 0. which is "p" in (B3)
	//        int qIndex        = (ifcn)%(Flow::nCumulantIntegrated_qMax); //like 0,1,...5  begin with 0. "q" in (B3)
      histFull[k].histFullHar[j].mCumuIntegratedG0[ifcn] /= 
       float(histFull[k].histFullHar[j].mNEvent);        //<Gn(z)> 

      if (mOldMethod)
      CpIntegrated[theCumulantOrder-1] +=
	(log(histFull[k].histFullHar[j].mCumuIntegratedG0[ifcn])/float(Flow::nCumulantIntegrated_qMax));
      else
      CpIntegrated[theCumulantOrder-1] +=
	(mAvMult*(pow(histFull[k].histFullHar[j].mCumuIntegratedG0[ifcn],1./mAvMult)-1.)/float(Flow::nCumulantIntegrated_qMax)); //(B3) 
      }


//add Xpq Ypq denominator to write out file list
      for (int ifcn =0; ifcn <  Flow::nCumulantDifferentialOrders*Flow::nCumulantDifferential_qMax; ifcn++){
      XpqYpqDenomNames->AddLast(histFull[k].histFullHar[j].mCumuDifferentialG0Denominator2D[ifcn]);
      XpqYpqDenomNames->AddLast(histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorPt[ifcn]);
      XpqYpqDenomNames->AddLast(histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorEta[ifcn]);
		    }
         cumulantIntegrated1[j] = //(B5)
            (3.*CpIntegrated[1-1] -
        (3./2.)*CpIntegrated[2-1] +
        (1./3.)*CpIntegrated[3-1])/(r0*r0);
 
         cumulantIntegrated2[j] = 
           ((((-10.)*CpIntegrated[1-1]) + 
            (8.*CpIntegrated[2-1]) - 
            (2.*CpIntegrated[3-1]))/(r0*r0*r0*r0)); 
 
         cumulantIntegrated3[j]=(( (18.*CpIntegrated[1-1]) - (18.*CpIntegrated[2-1]) + (6.*CpIntegrated[3-1]))/(r0*r0*r0*r0*r0*r0));
 

     //now histograms for flow results:
       histFull[k].histFullHar[j].mHist_v2D
= new TH2D*[Flow::nCumulantDifferentialOrders];
       histFull[k].histFullHar[j].mHist_vEta
= new TH1D*[Flow::nCumulantDifferentialOrders];
       histFull[k].histFullHar[j].mHist_vPt
= new TH1D*[Flow::nCumulantDifferentialOrders];


      for (int ddd =0; ddd<Flow::nCumulantDifferentialOrders; ddd++){

      char theCumulantOrder[2]; //if >10, need to use char*
      sprintf(theCumulantOrder,"%d",(ddd+1)*2);

      histTitle = new TString("Flow_v2D_cumulantOrder");
      histTitle->Append(*theCumulantOrder);
      histTitle->Append("_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
       histFull[k].histFullHar[j].mHist_v2D[ddd] = 
                new TH2D(*(histFull[k].histFullHar[j].mHistCumulant2D[ddd]->ProjectionXY(histTitle->Data(),"e")));
       histFull[k].histFullHar[j].mHist_v2D[ddd]->SetTitle(histTitle->Data());
      histFull[k].histFullHar[j].mHist_v2D[ddd]->SetXTitle((char*)xLabel.Data());
      histFull[k].histFullHar[j].mHist_v2D[ddd]->SetYTitle("Pt (GeV)");
      histFull[k].histFullHar[j].mHist_v2D[ddd]->SetZTitle("v (%)");
      delete histTitle;

      histTitle = new TString("Flow_vEta_cumulantOrder");
      histTitle->Append(*theCumulantOrder);
      histTitle->Append("_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
       histFull[k].histFullHar[j].mHist_vEta[ddd] = 
                new  TH1D(*(histFull[k].histFullHar[j].mHistCumulantEta[ddd]->ProjectionX(histTitle->Data(),"e")));
       histFull[k].histFullHar[j].mHist_vEta[ddd]->SetTitle(histTitle->Data());
       histFull[k].histFullHar[j].mHist_vEta[ddd]->SetXTitle((char*)xLabel.Data());
       histFull[k].histFullHar[j].mHist_vEta[ddd]->SetYTitle("v (%)");
      delete histTitle;

      histTitle = new TString("Flow_vPt_cumulantOrder");
      histTitle->Append(*theCumulantOrder);
      histTitle->Append("_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
       histFull[k].histFullHar[j].mHist_vPt[ddd] = 
                new  TH1D(*(histFull[k].histFullHar[j].mHistCumulantPt[ddd]->ProjectionX(histTitle->Data(),"e")));
       histFull[k].histFullHar[j].mHist_vPt[ddd]->SetTitle(histTitle->Data());
       histFull[k].histFullHar[j].mHist_vPt[ddd]->SetXTitle("Pt (GeV)");
       histFull[k].histFullHar[j].mHist_vPt[ddd]->SetYTitle("v (%)");
      delete histTitle;



}

     


       if (mOldMethod) {
           q2[j] = cumulantIntegrated1[j]-1.;  // old paper (Eq. 74a)
  	   q4[j] = -1.*cumulantIntegrated2[j]-(1./mAvWgtMult_q4); 
	   q6[j] = (1./4.)*cumulantIntegrated3[j]-(1./(mAvWgtMult_q6)); 
           meanIntegratedV[j]   = sqrt(q2[j]);        // <Q>  for 2-part,  m=1
           meanIntegratedV2[j]  = q2[j];              //<Q**2>for 2-part , m=2
           meanIntegratedV3[j]  = pow(q4[j],3./4.);   //<Q**3>for 4-part,  m=1
           meanIntegratedV4[j]  = q4[j];              //<Q**4>for 4-part,  m=2
       } else { //new method
          meanIntegratedV[j]     
            = sqrt(cumulantIntegrated1[j]);          //<v>    for 2-part,  m=1
          meanIntegratedV2[j]    
            = cumulantIntegrated1[j];                //<v**2> for 2-part , m=2
          meanIntegratedV3[j]    
            = pow(-1.*cumulantIntegrated2[j],3./4.); //<v**3> 4-part,  m=1
          meanIntegratedV4[j]    
            = -1.*cumulantIntegrated2[j];            //<v**4> 4-part,  m=2
       }


        if (m_M==1){

        histFull[k].histFullHar[j].mHist_v2D[0]->Scale(1./(meanIntegratedV[j]*perCent) ); 	//  (34a)

        histFull[k].histFullHar[j].mHist_v2D[1]->Scale(-1./(meanIntegratedV3[j]*perCent) ); //  (34b)

        histFull[k].histFullHar[j].mHist_vEta[0]->Scale(1./(meanIntegratedV[j]*perCent) ); 	//  (34a)

        histFull[k].histFullHar[j].mHist_vEta[1]->Scale(-1./(meanIntegratedV3[j]*perCent) ); //  (34b)


        histFull[k].histFullHar[j].mHist_vPt[0]->Scale(1./(meanIntegratedV[j]*perCent) ); 	//  (34a)

        histFull[k].histFullHar[j].mHist_vPt[1]->Scale(-1./(meanIntegratedV3[j]*perCent) ); //  (34b)


	}else if(m_M==2){

        histFull[k].histFullHar[j].mHist_v2D[0]->Scale(1./(meanIntegratedV2[j]*perCent) ); 	//  (35a)
        histFull[k].histFullHar[j].mHist_v2D[1]->Scale(-0.5/(meanIntegratedV4[j]*perCent) ); //  (35b)


        histFull[k].histFullHar[j].mHist_vEta[0]->Scale(1./(meanIntegratedV2[j]*perCent) ); 	//  (35a)
        histFull[k].histFullHar[j].mHist_vEta[1]->Scale(-0.5/(meanIntegratedV4[j]*perCent) ); //  (35b)


        histFull[k].histFullHar[j].mHist_vPt[0]->Scale(1./(meanIntegratedV2[j]*perCent) ); 	//  (35a)
        histFull[k].histFullHar[j].mHist_vPt[1]->Scale(-0.5/(meanIntegratedV4[j]*perCent) ); //  (35b)


	}

      for (int asdf=0; asdf<Flow::nCumulantDifferentialOrders;asdf++){
	AddHist(histFull[k].histFullHar[j].mHist_v2D[asdf]);
	AddHist(histFull[k].histFullHar[j].mHist_vEta[asdf]);
	AddHist(histFull[k].histFullHar[j].mHist_vPt[asdf]);
     }










      // Calculate PhiWgt
      double mean = histFull[k].histFullHar[j].mHistPhi->Integral() 
	/ (double)Flow::nPhiBins;
      double meanFtpcEast = histFull[k].histFullHar[j].mHistPhiFtpcEast->Integral() 
	/ (double)Flow::nPhiBinsFtpc;
      double meanFtpcWest = histFull[k].histFullHar[j].mHistPhiFtpcWest->Integral() 
	/ (double)Flow::nPhiBinsFtpc;
      {for (int i = 0; i < Flow::nPhiBins; i++) {
	// Tpc
	histFull[k].histFullHar[j].mHistPhiWgt->SetBinContent(i+1, mean);
	histFull[k].histFullHar[j].mHistPhiWgt->SetBinError(i+1, 0.);
      }}
      {for (int i = 0; i < Flow::nPhiBinsFtpc; i++) {
	// Ftpc (east)
	histFull[k].histFullHar[j].mHistPhiWgtFtpcEast->SetBinContent(i+1, meanFtpcEast);
	histFull[k].histFullHar[j].mHistPhiWgtFtpcEast->SetBinError(i+1, 0.);
	// Ftpc (west)
	histFull[k].histFullHar[j].mHistPhiWgtFtpcWest->SetBinContent(i+1, meanFtpcWest);
	histFull[k].histFullHar[j].mHistPhiWgtFtpcWest->SetBinError(i+1, 0.);
      }}
      // Tpc
      histFull[k].histFullHar[j].mHistPhiWgt->
	Divide(histFull[k].histFullHar[j].mHistPhi);
      phiWgtHistNames->AddLast(histFull[k].histFullHar[j].mHistPhiWgt);
      // Ftpc (east)
      histFull[k].histFullHar[j].mHistPhiWgtFtpcEast->
	Divide(histFull[k].histFullHar[j].mHistPhiFtpcEast);
      phiWgtHistNames->AddLast(histFull[k].histFullHar[j].mHistPhiWgtFtpcEast);
      // Ftpc (west)
      histFull[k].histFullHar[j].mHistPhiWgtFtpcWest->
	Divide(histFull[k].histFullHar[j].mHistPhiFtpcWest);
      phiWgtHistNames->AddLast(histFull[k].histFullHar[j].mHistPhiWgtFtpcWest);


    }



        if (m_M==1){

	  TH1D* hisOfMeanIntegratedV;
	  hisOfMeanIntegratedV = 
            new TH1D(*(histFull[k].mHist_v[0]));
          hisOfMeanIntegratedV->Reset();

	  TH1D* hisOfMeanIntegratedV3;
	  hisOfMeanIntegratedV3 = 
            new TH1D(*(histFull[k].mHist_v[1]));
          hisOfMeanIntegratedV3->Reset();

	  for (int nx = 1; nx <  Flow::nHars+1; nx++){
	    hisOfMeanIntegratedV->SetBinContent(nx, 1./(meanIntegratedV[nx-1]*perCent));
	    hisOfMeanIntegratedV->SetBinError(nx,0.);
	    hisOfMeanIntegratedV3->SetBinContent(nx, -1./(meanIntegratedV3[nx-1]*perCent));
	    hisOfMeanIntegratedV3->SetBinError(nx,0.);
	  }

	  histFull[k].mHist_v[0]->Multiply(hisOfMeanIntegratedV);
          histFull[k].mHist_v[1]->Multiply(hisOfMeanIntegratedV3);

          for (int nx = 1; nx <  Flow::nHars+1; nx++){
          cout << "##### 2-part v" << nx << " = (" 
   << histFull[k].mHist_v[0]->GetBinContent(nx) 
   <<" +/- "<< histFull[k].mHist_v[0]->GetBinError(nx)<<" )"<<endl;
          cout << "##### 4-part v" << nx << " = (" 
   << histFull[k].mHist_v[1]->GetBinContent(nx) 
   <<" +/- "<< histFull[k].mHist_v[1]->GetBinError(nx)<<" )"<<endl;
	  }
          

	  delete hisOfMeanIntegratedV; delete hisOfMeanIntegratedV3;

	}else if(m_M==2){

	  TH1D* hisOfMeanIntegratedV2;
	  hisOfMeanIntegratedV2 = 
            new TH1D(*(histFull[k].mHist_v[0]));
          hisOfMeanIntegratedV2->Reset();

	  TH1D* hisOfMeanIntegratedV4;
	  hisOfMeanIntegratedV4 = 
            new TH1D(*(histFull[k].mHist_v[1]));
          hisOfMeanIntegratedV4->Reset();

	  for (int nx = 1; nx <  Flow::nHars+1; nx++){
	    hisOfMeanIntegratedV2->SetBinContent(nx, 1./(meanIntegratedV2[nx-1]*perCent));
	    hisOfMeanIntegratedV2->SetBinError(nx,0.);
	    hisOfMeanIntegratedV4->SetBinContent(nx, -0.5/(meanIntegratedV4[nx-1]*perCent));
	    hisOfMeanIntegratedV4->SetBinError(nx,0.);
	  }

        histFull[k].mHist_v[0]->Multiply(hisOfMeanIntegratedV2);
        histFull[k].mHist_v[1]->Multiply(hisOfMeanIntegratedV4);
          for (int nx = 1; nx <  Flow::nHars+1; nx++){
          cout << "##### 2-part v" << nx << " = (" 
   << histFull[k].mHist_v[0]->GetBinContent(nx) 
   <<") +/- "<< histFull[k].mHist_v[0]->GetBinError(nx)<<endl;
          cout << "##### 4-part v" << nx << " = (" 
   << histFull[k].mHist_v[1]->GetBinContent(nx) 
   <<") +/- "<< histFull[k].mHist_v[1]->GetBinError(nx)<<endl;
	  }
          

	  delete hisOfMeanIntegratedV2; delete hisOfMeanIntegratedV4;
	}

      for (int asdf=0; asdf<Flow::nCumulantDifferentialOrders;asdf++)
        AddHist( histFull[k].mHist_v[asdf]);




  }
  //GetHistList()->ls();

  // Write all histograms
  TFile histFile("flow.hist.root", "RECREATE");
  //histFile.SetFormat(1);
  TList* hisList = GetHistList(); 
  for (int k = 0; k < Flow::nSels; k++) 
    for (int j =0; j < Flow::nHars; j++)
      for (int ifcn =0; ifcn <  Flow::nCumulantDifferentialOrders*Flow::nCumulantDifferential_qMax; ifcn++){
      hisList->Remove(histFull[k].histFullHar[j].mCumuDifferentialG0Denominator2D[ifcn]);
      hisList->Remove(histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorPt[ifcn]);
      hisList->Remove(histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorEta[ifcn]);
		    }
  hisList->Write();
  histFile.Close();
  
  // Write PhiWgt histograms
  TFile phiWgtNewFile("flowPhiWgtNew.hist.root", "RECREATE");
  //phiWgtNewFile.SetFormat(1);
  phiWgtHistNames->Write();
  phiWgtNewFile.Close();
  delete phiWgtHistNames;

  //write profile for the denominator of Xpq.
  TString*  fileName = new TString("denominatorNew.root");
  TFile XpqYpqDenomNewFile(fileName->Data(),"RECREATE"); 
  XpqYpqDenomNewFile.SetFormat(1);
  XpqYpqDenomNames->Write();
  XpqYpqDenomNewFile.Close();
  delete fileName;
  delete XpqYpqDenomNames;



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
// $Log: StFlowCumulantMaker.cxx,v $
// Revision 1.1  2001/11/02 04:47:42  aihong
// install cumulant maker
//
////////////////////////////////////////////////////////////////////////////
