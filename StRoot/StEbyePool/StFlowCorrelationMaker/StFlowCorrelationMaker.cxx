////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowCorrelationMaker.cxx,v 1.2 2002/02/10 22:12:45 perev Exp $
//
// Authors: Raimond Snellings and Art Poskanzer
//
////////////////////////////////////////////////////////////////////////////
//
// Description:  Maker to analyze Azimuthal Correlations using StFlowEvent
//
////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <math.h>
#include "StMaker.h"
#include "StFlowCorrelationMaker.h"
#include "StFlowMaker/StFlowMaker.h"
#include "StFlowMaker/StFlowEvent.h"
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
#define PR(x) cout << "##### FlowCorrelations: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowCorrelationMaker)


//-----------------------------------------------------------------------

StFlowCorrelationMaker::StFlowCorrelationMaker(const Char_t* name): StMaker(name),
  MakerName(name) {
  pFlowSelect = new StFlowSelection();
}

StFlowCorrelationMaker::StFlowCorrelationMaker(const Char_t* name,
					 const StFlowSelection& flowSelect) :
  StMaker(name), MakerName(name) {
  pFlowSelect = new StFlowSelection(flowSelect); //copy constructor
}

//-----------------------------------------------------------------------

StFlowCorrelationMaker::~StFlowCorrelationMaker() {
}

//-----------------------------------------------------------------------

Int_t StFlowCorrelationMaker::Make() {
  // Make histograms

  // Get a pointer to StFlowEvent
  StFlowMaker* pFlowMaker = NULL;
  TString* makerName = new TString("Flow");
  makerName->Append(pFlowSelect->Number());
  pFlowMaker = (StFlowMaker*)GetMaker(makerName->Data());
  delete makerName;
  if (pFlowMaker) { 
    pFlowEvent = pFlowMaker->FlowEventPointer();
  } else {
    gMessMgr->Info("##### FlowCorrelation: FlowMaker pointer null");
  }
  if (pFlowEvent && pFlowSelect->Select(pFlowEvent)) {     // event selected

    // Event quantities
    if (pFlowEvent) {
      FillFromFlowEvent();                   // get event quantities
      FillEventHistograms();                 // fill from FlowEvent
    } else {
      gMessMgr->Info("##### FlowCorrelation: FlowEvent pointer null");
      return kStOK;
    }
    // Particle quantities
    if (pFlowEvent) FillParticleHistograms(); // fill particle histograms
    
    if (Debug()) StMaker::PrintInfo();
  }
  
  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StFlowCorrelationMaker::Init() {
  // Book histograms

  float ptMaxPart = 2.;
  if (pFlowSelect->PtMaxPart()) {
    ptMaxPart = pFlowSelect->PtMaxPart();
  }
  xLabel = "Pseudorapidity";
  if (strlen(pFlowSelect->PidPart()) != 0) { xLabel = "Rapidity"; }

  // Commit with these values.
  const float etaMin          =  -1.5;
  const float etaMax          =   1.5;
  const float ptMin           =    0.;
  const float ptMax           =    2.;
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
  const float pidMin          =  -10.;
  const float pidMax          =   10.;
  const float centMin         =  -0.5;
  const float centMax         =   9.5;

  enum { // commit with this value
         nEtaBins          = 30,
	 nPtBins           = 40,
	 nChargeBins       = 50,
	 nDcaBins          = 60,
	 nChi2Bins         = 50,
	 nFitPtsBinsTpc    = 61,
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
	 nPhi3DBins        = 18,
	 nPsiBins          = 36,
	 nMultBins         = 40,
	 nPidBins          = 50,
         nCentBins         = 10,
	 nDedxBins         = 1000,
	 nMomenBins        = 400,
	 nDeltaPhiBins     = 40
  };
  
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
    
  // Distance of closest approach for global tracks
  // Tpc
  mHistDcaGlobalTpc = new TH1F("Flow_DcaGlobal_Tpc", "Flow_DcaGlobal_Tpc",
      nDcaBins, dcaMin, glDcaMax);
  mHistDcaGlobalTpc->SetXTitle("Global Track dca (cm)");
  mHistDcaGlobalTpc->SetYTitle("Counts");

  // Chi2
  // Tpc
  mHistChi2Tpc = new TH1F("Flow_Chi2_Tpc", "Flow_Chi2_Tpc",
      nChi2Bins, chi2Min, chi2Max);
  mHistChi2Tpc->SetXTitle("Chi square per df");
  mHistChi2Tpc->SetYTitle("Counts");
  
  // FitPts
  // Tpc
  mHistFitPtsTpc = new TH1F("Flow_FitPts_Tpc", "Flow_FitPts_Tpc",
      nFitPtsBinsTpc, fitPtsMinTpc, fitPtsMaxTpc);
  mHistFitPtsTpc->SetXTitle("Fit Points");
  mHistFitPtsTpc->SetYTitle("Counts");

  // MaxPts
  // Tpc
  mHistMaxPtsTpc = new TH1F("Flow_MaxPts_Tpc ", "Flow_MaxPts_Tpc ",
      nMaxPtsBinsTpc , maxPtsMinTpc , maxPtsMaxTpc );
  mHistMaxPtsTpc ->SetXTitle("Max Points");
  mHistMaxPtsTpc ->SetYTitle("Counts");
  
  // FitOverMax
  // Tpc
  mHistFitOverMaxTpc = new TH1F("Flow_FitOverMax_Tpc", "Flow_FitOverMax_Tpc",
      nFitOverMaxBins, fitOverMaxMin, fitOverMaxMax);
  mHistFitOverMaxTpc->SetXTitle("Fit Points / Max Points");
  mHistFitOverMaxTpc->SetYTitle("Counts");
    
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

  // MeanDedx
  mHistMeanDedx2D = new TH2F("Flow_MeanDedx2D", "Flow_MeanDedx2D",
			   nMomenBins, -5, 5,
			   nDedxBins, 0, 0.00005);
  mHistMeanDedx2D->SetXTitle("momentum/Z (GeV)");
  mHistMeanDedx2D->SetYTitle("mean dEdx");

  // MeanDedx PiPlus
  mHistMeanDedxPiPlus2D = new TH2F("Flow_MeanDedxPiPlus2D", 
				 "Flow_MeanDedxPiPlus2D",
				 nMomenBins, -5, 5,
				 nDedxBins, 0, 0.00005);
  mHistMeanDedxPiPlus2D->SetXTitle("momentum/Z (GeV)");
  mHistMeanDedxPiPlus2D->SetYTitle("mean dEdx");

  // MeanDedxPiMinus
  mHistMeanDedxPiMinus2D = new TH2F("Flow_MeanDedxPiMinus2D", 
				  "Flow_MeanDedxPiMinus2D",
				  nMomenBins, -5, 5,
				  nDedxBins, 0, 0.00005);
  mHistMeanDedxPiMinus2D->SetXTitle("momentum/Z (GeV)");
  mHistMeanDedxPiMinus2D->SetYTitle("mean dEdx");

  // MeanDedxProton
  mHistMeanDedxProton2D = new TH2F("Flow_MeanDedxProton2D", 
				 "Flow_MeanDedxProton2D",
				 nMomenBins, -5, 5,
				 nDedxBins, 0, 0.00005);
  mHistMeanDedxProton2D->SetXTitle("momentum/Z (GeV)");
  mHistMeanDedxProton2D->SetYTitle("mean dEdx");

  // MeanDedxPbar
  mHistMeanDedxPbar2D = new TH2F("Flow_MeanDedxPbar2D", 
			       "Flow_MeanDedxPbar2D",
			       nMomenBins, -5, 5,
			       nDedxBins, 0, 0.00005);
  mHistMeanDedxPbar2D->SetXTitle("momentum/Z (GeV)");
  mHistMeanDedxPbar2D->SetYTitle("mean dEdx");

  // MeanDedxKplus
  mHistMeanDedxKplus2D = new TH2F("Flow_MeanDedxKplus2D", 
				"Flow_MeanDedxKplus2D",
				nMomenBins, -5, 5,
				nDedxBins, 0, 0.00005);
  mHistMeanDedxKplus2D->SetXTitle("momentum/Z (GeV)");
  mHistMeanDedxKplus2D->SetYTitle("mean dEdx");

  // MeanDedxKminus
  mHistMeanDedxKminus2D = new TH2F("Flow_MeanDedxKminus2D", "Flow_MeanDedxKminus2D",
				 nMomenBins, -5, 5,
				 nDedxBins, 0, 0.00005);
  mHistMeanDedxKminus2D->SetXTitle("momentum/Z (GeV)");
  mHistMeanDedxKminus2D->SetYTitle("mean dEdx");

  // MeanDedxDeuteron
  mHistMeanDedxDeuteron2D = new TH2F("Flow_MeanDedxDeuteron2D", 
				   "Flow_MeanDedxDeuteron2D",
				   nMomenBins, -5, 5,
				   nDedxBins, 0, 0.00005);
  mHistMeanDedxDeuteron2D->SetXTitle("momentum/Z (GeV)");
  mHistMeanDedxDeuteron2D->SetYTitle("mean dEdx");

  // MeanDedxAntiDeuteron
  mHistMeanDedxAntiDeuteron2D = new TH2F("Flow_MeanDedxAntiDeuteron2D", 
				       "Flow_MeanDedxAntiDeuteron2D",
				       nMomenBins, -5, 5,
				       nDedxBins, 0, 0.00005);
  mHistMeanDedxAntiDeuteron2D->SetXTitle("momentum/Z (GeV)");
  mHistMeanDedxAntiDeuteron2D->SetYTitle("mean dEdx");

  // MeanDedxElectron
  mHistMeanDedxElectron2D = new TH2F("Flow_MeanDedxElectron2D", 
				   "Flow_MeanDedxElectron2D",
				   nMomenBins, -5, 5,
				   nDedxBins, 0, 0.00005);
  mHistMeanDedxElectron2D->SetXTitle("momentum/Z (GeV)");
  mHistMeanDedxElectron2D->SetYTitle("mean dEdx");

  // MeanDedxPositron
  mHistMeanDedxPositron2D = new TH2F("Flow_MeanDedxPositron2D", 
				   "Flow_MeanDedxPositron2D",
				   nMomenBins, -5, 5,
				   nDedxBins, 0, 0.00005);
  mHistMeanDedxPositron2D->SetXTitle("momentum/Z (GeV)");
  mHistMeanDedxPositron2D->SetYTitle("mean dEdx");


  // Delta Phi
  mHistDeltaPhi = new TH1F("Flow_DeltaPhi", "Flow_DeltaPhi",
      nDeltaPhiBins, 0, 3.14);
  mHistDeltaPhi->SetXTitle("Delta Phi");
  mHistDeltaPhi->SetYTitle("Counts");


  gMessMgr->SetLimit("##### FlowCorrelation", 20);
  gMessMgr->Info("##### FlowCorrelation: $Id: StFlowCorrelationMaker.cxx,v 1.2 2002/02/10 22:12:45 perev Exp $");

  return StMaker::Init();
}

//-----------------------------------------------------------------------

void StFlowCorrelationMaker::FillFromFlowEvent() {
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
    }
  }

}

//-----------------------------------------------------------------------

void StFlowCorrelationMaker::FillEventHistograms() {
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

  mHistCTBvsZDC2D->Fill(pFlowEvent->ZDCe() + pFlowEvent->ZDCe(), 
			                       pFlowEvent->CTB());

}

//-----------------------------------------------------------------------

void StFlowCorrelationMaker::FillParticleHistograms() {
  // Fill histograms from the particles

  float corrMultN  = 0.;
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
  StFlowTrackIterator itr2;
  
  for (itr = pFlowTracks->begin(); itr != pFlowTracks->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;

    Char_t pid[10];
    strcpy(pid, pFlowTrack->Pid());

    // no selections: Charge, Dca, Chi2, FitPts, MaxPts, FitOverMax, PID
    mHistCharge->Fill((float)pFlowTrack->Charge());

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

    mHistDcaTpc->Fill(pFlowTrack->Dca());
    mHistDcaGlobalTpc->Fill(pFlowTrack->DcaGlobal());
    mHistChi2Tpc->Fill(pFlowTrack->Chi2());
    mHistFitPtsTpc->Fill((float)pFlowTrack->FitPts());
    mHistMaxPtsTpc->Fill((float)pFlowTrack->MaxPts());
    if (pFlowTrack->MaxPts()) mHistFitOverMaxTpc->Fill((float)pFlowTrack->FitPts()/(float)pFlowTrack->MaxPts());
    
    mHistMeanDedx2D->Fill(pFlowTrack->P()/(float)pFlowTrack->Charge(),pFlowTrack->Dedx());
	
    if (pFlowTrack->Charge() == 1) {
      float piPlus = pFlowTrack->PidPiPlus();
      mHistPidPiPlus->Fill(piPlus);
      if (strcmp(pid, "pi+") == 0) {
	mHistMeanDedxPiPlus2D->Fill(pFlowTrack->P()/(float)pFlowTrack->Charge(),pFlowTrack->Dedx());
	mHistPidPiPlusPart->Fill(piPlus);
      }
      float kplus  = pFlowTrack->PidKaonPlus();
      mHistPidKplus->Fill(kplus);
      if (strcmp(pid, "k+") == 0) {
	mHistMeanDedxKplus2D->Fill(pFlowTrack->P()/(float)pFlowTrack->Charge(),pFlowTrack->Dedx());
	mHistPidKplusPart->Fill(kplus);
      }
      float proton  = pFlowTrack->PidProton();
      mHistPidProton->Fill(proton);
      if (strcmp(pid, "proton") == 0) {
	mHistMeanDedxProton2D->Fill(pFlowTrack->P()/(float)pFlowTrack->Charge(),pFlowTrack->Dedx());
	mHistPidProtonPart->Fill(proton);
      }
      float deuteron  = pFlowTrack->PidDeuteron();
      mHistPidDeuteron->Fill(deuteron);
      if (strcmp(pid, "d") == 0) {
	mHistMeanDedxDeuteron2D->Fill(pFlowTrack->P()/(float)pFlowTrack->Charge(),pFlowTrack->Dedx());
	mHistPidDeuteronPart->Fill(deuteron);
      }
      float positron  = pFlowTrack->PidPositron();
      mHistPidPositron->Fill(positron);
      if (strcmp(pid, "e+") == 0) {
	mHistMeanDedxPositron2D->Fill(pFlowTrack->P()/(float)pFlowTrack->Charge(),pFlowTrack->Dedx());
	mHistPidPositronPart->Fill(positron);
      }
    } else if (pFlowTrack->Charge() == -1) {
      float piMinus = pFlowTrack->PidPiMinus();
      mHistPidPiMinus->Fill(piMinus);
      if (strcmp(pid, "pi-") == 0) {
	mHistMeanDedxPiMinus2D->Fill(pFlowTrack->P()/(float)pFlowTrack->Charge(),pFlowTrack->Dedx());
	mHistPidPiMinusPart->Fill(piMinus);
      }
      float kminus  = pFlowTrack->PidKaonMinus();
      mHistPidKminus->Fill(kminus);
      if (strcmp(pid, "k-") == 0) {
	mHistMeanDedxKminus2D->Fill(pFlowTrack->P()/(float)pFlowTrack->Charge(),pFlowTrack->Dedx());
	mHistPidKminusPart->Fill(kminus);
      }
      float antiproton  = pFlowTrack->PidAntiProton();
      mHistPidAntiProton->Fill(antiproton);
      if (strcmp(pid, "pbar") == 0) {
	mHistMeanDedxPbar2D->Fill(pFlowTrack->P()/(float)pFlowTrack->Charge(),pFlowTrack->Dedx());
	mHistPidAntiProtonPart->Fill(antiproton);
      }
      float antideuteron  = pFlowTrack->PidAntiDeuteron();
      mHistPidAntiDeuteron->Fill(antideuteron);
      if (strcmp(pid, "dbar") == 0) {
	mHistMeanDedxAntiDeuteron2D->Fill(pFlowTrack->P()/(float)pFlowTrack->Charge(),pFlowTrack->Dedx());
	mHistPidAntiDeuteronPart->Fill(antideuteron);
      }
      float electron  = pFlowTrack->PidElectron();
      mHistPidElectron->Fill(electron);
      if (strcmp(pid, "e-") == 0) {
	mHistMeanDedxElectron2D->Fill(pFlowTrack->P()/(float)pFlowTrack->Charge(),pFlowTrack->Dedx());
	mHistPidElectronPart->Fill(electron);
      }
    }

    // double loop to get delta phi
    for (itr2 = itr; ++itr2 != pFlowTracks->end();) {
      StFlowTrack* pFlowTrack2 = *itr2;
      if (pFlowSelect->SelectPart(pFlowTrack2)) {
	corrMultN++;
	float deltaphi = fabs(pFlowTrack->Phi() - pFlowTrack2->Phi());
	if (deltaphi > twopi/2.) {
	  deltaphi = twopi - deltaphi;
	}
	//	cout << "delta phi: " << deltaphi << endl;
	mHistDeltaPhi->Fill(deltaphi);
      }
    }

  }

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
  mHistMultPart->Fill(corrMultN);

}

//-----------------------------------------------------------------------

Int_t StFlowCorrelationMaker::Finish() {
  //GetHistList()->ls();

  // Write all histograms
  TString* fileName = new TString("flowCorr.hist.root");
  fileName->Prepend(pFlowSelect->Number());
  TFile histFile(fileName->Data(), "RECREATE");
//VP  histFile.SetFormat(1);
  GetHistList()->Write();
  histFile.Close();
  delete fileName;
  
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
// $Log: StFlowCorrelationMaker.cxx,v $
// Revision 1.2  2002/02/10 22:12:45  perev
// Outdated SetFormat removed
//
// Revision 1.1  2001/01/31 19:47:22  snelling
// A simple correlation program so far only used for simulations (no mixing)
//
//  
////////////////////////////////////////////////////////////////////////////
