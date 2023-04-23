// $Id: StdEdxY2Maker.cxx,v 1.101 2021/05/10 16:54:45 fisyak Exp $
//#define __NEGATIVE_ONLY__
  #ifndef  __NEGATIVE_ONLY__
     #define __NEGATIVE_AND_POSITIVE__
  #endif /* ! __NEGATIVE_ONLY__ */
  #define __BEST_VERTEX__
#ifdef __TFG__VERSION__
//#define CompareWithToF 
//#define __CHECK_LargedEdx__
//#define __TEST_DX__
  #define __SpaceCharge__
//#define __LogProb__
//#define __DEBUG_dEdx__
  #define __DEBUG_dNdx__
//#define __ADD_PROB__
//#define __BENCHMARKS__DOFIT_ZN__
  #define __FIT_PULLS__
  #define __CHECK_RDOMAP_AND_VOLTAGE__
  #ifdef __CHECK_RDOMAP_AND_VOLTAGE__
     #include "TProfile3D.h"
  #endif /* __CHECK_RDOMAP_AND_VOLTAGE__ */
//#define __dZdY_dXdY__
//#define __Pad_Tmbk__
#endif /* __TFG__VERSION__ */
#include <Stiostream.h>		 
#include "StdEdxY2Maker.h"
#include "StTpcdEdxCorrection.h" 
#include <vector>
// ROOT
#include "TMinuit.h"
#include "TSystem.h"
#include "TMath.h"
#include "TH2.h"
#include "TH3.h"
#include "THnSparse.h"
#include "TF1.h"
#include "TStyle.h"
#include "TProfile.h"
#include "TProfile2D.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TCanvas.h"
#include "TRandom.h"
#include "TClonesArray.h"
#include "TArrayI.h"
#include "TArrayD.h"
#ifdef __BENCHMARKS__DOFIT_ZN__
#include "TBenchmark.h"
#endif /* __BENCHMARKS__DOFIT_ZN__ */
// StUtilities
#include "StMagF.h"
#include "StDbUtilities/StMagUtilities.h"
#include "StMessMgr.h" 
#include "StBichsel/Bichsel.h"
#include "StBichsel/StdEdxModel.h"
#include "StBichsel/StdEdxPull.h"
#include "StDetectorId.h"
#include "StDedxMethod.h"
// StarClassLibrary
#include "SystemOfUnits.h"
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif
#include "StarClassLibrary/StParticleTypes.hh"

// StDb
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StDbUtilities/StCoordinates.hh" 
#include "StTpcDb/StTpcDb.h"
// StEvent 
#include "StEventTypes.h"
#include "StProbPidTraits.h"
#ifdef CompareWithToF
#include "StBTofPidTraits.h"
#endif
#include "StTpcDedxPidAlgorithm.h"
#include "StDetectorDbMaker/St_tss_tssparC.h"
#include "StDetectorDbMaker/St_tpcAnodeHVavgC.h"
#include "StDetectorDbMaker/St_TpcAvgCurrentC.h"
#include "StDetectorDbMaker/St_TpcAvgPowerSupplyC.h"
#include "StDetectorDbMaker/St_trigDetSumsC.h"
#include "StDetectorDbMaker/StDetectorDbTpcRDOMasks.h"
#include "StPidStatus.h"
#include "dEdxHist.h"
#if defined(__CHECK_LargedEdx__) || defined( __DEBUG_dEdx__) || defined( __DEBUG_dNdx__)
#include "tables/St_g2t_track_Table.h" 
#endif
const static Int_t tZero= 19950101;
static Int_t tMin = 20000101;
static Int_t tMax = 20220101;
const static TDatime t0(tZero,0);
const static Int_t timeOffSet = t0.Convert();
static Double_t tpcTime = -1;
Int_t     StdEdxY2Maker::NdEdx = 0;
dEdxY2_t *StdEdxY2Maker::CdEdx = 0;
dEdxY2_t *StdEdxY2Maker::FdEdx = 0;
dEdxY2_t *StdEdxY2Maker::dEdxS = 0;
static Int_t numberOfSectors = 0;
static Int_t numberOfTimeBins = 0;
static Int_t NumberOfChannels = 8;
TGraph *StdEdxY2Maker::fdNdxGraph[3] = {0};

//const static Double_t pMomin = 0.35; // range for dE/dx calibration
//const static Double_t pMomax = 0.75;
const  Double_t pMoMIP = 0.526; // MIP from Heed bg = 3.77 => p_pion = 0.526
const  Double_t pMomin = pMoMIP - 0.05; // 0.45;
const  Double_t pMomax = pMoMIP + 0.05; // 0.55;

Double_t StdEdxY2Maker::bField = 0;
Bool_t   StdEdxY2Maker::fUsedNdx = kFALSE;
TH2F    *StdEdxY2Maker::fIntegratedAdc = 0;
//______________________________________________________________________________
// QA histograms
const static Int_t  fNZOfBadHits = 10 + StTpcdEdxCorrection::kTpcAllCorrections;
static TH1F **fZOfBadHits = 0;
static TH1F *fZOfGoodHits = 0;
static TH1F *fPhiOfGoodHits = 0;
static TH1F *fPhiOfBadHits = 0;
static TH1F *fTracklengthInTpcTotal = 0;
static TH1F *fTracklengthInTpc = 0;
static TH2F *fPadTbkAll = 0;
static TH2F *fPadTbkBad = 0;
#ifdef  __SpaceCharge__
static TH2F *AdcSC = 0, *AdcOnTrack = 0, *dEOnTrack = 0;
#endif
#ifdef __CHECK_RDOMAP_AND_VOLTAGE__
static TH3F *AlivePads = 0;
static TProfile3D *ActivePads = 0;
#endif /* __CHECK_RDOMAP_AND_VOLTAGE__ */
#ifdef __BEST_VERTEX__
static TH3F *PVxyz = 0, *PVxyzC = 0;
static TH2F *EtaVspT[2][2]  = {0}; // Global and Primary, Positive and Negative
static TH2F *EtaVspTC[2] = {0}; // Positive and Negative global track used for calibration
#endif /* __BEST_VERTEX__ */
//______________________________________________________________________________
ClassImp(StdEdxY2Maker);
//_____________________________________________________________________________
StdEdxY2Maker::StdEdxY2Maker(const char *name): StMaker(name), m_Mask(-1) {
  memset (beg, 0, end-beg);
  SETBIT(m_Mode,kPadSelection); 
  m_Minuit = new TMinuit(2);
  SetAttr("tMin",tMin);
  SetAttr("tMax",tMax);
}
//_____________________________________________________________________________
Int_t StdEdxY2Maker::Init(){
  if (IAttr("EmbeddingShortCut")) {
    m_Mode = 0;
    SETBIT(m_Mode,kEmbedding);
    SETBIT(m_Mode,kPadSelection); 
    SETBIT(m_Mask,StTpcdEdxCorrection::kTpcLast);
  }
  if (Debug()) {
    if (! TESTBIT(m_Mode, kOldClusterFinder)) {
      LOG_WARN << "StdEdxY2Maker::Init use new Cluster Finder parameterization" << endm;
    } else {
      LOG_WARN << "StdEdxY2Maker::Init use old Cluster Finder parameterization" << endm;
    }
    if (TESTBIT(m_Mode, kPadSelection)) {
      LOG_WARN << "StdEdxY2Maker::Init Pad Selection is ON" << endm;
    }
    if (TESTBIT(m_Mode, kDoNotCorrectdEdx)) {
      LOG_WARN << "StdEdxY2Maker::Init Don't Correct dEdx" << endm;
    }
    if (TESTBIT(m_Mode, kEmbedding)) {
      LOG_WARN << "StdEdxY2Maker::Init This is embedding run" << endm;
    }
  }
  gMessMgr->SetLimit("StdEdxY2Maker:: mismatched Sector",20);
  gMessMgr->SetLimit("StdEdxY2Maker:: pad/TimeBucket out of range:",20);
  gMessMgr->SetLimit("StdEdxY2Maker:: Helix Prediction",20);
  gMessMgr->SetLimit("StdEdxY2Maker:: Coordinates",20);
  gMessMgr->SetLimit("StdEdxY2Maker:: Prediction",20);
  gMessMgr->SetLimit("StdEdxY2Maker:: NdEdx",20);
  gMessMgr->SetLimit("StTpcdEdxCorrection:: Illegal time for scalers",20);
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StdEdxY2Maker::InitRun(Int_t RunNumber){
  tMin = IAttr("tMin"); 
  tMax = IAttr("tMax"); 
  static Int_t DoOnce = 0;
  if (!gStTpcDb) {
    cout << "Database Missing! Can't initialize StdEdxY2Maker" << endl;
    return kStFatal;
  }
  // 		TPG parameters
  numberOfSectors   = gStTpcDb->Dimensions()->numberOfSectors();
  numberOfTimeBins  = gStTpcDb->Electronics()->numberOfTimeBins();
  SafeDelete(m_TpcdEdxCorrection);
  m_TpcdEdxCorrection = new StTpcdEdxCorrection(m_Mask, Debug());

  if (! DoOnce) {
    DoOnce = 1;
    if (! IAttr("SkipdNdx")) {
    if ((GetDate() > 20171201 && m_TpcdEdxCorrection->IsFixedTarget()) ||
	(GetDate() > 20181201)) fUsedNdx = kTRUE; // use dN/dx for fixed target for Run XVIII and year >= XIX
    }
    if (TESTBIT(m_Mode, kCalibration)) {// calibration mode
      if (Debug()) LOG_WARN << "StdEdxY2Maker::InitRun Calibration Mode is On (make calibration histograms)" << endm;
      TFile *f = GetTFile();
      if (f) {
	f->cd();
	if ((TESTBIT(m_Mode, kGASHISTOGRAMS))) {
	  if (Debug()) LOG_WARN << "StdEdxY2Maker::InitRun Gas Histograms is ON" << endm;
	  TrigHistos();
	}
	Histogramming();
	if (TESTBIT(m_Mode, kXYZcheck))         XyzCheck();
      }
    }
    QAPlots(0);
    // Switch between usage prediction dependence of log2(dX), new option is not to use it in predicetion due to PicoDst
    fUsedx2 = kFALSE;
    if (m_TpcdEdxCorrection->Correction(StTpcdEdxCorrection::kTpcLengthCorrectionMD2) ||
	m_TpcdEdxCorrection->Correction(StTpcdEdxCorrection::kTpcLengthCorrectionMDF) ||
	m_TpcdEdxCorrection->Correction(StTpcdEdxCorrection::kTpcLengthCorrection   )) {
      fUsedx2 = kTRUE;
    } 
    if (m_TpcdEdxCorrection->Correction(StTpcdEdxCorrection::kTpcLengthCorrectionMDN)) {
      fUsedx2 = kFALSE;
    }
    LOG_WARN << "StdEdxY2Maker::InitRun Force ";
    if (fUsedx2) {
      LOG_WARN << "to USE"; 
    } else {
      LOG_WARN << "NOT to USE";
    }
    LOG_WARN << " dx2L in dE/dx predictions "<< endm;
  }
#ifdef __CHECK_RDOMAP_AND_VOLTAGE__
  St_tpcPadGainT0C::instance();  // activate extra gain corrections for tpx
  St_itpcPadGainT0C::instance(); // activate extra gain corrections for iTPC
#endif /* __CHECK_RDOMAP_AND_VOLTAGE__ */
  return kStOK;
}
//_______________________________________________________________________________
Double_t StdEdxY2Maker::gaus2(Double_t *x, Double_t *p) {
  Double_t NormL = p[0];
  Double_t mu    = p[1];
  Double_t muP   = mu + p[4];
  Double_t sigma = p[2];
  Double_t sigmaP = TMath::Sqrt(sigma*sigma + 0.101741*0.101741);
  Double_t phi   = p[3];
  Double_t frac = TMath::Sin(phi);
  frac *= frac;
  return TMath::Exp(NormL)*((1 - frac)*TMath::Gaus(x[0],mu ,sigma ,kTRUE) + 
			    frac      *TMath::Gaus(x[0],muP,sigmaP,kTRUE)); 
}
//________________________________________________________________________________
TF1 *StdEdxY2Maker::Gaus2() {
  TF1 *f = new TF1("Gaus2",gaus2,-3,3,5);
  f->SetParName(0,"NormL"); f->SetParLimits(0,-10.,10.);
  f->SetParName(1,"mu");    f->SetParLimits(1,-0.5,0.5);
  f->SetParName(2,"sigma"); f->SetParLimits(2, 0.2,0.5);
  f->SetParName(3,"phiP");  f->SetParLimits(3, 0.0,TMath::Pi()/4);
  f->SetParName(4,"muP");
  f->SetParameters(10,0,0.3,0.1,1.315);
  //  f->FixParameter(4,1.425);
  return f;
}
//_____________________________________________________________________________
Int_t StdEdxY2Maker::Finish() {
  //  static Double_t slope = 1.7502e-6;// slope from Blair   1/( O2 in ppm., cm )
  SafeDelete(m_Minuit);
  if (m_TpcdEdxCorrection && m_TpcdEdxCorrection->TestBit(kCanDelete)) delete m_TpcdEdxCorrection;
  m_TpcdEdxCorrection = 0;
  return StMaker::Finish();
}
//_____________________________________________________________________________
void StdEdxY2Maker::AddEdxTraits(StTrack *tracks[2], dst_dedx_st &dedx){ 
  for (Int_t l = 0; l < 2; l++) {
    if (tracks[l]) {
      StDedxPidTraits *trait = new StDedxPidTraits(dedx);
      tracks[l]->addPidTraits(trait);
    }
  }
}
//_____________________________________________________________________________
Int_t StdEdxY2Maker::Make(){ 
#ifdef __BENCHMARKS__DOFIT_ZN__
  TBenchmark myBenchmark;
#endif /* __BENCHMARKS__DOFIT_ZN__ */
  static Bool_t ForcedX = IAttr("ForcedX");
  tpcTime = GetDateTime().Convert() - timeOffSet;
  static  StTpcLocalSectorCoordinate        localSect[4];
  static  StTpcPadCoordinate                PadOfTrack, Pad;
  static  StTpcLocalSectorDirection         localDirectionOfTrack;
  static  StThreeVectorD xyz[4];
  static  StThreeVectorD dirG;
  static  Double_t s[2], s_in[2], s_out[2], w[2], w_in[2], w_out[2], dx, AdcI, dxC;
#ifdef __dZdY_dXdY__
  static  Double dZdY, dXdY;
#endif
  enum {kNdEdxMax  = 300};
  static dEdxY2_t CdEdxT[3*kNdEdxMax];//,FdEdxT[kNdEdxMax],dEdxST[kNdEdxMax];
  static Int_t sectorMin = 1, sectorMax = 24;
  CdEdx = CdEdxT; 
  FdEdx = CdEdxT + kNdEdxMax;   // sorted by row
  dEdxS = CdEdxT + 2*kNdEdxMax; // sorted by dE/dx
  St_tpcGas  *tpcGas = m_TpcdEdxCorrection->tpcGas();
  if (TESTBIT(m_Mode, kCalibration) && tpcGas) TrigHistos(1);
  StTpcCoordinateTransform transform(gStTpcDb);
  StEvent* pEvent = dynamic_cast<StEvent*> (GetInputDS("StEvent"));
  if (!pEvent) {
    LOG_INFO << "StdEdxY2Maker: no StEvent " << endm;
    return kStOK;        // if no event, we're done
  }
  if (pEvent->runInfo()) bField = pEvent->runInfo()->magneticField()*kilogauss;
  if (TMath::Abs(bField) < 1.e-5*kilogauss) return kStOK;
  UInt_t NoPV = pEvent->numberOfPrimaryVertices();
  if (! NoPV)  return kStOK;
#ifdef __BEST_VERTEX__
  const StBTofCollection* tof = pEvent->btofCollection();
  StPrimaryVertex *pVbest  =  pEvent->primaryVertex(0);
  Double_t VpdZ = -300;
  if (tof) {
    if (tof->tofHeader()) VpdZ = tof->tofHeader()->vpdVz();
  }
  if (m_TpcdEdxCorrection->IsFixedTarget())  VpdZ = 200;
  Double_t dZbest = 999;
  for (UInt_t ipr = 0; ipr < NoPV; ipr++) {
    StPrimaryVertex *pVertex = pEvent->primaryVertex(ipr);
    if (! pVertex) continue; 
    if (PVxyz) PVxyz->Fill( pVertex->position().x(),  pVertex->position().y(), pVertex->position().z());
    if (TMath::Abs(VpdZ) < 250) {
      Double_t zTPC = pVertex->position().z();
      Double_t dZ = TMath::Abs(zTPC-VpdZ);
      if (dZ < dZbest) {
	dZbest = dZ;
	pVbest = pVertex;
      }
    }
  }
  if (dZbest < 999 && dZbest > 3.0) pVbest = 0;
  if (pVbest &&PVxyzC) PVxyzC->Fill( pVbest->position().x(),  pVbest->position().y(), pVbest->position().z());
#endif /* __BEST_VERTEX__ */
  // no of tpc hits
  Int_t TotalNoOfTpcHits = 0;
  Int_t NoOfTpcHitsUsed  = 0;
  StTpcHitCollection* TpcHitCollection = pEvent->tpcHitCollection();
  if (! TpcHitCollection) {
    LOG_INFO << "StdEdxY2Maker: no TpcHitCollection " << endm;
    return kStOK;        // if no event, we're done
  }
  TotalNoOfTpcHits = TpcHitCollection->numberOfHits();
  StSPtrVecTrackNode& trackNode = pEvent->trackNodes();
  UInt_t nTracks = trackNode.size();
  for (UInt_t i=0; i < nTracks; i++) { 
    StTrackNode *node = trackNode[i]; 
    if (!node) continue;
    StGlobalTrack  *gTrack = dynamic_cast<StGlobalTrack *>(node->track(global));
    if (gTrack && gTrack->bad()) {gTrack = 0;}
    if (! gTrack ||  gTrack->flag() <= 0) continue;
    StPrimaryTrack *pTrack = 0;
    if (gTrack) {
      pTrack = static_cast<StPrimaryTrack*>(node->track(primary));
    }
    if (pTrack && pTrack->bad()) {pTrack = 0;}
    StTrack *track = 0;
    StTrack *tracks[2] = {gTrack, pTrack};
    Int_t sCharge = 0;                                // positive
    if (gTrack->geometry()->charge() < 0) sCharge = 1;// negative
    Int_t qB = (sCharge+1)%2;
    if (bField > 0) qB = (qB + 1)%2; // swap for Full Field
#ifdef __BEST_VERTEX__
    if (TESTBIT(m_Mode, kCalibration)) {// calibration mode
      for (Int_t l = 0; l < 2; l++) {
	track = tracks[l];
	if (track) {
	  StThreeVectorD g3 = track->geometry()->momentum(); // p of global track
	  EtaVspT[l][sCharge]->Fill(TMath::Log10(g3.perp()), g3.pseudoRapidity());
	}
      }
    }
#endif /* __BEST_VERTEX__ */   
    StPhysicalHelixD helixO = gTrack->outerGeometry()->helix();
    StPhysicalHelixD helixI = gTrack->geometry()->helix();
    StThreeVectorD g3 = gTrack->geometry()->momentum(); // p of global track
    Double_t etaG = g3.pseudoRapidity();
    if (Debug() > 1) {
      cout << "Track:" << i 
	   << "\ttype " << gTrack->type()
	   << "\tvertex " << gTrack->vertex()
	   << "\tkey " << gTrack->key()
	   << "\tflag " << gTrack->flag()
	   << "\tencodedMethod " << gTrack->encodedMethod()
	   << "\timpactParameter " << gTrack->impactParameter()
	   << "\tlength " << gTrack->length()
	   << "\tEtaG " << etaG
	   << "\tnumberOfPossiblePoints " << gTrack->numberOfPossiblePoints() << endl;
      cout << "pxyzI:        " << gTrack->geometry()->momentum() << "\tmag " << gTrack->geometry()->momentum().mag() << endl;
      cout << "pxyzO:        " << gTrack->outerGeometry()->momentum() << "\tmag " << gTrack->outerGeometry()->momentum().mag() << endl;
      cout << "start Point: " << helixI.at(0) << endl;
      cout << "end   Point: " << helixO.at(0) << endl;
    }
    //    StPtrVecHit hvec = gTrack->detectorInfo()->hits(kTpcId);
    StPtrVecHit hvec = gTrack->detectorInfo()->hits();
    // if no hits than make only histograms. Works if kDoNotCorrectdEdx mode is set
    if (! TESTBIT(m_Mode, kDoNotCorrectdEdx)) {
      // clean old PiD traits
      for (Int_t l = 0; l < 2; l++) {
	track = tracks[l]; 
	if (track) {
	  StSPtrVecTrackPidTraits &traits = track->pidTraits();
	  UInt_t size = traits.size();
	  if (size) {
	    for (UInt_t i = 0; i < size; i++) {
	      StDedxPidTraits *pid = dynamic_cast<StDedxPidTraits*>(traits[i]);
	      if (! pid) continue;
	      if (pid->detector() != kTpcId) continue;
	      traits[i]->makeZombie(1);
	    }
	  }
	}
      }
    }
    if (hvec.size() && ! TESTBIT(m_Mode, kDoNotCorrectdEdx)) {
      Int_t Id = gTrack->key();
      Int_t NoFitPoints = gTrack->fitTraits().numberOfFitPoints(kTpcId);
      NdEdx = 0;
      Double_t TrackLength70 = 0;
      Double_t TrackLength = 0;
      Double_t TrackLengthTotal = 0;
      for (UInt_t j=0; j<hvec.size(); j++) {// hit loop
	if (hvec[j]->detector() != kTpcId) continue;
	StTpcHit *tpcHit = static_cast<StTpcHit *> (hvec[j]);
	if (! tpcHit) continue;
	if (Debug() > 1) {tpcHit->Print();}
	if (! tpcHit->usedInFit()) {
	  BadHit(0,tpcHit->position());
	  continue;
	} if (  tpcHit->flag()) {
	  BadHit(1,tpcHit->position());
	  continue;
	}
	Int_t sector = tpcHit->sector();
	if (sector < sectorMin || sector > sectorMax) continue;
	Int_t row    = tpcHit->padrow();
	Int_t pad    = tpcHit->pad();
	Int_t iRdo    = StDetectorDbTpcRDOMasks::instance()->rdoForPadrow(sector,row,pad);
	if ( ! StDetectorDbTpcRDOMasks::instance()->isOn(sector,iRdo)) continue;
	if (! St_tpcAnodeHVavgC::instance()->livePadrow(sector,row)) continue;
	xyz[3] = StThreeVectorD(tpcHit->position().x(),tpcHit->position().y(),tpcHit->position().z());
	//________________________________________________________________________________      
	Float_t dX_TrackFit = tpcHit->dX();
	Float_t dX_Helix = 0;
	dx = dX_TrackFit; 
	AdcI = 0;
#ifdef __dZdY_dXdY__
	dZdY = dXdY = 0;
#endif
	static StGlobalDirection  globalDirectionOfTrack;
	Int_t iokCheck = 0;
#ifdef __TEST_DX__
	static Bool_t TestdX = kTRUE;
#else
	static Bool_t TestdX = kFALSE;
#endif /* __TEST_DX__ */
	// use cluster position for precalculated dx
	transform(xyz[3],localSect[3],sector,row);
	transform(localSect[3],Pad);
	while ((ForcedX || dX_TrackFit <= 0.0 || TestdX)) {
	  dX_Helix = -13;
	  StThreeVectorD middle = xyz[3];
	  StThreeVectorD upper(tpcHit->positionU().x(),tpcHit->positionU().y(),tpcHit->positionU().z());
	  StThreeVectorD lower(tpcHit->positionL().x(),tpcHit->positionL().y(),tpcHit->positionL().z());
	  StThreeVectorD dif = upper - lower;
	  StThreeVectorD normal = dif.unit();
	  StGlobalCoordinate globalOfTrack;
	  Double_t pad;
#if 0
	  StThreeVectorD &V = *&normal;
	  Double_t zd = sector <=12 ? 1: -1;
	  StThreeVectorD W = StThreeVectorD(0,0,zd);
	  StThreeVectorD U = V.cross(W);
	  StThreeVectorD D = dif.unit();
	  Double_t dY = 0;
#endif
	  // check that helix prediction is consistent with measurement
	  if (Propagate(middle,normal,helixI,helixO,xyz[0],dirG,s,w)) break;
	    if (Debug() > 1) {
	    cout << " Prediction:\t" << xyz[0] 
		 << "\tat s=\t" << s[0] << "/" << s[1] 
		 << "\tw = " << w[0] << "/" << w[1] << endl;
	    }
	    dif = xyz[3] - xyz[0];
	    if (dif.perp() > 2.0) {
	      if (Debug() > 1) {cout << "Prediction is to far from hit:\t" << xyz[3] << endl;}
	      break;
	    }
	    if (Propagate(upper,normal,helixI,helixO,xyz[1],dirG,s_out,w_out)) break;
	    if (Propagate(lower,normal,helixI,helixO,xyz[2],dirG,s_in ,w_in )) break;
	    dX_Helix = ((s_out[0] - s_in[0])*w[1] + (s_out[1] - s_in[1])*w[0]);
	    dif = xyz[1] - xyz[2];
	    // Check for Membernane
	    if (xyz[1].z() * xyz[2].z() < 0) {
	      Double_t dZ = TMath::Abs(xyz[1].z()) + TMath::Abs(xyz[2].z());
	      Double_t scaledX = 1;
	      if        (xyz[1].z() * xyz[3].z() > 0) {
		scaledX = TMath::Abs(xyz[1].z())/dZ;
	      } else if (xyz[2].z() * xyz[3].z() > 0) {
		scaledX = TMath::Abs(xyz[2].z())/dZ;
	      }
	      static Int_t ibreak = 0;
	      if (Debug() > 1) {
		cout << "Cross Membrane : upper " << xyz[1] << endl;
		cout << "                 hit   " << xyz[3] << endl;
		cout << "                 lower " << xyz[2] << "\tscale dX = " << scaledX << endl;
	      }
	      dX_Helix *= scaledX;
	    ibreak++;
	  }
	  if (dX_Helix <= 0.0) {
	    if (Debug() > 1) {cout << "negative dX_Helix " << dX_Helix << endl;}
	    break;
	  }
	  // Consistency check
	  globalDirectionOfTrack = StGlobalDirection(dirG);
	  for (Int_t l = 0; l < 4; l++) {
	    globalOfTrack = StGlobalCoordinate(xyz[l].x(),xyz[l].y(),xyz[l].z());
	    transform(globalOfTrack,localSect[l],sector,row);
	  }
	  if (ForcedX || dX_TrackFit <= 0.0 ) 
	    tpcHit->setdX(dx);
	  transform(localSect[0],PadOfTrack);
	  transform(globalDirectionOfTrack,localDirectionOfTrack,sector,row);
	  transform(localSect[3],Pad);
	  if (sector != Pad.sector() || // ? && TMath::Abs(xyz[0].x()) > 20.0 ||
	      row    != Pad.row()) {
	    LOG_WARN << "StdEdxY2Maker:: mismatched Sector " 
		     << Pad.sector() << " / " << sector
		     << " Row " << Pad.row() << " / " << row 
		     << "pad " << Pad.pad() << " TimeBucket :" << Pad.timeBucket() 
		     << endm;
	    iokCheck++;
	  }
	  pad = tpcHit->pad();
	  if (pad == 0) pad = Pad.pad();
	  if (Pad.timeBucket() < 0         ||
	      Pad.timeBucket() >= numberOfTimeBins) {
	    LOG_WARN << "StdEdxY2Maker:: TimeBucket out of range: " 
		     << Pad.timeBucket() << endm;
	    iokCheck++;
	  }
	  if (sector != PadOfTrack.sector() || 
	      row != PadOfTrack.row() ||	
	      TMath::Abs(Pad.pad()-PadOfTrack.pad()) > 5) {
	    if (Debug() > 1) {
	      LOG_WARN << "StdEdxY2Maker::	Helix Prediction " 
		       << "Sector = " 
		       << PadOfTrack.sector() << "/" 
		       << sector 
		       << " Row = " << PadOfTrack.row() << "/" 
		       << row 
		       << " Pad = " << PadOfTrack.pad() << "/" 
		       << Pad.pad() 
		       << " from Helix  is not matched with point/" << endm;;
	      LOG_WARN << "StdEdxY2Maker:: Coordinates Preiction: " 
		       << xyz[0] << "/Hit " << tpcHit->position()
		       << endm;
	    }
	    iokCheck++;
	  }
	  if (iokCheck) {
	    dX_Helix = -13;
	    break;
	  }
#ifdef __dZdY_dXdY__
	  dY = D.dot(V);
	  if (TMath::Abs(dY) > 1e-7) {
	    dZdY = D.dot(W)/dY;
	    dXdY = D.dot(U)/dY;
	  } else {
	    dZdY = dXdY = 0;
	  }
#endif
	  if (Debug() > 1) {
	    cout << "Helix Prediction with dX = " << dX_Helix << endl;
	  }
	  break;
	} // end of dx calculation
	if (dX_Helix < 0) {
	  if (Debug() > 1) {
	    cout << "Helix Prediction Failed" << endl;
	  }
	}
	dx = tpcHit->dX();
	if ((ForcedX || dX_TrackFit <= 0.0)) {
	  if (dX_Helix <= 0.0) continue;
	  if (Debug()) {
	  }
	  dx = dX_Helix;
	  if (ForcedX) tpcHit->setdX(dx);
	} else {
	  dx =  dX_TrackFit;
	}
	TrackLengthTotal += dx;
	//________________________________________________________________________________      
	if (tpcHit->adc() <= 0) {
	  LOG_WARN << "StdEdxY2Maker:: adc : " <<  tpcHit->adc() 
		   << " <= 0" << endm;
	  iokCheck++;
	}
	//	if ((TESTBIT(m_Mode, kXYZcheck)) && (TESTBIT(m_Mode, kCalibration))) XyzCheck(&global, iokCheck);
	if ((TESTBIT(m_Mode, kPadSelection)) && iokCheck) {BadHit(3, tpcHit->position()); continue;}
	if ((TESTBIT(m_Mode, kPadSelection)) && (dx < 0.5 || dx > 25.)) {BadHit(4, tpcHit->position()); continue;}
	// Corrections
	CdEdx[NdEdx].Reset();
	CdEdx[NdEdx].resXYZ[0] = localSect[3].position().x() - localSect[0].position().x();
	CdEdx[NdEdx].resXYZ[1] = localSect[3].position().y() - localSect[0].position().y();
	CdEdx[NdEdx].resXYZ[2] = localSect[3].position().z() - localSect[0].position().z();
	CdEdx[NdEdx].DeltaZ = 5.2; 
	CdEdx[NdEdx].QRatio = -2;
	CdEdx[NdEdx].QRatioA = -2.;
	CdEdx[NdEdx].QSumA = 0;
	CdEdx[NdEdx].sector = sector; 
	CdEdx[NdEdx].row    = row;
	CdEdx[NdEdx].pad    = Pad.pad();
	CdEdx[NdEdx].edge   = CdEdx[NdEdx].pad;
	Float_t NoPadsInRow = St_tpcPadConfigC::instance()->numberOfPadsAtRow(sector,row);
	if (CdEdx[NdEdx].edge > 0.5*NoPadsInRow) 	  CdEdx[NdEdx].edge -= 1 + NoPadsInRow;
	CdEdx[NdEdx].xpad = 2*(CdEdx[NdEdx].pad - 0.5)/NoPadsInRow - 1.0;
	CdEdx[NdEdx].xpadR = 2*(tpcHit->pad() - 0.5)/NoPadsInRow - 1.0;
	CdEdx[NdEdx].qB = qB;
	CdEdx[NdEdx].yrow = sector + 0.5*((row <= St_tpcPadConfigC::instance()->innerPadRows(sector)) ? 
					  (row - St_tpcPadConfigC::instance()->innerPadRows(sector) - 0.5)/St_tpcPadConfigC::instance()->innerPadRows(sector) : 
					  (row - St_tpcPadConfigC::instance()->innerPadRows(sector) - 0.5)/(St_tpcPadConfigC::instance()->numberOfRows(sector) - St_tpcPadConfigC::instance()->innerPadRows(sector)));
	CdEdx[NdEdx].dX_TrackFit = dX_TrackFit;
	CdEdx[NdEdx].dX_Helix = dX_Helix;
	CdEdx[NdEdx].Npads = tpcHit->padsInHit();
	CdEdx[NdEdx].Ntbks = tpcHit->timeBucketsInHit();
	CdEdx[NdEdx].dCharge = 0;
	CdEdx[NdEdx].rCharge=  0.5*m_TpcdEdxCorrection->Adc2GeV()*TMath::Pi()/4.*CdEdx[NdEdx].Npads*CdEdx[NdEdx].Ntbks;
	if (TESTBIT(m_Mode, kEmbeddingShortCut) && 
	    (tpcHit->idTruth() && tpcHit->qaTruth() > 95)) CdEdx[NdEdx].lSimulated = tpcHit->idTruth();
#ifdef __dZdY_dXdY__
	CdEdx[NdEdx].dZdY = dZdY;
	CdEdx[NdEdx].dXdY = dXdY;
#endif
	CdEdx[NdEdx].AdcI = AdcI;
	dxC = dx;
#if 1
	// Scale dX to full pad length
	if (St_tpcPadConfigC::instance()->iTpc(sector)) {
	  Int_t io = 1;
	  if (row > St_tpcPadConfigC::instance()->innerPadRows(sector)) io = 0;
	  Double_t padlength = (io == 1) ? 
	    St_tpcPadConfigC::instance()->innerSectorPadLength(sector) : 
	    St_tpcPadConfigC::instance()->outerSectorPadLength(sector);
	  Double_t rowPitch  = (io == 1) ? 
	    St_tpcPadConfigC::instance()->innerSectorRowPitch1(sector) : 
	    St_tpcPadConfigC::instance()->outerSectorRowPitch(sector);
	  dxC *= rowPitch/padlength;
	}
#endif
	CdEdx[NdEdx].dxC    = dxC;
	CdEdx[NdEdx].F.dE     = tpcHit->charge();
	CdEdx[NdEdx].F.dx   = dxC;
	CdEdx[NdEdx].xyz[0] = localSect[3].position().x();
	CdEdx[NdEdx].xyz[1] = localSect[3].position().y();
	CdEdx[NdEdx].xyz[2] = localSect[3].position().z();
	Double_t maxPad = NoPadsInRow/2;
	Double_t pitch = (row <= St_tpcPadConfigC::instance()->innerPadRows(sector)) ?
	  St_tpcPadConfigC::instance()->innerSectorPadPitch(sector) :
	  St_tpcPadConfigC::instance()->outerSectorPadPitch(sector);
	Double_t PhiMax = TMath::ATan2(maxPad*pitch, St_tpcPadConfigC::instance()->radialDistanceAtRow(sector,row));
	CdEdx[NdEdx].PhiR   = TMath::ATan2(CdEdx[NdEdx].xyz[0],CdEdx[NdEdx].xyz[1])/PhiMax;
	CdEdx[NdEdx].xyzD[0] = localDirectionOfTrack.position().x();
	CdEdx[NdEdx].xyzD[1] = localDirectionOfTrack.position().y();
	CdEdx[NdEdx].xyzD[2] = localDirectionOfTrack.position().z();
	CdEdx[NdEdx].ZdriftDistance = localSect[3].position().z();
	CdEdx[NdEdx].zG      = tpcHit->position().z();
	CdEdx[NdEdx].etaG    = etaG;
	Double_t pT2 = CdEdx[NdEdx].xyzD[0]*CdEdx[NdEdx].xyzD[0]+CdEdx[NdEdx].xyzD[1]*CdEdx[NdEdx].xyzD[1];
	if (pT2 > 1e-14)
	  CdEdx[NdEdx].TanL = -CdEdx[NdEdx].xyzD[2]/TMath::Sqrt(pT2);
	CdEdx[NdEdx].tpcTime = tpcTime;
	if (St_trigDetSumsC::instance())	CdEdx[NdEdx].Zdc     = St_trigDetSumsC::instance()->zdcX();
	CdEdx[NdEdx].adc     = tpcHit->adc();
	Bool_t doIT = kTRUE;
	if (TESTBIT(m_Mode,kEmbedding)) doIT = kFALSE;
	if (fPadTbkAll) fPadTbkAll->Fill(CdEdx[NdEdx].Ntbks, CdEdx[NdEdx].Npads);
	Int_t iok = m_TpcdEdxCorrection->dEdxCorrection(CdEdx[NdEdx],doIT);
	if (iok) {
	  BadHit(10+iok, tpcHit->position()); 
	  if (fPadTbkBad) fPadTbkBad->Fill(CdEdx[NdEdx].Ntbks, CdEdx[NdEdx].Npads);
	  continue;
	} 
	if (fZOfGoodHits) fZOfGoodHits->Fill(tpcHit->position().z());
	if (fPhiOfGoodHits!= 0) fPhiOfGoodHits->Fill(TMath::ATan2(tpcHit->position().y(),tpcHit->position().x()));
	if (NdEdx < kNdEdxMax) {
	  tpcHit->setCharge(CdEdx[NdEdx].F.dE);
	  TrackLength         += CdEdx[NdEdx].F.dx;
	  NdEdx++; 
	  NoOfTpcHitsUsed++; 	
	}
	if (NdEdx > NoFitPoints) 
	  LOG_ERROR << "StdEdxY2Maker:: NdEdx = " << NdEdx 
			    << ">  NoFitPoints ="<< NoFitPoints << endm;
      }
      if (fTracklengthInTpcTotal) fTracklengthInTpcTotal->Fill(TrackLengthTotal);
      if (fTracklengthInTpc)      fTracklengthInTpc->Fill(TrackLength);
      SortdEdx();
      if (Debug() > 1) PrintdEdx(2);
      Double_t I70 = 0, D70 = 0;
      Double_t dXavLog2 = 1;
      Double_t SumdEdX = 0;
      Double_t SumdX = 0;
      Int_t N70 = NdEdx - (int) (0.3*NdEdx + 0.5); 
      if (N70 > 1) {
	Int_t k;
	for (k = 0; k < N70; k++) {
	  I70 += dEdxS[k].F.dEdx;
	  D70 += dEdxS[k].F.dEdx*dEdxS[k].F.dEdx;
	  TrackLength70 += dEdxS[k].F.dx;
	  if (dEdxS[k].F.dx > 0) {
	    SumdEdX += dEdxS[k].F.dEdx;
	    SumdX   += dEdxS[k].F.dEdx*TMath::Log2(dEdxS[k].F.dx);
	  }
	}
	I70 /= N70; D70 /= N70;
	D70  = TMath::Sqrt(TMath::Abs(D70 - I70*I70));
	D70 /= I70;
#ifdef __CHECK_LargedEdx__
        static Double_t dEdxMin = 6e-6; // 6keV
	static Int_t iBreak = 0;
	if (I70 > dEdxMin) {
	  iBreak++;
	  gTrack->Print();
	  if (gTrack->idTruth() > 0) {
	    St_g2t_track  *g2t_track  = (St_g2t_track  *) GetDataSet("geant/g2t_track");
	    if (g2t_track) g2t_track->Print(gTrack->idTruth()-1,1);
	  }
	  PrintdEdx(2);
	}
#endif	
	if (SumdEdX > 0) dXavLog2 = SumdX/SumdEdX;
	dst_dedx_st dedx;
	dedx.id_track  =  Id;
	dedx.det_id    =  kTpcId;    // TPC track 
	dedx.method    =  kEnsembleTruncatedMeanId; // == kTruncatedMeanId+1;
	dedx.ndedx     =  TMath::Min(99,N70) + 100*((int) TrackLength);
	dedx.dedx[0]   =  I70;
	dedx.dedx[1]   =  D70;
	dedx.dedx[2]   =  dXavLog2;
	if ((TESTBIT(m_Mode, kCalibration)))  // uncorrected dEdx
	  AddEdxTraits(tracks, dedx);
	if (! TESTBIT(m_Mode, kDoNotCorrectdEdx)) { 
	  dedx.det_id    = kTpcId;    // TPC track
	  m_TpcdEdxCorrection->dEdxTrackCorrection(0,dedx, etaG); 
	  dedx.det_id    = kTpcId;    // TPC track 
	  dedx.method    = kTruncatedMeanId;
	  AddEdxTraits(tracks, dedx);
	}
	// likelihood fit
	Double_t chisq, fitZ, fitdZ;
#ifdef __BENCHMARKS__DOFIT_ZN__
	myBenchmark.Start("StdEdxY2Maker::DoFitZ");
#endif /* __BENCHMARKS__DOFIT_ZN__ */
	DoFitZ(chisq, fitZ, fitdZ);
	if (chisq >0 && chisq < 10000.0) {
	  dXavLog2 = 1;
	  SumdEdX = 0;
	  SumdX = 0;
	  for (k = 0; k < NdEdx; k++) {
	    SumdEdX += dEdxS[k].F.dEdx;
	    SumdX   += dEdxS[k].F.dEdx*TMath::Log2(dEdxS[k].F.dx);
	  }
	  if (SumdEdX > 0) dXavLog2 = SumdX/SumdEdX;
	  dedx.id_track  =  Id;
	  dedx.det_id    =  kTpcId;    // TPC track 
	  dedx.method    =  kWeightedTruncatedMeanId;// == kLikelihoodFitId+1;
	  dedx.ndedx     =  TMath::Min(99,NdEdx) + 100*((int) TrackLength);
	  dedx.dedx[0]   =  TMath::Exp(fitZ);
	  dedx.dedx[1]   =  fitdZ; 
	  dedx.dedx[2]   =  dXavLog2;
	  if ((TESTBIT(m_Mode, kCalibration)))  // uncorrected dEdx
	    AddEdxTraits(tracks, dedx);
	  if (! TESTBIT(m_Mode, kDoNotCorrectdEdx)) { 
	  dedx.det_id    = kTpcId;    // TPC track
 	    m_TpcdEdxCorrection->dEdxTrackCorrection(2,dedx, etaG); 
	    dedx.det_id    = kTpcId;    // TPC track 
	    dedx.method    = kLikelihoodFitId;
	    AddEdxTraits(tracks, dedx);
	  }
#ifdef __BENCHMARKS__DOFIT_ZN__
	  myBenchmark.Stop("StdEdxY2Maker::DoFitZ");
#endif /* __BENCHMARKS__DOFIT_ZN__ */
#ifdef __DEBUG1__
#if defined(__DEBUG_dEdx__) || defined(__DEBUG_dNdx__)
	  StThreeVectorD g3 = gTrack->geometry()->momentum(); // p of global track
	  Double_t pMomentum = g3.mag();
	  Double_t bgPion = pMomentum/StProbPidTraits::mPidParticleDefinitions[kPidPion]->mass();
	  Double_t dEdxFitL10 = TMath::Log10(	1e6*dedx.dedx[0]);
	  Double_t dNdx = StdEdxPull::EvalPred(bgPion, 2);
	  if ((TMath::Abs(pMomentum - 1.0) < 0.1 && dEdxFitL10 > 0.5 && dEdxFitL10 < 0.8) ||
	      (TMath::Abs(pMomentum - 0.5) < 0.1 && dEdxFitL10 > 1.0 && dEdxFitL10 < 1.4)) {
	    static Int_t ibreak = 0;
	    gTrack->Print();
	    if (gTrack->idTruth() > 0) {
	      St_g2t_track  *g2t_track  = (St_g2t_track  *) GetDataSet("geant/g2t_track");
	      if (g2t_track) g2t_track->Print(gTrack->idTruth()-1,1);
	    }
	    PrintdEdx(2);
	    ibreak++;
	  }
#endif
#endif /* __DEBUG1__ */
	  if (fUsedNdx) {
	    // likelihood fit of no. of primary cluster per cm
	    Double_t chisqN, fitN = fitZ, fitdN;
#ifdef __BENCHMARKS__DOFIT_ZN__
	    myBenchmark.Start("StdEdxY2Maker::DoFitN");
#endif /* __BENCHMARKS__DOFIT_ZN__ */
	    DoFitN(chisqN, fitN, fitdN);
	    if (chisqN > -900.0 &&chisqN < 10000.0) {
	      dedx.id_track  =  Id;
	      dedx.det_id    =  kTpcId;    // TPC track 
	      dedx.method    =  kOtherMethodId2;
	      dedx.ndedx     =  TMath::Min(99,NdEdx) + 100*((int) TrackLength);
	      dedx.dedx[0]   =  fitN;
	      dedx.dedx[1]   =  fitdN/fitN; 
	      dedx.dedx[2]   =  dXavLog2;
	      AddEdxTraits(tracks, dedx);
	      dedx.det_id    = kTpcId;    // TPC track
	      m_TpcdEdxCorrection->dEdxTrackCorrection(1,dedx, etaG); 
	      dedx.det_id    =  kTpcId;    // TPC track 
	      dedx.method    =  kOtherMethodId;
	      AddEdxTraits(tracks, dedx);
#ifdef  __DEBUG_dNdx__1
	      Double_t dEdxL10 = TMath::LogE()*fitZ + 6;
	      Double_t dNdxL10 = TMath::Log10(fitN);
	      if (dNdxL10 > 3.70 && dEdxL10 > -0.4) {
		static Int_t ibreak = 0;
		cout << "DEBUG dN/dx: dE/dx = " << TMath::Power(10.,dEdxL10) << " keV, N = " << fitN << endl;
		PrintdEdx(2);
		ibreak++;
	      }
#endif
	    }
#ifdef __BENCHMARKS__DOFIT_ZN__
	    myBenchmark.Stop("StdEdxY2Maker::DoFitN");
#endif /* __BENCHMARKS__DOFIT_ZN__ */
	  }
	}

#ifdef __ADD_PROB__
	if (! TESTBIT(m_Mode, kDoNotCorrectdEdx)) { 
	  StThreeVectorD g3 = gTrack->geometry()->momentum(); // p of global track
	  Double_t pMomentum = g3.mag();
	  Float_t Chisq[KPidParticles];
	  for (Int_t hyp = 0; hyp < KPidParticles; hyp++) {
	    Double_t bgL10 = TMath::Log10(pMomentum*TMath::Abs(StProbPidTraits::mPidParticleDefinitions[hyp]->charge())/StProbPidTraits::mPidParticleDefinitions[hyp]->mass());
	    Chisq[hyp] = LikeliHood(bgL10,NdEdx,FdEdx, StProbPidTraits::mPidParticleDefinitions[hyp]->charge()*StProbPidTraits::mPidParticleDefinitions[hyp]->charge());
	  }
	  for (Int_t l = 0; l < 2; l++) {if (tracks[l]) tracks[l]->addPidTraits(new StProbPidTraits(NdEdx,kTpcId,KPidParticles,Chisq));}
	}
#endif /* __ADD_PROB__ */
      }
    } // (hvec.size() && ! TESTBIT(m_Mode, kDoNotCorrectdEdx))
    if (pTrack) QAPlots(gTrack);
    if ((TESTBIT(m_Mode, kCalibration))) {
      if (! pTrack) continue; // reject non primary tracks
#ifdef __BEST_VERTEX__
      if (! pEvent->primaryVertex()) continue; 
      // AuAu could have wrong ranking
      if (pEvent->primaryVertex()->ranking() > 0) {
	if (pTrack->vertex() != pEvent->primaryVertex()) continue; // only the first primary vertex
	//      if ( ((StPrimaryVertex *) pTrack->vertex() )->numMatchesWithBEMC() <= 0) continue;
      } else {// try to use VpdZ to select best vertex
	if (pVbest && pTrack->vertex() != pVbest) continue;
      }
#endif /* __BEST_VERTEX__ */
      Histogramming(gTrack);
    }
  }
  if (Debug() > 1) {
    LOG_QA << "StdEdxY2Maker:"
		       << "  Type: " << pEvent->type()
		       << "  Run: " << pEvent->runId() 
		       << "  Event: " << pEvent->id()
		       << "  # track nodes: "
		       << pEvent->trackNodes().size() << endm;
  }
  if (mHitsUsage) mHitsUsage->Fill(TMath::Log10(TotalNoOfTpcHits+1.), TMath::Log10(NoOfTpcHitsUsed+1.));
#if defined(__SpaceCharge__) || defined(__CHECK_RDOMAP_AND_VOLTAGE__)
  if ((TESTBIT(m_Mode, kCalibration))) {
#ifdef  __SpaceCharge__
    if (! AdcSC) {
      AdcSC      = new TH2F("AdcSC","ADC total versus z and row (-ve for East)",210,-210,210, 145, -72.5, 72.5);
      AdcOnTrack = new TH2F("AdcOnTrack","ADC on Track versus z and row (-ve for East)",210,-210,210, 145, -72.5, 72.5);
      dEOnTrack  = new TH2F("dEOnTrack","dE (keV) on Track versus z and row (-ve for East)",210,-210,210, 145, -72.5, 72.5);
    }
#endif
#ifdef __CHECK_RDOMAP_AND_VOLTAGE__
    if (! AlivePads || ! ActivePads) {
      Int_t NoOfPads = 182;
      if (St_tpcPadConfigC::instance()->iTPC(1)) { // iTpc for all TPC sectors
	NoOfPads = St_tpcPadConfigC::instance()->numberOfPadsAtRow(1,72);
      } 
      Int_t nrows = St_tpcPadConfigC::instance()->numberOfRows(20);
      if (GetTFile()) GetTFile()->cd();
      AlivePads = new TH3F("AlivePads","Active pads from RDO map, tpcGainPadT0,  and Tpc Anode Voltage:sector:row:pad",24,0.5,24.5,nrows,0.5,nrows+.5,NoOfPads,0.5,NoOfPads+0.5);
      for (Int_t sector = 1; sector <= 24; sector++) {
	for(Int_t row = 1; row <= St_tpcPadConfigC::instance()->numberOfRows(sector); row++) {
	  Int_t noOfPadsAtRow = St_tpcPadConfigC::instance()->numberOfPadsAtRow(sector,row); 
	  if ( ! St_tpcAnodeHVavgC::instance()->livePadrow(sector,row)) continue;
	  for(Int_t pad = 1; pad<=noOfPadsAtRow; pad++) {
	    Int_t iRdo    = StDetectorDbTpcRDOMasks::instance()->rdoForPadrow(sector,row,pad);
	    if ( ! StDetectorDbTpcRDOMasks::instance()->isOn(sector,iRdo)) continue;
	    Double_t gain = St_tpcPadGainT0BC::instance()->Gain(sector,row,pad);
	    if (gain <= 0.0) continue;
	    AlivePads->Fill(sector, row, pad, gain);
	  }
	}
      }
      ActivePads = new TProfile3D("ActivePads","Cluster Adc:sector:row:pad",24,0.5,24.5,nrows,0.5,nrows+.5,NoOfPads,0.5,NoOfPads+0.5);
    }
#endif /* __CHECK_RDOMAP_AND_VOLTAGE__ */
    for (UInt_t i = 0; i <= TpcHitCollection->numberOfSectors(); i++) {
      StTpcSectorHitCollection* sectorCollection = TpcHitCollection->sector(i);
      if (sectorCollection) {
	Int_t numberOfPadrows = sectorCollection->numberOfPadrows();
	for (Int_t j = 0; j < numberOfPadrows; j++) {
	  StTpcPadrowHitCollection *rowCollection = sectorCollection->padrow(j);
	  if (rowCollection) {
	    StSPtrVecTpcHit &hits = rowCollection->hits();
	    Long_t NoHits = hits.size();
	    for (Long64_t k = 0; k < NoHits; k++) {
	      const StTpcHit *tpcHit = static_cast<const StTpcHit *> (hits[k]);
	      if (!tpcHit) continue;
	      Int_t sector = tpcHit->sector();
	      Int_t row    = tpcHit->padrow();
	      Int_t rowc   = row;
	      if (sector > 12) rowc = - row;
	      Int_t adc    = tpcHit->adc();
	      Double_t Z = tpcHit->position().z();
	      AdcSC->Fill(Z,rowc, adc);
	      if (tpcHit->usedInFit()) {
		if (tpcHit->charge() > 0) {
		  AdcOnTrack->Fill(Z,rowc, adc);
		  dEOnTrack->Fill(Z,rowc, 1e6*tpcHit->charge());
		}
	      }
#ifdef __CHECK_RDOMAP_AND_VOLTAGE__
	      Int_t pad    = tpcHit->pad();
#if 0
	      Int_t iRdo    = StDetectorDbTpcRDOMasks::instance()->rdoForPadrow(sector,row,pad);
	      if ( ! StDetectorDbTpcRDOMasks::instance()->isOn(sector,iRdo)) continue;
#endif
	      ActivePads->Fill(sector, row, pad, adc);
#endif /* __CHECK_RDOMAP_AND_VOLTAGE__ */
	    }
	  }
	}
      }
    }
  }
#endif /* __SpaceCharge__ || __CHECK_RDOMAP_AND_VOLTAGE__ */
#ifdef __BENCHMARKS__DOFIT_ZN__
  Float_t rt, cp;
  myBenchmark.Summary(rt,cp);
#endif /* __BENCHMARKS__DOFIT_ZN__ */
  return kStOK;
}
//________________________________________________________________________________
void StdEdxY2Maker::SortdEdx() {
  Int_t i;
  TArrayI idxT(NdEdx); Int_t *idx = idxT.GetArray();
  TArrayD dT(NdEdx);   Double_t *d = dT.GetArray();
  for (i = 0; i < NdEdx; i++) d[i] = CdEdx[i].F.dEdx;
  TMath::Sort(NdEdx,d,idx,0);
  for (i=0;i<NdEdx;i++) dEdxS[i] = CdEdx[idx[i]];
  TArrayI rowT(NdEdx); Int_t *r = rowT.GetArray();
  for (i = 0; i < NdEdx; i++) r[i] = CdEdx[i].row;
  TMath::Sort(NdEdx,r,idx,0);
  for (i=0;i<NdEdx;i++) FdEdx[i] = CdEdx[idx[i]];
}
//________________________________________________________________________________
void StdEdxY2Maker::Histogramming(StGlobalTrack* gTrack) {
  // Histograms
  static THnSparseF *Time = 0, *TimeC = 0; // , *TimeP = 0
  Int_t NoRows = St_tpcPadConfigC::instance()->numberOfRows(20);
  //  static Hists3D PressureT("PressureT","log(dE/dx)","row","Log(Pressure*298.2/inputGasTemperature)",-NoRows,150, 6.84, 6.99);
  
  //  static Hists3D Volt("Volt","log(dE/dx)","Sector*Channels","Voltage", numberOfSectors*NumberOfChannels,410,990.,1400.);

  static Hists3D AvCurrent("AvCurrent","log(dEdx/Pion)","Sector*Channels","Average Current [#{mu}A]",numberOfSectors*NumberOfChannels,200,0.,1.0);
  static Hists3D Qcm("Qcm","log(dEdx/Pion)","Sector*Channels","Accumulated Charge [uC/cm]",numberOfSectors*NumberOfChannels,200,0.,1000);
  static Hists3D ADC3("ADC3","<logADC)>","sector","row",numberOfSectors,
		      NoRows,0,-1, 
		      100,0.,10.,
		      0,-1,
		      1); //   Hists3D::NtotHist = 1;
#ifndef MakeString
#define MakeString(PATH) # PATH
#endif
#ifdef __dZdY_dXdY__ /* skip dZdY and dXdY */
#define __BOOK__VARS__dZdY(SIGN,NEGPOS) \
  static Hists3D dZdY3 ## SIGN ("dZdY3" MakeString(SIGN) ,"log(dEdx/Pion)" MakeString(NEGPOS) ,"row","dZdY",-NoRows,200,-5,5); \
  static Hists3D dXdY3 ## SIGN ("dXdY3" MakeString(SIGN) ,"log(dEdx/Pion)" MakeString(NEGPOS) ,"row","dXdY",-NoRows,200,-2.5,2.5); 
#else
#define __BOOK__VARS__dZdY(SIGN,NEGPOS)
#endif
#ifdef __Pad_Tmbk__  /* skip Pad and Tbk */
#define __BOOK__VARS__PadTmbk(SIGN,NEGPOS) \
  static Hists3D nPad3 ## SIGN ("nPad3" MakeString(SIGN) ,"log(dEdx/Pion)" MakeString(NEGPOS) ,"row","npad",-NoRows,18,0.5,18.5); \
  static Hists3D nTbk3 ## SIGN ("nTbk3" MakeString(SIGN) ,"log(dEdx/Pion)" MakeString(NEGPOS) ,"row","ntimebuckets",-NoRows,35,2.5,37.5);
#else 
#define __BOOK__VARS__PadTmbk(SIGN,NEGPOS)
#endif

#define __BOOK__VARS__(SIGN,NEGPOS) \
  static Hists3D SecRow3 ## SIGN ("SecRow3" MakeString(SIGN) ,"<log(dEdx/Pion)>"  MakeString(NEGPOS) ,"sector","row",numberOfSectors,NoRows); \
  static Hists3D Pressure ## SIGN ("Pressure" MakeString(SIGN) ,"log(dE/dx)" MakeString(NEGPOS) ,"row","Log(Pressure)",-NoRows,150, 6.84, 6.99); \
  static Hists3D Voltage ## SIGN ("Voltage" MakeString(SIGN) ,"log(dE/dx)" MakeString(NEGPOS) ,"Sector*Channels","Voltage - Voltage_{nominal}", numberOfSectors*NumberOfChannels,22,-210,10); \
  static Hists3D Z3 ## SIGN ("Z3" MakeString(SIGN) ,"<log(dEdx/Pion)>" MakeString(NEGPOS) ,"row","Drift Distance",-NoRows,220,-5,215); \
  static Hists3D G3 ## SIGN ("G3" MakeString(SIGN) ,"<log(dEdx/Pion)>" MakeString(NEGPOS) ,"row","drift time to Gating Grid (us)",-NoRows,100,-5,15); \
  static Hists3D xyPad3 ## SIGN ("xyPad3" MakeString(SIGN) ,"log(dEdx/Pion)" MakeString(NEGPOS) ,"sector+yrow[-0.5,0.5] and xpad [-1,1]"," xpad",numberOfSectors*20, 32,-1,1, 200, -5., 5., 0.5, 24.5); \
  static Hists3D dX3 ## SIGN ("dX3" MakeString(SIGN) ,"log(dEdx/Pion)" MakeString(NEGPOS) ,"row","log2(dX)",-NoRows,40,-0.5,7.5); \
  static Hists3D Eta3 ## SIGN ("Eta3" MakeString(SIGN) ,"log(dEdx/Pion) MC" MakeString(NEGPOS) ,"row","#eta_{G}",-NoRows,50,-2.5,2.5); \
  static Hists3D EtaB3 ## SIGN ("EtaB3" MakeString(SIGN) ,"log(dEdx/Pion) RC" MakeString(NEGPOS) ,"row","#eta_{G}",-NoRows,50,-2.5,2.5); \
__BOOK__VARS__dZdY(SIGN,NEGPOS) \
__BOOK__VARS__PadTmbk(SIGN,NEGPOS)
#if 0 /* skip Pad and Tbk */
  static Hists3D nPad3 ## SIGN ("nPad3" MakeString(SIGN) ,"log(dEdx/Pion)" MakeString(NEGPOS) ,"row","npad",-NoRows,18,0.5,18.5); \
  static Hists3D nTbk3 ## SIGN ("nTbk3" MakeString(SIGN) ,"log(dEdx/Pion)" MakeString(NEGPOS) ,"row","ntimebuckets",-NoRows,35,2.5,37.5);
#endif
  static Hists3D xyPad3qB("xyPad3qB","log(dEdx/Pion) for all","24*qB+sector+yrow[-0.5,0.5] and xpad [-1,1]"," xpad",2*numberOfSectors*20, 32,-1,1, 200, -5., 5., 0.5, 48.5);
#if ! defined(__NEGATIVE_ONLY__) && ! defined(__NEGATIVE_AND_POSITIVE__)
  __BOOK__VARS__(,);
#else
  __BOOK__VARS__(, for negative);
#ifdef __NEGATIVE_AND_POSITIVE__
  __BOOK__VARS__(P, for positive);
#endif
#endif
  static TH2F *ZdcCP = 0, *BBCP = 0;
  //  static TH2F *ctbWest = 0, *ctbEast = 0, *ctbTOFp = 0, *zdcWest = 0, *zdcEast = 0;
  enum  {kTotalMethods = 6};
  //                   ch
  static TH3F *TPoints[2][kTotalMethods] = {0}; // *N[6] = {"B","70B","BU","70BU","N", "NU"}; 0 => "+", 1 => "-";
  static TH3F *NPoints[2][kTotalMethods] = {0}; // *N[6] = {"B","70B","BU","70BU","N", "NU"}; 0 => "+", 1 => "-";
  static StDedxMethod kTPoints[kTotalMethods] = {// {"F","70","FU","70U","N", "NU"};
    kLikelihoodFitId,         // F
    kTruncatedMeanId,         // 70
    kWeightedTruncatedMeanId, // FU
    kEnsembleTruncatedMeanId  // 70U
    ,kOtherMethodId           // N
    ,kOtherMethodId2          // NU
  };
#ifdef  __FIT_PULLS__
  static TH2F *Pulls[2][kTotalMethods] = {0};
  static TH2F *nPulls[2][kTotalMethods] = {0};
  enum {kNPulls = 3};
  struct PullH_t {
    StPidStatus::PiDStatusIDs kPid;
    Hists2D* histograms;
  };
  static PullH_t PullH[kNPulls] = {
    { StPidStatus::kI70,  new Hists2D("I70")},
    { StPidStatus::kFit,  new Hists2D("fitZ")},
    { StPidStatus::kdNdx, 0}
  };
  static Bool_t NotYetDone = kTRUE;
  if (NotYetDone) {
    NotYetDone = kFALSE;
    if (fUsedNdx) PullH[2].histograms = new Hists2D("fitN");
  }
#endif /*  __FIT_PULLS__ */
#ifdef __TEST_DX__
  static TH3F *dXTest[2] = {0};
#endif /* __TEST_DX__ */
  const static Int_t Nlog2dx = 80;
  const static Double_t log2dxLow = 0.0, log2dxHigh = 4.0;
  // ProbabilityPlot
  //  static TH3F *Prob = 0;
  // end of ProbabilityPlot
  static Int_t hMade = 0;
  //#define __ETA_PLOTS__
#ifdef __ETA_PLOTS__
  static TH2F *Eta[2] = {0};     // 0 -> F, 1 -> 70
#endif /*  __ETA_PLOTS__ */ 
  if (! gTrack && !hMade) {
    TFile  *f = GetTFile();
    assert(f);
    f->cd();
    hMade=2004;
    // book histograms
    Bool_t fSetDefaultSumw2 = TH1::GetDefaultSumw2();
    TH1::SetDefaultSumw2(kFALSE);
#ifdef __BEST_VERTEX__
    PVxyz         = new TH3F("PVxyz","xyz for all primary vertices",100,-10,10,100,-10,10,210,-210,210); 
    PVxyzC        = new TH3F("PVxyzC","xyz for the best primary vertex",100,-10,10,100,-10,10,210,-210,210); 
    EtaVspT[0][0] = new TH2F("EtaVspTGlP","Eta vs Log_{10}p_{T} for All positive tracks", 350, -2, 1.5, 600, -3.0, 3.0);
    EtaVspT[0][1] = new TH2F("EtaVspTGlN","Eta vs Log_{10}p_{T} for All negative tracks", 350, -2, 1.5, 600, -3.0, 3.0);
    EtaVspT[1][0] = new TH2F("EtaVspTPrP","Eta vs Log_{10}p_{T} for All positive primary tracks", 350, -2, 1.5, 600, -3.0, 3.0);
    EtaVspT[1][1] = new TH2F("EtaVspTPrN","Eta vs Log_{10}p_{T} for All negative primary tracks", 350, -2, 1.5, 600, -3.0, 3.0);
    EtaVspTC[0]   = new TH2F("EtaVspTPC","Eta vs Log_{10}p_{T} for All positive global tracks used for calibration", 350, -2, 1.5, 600, -3.0, 3.0);
    EtaVspTC[1]   = new TH2F("EtaVspTNC","Eta vs Log_{10}p_{T} for All negative global tracks used for calibration", 350, -2, 1.5, 600, -3.0, 3.0);
#endif /* __BEST_VERTEX__ */
    mHitsUsage  = new TH2F("HitsUsage","log10(No.of Used in dE/dx hits) versus log10(Total no. of Tpc Hits",
			   80,0,8,60,0,6);
    Int_t      nZBins = 200;
    Double_t ZdEdxMin = -5;
    Double_t ZdEdxMax =  5;
    ZdcCP  = new TH2F("ZdcCP","ZdcCoincidenceRate (log10)",100,0,10,nZBins,ZdEdxMin,ZdEdxMax);
    BBCP   = new TH2F("BBCP","BbcCoincidenceRate (log10)",60,0,6,nZBins,ZdEdxMin,ZdEdxMax);
    // TPoints block
    const Char_t *NS[2] = {"P",""};
    const Char_t *TS[2] = {"Positive","Negative"};
    const Char_t *N[kTotalMethods] = {"F","70","FU","70U","N", "NU"};
    const Char_t *T[kTotalMethods] = {"dEdx(fit)/Pion",
				      "dEdx(I70)/Pion",
				      "dEdx(fit_uncorrected)/Pion ",
				      "dEdx(I70_uncorrected)/Pion",
				      "dNdx/Pion",
				      "dNdx(uncorrected)/Pion"};
    for (Int_t t = 0; t < kTotalMethods; t++) {
      if (! fUsedNdx && t >= 4) continue;
      for (Int_t s = 0; s < 2; s++) {// charge 0 => "+", 1 => "-"
 	TPoints[s][t]   = new TH3F(Form("TPoints%s%s",N[t],NS[s]),
	             	      Form("%s versus Length in Tpc and <log_{2}(dX)> in TPC - iTPC %s p > 0.4 GeV/c",T[t],TS[s]),
	             	      190,10,200., Nlog2dx, log2dxLow, log2dxHigh, 500,-1.,4.);
	NPoints[s][t]   = new TH3F(Form("NPoints%s%s",N[t],NS[s]),
				      Form("%s versus no. dEdx points in Tpc and #eta_{G} for %s p > 0.4 GeV/c",T[t],TS[s]),
				      100,0.5,100.5, 40, -2, 2, 500,-1.,4.);
#ifdef  __FIT_PULLS__
	Pulls[s][t] = new TH2F(Form("Pull%s%s",N[t],NS[s]),
			       Form("Pull %s versus Length in TPC %s",T[t],TS[s]),
			       190,10.,200,nZBins,ZdEdxMin,ZdEdxMax);
	nPulls[s][t] = new TH2F(Form("nPull%s%s",N[t],NS[s]),
			       Form("Pull %s versus no. dEdx points in TPC  %s",T[t],TS[s]),
			       100,0.5,100.5,nZBins,ZdEdxMin,ZdEdxMax);
#endif /*  __FIT_PULLS__ */
#ifdef __ETA_PLOTS__
	if (s == 0 && t < 2) {
	  Eta[t] = new TH2F(Form("Eta%s",N[t]),
			    Form("%s for primary tracks versus Eta for |zPV| < 10cm and TpcLength > 40cm, TPC - iTPC",T[t]),
			    100,-2.5,2.5,500,-1.,4.);
	}
#endif /*  __ETA_PLOTS__ */
      }
    }
    TDatime t1(tMin,0); // min Time and
    TDatime t2(tMax,0); // max 
    
    UInt_t i1 = t1.Convert() - timeOffSet;
    UInt_t i2 = t2.Convert() - timeOffSet;
    Int_t Nt = (i2 - i1)/(3600); // each hour 
    //     GainMonitor  = new TH2F("GainMonitor","log(dE/dx)_{corrected} - log(I(pi)) versus GainMonitor", 
    // 			    100,70.,120.,nZBins,ZdEdxMin,ZdEdxMax);
    Int_t    nBins[2] = {Nt, nZBins};
    Double_t xMin[2] = {(Double_t ) i1, ZdEdxMin};
    Double_t xMax[2] = {(Double_t ) i2, ZdEdxMax};
    Time   = new THnSparseF("Time","log(dE/dx)_{uncorrected} - log(I(pi)) versus Date& Time", 2, nBins, xMin, xMax); f->Add(Time);
    TimeC  = new THnSparseF("TimeC","log(dE/dx)_{corrected} - log(I(pi)) versus Date& Time after correction", 2, nBins, xMin, xMax); f->Add(TimeC);
    //    TimeP  = new THnSparseF("TimeP","log(dE/dx)_{after pressure correction} - log(I(pi)) versus Date& Time",  2, nBins, xMin, xMax); f->Add(TimeP);
    TH1::SetDefaultSumw2(fSetDefaultSumw2);
#ifdef __TEST_DX__
    if (! dXTest[0]) {
      dXTest[0] = new TH3F("dxTestP","dX = dX_TrackFit - dX_Helix > 1e-4 versus pad row and dX_TrackFit for Positive",145,-72.5,72.5,100,-1.,9.,100,-0.25,0.25);
      dXTest[1] = new TH3F("dxTest" ,"dX = dX_TrackFit - dX_Helix > 1e-4 versus pad row and dX_TrackFit for Negative",145,-72.5,72.5,100,-1.,9.,100,-0.25,0.25);
    }
#endif /* __TEST_DX__ */
    return;
  }
  // fill histograms 
  tpcGas_st           *tpcGas = 0;
  if ( m_TpcdEdxCorrection && m_TpcdEdxCorrection->tpcGas()) tpcGas = m_TpcdEdxCorrection->tpcGas()->GetTable();
  
  StThreeVectorD g3 = gTrack->geometry()->momentum(); // p of global track
  Double_t pMomentum = g3.mag();
  Double_t etaG = g3.pseudoRapidity();
  StPidStatus PiDs[2] = {StPidStatus(gTrack, kTRUE), StPidStatus(gTrack, kFALSE)} ; 
  StPidStatus &PiD = fUsedx2 ? *&PiDs[0] :  *&PiDs[1];
  if (PiD.PiDStatus < 0) return;
  //  Double_t bg = TMath::Log10(pMomentum/StProbPidTraits::mPidParticleDefinitions[kPidPion]->mass());
  Int_t sCharge = 0;                                 // positive
  if (gTrack->geometry()->charge() < 0) sCharge = 1; // negative
#ifdef __BEST_VERTEX__
  EtaVspTC[sCharge]->Fill(TMath::Log10(g3.perp()), g3.pseudoRapidity());
#endif /* __BEST_VERTEX__ */
#ifdef __TEST_DX__
  if (dXTest[0]) {
    for (Int_t k = 0; k < NdEdx; k++) {
      Int_t sector = FdEdx[k].sector;
      Int_t row    = FdEdx[k].row;
      Int_t rowS   = row;
      if (sector > 12) rowS = - rowS;
      if (FdEdx[k].dX_TrackFit > 0 && FdEdx[k].dX_Helix > 0) {
	if (TMath::Abs(FdEdx[k].dX_TrackFit - FdEdx[k].dX_Helix) > 1e-4) dXTest[sCharge]->Fill(rowS, FdEdx[k].dX_TrackFit, FdEdx[k].dX_Helix - FdEdx[k].dX_TrackFit);
      } else {
	if (FdEdx[k].dX_TrackFit  > 1e-4) dXTest[sCharge]->Fill(0., FdEdx[k].dX_TrackFit, 0.1);
	if (FdEdx[k].dX_Helix     > 1e-4) dXTest[sCharge]->Fill(0., FdEdx[k].dX_Helix,   -0.1);
      }
    }
    
  }
#endif /* __TEST_DX__ */
  StDedxMethod kMethod;
#ifdef  __FIT_PULLS__
  // Pulls
  Int_t k = PiD.PiDkeyU3;
  Int_t l;
  for (Int_t m = 0; m < kNPulls; m++) {// I70, Ifit, dNdx
    if (! PullH[m].histograms) continue;
    if (! PiD.fStatus[PullH[m].kPid]) continue;
    for (l = kPidElectron; l < KPidParticles; l++) {
      if (PiD.fI70 && PiD.fI70->fPiD) {
	PullH[m].histograms->dev[l][sCharge]->Fill(PiD.bghyp[l], PiD.fStatus[PullH[m].kPid]->dev[l]);
	PullH[m].histograms->dev[l][      2]->Fill(PiD.bghyp[l], PiD.fStatus[PullH[m].kPid]->dev[l]);
	if (k >= 0) {
	  PullH[m].histograms->devT[l][sCharge]->Fill(PiD.bghyp[l], PiD.fStatus[PullH[m].kPid]->dev[l]);
	  PullH[m].histograms->devT[l][      2]->Fill(PiD.bghyp[l], PiD.fStatus[PullH[m].kPid]->dev[l]);
	}
      }
    }
  }
#endif /*  __FIT_PULLS__ */
  for (Int_t j = 0; j < kTotalMethods; j++) {
    kMethod = kTPoints[j];
    if (pMomentum > 0.4) {
      if (PiDs[1].dEdxStatus(kMethod)) {
	TPoints[sCharge][j]->Fill(PiDs[1].dEdxStatus(kMethod)->TrackLength(),PiDs[1].dEdxStatus(kMethod)->log2dX(),PiDs[1].dEdxStatus(kMethod)->dev[kPidPion]);
	NPoints[sCharge][j]->Fill(PiDs[1].dEdxStatus(kMethod)->N(), etaG, PiDs[1].dEdxStatus(kMethod)->dev[kPidPion]);
#if 1
	if (Debug() > 100) {
	  gTrack->Print();
	  cout << "TPoints[" << sCharge << "][" << j << "] => " << TPoints[sCharge][j]->GetName() << "\t" << TPoints[sCharge][j]->GetTitle() << endl;
	}
#endif
      }
    }
    if (PiD.dEdxStatus(kMethod)) {
#ifdef  __FIT_PULLS__
      Pulls[sCharge][j]->Fill(PiD.dEdxStatus(kMethod)->TrackLength(),PiD.dEdxStatus(kMethod)->devS[kPidPion]);
      nPulls[sCharge][j]->Fill(PiD.dEdxStatus(kMethod)->N(),PiD.dEdxStatus(kMethod)->devS[kPidPion]);
#endif /*  __FIT_PULLS__ */
#ifdef __ETA_PLOTS__
      if (j < 2 && PiD.dEdxStatus(kMethod)->TrackLength() > 40) {
	StTrackNode *node = gTrack->node();
	StPrimaryTrack *pTrack = static_cast<StPrimaryTrack*>(node->track(primary));
	if (pTrack) {
	  StPrimaryVertex s*primVx = (StPrimaryVertex *) pTrack->vertex();
	  if (primVx) {
	    if (TMath::Abs(primVx->position().z()) < 10) {
	      StThreeVectorD P = pTrack->geometry()->helix().momentum(bField);
	      Double_t eta = P.pseudoRapidity();
	      Eta[j]->Fill(eta,PiD.dEdxStatus(kMethod)->dev[kPidPion]);
	    }
	  }
	}
      }
#endif /*  __ETA_PLOTS__ */
    }
  }
  kMethod = kLikelihoodFitId;
  if (PiD.dEdxStatus(kMethod) && PiD.dEdxStatus(kMethod)->TrackLength() > 20) { 
    //  if (NoFitPoints >= 20) { 
    Int_t k;
    Double_t betagamma = pMomentum/StProbPidTraits::mPidParticleDefinitions[kPidPion]->mass();
#if 0
    Double_t bgL10 = TMath::Log10(betagamma);
#endif
    for (k = 0; k < NdEdx; k++) {
      Int_t sector = FdEdx[k].sector;
      Int_t row    = FdEdx[k].row;
      Int_t rowS   = row;
      if (sector > 12) rowS = - rowS;
#if 0
      FdEdx[k].zP = // Bichsel::Instance()->GetMostProbableZ(bgL10,1.);
	Bichsel::Instance()->GetMostProbableZ(bgL10,TMath::Log2(FdEdx[k].F.dx)); //remove dX
      FdEdx[k].sigmaP = //Bichsel::Instance()->GetRmsZ(bgL10,1.);
	Bichsel::Instance()->GetRmsZ(bgL10,TMath::Log2(FdEdx[k].F.dx)); //remove dX	
      Double_t predB  = 1.e-6*TMath::Exp(FdEdx[k].zP);
      FdEdx[k].F.dEdxN  = TMath::Log(FdEdx[k].F.dEdx /predB);
#else
#if 0
      if (! PiD.fdNdx)  continue; // 
      Double_t n_P = FdEdx[k].dxC*PiD.fdNdx->Pred[kPidPion];
      //      Double_t zdEMPV = StdEdxModel::instance()->LogdEMPVGeV(n_P);//LogdEMPV(n_P) - Bichsel::Instance()->Parameterization()->MostProbableZShift(); 
#else
      Double_t n_P = FdEdx[k].dxC*StdEdxModel::instance()->dNdxEff(betagamma);
#endif
      Double_t zdEMPV = StdEdxModel::instance()->LogdEMPVGeV(n_P);//LogdEMPV(n_P) - Bichsel::Instance()->Parameterization()->MostProbableZShift(); 
      FdEdx[k].zP = zdEMPV;
      FdEdx[k].sigmaP = StdEdxModel::instance()->Sigma(n_P);
      Double_t predB  = TMath::Exp(FdEdx[k].zP);
      FdEdx[k].F.dEdxN  = TMath::Log(FdEdx[k].F.dE/predB);
#endif
      for (Int_t l = 0; l <= StTpcdEdxCorrection::kTpcLast; l++) {
	if (l == StTpcdEdxCorrection::kzCorrection || 
	    l == StTpcdEdxCorrection::kzCorrectionC) {
	  FdEdx[k].C[l].dEdxN = FdEdx[k].F.dEdxN - (FdEdx[k].C[ StTpcdEdxCorrection::kzCorrectionC].ddEdxL + 
						    FdEdx[k].C[ StTpcdEdxCorrection::kzCorrection ].ddEdxL);
	} else if (l == StTpcdEdxCorrection::kTpcSecRowB ||
	   	   l == StTpcdEdxCorrection::kTpcSecRowC ||
	   	   l == StTpcdEdxCorrection::kTpcRowQ) {    
	  FdEdx[k].C[l].dEdxN = FdEdx[k].F.dEdxN - (FdEdx[k].C[StTpcdEdxCorrection::kTpcSecRowB].ddEdxL +
	   					    FdEdx[k].C[StTpcdEdxCorrection::kTpcSecRowC].ddEdxL +
	   					    FdEdx[k].C[StTpcdEdxCorrection::kTpcRowQ].ddEdxL);   
	} else if (l == StTpcdEdxCorrection::kTpcPadMDF ||
	   	   l == StTpcdEdxCorrection::kTpcPadMDC ) {
	  FdEdx[k].C[l].dEdxN = FdEdx[k].F.dEdxN - (FdEdx[k].C[StTpcdEdxCorrection::kTpcPadMDF].ddEdxL +
	   					    FdEdx[k].C[StTpcdEdxCorrection::kTpcPadMDC].ddEdxL);
	} else {
	  FdEdx[k].C[l].dEdxN = FdEdx[k].F.dEdxN - FdEdx[k].C[l].ddEdxL;
	}
	if (l) FdEdx[k].C[l].dx = FdEdx[k].C[l-1].dx;
      }
      Int_t cs = NumberOfChannels*(sector-1)+FdEdx[k].channel;
      //      if (pMomentum > pMomin && pMomentum < pMomax &&PiD.dEdxStatus(kMethod)->TrackLength() > 40 ) continue; // { // Momentum cut
      if (pMomentum > pMomin && pMomentum < pMomax) { // Momentum cut
	if (St_trigDetSumsC::instance()) {
	  if (FdEdx[k].Zdc > 0 && ZdcCP) ZdcCP->Fill(TMath::Log10(FdEdx[k].Zdc), FdEdx[k].F.dEdxN);
	  if (St_trigDetSumsC::instance()->bbcX() > 0)  {
	    if (BBCP) BBCP->Fill(TMath::Log10(St_trigDetSumsC::instance()->bbcX()), FdEdx[k].F.dEdxN);
	  }
	}
	Double_t Vars[4] = {
	  FdEdx[k].C[StTpcdEdxCorrection::kTpcSecRowB].dEdxN, 
	  FdEdx[k].F.dEdxN,
	  0, 
	  FdEdx[k].F.dx};
#if 0
	Double_t dEN = 0;
	Double_t zdEMPV = 0;
	if (PiD.fdNdx) {
	  Double_t n_P = FdEdx[k].dxC*PiD.fdNdx->Pred[kPidPion];
	  dEN = TMath::Log(FdEdx[k].F.dE); // scale to <dE/dx>_MIP = 2.4 keV/cm
	  zdEMPV = StdEdxModel::instance()->LogdEMPV(n_P); // ? Check dx
	  Vars[2] = dEN - zdEMPV;
	};
#endif
	// SecRow3
	Double_t V = FdEdx[k].Voltage;
	Double_t VN = (row <= St_tpcPadConfigC::instance()->innerPadRows(sector)) ? V - 1170 : V - 1390;
	Double_t press = 0;
	// ADC3 
	if (FdEdx[k].adc > 0) {
	  Double_t ADCL = TMath::Log(FdEdx[k].adc);
	  ADC3.Fill(sector,row,&ADCL);
	}
	  
	if (tpcGas) {
	  Double_t p     = tpcGas->barometricPressure;
	  if (p > 0) {
	    press = TMath::Log(p);
	  }
	  Vars[0] = FdEdx[k].C[StTpcdEdxCorrection::kTpcAccumulatedQ].dEdxN;
	  Qcm.Fill(cs,FdEdx[k].Qcm,Vars);
	  Vars[0] = FdEdx[k].C[StTpcdEdxCorrection::kTpcCurrentCorrection].dEdxN;
	  AvCurrent.Fill(cs,FdEdx[k].Crow,Vars);
	}
	Double_t vars[2] = {tpcTime,FdEdx[k].C[ StTpcdEdxCorrection::ktpcTime].dEdxN};
	if (Time)    Time->Fill(vars);
	//	if (TimeP)  {vars[1] = FdEdx[k].C[StTpcdEdxCorrection::ktpcTime].dEdxN; TimeP->Fill(vars);}
	if (TimeC)  {vars[1] = FdEdx[k].F.dEdxN; TimeC->Fill(vars);}
	Double_t VarsY[8] = {0};
#ifdef __dZdY_dXdY__
#define __FILL___VARS__dZdY(SIGN) \
	Vars[0] = FdEdx[k].C[StTpcdEdxCorrection::kdZdY].dEdxN;   \
	dZdY3  ## SIGN .Fill(rowS,FdEdx[k].dZdY,Vars);       \
	Vars[0] = FdEdx[k].C[StTpcdEdxCorrection::kdXdY].dEdxN;   \
	dXdY3  ## SIGN .Fill(rowS,FdEdx[k].dXdY,Vars);	
#else
#define __FILL__VARS__dZdY(SIGN)
#endif
#ifdef __Pad_Tmbk__  /* skip Pad and Tbk */
#define __FILL__VARS__PadTmbk(SIGN) \
	Vars[0] = FdEdx[k].C[StTpcdEdxCorrection::knPad].dEdxN;   \
	nPad3  ## SIGN .Fill(rowS,FdEdx[k].Npads,&Vars[1]);       \
	Vars[0] = FdEdx[k].C[StTpcdEdxCorrection::knTbk].dEdxN;   \
	nTbk3  ## SIGN .Fill(rowS,FdEdx[k].Ntbks,&Vars[1]);      
#else
#define __FILL__VARS__PadTmbk(SIGN)
#endif
#define __FILL__VARS__(SIGN) \
	Vars[0] = FdEdx[k].C[StTpcdEdxCorrection::kTpcSecRowB].dEdxN; \
	SecRow3 ## SIGN .Fill(sector,row,Vars);			       \
	Voltage ## SIGN .Fill(cs,VN,Vars);			       \
	Vars[0] = FdEdx[k].C[StTpcdEdxCorrection::kzCorrection].dEdxN; \
	Z3     ## SIGN .Fill(rowS,FdEdx[k].ZdriftDistance,Vars);     \
	Vars[0] = FdEdx[k].C[StTpcdEdxCorrection::kGatingGrid].dEdxN; \
	G3     ## SIGN .Fill(rowS,FdEdx[k].driftTime,Vars);     \
	Vars[0] = FdEdx[k].C[StTpcdEdxCorrection::kTpcPadMDF].dEdxN;   \
	xyPad3 ## SIGN .Fill(FdEdx[k].yrow,FdEdx[k].xpad, Vars); \
	VarsY[0] = TMath::Log2(FdEdx[k].C[StTpcdEdxCorrection::kdXCorrection].dx); \
        VarsY[1] = TMath::Log2(FdEdx[k].F.dx); \
	Vars[0] = FdEdx[k].C[StTpcdEdxCorrection::kdXCorrection].dEdxN;   \
	dX3  ## SIGN .FillY(rowS,VarsY,Vars);	\
	Vars[0] = FdEdx[k].C[StTpcdEdxCorrection::kEtaCorrection].dEdxN;   \
	Eta3  ## SIGN .Fill(rowS,FdEdx[k].etaG,Vars);	\
	Vars[0] = FdEdx[k].C[StTpcdEdxCorrection::kEtaCorrectionB].dEdxN;   \
	EtaB3  ## SIGN .Fill(rowS,FdEdx[k].etaG,Vars);	\
	Vars[0] = FdEdx[k].C[StTpcdEdxCorrection::ktpcPressure].dEdxN;   \
	Pressure ## SIGN.Fill(rowS,press,Vars); \
__FILL__VARS__dZdY(SIGN) \
__FILL__VARS__PadTmbk(SIGN) 
#if ! defined(__NEGATIVE_ONLY__) && ! defined(__NEGATIVE_AND_POSITIVE__)
	__FILL__VARS__();
#else /* ! __NEGATIVE_AND_POSITIVE__ */
  #if defined(__NEGATIVE_ONLY__) || defined(__NEGATIVE_AND_POSITIVE__)
	if (sCharge == 1)  {
	__FILL__VARS__();
	}
     #if defined(__NEGATIVE_AND_POSITIVE__)
	if (sCharge == 0)  {
	__FILL__VARS__(P);
	}
     #endif /* __NEGATIVE_AND_POSITIVE__ */
  #endif /* __NEGATIVE_ONLY__ || __NEGATIVE_AND_POSITIVE__ */
#endif /* __NEGATIVE_AND_POSITIVE__ */
	Vars[0] = FdEdx[k].C[StTpcdEdxCorrection::kTpcPadMDC].dEdxN;	
	xyPad3qB.Fill(24*FdEdx[k].qB+FdEdx[k].yrow,FdEdx[k].xpadR, Vars); 
      } // MIP momentum cut
    } // loop over dEdx points 
  }
  return;
}
//_____________________________________________________________________________
void StdEdxY2Maker::PrintdEdx(Int_t iop) {
  const Int_t NOpts = 20;
  const Char_t *Names[NOpts] = {"CdEdx","FdEdx","dEdxS","dEdxU","dEdxR",
				"dEdxS","dEdxS","dEdxP","dEdxt","dEdxO",
				"dEdxM","dEdxZ","dEdxm","dEdxT","dEdxW",
				"dEdxC","dEdxE","dEdxp","dEdxX","dEdxd"};
  if (iop < 0 || iop >= NOpts) return;
  dEdxY2_t *pdEdx = 0; 
  Double_t dEdx;
  Double_t I = 0;
  Int_t N70 = NdEdx - (int) (0.3*NdEdx + 0.5); 
  Int_t N60 = NdEdx - (int) (0.4*NdEdx + 0.5);
  Double_t I70 = 0, I60 = 0;
  Double_t avrz = 0;
  for (Int_t i=0; i< NdEdx; i++) {
    dEdx = 0;
    if (iop == 0)      {pdEdx = &CdEdx[i]; dEdx = CdEdx[i].F.dEdx;}
    else if (iop == 1) {pdEdx = &FdEdx[i]; dEdx = FdEdx[i].F.dEdx;}
    else if (iop == 2) {pdEdx = &dEdxS[i]; dEdx = dEdxS[i].F.dEdx;}
    else if (iop >= 3) {pdEdx = &FdEdx[i]; dEdx = FdEdx[i].C[StTpcdEdxCorrection::kUncorrected+iop-3].dEdx;}
    I = (i*I + 1e6*pdEdx->F.dEdx)/(i+1);
    //     cout << Names[iop] << " " << i << " S/R " << dEdx->sector << "/" << dEdx->row
    // 	 << " dEdx(keV/cm) " << 1.e6*dEdx->dEdx << " dx " << dEdx->dx 
    //       //	 << " dx " << dEdx->dxH 
    // 	 << " x[" << dEdx->xyz[0] << "," << dEdx->xyz[1] << "," << dEdx->xyz[2] << "]" 
    // 	 << " d[" << dEdx->xyzD[0] << "," << dEdx->xyzD[1] << "," << dEdx->xyzD[2] << "]" 
    // 	 << " R[" << dEdx->resXYZ[0] << "," << dEdx->resXYZ[1] << "," << dEdx->resXYZ[2] << "]" 
    // 	 << " Sum " << 1.e6*I << "(keV)"
    // 	 << " Prob " << dEdx->Prob << endl;
    dEdx *= 1e6;
    cout << Form("%s %2i  S/R %2i/%2i dEdx(keV/cm) %8.2f dx %5.2f dxC %5.2f x[%8.2f,%8.2f,%8.2f] Qcm %7.2f AvC %7.3f", 
		 Names[iop],i,pdEdx->sector,pdEdx->row,dEdx, pdEdx->F.dx ,pdEdx->dxC, pdEdx->xyz[0], pdEdx->xyz[1], 
		 pdEdx->xyz[2],pdEdx->Qcm,pdEdx->Crow);
    cout << Form(" d[%8.2f,%8.2f,%8.2f] Sum %8.2f Prob %8.5f", pdEdx->xyzD[0], pdEdx->xyzD[1], pdEdx->xyzD[2],
		 I,pdEdx->Prob) << endl;
    if (iop == 2) {
      if (i < N60) I60 += dEdx;
      if (i < N70) I70 += dEdx;
      if (i == N60 - 1) {
	I60 /= N60;
	cout << " ======================= I60 \t" << I60 << endl;
      }
      if (i == N70 - 1) {
	I70 /= N70;
	cout << " ======================= I70 \t" << I70 << endl;
      }
    }
    avrz += TMath::Log(dEdx);
  }
  if (NdEdx) avrz /= NdEdx;
  cout << "mean dEdx \t" << I << "\tExp(avrz)\t" << TMath::Exp(avrz) << endl;
}
//________________________________________________________________________________
Double_t StdEdxY2Maker::LikeliHood(Double_t Xlog10bg, Int_t NdEdx, dEdxY2_t *dEdx, Double_t chargeSq) {
  //SecRowMipFitpHist298P02gh1.root  correction to most probable value vs log2(dx)
  //  static const Double_t probdx2[3] = {-3.58584e-02, 4.16084e-02,-1.45163e-02};// 
  const static Double_t ProbCut = 1.e-4;
  const static Double_t GeV2keV = TMath::Log(1.e-6);
  Double_t f = 0;
  for (Int_t i=0;i<NdEdx; i++) {
    Double_t Ylog2dx = TMath::Log2(dEdx[i].F.dx);
    //    Double_t Ylog2dx = 1;
    Double_t sigmaC = 0;
    Double_t zMostProb = Bichsel::Instance()->GetMostProbableZ(Xlog10bg,Ylog2dx) + TMath::Log(chargeSq);
    Double_t sigma     = Bichsel::Instance()->GetRmsZ(Xlog10bg,Ylog2dx) + sigmaC;
    Double_t xi = (dEdx[i].F.dEdxL - GeV2keV - zMostProb)/sigma;
    Double_t  Phi = Bichsel::Instance()->GetProbability(Xlog10bg,Ylog2dx,xi);
    dEdx[i].Prob = Phi/sigma;
    if (dEdx[i].Prob < ProbCut) {
      dEdx[i].Prob = ProbCut; 
    }
    f -= 2*TMath::Log( dEdx[i].Prob );
  }
  return f;
}
//________________________________________________________________________________
void StdEdxY2Maker::Landau(Double_t x, Double_t *val){
  //  TF1 *LandauF = new TF1("LandauF","exp([0]-0.5*((x-[1])/[2])**2+exp([3]-0.5*((x-[4])/[5])**2+exp([6]-0.5*((x-[7])/[8])**2)))",-4,10);
  Double_t params[9] = {
    -7.74975e+00,
    6.53414e+00,
    1.21524e+00,
    3.31409e+00,
    -2.58291e+00,
    3.51463e+00,
    -3.47755e+00,
    3.77698e-02,
    6.67913e-01};
  Double_t dev1 = (x-params[1])/params[2];
  Double_t dev2 = (x-params[4])/params[5];
  Double_t dev3 = (x-params[7])/params[8];
  Double_t d    = TMath::Exp(params[6]-0.5*dev3*dev3);
  Double_t dp   = -dev3/params[8]*d;
  Double_t c    = TMath::Exp(params[3]-0.5*dev2*dev2 + d);
  Double_t cp   = (-dev2/params[5] + dp)*c;
  val[0]        = params[0]-0.5*dev1*dev1+c;
  val[1]        = - dev1/params[2]+cp;
}
//________________________________________________________________________________
void StdEdxY2Maker::fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag) {
  Double_t Val[2];
  // P10
  // dEdxS->Draw("sigma_z:log(x)/log(2)","","prof")
  static Double_t sigma_p[3] = { 5.66734e-01,   -1.24725e-01,   1.96085e-02};
  f = 0.;
  gin[0] = 0.;
  gin[1] = 0.;
  for (Int_t i=0;i<NdEdx; i++) {
    //    Double_t sigma = StTpcdEdxCorrection::SumSeries(TMath::Log(FdEdx[i].dx),3,sigma_p);
    Double_t X = TMath::Log(FdEdx[i].F.dx);
    Double_t sigma = sigma_p[2];
    for (Int_t n = 1; n>=0; n--) sigma = X*sigma + sigma_p[n];
    FdEdx[i].zdev    = (FdEdx[i].F.dEdxL-par[0])/sigma;
    Landau(FdEdx[i].zdev,Val);
    FdEdx[i].Prob = TMath::Exp(Val[0]);
    f      -= Val[0];
    gin[0] += Val[1]/sigma;
  }
}
//________________________________________________________________________________
void StdEdxY2Maker::DoFitZ(Double_t &chisq, Double_t &fitZ, Double_t &fitdZ){
  Double_t avz = 0;
  for (Int_t i=0;i<NdEdx;i++) avz += FdEdx[i].F.dEdxL;
  if (NdEdx>5) {
    avz /= NdEdx;
    Double_t arglist[10];
    Int_t ierflg = 0;
    m_Minuit->SetFCN(fcn);
    //    m_Minuit->SetPrintLevel(-1);
    if (Debug() < 2) {
      arglist[0] = -1;
      m_Minuit->mnexcm("set print",arglist, 1, ierflg);
    }
    arglist[0] = 0.0;
    m_Minuit->mnexcm("set NOW",arglist, 0, ierflg);
    m_Minuit->mnexcm("CLEAR",arglist, 0, ierflg);
    arglist[0] = 0.5;
    m_Minuit->mnexcm("SET ERR", arglist ,1,ierflg);
    m_Minuit->mnparm(0, "mean", avz, 0.01,0.,0.,ierflg); //First Guess
    if (Debug() < 2)       arglist[0] = 1.;   // 1.
    else                   arglist[0] = 0.;   // Check gradient 
    m_Minuit->mnexcm("SET GRAD",arglist,1,ierflg);
    arglist[0] = 500;
    arglist[1] = 1.;
    m_Minuit->mnexcm("MIGRAD", arglist ,2,ierflg);
    m_Minuit->mnexcm("HESSE  ",arglist,0,ierflg);
    Double_t edm,errdef;
    Int_t nvpar,nparx,icstat;
    m_Minuit->mnstat(chisq,edm,errdef,nvpar,nparx,icstat);
    m_Minuit->GetParameter(0, fitZ, fitdZ);
  }
  else {
    fitZ = fitdZ = chisq =  -999.;
  }
}
//________________________________________________________________________________
void StdEdxY2Maker::TrigHistos(Int_t iok) {
  static TProfile *BarPressure = 0, *inputTPCGasPressure = 0;
  static TProfile *nitrogenPressure = 0, *gasPressureDiff = 0, *inputGasTemperature = 0;
  static TProfile *flowRateArgon1 = 0, *flowRateArgon2 = 0, *flowRateMethane = 0;
  static TProfile *percentMethaneIn = 0, *ppmOxygenIn = 0, *flowRateExhaust = 0;
  static TProfile *ppmWaterOut = 0, *ppmOxygenOut = 0, *flowRateRecirculation = 0;
  //  static TProfile *Center = 0, *Height = 0, *Width = 0,  *CenterPressure = 0;
  static TH1F *Multiplicity; // mult rate 
  static TH1F *ZdcC = 0; // ZdcCoincidenceRate
  static TH1F *BBC   = 0; // BbcCoincidenceRate
  if (! iok && !BarPressure) {
    TDatime t1(tMin,0); // min Time and
    TDatime t2(tMax,0); // max 
    UInt_t i1 = t1.Convert() - timeOffSet;
    UInt_t i2 = t2.Convert() - timeOffSet;
    Int_t Nt = (i2 - i1)/(3600); // each hour 
    BarPressure           = new TProfile("BarPressure","barometricPressure (mbar) versus time",Nt,i1,i2);                    
    inputTPCGasPressure   = new TProfile("inputTPCGasPressure","inputTPCGasPressure (mbar) versus time",Nt,i1,i2);           
    nitrogenPressure      = new TProfile("nitrogenPressure","nitrogenPressure (mbar) versus time",Nt,i1,i2);                 
    gasPressureDiff       = new TProfile("gasPressureDiff","gasPressureDiff (mbar) versus time",Nt,i1,i2);                   
    inputGasTemperature   = new TProfile("inputGasTemperature","inputGasTemperature (degrees K) versus time",Nt,i1,i2);      
    flowRateArgon1        = new TProfile("flowRateArgon1","flowRateArgon1 (liters/min) versus time",Nt,i1,i2);               
    flowRateArgon2        = new TProfile("flowRateArgon2","flowRateArgon2 (liters/min) versus time",Nt,i1,i2);               
    flowRateMethane       = new TProfile("flowRateMethane","flowRateMethane (liters/min) versus time",Nt,i1,i2);             
    percentMethaneIn      = new TProfile("percentMethaneIn","percentMethaneIn (percent) versus time",Nt,i1,i2);              
    ppmOxygenIn           = new TProfile("ppmOxygenIn","ppmOxygenIn (ppm) versus time",Nt,i1,i2);                            
    flowRateExhaust       = new TProfile("flowRateExhaust","flowRateExhaust (liters/min) versus time",Nt,i1,i2);             
    ppmWaterOut           = new TProfile("ppmWaterOut","ppmWaterOut (ppm) versus time",Nt,i1,i2);                             
    ppmOxygenOut          = new TProfile("ppmOxygenOut","ppmOxygenOut (ppm) versus time",Nt,i1,i2);
    flowRateRecirculation = new TProfile("flowRateRecirculation","flowRateRecirculation (liters/min) versus time",Nt,i1,i2);
    //     CenterPressure        = new TProfile("CenterPressureP","log(center) vs log(Pressure)",150, 6.84, 6.99);
    //     Center                = new TProfile("Center","Tpc Gain Monitor center versus Time",Nt,i1,i2);
    //     Height 		  = new TProfile("Height","Tpc Gain Monitor height versus Time",Nt,i1,i2);
    //     Width  		  = new TProfile("Width","Tpc Gain Monitor width versus Time",Nt,i1,i2);  
    // trigDetSums histograms
    ZdcC                  = new TH1F("ZdcC","ZdcCoincidenceRate (log10)",100,0,10);
    Multiplicity          = new TH1F("Multiplicity","Multiplicity (log10)",100,0,10);
    BBC                   = new TH1F("BBC","BbcCoincidenceRate (log10)",100,0,10);
  } else {
    tpcGas_st  *tpcgas = m_TpcdEdxCorrection && m_TpcdEdxCorrection->tpcGas() ? m_TpcdEdxCorrection->tpcGas()->GetTable():0;
    if (tpcgas) {
      if (BarPressure)           BarPressure->Fill(tpcTime,tpcgas->barometricPressure);             
      if (inputTPCGasPressure)   inputTPCGasPressure->Fill(tpcTime,tpcgas->inputTPCGasPressure);    
      if (nitrogenPressure)      nitrogenPressure->Fill(tpcTime,tpcgas->nitrogenPressure);          
      if (gasPressureDiff)       gasPressureDiff->Fill(tpcTime,tpcgas->gasPressureDiff);            
      if (inputGasTemperature)   inputGasTemperature->Fill(tpcTime,tpcgas->inputGasTemperature);    
      if (flowRateArgon1)        flowRateArgon1->Fill(tpcTime,tpcgas->flowRateArgon1);              
      if (flowRateArgon2)        flowRateArgon2->Fill(tpcTime,tpcgas->flowRateArgon2);              
      if (flowRateMethane)       flowRateMethane->Fill(tpcTime,tpcgas->flowRateMethane);            
      if (percentMethaneIn)      
	percentMethaneIn->Fill(tpcTime,tpcgas->percentMethaneIn*1000./tpcgas->barometricPressure);          
      if (ppmOxygenIn)           ppmOxygenIn->Fill(tpcTime,tpcgas->ppmOxygenIn);                    
      if (flowRateExhaust)       flowRateExhaust->Fill(tpcTime,tpcgas->flowRateExhaust);            
      if (ppmWaterOut)           ppmWaterOut->Fill(tpcTime,tpcgas->ppmWaterOut);                    
      if (ppmOxygenOut)          ppmOxygenOut->Fill(tpcTime,tpcgas->ppmOxygenOut);                
      if (flowRateRecirculation) flowRateRecirculation->Fill(tpcTime,tpcgas->flowRateRecirculation);
    }
    if (St_trigDetSumsC::instance()) {
      if (St_trigDetSumsC::instance()->zdcX() > 0 && ZdcC) ZdcC->Fill(TMath::Log10(St_trigDetSumsC::instance()->zdcX()));
      if (St_trigDetSumsC::instance()->bbcX() > 0 && BBC) BBC->Fill(TMath::Log10(St_trigDetSumsC::instance()->bbcX()));
      if (St_trigDetSumsC::instance()->mult() > 0 && Multiplicity) Multiplicity->Fill(TMath::Log10(St_trigDetSumsC::instance()->mult()));
    }
  } // (TESTBIT(m_Mode, kGASHISTOGRAMS))
}
//________________________________________________________________________________
void StdEdxY2Maker::XyzCheck(StGlobalCoordinate *global, Int_t iokCheck) {
  static TH3F *XYZ = 0, *XYZbad = 0;
  if (! global && !XYZ) {
    if (Debug()) LOG_WARN << "StdEdxY2Maker::XyzCheck XYZ check Histograms" << endm;
    XYZ    = new TH3F("XYZ","xyz for clusters",80,-200,200,80,-200,200,84,-210,210);
    XYZbad = new TH3F("XYZbad","xyz for clusters with mismatched sectors",
		      80,-200,200,80,-200,200,84,-210,210);
  }
  else 
    if (XYZ) XYZ->Fill( global->position().x(), global->position().y(), global->position().z());
  if (iokCheck && XYZbad) XYZbad->Fill( global->position().x(), global->position().y(), global->position().z());
}
//________________________________________________________________________________
void StdEdxY2Maker::QAPlots(StGlobalTrack* gTrack) {
  static TH2F *fTdEdx[3][5];
  static TH2F *fqTdEdx[3];
  static StTpcDedxPidAlgorithm PidAlgorithm70(kTruncatedMeanId);
  static StTpcDedxPidAlgorithm PidAlgorithmFitZ(kLikelihoodFitId);
  static StTpcDedxPidAlgorithm PidAlgorithmFitN(kOtherMethodId);
  static StElectron* Electron = StElectron::instance();
  static StPionPlus* Pion = StPionPlus::instance();
  static StKaonPlus* Kaon = StKaonPlus::instance();
  static StProton* Proton = StProton::instance();
  static const Double_t Log10E = TMath::Log10(TMath::Exp(1.));
  static Int_t first=0;
  if (! gTrack) {
    TFile *f = 0;
    if (TESTBIT(m_Mode, kCalibration)) {
      f = GetTFile();
      if (f) f->cd();
    }
    if (!first) {
      fZOfBadHits = new TH1F*[fNZOfBadHits]; memset(fZOfBadHits, 0, fNZOfBadHits*sizeof(TH1F*));
      fZOfBadHits[0] = new TH1F("ZOfBadHits0","Total no.of rejected clusters", 100,-210,210);
      //      AddHist(fZOfBadHits[0]);
      fZOfGoodHits = new TH1F("ZOfGoodHits","Z of accepted clusters",100,-210,210);                        
      fPhiOfGoodHits = new TH1F("PhiOfGoodHits","Phi of accepted clusters",100, -TMath::Pi(), TMath::Pi());
      fPhiOfBadHits = new TH1F("PhiOfBadHits","Phi of rejected clusters",100, -TMath::Pi(), TMath::Pi());
      fTracklengthInTpcTotal = new TH1F("TracklengthInTpcTotal","Total track in TPC",100,0,200);         
      fTracklengthInTpc = new TH1F("TracklengthInTpc","Track length in TPC used for dE/dx",100,0,200);   
      fPadTbkAll = new TH2F("PadTbkAll","no. Pads versus no. Timebuckets for all hits",35,2.5,37.,18,0.5,18.5);
      fPadTbkBad = new TH2F("PadTbkBad","no. Pads versus no. Timebuckets for rejected hits",35,2.5,37.,18,0.5,18.5);
      const Char_t *FitName[3] = {"I70","F","N"};

      enum  {kTotalMethods = 6};
      for (Int_t k = 0; k < kTotalMethods/2; k++) {
	const Char_t *parN[5] = {"","pi","e","K","P"};
	const Char_t *parT[5] = {"All","|nSigmaPion| < 1","|nSigmaElectron| < 1","|nSigmaKaon| < 1","|nSigmaProton| < 1"};
	Int_t ny = 500;
	Double_t ymin = 0, ymax = 2.5;
	if (k == 2) {ny = 600; ymin = 0.7; ymax = 3.7;}
	for (Int_t t = 0; t < 5; t++) {
	  TString Title(Form("log10(dE/dx(%s)(keV/cm)) versus log10(p(GeV/c)) for Tpc TrackLength > 40 cm %s",FitName[k],parT[t]));
	  if (k == 2) Title = Form("log10(dN/dx) versus log10(p(GeV/c)) for Tpc TrackLength > 40 cm %s",parT[t]);
	  fTdEdx[k][t] = new TH2F(Form("TdEdx%s%s",FitName[k],parN[t]),Title,
				  300,-1.5,1.5, ny, ymin, ymax);
	  fTdEdx[k][t]->SetMarkerStyle(1);
	  fTdEdx[k][t]->SetMarkerColor(t+1);
	}
	fqTdEdx[k] = new TH2F(Form("aTdEdx%s",FitName[k]),
			     Form("log10(dE/dx(%s)(keV/cm)) versus q*(1.5+log10(p(GeV/c))) for Tpc TrackLength > 40 cm",FitName[k]),
			     500, -2.5, 2.5, ny, ymin, ymax); 
      } 
    }
    if (! f && !first) {
      //      for (Int_t i = 0; i < fNZOfBadHits; i++) AddHist(fZOfBadHits[i]);           
      AddHist(fZOfGoodHits);
      AddHist(fPhiOfGoodHits);         
      AddHist(fPhiOfBadHits);         
      AddHist(fTracklengthInTpcTotal);
      AddHist(fTracklengthInTpc);     
      for (Int_t k = 0; k < 3; k++) {
	for (Int_t t = 0; t < 5; t++) {
	  AddHist(fTdEdx[k][t]);
	}
      }
    }
    first = 2004;
  }  else {
    StSPtrVecTrackPidTraits &traits = gTrack->pidTraits();
    static StDedxPidTraits *pid = 0;
    static Double_t TrackLength, I70, fitZ, fitN;
    StThreeVectorD g3 = gTrack->geometry()->momentum(); // p of global track
    Double_t pMomentum = g3.mag();
    Double_t qCharge = gTrack->geometry()->charge();
    Int_t k;
    for (UInt_t i = 0; i < traits.size(); i++) {
      if (! traits[i]) continue;
      if ( traits[i]->IsZombie()) continue;
      pid = dynamic_cast<StDedxPidTraits*>(traits[i]);
      if (pid) {
	if (pid->method() == kTruncatedMeanId) {
	  I70 = pid->mean(); 
	  TrackLength = pid->length(); 
	  if (TrackLength < 40) continue;
	  k = 0;
	  fTdEdx[k][0]->Fill(TMath::Log10(pMomentum), TMath::Log10(I70)+6.);
	  fqTdEdx[k]->Fill(qCharge*(1.5+TMath::Log10(pMomentum)), TMath::Log10(I70)+6.);
	  const StParticleDefinition* pd = gTrack->pidTraits(PidAlgorithm70);
	  if (pd) {
	    if (TMath::Abs(PidAlgorithm70.numberOfSigma(Pion))     < 1) fTdEdx[k][1]->Fill(TMath::Log10(pMomentum), TMath::Log10(I70)+6.);
	    if (TMath::Abs(PidAlgorithm70.numberOfSigma(Electron)) < 1) fTdEdx[k][2]->Fill(TMath::Log10(pMomentum), TMath::Log10(I70)+6.);
	    if (TMath::Abs(PidAlgorithm70.numberOfSigma(Kaon))     < 1) fTdEdx[k][3]->Fill(TMath::Log10(pMomentum), TMath::Log10(I70)+6.);
	    if (TMath::Abs(PidAlgorithm70.numberOfSigma(Proton))   < 1) fTdEdx[k][4]->Fill(TMath::Log10(pMomentum), TMath::Log10(I70)+6.);
	  }
	}
	if (pid->method() == kLikelihoodFitId) {
	  fitZ = TMath::Log(pid->mean()+3e-33); 
	  TrackLength = pid->length(); 
	  if (TrackLength < 40) continue;
	  k = 1;
	  fTdEdx[k][0]->Fill(TMath::Log10(pMomentum), Log10E*fitZ + 6.);
	  fqTdEdx[k]->Fill(qCharge*(1.5+TMath::Log10(pMomentum)), Log10E*fitZ + 6.);
	  const StParticleDefinition* pd = gTrack->pidTraits(PidAlgorithmFitZ);
	  if (pd) {
	    if (TMath::Abs(PidAlgorithmFitZ.numberOfSigma(Pion))     < 1) fTdEdx[k][1]->Fill(TMath::Log10(pMomentum), Log10E*fitZ + 6.);
	    if (TMath::Abs(PidAlgorithmFitZ.numberOfSigma(Electron)) < 1) fTdEdx[k][2]->Fill(TMath::Log10(pMomentum), Log10E*fitZ + 6.);
	    if (TMath::Abs(PidAlgorithmFitZ.numberOfSigma(Kaon))     < 1) fTdEdx[k][3]->Fill(TMath::Log10(pMomentum), Log10E*fitZ + 6.);
	    if (TMath::Abs(PidAlgorithmFitZ.numberOfSigma(Proton))   < 1) fTdEdx[k][4]->Fill(TMath::Log10(pMomentum), Log10E*fitZ + 6.);
	  }
	}
	if (pid->method() == kOtherMethodId) {
	  fitN = pid->mean(); 
	  TrackLength = pid->length(); 
	  if (TrackLength < 40) continue;
	  k = 2;
	  fTdEdx[k][0]->Fill(TMath::Log10(pMomentum),TMath::Log10(fitN));
	  fqTdEdx[k]->Fill(qCharge*(1.5+TMath::Log10(pMomentum)),TMath::Log10(fitN));
	  const StParticleDefinition* pd = gTrack->pidTraits(PidAlgorithmFitN);
	  if (pd) {
	    if (TMath::Abs(PidAlgorithmFitN.numberOfSigma(Pion))     < 1) fTdEdx[k][1]->Fill(TMath::Log10(pMomentum),TMath::Log10(fitN));
	    if (TMath::Abs(PidAlgorithmFitN.numberOfSigma(Electron)) < 1) fTdEdx[k][2]->Fill(TMath::Log10(pMomentum),TMath::Log10(fitN));
	    if (TMath::Abs(PidAlgorithmFitN.numberOfSigma(Kaon))     < 1) fTdEdx[k][3]->Fill(TMath::Log10(pMomentum),TMath::Log10(fitN));
	    if (TMath::Abs(PidAlgorithmFitN.numberOfSigma(Proton))   < 1) fTdEdx[k][4]->Fill(TMath::Log10(pMomentum),TMath::Log10(fitN));
	  }
	}
      }
    }
  }
}
//________________________________________________________________________________
void StdEdxY2Maker::BadHit(Int_t iFlag, const StThreeVectorF &xyz) {
#if 0
  static Int_t ibreak = 0;
  if (iFlag == 6 && TMath::Abs(xyz.z()) < 40) {
    ibreak++;
  }
#endif
  static const Char_t *BadCaseses[11] = 
    {"Total no.of rejected clusters",   // 0
     "it is not used in track fit",     // 1
     "it is flagged ",                  // 2
     "pad does mathc with helix prediction",            // 3
     "dx is out interval [0.5,25]",     // 4
     "Sector/Row gain < 0",             // 6
     "drift distance < min || drift distance > max", // 7
     "dE < 0 or dx < 0",                // 8
     "Edge effect",                     // 9
     "Anode Voltage problem"            // 10
    };
  if (fZOfBadHits[0]) {
    fZOfBadHits[0]->Fill(xyz.z());
    if (fPhiOfBadHits!= 0) fPhiOfBadHits->Fill(TMath::ATan2(xyz.y(),xyz.x()));
    if (! fZOfBadHits[iFlag+1]) {
      fZOfBadHits[0]->GetDirectory()->cd();
      fZOfBadHits[iFlag+1] = new TH1F(*fZOfBadHits[0]);
      fZOfBadHits[iFlag+1]->Reset();
      fZOfBadHits[iFlag+1]->SetName(Form("ZOfBadHits_%i",iFlag+1));
      if (iFlag < 11) {
	fZOfBadHits[iFlag+1]->SetTitle(BadCaseses[iFlag+1]);
      } else {
	assert(m_TpcdEdxCorrection);
	fZOfBadHits[iFlag+1]->SetTitle(m_TpcdEdxCorrection->CorrectionStatus(iFlag-10).Title);
      }
      //      AddHist(fZOfBadHits[iFlag+1]);
    }
    fZOfBadHits[iFlag+1]->Fill(xyz.z());
  }
}
//________________________________________________________________________________
Int_t StdEdxY2Maker::Propagate(const StThreeVectorD &middle,const StThreeVectorD &normal,
			       const StPhysicalHelixD &helixI, const StPhysicalHelixD &helixO,
			       StThreeVectorD &xyz, StThreeVectorD &dirG, Double_t s[2], Double_t w[2]) {
  xyz  = StThreeVectorD();
  dirG = StThreeVectorD();
  s[0] = helixI.pathLength(middle, normal);
  s[1] = helixO.pathLength(middle, normal);
  w[0] = w[1] = 0;
  Double_t sA[2] = {0};
  if (s[0] > 1e6 && s[1] > 1e6) {
    return 1;
  } else if (s[0] <= 1e6 && s[1] < 1e6) {
    sA[0] = s[0]*s[0];
    sA[1] = s[1]*s[1];
    Double_t sN = sA[0] + sA[1];
    w[0] = sA[0]/sN;
    w[1] = sA[1]/sN;
  } else if (s[0] <= 1e6) {
    w[1] = 1.;
  } else {
    w[0] = 1.;
  }
  if (w[0] > 1.e-4) {xyz += w[0]*helixO.at(s[1]); dirG += w[0]*helixO.momentumAt(s[1],bField);}
  if (w[1] > 1.e-4) {xyz += w[1]*helixI.at(s[0]); dirG += w[1]*helixI.momentumAt(s[0],bField);}
  return 0;
}
//________________________________________________________________________________
void StdEdxY2Maker::fcnN(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag) {
  static Int_t _debug = 0; 
#ifdef __DEBUG_dNdx__
  static TCanvas *c1 = 0;
  static vector<Double_t> X;
  static vector<Double_t> F;
  static vector<Double_t> E;
  static vector<Double_t> P;
  //  static vector<Double_t> I;
  if (_debug > 0 && iflag == 1) {
    X.clear();
    F.clear();
    E.clear();
    P.clear();
    //    I.clear();
    //    if (!c1) c1 = new TCanvas("fcn","fcn",500,1500);
    if (!c1) c1 = new TCanvas("fcn","fcn",500,1000);
    else     c1->Clear();
    //    c1->Divide(1,3);
    c1->Divide(1,2);
  }
#endif /* __DEBUG_dNdx__ */
  f = 0;
  gin[0] = 0.;
  Double_t dNdx = par[0]; // Mu
  //  Double_t sigma = par[1]; // extra sigma
  for (Int_t i = 0; i < NdEdx; i++) {
    Double_t dE = FdEdx[i].F.dE;
    Double_t dX = FdEdx[i].dxC;
    Double_t Np = dNdx*dX;
    Double_t derivative = 0;
    Double_t Prob = StdEdxModel::instance()->ProbdEGeVlog(TMath::Log(dE),Np, &derivative);
#ifdef __DEBUG_dNdx__
    if (_debug && iflag == 3) {
      //      Double_t ee = dE + TMath::Log(1e9) -TMath::Log(Np); // to eV/Np
      Double_t ee = StdEdxModel::instance()->Logne(TMath::Log(dE))  -TMath::Log(Np); // to eV/Np
      E.push_back(ee);
      P.push_back(Prob);
      //      Double_t In = StdEdxModel::instance()->GGaus()->Integral(-1.,ee);
      //      I.push_back(In);
    }
#endif /* __DEBUG_dNdx__ */
    if (Prob <= 0.0) {
      f += 100;
      FdEdx[i].Prob = 0;
      continue;
    }
    f -= TMath::Log(Prob);
    // d(dNdx) = dNp/dx
    gin[0] -= derivative/dX/Prob;
    FdEdx[i].Prob = Prob;
  }
  if (_debug > 0) {
    if (_debug > 2) {
      cout << " dNdx = " << dNdx << "\tf = " << f << endl;
      PrintdEdx(1);
      cout << "===================" << endl;
    }
#ifdef __DEBUG_dNdx__
    X.push_back(dNdx);
    F.push_back(f);
    if (iflag == 3) {
      c1->cd(1);
      Int_t N = X.size();
      TArrayD XA(N);
      TArrayD YA(N);
      for (Int_t i = 0; i < N; i++) {
	XA[i] = X[i];
	YA[i] = F[i];
      }
      if (fdNdxGraph[0]) delete fdNdxGraph[0];
      fdNdxGraph[0] = new TGraph(N, XA.GetArray(), YA.GetArray());
      fdNdxGraph[0]->SetTitle("fcn");
      fdNdxGraph[0]->Draw("axp");
      TArrayD EA(NdEdx);
      TArrayD PA(NdEdx);
      //      TArrayD IA(NdEdx);
      for (Int_t i = 0; i < NdEdx; i++) {
	EA[i] = E[i];
	PA[i] = P[i];
	//	IA[i] = I[i];
      }
      if (fdNdxGraph[1]) delete fdNdxGraph[1];
      fdNdxGraph[1] = new TGraph(NdEdx, EA.GetArray(), PA.GetArray());
      fdNdxGraph[1]->SetTitle("Prob");
      c1->cd(2);
      fdNdxGraph[1]->Draw("axp");
      if (fdNdxGraph[2]) delete fdNdxGraph[2];
//       fdNdxGraph[2] = new TGraph(N, EA.GetArray(), IA.GetArray());
//       fdNdxGraph[2]->SetTitle("Integral");
//       c1->cd(3);
//       fdNdxGraph[2]->Draw("axp");
      c1->Update();
    }
#endif /* __DEBUG_dNdx__ */
  }
}
//________________________________________________________________________________
void StdEdxY2Maker::DoFitN(Double_t &chisq, Double_t &fitZ, Double_t &fitdZ){
  Double_t dNdx = 1e9*TMath::Exp(fitZ)/92.24/1.1; // 0.080; // 80 eV per primary interaction
  //  Double_t dNdx = 1e9*TMath::Exp(fitZ)/80.; // 0.080; // 80 eV per primary interaction
  Int_t ierflg = 0;
  m_Minuit->SetFCN(fcnN);
  Double_t arglist[10] = {0};
  if (Debug() < 2) {
    arglist[0] = -1;
  }
  m_Minuit->mnexcm("set print",arglist, 1, ierflg);
  arglist[0] = 0.0;
  m_Minuit->mnexcm("set NOW",arglist, 0, ierflg);
  m_Minuit->mnexcm("CLEAR",arglist, 0, ierflg);
  arglist[0] = 0.5;
  m_Minuit->mnexcm("SET ERR", arglist ,1,ierflg);
  //    m_Minuit->mnparm(0, "LogdNdx", TMath::Log(dNdx), 0.5, 0.,0.,ierflg); //First Guess
  m_Minuit->DefineParameter(0, "dNdx", dNdx, 0.5, 0.2*dNdx, 5*dNdx);
  //  m_Minuit->DefineParameter(1, "sigma", 0.01, 0.01, 0.0, 0.5);
  arglist[0] = 1.0;
  m_Minuit->mnexcm("CALLfcn", arglist ,1,ierflg);
  if (Debug() < 4)       arglist[0] = 1.;   // 1.
  else                   arglist[0] = 0.;   // Check gradient 
  m_Minuit->mnexcm("SET GRAD",arglist,1,ierflg);
  arglist[0] = 500;
  arglist[1] = 1.;
  //    m_Minuit->mnexcm("MIGRAD", arglist ,2,ierflg);
  m_Minuit->mnexcm("MINIMIZE", arglist ,2,ierflg);
  m_Minuit->mnexcm("HESSE  ",arglist,0,ierflg);
  arglist[0] = 3.0;
  m_Minuit->mnexcm("CALLfcn", arglist ,1,ierflg);
  Double_t edm,errdef;
  Int_t nvpar,nparx,icstat;
  m_Minuit->mnstat(chisq,edm,errdef,nvpar,nparx,icstat);
  m_Minuit->GetParameter(0, fitZ, fitdZ);
}
//________________________________________________________________________________
void StdEdxY2Maker::IntegrateAdc(const StTpcHitCollection* TpcHitCollection) {
  if (! fIntegratedAdc) fIntegratedAdc = new TH2F("AdcI","Integrated Adc for timebuckets foreach High Anode Vltage socket",451,-0.5,450.5,192,0.5,192.5);
  else                  fIntegratedAdc->Reset();
  
  if (! TpcHitCollection) { cout << "No TPC Hit Collection" << endl; return;}
  UInt_t numberOfSectors = TpcHitCollection->numberOfSectors();
  for (UInt_t i = 0; i< numberOfSectors; i++) {
    const StTpcSectorHitCollection* sectorCollection = TpcHitCollection->sector(i);
    if (sectorCollection) {
      Int_t sector = i;
      Int_t numberOfPadrows = sectorCollection->numberOfPadrows();
      for (int j = 0; j< numberOfPadrows; j++) {
	const StTpcPadrowHitCollection *rowCollection = sectorCollection->padrow(j);
	if (rowCollection) {
	  Int_t row = j + 1;
	  Int_t channel = St_TpcAvgPowerSupplyC::instance()->ChannelFromRow(sector,row); 
	  Double_t sc = NumberOfChannels*(sector-1) + channel;
	  const StSPtrVecTpcHit &hits = rowCollection->hits();
	  Long64_t NoHits = hits.size();
	  for (Long64_t k = 0; k < NoHits; k++) {
	    const StTpcHit *tpcHit = static_cast<const StTpcHit *> (hits[k]);
	    if (! tpcHit) continue;
	    fIntegratedAdc->Fill(tpcHit->timeBucket(), sc, tpcHit->adc());
	  }
	}
      }
    }
  }
  if (fIntegratedAdc->GetEntries()) {
    Int_t ny = fIntegratedAdc->GetNbinsY();
    Int_t nx = fIntegratedAdc->GetNbinsX();
    for (Int_t iy = 1; iy <= ny; iy++) {
      Double_t sum = 0;
      for (Int_t ix = 1; ix <= nx; ix++) {
	sum += fIntegratedAdc->GetBinContent(ix,iy);
	if (sum > 0) fIntegratedAdc->SetBinContent(ix,iy, sum);
      }
    }
  }
}
//________________________________________________________________________________ 
Double_t StdEdxY2Maker::IntegratedAdc(const StTpcHit* tpcHit) {
  if (! fIntegratedAdc) return 0;
  Int_t sector = tpcHit->sector();
  Int_t row    = tpcHit->padrow();
  Int_t channel = St_TpcAvgPowerSupplyC::instance()->ChannelFromRow(sector,row);
  Double_t sc = NumberOfChannels*(sector-1) + channel;
  return fIntegratedAdc->Interpolate(tpcHit->timeBucket(), sc);
}
