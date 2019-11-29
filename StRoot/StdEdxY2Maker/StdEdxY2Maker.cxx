// $Id: StdEdxY2Maker.cxx,v 1.99 2019/11/29 19:00:08 fisyak Exp $
//#define CompareWithToF 
//#define __USEZ3A__
//#define __CHECK_LargedEdx__
#define __NEGATIVE_ONLY__
//#define __TEST_DX__
//#define __ZDC3__
//#define __LogProb__
#define __BEST_VERTEX__
//#define __iTPCOnly__
#include <Stiostream.h>		 
#include "StdEdxY2Maker.h"
#include "StTpcdEdxCorrection.h" 
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
// StUtilities
#include "StMagF.h"
#include "StDbUtilities/StMagUtilities.h"
#include "StMessMgr.h" 
#include "StBichsel/Bichsel.h"
#include "StBichsel/StdEdxModel.h"
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
#include "StPidStatus.h"
#include "dEdxHist.h"
#ifdef  __CHECK_LargedEdx__
#include "tables/St_g2t_track_Table.h" 
#endif
const static Int_t tZero= 19950101;
const static Int_t tMin = 20000101;
const static Int_t tMax = 20200101;
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

//const static Double_t pMomin = 0.35; // range for dE/dx calibration
//const static Double_t pMomax = 0.75;
const  Double_t pMoMIP = 0.526; // MIP from Heed bg = 3.77 => p_pion = 0.526
const  Double_t pMomin = pMoMIP - 0.05; // 0.45;
const  Double_t pMomax = pMoMIP + 0.05; // 0.55;

Double_t StdEdxY2Maker::bField = 0;
Bool_t   StdEdxY2Maker::fUsedNdx = kFALSE;
//______________________________________________________________________________
// QA histograms
const static Int_t  fNZOfBadHits = 11;
static TH1F **fZOfBadHits = 0;
static TH1F *fZOfGoodHits = 0;
static TH1F *fPhiOfBadHits = 0;
static TH1F *fTracklengthInTpcTotal = 0;
static TH1F *fTracklengthInTpc = 0;
#ifdef __USEZ3A__
static TH3F *Z3A = 0;
#endif
//______________________________________________________________________________
ClassImp(StdEdxY2Maker);
//_____________________________________________________________________________
StdEdxY2Maker::StdEdxY2Maker(const char *name): StMaker(name), m_Mask(-1) {
  memset (beg, 0, end-beg);
  SETBIT(m_Mode,kPadSelection); 
  m_Minuit = new TMinuit(2);
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
  static Int_t DoOnce = 0;
  if (!gStTpcDb) {
    cout << "Database Missing! Can't initialize StdEdxY2Maker" << endl;
    return kStFatal;
  }
  // 		TPG parameters
  numberOfSectors   = gStTpcDb->Dimensions()->numberOfSectors();
  numberOfTimeBins  = gStTpcDb->Electronics()->numberOfTimeBins();

  if (! DoOnce) {
    DoOnce = 1;
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
	if (TESTBIT(m_Mode, kV0CrossCheck))	V0CrossCheck();
	if (TESTBIT(m_Mode, kXYZcheck))         XyzCheck();
      }
    }
    QAPlots(0);
  }
  SafeDelete(m_TpcdEdxCorrection);
  m_TpcdEdxCorrection = new StTpcdEdxCorrection(m_Mask, Debug());
  if (GetDate() > 20181201) fUsedNdx = kTRUE; // use dN/dx for year > 2018
  return kStOK;
}
//________________________________________________________________________________
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
#ifdef __USEZ3A__
  if (Z3A) {// control Z slope
    const Char_t *IO[2] = {"Inner", "Outer"};
    Double_t    xmin[2] = { 40, 40};
    Double_t    xmax[2] = {200,180};
    TF1 *gg = Gaus2(); 
    for (Int_t io = 1; io <= 2; io++) {
      Z3A->GetXaxis()->SetRange(io,io);
      TH2 *I = (TH2 *) Z3A->Project3D(Form("zy%i",io));
      if (I) {
	I->FitSlicesY(gg);
	TH1D *proj = (TH1D*) gDirectory->Get(Form("%s_1",I->GetName()));
	if (proj) {
	  proj->Fit("pol1","erq","goff",xmin[io-1],xmax[io-1]);
	  TF1 *f = (TF1 *) proj->GetListOfFunctions()->FindObject("pol1");
	  if (f) {
	    LOG_INFO << "StdEdxY2Maker: Estimated content of O2 (ppm) from slope in drift distance for " << 
	      Form("%s = %10.2f +/- %10.2f", IO[io-1], -f->GetParameter(1)/slope, f->GetParError(1)/slope) << endm;
	  }
	}
      }
    }
  }
#endif
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
  static Bool_t ForcedX = IAttr("ForcedX");
#ifdef __TEST_DX__
  Double_t dX_GenFit = 0;
  TH3F *dXTest = 0;
  if (! dXTest) {
    TFile  *f = GetTFile();
    assert(f);
    f->cd();
    dXTest = new TH3F("dxTest","dX = dX_GenFit - dX_Propagete versus setor and dX_GenFit",24,0.5,24.5,100,-1.,9.,100,-0.1,0.1);
  }
#endif /* __TEST_DX__ */
  tpcTime = GetDateTime().Convert() - timeOffSet;
  static  StTpcLocalSectorCoordinate        localSect[4];
  static  StTpcPadCoordinate                PadOfTrack, Pad;
  static  StTpcLocalSectorDirection         localDirectionOfTrack;
  static  StThreeVectorD xyz[4];
  static  StThreeVectorD dirG;
  static  Double_t s[2], s_in[2], s_out[2], w[2], w_in[2], w_out[2], dx;
  enum {kNdEdxMax  = 300};
  static dEdxY2_t CdEdxT[3*kNdEdxMax];//,FdEdxT[kNdEdxMax],dEdxST[kNdEdxMax];
  static Int_t sectorMin = 1, sectorMax = 24;
  CdEdx = CdEdxT; 
  FdEdx = CdEdxT + kNdEdxMax; 
  dEdxS = CdEdxT + 2*kNdEdxMax; 
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
#ifdef __BEST_VERTEX__
  const StBTofCollection* tof = pEvent->btofCollection();
  StPrimaryVertex *pVbest  = 0;
  if (tof) {
    Double_t VpdZ = -300;
    if (tof->tofHeader()) VpdZ = tof->tofHeader()->vpdVz();
    if (TMath::Abs(VpdZ) < 200) {
      Double_t dZbest = 999;
      UInt_t NoPV = pEvent->numberOfPrimaryVertices();
      for (UInt_t ipr = 0; ipr < NoPV; ipr++) {
	StPrimaryVertex *pVertex = pEvent->primaryVertex(ipr);
	if (! pVertex) continue;
	Double_t zTPC = pVertex->position().z();
	Double_t dZ = TMath::Abs(zTPC-VpdZ);
	if (dZ < dZbest) {
	  dZbest = dZ;
	  pVbest = pVertex;
	}
      }
      if (dZbest > 3.0) pVbest = 0;
    }
  }
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
    StPhysicalHelixD helixO = gTrack->outerGeometry()->helix();
    StPhysicalHelixD helixI = gTrack->geometry()->helix();
    if (Debug() > 1) {
      cout << "Track:" << i 
	   << "\ttype " << gTrack->type()
	   << "\tvertex " << gTrack->vertex()
	   << "\tkey " << gTrack->key()
	   << "\tflag " << gTrack->flag()
	   << "\tencodedMethod " << gTrack->encodedMethod()
	   << "\timpactParameter " << gTrack->impactParameter()
	   << "\tlength " << gTrack->length()
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
	if (! St_tpcAnodeHVavgC::instance()->livePadrow(sector,row)) continue;
	xyz[3] = StThreeVectorD(tpcHit->position().x(),tpcHit->position().y(),tpcHit->position().z());
	//________________________________________________________________________________      
	dx = tpcHit->dX();
	if (ForcedX) dx = 0;
#ifdef __TEST_DX__
	dX_GenFit = dx;
	dx = 0;
#endif /* __TEST_DX__ */
	static StGlobalDirection  globalDirectionOfTrack;
	Int_t iokCheck = 0;
	if (dx <= 0.0) {
	  StThreeVectorD middle = xyz[3];
	  StThreeVectorD upper(tpcHit->positionU().x(),tpcHit->positionU().y(),tpcHit->positionU().z());
	  StThreeVectorD lower(tpcHit->positionL().x(),tpcHit->positionL().y(),tpcHit->positionL().z());
	  StThreeVectorD dif = upper - lower;
	  StThreeVectorD normal = dif.unit();
#if 0
	  if (St_tpcPadConfigC::instance()->numberOfRows(sector) == 45) {// ! iTpx
	    // Check that Voltage above "-100V" from nominal, mark as unrecoverable
	    Double_t V = St_tpcAnodeHVavgC::instance()->voltagePadrow(sector,row);
	    if ((row <= St_tpcPadConfigC::instance()->innerPadRows(sector) && 1170 - V > 100) || 
		(row >  St_tpcPadConfigC::instance()->innerPadRows(sector) && 1390 - V > 100)) {BadHit(9,tpcHit->position()); continue;}
	  }
#endif
	  // check that helix prediction is consistent with measurement
	  if (Propagate(middle,normal,helixI,helixO,xyz[0],dirG,s,w)) {BadHit(2,tpcHit->position()); continue;}
	  if (Debug() > 1) {
	    cout << " Prediction:\t" << xyz[0] 
		 << "\tat s=\t" << s[0] << "/" << s[1] 
		 << "\tw = " << w[0] << "/" << w[1] << endl;
	  }
	  dif = xyz[3] - xyz[0];
	  if (dif.perp() > 2.0) {if (Debug() > 1) {cout << "Prediction is to far from hit:\t" << xyz[3] << endl;}
	    continue;
	  }
	  if (Propagate(upper,normal,helixI,helixO,xyz[1],dirG,s_out,w_out)) {BadHit(2,tpcHit->position()); continue;}
	  if (Propagate(lower,normal,helixI,helixO,xyz[2],dirG,s_in ,w_in )) {BadHit(2,tpcHit->position()); continue;}
	  dx = ((s_out[0] - s_in[0])*w[1] + (s_out[1] - s_in[1])*w[0]);
#ifdef __TEST_DX__
	  dXTest->Fill(sector, dX_GenFit, dX_GenFit - dx);
#endif /* __TEST_DX__ */
	  if (dx <= 0.0) {if (Debug() > 1) {cout << "negative dx " << dx << endl;}
	    continue;
	  }
	  globalDirectionOfTrack = StGlobalDirection(dirG);
	  for (Int_t l = 0; l < 4; l++) {
	    StGlobalCoordinate globalOfTrack(xyz[l].x(),xyz[l].y(),xyz[l].z());
	    transform(globalOfTrack,localSect[l],sector,row);
	  }
#ifdef __PROMPT_HITS__
	  Double_t zP = TMath::Abs(xyz[0].z());
	  //----------------------------- Prompt Hits ? ------------------------------
	  if (zP > 205.0 && zP < 215.) {
	    Int_t iWestEast = 0;
	    if (sector > 12) iWestEast = 1;
	    Int_t io = 0;
	    if (row > St_tpcPadConfigC::instance()->innerPadRows(sector)) io = 1;
	    static Double_t z[2][3] = { 
	      // Anodes         GG          Pads
	      { -0.6 - 0.2,     0,  -0.6 - 2*0.2}, // Inner
	      { -0.6 - 0.4,     0,  -0.6 - 2*0.4}  // Outer
	    };
	    StTpcLocalSectorDirection  dirLS(0.,0.,(iWestEast) ? 1 : -1,sector,row);  if (Debug()>1) cout << "dirLS\t" << dirLS << endl;
	    StGlobalDirection directionG;
	    transform(dirLS,directionG);
	    const StThreeVectorD PromptNormal(directionG.position());
	    StTpcLocalSectorCoordinate local;
	    StThreeVectorD  anode, gg, pads;
	    StThreeVectorD* PromptPlanes[3] = {&anode, &gg, &pads};
	    StGlobalCoordinate glob;
	    Double_t y = transform.yFromRow(row);
	    for (Int_t l = 0; l < 3; l++) {
	      local = StTpcLocalSectorCoordinate(0.,y, z[io][l], sector, row);
	      transform(local,glob);
	      *PromptPlanes[l] = glob.position();
	      if (Debug()>1) cout << "mPromptPosition[" << sector-1 << "][" << row-1 << "][" << l << "] = " 
				  << *PromptPlanes[l]  << endl;
	    }
	    // check that helix prediction is consistent with measurement
	    if (Propagate(*((const StThreeVectorD *) &anode),PromptNormal,helixI,helixO,xyz[0],dirG,s,w)) {BadHit(2,tpcHit->position()); continue;}
	    if (Debug() > 1) {
	      cout << " Prediction:\t" << xyz[0] 
		   << "\tat s=\t" << s[0] << "/" << s[1] 
		   << "\tw = " << w[0] << "/" << w[1] << endl;
	    }
	    dif = xyz[3] - xyz[0];
	    if (dif.perp() > 2.0) {if (Debug() > 1) {cout << "Prediction is to far from hit:\t" << xyz[3] << endl;}
	      continue;
	    }
	    static Double_t s_inP[2], s_outP[2];
	    if (Propagate(*((const StThreeVectorD *) &pads),PromptNormal,helixI,helixO,xyz[1],dirG,s_outP,w_out)) {BadHit(2,tpcHit->position()); continue;}
	    if (Propagate(*((const StThreeVectorD *) &gg  ),PromptNormal,helixI,helixO,xyz[2],dirG,s_inP ,w_in )) {BadHit(2,tpcHit->position()); continue;}
	    s_out[0] = TMath::Min(s_outP[0], s_out[0]);
	    s_out[1] = TMath::Min(s_outP[1], s_out[1]);
	    s_in[0]  = TMath::Max(s_inP[0] , s_in[0] );
	    s_in[1]  = TMath::Max(s_inP[1] , s_in[1] );
	    dx = ((s_out[0] - s_in[0])*w[1] + (s_out[1] - s_in[1])*w[0]);
#ifdef __TEST_DX__
	    dXTest->Fill(sector, dX_GenFit, dX_GenFit - dx);
#endif /* __TEST_DX__ */
	    if (dx <= 0.0) {if (Debug() > 1) {cout << "negative dx " << dx << endl;}
	      continue;
	    }
	    globalDirectionOfTrack = StGlobalDirection(dirG);
	    for (Int_t l = 0; l < 4; l++) {
	      StGlobalCoordinate globalOfTrack(xyz[l].x(),xyz[l].y(),xyz[l].z());
	      transform(globalOfTrack,localSect[l],sector,row);
	    }
	  }
#endif /* __PROMPT_HITS__ */
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
	  Double_t pad = tpcHit->pad();
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
	  
	} else {
	  // use cluster position for precalculated dx
	  transform(xyz[3],localSect[3],sector,row);
	  transform(localSect[3],Pad);
	} // end of dx calculation
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
	Float_t Npads = St_tpcPadConfigC::instance()->numberOfPadsAtRow(sector,row);
	if (CdEdx[NdEdx].edge > 0.5*Npads) 	  CdEdx[NdEdx].edge -= 1 + Npads;
	CdEdx[NdEdx].xpad = 2*(CdEdx[NdEdx].pad - 0.5)/Npads - 1.0;
	CdEdx[NdEdx].yrow = sector + 0.5*((row <= St_tpcPadConfigC::instance()->innerPadRows(sector)) ? 
					  (row - St_tpcPadConfigC::instance()->innerPadRows(sector) - 0.5)/St_tpcPadConfigC::instance()->innerPadRows(sector) : 
					  (row - St_tpcPadConfigC::instance()->innerPadRows(sector) - 0.5)/(St_tpcPadConfigC::instance()->numberOfRows(sector) - St_tpcPadConfigC::instance()->innerPadRows(sector)));
	CdEdx[NdEdx].Npads  = tpcHit->padsInHit();
	CdEdx[NdEdx].Ntbins = tpcHit->pixelsInHit();
	//	CdEdx[NdEdx].dE     = tpcHit->chargeModified();
	CdEdx[NdEdx].F.dE     = tpcHit->charge();
	//	CdEdx[NdEdx].dCharge= tpcHit->chargeModified()/tpcHit->charge() - 1.;
	//	CdEdx[NdEdx].dCharge= tpcHit->chargeModified() - tpcHit->charge();
	CdEdx[NdEdx].dCharge = 0;
	Int_t p1 = tpcHit->minPad();
	Int_t p2 = tpcHit->maxPad();
	Int_t t1 = tpcHit->minTmbk();
	Int_t t2 = tpcHit->maxTmbk();
	CdEdx[NdEdx].npads = p2 - p1 + 1;
	CdEdx[NdEdx].ntmbks = t2 - t1 + 1;
	CdEdx[NdEdx].rCharge=  0.5*m_TpcdEdxCorrection->Adc2GeV()*TMath::Pi()/4.*(p2-p1+1)*(t2-t1+1);
	if (TESTBIT(m_Mode, kEmbeddingShortCut) && 
	    (tpcHit->idTruth() && tpcHit->qaTruth() > 95)) CdEdx[NdEdx].lSimulated = tpcHit->idTruth();
	CdEdx[NdEdx].F.dx   = dx;
	CdEdx[NdEdx].dxC    = dx;
	CdEdx[NdEdx].xyz[0] = localSect[3].position().x();
	CdEdx[NdEdx].xyz[1] = localSect[3].position().y();
	CdEdx[NdEdx].xyz[2] = localSect[3].position().z();
	Double_t maxPad = Npads/2;
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
	Double_t pT2 = CdEdx[NdEdx].xyzD[0]*CdEdx[NdEdx].xyzD[0]+CdEdx[NdEdx].xyzD[1]*CdEdx[NdEdx].xyzD[1];
	if (pT2 > 1e-14)
	  CdEdx[NdEdx].TanL = -CdEdx[NdEdx].xyzD[2]/TMath::Sqrt(pT2);
	CdEdx[NdEdx].tpcTime = tpcTime;
	if (St_trigDetSumsC::instance())	CdEdx[NdEdx].Zdc     = St_trigDetSumsC::instance()->zdcX();
	CdEdx[NdEdx].adc     = tpcHit->adc();
	Bool_t doIT = kTRUE;
	if (TESTBIT(m_Mode,kEmbedding)) doIT = kFALSE;
	Int_t iok = m_TpcdEdxCorrection->dEdxCorrection(CdEdx[NdEdx],doIT);
	if (iok) {BadHit(4+iok, tpcHit->position()); continue;} 
	if (fZOfGoodHits) fZOfGoodHits->Fill(tpcHit->position().z());
	if (NdEdx < kNdEdxMax) {
	  tpcHit->setChargeModified(CdEdx[NdEdx].F.dEdx);
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
#ifdef __iTPCOnly__
      Int_t noiTPC = 0;
      for (Int_t k = 0; k < NdEdx; k++) {
	if (St_tpcPadConfigC::instance()->iTPC(FdEdx[k].sector) &&
	    St_tpcPadConfigC::instance()->IsRowInner(FdEdx[k].sector, FdEdx[k].row))
	  noiTPC++;
      }
      Int_t iTPCOnly = (noiTPC >= 20) ? 1 : 0;
#endif /* __iTPCOnly__ */
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
	dedx.ndedx     =  N70 + 100*((int) TrackLength);
	dedx.dedx[0]   =  I70;
	dedx.dedx[1]   =  D70;
	dedx.dedx[2]   =  dXavLog2;
	if ((TESTBIT(m_Mode, kCalibration)))  // uncorrected dEdx
	  AddEdxTraits(tracks, dedx);
	if (! TESTBIT(m_Mode, kDoNotCorrectdEdx)) { 
#ifdef __iTPCOnly__
	  dedx.det_id    =  100*iTPCOnly + kTpcId;    // TPC track & iTPC
#else
	  dedx.det_id    = kTpcId;    // TPC track
#endif
	  m_TpcdEdxCorrection->dEdxTrackCorrection(0,dedx); 
	  dedx.det_id    = kTpcId;    // TPC track 
	  dedx.method    = kTruncatedMeanId;
	  AddEdxTraits(tracks, dedx);
	}
	// likelihood fit
	Double_t chisq, fitZ, fitdZ;
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
	  dedx.ndedx     =  NdEdx + 100*((int) TrackLength);
	  dedx.dedx[0]   =  TMath::Exp(fitZ);
	  dedx.dedx[1]   =  fitdZ; 
	  dedx.dedx[2]   =  dXavLog2;
	  if ((TESTBIT(m_Mode, kCalibration)))  // uncorrected dEdx
	    AddEdxTraits(tracks, dedx);
	  if (! TESTBIT(m_Mode, kDoNotCorrectdEdx)) { 
#ifdef __iTPCOnly__
	  dedx.det_id    =  100*iTPCOnly + kTpcId;    // TPC track & iTPC
#else
	  dedx.det_id    = kTpcId;    // TPC track
#endif
 	    m_TpcdEdxCorrection->dEdxTrackCorrection(2,dedx); 
	    dedx.det_id    = kTpcId;    // TPC track 
	    dedx.method    = kLikelihoodFitId;
	    AddEdxTraits(tracks, dedx);
	  }
	}
	if (fUsedNdx) {
	  // likelihood fit of no. of primary cluster per cm
	  Double_t chisqN, fitN, fitdN;
	  DoFitN(chisqN, fitN, fitdN);
	  if (chisqN > -900.0 &&chisqN < 10000.0) {
	    dedx.id_track  =  Id;
	    dedx.det_id    =  kTpcId;    // TPC track 
	    dedx.method    =  kOtherMethodId2;
	    dedx.ndedx     =  NdEdx + 100*((int) TrackLength);
	    dedx.dedx[0]   =  fitN;
	    dedx.dedx[1]   =  fitdN/fitN; 
	    dedx.dedx[2]   =  dXavLog2;
	    AddEdxTraits(tracks, dedx);
#ifdef __iTPCOnly__
	    dedx.det_id    =  100*iTPCOnly + kTpcId;    // TPC track & iTPC
#else
	    dedx.det_id    = kTpcId;    // TPC track
#endif
	    m_TpcdEdxCorrection->dEdxTrackCorrection(1,dedx); 
	    dedx.det_id    =  kTpcId;    // TPC track 
	    dedx.method    =  kOtherMethodId;
	    AddEdxTraits(tracks, dedx);
	  }
	}
#if 0
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
#endif
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
  if (TESTBIT(m_Mode, kCalibration) && TESTBIT(m_Mode,kV0CrossCheck) ) {
    V0CrossCheck();
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
  Hists3D::NtotHist = 2;
  Int_t NoRows = St_tpcPadConfigC::instance()->numberOfRows(20);
  static Hists3D Pressure("Pressure","log(dE/dx)","row","Log(Pressure)",-NoRows,150, 6.84, 6.99); // ? mix of inner and outer
  //  static Hists3D PressureT("PressureT","log(dE/dx)","row","Log(Pressure*298.2/inputGasTemperature)",-NoRows,150, 6.84, 6.99);
  
  static Hists3D Voltage("Voltage","log(dE/dx)","Sector*Channels","Voltage - Voltage_{nominal}", numberOfSectors*NumberOfChannels,22,-210,10);
  //  static Hists3D Volt("Volt","log(dE/dx)","Sector*Channels","Voltage", numberOfSectors*NumberOfChannels,410,990.,1400.);

  static Hists3D AvCurrent("AvCurrent","log(dEdx/Pion)","Sector*Channels","Average Current [#{mu}A]",numberOfSectors*NumberOfChannels,200,0.,1.0);
  static Hists3D Qcm("Qcm","log(dEdx/Pion)","Sector*Channels","Accumulated Charge [uC/cm]",numberOfSectors*NumberOfChannels,200,0.,1000);
  if (fUsedNdx) {
    Hists3D::NtotHist = 9;
  }
  static Hists3D SecRow3("SecRow3","<log(dEdx/Pion)>","sector","row",numberOfSectors,NoRows);
  Hists3D::NtotHist = 2;
  static Hists3D ADC3("ADC3","<logADC)>","sector","row",numberOfSectors,
		      NoRows,0,-1, 
		      100,0.,10.,
		      0,-1,1);
#if 0
  static Hists3D TanL3D("TanL3D","log(dEdx/Pion)","row","Tan(#lambda)",-NoRows,200,-2.,2.); // ? mix of inner and outer
#ifdef __iTPCOnly__
  static Hists3D TanL3DiTPC("TanL3DiTPC","log(dEdx/Pion)","row","Tan(#lambda)",-NoRows,200,-2.,2.); // ? mix of inner and outer
#endif
#endif
#ifdef __ZDC3__
  Hists3D::NtotHist = 2;
  static Hists3D Zdc3("Zdc3","<log(dEdx/Pion)>","row","log10(ZdcCoincidenceRate)",-NoRows,100,0.,10.);
#endif /* __ZDC3__ */
  Hists3D::NtotHist = 9;
  static Hists3D Z3("Z3","<log(dEdx/Pion)>","row","Drift Distance",-NoRows,220,-5,215);
  Hists3D::NtotHist = 2;
#ifdef __iTPCOnly__
  static Hists3D Z3iTPC("Z3iTPC","<log(dEdx/Pion)>","row","Drift Distance",-NoRows,220,-5,215);
#endif
#if 0
  //  static Hists3D Z3O("Z3O","<log(dEdx/Pion)>","row","(Drift)*ppmO2In",St_tpcPadConfigC::instance()->numberOfRows(sector),100,0,1e4);
  Hists3D::NtotHist = 2;
  static Hists3D Edge3("Edge3","log(dEdx/Pion)","sector*row"," Edge",numberOfSectors*NoRows, 201,-100.5,100.5);
#endif
  static Hists3D xyPad3("xyPad3","log(dEdx/Pion)","sector+yrow[-0.5,0.5] and xpad [-1,1]"," xpad",numberOfSectors*20, 32,-1,1, 200, -5., 5., 0.5, 24.5);
#if 0
  static Hists3D dX3("dX3","log(dEdx/Pion)","row"," dX(cm)",-NoRows, 100,0,10.);
#ifdef __iTPCOnly__
  static Hists3D dX3iTPC("dX3iTPC","log(dEdx/Pion)","row"," dX(cm)",-NoRows, 100,0,10.);
#endif
#endif
  static TH2F *ZdcCP = 0, *BBCP = 0;
  //  static TH2F *ctbWest = 0, *ctbEast = 0, *ctbTOFp = 0, *zdcWest = 0, *zdcEast = 0;
#if 0
  static TH2F *bbcYellowBkg = 0, *bbcBlueBkg = 0;
#endif
  static TH1F *hdEI = 0, *hdEUI = 0, *hdERI = 0, *hdEPI = 0, *hdETI = 0, *hdESI = 0, *hdEZI = 0, *hdEMI = 0;
  static TH1F *hdEO = 0, *hdEUO = 0, *hdERO = 0, *hdEPO = 0, *hdETO = 0, *hdESO = 0, *hdEZO = 0, *hdEMO = 0;
  enum  {kTotalMethods = 6};
  static TH3F *TPoints[2][kTotalMethods] = {0}; // *N[6] = {"B","70B","BU","70BU","N", "NU"}; 0 => "+", 1 => "-";
  static TH2F *Pulls[2][kTotalMethods] = {0};
#ifdef __iTPCOnly__
  static TH3F *TPointsiTPC[2][kTotalMethods] = {0}; // *N[6] = {"B","70B","BU","70BU","N", "NU"};
  static TH2F *PullsiTPC[2][kTotalMethods] = {0};
#endif
  static StDedxMethod kTPoints[kTotalMethods] = {// {"F","70","FU","70U","N", "NU"};
    kLikelihoodFitId,         // F
    kTruncatedMeanId,         // 70
    kWeightedTruncatedMeanId, // FU
    kEnsembleTruncatedMeanId  // 70U
    ,kOtherMethodId           // N
    ,kOtherMethodId2          // NU
  };
  static Hists2D I70("I70");
  static Hists2D fitZ("fitZ");
  static Hists2D *fitN = 0;
  if (fUsedNdx && ! fitN) fitN = new Hists2D("fitN");
  const static Int_t Nlog2dx = 80;
  const static Double_t log2dxLow = 0.0, log2dxHigh = 4.0;
#if 0
  static TH2F *inputTPCGasPressureP = 0, *nitrogenPressureP = 0, *gasPressureDiffP = 0, *inputGasTemperatureP = 0;
  static TH2F *flowRateArgon1P = 0, *flowRateArgon2P = 0;
  static TH2F *flowRateMethaneP = 0;
  static TH2F *percentMethaneInP = 0, *percentMethaneInPC = 0, *percentMethaneInPA = 0;
  static TH2F *ppmOxygenInP = 0, *flowRateExhaustP = 0;
  static TH2F *flowRateRecirculationP = 0; // *ppmOxygenOutP = 0,
  static TH2F *ppmWaterOutP = 0, *ppmWaterOutPC = 0, *ppmWaterOutPA = 0;
#endif
  // ProbabilityPlot
  //  static TH3F *Prob = 0;
  // end of ProbabilityPlot
#if 0
  static TH2F *BaddEdxZPhi70[2], *BaddEdxZPhiZ[2];
  static TH1F *BaddEdxMult70[2], *BaddEdxMultZ[2];
#endif
  static Int_t hMade = 0;
  static TH2F *Eta[2] = {0};     // 0 -> F, 1 -> 70
#ifdef __iTPCOnly__
  static TH2F *EtaiTPC[2] = {0};
#endif  
  if (! gTrack && !hMade) {
    TFile  *f = GetTFile();
    assert(f);
    f->cd();
    hMade=2004;
    // book histograms
    Bool_t fSetDefaultSumw2 = TH1::GetDefaultSumw2();
    TH1::SetDefaultSumw2(kFALSE);
    mHitsUsage  = new TH2F("HitsUsage","log10(No.of Used in dE/dx hits) versus log10(Total no. of Tpc Hits",
			   80,0,8,60,0,6);
    Int_t      nZBins = 200;
    Double_t ZdEdxMin = -5;
    Double_t ZdEdxMax =  5;
    ZdcCP  = new TH2F("ZdcCP","ZdcCoincidenceRate (log10)",100,0,10,nZBins,ZdEdxMin,ZdEdxMax);
    BBCP   = new TH2F("BBCP","BbcCoincidenceRate (log10)",60,0,6,nZBins,ZdEdxMin,ZdEdxMax);
#if 0
    bbcYellowBkg = new TH2F("bbcYellowBkg","(BBC Eastdelayed) and (BBC West) (log10)",
			    100,0,10,nZBins,ZdEdxMin,ZdEdxMax);
    bbcBlueBkg   = new TH2F("bbcBlueBkg","(BBC Westdelayed) and (BBC East) (log10)",
			    100,0,10,nZBins,ZdEdxMin,ZdEdxMax);
#endif
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
      if (! fUsedNdx && t > 4) continue;
      for (Int_t s = 0; s < 2; s++) {// charge 0 => "+", 1 => "-"
	TPoints[s][t]   = new TH3F(Form("TPoints%s%s",N[t],NS[s]),
				   Form("%s versus Length in Tpc and <log_{2}(dX)> in TPC - iTPC %s",T[t],TS[s]),
				   190,10,200., Nlog2dx, log2dxLow, log2dxHigh, 500,-1.,4.);
	Pulls[s][t] = new TH2F(Form("Pull%s%s",N[t],NS[s]),
			       Form("Pull %s versus Length in TPC - iTPC %s",T[t],TS[s]),
			       190,10.,200,nZBins,ZdEdxMin,ZdEdxMax);
#ifdef __iTPCOnly__
	TPointsiTPC[s][t]   = new TH3F(Form("TPoints%siTPC%s",N[t],NS[s]),
				       Form("%s versus Length in Tpc and <log_{2}(dX)> in iTPC %s",T[t],TS[s]),
				       190,10,200., Nlog2dx, log2dxLow, log2dxHigh, 500,-1.,4.);
	PullsiTPC[s][t] = new TH2F(Form("Pull%siTPC%s",N[t],NS[s]),
				   Form("Pull %s versus Length in iTPC %s",T[t],TS[s]),
				   190,10.,200,nZBins,ZdEdxMin,ZdEdxMax);
#endif
	if (s == 0 && t < 2) {
	  Eta[t] = new TH2F(Form("Eta%s",N[t]),
			    Form("%s for primary tracks versus Eta for |zPV| < 10cm and TpcLength > 40cm, TPC - iTPC",T[t]),
			    100,-2.5,2.5,500,-1.,4.);
#ifdef __iTPCOnly__
	  EtaiTPC[t] = new TH2F(Form("EtaiTPC%s",N[t]),
				Form("%s for primary tracks versus Eta for |zPV| < 10cm and TpcLength > 40cm, iTPC only",T[t]),
				100,-2.5,2.5,500,-1.,4.);
#endif
	}
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
#if 0
    TString title("log(dE/dx/Pion) vs inputTPCGasPressure (mbar)");
    inputTPCGasPressureP = new TH2F("inputTPCGasPressureP","log(dE/dx/Pion) vs inputTPCGasPressure (mbar)",100,1.0,3.0,nZBins,ZdEdxMin,ZdEdxMax);
    nitrogenPressureP = new TH2F("nitrogenPressureP","log(dE/dx/Pion) vs nitrogenPressure (mbar)",100,0.9,1.1,nZBins,ZdEdxMin,ZdEdxMax);
    gasPressureDiffP = new TH2F("gasPressureDiffP","log(dE/dx/Pion) vs gasPressureDiff (mbar)",100,0.6,1.,nZBins,ZdEdxMin,ZdEdxMax);
    inputGasTemperatureP = new TH2F("inputGasTemperatureP","log(dE/dx/Pion) vs inputGasTemperature (degrees K)",100,295.,300.,nZBins,ZdEdxMin,ZdEdxMax);
    flowRateArgon1P = new TH2F("flowRateArgon1P","log(dE/dx/Pion) vs flowRateArgon1 (liters/min)",100,14.95,15.0,nZBins,ZdEdxMin,ZdEdxMax);
    flowRateArgon2P = new TH2F("flowRateArgon2P","log(dE/dx/Pion) vs flowRateArgon2 (liters/min)",100,0.,0.25,nZBins,ZdEdxMin,ZdEdxMax);
    flowRateMethaneP = new TH2F("flowRateMethaneP","log(dE/dx/Pion) vs flowRateMethane (liters/min)",100,1.34,1.37,nZBins,ZdEdxMin,ZdEdxMax);
    percentMethaneInP = new TH2F("percentMethaneInP","log(dE/dx/Pion) vs percentMethaneIn (percent)",100,9.6,10.6,nZBins,ZdEdxMin,ZdEdxMax);
    percentMethaneInPC = new TH2F("percentMethaneInPC","log(dE/dx/Pion)(corrected) vs percentMethaneIn (percent)",100,9.6,10.6,nZBins,ZdEdxMin,ZdEdxMax);
    percentMethaneInPA = new TH2F("percentMethaneInPA","log(dE/dx/Pion)(just after correction) vs percentMethaneIn (percent)",
				  100,9.6,10.6,nZBins,ZdEdxMin,ZdEdxMax);
    ppmOxygenInP = new TH2F("ppmOxygenInP","log(dE/dx/Pion) vs ppmOxygenIn (ppm)",240,0.,60.,nZBins,ZdEdxMin,ZdEdxMax);
    flowRateExhaustP = new TH2F("flowRateExhaustP","log(dE/dx/Pion) vs flowRateExhaust (liters/min)",100,5.,20.,nZBins,ZdEdxMin,ZdEdxMax);
    ppmWaterOutP = new TH2F("ppmWaterOutP","log(dE/dx/Pion) vs ppmWaterOut (ppm)",100,0.,20.,nZBins,ZdEdxMin,ZdEdxMax);
    ppmWaterOutPC = new TH2F("ppmWaterOutPC","log(dE/dx/Pion) corrected vs ppmWaterOut (ppm)",100,0.,20.,nZBins,ZdEdxMin,ZdEdxMax);
    ppmWaterOutPA = new TH2F("ppmWaterOutPA","log(dE/dx/Pion) just after correction vs ppmWaterOut (ppm)",100,0.,20.,nZBins,ZdEdxMin,ZdEdxMax);
    //    ppmOxygenOutP = new TH2F("ppmOxygenOutP","log(dE/dx/Pion) vs ppmOxygenOut (ppm)",100,0,20,nZBins,ZdEdxMin,ZdEdxMax);
    flowRateRecirculationP = new TH2F("flowRateRecirculationP","log(dE/dx/Pion) vs flowRateRecirculation (liters/min)",
				      100,515.,545.,nZBins,ZdEdxMin,ZdEdxMax);
#endif
    hdEI  = new TH1F("hdEI","log10(dE) Inner after calibration",100,-8.,-3.);
    hdEUI = new TH1F("hdEUI","log10(dEU) Inner before correction",100,-8.,-3.);
    hdERI = new TH1F("hdERI","log10(dER) Inner after row correction  correction",100,-8.,-3.);
    hdEPI = new TH1F("hdEPI","log10(dEP) Inner after Pressure correction",100,-8.,-3.);
    hdETI = new TH1F("hdETI","log10(dET) Inner after TimeScale",100,-8.,-3.);
    hdESI = new TH1F("hdESI","log10(dES) Inner after after TimeScale + SecRow corrections",100,-8.,-3.);
    hdEZI = new TH1F("hdEZI","log10(dEZ) Inner after TimeScale + SecRow + Sec Z corrections ",100,-8.,-3.);
    hdEMI = new TH1F("hdEMI","log10(dEM) Inner after TimeScale + SecRow + Sec Z + Multiplicity corrections",100,-8.,-3.);
    hdEO  = new TH1F("hdEO","log10(dE) Outer after calibration",100,-8.,-3.);
    hdEUO = new TH1F("hdEUO","log10(dEU) Outer before correction",100,-8.,-3.);
    hdERO = new TH1F("hdERO","log10(dER) Outer after row correction  correction",100,-8.,-3.);
    hdEPO = new TH1F("hdEPO","log10(dEP) Outer after Pressure correction",100,-8.,-3.);
    hdETO = new TH1F("hdETO","log10(dET) Outer after TimeScale",100,-8.,-3.);
    hdESO = new TH1F("hdESO","log10(dES) Outer after after TimeScale + SecRow corrections",100,-8.,-3.);
    hdEZO = new TH1F("hdEZO","log10(dEZ) Outer after TimeScale + SecRow + Sec Z corrections ",100,-8.,-3.);
    hdEMO = new TH1F("hdEMO","log10(dEM) Outer after TimeScale + SecRow + Sec Z + Multiplicity corrections",100,-8.,-3.);
#if 0
    BaddEdxZPhi70[0] = new TH2F("BaddEdxZPhi700","Z and Phi for I70 below any limits by 5 s.d.",210,-210,210,360,-180.,180.);
    BaddEdxZPhi70[1] = new TH2F("BaddEdxZPhi701","Z and Phi for I70 above any limits by 5 s.d.",210,-210,210,360,-180.,180.);
    BaddEdxMult70[0] = new TH1F("BaddEdxMult700","Multiplicity (log10) for I70 below any limits by 5 s.d.",100,0.,10.);
    BaddEdxMult70[1] = new TH1F("BaddEdxMult701","Multiplicity (log10) for I70 above any limits by 5 s.d.",100,0.,10.);
    BaddEdxZPhiZ[0] = new TH2F("BaddEdxZPhiZ0","Z and Phi for Ifit below any limits by 5 s.d.",210,-210,210,360,-180.,180.);
    BaddEdxZPhiZ[1] = new TH2F("BaddEdxZPhiZ1","Z and Phi for Ifit above any limits by 5 s.d.",210,-210,210,360,-180.,180.);
    BaddEdxMultZ[0] = new TH1F("BaddEdxMultZ0","Multiplicity (log10) for Ifit below any limits by 5 s.d.",100,0.,10.);
    BaddEdxMultZ[1] = new TH1F("BaddEdxMultZ1","Multiplicity (log10) for Ifit above any limits by 5 s.d.",100,0.,10.);
#endif
    TH1::SetDefaultSumw2(fSetDefaultSumw2);
    return;
  }
  // fill histograms 
  tpcGas_st           *tpcGas = 0;
  if ( m_TpcdEdxCorrection && m_TpcdEdxCorrection->tpcGas()) tpcGas = m_TpcdEdxCorrection->tpcGas()->GetTable();
  
  StThreeVectorD g3 = gTrack->geometry()->momentum(); // p of global track
  Double_t pMomentum = g3.mag();
  StPidStatus PiD(gTrack); 
  if (PiD.PiDStatus < 0) return;
  //  Double_t bg = TMath::Log10(pMomentum/StProbPidTraits::mPidParticleDefinitions[kPidPion]->mass());
  Int_t sCharge = 0;
  if (gTrack->geometry()->charge() < 0) sCharge = 1;
  Int_t l;
  for (l = kPidElectron; l < KPidParticles; l++) {
    Int_t k = PiD.PiDkeyU3;
    if (PiD.fI70.fPiD) {
      I70.dev[l][sCharge]->Fill(PiD.bghyp[l],PiD.fI70.dev[l]);
      I70.dev[l][      2]->Fill(PiD.bghyp[l],PiD.fI70.dev[l]);
      if (k >= 0) {
	I70.devT[l][sCharge]->Fill(PiD.bghyp[l],PiD.fI70.dev[l]);
	I70.devT[l][      2]->Fill(PiD.bghyp[l],PiD.fI70.dev[l]);
      }
    }
    if (PiD.fFit.fPiD) {
      fitZ.dev[l][sCharge]->Fill(PiD.bghyp[l],PiD.fFit.dev[l]);
      fitZ.dev[l][      2]->Fill(PiD.bghyp[l],PiD.fFit.dev[l]);
      if (k >= 0) {
	fitZ.devT[l][sCharge]->Fill(PiD.bghyp[l],PiD.fFit.dev[l]);
	fitZ.devT[l][      2]->Fill(PiD.bghyp[l],PiD.fFit.dev[l]);
      }
    }
    if (fitN) {
      if (PiD.fdNdx.fPiD) {
	fitN->dev[l][sCharge]->Fill(PiD.bghyp[l],PiD.fdNdx.dev[l]);
	fitN->dev[l][      2]->Fill(PiD.bghyp[l],PiD.fdNdx.dev[l]);
	if (k >= 0) {
	  fitN->devT[l][sCharge]->Fill(PiD.bghyp[l],PiD.fdNdx.dev[l]);
	  fitN->devT[l][      2]->Fill(PiD.bghyp[l],PiD.fdNdx.dev[l]);
	}
      }
    }
  }
#if 0
  // Bad dE/dx
  Double_t L10Mult = -1;
  if (St_trigDetSumsC::instance()) L10Mult = St_trigDetSumsC::instance()->mult();
  //    StThreeVectorD pxyz = gTrack->geometry()->momentum();
  StThreeVectorD  xyz = gTrack->geometry()->helix().at(0);
  Double_t ZG  = xyz.z();
  Double_t PhiDG = 180*xyz.phi();
  if (PiD.Pred70BMN[1] > 0 && PiD.fI70.D() > 0) {
    if (TMath::Log(PiD.fI70.I()/PiD.Pred70BMN[0]) < -5*PiD.fI70.D()) {
      BaddEdxZPhi70[0]->Fill(ZG,PhiDG);
      BaddEdxMult70[0]->Fill(L10Mult);
    }
    if (TMath::Log(PiD.fI70.I()/PiD.Pred70BMN[1]) > 5*PiD.fI70.D()) {
      BaddEdxZPhi70[1]->Fill(ZG,PhiDG);
      BaddEdxMult70[1]->Fill(L10Mult);
    }
  }
  if (PiD.PredBMN[1] > 0 && PiD.fFit.D() > 0) {
    if (TMath::Log(PiD.fFit.I()) - TMath::Log(PiD.PredBMN[0]) < -5*PiD.fFit.D()) {
      BaddEdxZPhiZ[0]->Fill(ZG,PhiDG);
      BaddEdxMultZ[0]->Fill(L10Mult);
    }
    if (TMath::Log(PiD.fFit.I()) - TMath::Log(PiD.PredBMN[1]) > 5*PiD.fFit.D()) {
      BaddEdxZPhiZ[1]->Fill(ZG,PhiDG);
      BaddEdxMultZ[1]->Fill(L10Mult);
    }
  }
#endif
  if (PiD.fFit.Pred[kPidPion] <= 0) {
    LOG_WARN << "StdEdxY2Maker:: Prediction for p = " 
			<< pMomentum << " and TrackLength = " << PiD.fFit.TrackLength()
			<< " is wrong = " << PiD.fFit.Pred[kPidPion] << " <<<<<<<<<<<<<" << endl;
    return;
  };
#ifdef __iTPCOnly__
  Int_t noiTPC = 0;
  for (Int_t k = 0; k < NdEdx; k++) {
    if (St_tpcPadConfigC::instance()->iTPC(FdEdx[k].sector) &&
	St_tpcPadConfigC::instance()->IsRowInner(FdEdx[k].sector, FdEdx[k].row))
      noiTPC++;
  }
  Int_t iTPCOnly = (noiTPC >= 20) ? 1 : 0;
#endif
  for (Int_t j = 0; j < kTotalMethods; j++) {
    if (PiD.Status(kTPoints[j])) {
#ifdef __iTPCOnly__
      if (iTPCOnly) {
	TPointsiTPC[sCharge][j]->Fill(PiD.fFit.TrackLength(),PiD.fFit.log2dX(),PiD.Status(kTPoints[j])->dev[kPidPion]);
	PullsiTPC[sCharge][j]->Fill(PiD.fFit.TrackLength(),PiD.Status(kTPoints[j])->devS[kPidPion]);
      } else {
#endif
	TPoints[sCharge][j]->Fill(PiD.fFit.TrackLength(),PiD.fFit.log2dX(),PiD.Status(kTPoints[j])->dev[kPidPion]);
	Pulls[sCharge][j]->Fill(PiD.fFit.TrackLength(),PiD.Status(kTPoints[j])->devS[kPidPion]);
#ifdef __iTPCOnly__
      }
#endif
      if (j < 2 && PiD.fFit.TrackLength() > 40) {
	StTrackNode *node = gTrack->node();
	StPrimaryTrack *pTrack = static_cast<StPrimaryTrack*>(node->track(primary));
	if (pTrack) {
	  StPrimaryVertex *primVx = (StPrimaryVertex *) pTrack->vertex();
	  if (primVx) {
	    if (TMath::Abs(primVx->position().z()) < 10) {
	      StThreeVectorD P = pTrack->geometry()->helix().momentum(bField);
	      Double_t eta = P.pseudoRapidity();
#ifdef __iTPCOnly__
	      if (iTPCOnly) {
		EtaiTPC[j]->Fill(eta,PiD.Status(kTPoints[j])->dev[kPidPion]);
	      } else {
#endif
		Eta[j]->Fill(eta,PiD.Status(kTPoints[j])->dev[kPidPion]);
#ifdef __iTPCOnly__
	      }
#endif
	    }
	  }
	}
      }
    }
  }
  if (PiD.fFit.TrackLength() > 20) { 
    //  if (NoFitPoints >= 20) { 
    Int_t k;
    Double_t bgL10 = TMath::Log10(pMomentum/StProbPidTraits::mPidParticleDefinitions[kPidPion]->mass());
    for (k = 0; k < NdEdx; k++) {
      Int_t sector = FdEdx[k].sector;
      Int_t row    = FdEdx[k].row;
      Int_t rowS   = row;
      if (sector > 12) rowS = - rowS;
      FdEdx[k].zP = // Bichsel::Instance()->GetMostProbableZ(bgL10,1.);
	Bichsel::Instance()->GetMostProbableZ(bgL10,TMath::Log2(FdEdx[k].F.dx)); //remove dX
      FdEdx[k].sigmaP = //Bichsel::Instance()->GetRmsZ(bgL10,1.);
	Bichsel::Instance()->GetRmsZ(bgL10,TMath::Log2(FdEdx[k].F.dx)); //remove dX	
      Double_t predB  = 1.e-6*TMath::Exp(FdEdx[k].zP);
      FdEdx[k].F.dEdxN  = TMath::Log(FdEdx[k].F.dEdx /predB);
      for (Int_t l = 0; l <= StTpcdEdxCorrection::kTpcLast; l++) {
	if (FdEdx[k].C[l].dEdx > 0)
	  FdEdx[k].C[l].dEdxN = TMath::Log(FdEdx[k].C[l].dEdx/predB);
      }
      if (row <= St_tpcPadConfigC::instance()->innerPadRows(sector)) {
	hdEI->Fill(TMath::Log10(FdEdx[k].F.dE));
	hdEUI->Fill(TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::kUncorrected].dE));
	hdERI->Fill(TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::kAdcCorrection].dE));
	hdEPI->Fill(TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::ktpcPressure].dE));
	hdETI->Fill(TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::ktpcTime].dE));
	hdESI->Fill(TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::kTpcSecRowB].dE));
	hdEZI->Fill(TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::kzCorrection].dE));
	hdEMI->Fill(TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::kMultiplicity].dE));
      }
      else {
	hdEO->Fill(TMath::Log10(FdEdx[k].F.dE));
	hdEUO->Fill(TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::kUncorrected].dE));
	hdERO->Fill(TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::kAdcCorrection].dE));
	hdEPO->Fill(TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::ktpcPressure].dE));
	hdETO->Fill(TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::ktpcTime].dE));
	hdESO->Fill(TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::kTpcSecRowB].dE));
	hdEZO->Fill(TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::kzCorrection].dE));
	hdEMO->Fill(TMath::Log10(FdEdx[k].C[StTpcdEdxCorrection::kMultiplicity].dE));
      }
      if (pMomentum > pMomin && pMomentum < pMomax &&PiD.fFit.TrackLength() > 40 ) continue; // { // Momentum cut
#ifdef     __NEGATIVE_ONLY__
      if (sCharge != 1)  continue;
#endif /*  __NEGATIVE_ONLY__ */
#if 0
	if (tpcGas) {
	  if (inputTPCGasPressureP) inputTPCGasPressureP->Fill(tpcGas->inputTPCGasPressure,FdEdx[k].F.dEdxN);
	  if (nitrogenPressureP) nitrogenPressureP->Fill(tpcGas->nitrogenPressure,FdEdx[k].F.dEdxN);
	  if (gasPressureDiffP) gasPressureDiffP->Fill(tpcGas->gasPressureDiff,FdEdx[k].F.dEdxN);
	  if (inputGasTemperatureP) inputGasTemperatureP->Fill(tpcGas->inputGasTemperature,FdEdx[k].F.dEdxN);
	  if (flowRateArgon1P) flowRateArgon1P->Fill(tpcGas->flowRateArgon1,FdEdx[k].F.dEdxN);
	  if (flowRateArgon2P) flowRateArgon2P->Fill(tpcGas->flowRateArgon2,FdEdx[k].F.dEdxN);
	  if (flowRateMethaneP)  flowRateMethaneP->Fill(tpcGas->flowRateMethane,FdEdx[k].F.dEdxN);
	  if (percentMethaneInP) 
	    percentMethaneInP->Fill(tpcGas->percentMethaneIn*1000./tpcGas->barometricPressure,
				    FdEdx[k].C[StTpcdEdxCorrection::ktpcPressure-1].dEdxN);
	  if (percentMethaneInPA) 
	    percentMethaneInPA->Fill(tpcGas->percentMethaneIn*1000./tpcGas->barometricPressure,
				     FdEdx[k].C[StTpcdEdxCorrection::ktpcPressure].dEdxN);
	  if (percentMethaneInPC) 
	    percentMethaneInPC->Fill(tpcGas->percentMethaneIn*1000./tpcGas->barometricPressure,
				     FdEdx[k].F.dEdxN);
	  if (ppmOxygenInP) ppmOxygenInP->Fill(tpcGas->ppmOxygenIn,FdEdx[k].F.dEdxN);
	  if (flowRateExhaustP) flowRateExhaustP->Fill(tpcGas->flowRateExhaust,FdEdx[k].F.dEdxN);
	  if (ppmWaterOutP)  ppmWaterOutP->Fill(tpcGas->ppmWaterOut,FdEdx[k].C[StTpcdEdxCorrection::ktpcPressure-1].dEdxN);
	  if (ppmWaterOutPA) ppmWaterOutPA->Fill(tpcGas->ppmWaterOut,FdEdx[k].C[StTpcdEdxCorrection::ktpcPressure].dEdxN);
	  if (ppmWaterOutPC) ppmWaterOutPC->Fill(tpcGas->ppmWaterOut,FdEdx[k].F.dEdxN);
	  //	  if (ppmOxygenOutP) ppmOxygenOutP->Fill(tpcGas->ppmOxygenOut,FdEdx[k].F.dEdxN);
	  if (flowRateRecirculationP) flowRateRecirculationP->Fill(tpcGas->flowRateRecirculation,FdEdx[k].F.dEdxN);
	}
#endif
	if (St_trigDetSumsC::instance()) {
	  if (FdEdx[k].Zdc > 0 && ZdcCP) ZdcCP->Fill(TMath::Log10(FdEdx[k].Zdc), FdEdx[k].F.dEdxN);
#if 0
	  if (St_trigDetSumsC::instance()->bbcYellowBkg() > 0 && bbcYellowBkg) 
	    bbcYellowBkg->Fill(TMath::Log10(St_trigDetSumsC::instance()->bbcYellowBkg()), FdEdx[k].F.dEdxN);
	  if (St_trigDetSumsC::instance()->bbcBlueBkg() > 0 && bbcBlueBkg) 
	    bbcBlueBkg->Fill(TMath::Log10(St_trigDetSumsC::instance()->bbcBlueBkg()), FdEdx[k].F.dEdxN);
#endif
	  if (St_trigDetSumsC::instance()->bbcX() > 0)  {
	    if (BBCP) BBCP->Fill(TMath::Log10(St_trigDetSumsC::instance()->bbcX()), FdEdx[k].F.dEdxN);
	  }
	}
	Double_t dEN = 0;
	Double_t zdEMPV = 0;
	if (fUsedNdx) {
	  Double_t n_P = FdEdx[k].dxC*PiD.fdNdx.Pred[kPidPion];
	  dEN = TMath::Log(1e6*FdEdx[k].F.dE); // scale to <dE/dx>_MIP = 2.4 keV/cm
	  StdEdxModel::ETpcType kTpc = StdEdxModel::kTpcOuter;
	  if (St_tpcPadConfigC::instance()->IsRowInner(sector,row)) kTpc = StdEdxModel::kTpcInner;
#ifdef __LogProb__
	  zdEMPV = TMath::Log(1.e-3*n_P) + StdEdxModel::instance()->GetLogdEdNMPV(kTpc)->Interpolate(TMath::Log(n_P)); // log(dE[keV])
#else /* ! __LogProb__ */
	  zdEMPV = TMath::Log(1.e-3*n_P*StdEdxModel::instance()->GetdEdNMPV(kTpc)->Interpolate(TMath::Log(n_P))); // log(dE[keV])
#endif /* __LogProb__ */
	}
	Double_t Vars[9] = {
	  FdEdx[k].C[StTpcdEdxCorrection::kTpcSecRowB-1].dEdxN,
	  FdEdx[k].F.dEdxN,
	  dEN - zdEMPV,
	  TMath::Log10(FdEdx[k].dxC*PiD.fdNdx.Pred[kPidElectron]),
	  TMath::Log10(FdEdx[k].dxC*PiD.fdNdx.Pred[kPidPion]),
	  TMath::Log10(FdEdx[k].dxC*PiD.fdNdx.Pred[kPidKaon]),
	  TMath::Log10(FdEdx[k].dxC*PiD.fdNdx.Pred[kPidProton]),
	  TMath::Log10(FdEdx[k].dxC*PiD.fdNdx.Pred[kPidDeuteron]),
	  FdEdx[k].F.dx
	};
	Double_t VarsV[9] = {
	  FdEdx[k].C[StTpcdEdxCorrection::kTpcRowQ-1].dEdxN,
	  Vars[1],
	  Vars[2],
	  Vars[3],
	  Vars[4],
	  Vars[5],
	  Vars[6],
	  Vars[7],
	  Vars[8]
	};
	Double_t Pad2Edge = FdEdx[k].edge;
	if (TMath::Abs(Pad2Edge) > 5) {
	  SecRow3.Fill(sector,row,Vars);
	  if (FdEdx[k].adc > 0) {
	    Double_t ADCL = TMath::Log(FdEdx[k].adc);
	    ADC3.Fill(sector,row,&ADCL);
	  }
	}
#ifdef __ZDC3__
	if (FdEdx[k].Zdc > 0) Zdc3.Fill(rowS,TMath::Log10(FdEdx[k].Zdc),Vars);
#endif /* __ZDC3__ */
	//Double_t xyz[3]  = {FdEdx[k].xyz[0],FdEdx[k].xyz[1],FdEdx[k].xyz[2]};
	//Double_t xyzD[3] = {FdEdx[k].xyzD[0],FdEdx[k].xyzD[1],FdEdx[k].xyzD[2]};
	//Double_t Phi  = 180./TMath::Pi()*TMath::ATan2(xyz[0],xyz[1]);
	//	Double_t PhiD = 180./TMath::Pi()*TMath::ATan2(xyzD[0],xyzD[1]); 
#if 0
#ifdef __iTPCOnly__
	if (! St_tpcPadConfigC::instance()->iTPC(sector)) 
#endif
	  TanL3D.Fill(rowS,FdEdx[k].TanL,Vars);
#ifdef __iTPCOnly__xs
	else 
	  TanL3DiTPC.Fill(rowS,FdEdx[k].TanL,Vars);
#endif
#endif
	if (tpcGas) {
	  Double_t p     = tpcGas->barometricPressure;
	  //	  Double_t t     = tpcGas->inputGasTemperature/298.2;
	  if (p > 0) {
	    Double_t press = TMath::Log(p);
	    Pressure.Fill(rowS,press,Vars);
	  }
	  Int_t cs = NumberOfChannels*(sector-1)+FdEdx[k].channel;
	  Double_t V = FdEdx[k].Voltage;
	  Double_t VN = (row <= St_tpcPadConfigC::instance()->innerPadRows(sector)) ? V - 1170 : V - 1390;
	  Voltage.Fill(cs,VN,VarsV);
	  //	  Volt.Fill(cs,V,VarsV);
	  Qcm.Fill(cs,FdEdx[k].Qcm,VarsV);
	  AvCurrent.Fill(cs,FdEdx[k].Crow,VarsV);
// 	  if (p*t > 0) {
// 	    Double_t temp = TMath::Log(p/t);
// 	    PressureT.Fill(rowS,temp,Vars);
// 	  }
	}
	Double_t vars[2] = {tpcTime,FdEdx[k].C[ StTpcdEdxCorrection::ktpcTime-1].dEdxN};
	if (Time)    Time->Fill(vars);
	//	if (TimeP)  {vars[1] = FdEdx[k].C[StTpcdEdxCorrection::ktpcTime].dEdxN; TimeP->Fill(vars);}
	if (TimeC)  {vars[1] = FdEdx[k].F.dEdxN; TimeC->Fill(vars);}
#ifdef __iTPCOnly__
	if (! St_tpcPadConfigC::instance()->iTPC(sector)) {
#endif
	  Z3.Fill(rowS,FdEdx[k].ZdriftDistance,Vars);
#if 0
	  dX3.Fill(rowS,FdEdx[k].F.dx, Vars);
#endif
#ifdef __iTPCOnly__
	} else {
	  Z3iTPC.Fill(rowS,FdEdx[k].ZdriftDistance,Vars);
#if 0
	  dX3iTPC.Fill(rowS,FdEdx[k].F.dx, Vars);
#endif
	}
#endif
	//	Z3O.Fill(rowS,FdEdx[k].ZdriftDistanceO2,Vars);
#if 0
	Edge3.Fill(St_tpcPadConfigC::instance()->numberOfRows(20)*(sector-1)+row,FdEdx[k].edge, Vars);
#endif
	xyPad3.Fill(FdEdx[k].yrow,FdEdx[k].xpad, Vars);
	//      } end loop of dE/dx samples
    }
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
    I = (i*I +  pdEdx->F.dEdx)/(i+1);
    //     cout << Names[iop] << " " << i << " S/R " << dEdx->sector << "/" << dEdx->row
    // 	 << " dEdx(keV/cm) " << 1.e6*dEdx->dEdx << " dx " << dEdx->dx 
    //       //	 << " dx " << dEdx->dxH 
    // 	 << " x[" << dEdx->xyz[0] << "," << dEdx->xyz[1] << "," << dEdx->xyz[2] << "]" 
    // 	 << " d[" << dEdx->xyzD[0] << "," << dEdx->xyzD[1] << "," << dEdx->xyzD[2] << "]" 
    // 	 << " R[" << dEdx->resXYZ[0] << "," << dEdx->resXYZ[1] << "," << dEdx->resXYZ[2] << "]" 
    // 	 << " Sum " << 1.e6*I << "(keV)"
    // 	 << " Prob " << dEdx->Prob << endl;
    cout << Form("%s %2i  S/R %2i/%2i dEdx(keV/cm) %8.2f dx %5.2f dxC %5.2f x[%8.2f,%8.2f,%8.2f] Qcm %7.2f AvC %7.3f", 
		 Names[iop],i,pdEdx->sector,pdEdx->row,1.e6*dEdx, pdEdx->F.dx ,pdEdx->dxC, pdEdx->xyz[0], pdEdx->xyz[1], 
		 pdEdx->xyz[2],pdEdx->Qcm,pdEdx->Crow);
    cout << Form(" d[%8.2f,%8.2f,%8.2f] Sum %8.2f Prob %8.5f", pdEdx->xyzD[0], pdEdx->xyzD[1], pdEdx->xyzD[2],
		 1.e6*I,pdEdx->Prob) << endl;
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
    TDatime t1(tMin,0); /// min Time and
      TDatime t2(tMax,0); /// max 
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
  }
  else {
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
      fZOfBadHits = new TH1F*[fNZOfBadHits];
      static const Char_t *BadCaseses[fNZOfBadHits] = 
      {"it is not used in track fit",     // 0
       "it is flagged ",                  // 1
       "track length is inf ",            // 2
       "it does not pass check ",         // 3
       "dx is out interval [0.5,25]",     // 4
       "Sector/Row gain < 0",             // 5 iok + 4
       "drift distance < min || drift distance > max", // 6
       "dE < 0 or dx < 0",                // 7
       "Edge effect",                     // 8
       "Anode Voltage problem",           // 9
       "Total no.of rejected clusters"    // 10
      };
      for (Int_t i = 0; i < fNZOfBadHits; i++) 
	fZOfBadHits[i] = new TH1F(Form("ZOfBadHits%i",i),
				  Form("Z of rejected clusters  because %s",BadCaseses[i]),
				  100,-210,210);                        
      fZOfGoodHits = new TH1F("ZOfGoodHits","Z of accepted clusters",100,-210,210);                        
      fPhiOfBadHits = new TH1F("PhiOfBadHits","Phi of rejected clusters",100, -TMath::Pi(), TMath::Pi());
      fTracklengthInTpcTotal = new TH1F("TracklengthInTpcTotal","Total track in TPC",100,0,200);         
      fTracklengthInTpc = new TH1F("TracklengthInTpc","Track length in TPC used for dE/dx",100,0,200);   
      const Char_t *FitName[3] = {"I70","F","N"};

      enum  {kTotalMethods = 6};
      for (Int_t k = 0; k < kTotalMethods/2; k++) {
	const Char_t *parN[5] = {"","pi","e","K","P"};
	const Char_t *parT[5] = {"All","|nSigmaPion| < 1","|nSigmaElectron| < 1","|nSigmaKaon| < 1","|nSigmaProton| < 1"};
	Double_t ymin = 0, ymax = 2.5;
	if (k == 2) {ymin = 1.2; ymax = 3.7;}
	for (Int_t t = 0; t < 5; t++) {
	  TString Title(Form("log10(dE/dx(%s)(keV/cm)) versus log10(p(GeV/c)) for Tpc TrackLength > 40 cm %s",FitName[k],parT[t]));
	  if (k == 2) Title = Form("log10(dN/dx) versus log10(p(GeV/c)) for Tpc TrackLength > 40 cm %s",parT[t]);
	  fTdEdx[k][t] = new TH2F(Form("TdEdx%s%s",FitName[k],parN[t]),Title,
				  330,-1.3,2., 500, ymin, ymax);
	  fTdEdx[k][t]->SetMarkerStyle(1);
	  fTdEdx[k][t]->SetMarkerColor(t+1);
	}
      } 
#ifdef __USEZ3A__
      Int_t      nZBins = 200;
      Double_t ZdEdxMin = -5;
      Double_t ZdEdxMax =  5;
      Z3A = new TH3F("Z3A",
		     "log(dEdx/Pion) corrected versus row and Drift Distance for all MIP primary tracks (Inner/Outer)",
		     2,-0.5, 1.5,105,0.,210.,nZBins,ZdEdxMin,ZdEdxMax);
#endif
    }
    if (! f && !first) {
      for (Int_t i = 0; i < fNZOfBadHits; i++) AddHist(fZOfBadHits[i]);           
      AddHist(fZOfGoodHits);
      AddHist(fPhiOfBadHits);         
      AddHist(fTracklengthInTpcTotal);
      AddHist(fTracklengthInTpc);     
      for (Int_t k = 0; k < 3; k++) {
	for (Int_t t = 0; t < 5; t++) {
	  AddHist(fTdEdx[k][t]);
	}
      }
    }
#ifdef __USEZ3A__
    Z3A->SetDirectory(0);
#endif
    first = 2004;
  }  else {
    StSPtrVecTrackPidTraits &traits = gTrack->pidTraits();
    static StDedxPidTraits *pid = 0;
    static Double_t TrackLength, I70, fitZ, fitN;
    static StProbPidTraits *pidprob = 0;
    StThreeVectorD g3 = gTrack->geometry()->momentum(); // p of global track
    Double_t pMomentum = g3.mag();
    for (UInt_t i = 0; i < traits.size(); i++) {
      if (! traits[i]) continue;
      if ( traits[i]->IsZombie()) continue;
      pidprob = dynamic_cast<StProbPidTraits*>(traits[i]);
    }
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
	  if (pidprob) {
	    const StParticleDefinition* pd = gTrack->pidTraits(PidAlgorithmFitZ);
	    if (pd) {
	      if (TMath::Abs(PidAlgorithmFitZ.numberOfSigma(Pion))     < 1) fTdEdx[k][1]->Fill(TMath::Log10(pMomentum), Log10E*fitZ + 6.);
	      if (TMath::Abs(PidAlgorithmFitZ.numberOfSigma(Electron)) < 1) fTdEdx[k][2]->Fill(TMath::Log10(pMomentum), Log10E*fitZ + 6.);
	      if (TMath::Abs(PidAlgorithmFitZ.numberOfSigma(Kaon))     < 1) fTdEdx[k][3]->Fill(TMath::Log10(pMomentum), Log10E*fitZ + 6.);
	      if (TMath::Abs(PidAlgorithmFitZ.numberOfSigma(Proton))   < 1) fTdEdx[k][4]->Fill(TMath::Log10(pMomentum), Log10E*fitZ + 6.);
	    }
	  }
	}
	if (pid->method() == kOtherMethodId) {
	  fitN = pid->mean(); 
	  TrackLength = pid->length(); 
	  if (TrackLength < 40) continue;
	  k = 2;
	  fTdEdx[k][0]->Fill(TMath::Log10(pMomentum),TMath::Log10(fitN));
	  if (pidprob) {
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
#ifdef __USEZ3A__
    if (Z3A && pMomentum > pMomin && pMomentum < pMomax && TrackLength > 40) {
      Double_t bgL10 = TMath::Log10(pMomentum/StProbPidTraits::mPidParticleDefinitions[kPidPion]->mass());
      for (Int_t k = 0; k < NdEdx; k++) {
	Double_t zP = Bichsel::Instance()->GetMostProbableZ(bgL10,TMath::Log2(FdEdx[k].dx));
	Double_t predB  = 1.e-6*TMath::Exp(zP);
	Double_t dEdxN  = TMath::Log(FdEdx[k].dEdx /predB);
	Int_t io = 0;
	if (row > St_tpcPadConfigC::instance()->innerPadRows(sector)) io = 1;
	Z3A->Fill(io,FdEdx[k].ZdriftDistance,  dEdxN);
      }
    }
#endif
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
  if (iFlag >= 0 && iFlag < fNZOfBadHits && fZOfBadHits[iFlag]) fZOfBadHits[iFlag]->Fill(xyz.z());
  if (fZOfBadHits[fNZOfBadHits-1]) fZOfBadHits[fNZOfBadHits-1]->Fill(xyz.z());
  if (fPhiOfBadHits!= 0) fPhiOfBadHits->Fill(TMath::ATan2(xyz.y(),xyz.x()));
}
//________________________________________________________________________________
Int_t StdEdxY2Maker::Propagate(const StThreeVectorD &middle,const StThreeVectorD &normal,
			       const StPhysicalHelixD &helixI, const StPhysicalHelixD &helixO,
			       StThreeVectorD &xyz, StThreeVectorD &dirG, Double_t s[2], Double_t w[2]) {
  xyz  = StThreeVectorD();
  dirG = StThreeVectorD();
  s[0] = helixI.pathLength(middle, normal);
  s[1] = helixO.pathLength(middle, normal);
  Double_t sA[2] = {s[0]*s[0], s[1]*s[1]};
  if (sA[0] > 1.e6 || sA[1] > 1.e6) {return 1;}
  Double_t sN = sA[0] + sA[1];
  w[0] = sA[0]/sN;
  w[1] = sA[1]/sN;
  if (w[0] > 1.e-4) {xyz += w[0]*helixO.at(s[1]); dirG += w[0]*helixO.momentumAt(s[1],bField);}
  if (w[1] > 1.e-4) {xyz += w[1]*helixI.at(s[0]); dirG += w[1]*helixI.momentumAt(s[0],bField);}
  return 0;
}
//________________________________________________________________________________
void StdEdxY2Maker::V0CrossCheck() {
#ifdef  StTrackMassFit_hh
  static Int_t first=0;
  enum {NHYPSV0 = 3};        // e, pi, p
  static Int_t hyps[NHYPSV0] = {kPidElectron,  kPidPion, kPidProton};
  static TH2F *hist70B[NHYPSV0][2], *histzB[NHYPSV0][2];
  static TH2F *hist70BT[NHYPSV0][2], *histzBT[NHYPSV0][2];
  if (! first) {
    for (Int_t hyp=0; hyp<NHYPSV0;hyp++) {
      Int_t h = hyps[hyp];
      for (Int_t sCharge = 0; sCharge < 2; sCharge++) {
	TString nameP(StProbPidTraits::mPidParticleDefinitions[h]->name().data());
	nameP += "V0";
	nameP.ReplaceAll("-","");
	if (sCharge == 0) nameP += "P";
	else              nameP += "N";
	TString name = nameP;
	name += "70";
	TString title("V0: log(dE/dx70/I(");
	title += nameP;
	title += ")) versus log10(p/m)";
	name += "B";
	title += " Bichsel";
	hist70B[hyp][sCharge] = new TH2F(name.Data(),title.Data(),140,-1,6,600,-2,4);
	name += "T";
	title += " Unique";
	hist70BT[hyp][sCharge] = new TH2F(name.Data(),title.Data(),140,-1,6,600,-2,4);
	name = nameP;
	name += "z";
	title = "V0: zFit - log(I(";
	title += nameP;
	title += ")) versus log10(p/m)";
	name += "B";
	title += " Bichsel";
	histzB[hyp][sCharge] = new TH2F(name.Data(),title.Data(),140,-1,6,600,-2,4);
	name += "T";
	title += " Unique";
	histzBT[hyp][sCharge] = new TH2F(name.Data(),title.Data(),140,-1,6,600,-2,4);
      }
    }
    first = 2013;
  }
  StEvent* pEvent = dynamic_cast<StEvent*> (GetInputDS("StEvent"));
  if (! pEvent) return;
  StSPtrVecV0Vertex& v0Vertices = pEvent->v0Vertices();
  UInt_t nV0 = v0Vertices.size();
  if (! nV0) return;
  for (UInt_t iV0 = 0; iV0 < nV0; iV0++) {
    StV0Vertex *v0Vertex = v0Vertices[iV0];
    if (! v0Vertex) continue;
    const StTrackMassFit *mF = dynamic_cast<const StTrackMassFit *>(v0Vertex->parent());
    if (! mF) continue;
    const KFParticle *particle = mF->kfParticle();
    if (! particle) continue;
    Int_t PiDkeyU3 = 0;
    for (UInt_t jV0 = 0; jV0 < nV0; jV0++) {
      if (iV0 == jV0) continue;
      StV0Vertex *v0VertexJ = v0Vertices[jV0];
      if (! v0VertexJ) continue;
      const StTrackMassFit *mFJ = dynamic_cast<const StTrackMassFit *>(v0VertexJ->parent());
      if (! mFJ) continue;
      const KFParticle *particleJ = mFJ->kfParticle();
      if (! particleJ) continue;
      if (particleJ->Id() == particle->Id()) {
	PiDkeyU3 = -1; break;
      }
    }
    Int_t pdg = particle->GetPDG();
    Int_t h[2] = {-1,-1};
    Int_t l[2] = {-1,-1}; // index in hyps[]
    switch (pdg) {
    case 22   : h[0] = h[1] = kPidElectron;           l[0] =    l[1] = 0; break;
    case 310  : h[0] = h[1] = kPidPion;               l[0] =    l[1] = 1; break;
    case -3122: h[0] = kPidProton; h[1] = kPidPion;   l[0] = 2; l[1] = 1; break;
    case  3122: h[0] = kPidPion;   h[1] = kPidProton; l[0] = 1; l[1] = 2; break;
    default: break;
    };
    if (h[0] < 0 || h[1] < 0) continue;
    StGlobalTrack *gTracks[2] = 
      {dynamic_cast<StGlobalTrack *>(v0Vertex->daughter(negative)), 
       dynamic_cast<StGlobalTrack *>(v0Vertex->daughter(positive))};
    if (! gTracks[0] || ! gTracks[1]) continue;
    StPidStatus PiDN(gTracks[0]), PiDP(gTracks[1]);
    if (PiDN.PiDStatus < 0 || PiDP.PiDStatus < 0) continue;
    StPidStatus *PiDs[2] = {&PiDN, &PiDP};
    if (PiDkeyU3 >= 0 &&
	PiDs[0]->PiDkeyU3 == h[0] &&  
	PiDs[1]->PiDkeyU3 == h[1])  PiDkeyU3 = 1; 
    for (Int_t sCharge = negative; sCharge <= positive; sCharge++) {
      Int_t m = h[sCharge];
      Double_t bg10 = PiDs[sCharge]->bghyp[m];
      if (PiDs[sCharge]->fI70.fPiD) {
	Double_t z = TMath::Log(PiDs[sCharge]->fI70.I()/PiDs[sCharge]->fI70.Pred[m]);
	hist70B[l[sCharge]][sCharge]->Fill(bg10,z);
	if (PiDkeyU3 > 0) 
	  hist70BT[l[sCharge]][sCharge]->Fill(bg10,z);
      }
      if (PiDs[sCharge]->fFit.fPiD) {
	Double_t z = TMath::Log(PiDs[sCharge]->fFit.I()/PiDs[sCharge]->fFit.Pred[m]);
	histzB[l[sCharge]][sCharge]->Fill(bg10,z);
	if (PiDkeyU3 > 0) 
	  histzBT[l[sCharge]][sCharge]->Fill(bg10,z);
      }
    }
  }
#endif /*  StTrackMassFit_hh */
}
//________________________________________________________________________________
void StdEdxY2Maker::fcnN(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag) {
  static Int_t _debug = 0; 
  //  Double_t Val[2];
  static TH2F *ProbV[3] = {0}, *ProbdX[3] = {0}, *ProbdY[3] = {0};
  static Double_t xMin[3], xMax[3], yMin[3], yMax[3];
  if (! ProbV[2]) {
    for (Int_t k = 0; k < 3; k++) {
      StdEdxModel::ETpcType kTpc = (StdEdxModel::ETpcType) k;
#ifdef __LogProb__
      ProbV[kTpc]  = StdEdxModel::instance()->GetLogdEdN(StdEdxModel::kProb,    kTpc);
      ProbdX[kTpc] = StdEdxModel::instance()->GetLogdEdN(StdEdxModel::kdProbdX, kTpc);
      ProbdY[kTpc] = StdEdxModel::instance()->GetLogdEdN(StdEdxModel::kdProbdY, kTpc);
#else /* ! __LogProb__ */
      ProbV[kTpc]  = StdEdxModel::instance()->GetdEdN(StdEdxModel::kProb,    kTpc);
      ProbdX[kTpc] = StdEdxModel::instance()->GetdEdN(StdEdxModel::kdProbdX, kTpc);
      ProbdY[kTpc] = StdEdxModel::instance()->GetdEdN(StdEdxModel::kdProbdY, kTpc);
#endif /* __LogProb__ */
      xMin[kTpc] = ProbV[kTpc]->GetXaxis()->GetXmin();
      xMax[kTpc] = ProbV[kTpc]->GetXaxis()->GetXmax();
      yMin[kTpc] = ProbV[kTpc]->GetYaxis()->GetXmin();
      yMax[kTpc] = ProbV[kTpc]->GetYaxis()->GetXmax();
    }
  }
  f = 0;
  gin[0] = 0.;
  Double_t dNdx = par[0]; // Mu
  for (Int_t i = 0; i < NdEdx; i++) {
    StdEdxModel::ETpcType kTpc = StdEdxModel::kTpcOuter;
    if (St_tpcPadConfigC::instance()->IsRowInner(FdEdx[i].sector,FdEdx[i].row)) kTpc = StdEdxModel::kTpcInner;
    Double_t dE = 1e9*FdEdx[i].F.dE; // GeV => eV
    Double_t dX = FdEdx[i].dxC;
    Double_t Np = dNdx*dX;
    Double_t X = TMath::Log(Np);
#ifdef __LogProb__
    Double_t Y = TMath::Log(dE/Np);
#else /* ! __LogProb__ */
    Double_t Y = dE/Np;
#endif /* __LogProb__ */
    FdEdx[i].Prob = 0;
    if (X < xMin[kTpc] || X > xMax[kTpc] ||
	Y < yMin[kTpc] || Y > yMax[kTpc]) {
      f += 100;
      continue;
    }
    Double_t Prob = ProbV[kTpc]->Interpolate(X,Y);
    if (Prob <= 0.0) {
      f += 100;
      continue;
    }
    f -= 2*TMath::Log(Prob);
    FdEdx[i].Prob = Prob;
    Double_t dProbOverdX = ProbdX[kTpc]->Interpolate(X,Y);
    Double_t dProbOverdY = ProbdY[kTpc]->Interpolate(X,Y);
    Double_t dNpOverdMu = dX;
    Double_t dXOverdNp =  1./Np;
#ifdef __LogProb__
    Double_t dYOverdNp = - 1./Np;
#else /* ! __LogProb__ */
    Double_t dYOverdNp = - Y/Np;
#endif /* __LogProb__ */
    Double_t dProbOverNp  = dProbOverdX * dXOverdNp + dProbOverdY * dYOverdNp;
    Double_t dProbOverdMu = dProbOverNp * dNpOverdMu;
    Double_t dfOverdMu = -2*dProbOverdMu/Prob;
    gin[0] += dfOverdMu;
  }
  if (_debug) {
    cout << " dNdx = " << dNdx << "\tf = " << f << endl;
    PrintdEdx(1);
    cout << "===================" << endl;
  }
}
//________________________________________________________________________________
void StdEdxY2Maker::DoFitN(Double_t &chisq, Double_t &fitZ, Double_t &fitdZ){
  Double_t dNdx = 0;
  for (Int_t i=0;i<NdEdx;i++) {
    dNdx += FdEdx[i].F.dEdx*1e9/100; // ~100 eV per primary interaction
    FdEdx[i].Prob = 0;
  }
  if (NdEdx>5) {
    dNdx /= NdEdx;
    Double_t arglist[10];
    Int_t ierflg = 0;
    m_Minuit->SetFCN(fcnN);
    //    m_Minuit->SetPrintLevel(-1);
    if (Debug() < 2) {
      arglist[0] = -1;
      m_Minuit->mnexcm("set print",arglist, 1, ierflg);
    }
    m_Minuit->mnexcm("set NOW",arglist, 0, ierflg);
    m_Minuit->mnexcm("CLEAR",arglist, 0, ierflg);
    arglist[0] = 0.5;
    m_Minuit->mnexcm("SET ERR", arglist ,1,ierflg);
    //    m_Minuit->mnparm(0, "LogdNdx", TMath::Log(dNdx), 0.5, 0.,0.,ierflg); //First Guess
    m_Minuit->DefineParameter(0, "dNdx", dNdx, 0.5, 0.1*dNdx, 10*dNdx);
    if (Debug() < 2)       arglist[0] = 1.;   // 1.
    else                   arglist[0] = 0.;   // Check gradient 
    m_Minuit->mnexcm("SET GRAD",arglist,1,ierflg);
    arglist[0] = 500;
    arglist[1] = 1.;
    //    m_Minuit->mnexcm("MIGRAD", arglist ,2,ierflg);
    m_Minuit->mnexcm("MINIMIZE", arglist ,2,ierflg);
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

