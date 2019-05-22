/// \Author Y.Fisyak, fisyak@bnl.gov
/// \date
// doxygen info here
/*
  The maker's algorithms and formulae based on 
  http://www.inst.bnl.gov/programs/gasnobledet/publications/Mathieson's_Book.pdf,
  and  Photo Absorption Model
  "Ionization energy loss in very thin absorbers.", V.M. Grishin, V.K. Ermilova, S.K. Kotelnikov Nucl.Instrum.Meth.A309:476-484,1991
  "A method to improve tracking and particle identification in TPC's and silicon detectors.", Hans Bichsel, Nucl.Instrum.Meth.A562:154-197,2006
  HEED: "Modeling of ionization produced by fast charged particles in gases", I.B. Smirnov, Nucl.Instrum.Meth.A55(2005) 747-493. 
  
*/
#include <assert.h>
#include "StTpcRSMaker.h"
#include "Stiostream.h"
// SCL
#include "StGlobals.hh"
#include "StThreeVectorD.hh"
#include "StPhysicalHelixD.hh"
// ROOT
#include "TClassTable.h"
#include "TDataSetIter.h"
#include "TTableSorter.h"
#include "TRandom.h"
#include "TSystem.h"
#include "TFile.h"
#include "TBenchmark.h"
#include "TProfile2D.h"
#include "TVirtualMC.h"
#include "TInterpreter.h"
#include "Math/SpecFuncMathMore.h"
#include "StDbUtilities/StCoordinates.hh" 
#include "StDbUtilities/StTpcCoordinateTransform.hh"
// Dave's Header file
#include "StDbUtilities/StMagUtilities.h"
//#include "StDaqLib/TPC/trans_table.hh"
#include "StDetectorDbMaker/St_tpcAltroParamsC.h"
#include "StDetectorDbMaker/St_asic_thresholdsC.h"
#include "StDetectorDbMaker/St_tss_tssparC.h"
#include "StDetectorDbMaker/St_tpcPadGainT0BC.h"
#include "StDetectorDbMaker/St_TpcResponseSimulatorC.h"
#include "StDetectorDbMaker/St_tpcAnodeHVavgC.h"
#include "StDetectorDbMaker/StDetectorDbTpcRDOMasks.h"
#include "StDetectorDbMaker/St_tpcPadConfigC.h"
#include "StDetectorDbMaker/St_tpcGainCorrectionC.h"
#include "StDetectorDbMaker/St_TpcAvgCurrentC.h"
#include "StDetectorDbMaker/St_TpcAvgPowerSupplyC.h"
#include "StDetectorDbMaker/St_trigDetSumsC.h"
#include "StParticleTable.hh"
#include "StParticleDefinition.hh"
#include "Altro.h"
#include "TRVector.h"
#include "StBichsel/Bichsel.h"
#include "StdEdxY2Maker/StTpcdEdxCorrection.h"
// g2t tables
#include "tables/St_g2t_track_Table.h"
#include "tables/St_g2t_vertex_Table.h" 
#include "tables/St_g2t_tpc_hit_Table.h"
struct HitPoint_t {
  Int_t indx;
  Int_t TrackId;
  Double_t s; // track length to current point
  Double_t sMin, sMax;
  g2t_tpc_hit_st *tpc_hitC;
  StGlobalCoordinate   xyzG;
  StTpcLocalSectorCoordinate coorLS;
  StTpcLocalSectorDirection dirLS, BLS;
  StTpcPadCoordinate Pad;	
};
//#define ElectronHack
//#define __LASERINO__
//#define Old_dNdx_Table
#define __STOPPED_ELECTRONS__
#define __DEBUG__
#if defined(__DEBUG__)
#define PrPP(A,B) if (Debug()%10 > 2) {LOG_INFO << "StTpcRSMaker::" << (#A) << "\t" << (#B) << " = \t" << (B) << endm;}
#else
#define PrPP(A,B)
#endif
static const char rcsid[] = "$Id: StTpcRSMaker.cxx,v 1.89 2019/05/22 21:30:58 fisyak Exp $";
#define __ClusterProfile__
static Bool_t ClusterProfile = kFALSE;
#define Laserino 170
#define Chasrino 171
//                                    Inner        Outer
static       Double_t t0IO[2]   = {1.20868e-9, 1.43615e-9}; // recalculated in InducedCharge
static const Double_t tauC[2]   = {999.655e-9, 919.183e-9}; 
TF1F*     StTpcRSMaker::fgTimeShape3[2]    = {0, 0};
TF1F*     StTpcRSMaker::fgTimeShape0[2]    = {0, 0};
//________________________________________________________________________________
static const Int_t nx[2] = {200,500};
static const Double_t xmin[2] =  {-10., -6};
static const Double_t xmax[2] =  { 10., 44};
static const Int_t nz = 42;
static const Double_t zmin = -210;
static const Double_t zmax = -zmin;
//                     io pt
static TProfile2D *hist[5][3] = {0};
static const Int_t nChecks = 21;
static TH1  *checkList[2][21] = {0};
#ifdef __LASERINO__
static TProfile2D  *SecRow[15] = {0};
#endif /* __LASERINO__ */
static TString TpcMedium("TPCE_SENSITIVE_GAS");
//________________________________________________________________________________
ClassImp(StTpcRSMaker);
//________________________________________________________________________________
StTpcRSMaker::StTpcRSMaker(const char *name): 
  StMaker(name),
  mLaserScale(1),
  minSignal(1e-4),
  ElectronRange(0.0055), // Electron Range(.055mm)
  ElectronRangeEnergy(3000), // eV
  ElectronRangePower(1.78), // sigma =  ElectronRange*(eEnery/ElectronRangeEnergy)**ElectronRangePower
  NoOfSectors(24),
  NoOfPads(182),
  NoOfTimeBins(__MaxNumberOfTimeBins__),
  mCutEle(1e-5)
{
  memset(beg, 0, end-beg+1);
  m_Mode = 0;
  //  SETBIT(m_Mode,kHEED); 
  SETBIT(m_Mode,kBICHSEL);  // Default is Bichsel
  SETBIT(m_Mode,kdEdxCorr);
  SETBIT(m_Mode,kDistortion);
}
//________________________________________________________________________________
StTpcRSMaker::~StTpcRSMaker() {
  SafeDelete(mAltro);
  Finish();
}
//________________________________________________________________________________
Int_t StTpcRSMaker::Finish() {
  //  SafeDelete(fTree);
  if (m_SignalSum) {free(m_SignalSum); m_SignalSum = 0;}
  SafeDelete(mdNdx);
  SafeDelete(mdNdxL10);
  SafeDelete(mdNdEL10);
  for (Int_t io = 0; io < 2; io++) {// Inner/Outer
    for (Int_t sec = 0; sec < NoOfSectors; sec++) {
      if (mShaperResponses[io][sec] && !mShaperResponses[io][sec]->TestBit(kNotDeleted)) {SafeDelete(mShaperResponses[io][sec]);}
      SafeDelete(mChargeFraction[io][sec]);
      SafeDelete(mPadResponseFunction[io][sec]);
    }
    SafeDelete(mPolya[io]);
  }
  if (m_TpcdEdxCorrection && m_TpcdEdxCorrection->TestBit(kCanDelete)) delete m_TpcdEdxCorrection;
  m_TpcdEdxCorrection = 0;
  return StMaker::Finish();
}
//________________________________________________________________________________
Int_t StTpcRSMaker::InitRun(Int_t /* runnumber */) {
  if (!gStTpcDb) {
    LOG_ERROR << "Database Missing! Can't initialize TpcRS" << endm;
    return kStFatal;
  }
  mCutEle = GetCutEle();
  if (mCutEle > 0) {
    LOG_INFO << "StTpcRSMaker::InitRun: mCutEle set to = " << mCutEle << " from geant \"" << TpcMedium.Data() << "\" parameters" << endm;
  } else {
    mCutEle = 1e-4;
    LOG_ERROR << "StTpcRSMaker::InitRun: mCutEle has not been found in GEANT3 for \"" << TpcMedium.Data() << "\" parameters." 
	      << "Probably due to missing  Set it to default " << mCutEle << endm;
  }
  if (TESTBIT(m_Mode, kBICHSEL)) {
    LOG_INFO << "StTpcRSMaker:: use H.Bichsel model for dE/dx simulation" << endm;
    if (! mdNdEL10 || ! mdNdx) {
      const Char_t *path  = ".:./StarDb/dEdxModel:./StarDb/global/dEdx"
	":./StRoot/StBichsel:$STAR/StarDb/dEdxModel:$STAR/StarDb/global/dEdx:$STAR/StRoot/StBichsel";
      const Char_t *Files[2] = {"dNdE_Bichsel.root","dNdx_Bichsel.root"};
      for (Int_t i = 0; i < 2; i++) { // Inner/Outer
	Char_t *file = gSystem->Which(path,Files[i],kReadPermission);
	if (! file) Fatal("StTpcRSMaker::Init","File %s has not been found in path %s",Files[i],path);
	else        Warning("StTpcRSMaker::Init","File %s has been found as %s",Files[i],file);
	TFile       *pFile = new TFile(file);
	if (i == 0) {mdNdEL10 = (TH1D *) pFile->Get("dNdEL10"); assert(mdNdEL10);   mdNdEL10->SetDirectory(0);}
	if (i == 1) {mdNdx = (TH1D *) pFile->Get("dNdx"); assert(mdNdx);   mdNdx->SetDirectory(0);}
	delete pFile;
	delete [] file;
      }
    }
  } else if (TESTBIT(m_Mode, kHEED)) {
    LOG_INFO << "StTpcRSMaker:: use Heed model for dE/dx simulation" << endm;
    if (! mdNdEL10 || ! mdNdxL10) {
      const Char_t *path  = ".:./StarDb/dEdxModel:./StarDb/global/dEdx"
	":./StRoot/StBichsel:$STAR/StarDb/dEdxModel:$STAR/StarDb/global/dEdx:$STAR/StRoot/StBichsel";
      const Char_t *Files[2] = {"dNdx_Heed.root","dNdx_Heed.root"};
      for (Int_t i = 0; i < 2; i++) { // Inner/Outer
	Char_t *file = gSystem->Which(path,Files[i],kReadPermission);
	if (! file) Fatal("StTpcRSMaker::Init","File %s has not been found in path %s",Files[i],path);
	else        Warning("StTpcRSMaker::Init","File %s has been found as %s",Files[i],file);
	TFile       *pFile = new TFile(file);
	if (i == 0) {mdNdEL10 = (TH1D *) pFile->Get("dNdEL10"); assert(mdNdEL10);   mdNdEL10->SetDirectory(0);}
	if (i == 1) {mdNdxL10 = (TH1D *) pFile->Get("dNdxL10"); assert(mdNdxL10);   mdNdxL10->SetDirectory(0);}
	delete pFile;
	delete [] file;
      }
    }
  } else {LOG_INFO << "StTpcRSMaker:: use GEANT321 model for dE/dx simulation" << endm;}
  // Distortions
  if (TESTBIT(m_Mode,kdEdxCorr)) {
    LOG_INFO << "StTpcRSMaker:: use Tpc dE/dx correction from calibaration" << endm;
    Int_t Mask = -1; // 22 bits
    CLRBIT(Mask,StTpcdEdxCorrection::kAdcCorrection);
    CLRBIT(Mask,StTpcdEdxCorrection::kAdcCorrectionMDF);
    CLRBIT(Mask,StTpcdEdxCorrection::kdXCorrection);
    //    CLRBIT(Mask,StTpcdEdxCorrection::kEdge);
    //    CLRBIT(Mask,StTpcdEdxCorrection::kTanL);
    m_TpcdEdxCorrection = new StTpcdEdxCorrection(Mask, Debug());
  }
  if (TESTBIT(m_Mode,kDistortion)) {
    LOG_INFO << "StTpcRSMaker:: use Tpc distortion correction" << endm;
  }
  if (Debug() && gStTpcDb->PadResponse()) gStTpcDb->PadResponse()->Table()->Print(0,1);
  Double_t samplingFrequency     = 1.e6*gStTpcDb->Electronics()->samplingFrequency(); // Hz
  Double_t TimeBinWidth          = 1./samplingFrequency;
  /*
select firstInnerSectorAnodeWire,lastInnerSectorAnodeWire,numInnerSectorAnodeWires,firstOuterSectorAnodeWire,lastOuterSectorAnodeWire,numOuterSectorAnodeWires from  Geometry_tpc.tpcWirePlanes;
+---------------------------+--------------------------+--------------------------+---------------------------+--------------------------+--------------------------+
| firstInnerSectorAnodeWire | lastInnerSectorAnodeWire | numInnerSectorAnodeWires | firstOuterSectorAnodeWire | lastOuterSectorAnodeWire | numOuterSectorAnodeWires |
+---------------------------+--------------------------+--------------------------+---------------------------+--------------------------+--------------------------+
|             53.2000000000 |           120.8000000000 |                      170 |            122.7950000000 |           191.1950000000 |                      172 |
+---------------------------+--------------------------+--------------------------+---------------------------+--------------------------+--------------------------+
   */
  numberOfInnerSectorAnodeWires  = gStTpcDb->WirePlaneGeometry()->numberOfInnerSectorAnodeWires ();
  firstInnerSectorAnodeWire      = gStTpcDb->WirePlaneGeometry()->firstInnerSectorAnodeWire();
  lastInnerSectorAnodeWire       = gStTpcDb->WirePlaneGeometry()->lastInnerSectorAnodeWire ();
  numberOfOuterSectorAnodeWires  = gStTpcDb->WirePlaneGeometry()->numberOfOuterSectorAnodeWires ();
  firstOuterSectorAnodeWire      = gStTpcDb->WirePlaneGeometry()->firstOuterSectorAnodeWire();
  lastOuterSectorAnodeWire       = gStTpcDb->WirePlaneGeometry()->lastOuterSectorAnodeWire ();
  anodeWirePitch                 = gStTpcDb->WirePlaneGeometry()->anodeWirePitch           ();
  anodeWireRadius                = gStTpcDb->WirePlaneGeometry()->anodeWireRadius(); 
  Float_t BFieldG[3]; 
  Float_t xyz[3] = {0,0,0};
  StMagF::Agufld(xyz,BFieldG);
  // Shapers
  Double_t timeBinMin = -0.5;
  Double_t timeBinMax = 44.5;
  const Char_t *Names[2] = {"I","O"};
  Double_t CathodeAnodeGap[2] = {0.2, 0.4};
  for (Int_t sector = 1; sector <= 24; sector++) {
    innerSectorAnodeVoltage[sector-1] = outerSectorAnodeVoltage[sector-1] = 0;
    Int_t nAliveInner = 0;
    Int_t nAliveOuter = 0;
    for (Int_t row = 1; row <= St_tpcPadConfigC::instance()->numberOfRows(sector); row++) {
      if (St_tpcPadConfigC::instance()->IsRowInner(sector,row)) {
	nAliveInner++;
	innerSectorAnodeVoltage[sector-1] += St_tpcAnodeHVavgC::instance()->voltagePadrow(sector,row);
      } else {
	nAliveOuter++;
	outerSectorAnodeVoltage[sector-1] += St_tpcAnodeHVavgC::instance()->voltagePadrow(sector,row);
      }
    }
    if (! nAliveInner && ! nAliveOuter) {
      LOG_INFO << "Illegal date/time. Tpc sector " << sector << " Anode Voltage is not set to run condition: AliveInner: " << nAliveInner 
	       << "\tAliveOuter: " << nAliveOuter 
	       << "\tStop the run" << endm;
      assert(nAliveInner || nAliveOuter);
    } else {
      if (nAliveInner > 1) innerSectorAnodeVoltage[sector-1] /= nAliveInner;
      if (nAliveOuter > 1) outerSectorAnodeVoltage[sector-1] /= nAliveOuter;
    }
    for (Int_t io = 0; io < 2; io++) {// In/Out
      if (io == 0) {
	if (sector > 1 && TMath::Abs(innerSectorAnodeVoltage[sector-1] - innerSectorAnodeVoltage[sector-2]) < 1) {
	  InnerAlphaVariation[sector-1] = InnerAlphaVariation[sector-2];
	} else {
	  LOG_INFO << "Inner Sector " << sector << " ======================" << endm;
	  InnerAlphaVariation[sector-1] = InducedCharge(anodeWirePitch,
							CathodeAnodeGap[io],
							anodeWireRadius,
							innerSectorAnodeVoltage[sector-1], t0IO[io]);
	}
      }
      else {
	if (sector > 1 && TMath::Abs(outerSectorAnodeVoltage[sector-1] - outerSectorAnodeVoltage[sector-2]) < 1) {
	  OuterAlphaVariation[sector-1] = OuterAlphaVariation[sector-2];
	} else {
	  LOG_INFO << "Outer Sector " << sector << " ======================" << endm;
	  OuterAlphaVariation[sector-1] = InducedCharge(anodeWirePitch,
							CathodeAnodeGap[io],
							anodeWireRadius,
							outerSectorAnodeVoltage[sector-1], t0IO[io]);
	}
      }
    }
  }
  for (Int_t io = 0; io < 2; io++) {// In/Out
    //  mPolya = new TF1F("Polya;x = G/G_0;signal","sqrt(x)/exp(1.5*x)",0,10); // original Polya 
    //  mPolya = new TF1F("Polya;x = G/G_0;signal","pow(x,0.38)*exp(-1.38*x)",0,10); //  Valeri Cherniatin
    //   mPoly = new TH1D("Poly","polyaAvalanche",100,0,10);
    Double_t gamma;
    if (!io ) gamma = St_TpcResponseSimulatorC::instance()->PolyaInner();
    else      gamma = St_TpcResponseSimulatorC::instance()->PolyaOuter();
    if (gamma <= 0) gamma = 1.38;
    mPolya[io] = new TF1F(io == 0 ? "PolyaInner;x = G/G_0;signal" : "PolyaOuter;x = G/G_0;signal",polya,0,10,3);
    mPolya[io]->SetParameters(gamma, 0., 1./gamma);
    Double_t params3[7] = {t0IO[io], 
			   St_TpcResponseSimulatorC::instance()->tauF(), 
			   St_TpcResponseSimulatorC::instance()->tauP(), 
			   St_TpcResponseSimulatorC::instance()->tauIntegration(), 
			   TimeBinWidth,     0, (Double_t ) io};
    Double_t params0[5] = {t0IO[io],             St_TpcResponseSimulatorC::instance()->tauX()[io], TimeBinWidth,     0, (Double_t ) io};
    if (! fgTimeShape3[io]) {// old electronics, intergation + shaper alltogether
      fgTimeShape3[io] = new TF1F(Form("TimeShape3%s",Names[io]),
				  shapeEI3,timeBinMin*TimeBinWidth,timeBinMax*TimeBinWidth,7);
      fgTimeShape3[io]->SetParNames("t0","tauF","tauP", "tauI","width","tauC","io");
      fgTimeShape3[io]->SetParameters(params3);
      params3[5] = fgTimeShape3[io]->Integral(timeBinMin*TimeBinWidth,timeBinMax*TimeBinWidth);
      fgTimeShape3[io]->SetTitle(fgTimeShape3[io]->GetName());
      fgTimeShape3[io]->GetXaxis()->SetTitle("time (secs)");
      fgTimeShape3[io]->GetYaxis()->SetTitle("signal");
    }
    if (! fgTimeShape0[io]) {// new electronics only integration
      fgTimeShape0[io] = new TF1F(Form("TimeShape%s",Names[io]),
				  shapeEI,timeBinMin*TimeBinWidth,timeBinMax*TimeBinWidth,5);
      fgTimeShape0[io]->SetParNames("t0","tauI","width","tauC","io");
      params0[3] = St_TpcResponseSimulatorC::instance()->tauC()[io];
      fgTimeShape0[io]->SetParameters(params0);
      params0[3] = fgTimeShape0[io]->Integral(0,timeBinMax*TimeBinWidth);
      fgTimeShape0[io]->SetTitle(fgTimeShape0[io]->GetName());
      fgTimeShape0[io]->GetXaxis()->SetTitle("time (secs)");
      fgTimeShape0[io]->GetYaxis()->SetTitle("signal"); 
    }
    
    for (Int_t sector = 1; sector <= NoOfSectors; sector++) {
      //                             w       h         s      a       l   i
      //  Double_t paramsI[6] = {0.2850, 0.2000,  0.4000, 0.0010, 1.1500, 0};
      //  Double_t paramsO[6] = {0.6200, 0.4000,  0.4000, 0.0010, 1.1500, 0};
      Double_t xmaxP =  4.5;//4.5*St_tpcPadConfigC::instance()->innerSectorPadWidth(sector);// 4.5 
      Double_t xminP = -xmaxP; 
      Double_t params[6];
      if (! io) {
	params[0] = St_tpcPadConfigC::instance()->innerSectorPadWidth(sector);                     // w = width of pad       
	params[1] = gStTpcDb->WirePlaneGeometry()->innerSectorAnodeWirePadPlaneSeparation(); // h = Anode-Cathode gap   
	params[2] = gStTpcDb->WirePlaneGeometry()->anodeWirePitch();                         // s = wire spacing       
	params[3] = St_TpcResponseSimulatorC::instance()->K3IP();
	params[4] = 0;
	params[5] = St_tpcPadConfigC::instance()->innerSectorPadPitch(sector);
      } else {    
	params[0] = St_tpcPadConfigC::instance()->outerSectorPadWidth(sector);                    // w = width of pad       
	params[1] = gStTpcDb->WirePlaneGeometry()->outerSectorAnodeWirePadPlaneSeparation();// h = Anode-Cathode gap   
	params[2] = gStTpcDb->WirePlaneGeometry()->anodeWirePitch();                        // s = wire spacing       
	params[3] = St_TpcResponseSimulatorC::instance()->K3OP();
	params[4] = 0;
	params[5] = St_tpcPadConfigC::instance()->outerSectorPadPitch(sector);
      }
      if (! mPadResponseFunction[io][sector-1]) { 
	mPadResponseFunction[io][sector-1] = new TF1F(io == 0 ? "PadResponseFunctionInner" : "PadResponseFunctionOuter",StTpcRSMaker::PadResponseFunc,xminP,xmaxP,6); 
	mPadResponseFunction[io][sector-1]->SetParameters(params);
	mPadResponseFunction[io][sector-1]->SetParNames("PadWidth","Anode-Cathode gap","wire spacing","K3OP","CrossTalk","PadPitch");
	mPadResponseFunction[io][sector-1]->SetTitle(mPadResponseFunction[io][sector-1]->GetName());
	mPadResponseFunction[io][sector-1]->GetXaxis()->SetTitle("pads");
	mPadResponseFunction[io][sector-1]->GetYaxis()->SetTitle("Signal");
	// Cut tails
	Double_t x = 2.5;//cm   xmaxP;
#if 0
	Double_t ymax = mPadResponseFunction[io][sector-1]->Eval(0);
	for (; x > 1.5; x -= 0.05) {
	  Double_t r = mPadResponseFunction[io][sector-1]->Eval(x)/ymax;
	  if (r > 1e-2) break;
	}
#endif
	mPadResponseFunction[io][sector-1]->SetRange(-x,x);
	mPadResponseFunction[io][sector-1]->Save(xminP,xmaxP,0,0,0,0);
      }
      if (! mChargeFraction[io][sector-1]) {
	xmaxP = 2.5;//5*St_tpcPadConfigC::instance()->innerSectorPadLength(sector); // 1.42
	xminP = - xmaxP;
	mChargeFraction[io][sector-1] = new TF1F(io == 0 ? "ChargeFractionInner" : "ChargeFractionOuter",
						 StTpcRSMaker::PadResponseFunc,xminP,xmaxP,6);
	if (! io) {
	  params[0] = St_tpcPadConfigC::instance()->innerSectorPadLength(sector);
	  params[3] = St_TpcResponseSimulatorC::instance()->K3IR();
	  params[4] = 0;
	  params[5] = 1.; 
	} else {
	  params[0] = St_tpcPadConfigC::instance()->outerSectorPadLength(sector);
	  params[3] = St_TpcResponseSimulatorC::instance()->K3OR(); 
	  params[4] = 0;
	  params[5] = 1.; 
	}
	mChargeFraction[io][sector-1]->SetParameters(params);
	mChargeFraction[io][sector-1]->SetParNames("PadLength","Anode-Cathode gap","wire spacing","K3IR","CrossTalk","RowPitch");
	mChargeFraction[io][sector-1]->SetTitle(mChargeFraction[io][sector-1]->GetName());
	mChargeFraction[io][sector-1]->GetXaxis()->SetTitle("Distance (cm)");
	mChargeFraction[io][sector-1]->GetYaxis()->SetTitle("Signal");
	// Cut tails
	Double_t x = xmaxP;
	Double_t ymax = mChargeFraction[io][sector-1]->Eval(0);
	for (; x > 1.5; x -= 0.05) {
	  Double_t r = mChargeFraction[io][sector-1]->Eval(x)/ymax;
	  if (r > 1e-2) break;
	}
	mChargeFraction[io][sector-1]->SetRange(-x,x);
	mChargeFraction[io][sector-1]->Save(xminP,xmaxP,0,0,0,0);
      }
#if 0
      memset(mLocalYDirectionCoupling[io][sector-1], 0, sizeof(mLocalYDirectionCoupling[io][sector-1]));
      for (Int_t j = 0; j < 7; j++) {
	mLocalYDirectionCoupling[io][sector-1][j] = mChargeFraction[io][sector-1]->Eval(anodeWirePitch*j);
      }
#endif
      //  TF1F *func = new TF1F("funcP","x*sqrt(x)/exp(2.5*x)",0,10);
      // see http://www4.rcf.bnl.gov/~lebedev/tec/polya.html
      // Gain fluctuation in proportional counters follows Polya distribution. 
      // x = G/G_0
      // P(m) = m(m(x)**(m-1)*exp(-m*x)/Gamma(m);
      // original Polya  m = 1.5 (R.Bellazzini and M.A.Spezziga, INFN PI/AE-94/02). 
      // Valeri Cherniatin (cherniat@bnlarm.bnl.gov) recomends m=1.38
      // Trs uses x**1.5/exp(x)
      // tss used x**0.5/exp(1.5*x)
      if (St_tpcAltroParamsC::instance()->N(sector-1) < 0) {// old TPC
	// check if the function has been created
	for (Int_t sec = 1; sec < sector; sec++) {	 
	  if (St_tpcAltroParamsC::instance()->N(sec-1) < 0 && mShaperResponses[io][sec-1]) {
	    mShaperResponses[io][sector-1] = mShaperResponses[io][sec-1];
	    break;
	  }
	}
	if (! mShaperResponses[io][sector-1]) {
	  mShaperResponses[io][sector-1] = new TF1F(Form("ShaperFunc_%s_S%02i",Names[io],sector), 
						    StTpcRSMaker::shapeEI3_I,timeBinMin,timeBinMax,7);  
	  mShaperResponses[io][sector-1]->SetParameters(params3);
	  mShaperResponses[io][sector-1]->SetParNames("t0","tauF","tauP", "tauI", "width","norm","io");
	  mShaperResponses[io][sector-1]->SetTitle(mShaperResponses[io][sector-1]->GetName());
	  mShaperResponses[io][sector-1]->GetXaxis()->SetTitle("time (buckets)");
	  mShaperResponses[io][sector-1]->GetYaxis()->SetTitle("signal");
	  // Cut tails
	  Double_t t = timeBinMax;
	  Double_t ymax = mShaperResponses[io][sector-1]->Eval(0.5);
	  for (; t > 5; t -= 1) {
	    Double_t r = mShaperResponses[io][sector-1]->Eval(t)/ymax;
	    if (r > 1e-2) break;
	  }
	  mShaperResponses[io][sector-1]->SetRange(timeBinMin,t);
	  mShaperResponses[io][sector-1]->Save(timeBinMin,t,0,0,0,0);
	}
	continue;
      } 
      //Altro 
      for (Int_t sec = 1; sec < sector; sec++) {
	if (St_tpcAltroParamsC::instance()->N(sec-1) >= 0 && mShaperResponses[io][sec-1] ) {
	  mShaperResponses[io][sector-1] = mShaperResponses[io][sec-1];
	  break;
	}
      }
      if (! mShaperResponses[io][sector-1]) {
	mShaperResponses[io][sector-1] = new TF1F(Form("ShaperFunc_%s_S%02i",Names[io],sector),
						  StTpcRSMaker::shapeEI_I,timeBinMin,timeBinMax,5);  
	mShaperResponses[io][sector-1]->SetParameters(params0);
	mShaperResponses[io][sector-1]->SetParNames("t0","tauI", "width","norm","io");
	mShaperResponses[io][sector-1]->SetTitle(mShaperResponses[io][sector-1]->GetName());
	mShaperResponses[io][sector-1]->GetXaxis()->SetTitle("time (buckets)");
	mShaperResponses[io][sector-1]->GetYaxis()->SetTitle("signal");
	// Cut tails
	Double_t t = timeBinMax;
	Double_t ymax = mShaperResponses[io][sector-1]->Eval(0.5);
	for (; t > 5; t -= 1) {
	  Double_t r = mShaperResponses[io][sector-1]->Eval(t)/ymax;
	  if (r > 1e-2) break;
	}
	mShaperResponses[io][sector-1]->SetRange(timeBinMin,t);
	mShaperResponses[io][sector-1]->Save(timeBinMin,t,0,0,0,0);
      }
    }
  }
  // tss
  mGG = new TF1F("GaitingGridTransperency","TMath::Max(0.,1-6.27594134307865925e+00*TMath::Exp(-2.87987e-01*(x-1.46222e+01)))",10,56);
  if (Debug()) Print();
  memset (hist, 0, sizeof(hist));
  memset (checkList, 0, sizeof(checkList));
#ifdef __ClusterProfile__
  if (GetTFile()) {
    GetTFile()->cd();
    ClusterProfile = kTRUE;
  }
#endif /* __ClusterProfile__ */
#if 0
  StMagUtilities::SetDoDistortionT(gFile);
  StMagUtilities::SetUnDoDistortionT(gFile);
#endif
  mHeed = fEc(St_TpcResponseSimulatorC::instance()->W());
  if ( ! ClusterProfile) {
    return kStOK;
  }
  Int_t color = 1;
  struct Name_t {
    const Char_t *Name;
    const Char_t *Title;
  };
  const Name_t InOut[6] = {
    {"Inner","Inner old electronics or iTPC"},
    {"Outer","Outer old electronics or ITPC"},
    {"InnerX","Inner new electronics"},
    {"OuterX","Outer new electronics"},
    {"I","Inner"},
    {"O","Outer"}
  };
  const Name_t PadTime[3] = {
    {"Pad","Pad"},
    {"Time","Time"},
    {"Row","Row"},
  };
  for (Int_t io = 2; io < 4; io++) {
    for (Int_t pt = 0; pt < 2; pt++) {
      TString Name(InOut[io].Name); Name += PadTime[pt].Name; Name += "Mc";
      TString Title(InOut[io].Title); Title += PadTime[pt].Title; Title += "Mc";
      hist[io][pt] = (TProfile2D *) gDirectory->Get(Name);
      if (! hist[io][pt]) {
	hist[io][pt] = new TProfile2D(Name,Title,nx[pt],xmin[pt],xmax[pt],nz,zmin,zmax,""); 
	hist[io][pt]->SetMarkerStyle(20);
	hist[io][pt]->SetMarkerColor(color++);
      }
    }
  }
  hist[4][0] = new TProfile2D("dEdxCorSecRow","dEdx correction versus sector and row",
			      NoOfSectors,0.5,NoOfSectors+0.5,
			      St_tpcPadConfigC::instance()->numberOfRows(20),0.5,St_tpcPadConfigC::instance()->numberOfRows(20)+0.5,""); 
  hist[4][1] = new TProfile2D("GainSecRow","Overall gain versus sector and row",
			      NoOfSectors,0.5,NoOfSectors+0.5,
			      St_tpcPadConfigC::instance()->numberOfRows(20),0.5,St_tpcPadConfigC::instance()->numberOfRows(20)+0.5,""); 
  const Name_t Checks[21] = {
    {"dEGeant","dE in Geant"}, // 0
    {"dSGeant","ds in Geant"}, // 1
    {"Gain","Gas Gain after Voltage"}, // 2
    {"GainMc","Gas Gain after MC correction"}, // 3
    {"dEdxCor","correction of dEdx"}, // 4
    {"lgam","lgam"}, // 5
    {"NPGEANT","no. of primary electros from GEANT"}, // 6
    {"NP","no. of primary electros"}, // 7
    {"Nt","total no. of electors per cluster"}, // 8
    {"Qav","Gas gain flactuations"}, // 9
    {"localYDirectionCoupling","localYDirectionCoupling"}, //10
    {"n0","No. electrons per primary interaction"}, //11
    {"padGain","padGain"}, // 12
    {"localXDirectionCoupling","localXDirectionCoupling"}, // 13
    {"XYcoupling","XYcoupling"}, //14 
    {"dE","dE"}, // 15
    {"dS","dS"}, // 16
    {"adc","adc"},// 17
    {"NE","Total no. of generated electors"}, // 18
    {"dECl","Total log(signal/Nt) in a cluster versus Wire Index"}, // 19
    {"nPdT","log(Total no. of conducting electrons) - log(no. of primary one) versus log(no. primary electrons)"} // 20 
  };
  const Int_t Npbins  = 151;
  const Int_t NpbinsL =  10;
  const Double_t Xmax = 1e5;
  Double_t    dX = TMath::Log(Xmax/10)/(Npbins - NpbinsL);
  Double_t *pbins = new Double_t[Npbins];
  Double_t *pbinsL =  new Double_t[Npbins];
  pbins[0] = 0.5;
  pbinsL[0] = TMath::Log(pbins[0]);
  for (Int_t bin = 1; bin < Npbins; bin++) {
    if (bin <= NpbinsL) {
      pbins[bin] = pbins[bin-1] + 1;
    } else if (bin == Npbins - 1) {
      pbins[bin] = 1e5;
    } else {
      Int_t nM = 0.5*(pbins[NpbinsL-2] + pbins[NpbinsL-1])*TMath::Exp(dX*(bin-NpbinsL)); 
      Double_t dbin = TMath::Nint(nM - pbins[bin-1]);
      if (dbin < 1.0) dbin = 1.0;
      pbins[bin] = pbins[bin-1] + dbin;
    }
    pbinsL[bin] = TMath::Log(pbins[bin]);
  }
  for (Int_t io = 0; io < 2; io++) {
    for (Int_t i = 0; i < nChecks; i++) {
      TString Name(Checks[i].Name); Name += InOut[4+io].Name;
      TString Title(Checks[i].Title); Title += InOut[4+io].Title;
      if      (i == 11) checkList[io][i] = new TH2D(Name,Title,nz,zmin,zmax,100,-0.5,99.5); 
      else if (i == 19) checkList[io][i] = new TH2D(Name,Title,173,-.5,172.5,200,-10,10);
      //      else if (i == 20) checkList[io][i] = new TH2D(Name,Title,Npbins-1,pbinsL,Npbins-1,pbinsL);
      else if (i == 20) checkList[io][i] = new TH2D(Name,Title,Npbins-1,pbinsL,500,-2.0,8.0);
      else              checkList[io][i] = new TProfile(Name,Title,nz,zmin,zmax,"");  
    }
#ifdef __LASERINO__
    SecRow[0] = new TProfile2D("SecRowdE","Simu <dE> versus sector row",24,0.5,24.5,72,0.5,72.5);
    SecRow[1] = new TProfile2D("SecRowdS","Simu <dS> versus sector row",24,0.5,24.5,72,0.5,72.5);
    SecRow[2] = new TProfile2D("SecRowGain","<Gain> versus sector row",24,0.5,24.5,72,0.5,72.5);
    SecRow[3] = new TProfile2D("SecRowGainC","<GainCorrected> versus sector row",24,0.5,24.5,72,0.5,72.5);
    SecRow[4] = new TProfile2D("SecRowdEdxC","<dEdxC> versus sector row",24,0.5,24.5,72,0.5,72.5);
    SecRow[5] = new TProfile2D("SecRowNP","<NP> versus sector row",24,0.5,24.5,72,0.5,72.5);
    SecRow[6] = new TProfile2D("SecRowNt","<Nt> versus sector row",24,0.5,24.5,72,0.5,72.5);
    SecRow[7] = new TProfile2D("SecRowQAv","<QAv> versus sector row",24,0.5,24.5,72,0.5,72.5);
    SecRow[8] = new TProfile2D("SecRowgain","<gain> versus sector row",24,0.5,24.5,72,0.5,72.5);
    SecRow[9] = new TProfile2D("SecRowTotSigCl","<TotalSignalInCluster> versus sector row",24,0.5,24.5,72,0.5,72.5);
    SecRow[10] = new TProfile2D("SecRowADC","<ADC> versus sector row",24,0.5,24.5,72,0.5,72.5);
    SecRow[11] = new TProfile2D("SecRowADCAltro","<ADCAltro> versus sector row",24,0.5,24.5,72,0.5,72.5);
    SecRow[12] = new TProfile2D("SecRowRange","<row range> versus sector row",24,0.5,24.5,72,0.5,72.5);
    SecRow[13] = new TProfile2D("SecRowdY","<dY> versus sector row",24,0.5,24.5,72,0.5,72.5);
    SecRow[14] = new TProfile2D("SecRowChargeFraction","<ChargeFraction> versus sector row",24,0.5,24.5,72,0.5,72.5);
#endif /* __LASERINO__ */
  }
  delete [] pbins;
  delete [] pbinsL;
  SetAttr("minSector",1);
  SetAttr("maxSector",24);
  SetAttr("minRow",1);
  SetAttr("maxRow",St_tpcPadConfigC::instance()->numberOfRows(20));
  return kStOK;
}
//________________________________________________________________________________
Int_t StTpcRSMaker::Make(){  //  PrintInfo();
  static Int_t minSector = IAttr("minSector");
  static Int_t maxSector = IAttr("maxSector");
#if 0
  static Int_t minRow    = IAttr("minRow");
  static Int_t maxRow    = IAttr("maxRow");
#endif
  // constants
  static Int_t iBreak = 0;
#ifdef __DEBUG__
  if (Debug()%10) {
    gBenchmark->Reset();
    gBenchmark->Start("TpcRS");
    LOG_INFO << "\n -- Begin TpcRS Processing -- \n";
  }
#endif
  Double_t vminI = St_tpcGainCorrectionC::instance()->Struct(1)->min;
  Double_t vminO = St_tpcGainCorrectionC::instance()->Struct(0)->min;
  St_g2t_tpc_hit *g2t_tpc_hit = (St_g2t_tpc_hit *) GetDataSet("geant/g2t_tpc_hit");
  if (!g2t_tpc_hit) return kStWarn;
  Int_t no_tpc_hits       = g2t_tpc_hit->GetNRows();               if (no_tpc_hits<1) return kStOK;
  if (Debug() > 1) g2t_tpc_hit->Print(0,10);
  St_g2t_track *g2t_track = (St_g2t_track *) GetDataSet("geant/g2t_track"); //  if (!g2t_track)    return kStWarn;
  Int_t NoTpcTracks = 0; 
  if (g2t_track) NoTpcTracks = g2t_track->GetNRows();
  mNoTpcHitsAll = TArrayI(NoTpcTracks+1);
  mNoTpcHitsReal = TArrayI(NoTpcTracks+1);
  g2t_track_st *tpc_track = 0;
  if (g2t_track) tpc_track = g2t_track->GetTable();
  St_g2t_vertex  *g2t_ver = (St_g2t_vertex *) GetDataSet("geant/g2t_vertex");// if (!g2t_ver)      return kStWarn;
  g2t_vertex_st     *gver = 0;
  Int_t NV = 0;
  if (g2t_ver) {
    gver = g2t_ver->GetTable();
    NV = g2t_ver->GetNRows();
  }
  g2t_tpc_hit_st *tpc_hit_begin = g2t_tpc_hit->GetTable();
  g2t_tpc_hit_st *tpc_hit = tpc_hit_begin;
  if (m_TpcdEdxCorrection) {
    St_tpcGainCorrectionC::instance()->Struct(0)->min = -500;
    St_tpcGainCorrectionC::instance()->Struct(1)->min = -500;
    if (Debug()) {
      LOG_INFO << "Reset min for gain Correction to I/O\t" 
	       << St_tpcGainCorrectionC::instance()->Struct(1)->min 
	       << "\t" 
	       << St_tpcGainCorrectionC::instance()->Struct(0)->min 
	       << " (V)" << endm;
    }
  }
  mNSplittedHits = 0;
  // sort 
  TTableSorter sorter(g2t_tpc_hit,&SearchT,&CompareT);//, 0, no_tpc_hits);
  Int_t sortedIndex = 0;
  tpc_hit = tpc_hit_begin;
  for (Int_t sector = minSector; sector <= maxSector; sector++) {
    Int_t NoHitsInTheSector = 0;
    free(m_SignalSum); m_SignalSum = 0;
    ResetSignalSum(sector);
    // it is assumed that hit are ordered by sector, trackId, pad rows, and track length
    for (; sortedIndex < no_tpc_hits; sortedIndex++) {
      Int_t indx = sorter.GetIndex(sortedIndex);
      if (indx < 0) break;
      tpc_hit = tpc_hit_begin + indx;
      Int_t volId = tpc_hit->volume_id%10000;
      Int_t iSector = volId/100;
      if (iSector != sector) {
	if (! ( iSector > sector ) ) {
	  LOG_ERROR << "StTpcRSMaker::Make: g2t_tpc_hit table has not been ordered by sector no. " << sector << endm;
	  g2t_tpc_hit->Print(indx,1);
	  assert( iSector > sector );
	}
	break;
      }
      if (tpc_hit->volume_id <= 0 || tpc_hit->volume_id > 1000000) continue;
      Int_t Id  = tpc_hit->track_p;
      Int_t id3 = 0, ipart = 8, charge = 1;
      Double_t mass = 0;
      if (tpc_track) {
	id3        = tpc_track[Id-1].start_vertex_p;
	assert(id3 > 0 && id3 <= NV);
	ipart      = tpc_track[Id-1].ge_pid;
	charge     = (Int_t) tpc_track[Id-1].charge;
	StParticleDefinition *particle = StParticleTable::instance()->findParticleByGeantId(ipart);
	if (particle) {
	  mass = particle->mass();
	  charge = particle->charge();
	}
#if 0
	if (tpc_track[Id-1].next_parent_p && ipart == 3) { // delta electrons ?
	  Id = tpc_track[Id-1].next_parent_p;
	  ipart      = tpc_track[Id-1].ge_pid;
	}
#endif
	
      }
      if (ipart == Laserino || ipart == Chasrino) {
	charge = 0;
      } else {
	if (ipart == 1) {// gamma => electron
	  ipart = 3;
	  charge = -1;
	}
	if (charge == 0) {
	  continue;
	}
      } // special treatment for electron/positron 
      if (ipart == 2) charge =  101;
      if (ipart == 3) charge = -101;
      // Track segment to propagate
      enum {NoMaxTrackSegmentHits = 100};
      static HitPoint_t TrackSegmentHits[NoMaxTrackSegmentHits];
      msMin = 9999;
      msMax = -9999;
      Int_t nSegHits = 0;
      Int_t sIndex = sortedIndex;
      if (Debug() > 13) cout << "sortedIndex = " << sortedIndex << "\tno_tpc_hits = " << no_tpc_hits << endl;
      Int_t ID = 0;
      Int_t TrackDirection = 0; // 0 - increase no of row, 1 - decrease no of. row.
      for (nSegHits = 0, sIndex = sortedIndex;  
	   sIndex < no_tpc_hits && nSegHits < NoMaxTrackSegmentHits; sIndex++) {
	indx = sorter.GetIndex(sIndex);
	g2t_tpc_hit_st *tpc_hitC = tpc_hit_begin + indx;
	if ((tpc_hitC->volume_id%10000)/100 != sector) break;
	if (ID > 0 && ID != tpc_hitC->track_p) break;
	ID = tpc_hitC->track_p;
	if (nSegHits == 1) { // No Loopers !
	  if (TrackSegmentHits[nSegHits-1].tpc_hitC->volume_id%100 <= tpc_hitC->volume_id%100) {
	    TrackDirection = 0;
	  } else {
	    TrackDirection = 1;
	  }
	} else if (nSegHits > 1) {
	  if ((! TrackDirection && TrackSegmentHits[nSegHits-1].tpc_hitC->volume_id%100 > tpc_hitC->volume_id%100) ||
	      (  TrackDirection && TrackSegmentHits[nSegHits-1].tpc_hitC->volume_id%100 < tpc_hitC->volume_id%100))
	    break;
	}
	if (Debug() > 13) cout << "sIndex = " << sIndex << "\tindx = " << indx << "\ttpc_hitC = " << tpc_hitC << endl;
	TrackSegmentHits[nSegHits].indx = indx;
	TrackSegmentHits[nSegHits].s = tpc_hitC->length;
	if (tpc_hitC->length == 0 && nSegHits > 0) {
	  TrackSegmentHits[nSegHits].s = TrackSegmentHits[nSegHits-1].s + TrackSegmentHits[nSegHits].tpc_hitC->ds;
	}
	TrackSegment2Propagate(tpc_hitC, &gver[id3-1],TrackSegmentHits[nSegHits]);
 	if (TrackSegmentHits[nSegHits].Pad.timeBucket() < 0 || TrackSegmentHits[nSegHits].Pad.timeBucket() > NoOfTimeBins) continue;
	nSegHits++;
      }
      if (! nSegHits) continue;
      if (Debug() >= 10) {
	PrPP(Make,nSegHits);
	for (Int_t s = 0; s < nSegHits; s++) {
	  cout << "Seg[" << Form("%2i",s) << "]\tId " << TrackSegmentHits[s].TrackId << "\ts = " << TrackSegmentHits[s].s 
	       << "\tvolumeID :" <<  Form("%6i",TrackSegmentHits[s].tpc_hitC->volume_id) <<"\t" << TrackSegmentHits[s].Pad 
	       << "\ts1/s2 = " << TrackSegmentHits[s].tpc_hitC->length - TrackSegmentHits[s].tpc_hitC->ds/2 
	       << "\t" << TrackSegmentHits[s].tpc_hitC->length + TrackSegmentHits[s].tpc_hitC->ds/2 << "\tds = " << TrackSegmentHits[s].tpc_hitC->ds
	       << endl;
	}
      }
      sortedIndex = sIndex-1; // Irakli 05/06/19, reduce extra step in for loop
      Double_t s = msMin;
      memset (rowsdE, 0, sizeof(rowsdE));
      for (Int_t iSegHits = 0; iSegHits < nSegHits && s < msMax; iSegHits++) {
	memset (rowsdEH, 0, sizeof(rowsdEH));
	g2t_tpc_hit_st *tpc_hitC = TrackSegmentHits[iSegHits].tpc_hitC;
	tpc_hitC->adc = 0;
	volId = tpc_hitC->volume_id%100000;
	Int_t row = TrackSegmentHits[iSegHits].coorLS.fromRow();
	Int_t io = (row <= St_tpcPadConfigC::instance()->numberOfInnerRows(sector)) ? 0 : 1;
	// switch between Inner / Outer Sector paramters
	// Extra correction for simulation with respect to data
	Int_t iowe = 0;
	if (sector  > 12) iowe += 4;
	if (io) iowe += 2;
	Float_t  *AdditionalMcCorrection = St_TpcResponseSimulatorC::instance()->SecRowCor();
	Float_t  *AddSigmaMcCorrection   = St_TpcResponseSimulatorC::instance()->SecRowSig();
	// Generate signal 
	Double_t sigmaJitterT     = St_TpcResponseSimulatorC::instance()->SigmaJitterTI();
	Double_t sigmaJitterX     = St_TpcResponseSimulatorC::instance()->SigmaJitterXI();
	if(io) { // Outer
	  sigmaJitterT            = St_TpcResponseSimulatorC::instance()->SigmaJitterTO();
	  sigmaJitterX            = St_TpcResponseSimulatorC::instance()->SigmaJitterXO();
	}
	// Generate signal 
	Double_t Gain = St_tss_tssparC::instance()->gain(sector,row); 
	mShaperResponse = mShaperResponses[io][sector-1];
	if (ClusterProfile) {
	  checkList[io][2]->Fill(TrackSegmentHits[iSegHits].xyzG.position().z(),Gain);
#ifdef __LASERINO__
	  SecRow[2]->Fill(sector,row,Gain);
#endif /* __LASERINO__ */
	}
	Double_t GainXCorrectionL = AdditionalMcCorrection[iowe] + row*AdditionalMcCorrection[iowe+1];
	Gain *= TMath::Exp(-GainXCorrectionL);
	Double_t GainXSigma = AddSigmaMcCorrection[iowe] + row*AddSigmaMcCorrection[iowe+1];
	if (GainXSigma > 0) Gain *= TMath::Exp(gRandom->Gaus(0.,GainXSigma));
	if (ClusterProfile) {
	  checkList[io][3]->Fill(TrackSegmentHits[iSegHits].xyzG.position().z(),Gain);
#ifdef __LASERINO__
	  SecRow[3]->Fill(sector,row,Gain);
#endif /* __LASERINO__ */
	}
	// dE/dx correction
	Double_t dEdxCor = dEdxCorrection(TrackSegmentHits[iSegHits]);
#ifdef __DEBUG__
	if (TMath::IsNaN(dEdxCor)) {
	  iBreak++;
	}
#endif
	if (dEdxCor <= 0.) continue;
	if (ClusterProfile) {
	  checkList[io][4]->Fill(TrackSegmentHits[iSegHits].xyzG.position().z(),dEdxCor);
	  hist[4][0]->Fill(TrackSegmentHits[iSegHits].Pad.sector(),TrackSegmentHits[iSegHits].Pad.row(),dEdxCor);
#ifdef __LASERINO__
	  SecRow[4]->Fill(sector,row,dEdxCor);
#endif /* __LASERINO__ */
	}
	// Apply Gating Grid
	if (TrackSegmentHits[iSegHits].Pad.timeBucket() > mGG->GetXmin() && 
	    TrackSegmentHits[iSegHits].Pad.timeBucket() < mGG->GetXmax()) {
	  dEdxCor *= mGG->Eval(TrackSegmentHits[iSegHits].Pad.timeBucket());
	}
	if (dEdxCor < minSignal) continue;
	// Initialize propagation
	Float_t BField[3] = {(Float_t ) TrackSegmentHits[iSegHits].BLS.position().x(), 
			     (Float_t ) TrackSegmentHits[iSegHits].BLS.position().y(), 
			     (Float_t ) TrackSegmentHits[iSegHits].BLS.position().z()};
	StPhysicalHelixD track(TrackSegmentHits[iSegHits].dirLS.position(),
			       TrackSegmentHits[iSegHits].coorLS.position(),
			       BField[2]*kilogauss*charge,1);  
	StThreeVectorD unit = TrackSegmentHits[iSegHits].dirLS.position().unit();
	Double_t *cxyz = unit.xyz();
	TRMatrix L2L(3,3, 
		     cxyz[2], - cxyz[0]*cxyz[2]                  , cxyz[0],
		     cxyz[0], - cxyz[1]*cxyz[2]                  , cxyz[1],
		     0.0    ,   cxyz[0]*cxyz[0] + cxyz[1]*cxyz[1], cxyz[2]);
#ifdef __DEBUG__
	if (Debug() > 11) PrPP(Make,track);
#endif	
	Double_t dStep =  TMath::Abs(tpc_hitC->ds);
	Double_t s_low   = -dStep/2;
	Double_t s_upper = s_low + dStep;
	Double_t newPosition = s_low;
	static StThreeVectorD normal(0,1,0);
	static StTpcCoordinateTransform transform(gStTpcDb);
	StThreeVectorD rowPlane(0,transform.yFromRow(TrackSegmentHits[iSegHits].Pad.sector(),TrackSegmentHits[iSegHits].Pad.row()),0);
	Double_t sR = track.pathLength(rowPlane,normal);
	if (sR < 1e10) {
	  PrPP(Maker,sR);
	  PrPP(Make,TrackSegmentHits[iSegHits].coorLS); 
	  StThreeVectorD xyzP = track.at(sR);
	  TrackSegmentHits[iSegHits].coorLS.setPosition(xyzP); PrPP(Make,TrackSegmentHits[iSegHits].coorLS);
	  // don't useT0, don't useTau
	  PrPP(Make,TrackSegmentHits[iSegHits].Pad);
	  transform(TrackSegmentHits[iSegHits].coorLS,TrackSegmentHits[iSegHits].Pad,kFALSE,kFALSE); // don't use T0, don't use Tau
	  PrPP(Make,TrackSegmentHits[iSegHits].Pad); 
	}
	Int_t ioH = io;
	if (St_tpcAltroParamsC::instance()->N(sector-1) >= 0) ioH += 2;
	TotalSignal  = 0;
	Double_t lgam = tpc_hitC->lgam;
	if (ClusterProfile) {
	  checkList[io][5]->Fill(TrackSegmentHits[iSegHits].xyzG.position().z(),lgam);
	}
	Double_t gamma = TMath::Power(10.,lgam) + 1;
	Double_t betaGamma = TMath::Sqrt(gamma*gamma - 1.);
	StThreeVectorD       pxyzG(tpc_hitC->p[0],tpc_hitC->p[1],tpc_hitC->p[2]);
	Double_t bg = 0;
	static const Double_t m_e = .51099907e-3;
	Double_t eKin = -1;
#ifdef __STOPPED_ELECTRONS__
	if (mass > 0) {
	  bg = pxyzG.mag()/mass;
	  // special case stopped electrons
          if (tpc_hitC->ds < 0.0050 && tpc_hitC->de < 0) {
	    Int_t Id         = tpc_hitC->track_p;
	    Int_t ipart      = tpc_track[Id-1].ge_pid;
	    if (ipart == 3) {
	      eKin = -tpc_hitC->de;
	      gamma = eKin/m_e + 1;
              bg = TMath::Sqrt(gamma*gamma - 1.);
	    }
	  }
	}
#else /* ! __STOPPED_ELECTRONS__ */
	if (mass > 0) bg = pxyzG.mag()/mass;
#endif /* __STOPPED_ELECTRONS__ */
	if (bg > betaGamma) betaGamma = bg;
	Double_t bg2 = betaGamma*betaGamma;
	gamma = TMath::Sqrt(bg2 + 1.);
	Double_t Tmax; 
	if (mass < 2*m_e) {
	  if (charge > 0) Tmax =     m_e*(gamma - 1);
	  else            Tmax = 0.5*m_e*(gamma - 1);
	} else {
	  Double_t r = m_e/mass;
	  Tmax = 2*m_e*bg2/(1 + 2*gamma*r + r*r); 
	}
	if (Tmax > mCutEle) Tmax = mCutEle;
	Double_t padH = TrackSegmentHits[iSegHits].Pad.pad();        
	Double_t tbkH = TrackSegmentHits[iSegHits].Pad.timeBucket(); 
	tpc_hitC->pad = padH;
	tpc_hitC->timebucket = tbkH;
        pad0 = TMath::Nint(padH + xmin[0]);
	tbk0 = TMath::Nint(tbkH + xmin[1]);
	Double_t OmegaTau =St_TpcResponseSimulatorC::instance()->OmegaTau()*
	  TrackSegmentHits[iSegHits].BLS.position().z()/5.0;// from diffusion 586 um / 106 um at B = 0/ 5kG
	Double_t NP = TMath::Abs(tpc_hitC->de)/(St_TpcResponseSimulatorC::instance()->W()*eV*
						St_TpcResponseSimulatorC::instance()->Cluster()); // from GEANT
	if (ClusterProfile) {
	  checkList[io][6]->Fill(TrackSegmentHits[iSegHits].xyzG.position().z(),NP);
#ifdef __LASERINO__
	  SecRow[5]->Fill(sector,row,NP);
#endif /* __LASERINO__ */
	}
	Double_t driftLength = TMath::Abs(TrackSegmentHits[iSegHits].coorLS.position().z());
	Double_t D = 1. + OmegaTau*OmegaTau;
	Double_t SigmaT = St_TpcResponseSimulatorC::instance()->transverseDiffusion()*  TMath::Sqrt(   driftLength/D);
	//	Double_t SigmaL = St_TpcResponseSimulatorC::instance()->longitudinalDiffusion()*TMath::Sqrt(2*driftLength  );
	if (sigmaJitterX > 0) {SigmaT = TMath::Sqrt(SigmaT*SigmaT + sigmaJitterX*sigmaJitterX);}
	Double_t SigmaL = St_TpcResponseSimulatorC::instance()->longitudinalDiffusion()*TMath::Sqrt(   driftLength  );
	Double_t NoElPerAdc = St_TpcResponseSimulatorC::instance()->NoElPerAdc();
	if (NoElPerAdc <= 0) {
	  if (St_tpcPadConfigC::instance()->iTPC(sector) && St_tpcPadConfigC::instance()->IsRowInner(sector,row)) {
	    NoElPerAdc = St_TpcResponseSimulatorC::instance()->NoElPerAdcX(); // iTPC
	  } else if (St_tpcPadConfigC::instance()->IsRowInner(sector,row)) {
	    NoElPerAdc = St_TpcResponseSimulatorC::instance()->NoElPerAdcI(); // inner TPX
	  } else {
	    NoElPerAdc = St_TpcResponseSimulatorC::instance()->NoElPerAdcO(); // outer TPX
	  }
	}
#ifndef __NO_1STROWCORRECTION__
	if (row == 1) dEdxCor *= TMath::Exp(St_TpcResponseSimulatorC::instance()->FirstRowC());
#endif /* __NO_1STROWCORRECTION__ */
	mGainLocal = Gain/dEdxCor/NoElPerAdc; // Account dE/dx calibration
	// end of dE/dx correction
	// generate electrons: No. of primary clusters per cm
#ifdef __LASERINO__
	NP = 100;
#else /* !  __LASERINO__ */
	if (mdNdx || mdNdxL10) {
	  NP = GetNoPrimaryClusters(betaGamma,charge); // per cm
#ifdef __DEBUG__
	  if (NP <= 0.0) {
	    iBreak++; continue;
	  }
#endif
	}
#endif /* __LASERINO__ */
	if (ClusterProfile) {
	  checkList[io][7]->Fill(TrackSegmentHits[iSegHits].xyzG.position().z(),NP);
	}
	Int_t nP = 0; 
	Double_t dESum = 0;
	Double_t dSSum = 0;
	Int_t   nTotal = 0;
	memset (padsdE, 0, sizeof(padsdE));
	memset (tbksdE,  0, sizeof(tbksdE));
	Float_t dEr = 0;
	TArrayF rs(10); 
	
	do {// Clusters
	  Float_t dS = 0;
	  Float_t dE = 0;
#ifdef __LASERINO__
	  dE = 25; // eV
	  dS = 1./NP;
#else /* !  __LASERINO__ */
	  static Double_t cLog10 = TMath::Log(10.);
	  if (eKin >= 0.0) {
	    if (eKin == 0.0) break;
	    gamma = eKin/m_e + 1;
	    bg = TMath::Sqrt(gamma*gamma - 1.);
	    Tmax = 0.5*m_e*(gamma - 1);
	    if (Tmax <= St_TpcResponseSimulatorC::instance()->W()/2*eV) break;
	    NP = GetNoPrimaryClusters(betaGamma,charge); 
	    dE = TMath::Exp(cLog10*mdNdEL10->GetRandom());
	  } else {
	    if (charge) {
	      dS = - TMath::Log(gRandom->Rndm())/NP;
	      if (mdNdEL10) dE = TMath::Exp(cLog10*mdNdEL10->GetRandom());
	      else          dE = St_TpcResponseSimulatorC::instance()->W()*
			      gRandom->Poisson(St_TpcResponseSimulatorC::instance()->Cluster());
	    }
	    else { // charge == 0 geantino
	      // for Laserino assume dE/dx = 25 keV/cm;
	      dE = 10; // eV
	      dS = dE*eV/(TMath::Abs(mLaserScale*tpc_hitC->de/tpc_hitC->ds));
	    }	  
	  }
#endif /* __LASERINO__ */
#ifdef __DEBUG__
	  if (Debug() > 12) { 	
	    LOG_INFO << "s_low/s_upper/dSD\t" << s_low << "/\t" << s_upper << "\t" << dS <<  endm;
	  }
#endif
	  Double_t E = dE*eV;
	  newPosition += dS;
	  if (newPosition > s_upper) break;
	  if (dE < St_TpcResponseSimulatorC::instance()->W()/2 || E > Tmax) continue;
	  if (eKin > 0) {
	    if (eKin >= E) {eKin -= E;}
            else {E = eKin; eKin = 0; dE = E/eV;}
	  }
	  dESum += dE;
	  dSSum += dS;
	  nP++;
#ifdef __DEBUG__
	  if (Debug() > 12) {
	    LOG_INFO << "dESum = " << dESum << " /\tdSSum " << dSSum << " /\t newPosition " << newPosition << endm;
	  }
#endif
	  Double_t xRange = 0;
	  if (dE > ElectronRangeEnergy) xRange = ElectronRange*TMath::Power((dE+dEr)/ElectronRangeEnergy,ElectronRangePower);
	  Int_t Nt = 0; // HeedCsize(dE, dEr,rs);
	  Float_t dET = dE + dEr;
	  dEr = dET;
	  Float_t EC;
	  Int_t   Nr = 0; 
	  if (xRange > 0) {Nr = rs.GetSize();}
	  while ((EC = mHeed->GetRandom()) < dEr) { 
	    dEr -= EC; 
	    if (Nr) {
	      if (Nr <= Nt) {Nr = 2*Nt+1; rs.Set(Nr); }
	      rs[Nt] = 1 - dEr/dET;
	    }
	    Nt++;
	  }
	  if (! Nt) continue;
	  if (ClusterProfile) {
	    checkList[io][8]->Fill(TrackSegmentHits[iSegHits].xyzG.position().z(),Nt);
	    checkList[io][11]->Fill(TrackSegmentHits[iSegHits].xyzG.position().z(),Nt);
#ifdef __LASERINO__
	    SecRow[6]->Fill(sector,row,Nt);
#endif /* __LASERINO__ */
	  }
	  StThreeVectorD xyzC = track.at(newPosition);
	  Double_t phiXY = 2*TMath::Pi()*gRandom->Rndm();
	  Double_t rX = TMath::Cos(phiXY);
	  Double_t rY = TMath::Sin(phiXY);
	  Double_t sigmaT = SigmaT;
	  Double_t sigmaL = SigmaL;
	  TotalSignalInCluster = 0;
	  Int_t WireIndex = 0;
	  for (Int_t ie = 0; ie < Nt; ie++) {
	    nTotal++;
	    QAv = mPolya[io]->GetRandom();
	    // transport to wire
	    gRandom->Rannor(rX,rY);
	    StTpcLocalSectorCoordinate xyzE(xyzC.x()+rX*sigmaT,
					    xyzC.y()+rY*sigmaT,
					    xyzC.z()+gRandom->Gaus(0,sigmaL), sector, row);
	    if (xRange > 0) {
	      Double_t xR = rs[ie]*xRange;
	      TRVector xyzRangeL(3, xR*rX, xR*rY, 0.);
	      TRVector xyzR(L2L,TRArray::kAxB,xyzRangeL);
#ifdef __DEBUG__
	      if (Debug() > 13) {
		LOG_INFO << "xyzRangeL: " << xyzRangeL << endm;
		LOG_INFO << "L2L: " << L2L << endm;
		LOG_INFO << "xyzR: " << xyzR << endm;
	      }
#endif
	      TCL::vadd(xyzE.position().xyz(),xyzR.GetArray(),xyzE.position().xyz(),3);
	    }
	    Double_t y = xyzE.position().y();
	    Double_t alphaVariation = InnerAlphaVariation[sector-1];
	    // Transport to wire
	    if (y <= lastInnerSectorAnodeWire) {
	      WireIndex = TMath::Nint((y - firstInnerSectorAnodeWire)/anodeWirePitch) + 1;
#ifndef __NO_1STROWCORRECTION__
	      if (St_tpcPadConfigC::instance()->iTPC(sector)) {// two first and two last wires are removed, and 3rd wire is fat wiere
		if (WireIndex <= 3 || WireIndex >= numberOfInnerSectorAnodeWires - 3) continue;
	      } else { // old TPC the first and last wires are fat ones
		if (WireIndex <= 1 || WireIndex >= numberOfInnerSectorAnodeWires) continue;
	      }
#else /* __NO_1STROWCORRECTION__ */
	      if (WireIndex <= 1 || WireIndex >= numberOfInnerSectorAnodeWires) continue; // to check the 1-st pad row effect
#endif /* ! __NO_1STROWCORRECTION__ */
	      yOnWire = firstInnerSectorAnodeWire + (WireIndex-1)*anodeWirePitch;
	    } else { // the first and last wires are fat ones
	      WireIndex = TMath::Nint((y - firstOuterSectorAnodeWire)/anodeWirePitch) + 1;
	      if (WireIndex <= 1 || WireIndex >= numberOfOuterSectorAnodeWires) continue;
	      yOnWire = firstOuterSectorAnodeWire + (WireIndex-1)*anodeWirePitch;
	      alphaVariation = OuterAlphaVariation[sector-1];
	    }
	    Double_t distanceToWire = y - yOnWire; // Calculated effective distance to wire affected by Lorentz shift 
	    xOnWire = xyzE.position().x();
	    zOnWire = xyzE.position().z();
	    // Grid plane (1 mm spacing) focusing effect + Lorentz angle in drift volume 
	    Int_t iGridWire = (Int_t ) TMath::Abs(10.*distanceToWire);
	    Double_t dist2Grid = TMath::Sign(0.05 + 0.1*iGridWire, distanceToWire); // [cm]
	    // Ground plane (1 mm spacing) focusing effect
	    Int_t iGroundWire = (Int_t ) TMath::Abs(10.*dist2Grid);
	    Double_t distFocused = TMath::Sign(0.05 + 0.1*iGroundWire, dist2Grid);
	    // OmegaTau near wires taken from comparison with data
	    Double_t tanLorentz = OmegaTau/St_TpcResponseSimulatorC::instance()->OmegaTauScaleO(); 
	    if (y < firstOuterSectorAnodeWire) tanLorentz = OmegaTau/St_TpcResponseSimulatorC::instance()->OmegaTauScaleI(); 
	    xOnWire += distFocused*tanLorentz; // tanLorentz near wires taken from comparison with data
	    zOnWire += TMath::Abs(distFocused);
	    if (! iGroundWire ) QAv *= TMath::Exp( alphaVariation);
	    else                QAv *= TMath::Exp(-alphaVariation);
	    if (ClusterProfile) {
	      checkList[io][9]->Fill(TrackSegmentHits[iSegHits].xyzG.position().z(),QAv);
#ifdef __LASERINO__
	      SecRow[7]->Fill(sector,row,QAv);
#endif /* __LASERINO__ */
	    }
	    Double_t dY    = mChargeFraction[io][sector-1]->GetXmax();
	    Double_t yLmin = yOnWire - dY;
	    Double_t yLmax = yLmin + 2*dY;
	    Int_t    rowMin  = transform.rowFromLocalY(yLmin,sector);
	    Int_t    rowMax  = transform.rowFromLocalY(yLmax,sector);
	    Double_t yRmin = transform.yFromRow(sector,rowMin) - St_tpcPadConfigC::instance()->PadLengthAtRow(sector,rowMin)/2;
	    Double_t yRmax = transform.yFromRow(sector,rowMax) + St_tpcPadConfigC::instance()->PadLengthAtRow(sector,rowMax)/2;
	    if (yRmin > yLmax || yRmax < yLmin) {
	      iBreak++; continue;
	    }
#ifdef __LASERINO__
	    if (ClusterProfile) {
	      SecRow[12]->Fill(sector,row,rowMax-rowMin);
	      SecRow[13]->Fill(sector,row,yRmax-yRmin);
	    }
#endif /* __LASERINO__ */
	    GenerateSignal(TrackSegmentHits[iSegHits],sector,rowMin, rowMax, sigmaJitterT, sigmaJitterX);
	  }  // electrons in Cluster
	  if (ClusterProfile) {
	    if (TotalSignalInCluster > 0 && checkList[io][19]) {
	      checkList[io][19]->Fill(WireIndex,TMath::Log(TotalSignalInCluster));
	      
	    }
	  }
	  TotalSignal += TotalSignalInCluster;
	} while (kTRUE); // Clusters
	//	tpc_hitC->adc = -99;
	if (dESum > 0 && dSSum) {
#ifdef __DEBUG__
	  if (Debug() > 12) {
	    LOG_INFO << "sIndex = " << sIndex << " volId = " << volId
		     << " dESum = " << dESum << " /\tdSSum " << dSSum << " /\t TotalSignal " << TotalSignal << endm;
	  }
#endif
	  tpc_hitC->de = dESum*eV; 
	  tpc_hitC->ds = dSSum; 
	  //	  tpc_hitC->adc = TotalSignal;
	  tpc_hitC->np = nP;
	  if (ClusterProfile) {
	    if (TotalSignal > 0) {
#ifdef __LASERINO__
	      SecRow[9]->Fill(sector,row,TotalSignal);
#endif /* __LASERINO__ */
	      if (hist[ioH][0]) {
		for (Int_t p = 0; p < kPadMax; p++) 
		  hist[ioH][0]->Fill((p+pad0)-padH,TrackSegmentHits[iSegHits].xyzG.position().z(),padsdE[p]/TotalSignal);
	      }
	      if (hist[ioH][1]) {						                          
		for (Int_t t = 0; t < kTimeBacketMax; t++) 
		  hist[ioH][1]->Fill((t+tbk0+0.5)-tbkH,TrackSegmentHits[iSegHits].xyzG.position().z(),tbksdE[t]/TotalSignal);
	      }
	    }
	    checkList[io][15]->Fill(TrackSegmentHits[iSegHits].xyzG.position().z(),tpc_hitC->de);
	    checkList[io][16]->Fill(TrackSegmentHits[iSegHits].xyzG.position().z(),tpc_hitC->ds);
	    checkList[io][18]->Fill(TrackSegmentHits[iSegHits].xyzG.position().z(),nTotal);
	    if (nP > 0 && nTotal > 0) 
	      checkList[io][20]->Fill(TMath::Log(nP),TMath::Log(nTotal) - TMath::Log(nP));
	  }
	}
	NoHitsInTheSector++;
      } // end do loop over segments for a given particle
      for (Int_t iSegHits = 0; iSegHits < nSegHits; iSegHits++) {
	g2t_tpc_hit_st *tpc_hitC = TrackSegmentHits[iSegHits].tpc_hitC;
	if (tpc_hitC->volume_id > 10000) continue;
	Int_t row = tpc_hitC->volume_id%100;
	tpc_hitC->adc += rowsdE[row-1];
	Int_t io = (row <= St_tpcPadConfigC::instance()->numberOfInnerRows(sector)) ? 0 : 1;
	if (checkList[io][17])
	  checkList[io][17]->Fill(TrackSegmentHits[iSegHits].xyzG.position().z(),tpc_hitC->adc);
#ifdef __LASERINO__
	if (tpc_hitC->adc && SecRow[10]) SecRow[10]->Fill(sector,row,tpc_hitC->adc);
#endif /* __LASERINO__ */
      }
    }  // hits in the sector
    if (NoHitsInTheSector) {
      StTpcDigitalSector *digitalSector = DigitizeSector(sector);   
      if (Debug()) LOG_INFO << "StTpcRSMaker: Done with sector\t" << sector << " total no. of hit = " << NoHitsInTheSector << endm;
      if (Debug() > 2) digitalSector->Print();
#ifdef __LASERINO__
      for (Int_t row = 1; row <= St_tpcPadConfigC::instance()->numberOfRows(sector); row++) {
	Int_t Npads = digitalSector->numberOfPadsInRow(row);
	map<Int_t,Int_t> ADCmap2Track;
        for(Int_t pad = 1; pad <= Npads; pad++) {
	  UInt_t ntimebins = digitalSector->numberOfTimeBins(row,pad);
	  if (! ntimebins) continue;
	  static  Short_t ADCs[__MaxNumberOfTimeBins__];
	  static UShort_t IDTs[__MaxNumberOfTimeBins__];
	  digitalSector->getTimeAdc(row,pad,ADCs,IDTs);
	  for (UInt_t t = 0; t < __MaxNumberOfTimeBins__; t++) {
	    if (ADCs[t] > 0 && IDTs[t]) {
	      ADCmap2Track[IDTs[t]] += ADCs[t];
	    }
	  }
	}
	for (UInt_t i = 0; i < ADCmap2Track.size(); i++) {
	  SecRow[11]->Fill(sector,row,ADCmap2Track[i]);
	}
      }
#endif /* __LASERINO__ */
    }
  } // sector
  if (m_SignalSum) {free(m_SignalSum); m_SignalSum = 0;}
  if (Debug()%10) gBenchmark->Show("TpcRS");
  if (m_TpcdEdxCorrection) {
    St_tpcGainCorrectionC::instance()->Struct(1)->min = vminI;
    St_tpcGainCorrectionC::instance()->Struct(0)->min = vminO;
    if (Debug()) {
      LOG_INFO << "Reset min for gain Correction to I/O\t" 
	       << St_tpcGainCorrectionC::instance()->Struct(1)->min 
	       << "\t" 
	       << St_tpcGainCorrectionC::instance()->Struct(0)->min 
	       << " (V)" << endm;
    }
  }
  if (g2t_track) {
    // Reset no. Tpc hits in g2t_track
    tpc_track = g2t_track->GetTable();
    for (Int_t i = 0; i < NoTpcTracks; i++, tpc_track++) {
      Int_t Id = tpc_track->id;
      tpc_track->n_tpc_hit = (mNoTpcHitsReal[Id-1] << 8) + (0xff & mNoTpcHitsAll[Id-1]);
    }
  }
  return kStOK;
}
//________________________________________________________________________________
Double_t StTpcRSMaker::GetNoPrimaryClusters(Double_t betaGamma, Int_t charge) {
  Double_t beta = betaGamma/TMath::Sqrt(1.0 + betaGamma*betaGamma);
#if defined(Old_dNdx_Table) 
  Double_t dNdx = 1.21773e+01*Bichsel::Instance()->GetI70M(TMath::Log10(betaGamma));
#else
#if defined(ElectronHack) 
  Int_t elepos = charge/100;
  Double_t dNdx = 0;
  if      (mdNdx   ) dNdx = mdNdx->Interpolate(betaGamma);
  else if (mdNdxL10) dNdx = mdNdxL10->Interpolate(TMath::Log10(betaGamma));
  if (elepos) {
    dNdx += 1.21773e+01*Bichsel::Instance()->GetI70M(TMath::Log10(betaGamma));
    dNdx /= 2;
  }  
#else /* new H.Bichsel dNdx table 09/12/11 */
  Double_t dNdx = 0;
  if      (mdNdx   ) dNdx = mdNdx->Interpolate(betaGamma);
  else if (mdNdxL10) dNdx = mdNdxL10->Interpolate(TMath::Log10(betaGamma));
#endif /* Old_dNdx_Table || ElectronHack */
#endif
  Double_t Q_eff = TMath::Abs(charge%100);
  if (Q_eff > 1)   {
    // Effective charge from GEANT ghion.F
    Double_t w1 = 1.034 - 0.1777*TMath::Exp(-0.08114*Q_eff);
    Double_t w2 = beta*TMath::Power(Q_eff,-2./3.);
    Double_t w3 = 121.4139*w2 + 0.0378*TMath::Sin(190.7165*w2);
    Q_eff      *= 1. -w1*TMath::Exp(-w3);
  }
  return Q_eff*Q_eff*dNdx;
}
//________________________________________________________________________________
Double_t StTpcRSMaker::ShaperFunc(Double_t *x, Double_t *par) {
  Double_t tau = par[0];
  Double_t width = par[1];
  Double_t p = par[2];
  Double_t t = x[0]*width/tau;
  Double_t Delta = width/tau;
  Double_t t1 = t - Delta/2.;
  Double_t t2 = t1 + Delta;
  Double_t val = TMath::Gamma(p,t2) - TMath::Gamma(p,t1);
  return val;
}
//________________________________________________________________________________
Double_t StTpcRSMaker::PadResponseFunc(Double_t *x, Double_t *par) {
  Double_t CrossTalk = 0;
  Double_t Value = 0;
  Double_t X = par[5]*x[0];
  if (CrossTalk > 0) {
    for (Int_t i = -1; i <= 1; i++) {
      Double_t xx = X + par[5]*i;
      if (i == 0) Value += (1. - 2.*CrossTalk)*Gatti(&xx,par);
      else        Value +=          CrossTalk *Gatti(&xx,par);
    }
  } else   Value = Gatti(&X,par);
  return Value;
}
//________________________________________________________________________________
Double_t StTpcRSMaker::Gatti(Double_t *x, Double_t *par) {
  /************************************************************************
   *  Function    : generates the cathode signal using                    *
   *                the single-parameter Gatti formula:                   *
   *                              1 - tanh(K2 * lambda)**2                *
   *     GFunc(lambda) = K1 * -------------------------------             *
   *                           1 + K3 * tanh (K2 *lambda)**2              *
   *     lambda = x/h, h is anode cathode spacing                         *
   *                                                                      *
   *     K2 = pi/2*(1 - 0.5*sqrt(K3))                                     *
   *                                                                      *
   *              K2*sqrt(K3)                                             *
   *     K1 = -------------------                                         *
   *            4 * atan(sqrt(K3))                                        *
   *                                                                      *
   *  References  : E.Gatti, A.Longoni, NIM 163 (1979) 82-93.             *
   *  Authors : V.Balagura,V.Cherniatin,A.Chikanian                       *
   ************************************************************************/
  Double_t y = x[0];   // distance to center of strip [cm]
  Double_t w = par[0]; // w = width of pad       
  Double_t h = par[1]; // h = Anode-Cathode gap  
  Double_t K3  = par[3];
  Double_t lambda = y/h;
  Double_t K2 = TMath::PiOver2()*(1. - 0.5*TMath::Sqrt(K3));  
  //  Double_t K1 = K2*TMath::Sqrt(K3)/(2*TMath::ATan(TMath::Sqrt(K3)));
  Double_t sqK3 = TMath::Sqrt(K3);
  Double_t ATsqK3 = 0.5/TMath::ATan(sqK3);
  Double_t Y1 = lambda - w/h/2;
  Double_t Y2 = Y1 + w/h;
  Double_t X1 = K2*Y1;
  Double_t X2 = K2*Y2;
  Double_t Z1 = sqK3*TMath::TanH(X1);
  Double_t Z2 = sqK3*TMath::TanH(X2);
  Double_t val = ATsqK3*(TMath::ATan(Z2) - TMath::ATan(Z1));
  return val;
}
//________________________________________________________________________________
void  StTpcRSMaker::Print(Option_t */* option */) const {
  PrPP(Print, NoOfSectors);
  PrPP(Print, St_tpcPadConfigC::instance()->numberOfRows(1));
  PrPP(Print, St_tpcPadConfigC::instance()->numberOfRows(20));
  PrPP(Print, St_tpcPadConfigC::instance()->numberOfInnerRows(20));
  PrPP(Print, NoOfPads);
  PrPP(Print, St_TpcResponseSimulatorC::instance()->W());// = 26.2);//*eV
  PrPP(Print, St_TpcResponseSimulatorC::instance()->Cluster());
  PrPP(Print, St_TpcResponseSimulatorC::instance()->longitudinalDiffusion());
  PrPP(Print, St_TpcResponseSimulatorC::instance()->transverseDiffusion());
  //  PrPP(Print, Gain);
  PrPP(Print, NoOfTimeBins);
  PrPP(Print, numberOfInnerSectorAnodeWires); 
  PrPP(Print, firstInnerSectorAnodeWire);
  PrPP(Print, lastInnerSectorAnodeWire);
  PrPP(Print, numberOfOuterSectorAnodeWires);
  PrPP(Print, firstOuterSectorAnodeWire);
  PrPP(Print, lastOuterSectorAnodeWire);
  PrPP(Print, anodeWirePitch);
  PrPP(Print,St_TpcResponseSimulatorC::instance()->OmegaTau()); // tan of Lorentz angle
  PrPP(Print, St_TpcResponseSimulatorC::instance()->NoElPerAdcI());
  PrPP(Print, St_TpcResponseSimulatorC::instance()->NoElPerAdcO());
  PrPP(Print, St_TpcResponseSimulatorC::instance()->NoElPerAdcX());
  PrPP(Print, anodeWireRadius);
  PrPP(Print, St_TpcResponseSimulatorC::instance()->AveragePedestal());
  PrPP(Print, St_TpcResponseSimulatorC::instance()->AveragePedestalRMS());
  PrPP(Print, St_TpcResponseSimulatorC::instance()->AveragePedestalRMSX());
  PrPP(Print, St_TpcResponseSimulatorC::instance()->FanoFactor());
  for (Int_t sector = 1; sector <= 24; sector++) {
    PrPP(Print, innerSectorAnodeVoltage[sector-1]);
    PrPP(Print, outerSectorAnodeVoltage[sector-1]);
  }
  PrPP(Print, St_TpcResponseSimulatorC::instance()->K3IP());
  PrPP(Print, St_TpcResponseSimulatorC::instance()->K3IR());
  PrPP(Print, St_TpcResponseSimulatorC::instance()->K3OP());
  PrPP(Print, St_TpcResponseSimulatorC::instance()->K3OR());
  PrPP(Print, St_TpcResponseSimulatorC::instance()->SigmaJitterTI());
  PrPP(Print, St_TpcResponseSimulatorC::instance()->SigmaJitterTO());
}
//________________________________________________________________________________
StTpcDigitalSector  *StTpcRSMaker::DigitizeSector(Int_t sector){
  static Int_t iBreak = 0;
  static Int_t AdcCut = 500;
  //  static Int_t PedestalMem[__MaxNumberOfTimeBins__];
  TDataSet *event = GetData("Event");
  StTpcRawData *data = 0;
  if (! event) {
    data = new StTpcRawData(NoOfSectors);
    event = new TObjectSet("Event", data);
    AddData(event);
  } else data = (StTpcRawData *) event->GetObject();
  assert(data);
  SignalSum_t *SignalSum = GetSignalSum(sector);
  Double_t ped    = 0; 
  Int_t adc = 0;
  Int_t index = 0;
  Double_t gain = 1;
  Int_t row, pad, bin;
  Int_t Sector = TMath::Abs(sector);
  StTpcDigitalSector *digitalSector = data->GetSector(Sector);
  if (! digitalSector) {
    digitalSector = new StTpcDigitalSector(Sector);
    data->setSector(Sector,digitalSector);
  } else 
    digitalSector->clear();
  for (row = 1;  row <= St_tpcPadConfigC::instance()->numberOfRows(sector); row++) {
    Int_t NoOfPadsAtRow = St_tpcPadConfigC::instance()->padsPerRow(sector,row);
    Double_t pedRMS = St_TpcResponseSimulatorC::instance()->AveragePedestalRMS();
    if (St_tpcAltroParamsC::instance()->N(sector-1) > 0) {
      if (! (St_tpcPadConfigC::instance()->iTPC(sector) && St_tpcPadConfigC::instance()->IsRowInner(sector,row))) {
	pedRMS = St_TpcResponseSimulatorC::instance()->AveragePedestalRMSX();
      }
    }
#ifdef __DEBUG__
    Float_t AdcSumBeforeAltro = 0, AdcSumAfterAltro = 0;
#endif /*     __DEBUG__ */
    for (pad = 1; pad <= NoOfPadsAtRow; pad++) {
      gain = St_tpcPadGainT0BC::instance()->Gain(Sector,row,pad);
      if (gain <= 0.0) continue;
      ped    = St_TpcResponseSimulatorC::instance()->AveragePedestal();
      static  Short_t ADCs[__MaxNumberOfTimeBins__];
      static UShort_t IDTs[__MaxNumberOfTimeBins__];
      memset(ADCs, 0, sizeof(ADCs));
      memset(IDTs, 0, sizeof(IDTs));
      Int_t NoTB = 0;
      index = NoOfTimeBins*((row-1)*NoOfPads+pad-1);
      for (bin = 0; bin < NoOfTimeBins; bin++,index++) {
	//	Int_t index= NoOfTimeBins*((row-1)*NoOfPads+pad-1)+bin;
	// Digits : gain + ped
	//  GG TF1F *ff = new TF1F("ff","TMath::Sqrt(4.76658e+01*TMath::Exp(-2.87987e-01*(x-1.46222e+01)))",21,56)
	Double_t pRMS = pedRMS;
#if 0
	if (bin >= 21 && bin <= 56) {
	  pRMS = TMath::Sqrt(pedRMS*pedRMS + 4.76658e+01*TMath::Exp(-2.87987e-01*(bin-1.46222e+01)));
	}
#endif
	if (pRMS > 0) {
	  adc = (Int_t) (SignalSum[index].Sum/gain + gRandom->Gaus(ped,pRMS));
	  adc = adc - (Int_t) ped;
	}
	else            adc = (Int_t) (SignalSum[index].Sum/gain);
	if (adc > 1023) adc = 1023;
	if (adc < 1) continue;
	SignalSum[index].Adc = adc;
	NoTB++;
	ADCs[bin] = adc;
	IDTs[bin] = SignalSum[index].TrackId;
#ifdef __DEBUG__
        if (adc > 3*pedRMS)	AdcSumBeforeAltro += adc;
	if (Debug() > 11 && SignalSum[index].Sum > 0) {
	  LOG_INFO << "digi R/P/T/I = " << row << " /\t" << pad << " /\t" << bin << " /\t" << index 
		   << "\tSum/Adc/TrackId = " << SignalSum[index].Sum << " /\t" 
		   << SignalSum[index].Adc << " /\t" << SignalSum[index].TrackId << endm;
	}
#endif
      }
      if (! NoTB) continue;
      if (St_tpcAltroParamsC::instance()->N(sector-1) >= 0 && ! mAltro) {
	mAltro = new Altro(__MaxNumberOfTimeBins__,ADCs);
	if (St_tpcAltroParamsC::instance()->N(sector-1) > 0) {// Tonko 06/25/08
	  //      ConfigAltro(ONBaselineCorrection1, ONTailcancellation, ONBaselineCorrection2, ONClipping, ONZerosuppression)
	  mAltro->ConfigAltro(                    0,                  1,                     0,          1,                 1); 
	  //       ConfigBaselineCorrection_1(int mode, int ValuePeDestal, int *PedestalMem, int polarity)
	  //altro->ConfigBaselineCorrection_1(4, 0, PedestalMem, 0);  // Tonko 06/25/08
	  mAltro->ConfigTailCancellationFilter(St_tpcAltroParamsC::instance()->K1(),
					       St_tpcAltroParamsC::instance()->K2(),
					       St_tpcAltroParamsC::instance()->K3(), // K1-3
					       St_tpcAltroParamsC::instance()->L1(),
					       St_tpcAltroParamsC::instance()->L2(),
					       St_tpcAltroParamsC::instance()->L3());// L1-3
	} else {
	  mAltro->ConfigAltro(0,0,0,1,1); 
	}
	mAltro->ConfigZerosuppression(St_tpcAltroParamsC::instance()->Threshold(),
				      St_tpcAltroParamsC::instance()->MinSamplesaboveThreshold(),
				      0,0);
	mAltro->PrintParameters();
      }
      if (mAltro) {
	//#define PixelDUMP
#ifdef PixelDUMP
	static Short_t ADCsSaved[__MaxNumberOfTimeBins__];
	memcpy(ADCsSaved, ADCs,sizeof(ADCsSaved));
#endif
	mAltro->RunEmulation();
#ifdef PixelDUMP
	ofstream *out = new ofstream("digi.dump",ios_base::app);
	for (Int_t i = 0; i < __MaxNumberOfTimeBins__; i++) {
	  if (ADCsSaved[i] > 0 || ADCs[i] > 0) {
	    LOG_INFO << Form("s %2i r %i p %3i t %3i: %10i => %10i keep %10i",sector,row,pad,i,ADCsSaved[i],ADCs[i],mAltro->ADCkeep[i]) << endm;
	    *out << Form("s %2i r %i p %3i t %3i: %10i => %10i keep %10i",sector,row,pad,i,ADCsSaved[i],ADCs[i],mAltro->ADCkeep[i]) << endl;
	  }
	}
	delete out;
#endif
	NoTB = 0;
	Int_t ADCsum = 0;
	for (Int_t i = 0; i < __MaxNumberOfTimeBins__; i++) {
	  if (ADCs[i] && ! mAltro->ADCkeep[i]) {ADCs[i] = 0;}
	  if (ADCs[i]) {
	    NoTB++;
	    ADCsum += ADCs[i];
#ifdef __DEBUG__
	    if (ADCs[i] > 3*pedRMS) AdcSumAfterAltro += ADCs[i];
	    if (Debug() > 12) {
	      LOG_INFO << "Altro R/P/T/I = " << row << " /\t" << pad << " /\t" << i 
		       << "\tAdc/TrackId = " << ADCs[i] << " /\t" << IDTs[i] << endm;
	    }
#endif
	  } else {IDTs[i] = 0;}
	}
#ifdef __DEBUG__
	if (ADCsum > AdcCut) {
	  iBreak++;
	}
#endif	
      }
      else {
	if (St_tpcAltroParamsC::instance()->N(sector-1) < 0) NoTB = AsicThresholds(ADCs);
      }
      if (NoTB > 0 && digitalSector) {
	digitalSector->putTimeAdc(row,pad,ADCs,IDTs);
      }
    } // pads
#ifdef __DEBUG__
    if (Debug() > 10) {  
      LOG_INFO << "row = " << row << "\tAdcSumBeforeAltro = " << AdcSumBeforeAltro << "\tAdcSumAfterAltro = " << AdcSumAfterAltro << endm;
    }
#endif /*     __DEBUG__ */
  } // row
  return digitalSector;
}
//________________________________________________________________________________
Int_t StTpcRSMaker::AsicThresholds(Short_t ADCs[__MaxNumberOfTimeBins__]) {
  Int_t t1 = 0;
  Int_t nSeqLo = 0;
  Int_t nSeqHi = 0;
  Int_t noTbleft = 0;
  for (UInt_t tb = 0; tb < __MaxNumberOfTimeBins__; tb++) {
    if (ADCs[tb] <= St_asic_thresholdsC::instance()->thresh_lo()) {
      if (! t1) ADCs[tb] = 0;
      else {
	if (nSeqLo <= St_asic_thresholdsC::instance()->n_seq_lo() ||
	    nSeqHi <= St_asic_thresholdsC::instance()->n_seq_hi()) 
	  for (UInt_t t = t1; t <= tb; t++) ADCs[t] = 0;
	else noTbleft += nSeqLo;
      }
      t1 = nSeqLo = nSeqHi = 0;
    }
    nSeqLo++; 
    if (! t1) t1 = tb;
    if (ADCs[tb] > St_asic_thresholdsC::instance()->thresh_hi()) {nSeqHi++;}
  }
  return noTbleft;
}
//________________________________________________________________________________
Double_t StTpcRSMaker::InducedCharge(Double_t s, Double_t h, Double_t ra, Double_t Va, Double_t &t0) {
  // Calculate variation of induced charge due to different arrived angles 
  // alpha = -26 and -70 degrees
  LOG_INFO << "wire spacing = " << s << " cm"
	   << "\tcathode anode gap = " << h << " cm"
	   << "\tanode wire radius = " << ra << " cm"
	   << "\tpotential on anode wire = " << Va << " V" << endm;
  const Double_t B  = 30e-3; // 1/V
  const Double_t E0 = 20e3; // V/cm
  const Double_t mu = 2.26; // cm**2/V/sec CH4+ mobility 
  // const Double_t mu = 1.87; // cm**2/V/sec Ar+ mobility 
  Double_t alpha[2] = {-26., -70.};
  Double_t pi = TMath::Pi();
  // E.Mathieson (3.2b), V.Chernyatin said that it should be used this (Weber ) approximation 07/09/08
  Double_t rc = s/(2*pi)*TMath::Exp(pi*h/s); LOG_INFO << "rc(Cylinder approx) = " << rc << " cm" << endm; 
  //  Double_t rc = 4*h/pi; LOG_INFO << "rc = " << rc << " cm" << endm;   // E.Mathieson (4.3), no valid for our case
  Double_t C  = 1./(2*TMath::Log(rc/ra)); LOG_INFO << "C = " << C << endm;
  Double_t E  = 2*pi*C*Va/s; LOG_INFO << "E = " << E << " V/cm" << endm;
  // Gain variation: M = M0*(1 - k*cos(2*alpha))
  Double_t k = 2*B/3.*TMath::Power((pi/E0/s),2)*TMath::Power(C*Va,3); LOG_INFO << "k = " << k << endm;
  // Induced charge variation
  t0 = ra*ra/(4*mu*C*Va); 
  LOG_INFO << "t0 = " << 1e9*t0 << " ns" << endm;                                     // E.Mathieson (2.10)
  Double_t Tav = t0*h/s/(2*pi*C);  LOG_INFO << "Tav = " << 1e9*Tav << " ns" << endm;
  //  Double_t t = 5*55e-9;             LOG_INFO << "t = " << 1e9*t << " ns" << endm;
  Double_t t = 180e-9;             LOG_INFO << "t = " << 1e9*t << " ns" << endm; 
  Double_t rp = TMath::Sqrt(1. + t/t0); LOG_INFO << "r' = " << rp << endm;
  // qc = rp*ra*sin(alpha)/(2*h) + C/2*log(1 + t/t0) = A*sin(alpha) + B
  Double_t Aconstant = rp*ra/(2*h);        LOG_INFO << "Aconstant = " << Aconstant << endm;
  Double_t Bconstant = C/2*TMath::Log(1 + t/t0); LOG_INFO << "Bconstant = " << Bconstant << endm;
  Double_t Gains[2];
  for (Int_t i = 0; i < 2; i++) {
    Gains[i] = Aconstant*TMath::Sin(pi/180*alpha[i]) + Bconstant; 
    LOG_INFO << "Gain = " << Gains[i] << " at alpha = " << alpha[i] << " degree" << endm;
  }
  Double_t GainsAv = TMath::Sqrt(Gains[0]*Gains[1]);
  Double_t r = 0;
  for (Int_t i = 0; i < 2; i++) {
    r = TMath::Log(Gains[i]/GainsAv); LOG_INFO << "Relative gain " << r << " at alpha = " << alpha[i] << endm;
  }
  return r;
}
//________________________________________________________________________________
Int_t StTpcRSMaker::SearchT(const void *elem1, const void **elem2) { 
  g2t_tpc_hit_st *value1 = (g2t_tpc_hit_st *) elem1;    
  g2t_tpc_hit_st *value2 = (g2t_tpc_hit_st *) *elem2;   
  // sectors
  if ((value1->volume_id%100000)/100 != (value2->volume_id%100000)/100) 
    return (value1->volume_id%100000)/100 - (value2->volume_id%100000)/100;
  // track id
  if (value1->track_p          != value2->track_p) return value1->track_p - value2->track_p;
  // pad rows
  //  if (value1->volume_id%100 != value2->volume_id%100) return value1->volume_id%100 - value2->volume_id%100;
  // track length 
  return (Int_t) 100*(value1->length - value2->length);
}
//________________________________________________________________________________
Int_t StTpcRSMaker::CompareT(const void **elem1, const void **elem2) {
  return SearchT(*elem1, elem2);
}                                                     
#if 0
//________________________________________________________________________________
Double_t StTpcRSMaker::DriftLength(Double_t x, Double_t y) {
  static const Double_t Step = 5e-2;
  Double_t r = TMath::Sqrt(x*x + y*y);
  if (r < 0.25) return r;
  x = TMath::Abs(x);
  y = TMath::Abs(y);
  Int_t Nstep = 0;
  while (x > Step || y > Step) {
    Double_t Slope = TMath:SinH(TMath::Pi()*y/s)/TMath:Sin(TMath::Pi()*x/s);
    Double_t Co2 = 1./(1. + Slope*Slope);
    Double_t Si  = TMath::Sqrt(1. - Co2);
    Double_t Co  = TMath::Sqrt(Co2);
    x = TMath::Abs(x - Step*Co);
    y = TMath::Abs(y - Step*Si);
    NStep++
      }
  return NStep*Step;
}
#endif
//________________________________________________________________________________
Double_t StTpcRSMaker::fei(Double_t t, Double_t t0, Double_t T) {
  static const Double_t xmaxt = 708.39641853226408;
  static const Double_t xmaxD  = xmaxt - TMath::Log(xmaxt);
  Double_t t01 = xmaxD, t11 = xmaxD;
  if (T > 0) {t11 = (t+t0)/T;}
  if (t11 > xmaxD) t11 = xmaxD;
  if (T > 0) {t01 = t0/T;}
  if (t01 > xmaxD) t01  = xmaxD;
  return TMath::Exp(-t11)*(ROOT::Math::expint(t11) - ROOT::Math::expint(t01));
}
//________________________________________________________________________________
Double_t StTpcRSMaker::shapeEI(Double_t *x, Double_t *par) {// does not work. It is needed to 1/s
  Double_t t  = x[0];
  Double_t value = 0;
  if (t <= 0) return value;
  Double_t t0    = par[0];
  Double_t T1 = par[1]; // tau_I
  Double_t T2 = par[3]; // tau_C
  if (TMath::Abs((T1-T2)/(T1+T2)) < 1e-7) {
    return TMath::Max(0.,(t + t0)/T1*fei(t,t0,T1) + TMath::Exp(-t/T1) - 1);
  } 
  if (T2 <= 0) return fei(t,t0,T1);
  if (T1 <= 0) return 0;
  return T1/(T1 - T2)*(fei(t,t0,T1) - fei(t,t0,T2));
}
//________________________________________________________________________________
Double_t StTpcRSMaker::shapeEI3(Double_t *x, Double_t *par) {// does not work. It is needed to 1/s
  Double_t t  = x[0];
  Double_t value = 0;
  if (t <= 0) return value;
  Double_t t0    = par[0];
  Double_t tau_F = par[1];
  Double_t tau_P = par[2];
  Double_t tau_I = par[3];
  Double_t tau_C = par[5];
  Double_t d =   1./tau_P;
  Double_t a[3] = {- 1./tau_I, - 1./tau_F, 0};
  Double_t A[3] = {(a[0]+d)/(a[0]-a[1]), (a[1]+d)/(a[1]-a[0]), 0};
  Int_t N = 2;
  if (tau_C > 0) {
    N = 3;
    a[2] = -1./tau_C;
    A[0] = (a[0] + d)/a[0]/(a[0] - a[1])/(a[0] - a[2]);
    A[1] = (a[1] + d)/a[1]/(a[1] - a[0])/(a[1] - a[2]);
    A[2] = (a[2] + d)/a[2]/(a[2] - a[0])/(a[2] - a[1]); 
  }
  for (Int_t i = 0; i < N; i++) {
    value += A[i]*TMath::Exp(a[i]*(t+t0))*(ROOT::Math::expint(-a[i]*(t+t0))-ROOT::Math::expint(-a[i]*t0));
  }
  return value;
}
//________________________________________________________________________________
Double_t StTpcRSMaker::shapeEI_I(Double_t *x, Double_t *par) { //Integral of shape over time bin
  static Double_t sqrt2 = TMath::Sqrt(2.);
  Double_t TimeBinWidth = par[2];
  Double_t norm = par[3];
  Double_t t1 = TimeBinWidth*(x[0] - 0.5);
  Double_t t2 = t1 + TimeBinWidth;
  Int_t io = (Int_t) par[4];
  assert(io >= 0 && io <= 1);
  return sqrt2*fgTimeShape0[io]->Integral(t1,t2)/norm;
}
//________________________________________________________________________________
Double_t StTpcRSMaker::shapeEI3_I(Double_t *x, Double_t *par) { //Integral of shape over time bin
  static Double_t sqrt2 = TMath::Sqrt(2.);
  Double_t TimeBinWidth = par[4];
  Double_t norm = par[5];
  Double_t t1 = TimeBinWidth*(x[0] - 0.5);
  Double_t t2 = t1 + TimeBinWidth;
  Int_t io = (Int_t) par[6];
  assert(io >= 0 && io <= 1);
  return sqrt2*fgTimeShape3[io]->Integral(t1,t2)/norm;
}
//________________________________________________________________________________
SignalSum_t  *StTpcRSMaker::GetSignalSum(Int_t sector) {
  if (! m_SignalSum) 
    m_SignalSum = (SignalSum_t  *) malloc(St_tpcPadConfigC::instance()->numberOfRows(sector)*NoOfPads*NoOfTimeBins*sizeof(SignalSum_t)); 
  return m_SignalSum;
}
//________________________________________________________________________________
SignalSum_t  *StTpcRSMaker::ResetSignalSum(Int_t sector) {
  GetSignalSum(sector);
  memset (m_SignalSum, 0, St_tpcPadConfigC::instance()->numberOfRows(sector)*NoOfPads*NoOfTimeBins*sizeof(SignalSum_t));
  return m_SignalSum;
}
//________________________________________________________________________________
Double_t StTpcRSMaker::polya(Double_t *x, Double_t *par) {
  return TMath::GammaDist(x[0],par[0],par[1],par[2]);
}
//________________________________________________________________________________
Double_t StTpcRSMaker::Ec(Double_t *x, Double_t *p) {
  if (x[0] < p[0]/2 || x[0] > 3.064*p[0]) return 0;
  if (x[0] < p[0]) return 1;
  return TMath::Power(p[0]/x[0],4);
}
//________________________________________________________________________________
TF1 *StTpcRSMaker::StTpcRSMaker::fEc(Double_t w) {
  TF1 *f = new TF1("Ec",Ec,0,3.064*w,1);
  f->SetParameter(0,w);
  return f;
}
//________________________________________________________________________________
#ifndef WIN32
# define gcomad gcomad_
#else
# define gcomad  GCOMAD
#endif
//______________________________________________________________________
extern "C"
{
  void type_of_call gcomad(DEFCHARD, Int_t*& DEFCHARL);
}
Float_t StTpcRSMaker::GetCutEle() {
  //----------GCBANK
  //      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
  //     +             ,LMAIN,LR1,WS(KWBANK)
  struct Gcbank_t {
    Int_t nzebra;
    Float_t gversn;
    Float_t zversn;
    Int_t ixstor;
    Int_t ixdiv;
    Int_t ixcons;
    Float_t fendq[16];
    Int_t lmain;
    Int_t lr1;
  };
  Gcbank_t *fGcbank;          //! GCBANK common structure
  gcomad(PASSCHARD("GCBANK"),(int*&) fGcbank  PASSCHARL("GCBANK"));
//----------GCLINK
//      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
//     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
//     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
typedef struct {
  Int_t    jdigi;
  Int_t    jdraw;
  Int_t    jhead;
  Int_t    jhits;
  Int_t    jkine;
  Int_t    jmate;
  Int_t    jpart;
  Int_t    jrotm;
  Int_t    jrung;
  Int_t    jset;
  Int_t    jstak;
  Int_t    jgstat;
  Int_t    jtmed;
  Int_t    jtrack;
  Int_t    jvertx;
  Int_t    jvolum;
  Int_t    jxyz;
  Int_t    jgpar;
  Int_t    jgpar2;
  Int_t    jsklt;
} Gclink_t;
  Gclink_t *fGclink;          //! GCLINK common structure
  gcomad(PASSCHARD("GCLINK"),(int*&) fGclink  PASSCHARL("GCLINK"));

  //----------GCCUTS
  //  COMMON/GCCUTS/CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM
  //   +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS(5)
  struct  Gccuts_t {
    Float_t cutgam;
    Float_t cutele;
    Float_t cutneu;
    Float_t cuthad;
    Float_t cutmuo;
    Float_t bcute;
    Float_t bcutm;
    Float_t dcute;
    Float_t dcutm;
    Float_t ppcutm;
    Float_t tofmax;
    Float_t gcuts[5];
  };
  Gccuts_t *fGccuts;          //! GCCUTS common structure
  gcomad(PASSCHARD("GCCUTS"),(int*&) fGccuts  PASSCHARL("GCCUTS"));
  //----------GCNUM
  //   COMMON/GCNUM/NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
  //  +            ,NSTMAX,NVERTX,NHEAD,NBIT
  struct  Gcnum_t {
    Int_t    nmate;
    Int_t    nvolum;
    Int_t    nrotm;
    Int_t    ntmed;
    Int_t    ntmult;
    Int_t    ntrack;
    Int_t    npart;
    Int_t    nstmax;
    Int_t    nvertx;
    Int_t    nhead;
    Int_t    nbit;
  };
  Gcnum_t  *fGcnum;           //! GCNUM common structure
  gcomad(PASSCHARD("GCNUM"), (int*&) fGcnum   PASSCHARL("GCNUM"));
  Int_t *addr;
  // Variables for ZEBRA store
  gcomad(PASSCHARD("IQ"), addr  PASSCHARL("IQ"));
  Int_t   *fZiq = addr;
  gcomad(PASSCHARD("LQ"), addr  PASSCHARL("LQ"));
  Int_t   *fZlq = addr;
  Float_t *fZq       = (float*)fZiq;
  Int_t   ITPAR = 2; // IF(CHPAR.EQ.'CUTELE')ITPAR=2
  Int_t JTMED = fGclink->jtmed;
  for (Int_t i = 1; i <= fGcnum->ntmed; i++) {
    Int_t JTM = fZlq[JTMED-i];
    if (! JTM) continue;
    TString Medium((Char_t *)(&fZiq[JTM+1]),20);
    if (!Medium.BeginsWith(TpcMedium)) continue;
    Int_t JTMN = fZlq[JTM];
    if (! JTMN) continue;
    Float_t cutele = fZq[JTMN+ITPAR];
    return cutele;
  }
  LOG_INFO << "StTpcRSMaker::GetCutEle: specific CutEle for medium \"" << TpcMedium.Data() << "\" has not been found. Use default." << endm;
  return fGccuts->cutele;
}
//________________________________________________________________________________
Bool_t StTpcRSMaker::TrackSegment2Propagate(g2t_tpc_hit_st *tpc_hitC, g2t_vertex_st *gver, HitPoint_t &TrackSegmentHits) {
  static Int_t iBreak = 0;
  if (! tpc_hitC) {
    iBreak++;
  }
  Int_t Id = tpc_hitC->track_p;
  mNoTpcHitsAll[Id-1]++;
  if (tpc_hitC->volume_id < 10000) {
    // Account hits which can be splitted
    mNoTpcHitsReal[Id-1]++;
  }
  if (tpc_hitC->de > 0) {
    mNSplittedHits = 0;
  } else if (! mNSplittedHits) {
    mNSplittedHits++;
  }
  Int_t volId = tpc_hitC->volume_id%10000;
  Int_t sector = volId/100;
   static StGlobalCoordinate coorG;    // ideal 
  TrackSegmentHits.xyzG = 
    StGlobalCoordinate(tpc_hitC->x[0],tpc_hitC->x[1],tpc_hitC->x[2]);  PrPP(Make,TrackSegmentHits.xyzG);
  coorG = TrackSegmentHits.xyzG;
  static StTpcLocalCoordinate  coorLT;  // before do distortions
  static StTpcLocalDirection  dirLT, BLT;
  // calculate row
  static StTpcLocalSectorCoordinate coorS;
  static StTpcCoordinateTransform transform(gStTpcDb);
  transform(coorG, coorS,sector,0); PrPP(Make,coorS);
  Int_t row = coorS.fromRow();
  transform(coorG, coorLT,sector,row); PrPP(Make,coorLT);
  Int_t io = (row <= St_tpcPadConfigC::instance()->numberOfInnerRows(sector)) ? 0 : 1;
  TrackSegmentHits.TrackId    = Id;
  TrackSegmentHits.tpc_hitC = tpc_hitC;
  
  if (ClusterProfile) {
    checkList[io][0]->Fill(TrackSegmentHits.tpc_hitC->x[2],TMath::Abs(TrackSegmentHits.tpc_hitC->de));
    checkList[io][1]->Fill(TrackSegmentHits.tpc_hitC->x[2],           TrackSegmentHits.tpc_hitC->ds );	
#ifdef __LASERINO__
    SecRow[0]->Fill(sector,row,TrackSegmentHits.tpc_hitC->de);
    SecRow[1]->Fill(sector,row,TrackSegmentHits.tpc_hitC->ds);
#endif /* __LASERINO__ */
  }
  TrackSegmentHits.sMin = TrackSegmentHits.s - TrackSegmentHits.tpc_hitC->ds;
  TrackSegmentHits.sMax = TrackSegmentHits.s;
  if (TrackSegmentHits.sMin < msMin) msMin = TrackSegmentHits.sMin;
  if (TrackSegmentHits.sMax > msMax) msMax = TrackSegmentHits.sMax;
  // move up, calculate field at center of TPC
  static Float_t BFieldG[3]; 
  StMagF::Agufld(tpc_hitC->x,BFieldG);
  // distortion and misalignment 
  // replace pxy => direction and try linear extrapolation
  StThreeVectorD       pxyzG(tpc_hitC->p[0],tpc_hitC->p[1],tpc_hitC->p[2]);
  StGlobalDirection    dirG(pxyzG.unit());                               PrPP(Make,dirG);
  StGlobalDirection    BG(BFieldG[0],BFieldG[1],BFieldG[2]);             PrPP(Make,BG);
  transform( dirG,  dirLT,sector,row); PrPP(Make,dirLT); 
  transform(   BG,    BLT,sector,row); PrPP(Make,BLT);   
  // Distortions 
  if (TESTBIT(m_Mode, kDistortion) && StMagUtilities::Instance()) {
    Float_t pos[3] = {(Float_t ) coorLT.position().x(), (Float_t ) coorLT.position().y(), (Float_t ) coorLT.position().z()};
    Float_t posMoved[3];
    StMagUtilities::Instance()->DoDistortion(pos,posMoved,sector);   // input pos[], returns posMoved[]
    StThreeVector<double> position(posMoved[0],posMoved[1],posMoved[2]);
    coorLT.setPosition(position);        // after do distortions
    transform(coorLT,TrackSegmentHits.xyzG);                PrPP(Make,coorLT);
  }
  // end of distortion
  transform(coorLT,TrackSegmentHits.coorLS); PrPP(Make,TrackSegmentHits.coorLS);
  transform( dirLT, TrackSegmentHits.dirLS); PrPP(Make,TrackSegmentHits.dirLS); 
  transform(   BLT,   TrackSegmentHits.BLS); PrPP(Make,TrackSegmentHits.BLS);   
  Double_t tof = gver->ge_tof;
  //	if (! TESTBIT(m_Mode, kNoToflight)) 
  tof += tpc_hitC->tof;
  Double_t driftLength = TrackSegmentHits.coorLS.position().z() + tof*gStTpcDb->DriftVelocity(sector); // ,row); 
  if (driftLength > -1.0 && driftLength <= 0) {
    if ((row >  St_tpcPadConfigC::instance()->numberOfInnerRows(sector) && driftLength > -gStTpcDb->WirePlaneGeometry()->outerSectorAnodeWirePadPlaneSeparation()) ||
	(row <= St_tpcPadConfigC::instance()->numberOfInnerRows(sector) && driftLength > -gStTpcDb->WirePlaneGeometry()->innerSectorAnodeWirePadPlaneSeparation())) 
      driftLength = TMath::Abs(driftLength);
  }
  TrackSegmentHits.coorLS.position().setZ(driftLength); PrPP(Make,TrackSegmentHits.coorLS);
  // useT0, don't useTau
  transform(TrackSegmentHits.coorLS,TrackSegmentHits.Pad,kFALSE,kFALSE); // don't use T0, don't use Tau
  PrPP(Make,TrackSegmentHits.Pad); 
  return kTRUE;
}
//________________________________________________________________________________
void StTpcRSMaker::GenerateSignal(HitPoint_t &TrackSegmentHits, Int_t sector, Int_t rowMin, Int_t rowMax, Double_t sigmaJitterT, Double_t sigmaJitterX) {
  static StTpcCoordinateTransform transform(gStTpcDb);
  SignalSum_t *SignalSum = GetSignalSum(sector);
  for(Int_t row = rowMin; row <= rowMax; row++) {              
    if (St_tpcPadConfigC::instance()->numberOfRows(sector) == 45) { // ! iTpx
      if ( ! StDetectorDbTpcRDOMasks::instance()->isRowOn(sector,row)) continue;
      if ( ! St_tpcAnodeHVavgC::instance()->livePadrow(sector,row))  continue;
    }
    Int_t io = (row <= St_tpcPadConfigC::instance()->numberOfInnerRows(sector)) ? 0 : 1;
    StTpcLocalSectorCoordinate xyzW(xOnWire, yOnWire, zOnWire, sector, row);
    static StTpcPadCoordinate Pad;
    transform(xyzW,Pad,kFALSE,kFALSE); // don't use T0, don't use Tau
    Float_t bin = Pad.timeBucket();//L  - 1; // K
    Int_t binT = TMath::Nint(bin); //L bin;//K TMath::Nint(bin);// J bin; // I TMath::Nint(bin);
    if (binT < 0 || binT >= NoOfTimeBins) continue;
    Double_t dT = bin -  binT + St_TpcResponseSimulatorC::instance()->T0offset(); 
    dT += (row <= St_tpcPadConfigC::instance()->numberOfInnerRows(sector)) ? 
      St_TpcResponseSimulatorC::instance()->T0offsetI() : 
      St_TpcResponseSimulatorC::instance()->T0offsetO();
    if (sigmaJitterT) dT += gRandom->Gaus(0,sigmaJitterT);  // #1
#if 1
    Double_t dely      = {transform.yFromRow(sector,row)-yOnWire};    
    Double_t localYDirectionCoupling = mChargeFraction[io][sector-1]->GetSaveL(&dely);
#else
    Int_t idWire = TMath::Abs(TMath::Nint((transform.yFromRow(sector,row)-yOnWire)/anodeWirePitch));
    if (idWire > 6) continue;
    Double_t localYDirectionCoupling = mLocalYDirectionCoupling[io][sector-1][idWire];
#endif
    if (ClusterProfile) {
      checkList[io][10]->Fill(TrackSegmentHits.xyzG.position().z(),localYDirectionCoupling);
#ifdef __LASERINO__
      SecRow[14]->Fill(sector,r,localYDirectionCoupling);
#endif /* __LASERINO__ */
    }
    if(localYDirectionCoupling < minSignal) continue;
    Float_t padX = Pad.pad();
    Int_t CentralPad = TMath::Nint(padX);
    if (CentralPad < 1) continue;
    Int_t PadsAtRow = St_tpcPadConfigC::instance()->numberOfPadsAtRow(sector,row);
    if(CentralPad > PadsAtRow) continue;
    Int_t DeltaPad = TMath::Nint(mPadResponseFunction[io][sector-1]->GetXmax()) + 1;
    Int_t padMin = TMath::Max(CentralPad - DeltaPad ,1);
    Int_t padMax = TMath::Min(CentralPad + DeltaPad ,PadsAtRow);
    Int_t Npads = TMath::Min(padMax-padMin+1, kPadMax);
    Double_t xPadMin = padMin - padX;
    static Double_t XDirectionCouplings[kPadMax];
    static Double_t TimeCouplings[kTimeBacketMax];
    mPadResponseFunction[io][sector-1]->GetSaveL(Npads,xPadMin,XDirectionCouplings);
    //	      Double_t xPad = padMin - padX;
    for(Int_t pad = padMin; pad <= padMax; pad++) {
      Double_t gain = QAv*mGainLocal;
      Double_t dt = dT;
      //		if (St_tpcPadConfigC::instance()->numberOfRows(sector) ==45 && ! TESTBIT(m_Mode, kGAINOAtALL)) { 
      if (! TESTBIT(m_Mode, kGAINOAtALL)) { 
	gain   *= St_tpcPadGainT0BC::instance()->Gain(sector,row,pad);
	if (gain <= 0.0) continue;
	dt -= St_tpcPadGainT0BC::instance()->T0(sector,row,pad);
      }
      if (ClusterProfile) {
	checkList[io][12]->Fill(TrackSegmentHits.xyzG.position().z(),gain);
	hist[4][1]->Fill(sector,row,gain);
#ifdef __LASERINO__
	SecRow[8]->Fill(sector,row,gain);
#endif /* __LASERINO__ */
      }
      //		Double_t localXDirectionCoupling = localXDirectionCouplings[pad-padMin];
      Double_t localXDirectionCoupling = gain*XDirectionCouplings[pad-padMin];
      if (localXDirectionCoupling < minSignal) continue;
      if (ClusterProfile) {
	checkList[io][13]->Fill(TrackSegmentHits.xyzG.position().z(),localXDirectionCoupling);
      }
      Double_t XYcoupling = localYDirectionCoupling*localXDirectionCoupling;
      if (ClusterProfile) {
	checkList[io][14]->Fill(TrackSegmentHits.xyzG.position().z(),XYcoupling);
      }
      if(XYcoupling < minSignal)  continue;
      Int_t bin_low  = TMath::Max(0             ,binT + TMath::Nint(dt+mShaperResponse->GetXmin()-0.5));
      Int_t bin_high = TMath::Min(NoOfTimeBins-1,binT + TMath::Nint(dt+mShaperResponse->GetXmax()+0.5));
      Int_t index = NoOfTimeBins*((row-1)*NoOfPads+pad-1)+bin_low;
      Int_t Ntbks = TMath::Min(bin_high-bin_low+1, kTimeBacketMax);
      Double_t tt = -dt + (bin_low - binT);
      mShaperResponse->GetSaveL(Ntbks,tt,TimeCouplings);
      for(Int_t itbin=bin_low;itbin<=bin_high;itbin++, index++){
	Double_t signal = XYcoupling*TimeCouplings[itbin-bin_low];
	if (signal < minSignal)  continue;
#ifdef __DEBUG__
	static Int_t iBreak = 0;
	if (TMath::IsNaN(signal) || TMath::IsNaN(SignalSum[index].Sum)) {
	  iBreak++;
	}
#endif
	TotalSignalInCluster += signal;
	SignalSum[index].Sum += signal;
	if (ClusterProfile) {
	  if (pad >= pad0 && pad < pad0 + kPadMax && 
	      itbin >= tbk0 &&  itbin < tbk0 + kTimeBacketMax) {
	    padsdE[pad-pad0]   += signal;
	    tbksdE[itbin-tbk0] += signal;
	  }
	}
	rowsdE[row-1]     += signal;
	rowsdEH[row-1]    += signal;
	if ( TrackSegmentHits.TrackId ) {
	  if (! SignalSum[index].TrackId ) SignalSum[index].TrackId = TrackSegmentHits.TrackId;
	  else  // switch TrackId, works only for 2 tracks, more tracks ?
	    if ( SignalSum[index].TrackId != TrackSegmentHits.TrackId && SignalSum[index].Sum < 2*signal) 
	      SignalSum[index].TrackId = TrackSegmentHits.TrackId;
	}
#ifdef __DEBUG__
	if (Debug() > 13 && (SignalSum[index].Sum > 0 || ! TMath::Finite(SignalSum[index].Sum)) ) {
	  LOG_INFO << "simu row = " << TrackSegmentHits.tpc_hitC->volume_id%100 << "\tR/P/T/I = " << row << " /\t" << pad << " /\t" << itbin << " /\t" << index 
		   << "\tSum/Adc/TrackId = " << SignalSum[index].Sum << " /\t" 
		   << SignalSum[index].Adc << " /\t" << SignalSum[index].TrackId 
		   << "\tsignal = " << signal 
		   << "\trow Min/Max = " << rowMin << "/" << rowMax 
		   << endm;
	  if (! TMath::Finite(SignalSum[index].Sum)) {
	    LOG_INFO << "Not Finite" << endm;
	  }
	}
#endif /* __DEBUG__ */
      } // time 
    } // pad limits
  } // row limits
}
//________________________________________________________________________________
Double_t StTpcRSMaker::dEdxCorrection(HitPoint_t &TrackSegmentHits) {
  Double_t dEdxCor = 1;
  if (m_TpcdEdxCorrection) {
    //    dEdxCor = -1;
    Double_t dStep =  TMath::Abs(TrackSegmentHits.tpc_hitC->ds);
    dEdxY2_t CdEdx;
    memset (&CdEdx, 0, sizeof(dEdxY2_t));
    CdEdx.DeltaZ = 5.2; 
    CdEdx.QRatio = -2;
    CdEdx.QRatioA = -2.;
    CdEdx.QSumA = 0;
    CdEdx.sector = TrackSegmentHits.Pad.sector(); 
    CdEdx.row    = TrackSegmentHits.Pad.row();
    CdEdx.pad    = TMath::Nint(TrackSegmentHits.Pad.pad());
    CdEdx.edge   = CdEdx.pad;
    if (CdEdx.edge > 0.5*St_tpcPadConfigC::instance()->numberOfPadsAtRow(CdEdx.sector,CdEdx.row)) 
      CdEdx.edge += 1 - St_tpcPadConfigC::instance()->numberOfPadsAtRow(CdEdx.sector,CdEdx.row);
    CdEdx.F.dE     = 1;
#if 0
    CdEdx.dCharge= tpcHit->chargeModified() - tpcHit->charge();
    Int_t p1 = tpcHit->minPad();
    Int_t p2 = tpcHit->maxPad();
    Int_t t1 = tpcHit->minTmbk();
    Int_t t2 = tpcHit->maxTmbk();
    CdEdx.rCharge=  0.5*m_TpcdEdxCorrection->Adc2GeV()*TMath::Pi()/4.*(p2-p1+1)*(t2-t1+1);
    if (TESTBIT(m_Mode, kEmbeddingShortCut) && 
	(tpcHit->idTruth() && tpcHit->qaTruth() > 95)) CdEdx.lSimulated = tpcHit->idTruth();
#endif
    CdEdx.F.dx     = dStep;
    CdEdx.xyz[0] = TrackSegmentHits.coorLS.position().x();
    CdEdx.xyz[1] = TrackSegmentHits.coorLS.position().y();
    CdEdx.xyz[2] = TrackSegmentHits.coorLS.position().z();
    Double_t probablePad = St_tpcPadConfigC::instance()->numberOfPadsAtRow(CdEdx.sector,CdEdx.row)/2;
    Double_t pitch = (CdEdx.row <= St_tpcPadConfigC::instance()->numberOfInnerRows(CdEdx.sector)) ?
      St_tpcPadConfigC::instance()->innerSectorPadPitch(CdEdx.sector) :
      St_tpcPadConfigC::instance()->outerSectorPadPitch(CdEdx.sector);
    Double_t PhiMax = TMath::ATan2(probablePad*pitch, St_tpcPadConfigC::instance()->radialDistanceAtRow(CdEdx.sector,CdEdx.row));
    CdEdx.PhiR   = TMath::ATan2(CdEdx.xyz[0],CdEdx.xyz[1])/PhiMax;
    CdEdx.xyzD[0] = TrackSegmentHits.dirLS.position().x();
    CdEdx.xyzD[1] = TrackSegmentHits.dirLS.position().y();
    CdEdx.xyzD[2] = TrackSegmentHits.dirLS.position().z();
    CdEdx.ZdriftDistance = CdEdx.xyzD[2];
    CdEdx.zG      = CdEdx.xyz[2];
    if (St_trigDetSumsC::instance())	CdEdx.Zdc     = St_trigDetSumsC::instance()->zdcX();
    CdEdx.ZdriftDistance = TrackSegmentHits.coorLS.position().z(); // drift length
    St_tpcGas *tpcGas = m_TpcdEdxCorrection->tpcGas();
    if (tpcGas)
      CdEdx.ZdriftDistanceO2 = CdEdx.ZdriftDistance*(*tpcGas)[0].ppmOxygenIn;
    if (! m_TpcdEdxCorrection->dEdxCorrection(CdEdx)) {
      dEdxCor = CdEdx.F.dE;
    }
  }
  return dEdxCor;
}
//________________________________________________________________________________
#undef PrPP
//________________________________________________________________________________
// $Id: StTpcRSMaker.cxx,v 1.89 2019/05/22 21:30:58 fisyak Exp $
// $Log: StTpcRSMaker.cxx,v $
// Revision 1.89  2019/05/22 21:30:58  fisyak
// Fix bug3390 (thanks to Irakli), add St_TpcAdcCorrectionMDF
//
// Revision 1.88  2019/04/29 20:11:21  fisyak
// Fix for TrackDirection, add extra correction for the 1st pad row
//
// Revision 1.87  2019/04/18 14:02:23  fisyak
// Keep hits with bad dE/dx information, revisit handling of pedRMS for Tpx and iTPC
//
// Revision 1.86  2018/12/16 14:26:30  fisyak
// Switch off DEBUG, use local position for phi correction
//
// Revision 1.85  2018/12/09 23:22:59  fisyak
// Reshape
//
// Revision 1.84  2018/11/29 22:19:49  fisyak
// Restore __STOPPED_ELECTRONS__, split for Inner and Outer sectors, adjusted gain for Run XVIII
//
// Revision 1.83  2018/11/20 19:51:15  fisyak
// Temporarely disable __STOPPED_ELECTRONS__ to check effect of this on no. of primary tracks for 2010 AuAu200 sample
//
// Revision 1.82  2018/11/05 01:05:19  fisyak
// Replace assert to error message
//
// Revision 1.81  2018/11/01 13:27:20  fisyak
// Synchronize mCutEle with GEANT3 tracking media setting for TPCE_SENSITIVE_GAS, bug#3369
//
// Revision 1.80  2018/10/17 20:45:28  fisyak
// Restore update for Run XVIII dE/dx calibration removed by Gene on 08/07/2018
//
// Revision 1.77  2018/02/18 21:08:54  perev
// Move back DSmirnov correction
//
// Revision 1.75  2017/02/14 23:40:35  fisyak
// Add new Table to correct dE/dx pad dependence, 2017 dAu20-62 calibration
//
// Revision 1.74  2016/12/29 16:30:56  fisyak
// make switch to account __STOPPED_ELECTRONS__
//
// Revision 1.73  2016/12/19 15:22:39  fisyak
// Fix typo
//
// Revision 1.72  2016/12/19 15:20:20  fisyak
// Fix bug 3268: add missing correction for TpcAvgPowerSupply, add treatment for stopping electrons and gammas
//
// Revision 1.71  2016/09/18 22:45:25  fisyak
// Clean up, add Heed model, adjust for new StTpcdEdxCorrections
//
// Revision 1.71  2015/10/08 13:58:45  fisyak
// Add requirement for Check Plots for TTree file
//
// Revision 1.70  2015/07/19 22:14:07  fisyak
// Clean up __PAD_BLOCK__, recalculate no. of real hits in g2t_track n_tpc_hitC (excluding pseudo pad row), add current and accumulated charge in dE/dx correction
//
// Revision 1.69  2014/10/21 15:33:48  fisyak
// Clean up, fix bug found by gcc482
//
// Revision 1.68  2014/07/27 13:26:26  fisyak
// Add cast for c++11 option
//
// Revision 1.67  2013/02/01 15:54:09  fisyak
// Add handle for separate Inner and Outer sector time off set
//
// Revision 1.66  2012/11/13 20:46:16  fisyak
// Add wider Voltage range for accepted clusteds (-500V) than for dEdx calculation
//
// Revision 1.65  2012/10/23 20:08:57  fisyak
// Add corrections for iTpx upgrade
//
// Revision 1.64  2012/09/27 19:17:02  fisyak
// Fix missing declaration
//
// Revision 1.63  2012/09/27 16:14:43  fisyak
// Change debug print out scheme
//
// Revision 1.62  2012/09/13 21:54:43  fisyak
// replace elsif by else and if
//
// Revision 1.61  2012/09/13 21:02:52  fisyak
// Corrections for iTpx
//
// Revision 1.60  2012/06/04 15:14:18  fisyak
// restore old hack for dN/dx table to fix bug #2347
//
// Revision 1.59  2012/05/07 15:36:22  fisyak
// Remove hardcoded TPC parameters
//
// Revision 1.58  2012/04/03 14:05:18  fisyak
// Speed up using  GetSaveL (__PAD_BLOCK__), sluggish shape histograms, Heed electron generation
//
// Revision 1.57  2011/12/23 16:38:25  fisyak
// Remove __DEBUG__ and __ClusterProfile__ from default, reduce arrays and add check for bounds
//
// Revision 1.55  2011/12/20 21:09:56  fisyak
// change defaults: shark measurements: old default => 46.6%, wire histograms => 38.9%, wire map => 12.5 + 10.2, pad block => 15%
//
// Revision 1.54  2011/12/13 17:23:22  fisyak
// remove YXTProd, add WIREHISTOGRAM and WIREMAP, use particle definition from StarClassLibrary
//
// Revision 1.53  2011/10/14 23:27:51  fisyak
// Back to standard version
//
// Revision 1.51  2011/09/21 15:34:31  fisyak
// Restore K3IP parameter
//
// Revision 1.50  2011/09/18 22:39:48  fisyak
// Extend dN/dx table (H.Bichsel 09/12/2011) to fix bug #2174 and #2181, clean-up
//
// Revision 1.49  2011/04/05 20:55:02  fisyak
// Fix betaMin calculations (thanx VP)
//
// Revision 1.48  2011/03/17 14:29:31  fisyak
// Add extrapolation in region beta*gamma < 0.3
//
// Revision 1.47  2011/03/13 15:46:44  fisyak
// Replace step function by interpolation for dN/dx versus beta*gamma
//
// Revision 1.46  2011/02/23 20:14:31  perev
// Hack to avoid sqrt(-)
//
// Revision 1.45  2010/12/16 15:36:07  fisyak
// cut hits outside time buckets range
//
// Revision 1.44  2010/12/01 20:59:46  fisyak
// Remove special treatment for delta-electrons, this will cause that IdTruth for cluster will be degradated because charge from delta-electrons will be accounted with delta-electons track Id but not with original particle Id as was before
//
// Revision 1.43  2010/10/28 23:42:34  fisyak
// extra t0 off set for Altro chip
//
// Revision 1.42  2010/10/22 18:13:33  fisyak
// Add fix from Lokesh AuAu7 2010 embdedding
//
// Revision 1.41  2010/09/01 23:12:01  fisyak
// take out __ClusterProfile__
//
// Revision 1.40  2010/06/14 23:34:26  fisyak
// Freeze at Version V
//
// Revision 1.39  2010/05/24 16:11:03  fisyak
// Return back to time simulation for each pad, organize parameters into TpcResponseSimulator table
//
// Revision 1.38  2010/04/24 19:58:54  fisyak
// swap shift sign
//
// Revision 1.37  2010/04/24 15:56:32  fisyak
// Jan found shift in z by one time bucket
//
// Revision 1.36  2010/04/20 13:56:24  fisyak
// Switch off __ClusterProfile__
//
// Revision 1.35  2010/04/16 19:29:35  fisyak
// W is in eV now
//
// Revision 1.34  2010/04/01 22:17:06  fisyak
// Add checking for TPC is switched off at all and stop if so
//
// Revision 1.33  2010/03/22 23:45:05  fisyak
// Freeze version with new parameters table
//
// Revision 1.32  2010/03/16 19:41:46  fisyak
// Move diffusion and sec/row correction in DB, clean up
//
// Revision 1.31  2010/03/02 21:10:27  fisyak
// Make aware about TpcRDOMasks
//
// Revision 1.30  2010/02/26 18:53:33  fisyak
// Take longitudinal Diffusion from Laser track fit, add Gating Grid
//
// Revision 1.29  2010/02/16 00:21:23  fisyak
// Speed up by a factor 3.5 by ignoring individual pad T0
//
// Revision 1.28  2010/01/26 19:47:25  fisyak
// Include dE/dx calibration and distortions in the simulation
//
// Revision 1.27  2009/11/25 21:32:52  fisyak
// Comment out cluster profile histograms
//
// Revision 1.26  2009/11/03 22:38:53  fisyak
// Freeze version rcf9108.J
//
// Revision 1.25  2009/10/30 21:12:00  fisyak
// Freeze version rcf9108.F, Clean up
//
// Revision 1.24  2009/10/26 18:50:58  fisyak
// Clean up from Bichel's stuff
//
// Revision 1.23  2009/10/12 23:54:12  fisyak
// Restore T0Jitter, remove differential in Tpx signal
//
// Revision 1.22  2009/10/03 21:29:09  fisyak
// Clean up, move all TpcT related macro into StTpcMcAnalysisMaker
//
// Revision 1.21  2009/10/01 14:53:06  fisyak
// Add T0Jitter
//
// Revision 1.20  2009/09/27 01:30:48  fisyak
// Restate T0Jitter
//
// Revision 1.19  2009/09/27 01:24:58  fisyak
// Restate T0Jitter
//
// Revision 1.18  2009/09/21 13:20:39  fisyak
// Variant O4, no mSigmaJitter, 100 keV
//
// Revision 1.17  2009/09/01 15:06:44  fisyak
// Version N
//
// Revision 1.16  2009/08/25 20:39:40  fisyak
// Variant K
//
// Revision 1.15  2009/08/25 15:45:58  fisyak
// Version J
//
// Revision 1.14  2009/08/24 20:16:41  fisyak
// Freeze with new Altro parameters
//
// Revision 1.13  2008/12/29 15:24:54  fisyak
// Freeze ~/WWW/star/Tpc/TpcRS/ComparisonMIP31
//
// Revision 1.12  2008/12/18 23:06:37  fisyak
// Take care about references to TGiant
//
// Revision 1.11  2008/12/12 21:41:41  fisyak
// Freeze
//
// Revision 1.10  2008/10/06 19:10:23  fisyak
// BichlePPMIP3
//
// Revision 1.9  2008/10/03 20:25:29  fisyak
// Version BichselMIP2
//
// Revision 1.8  2008/08/19 16:01:15  fisyak
// Version 21
//
// Revision 1.7  2008/08/18 15:54:25  fisyak
// Version 20
//
// Revision 1.6  2008/07/30 23:53:19  fisyak
// Freeze
//
// Revision 1.5  2008/07/18 16:22:50  fisyak
// put a factor 2.5 for tauIntegration
//
// Revision 1.3  2008/06/25 20:02:32  fisyak
// The first set of parametrs for Altro, Remove gains for the moment
//
// Revision 1.2  2008/06/19 22:45:43  fisyak
// Freeze problem with TPX parameterization
//
// Revision 1.1.1.1  2008/04/28 14:39:47  fisyak
// Start new Tpc Response Simulator
//
// Revision 1.20  2008/04/24 10:42:03  fisyak
// Fix binning issues
//
// Revision 1.19  2008/04/04 15:00:11  fisyak
// Freeze before shaper modifications
//
// Revision 1.18  2005/02/07 21:40:31  fisyak
// rename antique TGeant3 to TGiant3
//
// Revision 1.17  2005/01/26 23:28:38  fisyak
// Check boundary for sorted tpc_hit array
//
// Revision 1.16  2005/01/26 21:45:31  fisyak
// Freeze correction made in June
//
// Revision 1.14  2004/06/04 17:09:02  fisyak
// Change tau in Chaper and OmegaTau for gas
//
// Revision 1.13  2004/05/29 21:16:27  fisyak
// Fix pad direction, add sorting for ADC/cluster nonlinearity, replace product by sum of logs
//
// Revision 1.12  2004/05/17 19:45:08  fisyak
// Clean up, add pseudo padrows
//
// Revision 1.11  2004/05/05 17:41:52  fisyak
// Take K3 from E.Mathieson book
//
// Revision 1.10  2004/05/04 13:39:06  fisyak
// Add TF1
//
// Revision 1.9  2004/05/02 20:54:18  fisyak
// fix t0 offset
//
// Revision 1.8  2004/04/22 01:05:03  fisyak
// Freeze the version before modification parametrization for K3
//
// Revision 1.7  2004/04/12 14:30:07  fisyak
// Propagate cluster as a whole
//
// Revision 1.6  2004/04/06 01:50:13  fisyak
// Switch from Double_t to Float_t for sum
//
// Revision 1.5  2004/03/30 19:30:04  fisyak
// Add Laser
//
// Revision 1.4  2004/03/21 19:00:43  fisyak
// switch to GEANT step length
//
// Revision 1.3  2004/03/20 17:57:15  fisyak
// Freeze the version of PAI model
//
// Revision 1.2  2004/03/17 20:47:43  fisyak
// Add version with TrsCluster TTree
//
// Revision 1.1.1.1  2004/03/05 20:51:25  fisyak
// replacement for Trs
//
