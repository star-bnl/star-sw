// $Id: StdEdxY2Maker.cxx,v 1.23 2004/04/04 22:33:54 fisyak Exp $
#define Mip 2002
#define PadSelection
#define  AdcCorrection
//#define  AdcHiststpcPresos
#define  PressureScaleFactor
#define  TpcSecRow
#define  DriftDistanceCorrection
//#define  XYZcheck      
//#define  SpaceChargeStudy
//#define  GetTpcGainMonitor
//#define  ProbabilityPlot
#include <Stiostream.h>
#include <time.h>
#include "StdEdxY2Maker.h"
// ROOT
#include "TMinuit.h"
#include "TSystem.h"
#include "TMath.h"
#include "TH2.h"
#include "TH3.h"
#include "TStyle.h"
#include "TF1.h"
#include "TProfile.h"
#include "TProfile2D.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TCanvas.h"
#include "TRandom.h"
// StUtilities
#include "StMagF/StMagF.h"
#include "StMessMgr.h" 
#include "BetheBloch.h"
#include "StBichsel/Bichsel.h"
// St_base, StChain
#include "StBFChain.h"
#include "St_DataSetIter.h"
#include "TTableSorter.h"
// tables
#include "tables/St_tpt_track_Table.h"
#include "tables/St_stk_track_Table.h"
#include "tables/St_dst_dedx_Table.h"
#include "tables/St_dst_track_Table.h"
#include "tables/St_TpcSecRowCor_Table.h"
#include "tables/St_tpcCorrection_Table.h"
#include "tables/St_dst_event_summary_Table.h"
#include "tables/St_tpcGas_Table.h"
#include "tables/St_tpcPressure_Table.h"
#include "tables/St_trigDetSums_Table.h"
#include "tables/St_tpcGainMonitor_Table.h"
// global
#include "StDetectorId.h"
#include "StDedxMethod.h"
// StarClassLibrary
#include "StThreeVector.hh" 
#include "StHelixD.hh"
#include "StTimer.hh"
// StDb
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StDbUtilities/StTpcLocalSectorCoordinate.hh"
#include "StDbUtilities/StTpcLocalCoordinate.hh"
#include "StDbUtilities/StTpcPadCoordinate.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StTpcDb/StTpcDb.h"
#include "StTpcDb/StTpcPadPlaneI.h"
// StEvent
#include "StEventTypes.h"
#include "StProbPidTraits.h"
const static StPidParticle NHYPS = kPidTriton;
#include "dEdxTrack.h"
extern  Double_t SumSeries(const Double_t &X,const Int_t &N,const Double_t *params);
extern  Double_t SumSeries(const Double_t &X,const Int_t &N,const Float_t *params);
enum ESector  {kTpcOuter = 0, kTpcInner = 1};
static Int_t  NdEdx = 0;
static dEdx_t CdEdx[60]; // corrected
static dEdx_t FdEdx[60]; // fit
static dEdx_t dEdxS[60]; // dEdx sorted

const static UInt_t NoSector =   24;
const static UInt_t NoRow    =   45;
const static  Int_t NoPads   =  185;
const static  Int_t NTimeBins = 513;
const static Double_t pMomin = 0.4; // range for dE/dx calibration
const static Double_t pMomax = 0.5;
const static Double_t GeV2keV = TMath::Log(1.e-6);
const static Double_t TrackLengthCut = 20.;
// Histograms
static TProfile2D *ETA = 0;
static TH3D *ETA3 = 0;
#ifdef SpaceChargeStudy
static TProfile2D *SpaceCharge = 0, *SpaceChargeU = 0, *SpaceChargeT = 0;
static TH2D *Space2Charge = 0, *Space2ChargeU = 0, *Space2ChargeT = 0;
static TH1D *TimeShift = 0;
static TH3D *Space3Charge = 0, *Space3ChargePRZ = 0,*Space3ChargeShifted = 0;
#endif // SpaceChargeStudy
static TH2D *Time = 0, *TimeP = 0, *TimeC = 0;
static TH3D *Pressure = 0, *PressureC = 0, *PressureA = 0;
#ifdef CORRELATION
static TH2D *corrI = 0, *corrO = 0, *corrI2 = 0, *corrO2 = 0, *corrI5 = 0, *corrO5 = 0;
static TH2D *corrIw = 0, *corrOw = 0, *corrI2w = 0, *corrO2w = 0, *corrI5w = 0, *corrO5w = 0;
static TH1D *corrI1w = 0, *corrO1w = 0;
#endif // CORRELATION
#ifdef XYZcheck
static TH3D *XYZ = 0, *XYZbad = 0;
#endif // XYZcheck
static TProfile2D  *dYrow = 0;
//static TH3D  *dYrow = 0;
#ifdef ZBGX
static TH3D **zbgx = 0;
#endif // ZBGX
static TH3D *MulRow = 0;
static TH3D *MulRowC = 0;
static TH2D *GainMonitor = 0;
static TH3D *SecRow3 = 0, *SecRow3C = 0, *SecRow3A = 0, *Z3 = 0, *Z3A = 0, *Z3C = 0, *Z3O = 0, *Z3OC = 0, *Z3OA = 0;
#ifdef AdcHistos
static TH2D *AdcI = 0, *AdcO = 0, *AdcIC = 0, *AdcOC = 0, *Adc3I = 0, *Adc3O = 0, *Adc3IC = 0, *Adc3OC = 0;
static TH3D *AdcIZP = 0, *AdcOZP = 0, *AdcIZN = 0, *AdcOZN = 0;
static TH2D **Adc3Ip = 0, **Adc3Op = 0;
#endif // AdcHistos
static TH2D *ZdcCP = 0, *BBCP = 0, *L0P = 0, *MultiplicityPI = 0, *MultiplicityPO = 0;
#ifdef Mip 
static TH3D *SecRow3Mip = 0;
#endif // Mip
static TH1D *hdEI = 0, *hdEUI = 0, *hdERI = 0, *hdEPI = 0, *hdETI = 0, *hdESI = 0, *hdEZI = 0, *hdEMI = 0;
static TH1D *hdEO = 0, *hdEUO = 0, *hdERO = 0, *hdEPO = 0, *hdETO = 0, *hdESO = 0, *hdEZO = 0, *hdEMO = 0;
static TH2D *Points =  0, *Points70 =  0;
static TH2D *PointsB = 0, *Points70B = 0; 
static TH2D *TPoints = 0, *TPoints70 = 0;
static TH2D *TPointsB = 0, *TPoints70B = 0;
static TH2D *TdEdxPF = 0, *TdEdxP70 = 0;
static TH2D *hist70[NHYPS][2], *histz[NHYPS][2];
static TH2D *hist70B[NHYPS][2], *histzB[NHYPS][2];
static TProfile *histB[NHYPS][2], *histBB[NHYPS][2]; 
static TH2D *FitPull = 0, *Pull70 = 0;
static TTree *ftree = 0;
static dEdxTrack *ftrack = 0;
static TH2D *ffitZ[NHYPS],  *ffitP[NHYPS], *ffitZU = 0, *ffitZU3 = 0, *ffitZA = 0;
const static Int_t Nlog2dx = 140;
const static Double_t log2dxLow = 0.0, log2dxHigh = 3.5;
static TH1D *Multiplicity; // mult rate 
static TH2D *Zdc  = 0; // ZdcEastRate versus ZdcWestRate
static TH1D *ZdcC = 0; // ZdcCoincidenceRate
static TH1D *BBC   = 0; // BbcCoincidenceRate
static TH1D *L0   = 0; // L0RateToRich
static TProfile *Center = 0, *Height = 0, *Width = 0, *BarPressure = 0, *CenterPressure = 0;
static TProfile *inputTPCGasPressure = 0, *nitrogenPressure = 0, *gasPressureDiff = 0, *inputGasTemperature = 0;
static TProfile *outputGasTemperature = 0, *flowRateArgon1 = 0, *flowRateArgon2 = 0, *flowRateMethane = 0;
static TProfile *percentMethaneIn = 0, *ppmOxygenIn = 0, *flowRateExhaust = 0;
static TProfile *ppmWaterOut = 0, *ppmOxygenOut = 0, *flowRateRecirculation = 0;
static TH2D *inputTPCGasPressureP = 0, *nitrogenPressureP = 0, *gasPressureDiffP = 0, *inputGasTemperatureP = 0;
static TH2D *outputGasTemperatureP = 0, *flowRateArgon1P = 0, *flowRateArgon2P = 0, *flowRateMethaneP = 0;
static TH2D *percentMethaneInP = 0, *ppmOxygenInP = 0, *flowRateExhaustP = 0;
static TH2D *ppmWaterOutP = 0, *ppmOxygenOutP = 0, *flowRateRecirculationP = 0;
#ifdef ProbabilityPlot
static TH3D *Prob = 0;
#endif
static TH2D *dx0dx = 0;
static TH3D *dXdE  = 0, *dXdEA  = 0, *dXdEC  = 0;
ClassImp(StdEdxY2Maker);

//_____________________________________________________________________________
StdEdxY2Maker::StdEdxY2Maker(const char *name):
  StMaker(name), 
  m_Bichsel(0), 
  m_TpcSecRow(0),
  m_drift(0), 
  m_Multiplicity(0),
  m_AdcCorrection(0),
  m_zCorrection(0),
  m_dXCorrection(0),
  m_TpcdEdxCor(0),
  m_TpcLengthCorrection(0),
  m_tpcGas(0),
  m_tpcPressure(0), 
  m_trigDetSums(0),
  m_trig(0),
  m_Simulation(kFALSE), 
  m_InitDone (kFALSE), 
  m_tpcGainMonitor(0),
  m_ClusterFinder(0),
  m_Calibration(0), 
  m_DoNotCorrectdEdx(0),
  m_Minuit(0)
{
  if (!m_Minuit) m_Minuit = new TMinuit(2);
  for (int i = 0; i < 24; i++) 
    for (int j = 0; j < 45; j++) 
      for (Int_t k = 0; k < 3; k++) 
	mRowPosition[i][j][k] = 0;
}
//_____________________________________________________________________________
StdEdxY2Maker::~StdEdxY2Maker(){}
//_____________________________________________________________________________
Int_t StdEdxY2Maker::Init(){
  m_Simulation  = m_Mode < 0;
  if (m_Simulation) 
    gMessMgr->Warning() << "StdEdxY2Maker:: use Simulation mode (no calibration) " << endm;
  m_Calibration = m_Mode%10;
  m_DoNotCorrectdEdx = TMath::Abs(m_Mode/100)%10;
  if (m_DoNotCorrectdEdx) gMessMgr->Warning() << "StdEdxY2Maker:: use 'DoNotCorrectdEdx' mode" << endm;
  if (! m_Calibration) gMessMgr->Warning() << "StdEdxY2Maker:: Calibration Mode" << m_Calibration << endm;
  if (! m_Simulation)  {
    m_ClusterFinder = (TMath::Abs(m_Mode)/10)%10;
    if (! m_ClusterFinder) gMessMgr->Warning() << "StdEdxY2Maker:: use old Cluster Finder parameterization" << endm;
  }
  m_Bichsel = new Bichsel();
  
  if (m_Calibration != 0) {// calibration mode
    StBFChain *chain = dynamic_cast<StBFChain*>(GetChain());
    TFile *f = 0;
    if (chain) f = chain->GetTFile();
    if (f) {
      f->cd();
      if (! m_DoNotCorrectdEdx) {
#ifdef XYZcheck
	XYZ = new THx3D("XYZ","xyz for clusters",20,-200,200,20,-200,200,20,-200,200);
	XYZbad = new TH3D("XYZbad","xyz for clusters with mismatched sectors",
			  20,-200,200,20,-200,200,20,-200,200);
#endif // XYZcheck
	Z3  = new TH3D("Z3",
		       "log(dEdx/Pion) versus row and Drift Distance",
		       NoRow,1., NoRow+1,100,0.,200., 200,-5.,5.);
	Z3A = new TH3D("Z3A",
		       "log(dEdx/Pion) just after corection versus row and Drift Distance",
		       NoRow,1., NoRow+1,100,0.,200., 200,-5.,5.);
	Z3C = new TH3D("Z3C",
		       "log(dEdx/Pion) corrected versus row and Drift Distance",
		       NoRow,1., NoRow+1,100,0.,200., 200,-5.,5.);
	Z3O = new TH3D("Z3O",
		       "log(dEdx/Pion) versus row and (Drift)*ppmO2In",
		       NoRow,1., NoRow+1,100,0.,1.e4, 200,-5.,5.);
	Z3OA= new TH3D("Z3OA",
		       "log(dEdx/Pion) just after correction versus row and (Drift)*ppmO2In",
		       NoRow,1., NoRow+1,100,0.,1.e4, 200,-5.,5.);
	Z3OC= new TH3D("Z3OC",
		       "log(dEdx/Pion) corrected versus row and (Drift)*ppmO2In",
		       NoRow,1., NoRow+1,100,0.,1.e4, 200,-5.,5.);
	ETA   = new TProfile2D("ETA",
			       "log(dEdx/Pion) versus row and #{eta}",
			       NoRow,1., NoRow+1, 135,-2.25,2.25);
	ETA3  = new TH3D("ETA3",
			 "log(dEdx/Pion) versus row and #{eta}",
			 NoRow,1., NoRow+1, 135,-2.25,2.25, 200,-5.,5.);
	SecRow3= new TH3D("SecRow3","<log(dEdx/Pion)> (uncorrected) versus sector and row",
			  NoSector,1., NoSector+1, NoRow,1., NoRow+1, 200,-5.,5.);
	SecRow3->SetXTitle("Sector number");
	SecRow3->SetYTitle("Row number");
	SecRow3C= new TH3D("SecRow3C","<log(dEdx/Pion)> (corrected) versus sector and row",
			   NoSector,1., NoSector+1, NoRow,1., NoRow+1, 200,-5.,5.);
	SecRow3C->SetXTitle("Sector number");
	SecRow3C->SetYTitle("Row number");
	SecRow3A= new TH3D("SecRow3A","<log(dEdx/Pion)> (just after correction) versus sector and row",
			   NoSector,1., NoSector+1, NoRow,1., NoRow+1, 200,-5.,5.);
	SecRow3A->SetXTitle("Sector number");
	SecRow3A->SetYTitle("Row number");
	MultiplicityPI = new TH2D("MultiplicityPI","Multiplicity (log10) Inner",100,0,4,200,-5.,5.);
	MultiplicityPO = new TH2D("MultiplicityPO","Multiplicity (log10) Outer",100,0,4,200,-5.,5.);
#ifdef AdcHistos
	if (! m_Simulation) {
	AdcI    = new TH2D("AdcI",
			   "log10dE (keV measured) versus log10dE(Predicted) for Inner rows",
			   120,-5.7,-3.3,185,-7.2,-3.5);
	AdcO    = new TH2D("AdcO",
			   "log10dE (keV measured) versus log10dE(Predicted) for Outer rows",
			   150,-5.5,-2.5,165,-6.5,-3.0);
	AdcIC   = new TH2D("AdcIC",
			   "log10dE (keV measured corrected) versus log10dE(Predicted) for Inner rows",
			   120,-5.7,-3.3,185,-7.2,-3.5);
	AdcOC   = new TH2D("AdcOC",
			   "log10dE (keV measured corrected) versus log10dE(Predicted) for Outer rows",
			   150,-5.5,-2.5,165,-6.5,-3.0);
	Adc3I    = new TH2D("Adc3I",
			    "Uniq 3*sigma log10dE (keV measured) versus log10dE(Predicted) for Inner rows",
			    120,-5.7,-3.3,185,-7.2,-3.5);
	Adc3O    = new TH2D("Adc3O",
			    "Uniq 3*sigma log10dE (keV measured) versus log10dE(Predicted) for Outer rows",
			    150,-5.5,-2.5,165,-6.5,-3.0);
	Adc3IC   = new TH2D("Adc3IC",
			    "Uniq 3*sigma log10dE (keV measured corrected) versus log10dE(Predicted) for Inner rows",
			    120,-5.7,-3.3,185,-7.2,-3.5);
	Adc3OC   = new TH2D("Adc3OC",
			    "Uniq 3*sigma log10dE (keV measured corrected) versus log10dE(Predicted) for Outer rows",
			    150,-5.5,-2.5,165,-6.5,-3.0);
	Adc3Ip    = new TH2D*[NHYPS];
	Adc3Op    = new TH2D*[NHYPS];
	for (int hyp = 0; hyp < NHYPS; hyp++) {
	  TString nameP(StProbPidTraits::mPidParticleDefinitions[hyp]->name().data());
	  nameP.ReplaceAll("-","");
	  Adc3Ip[hyp] = new 
	    TH2D(Form("Adc3I%s",nameP.Data()), 
		 Form("%s Uniq 3*sigma log10dE (keV meas.cor.) versus log10dE(Predicted) for Inner rows",
		      nameP.Data()),
		 120,-5.7,-3.3,185,-7.2,-3.5);
	  Adc3Op[hyp] = new 
	    TH2D(Form("Adc3O%s",nameP.Data()), 
		 Form("%s Uniq 3*sigma log10dE (keV meas.cor.) versus log10dE(Predicted) for Outer rows",
		      nameP.Data()),
		 120,-5.7,-3.3,185,-7.2,-3.5);
	}
	AdcIZP    = new TH3D("AdcIZP","z (Positive measured) versus dE(Predicted) and Z for Inner rows",
			     200,0,200,100,0.,1.e4,200,-5.,5.);
	AdcOZP    = new TH3D("AdcOZP","z (Positive measured) versus dE(Predicted) and Z for Outer rows",
			     500,0,500,100,0.,1.e4,200,-5.,5.);
	AdcIZN    = new TH3D("AdcIZN","z (Positive measured) versus dE(Predicted) and Z for Inner rows",
			     200,0,200,100,0.,1.e4,200,-5.,5.);
	AdcOZN    = new TH3D("AdcOZN","z (Positive measured) versus dE(Predicted) and Z for Outer rows",
			     500,0,500,100,0.,1.e4,200,-5.,5.);
	}
#endif // AdcHistos
      }
      // trigDetSums histograms
      Zdc     = new TH2D("Zdc","ZdcEastRate versus ZdcWestRate (log10)",100,0,4,100,0,10);
      ZdcC    = new TH1D("ZdcC","ZdcCoincidenceRate (log10)",100,0,4);
      Multiplicity = new TH1D("Multiplicity","Multiplicity (log10)",100,0,4);
      BBC      = new TH1D("BBC","BbcCoincidenceRate (log10)",100,0,5);
      L0      = new TH1D("L0","L0RateToRich (log10)",100,0,2);
      if (! m_DoNotCorrectdEdx) {
	ZdcCP  = new TH2D("ZdcCP","ZdcCoincidenceRate (log10)",100,0,4,200,-5.,5.);
	BBCP   = new TH2D("BBCP","BbcCoincidenceRate (log10)",100,0,5,200,-5.,5.);
	L0P    = new TH2D("L0P","L0RateToRich (log10)",100,0,2,200,-5.,5.);
#ifdef Mip
	SecRow3Mip = new TH3D
	  ("SecRow3Mip",
	   "<log(dEdx/Pion)>/sigma (corrected) versus row and log2(dx) for MIP particle)",
	   NoRow,1., NoRow+1,Nlog2dx, log2dxLow, log2dxHigh, 200,-5,15);
#endif // Mip
	MulRow = new TH3D("MulRow","log(dEdx/Pion) versus log10(Multplicity) and row",
			  20,0.,4., NoRow,1.,NoRow+1,200,-5.,5.);
	MulRowC = new TH3D("MulRowC","log(dEdx/Pion) versus log10(Multplicity) and row corrected",
			   20,0.,4., NoRow,1.,NoRow+1,200,-5.,5.);
	dYrow = new TProfile2D("dYrow","log(dEdx/Pion) versus cluseer projection on the wire for given row",
			       100,-2.5,2.5, NoRow,1., NoRow+1);
// 	dYrow = new TH3D("dYrow",
// 			 "log(dEdx/Pion) versus track projection (pads) on the wire for given row",
// 			 100,-10.,10., NoRow,1., NoRow+1, 200,-5.,5.);
#ifdef CORELATION
	corrI   = new TH2D("corrI","Correlation for Inner Sector for pair of nearest rows",
			   100,-10.,10., 100,-10.,10.);
	corrO   = new TH2D("corrO","Correlation for Outer Sector for pair of nearest rows",
			   100,-10.,10., 100,-10.,10.);
	corrI2   = new TH2D("corrI2","Correlation for Inner Sector for pair rows & row + 2",
			    100,-10.,10., 100,-10.,10.);
	corrO2   = new TH2D("corrO2","Correlation for Outer Sector for pair rows & row + 2",
			    100,-10.,10., 100,-10.,10.);
	corrI5   = new TH2D("corrI5","Correlation for Inner Sector for pair rows & row + 5",
			    100,-10.,10., 100,-10.,10.);
	corrO5   = new TH2D("corrO5","Correlation for Outer Sector for pair rows & row + 5",
			    100,-10.,10., 100,-10.,10.);
	corrIw   = new TH2D("corrIw","Weighted correlation for Inner Sector for pair of nearest rows",
			    100,-10.,10., 100,-10.,10.);
	corrOw   = new TH2D("corrOw","Weighted correlation for Outer Sector for pair of nearest rows",
			    100,-10.,10., 100,-10.,10.);
	corrI1w   = new TH1D("corrI1w","Weighted distribution for Inner Sector",100,-10.,10.);
	corrO1w   = new TH1D("corrO1w","Weighted distribution for Outer Sector",100,-10.,10.);
	corrI2w   = new TH2D("corrI2w","Weighted correlation for Inner Sector for pair rows & row + 2",
			     100,-10.,10., 100,-10.,10.);
	corrO2w   = new TH2D("corrO2w","Weighted correlation for Outer Sector for pair rows & row + 2",
			     100,-10.,10., 100,-10.,10.);
	corrI5w   = new TH2D("corrI5w","Weighted correlation for Inner Sector for pair rows & row + 5",
			     100,-10.,10., 100,-10.,10.);
	corrO5w   = new TH2D("corrO5w","Weighted correlation for Outer Sector for pair rows & row + 5",
			     100,-10.,10., 100,-10.,10.);
#endif // CORELATION
      }
      Points    = new TH2D("Points","dEdx(fit) versus no. of measured points",50,0,50., 500,-1.,4.);
      PointsB   = new TH2D("PointsB","dEdx(fit) versus no. of measured points Bichsel",50,0,50., 500,-1.,4.);
      TPoints  = new TH2D("TPoints","dEdx(fit) versus length", 
			  150,10.,160., 500,-1.,4.);
      TPointsB  = new TH2D("TPointsB","dEdx(fit) versus length Bichsel", 
			   150,10.,160., 500,-1.,4.);
      TdEdxPF  = new TH2D("TdEdxPF","log10(dE/dx(fit)(keV/cm)) versus log10(p(GeV/v))", 
			  150,-1.,2., 500,0.,2.5);
      TdEdxP70 = new TH2D("TdEdxP70","log10(dE/dx(I70)(keV/cm)) versus log10(p(GeV/v))", 
			  150,-1.,2., 500,0.,2.5);
      Points70  = new TH2D("Points70","dEdx(I70) versus no. of measured points",50,0,50.,500,-1.,4.);
      Points70B = new TH2D("Points70B","dEdx(I70) versus no. of measured points Bichsel",50,0,50.,500,-1.,4.);
      TPoints70= new TH2D("TPoints70","dEdx(fit) versus length", 
			  150,10.,160., 500,-1.,4.);
      TPoints70B= new TH2D("TPoints70B","dEdx(fit) versus length Bichsel", 
			   150,10.,160., 500,-1.,4.);
      if (! m_DoNotCorrectdEdx) {
#ifdef ZBGX
	zbgx = new (TF3D*)[NHYPS]; 
#endif // ZBGX
      }
      for (int hyp=0; hyp<NHYPS;hyp++) {
	TString nameP("fit");
	nameP += StProbPidTraits::mPidParticleDefinitions[hyp]->name().data();
	nameP.ReplaceAll("-","");
	TString title = "fitZ - Pred. for ";
	title += StProbPidTraits::mPidParticleDefinitions[hyp]->name().data();
	title.ReplaceAll("-","");
	title += " versus log10(beta*gamma) for pion";
	ffitZ[hyp]  = new TH2D(nameP.Data(),title.Data(),100,-1,4,100,-5,5);
	ffitZ[hyp]->SetMarkerColor(hyp+2);
	ffitP[hyp] = new TH2D(*ffitZ[hyp]);
	nameP.ReplaceAll("fit","fitP");
	ffitP[hyp]->SetName(nameP.Data());
	if (! m_DoNotCorrectdEdx) {
#ifdef ZBGX
	  nameP = "zbgx";
	  nameP += StProbPidTraits::mPidParticleDefinitions[hyp]->name().data();
	  if (zbgx) 
	    zbgx[hyp] = new TH3D(nameP.Data(),"z = log(dE/dx) versus log10(beta*gamma) and log2(dx)",
				 100,-1,4,Nlog2dx,log2dxLow,log2dxHigh,320,-2,6);
#endif // ZBGX
	}
  	for (int sCharge = 0; sCharge < 2; sCharge++) {
	  nameP = StProbPidTraits::mPidParticleDefinitions[hyp]->name().data();
	  nameP.ReplaceAll("-","");
	  if (sCharge == 0) nameP += "P";
	  else              nameP += "N";
	  TString name = nameP;
	  name += "70";
	  title = "log(dE/dx70/I(";
	  title += nameP;
	  title += ")) versus log10(p/m)";
	  hist70[hyp][sCharge] = new TH2D(name.Data(),title.Data(),100,-1.,4.,600,-2.,4.);
	  name += "B";
	  title += " Bichsel";
	  hist70B[hyp][sCharge] = new TH2D(name.Data(),title.Data(),100,-1.,4.,600,-2.,4.);
	  name = nameP;
	  name += "z";
	  title = "zFit - log(I(";
	  title += nameP;
	  title += ")) versus log10(p/m)";
	  histz[hyp][sCharge] = new TH2D(name.Data(),title.Data(),100,-1.,4.,600,-2.,4.);
	  name += "B";
	  title += " Bichsel";
	  histzB[hyp][sCharge] = new TH2D(name.Data(),title.Data(),100,-1.,4.,600,-2.,4.);
	  name = nameP;
	  name += "B";
	  title = "log(I_{Sirrf}(";
	  title += nameP;
	  title += ")) versus log10(p/m)";
	  histB[hyp][sCharge] = new TProfile(name.Data(),title.Data(),100,-1.,4.);
	  name += "B";
	  title = "log(I_{BB}(";
	  title += nameP;
	  title += ")) versus log10(p/m) Bichsel";
	  histBB[hyp][sCharge] = new TProfile(name.Data(),title.Data(),100,-1.,4.);
	}
      }
      TDatime t1(2001,6,1,0,0,0); UInt_t i1 = t1.Convert();
      TDatime t2(2004,6,1,0,0,0); UInt_t i2 = t2.Convert();
      Int_t Nt = (i2 - i1)/(3600); // each hour 
      if (! m_DoNotCorrectdEdx) {
	Pressure   = new TH3D("Pressure","log(dE/dx)_{uncorrected} - log(I(pi)) versus Row & Log(Pressure)", 
			      NoRow,1., NoRow+1,100,6.9,6.95, 200,-5.,5.);
	PressureA  = new TH3D("PressureA","log(dE/dx)_{just after correction} log(I(pi)) versus Log(Pressure)", 
			      NoRow,1., NoRow+1,100,6.9,6.95, 200,-5.,5.);
	PressureC  = new TH3D("PressureC","log(dE/dx)_{corrected} - row & log(I(pi)) versus Log(Pressure)", 
			      NoRow,1., NoRow+1,100,6.9,6.95, 200,-5.,5.);
	GainMonitor  = new TH2D("GainMonitor","log(dE/dx)_{corrected} - log(I(pi)) versus GainMonitor", 
				100,70.,120., 200,-5.,5.);
	Time   = new TH2D("Time","log(dE/dx)_{uncorrected} - log(I(pi)) versus Date& Time", 
			  Nt,i1,i2, 200,-5.,5.);
	TimeC  = new TH2D("TimeC","log(dE/dx)_{corrected} - log(I(pi)) versus Date& Time after correction", 
			  Nt,i1,i2, 200,-5.,5.);
	TimeP  = new TH2D("TimeP","log(dE/dx)_{after pressure correction} - log(I(pi)) versus Date& Time", 
			  Nt,i1,i2, 200,-5.,5.);
      }
      FitPull= new TH2D("FitPull","(zFit - log(I(pi)))/dzFit  versus track length", 
			150,10.,160, 200,-5.,5.);
      Pull70 = new TH2D("Pull70","log(I70/I(pi)))/D70  versus track length", 
			150,10.,160, 200,-5.,5.);
      Center = new TProfile("Center","Tpc Gain Monitor center versus Time",Nt,i1,i2);
      Height = new TProfile("Height","Tpc Gain Monitor height versus Time",Nt,i1,i2);
      Width  = new TProfile("Width","Tpc Gain Monitor width versus Time",Nt,i1,i2);
      
      BarPressure = new TProfile("BarPressure","barometricPressure (mbar) versus time",Nt,i1,i2);
      inputTPCGasPressure = new TProfile("inputTPCGasPressure","inputTPCGasPressure (mbar) versus time",Nt,i1,i2);
      nitrogenPressure = new TProfile("nitrogenPressure","nitrogenPressure (mbar) versus time",Nt,i1,i2);
      gasPressureDiff = new TProfile("gasPressureDiff","gasPressureDiff (mbar) versus time",Nt,i1,i2);
      inputGasTemperature = new TProfile("inputGasTemperature","inputGasTemperature (degrees C) versus time",Nt,i1,i2);
      outputGasTemperature = new TProfile("outputGasTemperature","outputGasTemperature (degrees C) versus time",Nt,i1,i2);
      flowRateArgon1 = new TProfile("flowRateArgon1","flowRateArgon1 (liters/min) versus time",Nt,i1,i2);
      flowRateArgon2 = new TProfile("flowRateArgon2","flowRateArgon2 (liters/min) versus time",Nt,i1,i2);
      flowRateMethane = new TProfile("flowRateMethane","flowRateMethane (liters/min) versus time",Nt,i1,i2);
      percentMethaneIn = new TProfile("percentMethaneIn","percentMethaneIn (percent) versus time",Nt,i1,i2);
      ppmOxygenIn = new TProfile("ppmOxygenIn","ppmOxygenIn (ppm) versus time",Nt,i1,i2);
      flowRateExhaust = new TProfile("flowRateExhaust","flowRateExhaust (liters/min) versus time",Nt,i1,i2);
      ppmWaterOut = new TProfile("ppmWaterOut","ppmWaterOut (ppm) versus time",Nt,i1,i2);
      ppmOxygenOut = new TProfile("ppmOxygenOut","ppmOxygenOut (ppm) versus time",Nt,i1,i2);
      flowRateRecirculation = new TProfile("flowRateRecirculation","flowRateRecirculation (liters/min) versus time",Nt,i1,i2);
      CenterPressure = new TProfile("CenterPressureP","log(center) vs log(Pressure)",100,6.9,6.95);
      inputTPCGasPressureP =new TH2D("inputTPCGasPressureP","log(dE/dx/Pion) vs inputTPCGasPressure (mbar)",100,1.95,2.05,200,-5.,5.);
      nitrogenPressureP =new TH2D("nitrogenPressureP","log(dE/dx/Pion) vs nitrogenPressure (mbar)",100,1.,2.,200,-5.,5.);
      gasPressureDiffP =new TH2D("gasPressureDiffP","log(dE/dx/Pion) vs gasPressureDiff (mbar)",100,0.6,1.,200,-5.,5.);
      inputGasTemperatureP =new TH2D("inputGasTemperatureP","log(dE/dx/Pion) vs inputGasTemperature (degrees C)",100,295.,300.,200,-5.,5.);
      outputGasTemperatureP =new TH2D("outputGasTemperatureP","log(dE/dx/Pion) vs outputGasTemperature (degrees C)",100,295.,300.,200,-5.,5.);
      flowRateArgon1P =new TH2D("flowRateArgon1P","log(dE/dx/Pion) vs flowRateArgon1 (liters/min)",100,14.95,15.0,200,-5.,5.);
      flowRateArgon2P =new TH2D("flowRateArgon2P","log(dE/dx/Pion) vs flowRateArgon2 (liters/min)",100,0.,0.25,200,-5.,5.);
      flowRateMethaneP =new TH2D("flowRateMethaneP","log(dE/dx/Pion) vs flowRateMethane (liters/min)",100,1.34,1.37,200,-5.,5.);
      percentMethaneInP =new TH2D("percentMethaneInP","log(dE/dx/Pion) vs percentMethaneIn (percent)",100,9.,11.,200,-5.,5.);
      ppmOxygenInP =new TH2D("ppmOxygenInP","log(dE/dx/Pion) vs ppmOxygenIn (ppm)",100,20.,30.,200,-5.,5.);
      flowRateExhaustP =new TH2D("flowRateExhaustP","log(dE/dx/Pion) vs flowRateExhaust (liters/min)",100,5.,20.,200,-5.,5.);
      ppmWaterOutP =new TH2D("ppmWaterOutP","log(dE/dx/Pion) vs ppmWaterOut (ppm)",100,6.,12.,200,-5.,5.);
      ppmOxygenOutP =new TH2D("ppmOxygenOutP","log(dE/dx/Pion) vs ppmOxygenOut (ppm)",100,-.285,-.265,200,-5.,5.);
      flowRateRecirculationP =new TH2D("flowRateRecirculationP","log(dE/dx/Pion) vs flowRateRecirculation (liters/min)",100,515.,545.,200,-5.,5.);
#ifdef SpaceChargeStudy
      SpaceCharge = new TProfile2D("SpaceCharge","dE versus R and Z",85,40.,210.,92,-230.,230.);
      SpaceChargeU = new TProfile2D("SpaceChargeU","dEU versus R and Z",85,40.,210.,92,-230.,230.);
      SpaceChargeT = new TProfile2D("SpaceChargeT","dEU (all) versus R and Z",85,40.,210.,92,-230.,230.);
      Space2Charge = new TH2D("Space2Charge","dE versus R and Z",85,40.,210.,92,-230.,230.);
      Space2ChargeU = new TH2D("Space2ChargeU","dEU versus R and Z",85,40.,210.,92,-230.,230.);
      Space2ChargeT = new TH2D("Space2ChargeT","dEU (all) versus R and Z",85,40.,210.,92,-230.,230.);
      TimeShift     = new TH1D("TimeShift","Shift in time wrt collsion",200,0,2000.);
      Space3Charge  = new TH3D("Space3Charge","Space charged versus Sector, Row and Z",
			       NoSector,1., NoSector+1, NoRow,1., NoRow+1,105,0.,210.);
      Space3Charge->Sumw2();
      Space3ChargePRZ  = new TH3D("Space3ChargePRZ","Space charged versus Phi(rads), Rho and Z",
				  36,-TMath::Pi(), TMath::Pi(), 85, 40., 210.,105,-210.,210.);
      Space3ChargePRZ->Sumw2();
      Space3ChargeShifted = new TH3D("Space3ChargeShifted","Space charged shifted in time versus Sector, Row and Z",
				     NoSector,1., NoSector+1, NoRow,1., NoRow+1,105,0.,210.);
      Space3ChargeShifted->Sumw2();
#endif // SpaceChargeStudy
    }
    ffitZU = new TH2D("fitZU","fitZ - PredPi Unique versus log10(beta*gamma)",100,-1,4,100,-5,5);
    ffitZU->SetMarkerColor(7);
    ffitZU3 = new TH2D("fitZU3","fitZ - PredPi Unique and 3 sigma away versus log10(beta*gamma)",100,-1,4,100,-5,5);
    ffitZU3->SetMarkerColor(6);
    ffitZA = new TH2D("fitZA","fitZ - PredPi All versus log10(beta*gamma)",100,-1,4,100,-5,5);
    ffitZA->SetMarkerColor(1);
    if (! m_DoNotCorrectdEdx) {
      hdEI  = new TH1D("hdEI","log10(dE) Inner after calibration",100,-8.,-3.);
      hdEUI = new TH1D("hdEUI","log10(dEU) Inner before correction",100,-8.,-3.);
      hdERI = new TH1D("hdERI","log10(dER) Inner after row correction  correction",100,-8.,-3.);
      hdEPI = new TH1D("hdEPI","log10(dEP) Inner after Pressure correction",100,-8.,-3.);
      hdETI = new TH1D("hdETI","log10(dET) Inner after TimeScale",100,-8.,-3.);
      hdESI = new TH1D("hdESI","log10(dES) Inner after after TimeScale + SecRow corrections",100,-8.,-3.);
      hdEZI = new TH1D("hdEZI","log10(dEZ) Inner after TimeScale + SecRow + Sec Z corrections ",100,-8.,-3.);
      hdEMI = new TH1D("hdEMI","log10(dEM) Inner after TimeScale + SecRow + Sec Z + Multiplicity corrections",100,-8.,-3.);
      hdEO  = new TH1D("hdEO","log10(dE) Outer after calibration",100,-8.,-3.);
      hdEUO = new TH1D("hdEUO","log10(dEU) Outer before correction",100,-8.,-3.);
      hdERO = new TH1D("hdERO","log10(dER) Outer after row correction  correction",100,-8.,-3.);
      hdEPO = new TH1D("hdEPO","log10(dEP) Outer after Pressure correction",100,-8.,-3.);
      hdETO = new TH1D("hdETO","log10(dET) Outer after TimeScale",100,-8.,-3.);
      hdESO = new TH1D("hdESO","log10(dES) Outer after after TimeScale + SecRow corrections",100,-8.,-3.);
      hdEZO = new TH1D("hdEZO","log10(dEZ) Outer after TimeScale + SecRow + Sec Z corrections ",100,-8.,-3.);
      hdEMO = new TH1D("hdEMO","log10(dEM) Outer after TimeScale + SecRow + Sec Z + Multiplicity corrections",100,-8.,-3.);
    }
#ifdef ProbabilityPlot
    Prob = new TH3D("Prob","Z(=log(I70/Bichsel)) versun log10(bg) for pion and Probability",
		    100,-1.,4.,10*NHYPS+1,-.1,NHYPS,600,-2.,4.);
#endif // ProbabilityPlot
    dx0dx = new TH2D("dx0dx","segment length versus sright line approximation",50,0.,5.,80,0.,8.);
    dXdE  = new TH3D("dXdE","log(dEdx/Pion) versus dX and row",
			   100,0.,5., NoRow,1.,NoRow+1,200,-5.,5.);
    dXdEA = new TH3D("dXdEA","log(dEdx/Pion) just after correction versus dX and row",
			   100,0.,5., NoRow,1.,NoRow+1,200,-5.,5.);
    dXdEC = new TH3D("dXdEC","log(dEdx/Pion) corrected versus dX and row",
			   100,0.,5., NoRow,1.,NoRow+1,200,-5.,5.);
    // Create a ROOT Tree and one superbranch
    if (TMath::Abs(m_Calibration) > 2) { 
      ftree = new TTree("dEdxT","dEdx tree");
      ftree->SetAutoSave(1000000000);  // autosave when 1 Gbyte written
      Int_t bufsize = 64000;
      Int_t split = 99;
      if (split)  bufsize /= 4;
      ftrack = new dEdxTrack();
      TTree::SetBranchStyle(1); //new style by default
      TBranch *branch = ftree->Branch("dEdxTrack", "dEdxTrack", &ftrack, bufsize,split);
      branch->SetAutoDelete(kFALSE);
    }
  }
  gMessMgr->SetLimit("StdEdxY2Maker:: mismatched Sector",20);
  gMessMgr->SetLimit("StdEdxY2Maker:: pad/TimeBucket out of range:",20);
  gMessMgr->SetLimit("StdEdxY2Maker:: Helix Pediction",20);
  gMessMgr->SetLimit("StdEdxY2Maker:: Coordinates",20);
  gMessMgr->SetLimit("StdEdxY2Maker:: Prediction",20);
  gMessMgr->SetLimit("StdEdxY2Maker:: NdEdx",20);
  gMessMgr->SetLimit("StdEdxY2Maker:: Illegal time for scalers",20);
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StdEdxY2Maker::InitRun(Int_t RunNumber){
  // 		TPG parameters
  // Normal to Sector
  StTpcCoordinateTransform transform(gStTpcDb);
  Double_t alpha = 2.*TMath::Pi()/NoSector;
  for (UInt_t sector=1; sector<=NoSector; sector++) {
    Double_t beta = (sector > 12) ? 2*(NoSector-sector)*alpha: 2*sector*alpha;
    mNormal[sector-1] = new StThreeVectorD(sin(beta), cos(beta), 0.);
    for (UInt_t row = 1; row <= NoRow; row++) {
      StTpcPadCoordinate padCoord(sector, row, 1, 1);
      StTpcLocalSectorCoordinate lsMidCoord; transform(padCoord, lsMidCoord);
      StTpcLocalSectorCoordinate  lsPos(0,lsMidCoord.position().y(),0, sector);
      StGlobalCoordinate          gMidCoord; transform(lsPos, gMidCoord);
      for (Int_t l = 0; l < 3; l++) 
	if (mRowPosition[sector-1][row-1][l]) SafeDelete(mRowPosition[sector-1][row-1][l]);
      mRowPosition[sector-1][row-1][0] = new  StThreeVectorD(gMidCoord.position().x(), // gMidPos 
							     gMidCoord.position().y(),
							     gMidCoord.position().z());
      Double_t padlength;
      if (row<14) padlength = gStTpcDb->PadPlaneGeometry()->innerSectorPadLength();
      else 	  padlength = gStTpcDb->PadPlaneGeometry()->outerSectorPadLength();
      StTpcLocalSectorCoordinate lsTopCoord(0,
					    lsPos.position().y()+padlength/2.,
					    0,
					    sector);
      StTpcLocalSectorCoordinate lsBotCoord(0,
					    lsPos.position().y()-padlength/2.,
					    0,
					    sector);
      //Transform back to global coordinates
      StGlobalCoordinate gTopCoord; transform(lsTopCoord, gTopCoord);
      StGlobalCoordinate gBotCoord; transform(lsBotCoord, gBotCoord);
      mRowPosition[sector-1][row-1][1] = new StThreeVectorD(gTopCoord.position().x(), // gTopPos
							    gTopCoord.position().y(),
							    gTopCoord.position().z());
      mRowPosition[sector-1][row-1][2] = new StThreeVectorD(gBotCoord.position().x(), // gBotPos
							    gBotCoord.position().y(),
							    gBotCoord.position().z());
    }
  }
#ifdef GetTpcGainMonitor
  m_tpcGainMonitor = (St_tpcGainMonitor *) GetDataBase("Conditions/tpc/tpcGainMonitor");
#endif // GetTpcGainMonitor
  TDataSet *tpc_calib  = GetDataBase("Calibrations/tpc"); assert(tpc_calib);
  if (! m_Simulation) { // calibration constants
    m_tpcGas = (St_tpcGas *) tpc_calib->Find("tpcGas");
    if (!m_tpcGas) cout << "=== tpcGas is missing ===" << endl;
    m_tpcPressure = (St_tpcCorrection *) tpc_calib->Find("tpcPressureB");
    if (!m_tpcPressure) {
      cout << "=== tpcPressure is missing ===" << endl;
    }
    assert (m_tpcPressure && m_tpcGas);
    assert(m_tpcGas->GetNRows());
    assert(m_tpcPressure->GetNRows());
    
#ifdef DriftDistanceCorrection
    if (! m_drift) {
      m_drift = (St_tpcCorrection *) tpc_calib->Find("TpcDriftDistOxygen"); 
      assert(m_drift); 
      assert(m_drift->GetNRows());
    }
#endif // DriftDistanceCorrection
#ifdef AdcCorrection 
    if (! m_AdcCorrection) {
      m_AdcCorrection = (St_tpcCorrection *) tpc_calib->Find("TpcAdcCorrection"); 
      assert(m_AdcCorrection); 
      assert(m_AdcCorrection->GetNRows());
    }
#endif // AdcCorrection
    if (! m_zCorrection) {
      m_zCorrection = (St_tpcCorrection *) tpc_calib->Find("TpcZCorrection"); 
      if (! m_zCorrection) cout << "=== TpcZCorrection is missing ===" << endl;
      else assert(m_zCorrection->GetNRows());
    }
    if (! m_dXCorrection) {
      m_dXCorrection = (St_tpcCorrection *) tpc_calib->Find("TpcdXCorrection"); 
      if (! m_dXCorrection) cout << "=== TpcdXCorrection is missing ===" << endl;
      else assert(m_dXCorrection->GetNRows());
    }
    if (! m_TpcdEdxCor) {
      m_TpcdEdxCor = (St_tpcCorrection *) tpc_calib->Find("TpcdEdxCor"); 
      if (! m_TpcdEdxCor) cout << "=== TpcdEdxCor is missing ===" << endl;
      else assert(m_TpcdEdxCor->GetNRows());
    }
    if (! m_TpcLengthCorrection) {
      m_TpcLengthCorrection = (St_tpcCorrection *) tpc_calib->Find("TpcLengthCorrection"); 
      if (! m_TpcLengthCorrection) cout << "=== TpcLengthCorrection is missing ===" << endl;
      else assert(m_TpcLengthCorrection->GetNRows());
    }
    m_TpcSecRow  = (St_TpcSecRowCor *) tpc_calib->Find("TpcSecRowB"); assert(m_TpcSecRow); 
    if (! m_trigDetSums) 
      m_trigDetSums = (St_trigDetSums *) GetDataBase("Calibrations/rich/trigDetSums");
    else assert(m_trigDetSums->GetNRows());
    if (m_trigDetSums) {
      trigDetSums_st *m_trig = m_trigDetSums->GetTable();
      UInt_t date = GetDateTime().Convert();
      if (date < m_trig->timeOffset) {
	gMessMgr->Error() << "StdEdxY2Maker:: Illegal time for scalers = " 
			  << m_trig->timeOffset << "/" << date
			  << " Run " << m_trig->runNumber << "/" << GetRunNumber() << endm;
	SafeDelete(m_trigDetSums); m_trig = 0;
      }
    }
  }  
  m_InitDone = kTRUE;
  return kStOK;
}
//_____________________________________________________________________________
Int_t StdEdxY2Maker::FinishRun(Int_t OldRunNumber) {
  // Move Clean up to InitRun
  m_InitDone = kFALSE;
  return StMaker::FinishRun(OldRunNumber);
}
//_____________________________________________________________________________
Int_t StdEdxY2Maker::Finish() {
  FinishRun(0);
  return StMaker::Finish();
}
//_____________________________________________________________________________
Int_t StdEdxY2Maker::Make(){ 
  static StTimer timer;
  if (Debug() > 0) timer.start();
  m_trig = 0;
  if (m_trigDetSums) m_trig = m_trigDetSums->GetTable();
  Double_t date = GetDateTime().Convert();
  if (m_Calibration != 0 && m_tpcGas) {
    if (BarPressure) BarPressure->Fill(date,(*m_tpcGas)[0].barometricPressure);
    if (inputTPCGasPressure) inputTPCGasPressure->Fill(date,(*m_tpcGas)[0].inputTPCGasPressure);
    if (nitrogenPressure) nitrogenPressure->Fill(date,(*m_tpcGas)[0].nitrogenPressure);
    if (gasPressureDiff) gasPressureDiff->Fill(date,(*m_tpcGas)[0].gasPressureDiff);
    if (inputGasTemperature) inputGasTemperature->Fill(date,(*m_tpcGas)[0].inputGasTemperature);
    if (outputGasTemperature) outputGasTemperature->Fill(date,(*m_tpcGas)[0].outputGasTemperature);
    if (flowRateArgon1) flowRateArgon1->Fill(date,(*m_tpcGas)[0].flowRateArgon1);
    if (flowRateArgon2) flowRateArgon2->Fill(date,(*m_tpcGas)[0].flowRateArgon2);
    if (flowRateMethane) flowRateMethane->Fill(date,(*m_tpcGas)[0].flowRateMethane);
    if (percentMethaneIn) percentMethaneIn->Fill(date,(*m_tpcGas)[0].percentMethaneIn);
    if (ppmOxygenIn) ppmOxygenIn->Fill(date,(*m_tpcGas)[0].ppmOxygenIn);
    if (flowRateExhaust) flowRateExhaust->Fill(date,(*m_tpcGas)[0].flowRateExhaust);
    if (ppmWaterOut) ppmWaterOut->Fill(date,(*m_tpcGas)[0].ppmWaterOut);
    if (ppmOxygenOut) ppmOxygenOut->Fill(date,(*m_tpcGas)[0].ppmOxygenOut);
    if (flowRateRecirculation) flowRateRecirculation->Fill(date,(*m_tpcGas)[0].flowRateRecirculation);
  }
  Double_t TimeScale = 1;
  Double_t PressureScaleI = 1;
  Double_t PressureScaleO = 1;
  if (! m_DoNotCorrectdEdx) {
#ifdef PressureScaleFactorOld
  if (m_tpcGas && m_tpcPressure) {
    if (m_tpcPressure->GetNRows() == 1) 
      PressureScaleI = PressureScaleO =
	TMath::Exp(-((*m_tpcPressure)[0].A0 + 
		     (*m_tpcPressure)[0].A1*TMath::Log((*m_tpcGas)[0].barometricPressure)));
    else {
      PressureScaleO = 
	TMath::Exp(-((*m_tpcPressure)[0].A0 + 
		     (*m_tpcPressure)[0].A1*TMath::Log((*m_tpcGas)[0].barometricPressure)));
      PressureScaleI = 
	TMath::Exp(-((*m_tpcPressure)[1].A0 + 
		     (*m_tpcPressure)[1].A1*TMath::Log((*m_tpcGas)[0].barometricPressure)));
    }
  }
#else
  if (m_tpcGas && m_tpcPressure && m_tpcPressure->GetNRows() >= 2) {
    Double_t LogPressure = TMath::Log((*m_tpcGas)[0].barometricPressure);
    tpcCorrection_st *cor = m_tpcPressure->GetTable();
    PressureScaleO = TMath::Exp(-CalcCorrection(cor,LogPressure));
    PressureScaleI = TMath::Exp(-CalcCorrection(cor+1,LogPressure));
  }
#endif // PressureScaleFactor
  } //! m_DoNotCorrectdEdx
#ifdef GetTpcGainMonitor
  if (m_Calibration != 0 && m_tpcGainMonitor) {
    if (Center) {
      Center->Fill(date,(*m_tpcGainMonitor)[0].center);
      if (CenterPressure) CenterPressure->Fill(TMath::Log((*m_tpcGas)[0].barometricPressure),
					       TMath::Log((*m_tpcGainMonitor)[0].center));
    }
    if (Height) Height->Fill(date,(*m_tpcGainMonitor)[0].height);
    if (Width)  Width->Fill(date,(*m_tpcGainMonitor)[0].width);
  }
#endif // GetTpcGainMonitor
  dst_dedx_st dedx;
  StTpcCoordinateTransform transform(gStTpcDb);
  Float_t bField = 0;
  StEvent* pEvent = dynamic_cast<StEvent*> (GetInputDS("StEvent"));
  if (!pEvent) {
    gMessMgr->Info() << "StdEdxY2Maker: no StEvent " << endm;
    return kStOK;        // if no event, we're done
  }
  if (pEvent->runInfo()) bField = pEvent->runInfo()->magneticField();
  if (m_Calibration != 0 && m_trig) {
    if (m_trig->zdcWest > 0 &&
	m_trig->zdcEast > 0 && Zdc) Zdc->Fill(TMath::Log10(m_trig->zdcWest),
					      TMath::Log10(m_trig->zdcEast));
    if (m_trig->zdcX > 0 && ZdcC) ZdcC->Fill(TMath::Log10(m_trig->zdcX));
    if (m_trig->bbcX > 0 && BBC) BBC->Fill(TMath::Log10(m_trig->bbcX));
    if (m_trig->L0   > 0 && L0) L0->Fill(TMath::Log10(m_trig->L0));
    if (m_trig->mult > 0 && Multiplicity) Multiplicity->Fill(TMath::Log10(m_trig->mult));
  }
#ifdef SpaceChargeStudy
  if (m_Calibration != 0 && SpaceChargeT) {
    StTpcHitCollection* TpcHitCollection = pEvent->tpcHitCollection();
    if (TpcHitCollection) {
      UInt_t NoSectors = TpcHitCollection->numberOfSectors();
      const Int_t EventId  = GetEventNumber();
      Int_t Ids[24];
      Int_t secs[24];
      Double_t zshift[24];
      const Int_t NMixEvts = 1152;
      for (UInt_t ev = 0; ev< NoSectors; ev++) {
	Ids[ev] = (EventId + gRandom->Integer(NMixEvts))%NMixEvts;
	if (TimeShift) TimeShift->Fill(Ids[ev]);
	secs[ev] = gRandom->Integer(24);
	zshift[ev] = 0.18*Ids[ev];
      }
      for (UInt_t i = 0; i< NoSectors; i++) {
	StTpcSectorHitCollection* sectorCollection = TpcHitCollection->sector(i);
	if (sectorCollection) {
	  Int_t numberOfPadrows = sectorCollection->numberOfPadrows();
	  for (int j = 0; j< numberOfPadrows; j++) {
	    StTpcPadrowHitCollection *rowCollection = TpcHitCollection->sector(i)->padrow(j);
	    if (rowCollection) {
	      UInt_t NoHits = rowCollection->hits().size();
	      for (UInt_t k = 0; k < NoHits; k++) {
		StTpcHit* tpcHit = TpcHitCollection->sector(i)->padrow(j)->hits().at(k);
		SpaceChargeT->Fill(tpcHit->position().perp(),tpcHit->position().z(),tpcHit->charge());
		if (Space2ChargeT) 
		  Space2ChargeT->Fill(tpcHit->position().perp(),tpcHit->position().z(),tpcHit->charge());
		if (Space3Charge) {
		  Double_t z = TMath::Abs(tpcHit->position().z());
		  Space3Charge->Fill(i+1,j+1,z,tpcHit->charge());
		  if (Space3ChargeShifted) {
		    for (UInt_t ev = 0; ev<NoSectors; ev++) {
		      Double_t zz = z - zshift[ev];
		      if (zz > 0) {
			int sec = (i + secs[ev])%24;
			Space3ChargeShifted->Fill(sec+1,j+1,zz,tpcHit->charge());
		      }
		    }
		  }
		}
		if (Space3ChargePRZ) {
		  Space3ChargePRZ->Fill(tpcHit->position().phi(),
					tpcHit->position().perp(),
					tpcHit->position().z(),tpcHit->charge());
		} 
	      }
	    }
	  }
	}
      }
    }
  }
#endif // SpaceChargeStudy
  StSPtrVecTrackNode& trackNode = pEvent->trackNodes();
  UInt_t nTracks = trackNode.size();
  StTrackNode *node=0;
  for (unsigned int i=0; i < nTracks; i++) {
    node = trackNode[i]; if (!node) continue;
    StGlobalTrack* gTrack = static_cast<StGlobalTrack*>(node->track(global));
    if (gTrack && gTrack->flag() > 0) {
      if (! m_DoNotCorrectdEdx) {
	StPtrVecHit hvec = gTrack->detectorInfo()->hits(kTpcId);
	if (! hvec.size()) continue;
	// clean up old Tpc traits if any
	//      gTrack->pidTraits().clear(); gTrack->pidTraits().resize(0);
	//      if (pTrack) {pTrack->pidTraits().clear(); pTrack->pidTraits().resize(0);}
	//      if (tTrack) {tTrack->pidTraits().clear(); tTrack->pidTraits().resize(0);}
	StPrimaryTrack* pTrack = 
	  static_cast<StPrimaryTrack*>(node->track(primary));
	StTptTrack   *tTrack =
	  static_cast<StTptTrack    *>(node->track(tpt));
	StTrack *track;
	// clean old PiD traits
	for (int l = 0; l < 3; l++) {
	  if (l == 0) track = gTrack;
	  if (l == 1) track = pTrack;
	  if (l == 2) track = tTrack;
	  if (track) {
	    StSPtrVecTrackPidTraits &traits = track->pidTraits();
	    unsigned int size = traits.size();
	    if (size) {
	      for (unsigned int i = 0; i < size; i++) {
		StDedxPidTraits *pid = dynamic_cast<StDedxPidTraits*>(traits[i]);
		if (! pid) continue;
		if (pid->detector() != kTpcId) continue;
		traits[i]->makeZombie(1);
	      }
	    }
	  }
	}
	Int_t Id = gTrack->key();
	Int_t NoFitPoints = gTrack->fitTraits().numberOfFitPoints();
	NdEdx = 0;
	Double_t TrackLength70 = 0, TrackLength = 0;
	for (unsigned int j=0; j<hvec.size(); j++) {
	  StTpcHit *tpcHit = static_cast<StTpcHit *> (hvec[j]);
	  if (! tpcHit) continue;
	  if (Debug() > 1) {
	    cout << "Hit: " << j << " charge: " << tpcHit->charge()
		 << " flag: " << tpcHit->flag() 
		 << " useInFit :" << tpcHit->usedInFit()
		 << " position: " << tpcHit->position() 
		 << " positionError: " << tpcHit->positionError() << endl;
	  }
	  if (! tpcHit->usedInFit()) continue;
	  if (  tpcHit->flag()) continue;
	  Int_t sector = tpcHit->sector();
	  Int_t row    = tpcHit->padrow();
	  StThreeVectorD &normal = *mNormal[sector-1];
	  const StThreeVectorD  &gMidPos = *mRowPosition[sector-1][row-1][0];
	  // check that helix prediction is consistent with measurement
	  Double_t s = gTrack->geometry()->helix().pathLength(gMidPos, normal);
	  if (s > 1.e4) continue;
	  StThreeVectorD xyzOnPlane = gTrack->geometry()->helix().at(s);
	  StGlobalCoordinate globalOnPlane(xyzOnPlane.x(),xyzOnPlane.y(),xyzOnPlane.z());
	  StTpcPadCoordinate PadOnPlane;      transform(globalOnPlane,PadOnPlane);
	  StGlobalCoordinate global(tpcHit->position());
	  StTpcPadCoordinate Pad;      transform(global,Pad);
	  Int_t iokCheck = 0;
	  if (sector != Pad.sector() || // ? && TMath::Abs(xyzOnPlane.x()) > 20.0 ||
	      row    != Pad.row()) {
	    gMessMgr->Warning() << "StdEdxY2Maker:: mismatched Sector " 
				<< Pad.sector() << " / " << sector
				<< " Row " << Pad.row() << " / " << row 
				<< "pad " << Pad.pad() << " TimeBucket :" << Pad.timeBucket() 
				<< " x/y/z: " << tpcHit->position()
				<< endm;
	    iokCheck++;
	  }
	  if (Pad.pad()    < 1             ||
	      Pad.pad()    >= NoPads       ||
	      Pad.timeBucket() < 0         ||
	      Pad.timeBucket() >= NTimeBins) {
	    gMessMgr->Warning() << "StdEdxY2Maker:: pad/TimeBucket out of range: " 
				<<  Pad.pad() << " / " << Pad.timeBucket() << endm;
	    iokCheck++;
	  }
	  if (sector != PadOnPlane.sector() || row != PadOnPlane.row() ||	TMath::Abs(Pad.pad()-PadOnPlane.pad()) > 5) {
	    if (Debug() > 1) {
	      gMessMgr->Warning() << "StdEdxY2Maker::	Helix Pediction " 
				  << "Sector = " 
				  << PadOnPlane.sector() << "/" 
				  << sector 
				  << " Row = " << PadOnPlane.row() << "/" 
				  << row 
				  << " Pad = " << PadOnPlane.pad() << "/" 
				  << Pad.pad() 
				  << " from Helix  is not matched with point/" << endm;;
	      gMessMgr->Warning() << "StdEdxY2Maker:: Coordinates " 
				  << xyzOnPlane << "/" << tpcHit->position()
				  << endm;
	    }
	    iokCheck++;
	  }
#ifdef XYZcheck
	  if (m_Calibration != 0) {
	    if (XYZ) XYZ->Fill( global.position().x(), global.position().y(), global.position().z());
	    if (iokCheck && XYZbad) XYZbad->Fill( global.position().x(), global.position().y(), global.position().z());
	  }
#endif // XYZcheck
#ifdef PadSelection
	  if (iokCheck) continue;
#endif // PadSelection
	  
	  const StThreeVectorD  &gTopPos = *mRowPosition[sector-1][row-1][1];
	  const StThreeVectorD  &gBotPos = *mRowPosition[sector-1][row-1][2];
	  double s_out = gTrack->geometry()->helix().pathLength(gTopPos, normal);
	  if (s_out > 1.e4) continue;
	  double s_in  = gTrack->geometry()->helix().pathLength(gBotPos, normal);
	  if (s_in > 1.e4) continue;
	  Double_t dx = TMath::Abs(s_out-s_in);
#ifdef PadSelection
	  if (dx < 0.5 || dx > 25.) continue;
#endif // PadSelection
	  ESector kTpcOutIn = kTpcOuter;
	  if (row <= 13) kTpcOutIn = kTpcInner;
	  Double_t d_x = gTrack->geometry()->helix().x(s_in) - 
	    gTrack->geometry()->helix().x(s_out);
	  Double_t d_y = gTrack->geometry()->helix().y(s_in) - 
	    gTrack->geometry()->helix().y(s_out);
	  Double_t dY = normal.x()*d_y  - normal.y()*d_x;
	  if (kTpcOutIn == kTpcOuter) dY /= 0.67;
	  else                        dY /= 0.335;
	  Double_t padlength;
	  if (row<14) padlength = gStTpcDb->PadPlaneGeometry()->innerSectorPadLength();
	  else 	  padlength = gStTpcDb->PadPlaneGeometry()->outerSectorPadLength();
	  StThreeVectorF pxyz = gTrack->geometry()->momentum();
	  pxyz += gTrack->outerGeometry()->momentum();
	  Double_t dn0 = pxyz*normal;
	  dn0         /= pxyz.mag();
	  Double_t dx0 = padlength/dn0;

	  Double_t dE, dER, dEP, dET, dEO, dES, dEZ, dEM, dEX;
	  const Double_t dEU = tpcHit->charge();
	  dE  = dEU;
	  // Corrections
	  Double_t RMS = 1;
	  if (!m_Simulation) {
#ifdef AdcCorrection 
	    if (m_AdcCorrection) {
	      Int_t nc = m_AdcCorrection->GetNRows() - 2;
	      tpcCorrection_st *cor = m_AdcCorrection->GetTable();
	      if (nc >= 0) {
		// correction using positive tracks with built it drift time correction
		// x:x*pow(10.,mu+7.6e-7*y); x = predicted; y = DriftLength*ppOx
		dE = 1.e-6*CalcCorrection(cor+nc+kTpcOutIn,1.e6*dE);
	      }
	    }
#endif
	    dER = dE;
	    Double_t gc = 1;
#ifdef TpcSecRow
	    if (m_TpcSecRow) {
	      TpcSecRowCor_st *gain = m_TpcSecRow->GetTable() + sector - 1;
	      gc =  gain->GainScale[row-1];
	      CdEdx[NdEdx].SigmaFee = gain->GainRms[row-1];
#if 0
	      if (row <= 1) {
		cout << "Sector/row" << sector << "/" << row << "\tgc = " << gc << endl;
		assert(row > 0);
	      }
#endif
	    }
#endif // TpcSecRow
	    if (gc < 0.0) continue;
	    dE *= gc;
	    dES = dE;
	    dE *= row > 13 ? PressureScaleO : PressureScaleI; 
	    dEP = dE;
	    dE = dE*TimeScale;
	    dET = dE;
	    dEM = dE;
	    Double_t DriftDistance;
	    if (row<14) DriftDistance = gStTpcDb->Dimensions()->innerEffectiveDriftDistance();
	    else        DriftDistance = gStTpcDb->Dimensions()->outerEffectiveDriftDistance();
	    CdEdx[NdEdx].ZdriftDistance = DriftDistance - TMath::Abs(global.position().z());
	    CdEdx[NdEdx].ZdriftDistanceO2 = CdEdx[NdEdx].ZdriftDistance*(*m_tpcGas)[0].ppmOxygenIn;
	    if (m_drift) {// Blair correction 
	      tpcCorrection_st *cor = m_drift->GetTable();
	      //	    Double_t DriftCorr = cor->a[1]*(CdEdx[NdEdx].ZdriftDistanceO2 - cor->a[0]);
	      //	    dE *= TMath::Exp(-DriftCorr);
	      dE *= TMath::Exp(-CalcCorrection(cor+kTpcOutIn,CdEdx[NdEdx].ZdriftDistanceO2));
	    }
	    dEO = dE;
#ifdef DriftDistanceCorrection
	    if (m_zCorrection) {
	      tpcCorrection_st *cor = m_zCorrection->GetTable()+kTpcOutIn;
	      if (cor->min > 0 && cor->min > CdEdx[NdEdx].ZdriftDistance) continue;
	      if (cor->max > 0 && CdEdx[NdEdx].ZdriftDistance > cor->max) continue;
	      dE *= TMath::Exp(-CalcCorrection(cor,CdEdx[NdEdx].ZdriftDistance));
	    }
#endif // DriftDistanceCorrection
	    dEZ = dE;
	    if (m_dXCorrection) {
	      tpcCorrection_st *cor = m_dXCorrection->GetTable();
	      Int_t N = m_dXCorrection->GetNRows();
	      Double_t xL2 = TMath::Log2(dx);
	      Double_t dXCorr = CalcCorrection(cor+kTpcOutIn,xL2); 
	      if (N > 2) dXCorr += CalcCorrection(cor+2,xL2);
	      if (N > 6) dXCorr += CalcCorrection(cor+5+kTpcOutIn,xL2);
	      dE *= TMath::Exp(-dXCorr);
	    }
	    dEX = dE;
	  }
	  else {
	    dER = dE;
	    dEP = dE;
	    dET = dE;
	    dEO = dE;
	    dES = dE;
	    dEZ = dE;
	    dEX = dE;
	    dEM = dE;
	  }
	  CdEdx[NdEdx].dE      = dE; // corrected
	  CdEdx[NdEdx].dEU     = dEU; // uncorrected
	  CdEdx[NdEdx].dER     = dER; // row correction
	  CdEdx[NdEdx].dEM     = dEM; // Multiplicity correction
	  CdEdx[NdEdx].dEP     = dEP; // Pressure
	  CdEdx[NdEdx].dET     = dET; // Time 
	  CdEdx[NdEdx].dEO     = dEO; // Blair O2 
	  CdEdx[NdEdx].dES     = dES; // SecRow 
	  CdEdx[NdEdx].dEZ     = dEZ; // Drift Distance
	  CdEdx[NdEdx].dEX     = dEX; // dE correction
	  if (dE <= 0 || dx <= 0) continue;
	  TrackLength         += dx;
	  CdEdx[NdEdx].sector  = sector;
	  CdEdx[NdEdx].row     = row;
	  CdEdx[NdEdx].pad     = Pad.pad();
	  CdEdx[NdEdx].dx      = dx;
	  CdEdx[NdEdx].dx0     = dx0;
	  CdEdx[NdEdx].dEdx    = CdEdx[NdEdx].dE/CdEdx[NdEdx].dx;
	  CdEdx[NdEdx].dEdxL   = TMath::Log(CdEdx[NdEdx].dEdx);
	  CdEdx[NdEdx].dEUdx   = CdEdx[NdEdx].dEU/CdEdx[NdEdx].dx;
	  CdEdx[NdEdx].dEUdxL  = TMath::Log(CdEdx[NdEdx].dEUdx);
	  CdEdx[NdEdx].dERdx   = CdEdx[NdEdx].dER/CdEdx[NdEdx].dx;
	  CdEdx[NdEdx].dERdxL  = TMath::Log(CdEdx[NdEdx].dERdx);
	  CdEdx[NdEdx].dEPdx   = CdEdx[NdEdx].dEP/CdEdx[NdEdx].dx;
	  CdEdx[NdEdx].dEPdxL  = TMath::Log(CdEdx[NdEdx].dEPdx);
	  CdEdx[NdEdx].dETdx   = CdEdx[NdEdx].dET/CdEdx[NdEdx].dx;
	  CdEdx[NdEdx].dEdxLT  = TMath::Log(CdEdx[NdEdx].dETdx);
	  CdEdx[NdEdx].dEOdx   = CdEdx[NdEdx].dEO/CdEdx[NdEdx].dx;
	  CdEdx[NdEdx].dEdxLO  = TMath::Log(CdEdx[NdEdx].dEOdx);
	  CdEdx[NdEdx].dESdx   = CdEdx[NdEdx].dES/CdEdx[NdEdx].dx;
	  CdEdx[NdEdx].dEdxLS  = TMath::Log(CdEdx[NdEdx].dESdx);
	  CdEdx[NdEdx].dEZdx   = CdEdx[NdEdx].dEZ/CdEdx[NdEdx].dx;
	  CdEdx[NdEdx].dEZdxL  = TMath::Log(CdEdx[NdEdx].dEZdx);
	  CdEdx[NdEdx].dEZdx   = CdEdx[NdEdx].dEZ/CdEdx[NdEdx].dx;
	  CdEdx[NdEdx].dEXdx   = CdEdx[NdEdx].dEX/CdEdx[NdEdx].dx;
	  CdEdx[NdEdx].dEXdxL  = TMath::Log(CdEdx[NdEdx].dEXdx);
	  CdEdx[NdEdx].dEMdx   = CdEdx[NdEdx].dEM/CdEdx[NdEdx].dx;
	  CdEdx[NdEdx].dEMdxL  = TMath::Log(CdEdx[NdEdx].dEMdx);
	  CdEdx[NdEdx].xyz[0]  = global.position().x();
	  CdEdx[NdEdx].xyz[1]  = global.position().y();
	  CdEdx[NdEdx].xyz[2]  = global.position().z();
	  CdEdx[NdEdx].Fee     = -1;
	  CdEdx[NdEdx].dY      = dY;
	  CdEdx[NdEdx].RMS     = RMS;
#ifdef SpaceChargeStudy
	  if (m_Calibration != 0) {
	    if (SpaceCharge) SpaceCharge->Fill(global.position().perp(),global.position().z(),CdEdx[NdEdx].dE);
	    if (SpaceChargeU) SpaceChargeU->Fill(global.position().perp(),global.position().z(),CdEdx[NdEdx].dEU);
	    if (Space2Charge) Space2Charge->Fill(global.position().perp(),global.position().z(),CdEdx[NdEdx].dE);
	    if (Space2ChargeU) Space2ChargeU->Fill(global.position().perp(),global.position().z(),CdEdx[NdEdx].dEU);
	  }
#endif // SpaceChargeStudy
	  NdEdx++;
	  if (NdEdx > NoFitPoints) 
	    gMessMgr->Error() << "StdEdxY2Maker:: NdEdx = " << NdEdx 
			      << ">  NoFitPoints ="<< NoFitPoints << endm; 
	}
	if (NdEdx <= 0) continue;
	//      if (TrackLength < TrackLengthCut) continue;
	SortdEdx(NdEdx,CdEdx,dEdxS);
#if 0
	PrintdEdx(2);
#endif
	Double_t I70 = 0, D70 = 0;
	Int_t N70 = NdEdx - (int) (0.3*NdEdx + 0.5); 
	Int_t k;
	for (k = 0; k < N70; k++) {
	  I70 += dEdxS[k].dEdx;
	  D70 += dEdxS[k].dEdx*dEdxS[k].dEdx;
	  TrackLength70 += dEdxS[k].dx;
	}
	Double_t chisq, fitZ, fitdZ;
	DoFitZ(chisq, fitZ, fitdZ);
	tpcCorrection_st *cor = 0;
	tpcCorrection_st *cordEdx = 0;
	if (m_TpcLengthCorrection) cor = m_TpcLengthCorrection->GetTable();
	if (m_TpcdEdxCor) cordEdx = m_TpcdEdxCor->GetTable();
	Double_t LogTrackLength = TMath::Log(TrackLength);
	Int_t NRrowsTL = 0;
	if (m_TpcLengthCorrection) NRrowsTL = m_TpcLengthCorrection->GetNRows();
	if (N70 > 1) {
	  I70 /= N70; D70 /= N70;
	  D70  = TMath::Sqrt(D70 - I70*I70);
	  D70 /= I70;
	  if (cor) {
	    I70 *= TMath::Exp(-CalcCorrection(cor,LogTrackLength));
	    D70  = CalcCorrection(cor+1,LogTrackLength);
	    if (NRrowsTL > 6) {
	      I70 *= TMath::Exp(-CalcCorrection(cor+6,LogTrackLength));
	      if (NRrowsTL > 10 && ! m_ClusterFinder) 
		I70 *= TMath::Exp(-CalcCorrection(cor+10,LogTrackLength));
	      //?	    D70  = CalcCorrection(cor+7,LogTrackLength);
	    }
	  }
	  if (cordEdx) {
	    Double_t I70L = TMath::Log(1.e6*I70) - cordEdx->min;
	    if (I70L > 0) 
	      I70 *= TMath::Exp(-SumSeries(I70L,cordEdx->npar,&cordEdx->a[0]));
	  }
	  dedx.id_track  =  Id;
	  dedx.det_id    =  kTpcId;    // TPC track 
	  dedx.method    =  kTruncatedMeanIdentifier;
	  dedx.ndedx     =  N70 + 100*((int) TrackLength);
	  dedx.dedx[0]   =  I70;
	  dedx.dedx[1]   =  D70;
	  gTrack->addPidTraits(new StDedxPidTraits(dedx));
	  if (pTrack) pTrack->addPidTraits(new StDedxPidTraits(dedx));
	  if (tTrack) tTrack->addPidTraits(new StDedxPidTraits(dedx));
	}
	if (cor) {
	  fitZ -= CalcCorrection(cor+4,LogTrackLength);
	  if (NRrowsTL > 12 && ! m_ClusterFinder) 
	    fitZ -= CalcCorrection(cor+12,LogTrackLength);
	  //	fitdZ = CalcCorrection(cor+5,LogTrackLength);
	  if (cordEdx) {
	    Double_t I70L = TMath::Log(1.e6) + fitZ - cordEdx->min;
	    if (I70L > 0) 
	      fitZ -= SumSeries(I70L,cordEdx->npar,&cordEdx->a[0]);
	  }
	}
	if (chisq >0 && chisq < 10000.0) {
	  dedx.id_track  =  Id;
	  dedx.det_id    =  kTpcId;    // TPC track 
	  dedx.method    =  kLikelihoodFitIdentifier;
	  dedx.ndedx     =  NdEdx + 100*((int) TrackLength);
	  dedx.dedx[0]   =  TMath::Exp(fitZ);
	  dedx.dedx[1]   =  fitdZ; 
	  gTrack->addPidTraits(new StDedxPidTraits(dedx));
	  if (pTrack) pTrack->addPidTraits(new StDedxPidTraits(dedx));
	  if (tTrack) tTrack->addPidTraits(new StDedxPidTraits(dedx));
	}
	StThreeVectorD g3 = gTrack->geometry()->momentum(); // p of global track
	Double_t pMomentum = g3.mag();
	Float_t Chisq[NHYPS];
	for (int hyp = 0; hyp < NHYPS; hyp++) {
	  Double_t bgL10 = TMath::Log10(pMomentum/StProbPidTraits::mPidParticleDefinitions[hyp]->mass());
	  Chisq[hyp] = LikeliHood(bgL10,NdEdx,FdEdx);
	}
	gTrack->addPidTraits(new StProbPidTraits(NdEdx,kTpcId,NHYPS,Chisq));
	if (pTrack) pTrack->addPidTraits(new StProbPidTraits(NdEdx,kTpcId,NHYPS,Chisq));
	if (tTrack) tTrack->addPidTraits(new StProbPidTraits(NdEdx,kTpcId,NHYPS,Chisq));
	//    if (primtrkC && iprim >= 0&& m_Calibration > 0) {
	//      if (m_Calibration != 0 && bField && pTrack) Histogramming(gTrack);
      }
      if (m_Calibration != 0) Histogramming(gTrack);
    }
  }
  if (Debug() > 1) {
    gMessMgr->QAInfo() << "StdEdxY2Maker:"
		       << "  Type: " << pEvent->type()
		       << "  Run: " << pEvent->runId() 
		       << "  Event: " << pEvent->id()
		       << "  # track nodes: "
		       << pEvent->trackNodes().size() << endm;
    
  }
  if (Debug()) {
    timer.stop();
    gMessMgr->QAInfo() << "CPU time for StdEdxY2Maker::Make(): "
		       << timer.elapsedTime() << " sec\n" << endm;
  }
  return kStOK;
}
//________________________________________________________________________________
void StdEdxY2Maker::SortdEdx(Int_t N, dEdx_t *dE, dEdx_t *dES) {
  int i;
  for (i = 0; i < N; i++) dES[i] = dE[i];
  for (i = 0; i < N-1; i++) {
    for (int j = i+1; j < N; j++) {
      if (dES[i].dEdx > dES[j].dEdx) {
	dEdx_t temp = dES[i];
	dES[i] = dES[j];
	dES[j] = temp;
      }
    }
  }
}
//________________________________________________________________________________
void StdEdxY2Maker::Histogramming(StGlobalTrack* gTrack) {
  static const Double_t Log10E = TMath::Log10(TMath::Exp(1.));
  StThreeVectorD g3 = gTrack->geometry()->momentum(); // p of global track
  Double_t pMomentum = g3.mag();
  Double_t Eta = g3.pseudoRapidity();
  Int_t sCharge = 0;
  if (gTrack->geometry()->charge() < 0) sCharge = 1;
  Int_t NoFitPoints = gTrack->fitTraits().numberOfFitPoints();
  //  StTpcDedxPidAlgorithm tpcDedxAlgo;
  // dE/dx
  //  StPtrVecTrackPidTraits traits = gTrack->pidTraits(kTpcId);
  StSPtrVecTrackPidTraits &traits = gTrack->pidTraits();
  unsigned int size = traits.size();
  StDedxPidTraits *pid, *pid70 = 0, *pidF = 0;
  StProbPidTraits *pidprob = 0;
  Double_t I70 = 0, D70 = 0;
  Double_t chisq = 1e10, fitZ = 0, fitdZ = 1e10;
  Int_t N70 = 0, NF = 0;
  Double_t TrackLength70 = 0, TrackLength = 0;
  if (size) {
    for (unsigned int i = 0; i < traits.size(); i++) {
      if (! traits[i]) continue;
      if ( traits[i]->IsZombie()) continue;
      pid = dynamic_cast<StDedxPidTraits*>(traits[i]);
      if (pid) {
	if (pid->method() == kTruncatedMeanIdentifier) {
	  pid70 = pid; I70 = pid70->mean(); N70 = pid70->numberOfPoints();
	  TrackLength70 = pid70->length(); D70 = pid70->errorOnMean();
	}
	if (pid->method() == kLikelihoodFitIdentifier) {
	  pidF = pid;
	  fitZ = TMath::Log(pidF->mean()); NF = pidF->numberOfPoints(); 
	  TrackLength = pidF->length(); fitdZ = pidF->errorOnMean(); 
	}
      }
      else pidprob = dynamic_cast<StProbPidTraits*>(traits[i]);
    }
  }
  if (pid70 && ! pidF) TrackLength = TrackLength70;
  Double_t Pred[NHYPS],  Pred70[NHYPS];
  Double_t PredB[NHYPS], Pred70B[NHYPS];
  Double_t date = GetDateTime().Convert();
  Double_t devZ[NHYPS], devZs[NHYPS];
  Double_t bg = TMath::Log10(pMomentum/StProbPidTraits::mPidParticleDefinitions[kPidPion]->mass());
  Double_t bghyp[NHYPS];
  Int_t l;
  for (l = kPidElectron; l < NHYPS; l += 1) {
    bghyp[l] = TMath::Log10(pMomentum/StProbPidTraits::mPidParticleDefinitions[l]->mass());
    PredB[l]   = 1.e-6*TMath::Exp(m_Bichsel->GetMostProbableZ(bghyp[l],1.0)); 
    Pred70B[l] = 1.e-6*m_Bichsel->GetI70(bghyp[l],1.0); 
    Pred[l] = 1.e-6*BetheBloch::Sirrf(pMomentum/StProbPidTraits::mPidParticleDefinitions[l]->mass(),60.,l==3); 
    Pred70[l] = Pred[l];
    if (pid70 && TrackLength70 > 40.) {
      hist70[l][sCharge]->Fill(bghyp[l],TMath::Log(I70/Pred70[l]));
      histB[l][sCharge]->Fill(bghyp[l],TMath::Log(Pred[l]));
      hist70B[l][sCharge]->Fill(bghyp[l],TMath::Log(I70/Pred70B[l]));
      histBB[l][sCharge]->Fill(bghyp[l],TMath::Log(PredB[l]));
      TdEdxP70->Fill(TMath::Log10(pMomentum), TMath::Log10(I70)+6.);
      devZ[l] = TMath::Log(I70/Pred70B[l]);
    }
    if (pidF && TrackLength > 40.) {
      histz[l][sCharge]->Fill(bghyp[l],fitZ - TMath::Log(Pred[l]));
      histzB[l][sCharge]->Fill(bghyp[l],fitZ - TMath::Log(PredB[l]));
      TdEdxPF->Fill(TMath::Log10(pMomentum), Log10E*fitZ + 6.);
      devZs[l] = TMath::Abs(devZ[l])/fitdZ; //D70;
    }
  }
  Double_t devZmin = 999;
  Int_t PiDStatus = 0;
  Int_t  PiDkey   = -1; // best
  Int_t  PiDkeyU  = -1; // only one with devZs<3, 
  Int_t  PiDkeyU3 = -1; // -"- and devZs > 5 for all others 
  Int_t lBest  = -1;
  if (pidF) {
    for (l = kPidElectron; l < NHYPS; l += 1) {
      if (l == kPidMuon) continue;
      if (devZs[l] < 3.0) PiDStatus += 1<<l;
      if (devZs[l] < devZmin) {devZmin = devZs[l]; lBest = l;}
    }
    if (devZs[lBest] < 3.0) PiDkey = lBest;
    Int_t lNext = -1;
    devZmin = 999;
    for (l = kPidElectron; l < NHYPS; l += 1) {
      if (l == kPidMuon) continue;
      if (l == lBest) continue;
      if (devZs[l] < devZmin) {devZmin = devZs[l]; lNext = l;}
    }
    if (PiDkey >= 0 && devZs[lNext] > 3.) PiDkeyU = PiDkey;
    if (PiDkeyU && devZs[lNext] > 5.) PiDkeyU3 = PiDkeyU;
    else  {if (devZs[lBest] < devZs[lNext] + 5.) PiDkeyU3 = lBest;}
  }
#ifdef ProbabilityPlot
  if (TrackLength70 > 40) { 
    Double_t Z70 = TMath::Log(I70/Pred70[kPidPion]);
    Prob->Fill(bghyp[kPidPion],-0.05,Z70);
    if (pidprob) {
      Int_t N = pidprob->GetPidArray()->GetSize();
      for (int i = 0; i < N; i++) {
	Double_t p = pidprob->GetProbability(i);
	if (p > 0.99) p = 0.99;
	Prob->Fill(bghyp[kPidPion],i+p,Z70);
      }
    }
  }
#endif // ProbabilityPlot
  //  if (D70 > 0.05 && D70 < 0.15) {
  if (pidF && fitdZ > 0.05 && fitdZ < 0.15) {
    ffitZA->Fill(bg,devZ[kPidPion]);
    for (l = kPidElectron; l < NHYPS; l += 1) {
      ffitZ[l]->Fill(bg,devZ[kPidPion]);
      if (pidprob && pidprob->GetSum() > 0 && pidprob->GetProbability(l) > 0.9) ffitP[l]->Fill(bg,devZ[kPidPion]);
      if (PiDkeyU  >= 0) ffitZU->Fill(bghyp[kPidPion],devZ[kPidPion]);
      if (PiDkeyU3 >= 0) ffitZU3->Fill(bghyp[kPidPion],devZ[kPidPion]);
    }
  }
  if (Pred[kPidPion] <= 0) {
    gMessMgr->Warning() << "StdEdxY2Maker:: Prediction for p = " 
			<< pMomentum << " and TrackLength = " << TrackLength 
			<< " is wrong = " << Pred[kPidPion] << " <<<<<<<<<<<<<" << endl;
    return;
  };
  if (pidF) {
    Points->Fill(NdEdx,fitZ-TMath::Log(Pred[kPidPion]));
    PointsB->Fill(NdEdx,fitZ-TMath::Log(PredB[kPidPion]));
    TPoints->Fill(TrackLength,fitZ-TMath::Log(Pred[kPidPion]));
    TPointsB->Fill(TrackLength,fitZ-TMath::Log(PredB[kPidPion]));
    FitPull->Fill(TrackLength,(fitZ - TMath::Log(PredB[kPidPion]))/fitdZ);
  }
  if (pid70) {
    Points70->Fill(N70,TMath::Log(I70/Pred70[kPidPion]));
    Points70B->Fill(N70,TMath::Log(I70/PredB[kPidPion]));
    TPoints70->Fill(TrackLength,TMath::Log(I70/Pred70[kPidPion]));
    TPoints70B->Fill(TrackLength,TMath::Log(I70/Pred70B[kPidPion]));
    Pull70->Fill(TrackLength,TMath::Log(I70/Pred70B[kPidPion])/D70);
  }
  if (! m_DoNotCorrectdEdx) {
    if (NoFitPoints >= 30 && TrackLength > 40) { 
      Int_t k;
      for (k = 0; k < NdEdx; k++) {
	FdEdx[k].zP = m_Bichsel->GetMostProbableZ(TMath::Log10(pMomentum/StProbPidTraits::mPidParticleDefinitions[kPidPion]->mass()),TMath::Log2(FdEdx[k].dx));
	//	FdEdx[k].zP = m_Bichsel->GetAverageZ(TMath::Log10(pMomentum/StProbPidTraits::mPidParticleDefinitions[kPidPion]->mass()),TMath::Log2(FdEdx[k].dx));
	FdEdx[k].sigmaP = m_Bichsel->GetRmsZ(TMath::Log10(pMomentum/StProbPidTraits::mPidParticleDefinitions[kPidPion]->mass()),TMath::Log2(FdEdx[k].dx));
	Double_t predB  = 1.e-6*TMath::Exp(FdEdx[k].zP);
	FdEdx[k].dEdxN  = TMath::Log(FdEdx[k].dEdx /predB);
	FdEdx[k].dEUdxN = TMath::Log(FdEdx[k].dEUdx/predB);
	FdEdx[k].dERdxN = TMath::Log(FdEdx[k].dERdx/predB);
	FdEdx[k].dETdxN = TMath::Log(FdEdx[k].dETdx/predB);
	FdEdx[k].dEPdxN = TMath::Log(FdEdx[k].dEPdx/predB);
	FdEdx[k].dEOdxN = TMath::Log(FdEdx[k].dEOdx/predB);
	FdEdx[k].dESdxN = TMath::Log(FdEdx[k].dESdx/predB);
	FdEdx[k].dEZdxN = TMath::Log(FdEdx[k].dEZdx/predB);
	FdEdx[k].dEXdxN = TMath::Log(FdEdx[k].dEXdx/predB);
#ifdef CORRELATION
	if (chisq > 0 && chisq < 10000.0) {
	  Double_t zk  = FdEdx[k].zdev;
	  if (FdEdx[k].Prob > 1.e-12) {
	    if (FdEdx[k].row > 13) corrO1w->Fill(zk,1./FdEdx[k].Prob);
	    else                   corrI1w->Fill(zk,1./FdEdx[k].Prob);
	  }
	  for (int m = 0; m < NdEdx; m++){
	    if (k == m) continue;
	    Double_t zl  = FdEdx[m].zdev;
	    if (FdEdx[m].row%2 == 1 && FdEdx[m].row - FdEdx[k].row  == 1) {
	      if (FdEdx[k].row > 13) {
		corrO->Fill(zk,zl); 
		if (FdEdx[k].Prob*FdEdx[m].Prob > 1.e-12) 
		  corrOw->Fill(zk,zl,1./(FdEdx[k].Prob*FdEdx[m].Prob));
	      }
	      else {
		corrI->Fill(zk,zl); 
		if (FdEdx[k].Prob*FdEdx[m].Prob > 1.e-12) 
		  corrIw->Fill(zk,zl,1./(FdEdx[k].Prob*FdEdx[m].Prob));
	      }
	    }
	    if (FdEdx[m].row%2 == 1 && FdEdx[m].row - FdEdx[k].row  == 2) {
	      if (FdEdx[k].row > 13) {
		corrO2->Fill(zk,zl); 
		if (FdEdx[k].Prob*FdEdx[m].Prob > 1.e-12) 
		  corrO2w->Fill(zk,zl,1./(FdEdx[k].Prob*FdEdx[m].Prob));
	      }
	      else {
		corrI2->Fill(zk,zl); 
		if (FdEdx[k].Prob*FdEdx[m].Prob > 1.e-12) 
		  corrI2w->Fill(zk,zl,1./(FdEdx[k].Prob*FdEdx[m].Prob));
	      }
	    }
	    if (FdEdx[m].row%2 == 1 && FdEdx[m].row - FdEdx[k].row  == 5) {
	      if (FdEdx[k].row > 13) {
		corrO5->Fill(zk,zl); 
		if (FdEdx[k].Prob*FdEdx[m].Prob > 1.e-12) 
		  corrO5w->Fill(zk,zl,1./(FdEdx[k].Prob*FdEdx[m].Prob));
	      }
	      else {
		corrI5->Fill(zk,zl); 
		if (FdEdx[k].Prob*FdEdx[m].Prob > 1.e-12) 
		  corrI5w->Fill(zk,zl,1./(FdEdx[k].Prob*FdEdx[m].Prob));
	      }
	    }
	  } // end of l loop
	}
#endif // CORRELATION
	if (FdEdx[k].row < 14) {
	  hdEI->Fill(TMath::Log10(FdEdx[k].dE));
	  hdEUI->Fill(TMath::Log10(FdEdx[k].dEU));
	  hdERI->Fill(TMath::Log10(FdEdx[k].dER));
	  hdEPI->Fill(TMath::Log10(FdEdx[k].dEP));
	  hdETI->Fill(TMath::Log10(FdEdx[k].dET));
	  hdESI->Fill(TMath::Log10(FdEdx[k].dES));
	  hdEZI->Fill(TMath::Log10(FdEdx[k].dEZ));
	  hdEMI->Fill(TMath::Log10(FdEdx[k].dEM));
	}
	else {
	  hdEO->Fill(TMath::Log10(FdEdx[k].dE));
	  hdEUO->Fill(TMath::Log10(FdEdx[k].dEU));
	  hdERO->Fill(TMath::Log10(FdEdx[k].dER));
	  hdEPO->Fill(TMath::Log10(FdEdx[k].dEP));
	  hdETO->Fill(TMath::Log10(FdEdx[k].dET));
	  hdESO->Fill(TMath::Log10(FdEdx[k].dES));
	  hdEZO->Fill(TMath::Log10(FdEdx[k].dEZ));
	  hdEMO->Fill(TMath::Log10(FdEdx[k].dEM));
	}
#ifdef AdcHistos
	if (PiDkeyU3 >= 0) {
	  Double_t betaXgamma = pMomentum/StProbPidTraits::mPidParticleDefinitions[PiDkeyU3]->mass();
	  Double_t zA = m_Bichsel->GetMostProbableZ(TMath::Log10(betaXgamma),TMath::Log2(FdEdx[k].dx));
	  Double_t PredA = 1.e-6*TMath::Exp(zA);
	  Double_t PredE =  PredA*FdEdx[k].dx;
	  Double_t PredEL = TMath::Log10(PredE);
	  if (FdEdx[k].row < 14) {
	    if (AdcI)        AdcI->Fill(PredEL,TMath::Log10(FdEdx[k].dEU));
	    if (AdcIC)      AdcIC->Fill(PredEL,TMath::Log10(FdEdx[k].dE));
	  }
	  else {
	    if (AdcO)        AdcO->Fill(PredEL,TMath::Log10(FdEdx[k].dEU));
	    if (AdcOC)      AdcOC->Fill(PredEL,TMath::Log10(FdEdx[k].dE));
	  }
	  Double_t PredEU =  PredE;
	  Double_t PredEUL = TMath::Log10(PredEU);
	  Double_t PredEULN = TMath::Log(PredEU);
	  Double_t PredEUkeV = 1.e6*PredE;
	  if (m_tpcGas) {
	  if (FdEdx[k].row < 14) {
	    if (Adc3I)   Adc3I->Fill(PredEUL,TMath::Log10(FdEdx[k].dEU));
	    if (Adc3IC) Adc3IC->Fill(PredEUL,TMath::Log10(FdEdx[k].dE));
	    if (Adc3Ip && Adc3Ip[PiDkeyU3]) 
	      Adc3Ip[PiDkeyU3]->Fill(PredEUL,TMath::Log10(FdEdx[k].dEU));
	    if (sCharge == 0) 
	      AdcIZP->Fill(PredEUkeV,FdEdx[k].ZdriftDistanceO2,TMath::Log(FdEdx[k].dEU)-PredEULN);
	    else 
	      AdcIZN->Fill(PredEUkeV,FdEdx[k].ZdriftDistanceO2,TMath::Log(FdEdx[k].dEU)-PredEULN);
	  }
	  else {		   					                            
	    if (Adc3O)   Adc3O->Fill(PredEUL,TMath::Log10(FdEdx[k].dEU));
	    if (Adc3OC) Adc3OC->Fill(PredEUL,TMath::Log10(FdEdx[k].dE));
	    if (Adc3Op && Adc3Op[PiDkeyU3]) 
	      Adc3Op[PiDkeyU3]->Fill(PredEUL,TMath::Log10(FdEdx[k].dEU));
	    if (sCharge == 0) 
	      AdcOZP->Fill(PredEUkeV,FdEdx[k].ZdriftDistanceO2,TMath::Log(FdEdx[k].dEU)-PredEULN);
	    else 
	      AdcOZN->Fill(PredEUkeV,FdEdx[k].ZdriftDistanceO2,TMath::Log(FdEdx[k].dEU)-PredEULN);
	  }
	  }
	}
#endif // AdcHistos
	  if (m_tpcGas) {
	    if (inputTPCGasPressureP) inputTPCGasPressureP->Fill((*m_tpcGas)[0].inputTPCGasPressure,FdEdx[k].dEdxN);
	    if (nitrogenPressureP) nitrogenPressureP->Fill((*m_tpcGas)[0].nitrogenPressure,FdEdx[k].dEdxN);
	    if (gasPressureDiffP) gasPressureDiffP->Fill((*m_tpcGas)[0].gasPressureDiff,FdEdx[k].dEdxN);
	    if (inputGasTemperatureP) inputGasTemperatureP->Fill((*m_tpcGas)[0].inputGasTemperature,FdEdx[k].dEdxN);
	    if (outputGasTemperatureP) outputGasTemperatureP->Fill((*m_tpcGas)[0].outputGasTemperature,FdEdx[k].dEdxN);
	    if (flowRateArgon1P) flowRateArgon1P->Fill((*m_tpcGas)[0].flowRateArgon1,FdEdx[k].dEdxN);
	    if (flowRateArgon2P) flowRateArgon2P->Fill((*m_tpcGas)[0].flowRateArgon2,FdEdx[k].dEdxN);
	    if (flowRateMethaneP) flowRateMethaneP->Fill((*m_tpcGas)[0].flowRateMethane,FdEdx[k].dEdxN);
	    if (percentMethaneInP) percentMethaneInP->Fill((*m_tpcGas)[0].percentMethaneIn,FdEdx[k].dEdxN);
	    if (ppmOxygenInP) ppmOxygenInP->Fill((*m_tpcGas)[0].ppmOxygenIn,FdEdx[k].dEdxN);
	    if (flowRateExhaustP) flowRateExhaustP->Fill((*m_tpcGas)[0].flowRateExhaust,FdEdx[k].dEdxN);
	    if (ppmWaterOutP) ppmWaterOutP->Fill((*m_tpcGas)[0].ppmWaterOut,FdEdx[k].dEdxN);
	    if (ppmOxygenOutP) ppmOxygenOutP->Fill((*m_tpcGas)[0].ppmOxygenOut,FdEdx[k].dEdxN);
	    if (flowRateRecirculationP) flowRateRecirculationP->Fill((*m_tpcGas)[0].flowRateRecirculation,FdEdx[k].dEdxN);
	  }
	  if (m_trig) {
	    if (m_trig->zdcX > 0 && ZdcCP) ZdcCP->Fill(TMath::Log10(m_trig->zdcX), FdEdx[k].dEdxN);
	    if (m_trig->bbcX > 0 && BBCP) BBCP->Fill(TMath::Log10(m_trig->bbcX), FdEdx[k].dEdxN);
	    if (m_trig->L0   > 0 && L0P) L0P->Fill(TMath::Log10(m_trig->L0), FdEdx[k].dEdxN);
	    if (m_trig->mult > 0) {
	      if (MultiplicityPI && FdEdx[k].row < 14) MultiplicityPI->Fill(TMath::Log10(m_trig->mult), FdEdx[k].dEdxN);
	      if (MultiplicityPO && FdEdx[k].row > 13) MultiplicityPO->Fill(TMath::Log10(m_trig->mult), FdEdx[k].dEdxN);
	    }
	  }
#ifdef GetTpcGainMonitor
	  if (m_tpcGainMonitor) {
	    if (GainMonitor)  GainMonitor->Fill((*m_tpcGainMonitor)[0].center, FdEdx[k].dEdxN);
	  }
#endif // GetTpcGainMonitor 
	if (pMomentum > pMomin && pMomentum < pMomax) { // Momentum cut
	  // Correction 
	  Double_t eta =  Eta;
	  if (SecRow3 )  SecRow3->Fill(FdEdx[k].sector+0.5,FdEdx[k].row+0.5,FdEdx[k].dERdxN);
	  if (SecRow3A) SecRow3A->Fill(FdEdx[k].sector+0.5,FdEdx[k].row+0.5,FdEdx[k].dESdxN);
	  if (SecRow3C) SecRow3C->Fill(FdEdx[k].sector+0.5,FdEdx[k].row+0.5,FdEdx[k].dEdxN);
#ifdef Mip
	  if (SecRow3Mip && TMath::Abs(devZs[kPidPion]) < 2) 
	    SecRow3Mip->Fill(FdEdx[k].row+0.5,
			     TMath::Log(FdEdx[k].dx)/TMath::Log(2.),
			     FdEdx[k].dEdxN);// /FdEdx[k].sigmaP);
#endif // Mip
	  if (m_tpcGas && Pressure) {
	    Pressure->Fill(FdEdx[k].row+0.5,TMath::Log((*m_tpcGas)[0].barometricPressure),
			   FdEdx[k].dESdxN);
			   //		  FdEdx[k].dERdxL - TMath::Log(predB));
	    //				  FdEdx[k].dEZdxL - TMath::Log(predB));
	    if (PressureA) PressureA->Fill(FdEdx[k].row+0.5,TMath::Log((*m_tpcGas)[0].barometricPressure),
					   FdEdx[k].dEPdxN);
	    if (PressureC) PressureC->Fill(FdEdx[k].row+0.5,TMath::Log((*m_tpcGas)[0].barometricPressure),
					   FdEdx[k].dEdxN);
	  }
	  if (Time)    Time->Fill(date,FdEdx[k].dEUdxN);
	  if (TimeP)  TimeP->Fill(date,FdEdx[k].dEPdxN);
	  if (TimeC)  TimeC->Fill(date,FdEdx[k].dEdxN);
	  Int_t SectN = FdEdx[k].sector; // drift distance
	  if (FdEdx[k].row < 14) SectN += 24;
	  if (Z3O)  Z3O->Fill(SectN,FdEdx[k].ZdriftDistanceO2,FdEdx[k].dEPdxN);
	  if (Z3OA)Z3OA->Fill(SectN,FdEdx[k].ZdriftDistanceO2,FdEdx[k].dEOdxN);
	  if (Z3OC)Z3OC->Fill(SectN,FdEdx[k].ZdriftDistanceO2,FdEdx[k].dEdxN);
	  if (Z3)    Z3->Fill(SectN,FdEdx[k].ZdriftDistance,  FdEdx[k].dEOdxN);
	  if (Z3A)  Z3A->Fill(SectN,FdEdx[k].ZdriftDistance,  FdEdx[k].dEZdxN);
	  if (Z3C)  Z3C->Fill(SectN,FdEdx[k].ZdriftDistance,  FdEdx[k].dEdxN);
	  if (ETA)  ETA->Fill(SectN,eta,FdEdx[k].dEdxN);
	  if (ETA3)ETA3->Fill(SectN,eta,FdEdx[k].dEdxN);
	  if (m_trig && m_trig->mult > 0) {
	    if (MulRow)   MulRow->Fill(TMath::Log10(m_trig->mult),FdEdx[k].row+0.5,FdEdx[k].dEZdxN);
	    if (MulRowC) MulRowC->Fill(TMath::Log10(m_trig->mult),FdEdx[k].row+0.5,FdEdx[k].dEdxN);
	  }
	  if (dYrow) dYrow->Fill(FdEdx[k].dY,FdEdx[k].row+0.5,FdEdx[k].dEdxN);
	  if (dx0dx) dx0dx->Fill(FdEdx[k].dx, FdEdx[k].dx0);
	  if (dXdE )  dXdE->Fill(FdEdx[k].dx,FdEdx[k].row+0.5,FdEdx[k].dEZdxN);
	  if (dXdEA) dXdEA->Fill(FdEdx[k].dx,FdEdx[k].row+0.5,FdEdx[k].dEXdxN);
	  if (dXdEC) dXdEC->Fill(FdEdx[k].dx,FdEdx[k].row+0.5,FdEdx[k].dEdxN);
	}
#ifdef ZBGX
	if (pMomentum > 1.5 && PiDkey >= 0 && zbgx) {
	  zbgx[PiDkey]->Fill(bghyp[PiDkey],TMath::Log(FdEdx[k].dx)/TMath::Log(2.),FdEdx[k].dEdxL-GeV2keV);
	}
#endif // ZBGX
      }
    }
  }
  // dE/dx tree
  if (ftrack && ftree && TrackLength70 > 20.0) {
    ftrack->Clear();
    ftrack->sCharge = 1 - 2*sCharge;
    ftrack->p = pMomentum;
    ftrack->Eta = Eta;
    ftrack->R0 = gTrack->geometry()->origin().mag();
    ftrack->Z0 = gTrack->geometry()->origin().z();
    ftrack->Phi0 = gTrack->geometry()->origin().phi();
    ftrack->NoFitPoints = NoFitPoints;
    ftrack->N70 = N70;
    ftrack->I70 = I70;
    ftrack->TrackLength70 = TrackLength70;
    ftrack->NdEdx = NdEdx;
    ftrack->chisq = chisq;
    ftrack->fitZ = fitZ;
    ftrack->fitdZ = fitdZ;
    ftrack->TrackLength = TrackLength;
    ftrack->PredP = Pred70[kPidProton];
    ftrack->PredK = Pred70[kPidKaon];
    ftrack->PredPi = Pred70[kPidPion];
    ftrack->PredE = Pred70[kPidElectron];
    for (Int_t k = 0; k < NdEdx; k++) {
      ftrack->AddPoint(FdEdx[k]);
    }
    ftree->Fill();
  }
  return;
}
//_____________________________________________________________________________
void StdEdxY2Maker::PrintdEdx(Int_t iop) {
  const Char_t *Names[3] = {"CdEdx","FdEdx","dEdxS"};
  if (iop < 0 || iop > 2) return;
  dEdx_t *dEdx;
  Double_t I = 0, avrz = 0;
  Int_t N70 = NdEdx - (int) (0.3*NdEdx + 0.5); 
  Int_t N60 = NdEdx - (int) (0.4*NdEdx + 0.5);
  Double_t I70 = 0, I60 = 0;
  for (int i=0; i< NdEdx; i++) {
    if (iop == 0) dEdx = &CdEdx[i];
    else if (iop == 1) dEdx = &FdEdx[i];
    else if (iop == 2) dEdx = &dEdxS[i];
    I = (i*I +  dEdx->dEdx)/(i+1);
    cout << Names[iop] << "\t" << i << "\tsector\t" << dEdx->sector << "\trow\t" << dEdx->row
	 << "\tdEdx(keV/cm)\t" << 1.e6*dEdx->dEdx << "\tdx\t" << dEdx->dx << "\tSum\t" << 1.e6*I << "(keV)\tProb\t" 
	 << dEdx->Prob << endl;
    if (iop == 2) {
      if (i < N60) I60 += dEdx->dEdx;
      if (i < N70) I70 += dEdx->dEdx;
      if (i == N60 - 1) {
	I60 /= N60;
	cout << " ======================= I60 \t" << I60 << endl;
      }
      if (i == N70 - 1) {
	I70 /= N70;
	cout << " ======================= I70 \t" << I70 << endl;
      }
    }
    avrz += TMath::Log(dEdx->dEdx);
  }
  avrz /= NdEdx;
  cout << "mean dEdx \t" << I << "\tExp(avrz)\t" << TMath::Exp(avrz) << endl;
}
//________________________________________________________________________________
Double_t StdEdxY2Maker::LikeliHood(Double_t Xlog10bg, Int_t NdEdx, dEdx_t *dEdx) {
  //SecRowMipFitpHist298P02gh1.root  correction to most probable value vs log2(dx)
  //  static const Double_t probdx2[3] = {-3.58584e-02, 4.16084e-02,-1.45163e-02};// 
  static Double_t ProbCut = 1.e-4;
  Double_t f = 0;
  tpcCorrection_st *cor = 0;
  if (m_dXCorrection) {
    cor = m_dXCorrection->GetTable();
    Int_t N = m_dXCorrection->GetNRows();
    if (N > 3) cor += 3; 
    else       cor = 0;
  } 
  for (int i=0;i<NdEdx; i++) {
    Double_t Ylog2dx = TMath::Log2(dEdx[i].dx);
    ESector l = kTpcInner;
    if (dEdx[i].row > 13) l = kTpcOuter;
    Double_t sigmaC = 0;
    if (cor) sigmaC = CalcCorrection(cor+l,Ylog2dx);
    Double_t zMostProb = m_Bichsel->GetMostProbableZ(Xlog10bg,Ylog2dx);
    Double_t sigma     = m_Bichsel->GetRmsZ(Xlog10bg,Ylog2dx) + sigmaC;
    Double_t xi = (dEdx[i].dEdxL - GeV2keV - zMostProb)/sigma;
    Double_t  Phi = m_Bichsel->GetProbability(Xlog10bg,Ylog2dx,xi);
    dEdx[i].Prob = Phi/sigma;
    if (dEdx[i].Prob < ProbCut) {
      dEdx[i].Prob = ProbCut; 
    }
    f -= 2*TMath::Log( dEdx[i].Prob );
  }
  return f;
}
//________________________________________________________________________________
Double_t SumSeries(const Double_t &X,const Int_t &N,const Double_t *params) {
  Double_t Sum = 0;
  if (N > 0) {
    Sum = params[N-1];
    for (int n = N-2; n>=0; n--) Sum = X*Sum + params[n];
  }
  return Sum;
}
//________________________________________________________________________________
Double_t SumSeries(const Double_t &X,const Int_t &N,const Float_t *params) {
  Double_t Sum = 0;
  if (N > 0) {
    Sum = params[N-1];
    for (int n = N-2; n>=0; n--) Sum = X*Sum + params[n];
  }
  return Sum;
}
//________________________________________________________________________________
Double_t StdEdxY2Maker::CalcCorrection(const tpcCorrection_st *cor,const Double_t x) {
  Int_t N = TMath::Abs(cor->npar);
  Double_t X = x;
  if (cor->npar  < 0) X = TMath::Exp(x);
  if (N > 0) {
    if (cor->min < cor->max) {
      if (X < cor->min) X = cor->min;
      if (X > cor->max) X = cor->max;
    }
    return SumSeries(X,N,&cor->a[0]);
  }
  else return 0;
}
//________________________________________________________________________________
void StdEdxY2Maker::Landau(Double_t x, Double_t *val){
  // TMath::Log from Landau
  //  TF1 *LandauF = new TF1("LandauF","exp([0]-0.5*((x-[1])/[2])*((x-[1])/[2])+exp([3]-0.5*((x-[4])/[5])*((x-[4])/[5])))");
  static Double_t params[6] = {
    -3.93739e+00,//    1  p0           5.96123e-03   2.40826e-06  -8.19249e-03
      1.98550e+00,//    2  p1           7.17058e-03   3.54798e-06   2.69672e-03
      1.56338e+00,//    3  p2           1.37436e-03   2.59636e-06   3.43991e-03
      1.44692e+00,//    4  p3           3.29935e-03   7.69892e-07  -3.93145e-02
      -4.93793e-01,//    5  p4           2.46951e-03   2.28224e-06  -8.62375e-03
      1.54585e+00 //    6  p5           2.63794e-03   2.24202e-06  -8.62431e-03
      };
  Double_t dev1 = (x-params[1])/params[2];
  Double_t dev2 = (x-params[4])/params[5];
  Double_t c    = TMath::Exp(params[3]-0.5*dev2*dev2);
  val[0] = params[0]-0.5*dev1*dev1+c;
  val[1] = - (dev1/params[2]+c*dev2/params[5]);
}
//________________________________________________________________________________
void StdEdxY2Maker::fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag)
{
  static Double_t sigma_p[3] = {// sigma versus ::log(dX)
    5.31393e-01,//    1  p0  1.33485e-03   7.13072e-07   7.08416e-08
      -1.43277e-01,//    2  p1  3.36846e-03   6.62434e-07  -1.13681e-05
      2.43800e-02 //    3  p2  1.81240e-03   4.02492e-07  -2.08423e-05
      };
  f = 0.;
  gin[0] = 0.;
  gin[1] = 0.;
  Double_t Val[2];
  for (int i=0;i<NdEdx; i++) {
    Double_t sigma = SumSeries(TMath::Log(FdEdx[i].dx),3,sigma_p);
    FdEdx[i].zdev    = (FdEdx[i].dEdxL-par[0])/sigma;
    Landau(FdEdx[i].zdev,Val);
    FdEdx[i].Prob = TMath::Exp(Val[0]);
    f      -= Val[0];
    gin[0] += Val[1]/sigma;
  }
}
//________________________________________________________________________________
void StdEdxY2Maker::DoFitZ(Double_t &chisq, Double_t &fitZ, Double_t &fitdZ){
  Double_t avz = 0;
  for (int i=0;i<NdEdx;i++) {
    FdEdx[i] = CdEdx[i];
    avz += FdEdx[i].dEdxL;
    for (int j=0;j<i;j++) {// order by rows to account correlations
      if (FdEdx[i].sector == FdEdx[j].sector &&
	  FdEdx[i].row    <  FdEdx[j].row) {
	dEdx_t temp = FdEdx[j];
	FdEdx[j] = FdEdx[i];
	FdEdx[i] = temp;
      }
    }
  }
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
