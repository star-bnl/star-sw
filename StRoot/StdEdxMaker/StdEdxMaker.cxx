// $Id: StdEdxMaker.cxx,v 1.17 2001/08/03 18:45:05 fine Exp $
#include <iostream.h>
#include "StdEdxMaker.h"
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
// StUtilities
#include "StMessMgr.h" 
#include "BetheBloch.h"
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
#include "tables/St_TpcTimeGain_Table.h"
#include "tables/St_TpcDriftDistCorr_Table.h"
#include "tables/St_fee_vs_pad_row_Table.h"
#include "tables/St_tpcBadPad_Table.h"
#include "tables/St_dst_event_summary_Table.h"

// global
#include "StDetectorId.h"
#include "StDedxMethod.h"
// StarClassLibrary
#include "StThreeVector.hh" 
#include "StHelixD.hh"
// Chairs
#include "StDstPointChair.h"
#include "StTrackChair.h"
// StDb
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StDbUtilities/StTpcLocalSectorCoordinate.hh"
#include "StDbUtilities/StTpcLocalCoordinate.hh"
#include "StDbUtilities/StTpcPadCoordinate.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StTpcDb/StTpcDb.h"
#include "StTpcDb/StTpcPadPlaneI.h"
const Int_t NRows =  45;
const Int_t NPads = 185;
const Int_t NTimeBins = 513;
BetheBloch BB;
const Int_t NHYPS = 5;
const Char_t *Names[] = {"p",
			 "K",
			 "pi",
			 "e",
			 "d"};
const Double_t Masses[] = {0.93827231,
			   0.493677,
			   0.13956995,
			   0.51099907e-3,
			   1.87561339};

#include "dEdxPoint.h"
TableClassImpl(St_dEdxPoint,dEdxPoint);
class MyTableSorter : public TTableSorter {
public: 
  MyTableSorter() : TTableSorter() {;}
  MyTableSorter(const TTable &table, TString &colName, Int_t firstRow=0,Int_t numbeRows=0) :
    TTableSorter(table,colName, firstRow, numbeRows) {;}
  MyTableSorter(const TTable *table, TString &colName, Int_t firstRow=0,Int_t numbeRows=0) :
    TTableSorter(table, colName, firstRow, numbeRows){;}
  virtual ~MyTableSorter() {;}
  void **GetSortIndex() {return fSortIndex;}
};
struct dEdx_t {
  Int_t sector;
  Int_t row;
  Int_t pad;
  Int_t Fee;
  Double_t dE;
  Double_t dEU; // before correction
  Double_t dx;
  Double_t dEdx; 
  Double_t dEdxU; 
  Double_t dEdxP; // after pulser correction only
  Double_t dEdxL; // log of dEdx
  Double_t dEdxLU; // log of dEdx
  Double_t dEdxN; // normolized to BB
  Double_t dEdxNP; // normolized to BB
  Double_t dETot; 
  Double_t xyz[3];
  Double_t Prob; 
  Double_t SigmaFee;
  Double_t xscale;
  Double_t dEIpad;  // total charge integrated so far in the pad
  Double_t dEI3pad; // total charge integrated so far in the pad +/-
  Double_t dEIrow;  // total charge integrated so far in the row
  Double_t dETrow;  // total charge not integrated (time bucket only) in the row
  Double_t dET3row; // total charge not integrated (+0 + 2 time buckets only) in the row
  Double_t dET5row; // total charge not integrated (+0 + 4 time buckets only) in the row
  Double_t zdev; 
  Double_t dY;      // Projection on the wire
  Double_t RMS;     // rms from volume charge
};
enum EFitCase {kVal, kGrad};
enum ESector  {kTpcInner, kTpcOuter};
dEdx_t CdEdx[60]; // corrected
dEdx_t FdEdx[60]; // fit
Int_t NdEdx = 0, NPoints = 0;
dEdx_t dEdxS[60]; // dEdx sorted
Double_t dE, dEP, dx, dEdx, Sigma_dEdx;
Int_t NoSector = 24;
Int_t NoRow    = 45;

TF1 *fFunc = 0;
TCanvas *fCanvas = 0;
TH1F    *h1 = 0;
TMinuit *gMinuit = new TMinuit(2);
static const Double_t VolChargeRMS[2][3] = {
  { 6.07200e-01, 2.69551e+03,-9.37022e+06}, // Inner
  { 5.03490e-01, 5.79999e+02,-1.17173e+06}  // Outer
};
static const Double_t VolChargePars[2][3] = {
  { 3.74430e-02,-4.61520e+03, 1.21373e+07}, // Inner
  { 4.50309e-02,-8.74618e+02, 8.21877e+05}  // Outer
};
static const Double_t VolChargePar2[2][3] = {
  {-4.77366e-02, 2.14448e-02, 7.71856e+05},
  {-6.43962e-02, 2.56413e-02, 3.54593e+05}
};
static const Double_t TimeBucketPars[2][3] = {
  { 2.65854e-02,-7.27281e+02, 1.76017e+06}, // Inner
  { 3.97145e-02,-2.23166e+02, 1.22639e+05}  // Outer
};
// correction for signal width versus Half Fee no.
// Shape 
static const Double_t ShapeMean[2][3] = {
  {-2.32656e-01, 7.63952e-02, 6.59020e-03}, // Inner
  {-3.75038e-01, 1.28860e-01,-6.93063e-02}	// Outer
};
static const Double_t ShapeMean2[2][3] = {
//{-3.33717e-02,-6.35287e-02,-2.80961e-02}, // Inner
//{-3.62925e-02,-2.16005e-02,-4.50694e-02}, // Inner hist104
//{-3.14945e-02, 2.71034e-02,-1.32465e-01}, // Inner
//{-3.26109e-02, 1.37051e-01,-9.43474e-02}, // Outer hist104
//{-7.71826e-02, 2.30737e-01,-1.65576e-01}
  {-3.35910e-02,-2.99541e-03,-1.11952e-01}, // Inner hist105
  {-5.83643e-02, 2.27074e-01,-1.64927e-01}  // Outer hist105
};
static const Double_t ShapeMean3[2][3] = {
  { 1.06973e-02, 6.54256e-03,-6.50178e-02},
  {-4.25648e-02, 1.14774e-01,-7.74049e-02}
};

static const Double_t ShapeSigma[2][3] = {
  { 6.41952e-01,-2.19547e-01, 9.63439e-03}, // Inner
  { 8.29636e-01,-3.71708e-01, 8.65136e-02}	// Outer
};
static const Int_t NpGaus = 18;  // hist108
static const Double_t parGaus[2][NpGaus] = {
  {2.57760e-02, 1.44032e+00, 2.45484e+00, // inner  FCN=827.986
   7.05363e-02,-7.36314e-01, 1.12509e+00,
   2.27172e-02, 1.76234e+00, 8.55844e-01,
   1.29267e-02, 2.48337e+00, 9.63194e-01,
   2.33560e-01, 1.12666e-01, 9.40526e-01,
   3.68642e-03,-2.55756e+00, 1.28595e+00},
  {3.98911e-01, 3.62377e-02, 1.39581e+00, // outer FCN=1299.15 
   1.85302e-01,-7.82685e-02, 8.77156e-01,
   8.66260e-03, 2.37183e+00, 1.06643e+00,
   1.08442e-03, 3.07282e+00, 2.79578e+00,
  -2.64447e-01,-8.55225e-02, 1.40087e+00,
   1.70728e-02, 2.29372e+00, 2.21135e+00}
//   // hist109 
//   { 3.18766e-02, 1.33053e+00, 2.42907e+00, //inner FCN=1182.04
//     2.27104e-01, 9.24565e-01, 1.72365e+00,
//     2.84830e-01, 3.25272e-01, 1.09503e+00,
//    -1.63260e-01, 1.28070e+00, 1.69563e+00,
//    -1.19206e-01, 1.02090e+00, 8.82685e-01,
//     3.50740e-04, 3.79671e+00, 5.87450e-01},// outer FCN=57017.9
//   { 1.38222e-01, 2.01722e+00, 1.93379e+00,
//     4.69376e-01, 3.26876e-01, 1.11854e+00,
//     1.65910e-01, 2.02170e+00, 8.72250e-01,
//     3.34840e-01, 2.97259e+00, 1.19772e+00,
//    -2.88727e-02, 3.68941e+00, 9.47871e-01,
//    -5.96186e-01, 2.12406e+00, 1.52948e+00}
};
extern    void fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *u, Int_t flag);
extern    void fGaus(Double_t x, Double_t *val, ESector l = kTpcInner);
extern    Double_t II3padC(Double_t x, Double_t y, Int_t IO);
// Histograms
TProfile2D *Z = 0, *ZC = 0, *ETA = 0, *SecRow = 0, *MulRow = 0, *ZRow = 0, *Fee = 0, *FeeC = 0;// *sXY = 0, *SXY = 0;
TH2F *TPoints = 0, *TPoints60 = 0, *TPoints70 = 0;
TH2F *Time = 0, *Points70 = 0, *Points60 = 0, *Points70B = 0, *Points60B = 0, *SR = 0, *PointsFit = 0;
TH2F *FShapeI = 0, *FShapeO = 0, *FShapeIC = 0, *FShapeOC = 0;
TH2F *corrI = 0, *corrO = 0, *corrI2 = 0, *corrO2 = 0, *corrI5 = 0, *corrO5 = 0;
TH2D *hist70[NHYPS][2], *hist60[NHYPS][2], *histz[NHYPS][2];
TH2F *FitPull = 0;
TH2F *corrIw = 0, *corrOw = 0, *corrI2w = 0, *corrO2w = 0, *corrI5w = 0, *corrO5w = 0;
TH1F *corrI1w = 0, *corrO1w = 0;
//TProfile2D *XYZ = 0, *XYZbad = 0;
TProfile2D *Ipad = 0, *I3pad = 0, *II3padI = 0,*II3padO = 0, *Irow = 0, *Trow = 0, *T3row = 0, *T5row = 0, *dYrow = 0;
TProfile *histB[NHYPS][2], *histBB[NHYPS][2]; 
ClassImp(StdEdxMaker)  

//_____________________________________________________________________________
StdEdxMaker::StdEdxMaker(const char *name):StMaker(name), 
m_TpcSecRow(0),m_fee_vs_pad_row(0), m_tpcTime(0), m_drift(0), m_badpads(0), m_Simulation(kFALSE) {}
//_____________________________________________________________________________
StdEdxMaker::~StdEdxMaker(){}
//_____________________________________________________________________________
Int_t StdEdxMaker::Init(){
  if (((StBFChain *)GetChain())->GetOption("Simu")) {
    m_Simulation = kTRUE;
    gMessMgr->Warning() << "StdEdxMaker:: use Simulation mode (no calibration) " << endm;
  }
  if (m_Mode > 0) {// calibration mode
    TFile *f = (TFile *) ((StBFChain *)GetChain())->GetTFile();
    if (f) {
      f->cd();
      //      XYZ = new TProfile2D("XYZ","xyz for clusters",100,-200,200,100,-200,200,100,-200,200);
      //      XYZbad = new TProfile2D("XYZbad","xyz for clusters with mismatched sectors",
      //			100,-200,200,100,-200,200,100,-200,200);
      Z   = new TProfile2D("Z","log(dEdx/Pion) versus sector(inner <= 24 and outter > 24)  and Z",
		     2*NoSector,1., 2*NoSector+1,100,0.,200.);
      ZC  = new TProfile2D("ZC","dEdxN versus sector(inner <= 24 and outter > 24)  and Z",
		     2*NoSector,1., 2*NoSector+1,100,0.,200.);
      ETA   = new TProfile2D("ETA","log(dEdx/Pion) versus sector(inner <= 24 and outter > 24)  and #{eta}",
		       2*NoSector,1., 2*NoSector+1, 125,-.25,2.25);
      SR = new TH2F("SR","sector and row from points (input)",
		       NoSector,1., NoSector+1, NoRow,1., NoRow+1);
      SecRow = new TProfile2D("SecRow","log(dEdx/Pion) versus sector  and row",
		       NoSector,1., NoSector+1, NoRow,1., NoRow+1);
      MulRow = new TProfile2D("MulRow","log(dEdx/Pion) versus Multiplicity of global tracks and row",
		       36,0.,6000., NoRow,1.,NoRow+1);
      ZRow = new TProfile2D("ZRow","log(dEdx/Pion) versus Z and row",
		       80,-200.,200., NoRow,1., NoRow+1);
      Ipad = new TProfile2D("Ipad","log(dEdx/Pion) versus charge collected on given pad and row",
		       200,0,200e-6, NoRow,1.,NoRow+1);
      I3pad = new TProfile2D("I3pad","log(dEdx/Pion) versus charge collected +/- pad and row",
		       200,0,400e-6, NoRow,1.,NoRow+1);
      II3padI = new TProfile2D("II3padI","log(dEdx/Pion) versus charge collected Ipad vs I3pad-Ipad Inner",
		       200,0,200e-6, 200, 0, 400e-6);
      II3padO = new TProfile2D("II3padO","log(dEdx/Pion) versus charge collected Ipad vs I3pad-Ipad Outer",
		       200,0,200e-6, 200, 0, 400e-6);
      Irow = new TProfile2D("Irow","log(dEdx/Pion) versus charge collected on given row and row",
		       200,0,1e-3, NoRow,1., NoRow+1);
      Trow = new TProfile2D("Trow","log(dEdx/Pion) versus charge collected on given row at given time and row",
		       200,0,200e-6, NoRow,1., NoRow+1);
      T3row = new TProfile2D("T3row","log(dEdx/Pion) versus charge collected on given row at given time and row",
		       200,0,200e-6, NoRow,1., NoRow+1);
      T5row = new TProfile2D("T5row","log(dEdx/Pion) versus charge collected on given row at given time and row",
		       200,0,200e-6, NoRow,1., NoRow+1);
      dYrow = new TProfile2D("dYrow","log(dEdx/Pion) versus cluseer projection on the wire for given row",
		       100,-2.5,2.5, NoRow,1., NoRow+1);
      corrI   = new TH2F("corrI","Correlation for Inner Sector for pair of nearest rows",
			100,-10.,10., 100,-10.,10.);
      corrO   = new TH2F("corrO","Correlation for Outer Sector for pair of nearest rows",
			100,-10.,10., 100,-10.,10.);
      corrI2   = new TH2F("corrI2","Correlation for Inner Sector for pair rows & row + 2",
			100,-10.,10., 100,-10.,10.);
      corrO2   = new TH2F("corrO2","Correlation for Outer Sector for pair rows & row + 2",
			100,-10.,10., 100,-10.,10.);
      corrI5   = new TH2F("corrI5","Correlation for Inner Sector for pair rows & row + 5",
			100,-10.,10., 100,-10.,10.);
      corrO5   = new TH2F("corrO5","Correlation for Outer Sector for pair rows & row + 5",
			100,-10.,10., 100,-10.,10.);
      corrIw   = new TH2F("corrIw","Weighted correlation for Inner Sector for pair of nearest rows",
			100,-10.,10., 100,-10.,10.);
      corrOw   = new TH2F("corrOw","Weighted correlation for Outer Sector for pair of nearest rows",
			100,-10.,10., 100,-10.,10.);
      corrI1w   = new TH1F("corrI1w","Weighted distribution for Inner Sector",100,-10.,10.);
      corrO1w   = new TH1F("corrO1w","Weighted distribution for Outer Sector",100,-10.,10.);
      corrI2w   = new TH2F("corrI2w","Weighted correlation for Inner Sector for pair rows & row + 2",
			100,-10.,10., 100,-10.,10.);
      corrO2w   = new TH2F("corrO2w","Weighted correlation for Outer Sector for pair rows & row + 2",
			100,-10.,10., 100,-10.,10.);
      corrI5w   = new TH2F("corrI5w","Weighted correlation for Inner Sector for pair rows & row + 5",
			100,-10.,10., 100,-10.,10.);
      corrO5w   = new TH2F("corrO5w","Weighted correlation for Outer Sector for pair rows & row + 5",
			100,-10.,10., 100,-10.,10.);
      Points70 = new TH2F("Points70","dEdx(I70) versus no. of measured points",50,0,50.,200,-1.,1.);
      Points60 = new TH2F("Points60","dEdx(I60) versus no. of measured points",50,0,50.,200,-1.,1.);
      Points70B = new TH2F("Points70B","dEdx(I70) versus no. of measured points BB",50,0,50.,200,-1.,1.);
      Points60B = new TH2F("Points60B","dEdx(I60) versus no. of measured points BB",50,0,50.,200,-1.,1.);
      PointsFit  = new TH2F("PointsFit","dEdx(fit) versus no. of measured points",
			    50,0,50., 200,-1.,1.);
      TPoints  = new TH2F("TPoints","dEdx(fit) versus length", 
			  150,10.,160., 200,-1.,1.);
      TPoints60= new TH2F("TPoints60","dEdx(fit) versus length", 
			  150,10.,160., 200,-1.,1.);
      TPoints70= new TH2F("TPoints70","dEdx(fit) versus length", 
			  150,10.,160., 200,-1.,1.);
      FShapeI  = new TH2F("FShapeI","(log(dEdx)-<z_{fit}>)*(dx)**0.36 versus log(dx) for Inner Sector", 
			  26,0.1, 1.4, 800,-15.,25.);
      FShapeO  = new TH2F("FShapeO","(log(dEdx)-<z_{fit}>)*(dx)**0.36 versus log(dx) for Outer Sector", 
			  36,0.6, 2.4, 800,-15.,25.);
      for (int hyp=0; hyp<NHYPS;hyp++) {
	for (int sCharge = 0; sCharge < 2; sCharge++) {
	  TString nameP = Names[hyp];
	  if (sCharge == 0) nameP += "P";
	  else        nameP += "N";
	  TString name = nameP;
	  name += "70";
	  TString title = "log(dE/dx70/I(";
	  title += Names[hyp];
	  title += ")) versus log10(p/m)";
	  hist70[hyp][sCharge] = new TH2D(name.Data(),title.Data(),100,-1.,4.,200,-1.,1.);
	  name = nameP;
	  name += "60";
	  title = "log(dE/dx60/I(";
	  title += nameP;
	  title += ")) versus log10(p/m)";
	  hist60[hyp][sCharge] = new TH2D(name.Data(),title.Data(),100,-1.,4.,200,-1.,1.);
	  name = nameP;
	  name += "z";
	  title = "zFit - log(I(";
	  title += nameP;
	  title += ")) versus log10(p/m)";
	  histz[hyp][sCharge] = new TH2D(name.Data(),title.Data(),100,-1.,4.,200,-1.,1.);
	  name = nameP;
	  name += "B";
	  title = "log(I_{Sirrf}(";
	  title += nameP;
	  title += ")) versus log10(p/m)";
	  histB[hyp][sCharge] = new TProfile(name.Data(),title.Data(),100,-1.,4.);
	  name += "B";
	  title = "log(I_{BB}(";
	  title += nameP;
	  title += ")) versus log10(p/m)";
	  histBB[hyp][sCharge] = new TProfile(name.Data(),title.Data(),100,-1.,4.);
	}
      }
      Time   = new TH2F("Time","zFit - log(I(pi)) versus Date& Time", 280,0.,70., 200,-1.,1.);
      FitPull= new TH2F("FitPull","(zFit - log(I(pi)))/dzFit  versus track length", 
			150,10.,160, 200,-5.,5.);
    }
  }
  gMessMgr->SetLimit("StdEdxMaker:: mismatched Sector",20);
  gMessMgr->SetLimit("StdEdxMaker:: pad/TimeBucket out of range:",20);
  gMessMgr->SetLimit("StdEdxMaker:: Helix Pediction",20);
  gMessMgr->SetLimit("StdEdxMaker:: Coordinates",20);
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StdEdxMaker::InitRun(Int_t RunNumber){
// 		TPG parameters
      // Normal to Sector
  StTpcCoordinateTransform transform(gStTpcDb);
  Double_t alpha = 2.*TMath::Pi()/NoSector;
  for (Int_t sector=1; sector<=NoSector; sector++) {
    Double_t beta = (sector > 12) ? 2*(NoSector-sector)*alpha: 2*sector*alpha;
    mNormal[sector-1] = new StThreeVectorD(sin(beta), cos(beta), 0.);
    for (Int_t row = 1; row <= NoRow; row++) {
      StTpcPadCoordinate padCoord(sector, row, 1, 1);
      StTpcLocalSectorCoordinate lsMidCoord; transform(padCoord, lsMidCoord);
      StTpcLocalSectorCoordinate  lsPos(0,lsMidCoord.position().y(),0, sector);
      StGlobalCoordinate          gMidCoord; transform(lsPos, gMidCoord);
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
  if (m_Mode >= 0) {
    // calibration constants
    if (!m_tpcTime) {
      TDataSet *tpc_calib  = GetDataBase("Calibrations/tpc"); assert(tpc_calib);
      m_tpcTime = (St_TpcTimeGain *) tpc_calib->Find("TpcTimeGain"); 
      TDataSet *tpc_daq  = GetDataBase("tpc/daq"); assert(tpc_daq);
      m_fee_vs_pad_row = (St_fee_vs_pad_row *) tpc_daq->Find("fee_vs_pad_row");
      if (!m_tpcTime) {
	cout << "TpcTimeGain is missing <=========== switch off time dependent calibration" << endl;
	assert(m_tpcTime); 
      }
      m_drift = (St_TpcDriftDistCorr *) tpc_calib->Find("TpcDriftDistCorr"); 
      if (!m_drift) {
	cout << "TpcDriftDistCorr is missing <=========== "
	     << "switch off dirft dependent calibration" << endl;
	assert(m_drift); 
      }
      m_badpads = (St_tpcBadPad *) tpc_calib->Find("BadPad");
      if (!m_badpads) cout << "=== List of bad pads is missing ===" << endl;
      m_TpcSecRow = (St_TpcSecRowCor *) tpc_calib->Find("TpcSecRow"); assert(m_TpcSecRow); 
    }
  }
  return kStOK;
}
//_____________________________________________________________________________
Int_t StdEdxMaker::FinishRun(Int_t OldRunNumber) {
  // Clean up
  Int_t NoSector = 24;
  Int_t NoRow    = 45;
  for (Int_t sector=1; sector<=NoSector; sector++) {
    SafeDelete(mNormal[sector-1]);
    for (Int_t row = 1; row <= NoRow; row++) {
      for (int i = 0; i<3; i++) SafeDelete(mRowPosition[sector-1][row-1][i]);
    }
  }
  SafeDelete(m_TpcSecRow);
  SafeDelete(m_fee_vs_pad_row); 
  SafeDelete(m_tpcTime); 
  SafeDelete(m_drift); 
  SafeDelete(m_badpads); 
  return StMaker::FinishRun(OldRunNumber);
}
//_____________________________________________________________________________
Int_t StdEdxMaker::Finish() {
  FinishRun(0);
  return StMaker::Finish();
}
//_____________________________________________________________________________
Int_t StdEdxMaker::Make(){ 
// Get the magnetic field direction to build the helix properly:
  St_dst_event_summary *summary = (St_dst_event_summary *)GetDataSet("dst/event_summary");
  float bField = -1;
  if (summary) { 
      assert(summary->GetTable());
      bField = summary->GetTable()->field;
  }

  Double_t Scale2keV = 560./335.;
  Double_t Scale70   = 1.2684;//1.2876; //1.2315;
  Double_t Scale60   = 1.3506;//1.3712; //1.3146;
  StTpcCoordinateTransform transform(gStTpcDb);
  St_DataSet *Dst = GetDataSet("dst"); assert(Dst);
  St_DataSet *dst = Dst->Find(".data/dst");
  if (!dst) dst = Dst;
  St_dst_dedx *dst_dedx     = (St_dst_dedx *) dst->Find("dst_dedx");
  SafeDelete(dst_dedx);
  St_dst_track     *globtrk     = (St_dst_track *)  dst->Find("globtrk");
  if (! globtrk) { 
    gMessMgr->Error() << "StdEdxMaker::Make(): global track table is not found " << endm; 
    return kStOK;
  }
  Double_t GMult = globtrk->GetNRows();
  dst_dedx = new St_dst_dedx("dst_dedx",20000); dst->Add(dst_dedx);
  St_dst_track     *primtrk     = (St_dst_track *)  dst->Find("primtrk");
  St_stk_track     *stk_track   = (St_stk_track *) GetDataSet("stk_track");
  if (stk_track) {//Silicon if any
    Int_t NoSvtTracks = stk_track->GetNRows();
    stk_track_st *stktrack = stk_track->GetTable();
    dst_dedx_st dedx;
    for (Int_t i=0; i<NoSvtTracks; i++, stktrack) {
      dedx.id_track  =  stktrack->id_globtrk;
      dedx.det_id    =  kSvtId;    // SVT track
      dedx.method    =  0;
      dedx.ndedx     =  0;
      dedx.dedx[0]   =  stktrack->dedx[0];
      dedx.dedx[1]   =  stktrack->dedx[1];
      dst_dedx->AddAt(&dedx);
    }
  }
  St_dst_point     *point      = (St_dst_point *)  dst->Find("point");
  if (! point) {
    gMessMgr->Error() << "StdEdxMaker::Make(): dst points is not found " 
		      << endm; return kStOK;
  }
  // Corrections 
  StDstPointChair  *pointC     = new StDstPointChair(point);
  NPoints = point->GetNRows();
  St_dEdxPoint *dedx_point = new St_dEdxPoint("dEdxPoint",NPoints);
  AddGarb(dedx_point);
  dEdxPoint dEdxp;
  Int_t ipoint;
  for (ipoint = 0; ipoint< NPoints; ipoint++) {
    if (pointC->DetectorId(ipoint) != kTpcId) continue;
//     // remove mismatched 
//     if (pointC->Sector(ipoint) <=12 && pointC->GetZ(ipoint) < 0 ||
// 	pointC->Sector(ipoint) > 12 && pointC->GetZ(ipoint) > 0) continue;
    memset (&dEdxp, 0, sizeof(dEdxPoint));
    dEdxp.id_track = pointC->TrackId(ipoint);
    dEdxp.FitFlag  = pointC->GetFitFlag(ipoint);
    dEdxp.iflag    = pointC->GetFlag(ipoint);
    dEdxp.dE = pointC->GetCharge(ipoint);
    dEdxp.sector   = pointC->Sector(ipoint);
    dEdxp.row      = pointC->PadRow(ipoint);
    dEdxp.xyz[0]   = pointC->GetX(ipoint);
    dEdxp.xyz[1]   = pointC->GetY(ipoint);
    dEdxp.xyz[2]   = pointC->GetZ(ipoint);
    StGlobalCoordinate global(dEdxp.xyz[0],dEdxp.xyz[1],dEdxp.xyz[2]);
    StTpcPadCoordinate Pad;      transform(global,Pad);
    //    if (XYZ) XYZ->Fill(dEdxp.xyz[0],dEdxp.xyz[1],dEdxp.xyz[2]);
    dEdxp.pad = Pad.pad();
    dEdxp.timeBucket = Pad.timeBucket() + 10;
    if (dEdxp.sector != Pad.sector() ||
	dEdxp.row    != Pad.row()) {
      //      if (XYZbad) XYZbad->Fill(dEdxp.xyz[0],dEdxp.xyz[1],dEdxp.xyz[2]);
      gMessMgr->Warning() << "StdEdxMaker:: mismatched Sector " 
			  << Pad.sector() << " / " << dEdxp.sector
			  << " Row " << Pad.row() << " / " << dEdxp.row 
			  << "pad " << dEdxp.pad << " TimeBucket :" << dEdxp.timeBucket 
			  << " x: " << dEdxp.xyz[0] << " y: "
			  << dEdxp.xyz[1] << " z: " << dEdxp.xyz[2] 
			  << endm;
      continue;
    }
    if (dEdxp.pad    < 1             ||
	dEdxp.pad    >= NPads        ||
	dEdxp.timeBucket < 0         ||
	dEdxp.timeBucket >= NTimeBins) {
      gMessMgr->Warning() << "StdEdxMaker:: pad/TimeBucket out of range: " 
			  <<  dEdxp.pad << " / " << dEdxp.timeBucket << endm;
      continue;
    }
    dEdxp.sortId = dEdxp.timeBucket + 1000*(dEdxp.pad + 1000*(dEdxp.row + 100*dEdxp.sector));
    dedx_point->AddAt(&dEdxp);
  }
  TString sortId("sortId");
  MyTableSorter     *dedx_pointS     = new MyTableSorter(dedx_point,sortId);
  dEdxPoint *dEdxP = dedx_point->GetTable();
  Double_t PadTime[NPads][NTimeBins];
  NPoints = dedx_pointS->GetNRows();
  dEdxPoint **fdEdxP = (dEdxPoint **) dedx_pointS->GetSortIndex();
  Double_t RowTime[NTimeBins];
  UInt_t idPT = 0;
  Int_t padOrdered[NPads];
  Int_t timeList[NTimeBins], timeOrdered[NTimeBins];
  Int_t nTimeBinT, nTimeBins;
  Int_t nPadBins;
  Int_t index[NTimeBins];
  for (int k=0; k < NPoints;k++) {
    if (fdEdxP[k]->sortId/1000000 !=idPT) {// get integral of charge
      idPT = fdEdxP[k]->sortId/1000000;
      int k1;
      nTimeBinT = nTimeBins = nPadBins = 0;
      for (k1=k; k1 < NPoints && fdEdxP[k1]->sortId/1000000 == idPT; k1++) {
	padOrdered[nPadBins] = fdEdxP[k1]->pad;
	if (nPadBins == 0 || padOrdered[nPadBins] != padOrdered[nPadBins-1]) nPadBins++;
	timeList[nTimeBinT] = fdEdxP[k1]->timeBucket;
	nTimeBinT++;
      }
      TMath::Sort(nTimeBinT,timeList, index, kFALSE); 
      for (int i=0; i<nTimeBinT; i++) {
	timeOrdered[nTimeBins] = timeList[index[i]];
	if (nTimeBins == 0 || timeOrdered[nTimeBins] != timeOrdered[nTimeBins-1]) nTimeBins++;
      }
      Int_t k2 = k1;
      memset (PadTime, 0, NPads*NTimeBins*sizeof(Double_t));
      for (k1 = k; k1 < k2; k1++) {
	Int_t padI = TMath::BinarySearch(nPadBins,padOrdered,fdEdxP[k1]->pad);
	Int_t timeI = TMath::BinarySearch(nTimeBins,timeOrdered,fdEdxP[k1]->timeBucket);
	PadTime[padI][timeI] += fdEdxP[k1]->dE;
      }
      memset (RowTime, 0, NTimeBins*sizeof(Double_t));
      for (int pad=0; pad<nPadBins; pad++) {
	for (int time=0; time<nTimeBins; time++) {
	  RowTime[time]    += PadTime[pad][time];
          if (time>0) {
	    PadTime[pad][time] += PadTime[pad][time-1];
	    PadTime[nPadBins][time] += PadTime[pad][time-1];
	  }
	}
      }
      for (k1 = k; k1 < k2; k1++) {
	Int_t padI  = TMath::BinarySearch(nPadBins,padOrdered,fdEdxP[k1]->pad);
	Int_t timeI = TMath::BinarySearch(nTimeBins,timeOrdered,fdEdxP[k1]->timeBucket);
	if (timeI > 0) {
	  fdEdxP[k1]->dEIpad  = PadTime[padI][timeI-1];
	  fdEdxP[k1]->dEI3pad = fdEdxP[k1]->dEIpad;
	  if (padI > 0 && padOrdered[padI] - padOrdered[padI-1] == 1) 
	    fdEdxP[k1]->dEI3pad += 	PadTime[padI-1][timeI-1];
	  if (padI < nPadBins - 1 && padOrdered[padI+1] - padOrdered[padI] == 1) 
	    fdEdxP[k1]->dEI3pad += 	PadTime[padI+1][timeI-1];
	  fdEdxP[k1]->dEIrow  = PadTime[nPadBins][timeI-1];
	}
	fdEdxP[k1]->dETrow  = RowTime[timeI] - fdEdxP[k1]->dE;
	Int_t t=timeI;
	fdEdxP[k1]->dET3row = -fdEdxP[k1]->dE + RowTime[t] + RowTime[t+1] + RowTime[t+2];
	fdEdxP[k1]->dET5row =  fdEdxP[k1]->dET3row + RowTime[t+3] +  RowTime[t+4];
      }
      k = k2 - 1;
    }
  }  
  SafeDelete(dedx_pointS);

  TString          id_track("id_track");
  TString          id("id");
  TTableSorter     *pointS     = new TTableSorter(dedx_point,id_track);
  TTableSorter     *globtrkS   = new TTableSorter(globtrk,id);
  StTrackChair     *globtrkC   = new StTrackChair(globtrk);
  
  TTableSorter     *primtrkS   = 0;
  StTrackChair     *primtrkC   = 0;
  if (primtrk){
		    primtrkS   = new TTableSorter(primtrk,id);
                    primtrkC   = new StTrackChair(primtrk);
  }
  Double_t TimeScale = 1;
  if (m_tpcTime) {TimeScale = (*m_tpcTime)[0].ScaleFactor;}
  StHelixD         *helix      = 0;
  Int_t iprim, jprim, iglob; 
  Int_t nPrimaryTracks = 0;
  Double_t pTinv = 0;
  Double_t p = 0;
  Double_t Eta = 0;
  Int_t NoFitPoints = 0;
  dst_track_st *primTrk = 0, *globTrk = 0, *primTRK = 0;
  if (primtrk) {
    primTrk = primtrk->GetTable();
    Int_t N = primtrk->GetNRows();
    for (iprim = 0; iprim < N; iprim++, primTrk++) 
      if (primTrk->iflag > 0) nPrimaryTracks++;
  }
  Int_t kglob = 0, kprim = 0, kpoint = 0; 
  Int_t N =  globtrkS->GetNRows();
  Int_t NP = 0; if (primtrkS) NP = primtrkS->GetNRows();
  for (; kglob < N;kglob++) {
    iglob = globtrkS->GetIndex(kglob); assert (iglob >= 0);
    globTrk = globtrk->GetTable() + iglob;
    primTrk = 0;
    iprim = -1;
    if (globTrk->iflag < 0) continue;
    Int_t Id = globTrk->id;
    if (primtrkS && kprim < NP) {
      for (; kprim < NP; kprim++) {
	jprim = primtrkS->GetIndex(kprim); assert(jprim >=0);
	primTRK = primtrk->GetTable() + jprim;
	if (primTRK->iflag < 0) continue;
	if (primTRK->id < Id) continue;
	if (primTRK->id > Id) break;
	iprim = jprim;
	primTrk = primTRK;
	helix  = primtrkC->MakeHelix(iprim,bField);
	NoFitPoints = primTrk->n_fit_point;
	kprim++;
	if (m_Mode > 0) {
	  Double_t tanl  = primTrk->tanl;
	  pTinv  = primtrkC->Invpt(iprim);
	  p = 1.e6;
	  if (pTinv > 1.e-6) p = 1./pTinv*TMath::Sqrt(1. + tanl*tanl);
	  Double_t Theta = TMath::Pi()/2 - TMath::ATan(tanl);
	  Eta = - TMath::Log(TMath::Tan(Theta/2.));
	}
	break;
      }
    }
    if (!helix) {
      helix = globtrkC->MakeHelix(iglob,bField);
      NoFitPoints = globTrk->n_fit_point;
      if (m_Mode > 0) {
	Double_t tanl  = globTrk->tanl;
	pTinv  = globtrkC->Invpt(iglob);
	p = 1.e6;
	if (pTinv > 1.e-6) p = 1./pTinv*TMath::Sqrt(1. + tanl*tanl);
	Double_t Theta = TMath::Pi()/2 - TMath::ATan(tanl);
	Eta = - TMath::Log(TMath::Tan(Theta/2.));
      }
    }
    dst_dedx_st dedx;
    NPoints = 0;
    Int_t NFitPoints=0;
    NdEdx = 0;
    Double_t TrackLength60 = 0, TrackLength70 = 0, TrackLength = 0;
    Double_t avrgZ = 0;
    Int_t NoPoints =  pointS->GetNRows();
    for (;kpoint < NoPoints;kpoint++) {
      ipoint = pointS->GetIndex(kpoint); assert (ipoint >= 0);
//       if (pointC->TrackId(ipoint) < Id) continue;
//       if (pointC->TrackId(ipoint) > Id) break;
      if (dEdxP[ipoint].id_track < Id) continue;
      if (dEdxP[ipoint].id_track > Id) break;
      NPoints++;
      if (!dEdxP[ipoint].FitFlag) continue;
      NFitPoints++;
      if (dEdxP[ipoint].iflag) continue;
      Int_t sector = dEdxP[ipoint].sector;
      Int_t row    = dEdxP[ipoint].row;
      //      if (row == 13) continue; // Skip row 13
      StThreeVectorD &normal = *mNormal[sector-1];
      const StThreeVectorD  &gMidPos = *mRowPosition[sector-1][row-1][0];
      // check that helix prediction is consistent with measurement
      Double_t s = helix->pathLength(gMidPos, normal);
      StThreeVectorD xyzOnPlane = helix->at(s);
      StGlobalCoordinate globalOnPlane(xyzOnPlane.x(),xyzOnPlane.y(),xyzOnPlane.z());
      StTpcPadCoordinate PadOnPlane;      transform(globalOnPlane,PadOnPlane);
      if (sector != PadOnPlane.sector() || 
	  row != PadOnPlane.row() ||
	  TMath::Abs(dEdxP[ipoint].pad-PadOnPlane.pad()) > 5) {
	if (iprim >= 0  && Debug() > 1) {
	  gMessMgr->Warning() << "StdEdxMaker::	Helix Pediction " 
			      << "Sector = " 
			      << PadOnPlane.sector() << "/" 
			      << dEdxP[ipoint].sector << "/" << sector 
			      << " Row = " << PadOnPlane.row() << "/" 
			      << dEdxP[ipoint].row << "/" << row 
			      << " Pad = " << PadOnPlane.pad() << "/" 
			      << dEdxP[ipoint].pad 
			      << " from Helix  is not matched with point/" << endm;;
	  gMessMgr->Warning() << "StdEdxMaker:: Coordinates " << 
	    " x: " << xyzOnPlane.x() << "/" << dEdxP[ipoint].xyz[0] <<
	    " y: " << xyzOnPlane.y() << "/" << dEdxP[ipoint].xyz[1] <<
	    " z: " << xyzOnPlane.z() << "/" << dEdxP[ipoint].xyz[2] << endm;
	}
	continue;
      }
      const StThreeVectorD  &gTopPos = *mRowPosition[sector-1][row-1][1];
      const StThreeVectorD  &gBotPos = *mRowPosition[sector-1][row-1][2];
      double s_out = helix->pathLength(gTopPos, normal);
      double s_in  = helix->pathLength(gBotPos, normal);
      dx = TMath::Abs(s_out-s_in);
      if (dx < 0.5 || dx > 25.) continue;
      Double_t dY = 
	normal.x()*(helix->y(s_in) - helix->y(s_out))  - 
	normal.y()*(helix->x(s_in) - helix->x(s_out));
      dE = dEdxP[ipoint].dE*Scale2keV;
      // Corrections
      dE *= TimeScale;
      Int_t pad = dEdxP[ipoint].pad;
      // Volume charge
      ESector lsc = kTpcInner;
      if (row > 13) lsc = kTpcOuter;
      double gc = 0;
      Double_t RMS = 1;
      dEP = dE;
      CdEdx[NdEdx].SigmaFee = 1;
      if (!m_Simulation) {
	Double_t dEpad = dEdxP[ipoint].dEIpad;
	RMS = VolChargeRMS[lsc][0] + dEpad*(VolChargeRMS[lsc][1] + dEpad*VolChargeRMS[lsc][2]);
	gc = VolChargePars[lsc][0] + dEpad*(VolChargePars[lsc][1] + dEpad*VolChargePars[lsc][2]);
	gc += VolChargePar2[lsc][0] + VolChargePar2[lsc][1]*TMath::TanH(VolChargePar2[lsc][2]*dEpad);
	Double_t dETrow = dEdxP[ipoint].dETrow;
	gc += TimeBucketPars[lsc][0] + dETrow*(TimeBucketPars[lsc][1] + dETrow*TimeBucketPars[lsc][2]);
	gc += II3padC(dEdxP[ipoint].dEIpad,dEdxP[ipoint].dEI3pad-dEdxP[ipoint].dEIpad,lsc);
	dE *= TMath::Exp(-gc);
	gc = 1;
	if (m_TpcSecRow) {
	  TpcSecRowCor_st *gain = m_TpcSecRow->GetTable() + sector - 1;
	  gc =  gain->GainScale[row-1];
	  CdEdx[NdEdx].SigmaFee = gain->GainRms[row-1];
	}
	if (gc < 0.0) continue;
	dE *= gc;
	Int_t SectN = sector; // drift distance
	if (row < 14) SectN += 24;
	Double_t z = dEdxP[ipoint].xyz[2];
	if (sector > 12) z = - z;
	if (m_drift) {
	  TpcDriftDistCorr_st *cor = m_drift->GetTable();
	  if (row <= 13) cor++;
	  Double_t DriftCorr = 8.26477e-02 + 
	    cor->a[0]+z*(cor->a[1]+z*cor->a[2]);
	  dE *= TMath::Exp(-DriftCorr);
	}
      }
      TrackLength         += dx;
      CdEdx[NdEdx].sector  = sector;
      CdEdx[NdEdx].row     = row;
      CdEdx[NdEdx].pad     = pad;
      CdEdx[NdEdx].dx      = dx;
      CdEdx[NdEdx].dEU     = dEdxP[ipoint].dE;
      CdEdx[NdEdx].dE      = dE; // corrected
      CdEdx[NdEdx].dEdxU   = CdEdx[NdEdx].dEU/CdEdx[NdEdx].dx;
      CdEdx[NdEdx].dEdx    = CdEdx[NdEdx].dE/CdEdx[NdEdx].dx;
      CdEdx[NdEdx].dEdxP   = dEP/CdEdx[NdEdx].dx;
      CdEdx[NdEdx].dEdxLU  = TMath::Log(CdEdx[NdEdx].dEdxU);
      CdEdx[NdEdx].dEdxL   = TMath::Log(CdEdx[NdEdx].dEdx);
      CdEdx[NdEdx].xyz[0]  = dEdxP[ipoint].xyz[0];
      CdEdx[NdEdx].xyz[1]  = dEdxP[ipoint].xyz[1];
      CdEdx[NdEdx].xyz[2]  = dEdxP[ipoint].xyz[2];
      CdEdx[NdEdx].dEIpad  = dEdxP[ipoint].dEIpad;
      CdEdx[NdEdx].dEI3pad = dEdxP[ipoint].dEI3pad;
      CdEdx[NdEdx].dEIrow  = dEdxP[ipoint].dEIrow;
      CdEdx[NdEdx].dETrow  = dEdxP[ipoint].dETrow;
      CdEdx[NdEdx].dET3row = dEdxP[ipoint].dET3row;
      CdEdx[NdEdx].dET5row = dEdxP[ipoint].dET5row;
      CdEdx[NdEdx].Fee     = -1;
      CdEdx[NdEdx].dY      = dY;
      CdEdx[NdEdx].RMS      = RMS;
      avrgZ += CdEdx[NdEdx].dEdxL;
      NdEdx++;
    }
    if (!NdEdx) continue;
    avrgZ /=NdEdx;
    SortdEdx(dEdxS,NdEdx);
    Double_t I70 = 0, I60 = 0, D70 = 0, D60 = 0;
    Int_t N70 = NdEdx - (int) (0.3*NdEdx + 0.5); 
    Int_t N60 = NdEdx - (int) (0.4*NdEdx + 0.5);
    Int_t k;
    for (k = 0; k < N70; k++) {
      I70 += dEdxS[k].dEdx;
      D70 += dEdxS[k].dEdx*dEdxS[k].dEdx;
      TrackLength70 += dEdxS[k].dx;
      if (k < N60) {
	I60 += dEdxS[k].dEdx;
	D60 += dEdxS[k].dEdx*dEdxS[k].dEdx;
	TrackLength60 += dEdxS[k].dx;
      }
    }
    if (N70 > 0) {
      I70 /= N70; D70 /= N70;
      D70  = sqrt(D70 - I70*I70);
      I70 *= Scale70;
      D70 *= Scale70;
      dedx.id_track  =  Id;
      dedx.det_id    =  kTpcId;    // TPC track 
      dedx.method    =  kTruncatedMeanIdentifier;
      dedx.ndedx     =  N70 + 100*((int) TrackLength);
      dedx.dedx[0]   =  I70;
      dedx.dedx[1]   =  0.708437*pow(TrackLength,-0.484222);// range = [25,140] cm
      dst_dedx->AddAt(&dedx);
    }
    if (N60 > 0) {
      I60 /= N60; D60 /= N60;
      D60  = sqrt(D60 - I60*I60);
      I60 *= Scale60;
      D60 *= Scale60;
      dedx.id_track  =  Id;
      dedx.det_id    =  kTpcId;    // TPC track 
      dedx.method    =  kOtherMethodIdentifier;
      dedx.ndedx     =  N60 + 100*((int) TrackLength);
      dedx.dedx[0]   =  I60;
      dedx.dedx[1]   =  0.736496*pow(TrackLength,-0.490062);// range = [25,140] cm
      dst_dedx->AddAt(&dedx);
    }
    Double_t chisq, fitZ, fitdZ;
    DoFitZ(chisq, fitZ, fitdZ);
    //    fitdZ *= 1.32920/(1.01405e+00 -8.12759e-04*TrackLength); // scale errors
    fitdZ *= (1.35095e+00 -1.10405e-03*TrackLength); // hist 110 scale errors
    if (chisq > 0 && chisq < 10000.0) {
      dedx.id_track  =  Id;
      dedx.det_id    =  kTpcId;    // TPC track 
      dedx.method    =  kLikelihoodFitIdentifier;
      dedx.ndedx     =  NdEdx + 100*((int) TrackLength);
      dedx.dedx[0]   =  TMath::Exp(fitZ);
      if (fitdZ >= 1.) fitdZ = 0.999;
      dedx.dedx[1]   =  fitdZ; // 0.660001*pow(TrackLength,-0.468743);// range = [25,140] cm
      dst_dedx->AddAt(&dedx);
    }
    if (primtrkC && iprim >= 0&& m_Mode > 0) {
      Double_t Pred[NHYPS], PredBB[NHYPS];
      Int_t sCharge = 0;
      if (primtrkC->Charge(iprim) < 0) sCharge = 1;
      for (int l = 0; l < NHYPS; l++) {
	Pred[l] = 1.e-6*BetheBloch::Sirrf(p/Masses[l],TrackLength,l==3); 
	//	Pred[l] = 1.e-6*BetheBloch::Sirrf(p/Masses[l],60.,l==3); 
	PredBB[l] = BB(p/Masses[l]);
      }
      for (k = 0; k < NdEdx; k++) {
	FdEdx[k].dEdxN = TMath::Log(FdEdx[k].dEdx/Pred[2]);
	FdEdx[k].dEdxNP = TMath::Log(FdEdx[k].dEdxP/Pred[2]);
	if (SR) SR->Fill(FdEdx[k].sector,FdEdx[k].row);
	if (chisq > 0 && chisq < 10000.0) {
	  Double_t zk  = FdEdx[k].zdev;
	  if (FdEdx[k].Prob > 1.e-12) {
	    if (FdEdx[k].row > 13) corrO1w->Fill(zk,1./FdEdx[k].Prob);
	    else                   corrI1w->Fill(zk,1./FdEdx[k].Prob);
	  }
	  for (int l = 0; l < NdEdx; l++){
	    if (k == l) continue;
	    Double_t zl  = FdEdx[l].zdev;
	    if (FdEdx[l].row%2 == 1 && FdEdx[l].row - FdEdx[k].row  == 1) {
	      if (FdEdx[k].row > 13) {
		corrO->Fill(zk,zl); 
		if (FdEdx[k].Prob*FdEdx[l].Prob > 1.e-12) 
		  corrOw->Fill(zk,zl,1./(FdEdx[k].Prob*FdEdx[l].Prob));
	      }
	      else {
		corrI->Fill(zk,zl); 
		if (FdEdx[k].Prob*FdEdx[l].Prob > 1.e-12) 
		  corrIw->Fill(zk,zl,1./(FdEdx[k].Prob*FdEdx[l].Prob));
	      }
	    }
	    if (FdEdx[l].row%2 == 1 && FdEdx[l].row - FdEdx[k].row  == 2) {
	      if (FdEdx[k].row > 13) {
		corrO2->Fill(zk,zl); 
		if (FdEdx[k].Prob*FdEdx[l].Prob > 1.e-12) 
		  corrO2w->Fill(zk,zl,1./(FdEdx[k].Prob*FdEdx[l].Prob));
	      }
	      else {
		corrI2->Fill(zk,zl); 
		if (FdEdx[k].Prob*FdEdx[l].Prob > 1.e-12) 
		  corrI2w->Fill(zk,zl,1./(FdEdx[k].Prob*FdEdx[l].Prob));
	      }
	    }
	    if (FdEdx[l].row%2 == 1 && FdEdx[l].row - FdEdx[k].row  == 5) {
	      if (FdEdx[k].row > 13) {
		corrO5->Fill(zk,zl); 
		if (FdEdx[k].Prob*FdEdx[l].Prob > 1.e-12) 
		  corrO5w->Fill(zk,zl,1./(FdEdx[k].Prob*FdEdx[l].Prob));
	      }
	      else {
		corrI5->Fill(zk,zl); 
		if (FdEdx[k].Prob*FdEdx[l].Prob > 1.e-12) 
		  corrI5w->Fill(zk,zl,1./(FdEdx[k].Prob*FdEdx[l].Prob));
	      }
	    }
	  }
	}
      //      if (sXY) sXY->Fill(FdEdx[k].x[0],FdEdx[k].x[0],FdEdx[k].sector);
	if (Z && NoFitPoints >= 30) {
	  Double_t eta =  Eta;
	  Int_t SectN = FdEdx[k].sector; // drift distance
	  Double_t z = FdEdx[k].xyz[2];
	  if (SectN > 12) z = -z;
	  if (FdEdx[k].row < 14) SectN += 24;
	  Z->Fill(SectN,z,FdEdx[k].dEdxN);
	  ZC->Fill(SectN,z,FdEdx[k].zdev);
	  if (FdEdx[k].sector > 12) eta = -eta;
	  if (ETA) ETA->Fill(SectN,eta,FdEdx[k].dEdxN);
	  if (SecRow) SecRow->Fill(FdEdx[k].sector+0.5,FdEdx[k].row+0.5,FdEdx[k].dEdxNP);
	  if (MulRow) MulRow->Fill(GMult,FdEdx[k].row+0.5,FdEdx[k].dEdxN);
	  if (ZRow) ZRow->Fill(FdEdx[k].xyz[2],FdEdx[k].row+0.5,FdEdx[k].dEdxN);
	  if (Ipad) Ipad->Fill(FdEdx[k].dEIpad,FdEdx[k].row+0.5,FdEdx[k].dEdxN);
	  if (I3pad) I3pad->Fill(FdEdx[k].dEI3pad,FdEdx[k].row+0.5,FdEdx[k].dEdxN);
	  if (II3padI && FdEdx[k].row <  14) II3padI->Fill(FdEdx[k].dEIpad,FdEdx[k].dEI3pad-FdEdx[k].dEIpad,FdEdx[k].dEdxN);
	  if (II3padO && FdEdx[k].row >= 14) II3padO->Fill(FdEdx[k].dEIpad,FdEdx[k].dEI3pad-FdEdx[k].dEIpad,FdEdx[k].dEdxN);
	  if (Trow) Trow->Fill(FdEdx[k].dETrow,FdEdx[k].row+0.5,FdEdx[k].dEdxN);
	  if (T3row) T3row->Fill(FdEdx[k].dET3row,FdEdx[k].row+0.5,FdEdx[k].dEdxN);
	  if (T5row) T5row->Fill(FdEdx[k].dET5row,FdEdx[k].row+0.5,FdEdx[k].dEdxN);
	  if (dYrow) dYrow->Fill(FdEdx[k].dY,FdEdx[k].row+0.5,FdEdx[k].dEdxN);
	}
      }
      Points70->Fill(N70,TMath::Log(I70/Pred[2]));
      Points60->Fill(N60,TMath::Log(I60/Pred[2]));
      Points70B->Fill(N70,TMath::Log(I70/PredBB[2]));
      Points60B->Fill(N60,TMath::Log(I60/PredBB[2]));
      PointsFit->Fill(NdEdx,fitZ-TMath::Log(Pred[2]));
      TPoints->Fill(TrackLength,fitZ-TMath::Log(Pred[2]));
      TPoints60->Fill(TrackLength,TMath::Log(I60/Pred[2]));
      TPoints70->Fill(TrackLength,TMath::Log(I70/Pred[2]));
      if (NdEdx > 30) {
	for (k = 0; k < NdEdx; k++) {
	  if (FdEdx[k].SigmaFee > 0) {
	    Double_t xscale = 1;
	    Double_t dxLog   = TMath::Log(FdEdx[k].dx);
	    Double_t dEdxLog = FdEdx[k].dEdxL;
	    xscale /= FdEdx[k].RMS;
	    Double_t Shift   = (dEdxLog-fitZ)*xscale;
	    ESector lsc = kTpcInner;
	    if (FdEdx[k].row > 13) lsc = kTpcOuter;
	    Shift -= ShapeMean[lsc][0]  + dxLog*(ShapeMean[lsc][1]  + dxLog*ShapeMean[lsc][2]);
	    Shift /= ShapeSigma[lsc][0] + dxLog*(ShapeSigma[lsc][1] + dxLog*ShapeSigma[lsc][2]);
	    Shift -= ShapeMean2[lsc][0] + dxLog*(ShapeMean2[lsc][1] + dxLog*ShapeMean2[lsc][2]);
	    Shift -= ShapeMean3[lsc][0] + dxLog*(ShapeMean3[lsc][1] + dxLog*ShapeMean3[lsc][2]);
	    if (FdEdx[k].row <= 13) {
	      FShapeI->Fill(dxLog,Shift);
	    }
	    else {
	      FShapeO->Fill(dxLog,Shift);
	    }
	  }
	}
	Int_t best = -1; Double_t valBest = 9999.;
	Int_t good = -1; Double_t valGood = 9999.;
	for (int hyp=0; hyp<NHYPS;hyp++) {
	  hist70[hyp][sCharge]->Fill(TMath::Log10(p/Masses[hyp]),TMath::Log(I70/Pred[hyp]));
	  hist60[hyp][sCharge]->Fill(TMath::Log10(p/Masses[hyp]),TMath::Log(I60/Pred[hyp]));
	  Double_t dev = 9999.;
	  if (fitdZ > 0.) dev = TMath::Abs((fitZ - TMath::Log(Pred[hyp]))/fitdZ);
	  if (dev < valBest) {
	    good = best; valGood = valBest;
	    best = hyp;  valBest = dev;
	  }
	  if (hyp != best && dev < valGood) {
	    good = hyp; valGood = dev;
	  }
	  histz[hyp][sCharge]->Fill(TMath::Log10(p/Masses[hyp]),fitZ - TMath::Log(Pred[hyp]));
	  histB[hyp][sCharge]->Fill(TMath::Log10(p/Masses[hyp]),TMath::Log(Pred[hyp]));
	  histBB[hyp][sCharge]->Fill(TMath::Log10(p/Masses[hyp]),TMath::Log(PredBB[hyp]));
	}
	Double_t date = MyDate(GetDate(),GetTime());
	Time->Fill(date,fitZ - TMath::Log(Pred[2]));
	FitPull->Fill(TrackLength,(fitZ - TMath::Log(Pred[2]))/fitdZ);
      }
     SafeDelete(helix);
    }
  }
  SafeDelete(pointS);
  SafeDelete(primtrkS);
  SafeDelete(globtrkS);
  SafeDelete(pointC);
  SafeDelete(primtrkC);
  SafeDelete(globtrkC);
  return kStOK;
}
//________________________________________________________________________________
void StdEdxMaker::SortdEdx(dEdx_t *dEdxS, Int_t NdEdx) {
  int i;
  for (i = 0; i < NdEdx; i++) dEdxS[i] = CdEdx[i];
  for (i = 0; i < NdEdx-1; i++) {
    for (int j = i+1; j < NdEdx; j++) {
      if (dEdxS[i].dEdx > dEdxS[j].dEdx) {
	dEdx_t temp = dEdxS[i];
	dEdxS[i] = dEdxS[j];
	dEdxS[j] = temp;
      }
    }
  }
}
//__________________________________________________
Double_t StdEdxMaker::MyDate(Int_t date,Int_t time) {// combine date in time in one number startig 07/01/2000
  Int_t md = date-20000000; // printf("md %i\n",md);
  Int_t d  = md%100;        // printf("d %i\n",d);
  Int_t m  = md/100;        // printf("m %i\n",m);
  Double_t dd = d;
  if (m > 7) dd += 30; // July
  if (m > 8) dd += 31; // August
  if (m > 9) dd += 31; // September;
  // printf("dd = %f\n",dd);
  Double_t S = time%100;               // printf("S %i\n",S);
  Double_t M = (time/100)%100;         // printf("M %i\n",M);
  Double_t H = time/10000;             // printf("H %i\n",H);
  Double_t Date = dd + (H + (M + S/60)/60)/24;             // printf("Date %f\n",Date);
  return Date;
}
//________________________________________________________________________________
void fGaus(Double_t x, Double_t *val, ESector l){
  val[0] = val[1] = 0;
  for (Int_t j=0; j<NpGaus; j+=3){
    //    if (parGaus[l][j+2] > 0) {
    Double_t dev = (x - parGaus[l][j+1])/parGaus[l][j+2];
    val[1] += parGaus[l][j]*exp(-0.5*dev*dev)*(-dev/parGaus[l][j+2]);
    val[0] += parGaus[l][j]*exp(-0.5*dev*dev);
    //    }
  }
}
//________________________________________________________________________________
void fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag)
{
  f = 0.;
  gin[0] = 0.;
  gin[1] = 0.;
  Double_t Val[2];
  for (int i=0;i<NdEdx; i++) {
    ESector l = kTpcInner;
    if (FdEdx[i].row > 13) l = kTpcOuter;
    FdEdx[i].xscale = 1;
    ESector lsc = kTpcInner;
    if (FdEdx[i].row > 13) lsc = kTpcOuter;
    Double_t dx =FdEdx[i].dx; 
    if (dx <  1.) dx = 1.;
    if (dx > 10.) dx = 10.;
    Double_t dxLog   = TMath::Log(dx);
    Double_t Scale   = ShapeSigma[lsc][0] + dxLog*(ShapeSigma[lsc][1] + dxLog*ShapeSigma[lsc][2]);
    FdEdx[i].xscale /= FdEdx[i].RMS;
    FdEdx[i].zdev    = (FdEdx[i].dEdxL-par[0])*FdEdx[i].xscale;
    FdEdx[i].zdev   -= ShapeMean[lsc][0] + dxLog*(ShapeMean[lsc][1] + dxLog*ShapeMean[lsc][2]);
    FdEdx[i].zdev   /= Scale;
    FdEdx[i].zdev   -= ShapeMean2[lsc][0] + dxLog*(ShapeMean2[lsc][1] + dxLog*ShapeMean2[lsc][2]);
    FdEdx[i].xscale /= Scale;
    fGaus(FdEdx[i].zdev,Val,l);
    FdEdx[i].Prob = Val[0];
    f -= TMath::Log( FdEdx[i].Prob );
    Double_t gg = Val[1];
    gin[0] +=  FdEdx[i].xscale*gg/FdEdx[i].Prob;
  }
}
//________________________________________________________________________________
void StdEdxMaker::DoFitZ(Double_t &chisq, Double_t &fitZ, Double_t &fitdZ)
{
  Double_t avz = 0;
  for (int i=0;i<NdEdx;i++) {
    FdEdx[i] = CdEdx[i];
    FdEdx[i].dEdxL = FdEdx[i].dEdxL;
    avz += FdEdx[i].dEdxL;
    for (int j=0;j<i;j++) {// order by rowsto account correlations
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
    gMinuit->SetFCN(fcn);
    //    gMinuit->SetPrintLevel(-1);
#if 1
    arglist[0] = -1;
    gMinuit->mnexcm("set print",arglist, 1, ierflg);
#endif
    gMinuit->mnexcm("set NOW",arglist, 0, ierflg);
    gMinuit->mnexcm("CLEAR",arglist, 0, ierflg);
    arglist[0] = 0.5;
    gMinuit->mnexcm("SET ERR", arglist ,1,ierflg);
    gMinuit->mnparm(0, "mean", avz, 0.01,0.,0.,ierflg); //First Guess
#if 1    
    arglist[0] = 1.;   // 1.
#else
    arglist[0] = 0.;   // Check gradient 
#endif
    gMinuit->mnexcm("SET GRAD",arglist,1,ierflg);
    arglist[0] = 500;
    arglist[1] = 1.;
    gMinuit->mnexcm("MIGRAD", arglist ,2,ierflg);
    gMinuit->mnexcm("HESSE  ",arglist,0,ierflg);
    Double_t edm,errdef;
    Int_t nvpar,nparx,icstat;
    gMinuit->mnstat(chisq,edm,errdef,nvpar,nparx,icstat);
    gMinuit->GetParameter(0, fitZ, fitdZ);
  }
  else {
    fitZ = fitdZ = chisq =  -999.;
  }
}
