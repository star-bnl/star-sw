// To build profile histograms: root.exe -q -b TpcT.C+
// To fit them                : root.exe -q *H.root FitTpcT.C
// To draw all of them        : root.exe */*Fit.root
//                              .L DrawList.C+
//                              DrawFAll()
// t0 Offset:
// TpcT->Draw("fRcHit.mMcl_t/64-3.0*0.055*Frequency-fMcHit.mMcl_t:fMcHit.mPosition.mX3>>T(210,-210,210,100,-2,3)","fNoMcHit==1&&fNoRcHit==1&&fRcHit.mQuality>90","colz")
// TpcT->Draw("fMcHit.mMcl_t+(0.165+fMcHit.mTof)*Frequency-fRcHit.mMcl_t/64:fMcHit.mPosition.mX3>>T(210,-210,210,100,-2,3)","fNoMcHit==1&&fNoRcHit==1&&fRcHit.mQuality>90","colz",1e7)
// pads :  root.exe -q -b PadsTpcT.C
//#define __iTpx__
#define PRINT
//#define runY2008
//#define __LASER__
//#define __Cosmics__
//#define __REAL_DATA__
//#define __useGainT0__
//#define __PADCorrection__
//#define __PRINCIPLE__
//#define __MUDIFI_EXT__
#if !defined(__CINT__) || defined(__MAKECINT__)
#include <assert.h>
#include "Riostream.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TProfile2D.h"
#include "TF1.h"
#include "TF2.h"
#include "TLegend.h"
#include "Riostream.h"
#include "TSystem.h"
#include "TMath.h"
#include "TGraphErrors.h"
#include "TDirIter.h"
#include "TTreeIter.h"
#include "TDirectory.h"
#include "TROOT.h"
#include "TChain.h"
#include "TFile.h"
#include "TVector3.h"
#include "TRMatrix.h"
#include "TNtuple.h"
//#include "MakePol.C"
#include "TPolynomial.h"
#include "THnSparse.h"
#include "TArrayI.h"
#include "TMultiDimFit.h"
#ifdef __PRINCIPLE__
#include "TPrincipal.h"
#endif
#include "StCloseFileOnTerminate.h"
#define __TEST__
#ifdef __TEST__
#include "TMDFParameters.h"
#endif
#endif
static Int_t NoInnerRows = -1; // Tpx
static Int_t NoOfRows    = -1;
TMultiDimFit* MDfit = 0;
TFile *fOut = 0;
//#include "StTpcMcAnalysisMaker/TpcCluster.h"
TF1  *mShaperResponse = 0;             //!
TF1  *mChargeFractionInner = 0;        //!
TF1  *mPadResponseFunctionInner = 0;   //!
TF1  *mChargeFractionOuter = 0;        //!
TF1  *mPadResponseFunctionOuter = 0;   //!
TF1  *mConvolution = 0;                //!
//static Double_t K3I = 1.5236; // fit data - res. 1.709;// data Full Field; 0.68;  // K3 from E.Mathieson, Fig. 5.3b (pads) for a/s = 2.5e-3 and h/s = 0.5
//static Double_t K3O = 1.0270; // -"-      1.044;//      -"-       ; 0.89;    // K3 from E.Mathieson, Fig. 5.3a (row) for a/s = 2.5e-3 and h/s = 0.5
static const Double_t K3IP = 0.68;    // K3 from E.Mathieson, Fig. 5.3b (pads) for a/s = 2.5e-3 and h/s = 0.5
static const Double_t K3OP = 0.55;    // K3 from E.Mathieson, Fig. 5.3b (pads) for a/s = 2.5e-3 and h/s = 1
static const Double_t K3IR = 0.89;    // K3 from E.Mathieson, Fig. 5.3a (row)  for a/s = 2.5e-3 and h/s = 0.5
static const Double_t K3OR = 0.61;    // K3 from E.Mathieson, Fig. 5.3a (row)  for a/s = 2.5e-3 and h/s = 1.0
static const Double_t tau     = 71e-9; // from pulser fit
//static const Double_t pShaper = 5.15;  //     -"-         ?
static const Double_t FWHM = 2.827;  //     FWHM = sqrt(p)*tau*(2*TMath::Sqrt(2*TMath::Log(2.)));
// inner  FWHM = 2.827; tau = 0.5545
// outer  FWHM = 3.038; tau = 0.5533
static const Double_t CrossTalkInner = 0; // 0.004;
static const Double_t CrossTalkOuter = 0; // 0.004;
static const Double_t innerSectorAnodeVoltage = 1170;
static const Double_t outerSectorAnodeVoltage = 1390;
static const Double_t anodeWireRadius = 1e-3;
static const Double_t anodeWirePitch  = 0.4;
static const Double_t innerSectorPadWidth = 0.285;
static const Double_t innerSectorPadPitch = 0.335;
static const Double_t innerSectorPadLength = 1.15;
static const Double_t outerSectorPadWidth = 0.620;
static const Double_t outerSectorPadPitch = 0.675;
static const Double_t outerSectorPadLength = 1.95;
static const Double_t tauIntegraton        =  74.6e-9; // secs
static const Double_t tauF                 = 394.0e-9; 
static const Double_t tauP                 = 775.0e-9;
// H(s) = (1 + s*tau_P)/(1 + s*tau_F) => (s + d)/(s - b);
/*1.6  (s + d) / ((s - a)*(s - b)) => A*exp(a*t) + B*exp(b*t) =
  (a+d)/(a-b)*exp(a*t) + (b+d)/(b-a)*exp(b*t) 
  d = 1/tau_F; a = - 1/tau_I; b = - 1/tau_P;
  06/27/09  
  Inner  Decay DAQ = 5.879 timebuckets
  SIM = 7.892 -"-
  extra decay       23.059 -"- ->  2.457e-6 sec
  Outer        DAQ = 7.310 -"-
  SIM = 7.843
  107.55  -"- -> 11.463e-6 sec
  
*/
//                                    Inner        Outer
static const Double_t t0IO[2]   = {1.20868e-9, 1.43615e-9};
static const Double_t tauC[2]   = {999.655e-9, 919.183e-9}; 
/*1.11
  (s+d)/(s-a)/(s-b)/(s-c) => A*exp(a*t) + B*exp(b*t) + C*exp(c*t)
  A = (a + d)/(a - b)/(a - c); B = (b + d)/(b - a)/(b - c);  C = (c + d)/(c - a)/(c - b); 
  (s+d)/(s*(s-a)*(s-b)*(s-c)) => -d/(a*b*c) + (a+d)/(a*(a-b)*(a-c))*exp(a*t) + (b+d)/(b*(b-a)*(b-c))*exp(b*t) + (c+d)/(c*(c-a)*(c-b))*exp(c*t);  
*/
const  Double_t mTimeBinWidth              = 1.06580379191673078e-07;//1.e-6/gStTpcDb->Electronics()->samplingFrequency();
const  Double_t timeBin = 5.78602945878541108e-01;// (cm)
const  Double_t pPI = 0.335;
const  Double_t pPO = 0.670;
const  Double_t rI  =  89.9;
const  Double_t rO  = 156.2;
const  Double_t lI  = 1.15;
const  Double_t lO  = 1.95;
#include "HardWarePosition.C"
#ifdef __useGainT0__
#include "tables/St_tpcPadGainT0B_Table.h"
#endif
//#include "DrawList.C"
//________________________________________________________________________________
struct Var_t {
  Double_t    sec;
  Double_t    row; // 
  Double_t  Npads; // npads or ntmbks
  Double_t Ntmbks; // npads or ntmbks
  Double_t   phiL;
  Double_t    eta;
  Double_t     zL;
  Double_t   AdcL;
  Double_t   xPad; // xRC hit position within pad
  Double_t   zTbk; // zRC hit position within time bucket
  Double_t     dX; // xRC - xMC or zRC - xMC
};
const static Int_t NoDim = sizeof(Var_t)/sizeof(Double_t);
const Char_t *Names[NoDim] = {"sec","row","Npads", "Ntmbks",  "phiL", "eta","zL", "AdcL","xPad", "zTbk","dX"};
const Int_t  nBins[NoDim]  = {    2,    2,      7,        7,      22,     32,  22,    12,     16,     16,  32};
const Var_t  xMin          = {  0.5,  0.5,    0.5,      0.5,    -1.1,   -1.5,  -5,     3,   -0.5,   -0.5,-0.5};
const Var_t  xMax          = { 24.5, 64.5,    7.5,     21.5,     1.1,    1.5, 215,     9,    0.5,    0.5, 0.5};
//________________________________________________________________________________
void SetInnerPadrows() {
  if (NoInnerRows > 0) return;
  TH3 *h3 = (TH3 *) gDirectory->Get("SecRow3");
  if (! h3) {
    NoOfRows = 45;
  } else {
    NoOfRows = h3->GetNbinsY();
  }
  NoInnerRows = NoOfRows - 32;
  cout << "SetInnerPadrows: NoInnerRows = " << NoInnerRows << "\tNoOfRows = " << NoOfRows << endl;
  assert(NoInnerRows >= 13);
}
#if 0
//________________________________________________________________________________
class TH1FSet : public TNamed {
private:
  Int_t      fNdimensions;  // number of dimensions
  TObjArray  fAxes;         // axes of the histogram
  TH1F     **fHist;         //!
  TArrayI    fBins;         //!
public:
  TH1FSet(const char* name="", const char* title="", Int_t nDim=0,
	 const Int_t* nbins=0, const Double_t* xmin=0, const Double_t* xmax=0) :
    TNamed(name, title), fNdimensions(nDim-1), fAxes(nDim), fHist(0), fBins(nDim-1) {
    for (Int_t i = 0; i <= fNdimensions; ++i) {
      TAxis* axis = new TAxis(nbins[i], xmin ? xmin[i] : 0., xmax ? xmax[i] : 1.);
      fAxes.AddAtAndExpand(axis, i);
    }
  }
  virtual ~TH1FSet() {delete [] fHist;}
  TAxis* GetAxis(Int_t nDim) const { return (TAxis*)fAxes[nDim]; }
  TH1F*  GetHist(Int_t indx) {return fHist[indx];}
  Int_t  GetNbins() {
    Int_t N = 1;
    for (Int_t i = 0; i < fNdimensions; i++) {
      N *= GetAxis(i)->GetNbins();
    }
    return N;
  }
  Int_t Index(const Double_t *x) {
    Int_t index = 0;
    for (Int_t i = 0; i < fNdimensions; i++) {
      fBins[i] = GetAxis(i)->FindBin(x[i]);
      if (fBins[i] <= 0) fBins[i] = 1;
      if (fBins[i] > GetAxis(i)->GetNbins()) fBins[i] = GetAxis(i)->GetNbins(); 
      if (i > 0) index *= GetAxis(i-1)->GetNbins();
      index += fBins[i]-1;
    }
    return index;
  }
  void GetX(Int_t index, Double_t *x) {
    for (Int_t i = fNdimensions - 1; i >=0; i--) {
      fBins[i] = index%GetAxis(i)->GetNbins() + 1;
      x[i] = GetAxis(i)->GetBinCenter(fBins[i]);
      index /= GetAxis(i)->GetNbins();
    }
  }
  Int_t Fill(const Double_t *x, Double_t w = 1.) {
    if (! fHist) {fHist = new TH1F*[GetNbins()]; memset(fHist, 0, GetNbins()*sizeof(TH1F*));}
    Int_t indx = Index(x);
    if (! fHist[indx]) {
      TString name(GetName());
      TString title(GetTitle());
      for (Int_t i = 0; i < fNdimensions; i++) {
	name += "_"; name += GetAxis(i)->GetName(); name += fBins[i];
	title += Form(" %s [%f,%f]",GetAxis(i)->GetName(),
		      GetAxis(i)->GetBinLowEdge(fBins[i]),GetAxis(i)->GetBinUpEdge(fBins[i]));
      }
      TAxis *ax = GetAxis(fNdimensions);
      fHist[indx] = new TH1F(name,title,ax->GetNbins(), ax->GetXmin(), ax->GetXmax());
      //      fHist[indx]->SetDirectory(0);
    }
    return fHist[indx]->Fill(x[fNdimensions],w);
  }
  ClassDef(TH1FSet,1)
};
ClassImp(TH1FSet);
#endif
//--------------------------------------------------------------------------------
void TpcT(const Char_t *files="*.root", const Char_t *opt = "H", const Char_t *Out = ""){//, const Char_t *Time = "20090415.000000") {
  //	   Int_t ev, Double_t tanCut, Int_t NpadCut, Double_t pMomin, Double_t pMomax) {
#ifdef __useGainT0__
  gSystem->Load("libStDb_Tables.so");
  TFile *f = new TFile(Form("$STAR/StarDb/Calibrations/tpc/tpcPadGainT0B.%s.root",Time));
  if (! f) return;
  St_tpcPadGainT0B *G = (St_tpcPadGainT0B*) f->Get("tpcPadGainT0B");
  tpcPadGainT0B_st *gainT0s = G->GetTable();
  delete f;
  if (! G) return;
#endif
  TDirIter Dir(files);
  Char_t *file = 0;
  Char_t *file1 = 0;
  Int_t NFiles = 0;
  TTreeIter iter("TpcT");
  while ((file = (Char_t *) Dir.NextFile())) {
    TString File(file);
    if (File.Contains("Plot") || File.Contains("Fit") || File.Contains("ADC") || File.Contains("Pads") || 
	File.Contains("hist") || File.Contains("tags") || File.Contains("MuMc") ||
	File.Contains("minimc") || File.Contains("event") ||
	File.Contains("All") ||
	File.Contains("MuDst.root")) continue;
    TFile *f = new TFile (File);
    if (f) {
      TTree *tree = (TTree *) f->Get("TpcT");
      if (tree ) {
	//    tree->Show(0);
	iter.AddFile(file); 
	NFiles++; 
	file1 = file;
	SetInnerPadrows();
      }
    }
    delete f;
  }
  cout << files << "\twith " << NFiles << " files" << endl; 
  if (! file1 ) return;
  TString output(Out);
  if (output == "") {
    output = file1;
    output.ReplaceAll(".root",".Plots.");
    output += opt;
    output += ".root";
  }
  cout << "Output for " << output << endl;
  const Int_t&       fNoPixels                                = iter("fNoPixels");
#ifndef __REAL_DATA__
  const Int_t&       fNoMcHit                                 = iter("fNoMcHit");
#endif
  const Int_t&       fNofPV                                   = iter("fNofPV");
#if 0
  const Int_t&       fNoTracksAtBestPV                        = iter("fNoTracksAtBestPV");
#endif
  const Float_t&     fxV                                      = iter("fxV");
  const Float_t&     fyV                                      = iter("fyV");
  const Float_t&     fzV                                      = iter("fzV");
  const Int_t&       fNoRcHit                                 = iter("fNoRcHit");
  const Int_t&       fNoRcTrack                               = iter("fNoRcTrack");
  const Int_t&       fAdcSum                                  = iter("fAdcSum");
#if 0
  const Int_t&       fPixels_                                 = iter("fPixels");
  const UChar_t*&    fPixels_mDetector                        = iter("fPixels.mDetector");
#endif
  const UChar_t*&    fPixels_mSector                          = iter("fPixels.mSector");
  const UChar_t*&    fPixels_mRow                             = iter("fPixels.mRow");
  const UChar_t*&    fPixels_mPad                             = iter("fPixels.mPad");
  const UShort_t*&   fPixels_mTimeBin                         = iter("fPixels.mTimeBin");
  const UShort_t*&   fPixels_mAdc                             = iter("fPixels.mAdc");
#ifdef PRINT
  const Int_t*&   fPixels_mIdTruth                         = iter("fPixels.mIdTruth");
#endif
#ifndef __REAL_DATA__
#if 0
  const Short_t*&    fPixels_mId                              = iter("fPixels.mId");
  const Float_t*&    fMcHit_mPosition_mX1                     = iter("fMcHit.mPosition.mX1");
  const Float_t*&    fMcHit_mPosition_mX2                     = iter("fMcHit.mPosition.mX2");
#endif
  const Float_t*&    fMcHit_mPosition_mX3                     = iter("fMcHit.mPosition.mX3");
#if 0
  const Float_t*&    fMcHit_mLocalMomentum_mX1                = iter("fMcHit.mLocalMomentum.mX1");
  const Float_t*&    fMcHit_mLocalMomentum_mX2                = iter("fMcHit.mLocalMomentum.mX2");
  const Float_t*&    fMcHit_mLocalMomentum_mX3                = iter("fMcHit.mLocalMomentum.mX3");
  const Float_t*&    fMcHit_mdE                               = iter("fMcHit.mdE");
  const Float_t*&    fMcHit_mdS                               = iter("fMcHit.mdS");
  const Long_t*&     fMcHit_mKey                              = iter("fMcHit.mKey");
#endif
#if 0
#ifdef PRINT
  const Long_t*&     fMcHit_mVolumeId                         = iter("fMcHit.mVolumeId");
#endif
#endif
#if 0
  const Float_t*&    fMcHit_mAdc                           = iter("fMcHit.mAdc");
#endif
  const Float_t*&    fMcHit_mMcl_x                            = iter("fMcHit.mMcl_x");
  const Float_t*&    fMcHit_mMcl_t                            = iter("fMcHit.mMcl_t");
#endif
#if 0
  const Float_t*&    fRcHit_mPosition_mX1                     = iter("fRcHit.mPosition.mX1");
  const Float_t*&    fRcHit_mPosition_mX2                     = iter("fRcHit.mPosition.mX2");
#endif
  const Float_t*&    fRcHit_mPosition_mX3                     = iter("fRcHit.mPosition.mX3");
#if 0
#ifdef PRINT
  const UInt_t*&     fRcHit_mHardwarePosition                 = iter("fRcHit.mHardwarePosition");
#endif
#endif
  const Float_t*&    fRcHit_mCharge                           = iter("fRcHit.mCharge");
#if 1
#if 0
  const Int_t*&      fRcHit_mId                               = iter("fRcHit.mId");
#endif
  const Int_t*&   fRcHit_mIdTruth                          = iter("fRcHit.mIdTruth");
#endif
  const UShort_t*&   fRcHit_mQuality                          = iter("fRcHit.mQuality");
#if 0
  const UChar_t*&    fRcHit_mFitFlag                          = iter("fRcHit.mFitFlag");
  const UChar_t*&    fRcHit_mTrackRefCount                    = iter("fRcHit.mTrackRefCount");
  const UShort_t*&    fRcHit_mFlag                             = iter("fRcHit.mFlag");
#endif
  const UChar_t*&    fRcHit_mMinpad                           = iter("fRcHit.mMinpad");
  const UChar_t*&    fRcHit_mMaxpad                           = iter("fRcHit.mMaxpad");
  const UChar_t*&    fRcHit_mMintmbk                          = iter("fRcHit.mMintmbk");
  const UChar_t*&    fRcHit_mMaxtmbk                          = iter("fRcHit.mMaxtmbk");
  const Short_t*&    fRcHit_mMcl_x                            = iter("fRcHit.mMcl_x");
  const Short_t*&    fRcHit_mMcl_t                            = iter("fRcHit.mMcl_t");
  //  const Int_t*&      fRcTrack_fSector                         = iter("fRcTrack.fSector");
  //  const Int_t*&      fRcTrack_fRow                            = iter("fRcTrack.fRow");
#if 1
  const Short_t*&    fRcTrack_mMcl_x                          = iter("fRcTrack.mMcl_x");
  const Short_t*&    fRcTrack_mMcl_t                          = iter("fRcTrack.mMcl_t");
#endif
  const Int_t*&      fRcTrack_fNpoints                        = iter("fRcTrack.fNpoints");
#if 0
  const Int_t*&      fRcTrack_fNfitpoints                     = iter("fRcTrack.fNfitpoints");
  const Int_t*&      fRcTrack_fifPrim                         = iter("fRcTrack.fifPrim");
#endif
  const Float_t*&    fRcTrack_fpx                             = iter("fRcTrack.fpx");
  const Float_t*&    fRcTrack_fpy                             = iter("fRcTrack.fpy");
  const Float_t*&    fRcTrack_fpz                             = iter("fRcTrack.fpz");
#if 0
  const Float_t*&    fRcTrack_fdEdx                           = iter("fRcTrack.fdEdx");
  const Float_t*&    fRcTrack_fTrackLength70                  = iter("fRcTrack.fTrackLength70");
#endif
#if 0
  if (! iter.Chain()) return;
  iter.Chain()->Show(0);
#endif
  static Int_t nx = 200;
  static Double_t xmin =   -5.;
  static Double_t xmax =   15.;
  static Int_t nz = 42;
  static Double_t zmin = -210;
  static Double_t zmax = -zmin;
#if 0
  static Int_t ny = 140;
  static Double_t ymin = -4;
  static Double_t ymax = 10;
#endif
  if (! fOut) fOut = new TFile(output,"recreate");
  fOut->cd();
  struct Name_t {
    const Char_t *Name;
    const Char_t *Title;
  };
#ifdef runY2008
  Name_t InOut[4] = {
    {"Inner","Inner for all except 16"},
    {"Outer","Outer for all except 16"},
    {"InnerX","Inner for Sector 16"},
    {"OuterX","Outer for Sector 16"}
  };
#else
  Name_t InOut[2] = {
    {"Inner","Inner TPX"},
    {"Outer","Outer TPX"}
  };
#endif
  Name_t RcMcTk[3] = {
    {"Rc","Rc"},
    {"Mc","Mc"},
    {"Tk","Tk"}
  };
  Name_t PadTime[2] = {
    {"Pad","Pad"},
    {"Time","Time"}
  };
  //               io  r  pt
  TProfile2D  *hist[4][3][2];
  TH1D       *histA[4][2];
  TH2D       *histB[4];
  TProfile  *profdL[4];
  TH2D       *lqa[4]; memset (lqa, 0, sizeof(lqa));
  memset (hist, 0, sizeof(hist));
#ifdef   runY2008
  Int_t NS = 4; // TPC + TPX
#else
  Int_t NS = 2; // TPX
#endif
  for (Int_t io = 0; io < NS; io++) {
    Int_t color = 1;
    for (Int_t rmt = 0; rmt < 3; rmt++) {
#ifdef __REAL_DATA__
      if (rmt == 1) continue;
#endif
      for (Int_t pt = 0; pt < 2; pt++) {
	TString Name(InOut[io].Name); Name += PadTime[pt].Name; Name += RcMcTk[rmt].Name;
	TString Title(InOut[io].Title); Title += PadTime[pt].Title; Title += RcMcTk[rmt].Title;
	hist[io][rmt][pt] = (TProfile2D *) gDirectory->Get(Name);
	if (! hist[io][rmt][pt]) {
	  hist[io][rmt][pt] = new TProfile2D(Name,Title,nx,xmin,xmax,nz,zmin,zmax,""); 
	  hist[io][rmt][pt]->SetMarkerStyle(20);
	  hist[io][rmt][pt]->SetMarkerColor(color++);
	}
      }
    }
    histA[io][0] = new TH1D(Form("ADCsum%s",InOut[io].Name),Form("ADCsum %s",InOut[io].Title),150,0,3e3);
    histA[io][1] = new TH1D(Form("ADCMax%s",InOut[io].Name),Form("ADCMax %s",InOut[io].Title),100,0,1e3);
    histB[io]    = new TH2D(Form("dPdT%s",InOut[io].Name),Form("delta Pad vs delta Time %s",InOut[io].Title),100,-.5,0.5,100,-.5,0.5);
    profdL[io]   = new TProfile(Form("dL%s",InOut[io].Name),Form("cos^{-2}(#lambda) vs delta hit Z for  %s",InOut[io].Title),nz,zmin,zmax);
    lqa[io]      = new TH2D(Form("lqa%s",InOut[io].Name),Form("Lambda versus QA for %s",InOut[io].Title),100,0,100,100,0.,100.);
  }
#ifdef __LASER__
  static   Double_t offset = -178.3;
  Double_t x[7] = {offset  -0.947, offset +  26.752, offset +  57.808, 
		   offset + 87.667, offset + 118.674, offset + 146.232, offset + 178.3};
  Double_t zShift[2] = {- (6.576 + 0.5), (7.346 - 0.5)};
  
#endif
  TChain *chain = iter.Chain();
  TString  currentFileName;
#ifdef PRINT
  Int_t entry = 0;
#endif
  while (iter.Next()) {
    if (currentFileName != TString(chain->GetFile()->GetName())) {
      currentFileName = chain->GetFile()->GetName();
      cout << "Open File " << currentFileName.Data() << endl;
#ifdef PRINT
      entry = 0;
#endif
    }
    if (! fNoPixels ) continue;
    if (fNoRcHit != 1 || fNoRcTrack < 1) continue;
    if (fRcTrack_fNpoints[0] < 10) continue;
    TVector3 mom(fRcTrack_fpx[0],fRcTrack_fpy[0],fRcTrack_fpz[0]);
#if !defined( __LASER__ ) && !defined(__Cosmics__)
    if (fNofPV != 1) continue;
    if (TMath::Abs(fzV) > 20) continue;
    if (TMath::Abs(fxV) > 1 || TMath::Abs(fyV) > 1) continue;
    if (mom.Pt()  < 0.4 || mom.Pt()  > 0.6) continue;
#if 0
    if (mom.Eta() <-1.0 || mom.Eta() > 0.0) continue;
    if (fRcTrack_fTrackLength70[0] < 40) continue;
    if (fRcTrack_fdEdx[0] < 2e-6 || fRcTrack_fdEdx[0] > 3e-6) continue;
#endif
#endif
    //    if (fNoPixels > 80) continue;
    //    if (fNoRcHit < NpadCut) continue;
    if (fRcHit_mCharge[0] < 1.e-6 || fRcHit_mCharge[0] > 100.e-6) continue;
#if 0
    Double_t pmag = mom.Mag();
    //    if (pMomin > 0 && pmag < pMomin || pMomax >0 && pmag > pMomax) continue;
    //    if (tanCut > 0 && TMath::Abs(fRcTrack_fpy[0]) > 1e-7 && TMath::Abs(fRcTrack_fpx[0]/fRcTrack_fpy[0]) > tanCut) continue;
    // if (Cut(ientry) < 0) continue;
#endif
    if (fNoRcHit != 1) continue;
    if (fAdcSum < 100 || fAdcSum > 3.e3) continue;
    Int_t kPadMin = 999;
    Int_t kPadMax =   0;
    Int_t kTbMin  = 999;
    Int_t kTbMax  =   0;
    Int_t pad  = 0;
    Int_t tb   = 0;
    Double_t sum = 0;
    Double_t pav = 0;
    Double_t tav = 0;
    Double_t cXX = 0, cZZ = 0, cXZ = 0;
    Int_t sector = fPixels_mSector[0];
    Int_t row    = fPixels_mRow[0];
    Double_t zRc = fRcHit_mPosition_mX3[0];
#ifdef __LASER__
    Int_t l = 0; // East
    if (zRc > 0) l = 1;
    zRc += zShift[l];
    Double_t zRcA = TMath::Abs(zRc);
    Int_t found = 0;
    for (l = 0; l < 7; l++) {
      if (TMath::Abs(-zRcA - x[l]) < 4) {
	found = 1;
	break;
      }
    }
    if (! found) continue;
#endif    
    Int_t io = 0;
    if (row > NoInnerRows) io = 1;
#ifdef runY2008
    if (sector  == 16) io += 2;
#endif
    Double_t cosL2I = 1. + fRcTrack_fpz[0]*fRcTrack_fpz[0]/(fRcTrack_fpx[0]*fRcTrack_fpx[0] + fRcTrack_fpy[0]*fRcTrack_fpy[0]);
    profdL[io]->Fill(zRc,cosL2I);
    for (Int_t i = 0; i < fNoPixels; i++) {
      if (fRcHit_mIdTruth[0] != fPixels_mIdTruth[i]) continue;
      pad = fPixels_mPad[i];
      tb  = fPixels_mTimeBin[i];
      if (tb  >= 512) continue;
      if (pad > 182) continue;
      if (pad < kPadMin) kPadMin = pad;
      if (pad > kPadMax) kPadMax = pad;
      if (tb  < kTbMin) kTbMin = tb;
      if (tb  > kTbMax) kTbMax = tb;
    }
    if (kPadMin > kPadMax) continue;
    if (kTbMin  > kTbMax) continue;
    const Int_t NP = kPadMax - kPadMin + 3;
    const Int_t NT = kTbMax  - kTbMin  + 2;
    TRMatrix adcs(NP,NT);
    Int_t tMax = -1;
    Int_t pMax = -1;
    Double_t AtMax = -1; 
    for  (Int_t i = 0; i < fNoPixels; i++) {
      if (fRcHit_mIdTruth[0] != fPixels_mIdTruth[i]) continue;
      pad = fPixels_mPad[i];
      tb  = fPixels_mTimeBin[i];
      if (tb  < kTbMin  || tb  > kTbMax) continue;
      if (pad < kPadMin || pad > kPadMax) continue;
#ifdef __useGainT0__
      Double_t ADC = fPixels_mAdc[i]*(gainT0s+sector-1)->Gain[row-1][pad-1];
#else
      Double_t ADC = fPixels_mAdc[i];
#endif
      if (ADC <= 0.0) continue;
      adcs(pad     - kPadMin,tb - kTbMin) = ADC;
      adcs(pad     - kPadMin,NT-1)       += ADC;
      adcs(NP-2,tb - kTbMin)             += ADC;
      Double_t x = pad;
#ifdef __useGainT0__
      Double_t z = (tb+(gainT0s+sector-1)->T0[row-1][pad-1]);
#else
      Double_t z = tb;
#endif
      adcs(NP-1,tb - kTbMin)             += z*ADC;
      adcs(NP-1,NT-1)                    += ADC;
      pav                                += x*ADC;
      cXX                                += x*x*ADC;
      tav                                += z*ADC;
      cZZ                                += z*z*ADC;
      cXZ                                += x*z*ADC;
      if (ADC > AtMax) {
	AtMax = ADC;
	tMax  = tb;
	pMax  = pad;
      }
    }
    sum = adcs(NP-1,NT-1);
    if (sum <= 0) continue;
    pav /= sum;
    tav /= sum;
    cXX /= sum; cXX -= pav*pav;
    cXZ /= sum; cXZ -= pav*tav;
    cZZ /= sum; cZZ -= tav*tav;
    Double_t lambda = ((cXX + cZZ) + TMath::Sqrt((cXX - cZZ)*(cXX - cZZ) + 4*cXZ*cXZ))/2;
    lqa[io]->Fill(fRcHit_mQuality[0],lambda);
    Double_t pax  = (fRcHit_mMcl_x[0])/64.;
    Double_t tax  = (fRcHit_mMcl_t[0])/64.;
    histB[io]->Fill(pav-pax,tav-tax);
    entry++;
    if (entry%1000 == 1) {
      cout << entry << "\t =======================================================" << endl;
      cout << "Sector/Row = " << sector << " / " << row;
      cout << "\tfNoPixels \t" << fNoPixels << "\tNPads " << endl; // fNoRcHit << endl;
      cout << "Charge(keV) \t" << 1e6*fRcHit_mCharge[0];
      if (TMath::Abs(fRcTrack_fpy[0]) > 1e-7) 
	cout << "\tpx/py\t" << fRcTrack_fpx[0]/fRcTrack_fpy[0] 
	     << "\tpT\t" << TMath::Sqrt(fRcTrack_fpx[0]*fRcTrack_fpx[0]+fRcTrack_fpy[0]*fRcTrack_fpy[0]);
      cout << endl;
      cout << "\tfAdcSum "  << fAdcSum;
      cout << "\tRc:Pad\t"    << pax  
	   << "\tRc:Time\t" << tax  << endl;
#ifndef __REAL_DATA__
      cout << "Mc:Pad\t" << fMcHit_mMcl_x[0]
	   << "\tMc:Time\t" << fMcHit_mMcl_t[0] << endl;
#endif
      cout << "\tsum = " << sum << "\tpav = " << pav << "\ttav = " << tav << endl;
      cout << "\tsum = " << sum << "\tpax = " << pax << "\ttax = " << tax << endl;
      cout << "cXX = " << cXX << "\tcXZ = " << cXZ << "\tcZZ = " << cZZ << "\tlambda = " << lambda 
	   << "\tQA = " << fRcHit_mQuality[0]
	   << endl;
      cout << "kPadMin/kPadMax\t" << kPadMin << "/" << kPadMax
	   << "\tkTbMin/kTbMax\t" << kTbMin << "/" << kTbMax << endl;
      cout << "Hit PadMin/Max\t"  << (fRcHit_mMcl_x[0])/64 - fRcHit_mMinpad[0] 
	   << "/"                 << (fRcHit_mMcl_x[0])/64 + fRcHit_mMaxpad[0] 
	   << "\tTbMin/Max\t"     << (fRcHit_mMcl_t[0])/64 - fRcHit_mMintmbk[0] 
	   << "/"                 << (fRcHit_mMcl_t[0])/64 + fRcHit_mMaxtmbk[0] 
	   << endl;
      cout << " ";
      for (tb = kTbMin; tb <= kTbMax + 1; tb++) cout << "\t|" << tb;
      cout << endl;
      cout << "_";
      for (tb = kTbMin; tb <= kTbMax + 1; tb++) cout << "\t|____";
      cout << endl;
      for (pad = kPadMin; pad <= kPadMax + 2; pad++) {
	if (pad >= kPadMax + 1) cout << "---------------------------------------------" << endl;
#ifdef __useGainT0__
	cout << pad << "|" << "G:" << (gainT0s+sector-1)->Gain[row-1][pad-1] 
	     << "|T0:" << (gainT0s+sector-1)->T0[row-1][pad-1] << "|";
#else
	cout << pad << "|";
#endif
	for (tb = kTbMin; tb <= kTbMax + 1; tb++) {
	  if (tb <= kTbMax) cout << "\t";
	  else              cout << "\t|";
	  cout << Form("%4.2f",adcs(pad-kPadMin,tb-kTbMin));
	}
	cout << endl;
      }
    }
    //    if (TMath::Abs(pax - pav) > 0.2 || TMath::Abs(tax - tav) > 0.2) continue;
    if (adcs(NP-1,NT-1) <= 0) continue;
    histA[io][0]->Fill(sum);
    histA[io][1]->Fill(AtMax);
    for (Int_t j = 0; j < NT-1; j++) {
      if (adcs(NP-2,j) > 0) adcs(NP-1,j) /= adcs(NP-2,j);
    }
    for (Int_t i = 0; i < NP-1 ; i++) {
      for (Int_t j = 0; j < NT; j++) {
	adcs(i,j) /= adcs(NP-1,NT-1);
      }
    }
    if (entry%1000 == 1) {
#ifdef PRINT
      adcs.Print();
#if 0
      for (Int_t i = 0; i < fNoPixels; i++) 
	cout << i 
	     << "\tRow\t" << (Int_t) row[i]
	     << "\tPad\t" << (Int_t)fPixels_mPad[i]
	     << "\tTimeBin\t" << fPixels_mTimeBin[i]
	     << "\tadc\t" << (Int_t) fPixels_mAdc[i]
	     << "\tId\t" << (Int_t) fPixels_mIdTruth[i]
	     << "\tR\t" << ((Double_t) fPixels_mAdc[i])/((Double_t) fAdcSum)
	     << endl;
      for (Int_t i = 0; i < fNoRcHit; i++) 
	cout << "Rc\t" << i << "\tRow\t" << padrow(fRcHit_mHardwarePosition[i])
	     << "\tPad\t" << (fRcHit_mMcl_x[i])/64.
	     << "\tTimeBin\t" << (fRcHit_mMcl_t[i])/64.
	     << endl;
#ifndef __REAL_DATA__
      for (Int_t i = 0; i < fNoMcHit; i++) 
	cout << "Mc\t" << i << "\tRow\t" << fMcHit_mVolumeId[i]%100
	     << "\tPad\t" << fMcHit_mMcl_x[i]
	     << "\tTimeBin\t" << fMcHit_mMcl_t[i]
	     << endl;
#endif
#endif
    } // end of print
      //________________________________________________________________________________
#endif
    // Sector 1; RDO 4;
    //    if (fRcTrack_fSector[0] == 1 && fRcTrack_fRow[0] >= 22 && fRcTrack_fRow[0] <= 29) continue;
#ifndef __REAL_DATA__
    Double_t zMc = fMcHit_mPosition_mX3[0];
#endif
#ifndef __REAL_DATA__
    //    if (fRcHit_mQuality[0] < 95) continue;
#endif
    for (Int_t i = 0; i < NP - 2; i++) {
      Double_t ratio = adcs(i,NT-1);
      //      Double_t delPadRc = i + kPadMin - pax;
      Double_t delPadRc = i + kPadMin - pav;
      Double_t delPadTk = i + kPadMin - ((fRcTrack_mMcl_x[0])/64.);
      hist[io][0][0]->Fill(delPadRc, zRc, ratio);
      if (fNoRcTrack == 1)
	hist[io][2][0]->Fill(delPadTk, zRc,ratio);
#ifndef __REAL_DATA__
      Double_t delPadMc = i + kPadMin - (fMcHit_mMcl_x[0]);
      if (fNoMcHit == 1 || fNoMcHit == 3)
	hist[io][1][0]->Fill(delPadMc, zMc,ratio);
#endif
    }
    for (Int_t j = 0; j < NT - 1; j++) {
      Double_t ratio = adcs(NP-2,j);
      tb    = adcs(NP-1,j);
      //      Double_t delTbRc = tb + 0.5 - tax;
      Double_t delTbRc = tb + 0.5 - tav;
      Double_t delTbTk = tb + 0.5 - ((fRcTrack_mMcl_t[0])/64.);
      hist[io][0][1]->Fill(delTbRc, zRc,ratio);
      if (fNoRcTrack == 1)
	hist[io][2][1]->Fill(delTbTk, zRc,ratio);
#ifndef __REAL_DATA__
      Double_t delTbMc = tb  + 0.5 - (fMcHit_mMcl_t[0]);
      if (fNoMcHit == 1 || fNoMcHit == 3)
	hist[io][1][1]->Fill(delTbMc,   zMc,ratio);
#endif
    }
  }
  fOut->Write();
}
//________________________________________________________________________________
void DrawTpcTPlots(const Char_t *histname="OuterPad",const Char_t *Option="") {
  if (Option) {}
  const Int_t NT = 3; 
  const Char_t *types[NT] = {"Rc","Mc","Tk"};
  TFile *FitFiles[10];
  Int_t NF = 0;
  TList *files = (TList *) gROOT->GetListOfFiles();
  if (! files) return;
  TIter next(files);
  TFile *f = 0;
  while ( (f = (TFile *) next()) ) { 
    for (Int_t type = 0; type < NT; type++) {
      TProfile *h = (TProfile *) f->Get(Form("%s%s",histname,types[type]));
      if ( h ) {
	FitFiles[NF] = f; NF++;
	cout << "Found " << h->GetName() << " in " << f->GetName() << endl;
	break;
      }
    }
  }
  TCanvas *c1 = new TCanvas(histname,Form("Compare %s",histname));
  c1->SetGrid(); 
  TH1F *frame = c1->DrawFrame(-5,0.0,10,0.42);
  TString Histname(histname);
  TLegend *leg = new TLegend(0.65,0.6,0.9,0.9,"");
  frame->Draw();
  TString same("same");
  for (Int_t i = 0; i<NF; i++) {
    if (FitFiles[i]) { 
      FitFiles[i]->cd();
      for (Int_t type = 0; type < NT; type++) {
	TProfile *h = (TProfile *) FitFiles[i]->Get(Form("%s%s",histname,types[type]));
	if (! h) {
	  cout << Form("%s%s",histname,types[type]) << " has not been found." << endl;
	  continue;
	}
	if (h->GetEntries() < 1) {
	  cout << h->GetName() << " is empty." << endl;
	  continue;
	}
	cout << "Draw " << h->GetName() << endl;
	h->SetMarkerColor(2*i + 1 + type);
	h->Draw(same.Data());
	same = "same";
	TString Title(gSystem->BaseName(FitFiles[i]->GetName()));
	Title.ReplaceAll(".root","");
	Title += " ";
	Title += types[type];
	leg->AddEntry(h,Form("%s",Title.Data()));
      }
    }
  }
  leg->Draw();
}
//________________________________________________________________________________
Double_t ShaperFunc(Double_t *x, Double_t *par) {
  Double_t taup = par[0];
  Double_t width = par[1];
  Double_t FWHMp = par[2];
  Double_t t = (x[0]-par[3])*width/taup;
  Double_t Delta = width/taup;
  Double_t t1 = t - Delta/2.;
  Double_t t2 = t1 + Delta;
  if (t1 < 0) t1 = 0;
  if (t2 < 0) t2 = 0;
  Double_t p = TMath::Power(FWHMp/((2*TMath::Sqrt(2*TMath::Log(2.)))*taup),2);
  Double_t val = TMath::Gamma(p,t2) - TMath::Gamma(p,t1);
  //  return (val > 0) ? TMath::Log(val) : -999.;
  return par[4]*val;
}
//________________________________________________________________________________
Double_t Gatti(Double_t *x, Double_t *par) {
  /************************************************************************
   *  Function    : generates the cathode signal using                    *
   *                the single-parameter Gatti formula:                   *
   *                              1 - tanh(K2 * lambda)**2                *
   *     GFunc(lambda) = K1 * -------------------------------             *
   *                           1 + K3 * tanh (K2 *lambda)**2              *
   *     lambda = p*x/h, h is anode cathode spacing, p - strip pitch,     *
   *                     x distnace from center of pad in pads            *
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
  Double_t w = par[0]; // w = pad width       
  Double_t p = par[5]; // p = pad pitch
  Double_t y = p*x[0];   // distance to center of strip
  Double_t h = par[1]; // h = Anode-Cathode gap  
  Double_t lambda = TMath::Abs(y/h);
  Double_t K3 = par[3]; 
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
  //  return val > 0 ? TMath::Log(val) : -999.;
  return val;
}
//________________________________________________________________________________
Double_t PadResponseFunc(Double_t *x, Double_t *par) {
  Double_t CrossTalk = par[4];
  Double_t Value = 0;
  Double_t X = x[0];
  if (CrossTalk > 0) {
    for (Int_t i = -1; i <= 1; i++) {
      Double_t xx = X + i;
      if (i == 0) Value += (1. - 2.*CrossTalk)*Gatti(&xx,par);
      else        Value +=          CrossTalk *Gatti(&xx,par);
    }
  } else   Value = Gatti(&X,par);
  return Value;
}
//________________________________________________________________________________
Double_t ConvolutionF(Double_t *x, Double_t *par) {
  Int_t icase = (Int_t) par[0]; // 0 - Inner, 1 - Outer, 2 - Time Shape, 3 - Gaussian fit (inner), 4 - Gaussian fit (outer)
  if (icase == 3 || icase == 4) {
    if (par[3] < 0 && par[4] < 0 && par[2] > 0) {
      Double_t w = par[14]; // pad width
      Double_t p = par[15]; // pad pitch
      if (icase == 4) {w =  par[17]; p = par[18];}
      Double_t xx = p*x[0];
      Double_t sigma = p*TMath::Sqrt(par[2]);
      Double_t t1 = (xx - 0.5*w)/sigma;
      Double_t t2 = (xx + 0.5*w)/sigma;
      Double_t dd = TMath::Erfc(t1) - TMath::Erfc(t2);
      return par[1]*dd + par[6];
    } else {
      return 0; 
    }
  }
  TF1 *pfunc = mPadResponseFunctionInner;
  pfunc->SetParameter(3,par[3]);
  pfunc->SetParameter(4,par[9]);
  Double_t xshft = par[5];
  Double_t noise = par[6];
  if (icase == 1) {
    pfunc = mPadResponseFunctionOuter;
    pfunc->SetParameter(3,par[4]);
    pfunc->SetParameter(4,par[9]);
  }
  else if (icase == 2) {
    pfunc = mShaperResponse;
    pfunc->SetParameter(0,par[8]); // tau
    pfunc->SetParameter(2,par[7]); // FWHM
    pfunc->SetParameter(3,par[5]); // t0
  }
  if (! pfunc) return 0;
  Double_t Area   = par[1];
  Double_t sigma2 = TMath::Abs(par[2]);
  Double_t sigma  = TMath::Sqrt(sigma2);
  Double_t Frac   = par[10];
  Double_t Sigma = par[11];
  if (Frac < 0.0 || Frac > 1.0 || Sigma <= 0.0) Frac = 0;
  Double_t sg = 5;
  static const Int_t N = 100;
  Double_t dx = 2*sg/N;
  Double_t xc = x[0] - xshft;
  Double_t value = 0;
  if (sigma > 0.0) {
    for (Int_t i = 0; i <= N; i++) {
      Double_t xx = xc + sigma*(-sg + dx*i);
      Double_t xa = xx;
      if (icase < 2) xa = TMath::Abs(xx);
      Double_t pfuncXa = pfunc->Eval(xa);
      value               += pfuncXa*TMath::Gaus(xx, xc, sigma, 1)*(1 - Frac);
      if (Frac > 0) value += pfuncXa*TMath::Gaus(xx, xc, Sigma, 1)*Frac;
    }
    value *= dx*Area*sigma;
  }
  else value = Area*pfunc->Eval(xc);
  return value + noise;
}
//________________________________________________________________________________
Double_t ConvGausShaperF(Double_t *x, Double_t *par) {
  /*
    Int_t bin = 21;
    TH2 *hist = (TH2 *) gDirectory->Get("InnerTimeRc");
    TProfile *prof = (TProfile *) gDirectory->Get("dLInner");
    Double_t lH = lI;
    TF1 *ft = new TF1("tf",ConvGausShaperF,-2,5,5);
    tf->SetParameters(0,1,1,0,1);
    tf->SetParNames("shift","Area","sigmaSQ","noise","spread");
    tf->SetParLimits(2,0.,10.);
    tf->FixParameter(3,0);
    Double_t cosL2I = dLOuter->GetBinContent(bin);
    Double_t spread = lH/TMath::Sqrt(cosL2I)/timeBin;
    tf->FixParameter(4,spread);
    TH1 *proj = hist->ProjectionX(Form("%s_%i",hist->GetName(),bin),bin,bin);
    proj->Draw();
    proj->Fit("tf","er","",-2.5,2.5);
   */
  static TF1* shape = 0;
  if (! shape) {
    TDirectory *dir = gDirectory;
    TFile *_fil = TFile::Open("/afs/rhic.bnl.gov/star/users/fisyak/macros/ShaperResponse.root");
    if (!_fil) return 0;
    TF1 *f = (TF1 *) _fil->Get("ShaperFunc_O_S01");
    if (! f) return 0;
    dir->cd();
    shape = new TF1(*f);
    delete _fil;
  }
  Double_t xshift = par[0];
  Double_t Area   = par[1];
  Double_t sigma2 = TMath::Abs(par[2]);
  Double_t sigma  = TMath::Sqrt(sigma2);
  Double_t noise  = par[3];
  Double_t spread = par[4]; // spread on pad row in time bins
  Double_t sg = 5;
  static const Int_t N = 100;
  Double_t dx = 2*sg/N;
  Double_t xc = x[0] - xshift;
  Double_t value = 0;
  if (sigma > 0.0) {
    for (Int_t i = 0; i <= N; i++) {
      Double_t xx = xc + sigma*(-sg + dx*i);
      Double_t xa = xx;
      Double_t pfuncXa = shape->Eval(xa);
      if (spread <= 0) value += pfuncXa*TMath::Gaus(xx, xc, sigma, 1);
      else             value += pfuncXa*(TMath::Freq((xx-xc + spread/2)/sigma) - TMath::Freq((xx-xc - spread/2)/sigma));
    }
    value *= dx*Area*sigma;
  }
  else value = Area*shape->Eval(xc);
  return value + noise;
}
//________________________________________________________________________________
TGraphErrors *OmegaTau() {
  Double_t xx[]={0,0.5,1,0.,0.5,1};
  Double_t yy[]={647,331,111,517,311,143};
  Double_t dxx[6];
  Double_t dyy[6];
  for (Int_t i = 0; i < 6; i++) {dxx[i] = 0.1; dyy[i] = 0.1*yy[i];}
  TGraphErrors *gr2 = new TGraphErrors(6,xx,yy,dxx,dyy);
  gr2->SetMarkerStyle(20);
  return gr2;
}
//________________________________________________________________________________
void TpcTAdc(const Char_t *files="*.root", const Char_t *Out = "") {
  TDirIter Dir(files);
  Char_t *file = 0;
  Char_t *file1 = 0;
  Int_t NFiles = 0;
  TTreeIter iter("TpcT");
  while ((file = (Char_t *) Dir.NextFile())) {
    TString File(file);
    if (File.Contains("Plot") || File.Contains("Fit") || File.Contains("ADC") || File.Contains("Pads") || 
	File.Contains("hist") || File.Contains("tags") || File.Contains("MuMc") ||
	File.Contains("minimc") || File.Contains("event") ||
	File.Contains("All") || File.Contains("Sparse") ||
	File.Contains("MuDst.root")) continue;
    TFile *f = new TFile (File);
    if (f) {
      TTree *tree = (TTree *) f->Get("TpcT");
      if (! tree ) continue;
      //    tree->Show(0);
      iter.AddFile(file); 
      NFiles++; 
      file1 = file;
      SetInnerPadrows();
    }
    delete f;
  }
  cout << files << "\twith " << NFiles << " files" << endl; 
  if (! file1 ) return;
  TString output(Out);
  if (output == "") {
    output = file1;
    output.ReplaceAll(".root",".ADC.root");
  }
  cout << "Output for " << output << endl;
  const Int_t&       fNoRcHit                                 = iter("fNoRcHit");
  const Int_t&       fNoMcHit                                 = iter("fNoMcHit");
  const Int_t&       fAdcSum                                  = iter("fAdcSum");
  const Float_t*&    fMcHit_mPosition_mX3                     = iter("fMcHit.mPosition.mX3");
  const Float_t*&    fMcHit_mdE                               = iter("fMcHit.mdE");
  const Float_t*&    fMcHit_mdS                               = iter("fMcHit.mdS");
  const Long_t*&     fMcHit_mKey                              = iter("fMcHit.mKey");
  const Long_t*&     fMcHit_mVolumeId                         = iter("fMcHit.mVolumeId");
  const Float_t*&    fMcHit_mAdc                           = iter("fMcHit.mAdc");
  //  const Int_t*&      fRcHit_mId                               = iter("fRcHit.mId");
  const Int_t*&   fRcHit_mIdTruth                          = iter("fRcHit.mIdTruth");
  const UShort_t*&   fRcHit_mQuality                          = iter("fRcHit.mQuality");
  if (! fOut) fOut = new TFile(output,"recreate");
  fOut->cd();
#if 0
  TF1* off = new TF1("off","exp(log(1.+[0]/exp(x)))",3,10);
#endif
  TProfile2D *inout[4];
  inout[0] = new TProfile2D("inner","log(simulated ADC) versus log(recon. ADC) and Z",
			    70,3.,10.,210,-210,210,"");
  inout[1] = new TProfile2D("outer","log(simulated ADC) versus log(recon. ADC) and Z",
			    70,3.,10.,210,-210,210,"");
  inout[2] = new TProfile2D("innerR","log(simulated ADC)-log(recon. ADC) versus log(recon. ADC) and Z",
			    70,3.,10.,210,-210,210,"");
  inout[3] = new TProfile2D("outerR","log(simulated ADC)-log(recon. ADC) versus log(recon. ADC) and Z",
			    70,3.,10.,210,-210,210,"");
  Double_t dsCut[2] = {1., 2.};
  while (iter.Next()) {
    if (fNoRcHit != 1) continue;
    if (fNoMcHit != 1 && fNoMcHit != 3) continue;
    if (fAdcSum <= 0) continue;
    for (Int_t k = 0; k < fNoMcHit; k++) {
      if (fMcHit_mKey[k] != fRcHit_mIdTruth[k]) continue;
      if (fRcHit_mQuality[k] < 95) continue;
      Int_t io = 0;
      if (fMcHit_mVolumeId[k]%100 > NoInnerRows) io = 1;
      if (fMcHit_mdS[k] < dsCut[io]) continue;
      if (fMcHit_mdE[k] <= 0 || fMcHit_mdE[k] > 1e-3) continue;
      if (fMcHit_mAdc[k] <= 0) continue;
      Double_t ratio = fMcHit_mAdc[k]/fAdcSum;
      if (ratio < 0.1 || ratio > 10) continue;
      inout[io]->Fill(TMath::Log(fAdcSum),fMcHit_mPosition_mX3[k], TMath::Log(fMcHit_mAdc[k]));
      inout[io+2]->Fill(TMath::Log(fAdcSum),fMcHit_mPosition_mX3[k], TMath::Log(fMcHit_mAdc[k])-TMath::Log(fAdcSum));
    }
  }
  fOut->Write();
}
//________________________________________________________________________________
void TpcTdENP(const Char_t *files="*.root", const Char_t *Out = "") {
  TDirIter Dir(files);
  Char_t *file = 0;
  Char_t *file1 = 0;
  Int_t NFiles = 0;
  TTreeIter iter("TpcT");
  while ((file = (Char_t *) Dir.NextFile())) {
    TString File(file);
    if (File.Contains("Plot") || File.Contains("Fit") || File.Contains("ADC") || File.Contains("Pads") || 
	File.Contains("hist") || File.Contains("tags") || File.Contains("MuMc") ||
	File.Contains("minimc") || File.Contains("event") ||
	File.Contains("All") || File.Contains("Sparse") ||
	File.Contains("MuDst.root")) continue;
    TFile *f = new TFile (File);
    if (f) {
      TTree *tree = (TTree *) f->Get("TpcT");
      if (! tree ) continue;
      //    tree->Show(0);
      iter.AddFile(file); 
      NFiles++; 
      file1 = file;
      SetInnerPadrows();
    }
    delete f;
  }
  cout << files << "\twith " << NFiles << " files" << endl; 
  if (! file1 ) return;
  TString output(Out);
  if (output == "") {
    output = file1;
    output.ReplaceAll(".root",".dENP.root");
  }
  cout << "Output for " << output << endl;
  // TpcT->Draw("log(1e9*fRcHit.mCharge/fMcHit.mnP):log(fMcHit.mnP)>>h(200,0,8,500,0,10)","fMcHit.mVolumeId%100<=13&&fNoMcHit==1&&fNoRcHit==1&&fRcHit.mIdTruth==fMcHit.mKey&&fRcHit.mQuality>90","colz")
  const Int_t&       fNoRcHit                                 = iter("fNoRcHit");
  const Int_t&       fNoMcHit                                 = iter("fNoMcHit");
  const Int_t&       fSector                                  = iter("fSector");
  const Int_t&       fRow                                     = iter("fRow");
  const Int_t&       fAdcSum                                  = iter("fAdcSum");
  const Float_t*&    fMcHit_mPosition_mX3                     = iter("fMcHit.mPosition.mX3");
  const Float_t*&    fMcHit_mdE                               = iter("fMcHit.mdE");
  const Float_t*&    fMcHit_mdS                               = iter("fMcHit.mdS");
  const Long_t*&     fMcHit_mKey                              = iter("fMcHit.mKey");
  const Long_t*&     fMcHit_mVolumeId                         = iter("fMcHit.mVolumeId");
  const Float_t*&    fMcHit_mAdc                           = iter("fMcHit.mAdc");
  const Int_t*&      fMcHit_mnP                               = iter("fMcHit.mnP");
  //  const Int_t*&      fRcHit_mId                               = iter("fRcHit.mId");
  const Float_t*&    fRcHit_mCharge                           = iter("fRcHit.mCharge");
  const Int_t*&   fRcHit_mIdTruth                          = iter("fRcHit.mIdTruth");
  const UShort_t*&   fRcHit_mQuality                          = iter("fRcHit.mQuality");
  if (! fOut) fOut = new TFile(output,"recreate");
  fOut->cd();
#if 0
  TF1* off = new TF1("off","exp(log(1.+[0]/exp(x)))",3,10);
#endif
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
  TH2D *inout[2] = {0};
  inout[0] = new TH2D("dPdTI","log(dE(eV)/nP) versus log(nP) for Inner sector",Npbins-1,pbinsL,500,0.0,10.0);
  inout[1] = new TH2D("dPdTO","log(dE(eV)/nP) versus log(nP) for Outer sector",Npbins-1,pbinsL,500,0.0,10.0);
  while (iter.Next()) {
    if (fNoRcHit != 1) continue;
    if (fNoMcHit != 1 && fNoMcHit != 3) continue;
    for (Int_t k = 0; k < fNoMcHit; k++) {
      if (fMcHit_mKey[k] != fRcHit_mIdTruth[k]) continue;
      if (fRcHit_mQuality[k] < 95) continue;
      if (fRcHit_mCharge[k] <= 0) continue;
      Int_t io = 0;
      if (fMcHit_mVolumeId[k]%100 > NoInnerRows) io = 1;
      if (fMcHit_mdE[k] <= 0 || fMcHit_mdE[k] > 1e-3) continue;
      if (fMcHit_mAdc[k] <= 0) continue;
      if (fMcHit_mnP[k] <= 0) continue;
      inout[io]->Fill(TMath::Log(fMcHit_mnP[k]),TMath::Log(1e9*fRcHit_mCharge[k]/fMcHit_mnP[k]));
    }
  }
  fOut->Write();
}
//________________________________________________________________________________
void TpcTPads(const Char_t *files="*.root", const Char_t *Out = "") {
  TDirIter Dir(files);
  Char_t *file = 0;
  Char_t *file1 = 0;
  Int_t NFiles = 0;
  TTreeIter iter("TpcT");
  while ((file = (Char_t *) Dir.NextFile())) {
    TString File(file);
    TString bN(gSystem->BaseName(file));
    cout << "File " << file << " bname = " << bN.Data() << endl;
    if (bN.Contains("Plot") || bN.Contains("Fit") || bN.Contains("ADC") || bN.Contains("Pads") || 
	bN.Contains("hist") || bN.Contains("tags") || bN.Contains("MuMc") || bN.Contains("minimc") ||
	bN.Contains("minimc") || bN.Contains("event") ||
	bN.Contains("All") || bN.Contains("Sparse") ||
	bN.Contains("MuDst.root")) continue;
    TFile *f = new TFile (File);
    if (f) {
      TTree *tree = (TTree *) f->Get("TpcT");
      if (! tree ) continue;
      //    tree->Show(0);
      iter.AddFile(file); 
      NFiles++; 
      file1 = file;
      SetInnerPadrows();
    }
    delete f;
  }
  cout << files << "\twith " << NFiles << " files" << endl; 
  if (! file1 ) return;
  TString output(Out);
  if (output == "") {
    output = file1;
#ifdef __PADCorrection__
    output.ReplaceAll(".root",".Pads.CT.root");
#else
    output.ReplaceAll(".root",".Pads.BTt.root");
#endif
  }
  cout << "Output for " << output << endl;
  const Int_t&       fNoRcHit                                 = iter("fNoRcHit");
  const Int_t&       fNoMcHit                                 = iter("fNoMcHit");
  const Int_t&       fAdcSum                                  = iter("fAdcSum");
#if 0
  const Float_t&     DVelWest                                 = iter("DVelWest");
  const Float_t&     DVelEast                                 = iter("DVelEast");
#endif  
  const Float_t&     Frequency                                = iter("Frequency");
  const Float_t*&    fMcHit_mLocalMomentum_mX1                = iter("fMcHit.mLocalMomentum.mX1");
  const Float_t*&    fMcHit_mLocalMomentum_mX2                = iter("fMcHit.mLocalMomentum.mX2");
  const Float_t*&    fMcHit_mLocalMomentum_mX3                = iter("fMcHit.mLocalMomentum.mX3");
  const Float_t*&    fMcHit_mPosition_mX3                     = iter("fMcHit.mPosition.mX3");
  //  const Float_t*&    fMcHit_mdE                               = iter("fMcHit.mdE");
  //  const Float_t*&    fMcHit_mdS                               = iter("fMcHit.mdS");
#if 1
  const Float_t*&    fMcHit_mTof                              = iter("fMcHit.mTof");
#endif
  const Long_t*&     fMcHit_mKey                              = iter("fMcHit.mKey");
  const Long_t*&     fMcHit_mVolumeId                         = iter("fMcHit.mVolumeId");
  //  const Float_t*&    fMcHit_mAdc                           = iter("fMcHit.mAdc");
  const Float_t*&    fMcHit_mMcl_x                            = iter("fMcHit.mMcl_x");
  const Float_t*&    fMcHit_mMcl_t                            = iter("fMcHit.mMcl_t");
#if 0
  //  const Int_t*&      fRcHit_mId                               = iter("fRcHit.mId");
  const Float_t*&    fRcHit_mPosition_mX3                     = iter("fRcHit.mPosition.mX3");
  const Float_t*&    fRcHit_mCharge                           = iter("fRcHit.mCharge");
#endif
  const UChar_t*&    fRcHit_mMinpad                           = iter("fRcHit.mMinpad");
  const UChar_t*&    fRcHit_mMaxpad                           = iter("fRcHit.mMaxpad");
  const UChar_t*&    fRcHit_mMintmbk                          = iter("fRcHit.mMintmbk");
  const UChar_t*&    fRcHit_mMaxtmbk                          = iter("fRcHit.mMaxtmbk");
  const Short_t*&    fRcHit_mMcl_x                            = iter("fRcHit.mMcl_x");
  const Short_t*&    fRcHit_mMcl_t                            = iter("fRcHit.mMcl_t");
  //  const UShort_t*&    fRcHit_mFlag                             = iter("fRcHit.mFlag");
  const Int_t*&   fRcHit_mIdTruth                          = iter("fRcHit.mIdTruth");
  const UShort_t*&   fRcHit_mQuality                          = iter("fRcHit.mQuality");
#if 0
  const Float_t*&    fRcTrack_fpx                             = iter("fRcTrack.fpx");
  const Float_t*&    fRcTrack_fpy                             = iter("fRcTrack.fpy");
#endif
  if (! fOut) fOut = new TFile(output,"recreate");
  fOut->cd();
#ifdef __PADCorrection__
  struct TpcPadCorrection_st {
    char HistName[32]; 
    char Type[8]; 
    short npads; 
    short InOut; 
    short N; 
    short R; 
    double a0; 
    double a1; 
    double a2; 
    double a3; 
    double a4; 
    double a5; 
    double a6; 
    double a7; 
    double a8; 
    double a9; 
    double prob; 
  };
  TpcPadCorrection_st row[28] = {
    //HistName           Type   n I  N R a0     a1           a2           a3           a4           a5           a6           a7           a8           a9           prob
    {"pad_RC_Innerp1_1","Tcheb",1,1,10,8,80110, 1.17459e-03,-2.78168e-02,-3.56898e-02, 1.69911e-02,-9.42231e-03,-1.34990e-03,-2.07366e-03,-7.10711e-04, 1.59502e-03, 0.00000e+00},
    {"pad_RC_Innerp1_2","Tcheb",1,1, 9,7,70109, 1.49321e-01,-1.44462e-03,-6.11248e-04, 4.43956e-04,-8.12956e-04, 8.59755e-05,-2.95338e-04, 1.22221e-04, 0.00000e+00, 5.91797e-03},
    {"pad_RC_Innerp2_1","Tcheb",2,1,10,8,80110,-1.76997e-03,-8.92412e-01, 5.87395e-01, 3.11457e-01,-9.07539e-01, 5.10610e-01,-2.49811e-01,-8.46685e-02, 3.71485e-02, 0.00000e+00},
    {"pad_RC_Innerp2_2","Tcheb",2,1, 9,7,70109, 6.74188e-01,-6.94448e-01, 2.20993e-02, 4.95669e-01,-6.28590e-01, 4.59344e-01,-2.12816e-01, 5.30122e-02, 0.00000e+00, 5.16669e-07},
    {"pad_RC_Innerp3_1","Tcheb",3,1,10,8,80110,-1.19325e-03, 1.70577e+00, 2.70571e+00, 1.93181e+00, 1.25002e+00, 6.78614e-01, 3.19551e-01, 1.15119e-01, 3.06700e-02, 3.36786e-03},
    {"pad_RC_Innerp3_2","Tcheb",3,1, 9,7,70109, 2.29093e-01, 1.82880e-01, 1.50656e-01, 9.39396e-02, 4.38449e-02, 1.56946e-02, 3.41103e-03, 5.14954e-04, 0.00000e+00, 3.37772e-03},
    {"pad_RC_Innerp4_1","Tcheb",4,1,10,8,80110,-4.35460e-07,-1.22723e-01, 1.21555e-01,-4.86069e-02, 5.23343e-02,-1.67169e-02, 2.66717e-02,-3.14092e-03, 1.21403e-02, 0.00000e+00},
    {"pad_RC_Innerp4_2","Tcheb",4,1, 9,7,70109, 1.36550e-01, 2.85916e-03,-1.03675e-03,-1.43759e-03, 8.27511e-04,-1.42359e-03, 4.27323e-04,-6.89917e-04, 0.00000e+00, 0.00000e+00},
    {"pad_RC_Innerp5_1","Tcheb",5,1, 3,8,80104, 1.33170e-03, 5.72400e-02, 4.39525e-02, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 7.13113e-02},
    {"pad_RC_Innerp5_2","Tcheb",5,1, 4,7,70105, 1.64072e-01, 1.04555e-02, 1.30864e-03, 1.40747e-03, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 3.29019e-02},
    {"pad_RC_Innerp6_1","Tcheb",6,1, 4,8,80105, 4.63435e-03, 6.51733e-02,-1.06345e-02, 4.01786e-02, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 1.70798e-02},
    {"pad_RC_Innerp6_2","Tcheb",6,1, 9,7,70109, 1.90653e-01,-1.47077e-02, 8.38526e-03,-4.73579e-03, 2.66308e-03,-2.05990e-03, 8.28226e-04,-4.47968e-04, 0.00000e+00, 0.00000e+00},
    {"pad_RC_Innerp7_1","Tcheb",7,1, 2,8,80103, 8.08995e-03,-6.01568e-02, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 1.07713e-01},
    {"pad_RC_Innerp7_2","Tcheb",7,1, 3,7,70104, 2.54596e-01, 2.36554e-02,-8.74826e-03, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 4.48575e-02},
    {"pad_RC_Outerp1_1","Tcheb",1,2,10,8,80110,-6.44149e-04,-5.72243e-03,-1.13195e-01,-3.81004e-02, 1.36959e-02, 4.62487e-03,-2.11467e-03,-2.80067e-03,-3.23696e-04, 0.00000e+00},
    {"pad_RC_Outerp1_2","Tcheb",1,2, 9,7,70109, 1.07642e-01, 1.29283e-03,-7.59827e-03,-1.56777e-03, 2.89895e-03, 8.03608e-04,-3.12548e-04,-2.23534e-04, 0.00000e+00, 0.00000e+00},
    {"pad_RC_Outerp2_1","Tcheb",2,2,10,8,80110,-4.25603e-03,-8.31655e-01, 9.82213e-01,-7.28482e-01, 5.32711e-01,-3.31683e-01, 1.90131e-01,-8.00700e-02, 3.16020e-02, 0.00000e+00},
    {"pad_RC_Outerp2_2","Tcheb",2,2, 9,7,70109, 1.59776e-01,-1.61840e-01, 1.25438e-01,-8.12072e-02, 4.53766e-02,-2.26046e-02, 8.89047e-03,-2.47026e-03, 0.00000e+00, 0.00000e+00},
    {"pad_RC_Outerp3_1","Tcheb",3,2,10,8,80110,-1.37390e-03, 6.87376e-01, 1.01860e+00, 6.61023e-01, 4.08960e-01, 2.20678e-01, 1.05842e-01, 3.86891e-02, 1.01776e-02, 0.00000e+00},
    {"pad_RC_Outerp3_2","Tcheb",3,2, 9,7,70109, 8.04674e-02,-2.32913e-03, 8.78259e-03, 5.42018e-03, 4.37133e-03, 2.13240e-03, 1.07526e-03, 3.81819e-04, 0.00000e+00, 0.00000e+00},
    {"pad_RC_Outerp4_1","Tcheb",4,2,10,8,80110,-4.63219e-05, 2.80074e-02,-7.63640e-02, 4.63928e-02,-6.40244e-02, 2.24043e-02,-3.37489e-02, 6.21444e-03,-1.33286e-02, 0.00000e+00},
    {"pad_RC_Outerp4_2","Tcheb",4,2, 9,7,70109, 1.33572e-01,-1.91998e-02, 3.89001e-03,-2.88304e-03, 2.61475e-03,-3.51715e-03, 1.39551e-03,-1.87114e-03, 0.00000e+00, 0.00000e+00},
    {"pad_RC_Outerp5_1","Tcheb",5,2,10,8,80110, 4.02456e-03,-3.71741e-01,-3.72257e-01,-8.72703e-02, 1.82812e-02, 2.80848e-02, 1.77154e-02, 6.30182e-03, 2.08673e-03, 0.00000e+00},
    {"pad_RC_Outerp5_2","Tcheb",5,2, 9,7,70109, 2.22069e-01, 6.72734e-02, 2.44592e-02, 4.56191e-03,-2.86917e-03, 5.00334e-04, 1.55730e-03, 1.66587e-03, 0.00000e+00, 0.00000e+00},
    {"pad_RC_Outerp6_1","Tcheb",6,2, 7,8,80108, 6.46415e-03, 2.41423e-01,-2.38816e-01, 7.73091e-02,-8.32941e-02, 1.40819e-02,-2.79615e-02, 0.00000e+00, 0.00000e+00, 8.34810e-02},
    {"pad_RC_Outerp6_2","Tcheb",6,2, 9,7,70109, 2.25118e-01,-4.57903e-02, 2.79346e-02,-1.83736e-02, 1.27081e-02,-7.70491e-03, 4.24942e-03,-1.61331e-03, 0.00000e+00, 0.00000e+00},
    {"pad_RC_Outerp7_1","Tcheb",7,2, 2,8,80103, 5.99832e-03,-1.22963e-01, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 1.31096e-02},
    {"pad_RC_Outerp7_2","Tcheb",7,2, 3,7,70104, 2.96445e-01, 2.68613e-02,-7.71130e-03, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 0.00000e+00, 3.55054e-01}
  };
  Int_t nrows = 28;
  TF1 *fFunc[28]; memset(fFunc,0,sizeof(fFunc));
  for (Int_t i = 0; i < nrows; i++) {
    Short_t io = row[i].InOut;
    Short_t np = row[i].npads;
    Short_t MuOrSigma  = -1;
    if (row[i].R == 8) MuOrSigma = 0;
    if (row[i].R == 7) MuOrSigma = 1;
    if ((io < 1 || io > 2) ||
	(np < 1 || np > 7) ||
	(MuOrSigma < 0 || MuOrSigma > 1)) continue;
    Int_t  indx = 2*(7*(io-1) + np-1)+MuOrSigma;
    fFunc[indx] = TPolynomial::MakePoly(Form("Tcheb_%i_%i",np,io),row[i].N-1,row[i].R);
    fFunc[indx]->SetParameters(&row[i].a0);
  }
#endif /*  __PADCorrection__ */
  //  pad/time    MC/RC   In/Out Vars    no. pads  
  enum {NPT = 2, NK = 2, IO = 2, NV = 5, NP = 7, NTbPerBin = 3};
  //           pt   k   i   v   p
  TH2F *inout[NPT][NK][IO][NV][NP];
  memset(inout, 0, sizeof(inout));
  const Char_t *padtime[NPT] = {"pad","tmbk"}; // pt
  const Char_t *RMC[NK]      = {"MC","RC"};        // k
  const Char_t *ion[IO]      = {"Inner","Outer"};  // i  
  const Char_t *varT[NV]     = {"X","phiL","eta","zL","AdcL"}; //v
#ifdef __PRINCIPLE__
  struct Princ_t {
    Double_t dX; // dPad or dTime
    Double_t Pad;
    Double_t TB;
    Double_t phiL;
    Double_t eta;
    Double_t zL;
    Double_t AdcL;
  };
  Princ_t xP;
  Int_t N = sizeof(Princ_t)/sizeof(Double_t);
  TPrincipal *princ[2];
  princ[0] = new TPrincipal(N,"N"); princ[0]->SetName("PadP");
  princ[1] = new TPrincipal(N,"N"); princ[1]->SetName("TimeP");
#endif
  TString Name, Title;
  const Double_t padMax = 2.0;
  for (Int_t ipt = 0; ipt < NPT; ipt++) { // pad, time
    for (Int_t v = 0; v < NV; v++) {// vars
      Int_t k1 = 1;
      Int_t k2 = NK;
      if (v == 0) k1 = 0; // MC only for pad and tmbk
      for (Int_t k = k1; k < k2; k++) {// MC RC
	Int_t l = (k+1)%2;
	for (Int_t io = 0; io < IO; io++) {
	  for (Int_t npads = 1; npads <= NP; npads++) {
	    Name  = Form("%s_%s_%s_%sp%i",                   padtime[ipt],varT[v],RMC[k],ion[io],npads);
	    Title = Form("%s_{%s} - %s_{%s} versus %s_{%s} ",padtime[ipt],RMC[l],padtime[ipt],RMC[k],varT[v],RMC[k]);
	    if (npads > 1 && npads <= NP) 	
	      if (ipt == 0) Title += Form("for %s sectors for clusters with %i %ss",ion[io],npads,padtime[ipt]);
	      else          Title += Form("for %s sectors for clusters with [%i,%i] %ss",ion[io],NTbPerBin*(npads-1)+1,NTbPerBin*npads,padtime[ipt]);
	    else {
	      if (npads ==  1) Title += Form("for %s sectors for All clusters",ion[io]);
	      if (npads == NP) {
		if (ipt == 0) Title += Form("for %s sectors for clusters with >=%i %ss",ion[io],NP,padtime[ipt]);
		else          Title += Form("for %s sectors for clusters with >=%i %ss",ion[io],NTbPerBin*(NP-1)+1,padtime[ipt]);
	      }
	    }
	    if (v == 0) {
	      inout[ipt][k][io][v][npads-1] =  new TH2F(Name,Title, 64, -0.5, 0.5, 512, -padMax, padMax);
	      inout[ipt][k][io][v][npads-1]->SetXTitle(Form("%s%s_{%s}",padtime[ipt],varT[v],RMC[k]));
	    } else if (v == 1) {
	      inout[ipt][k][io][v][npads-1] =  new TH2F(Name,Title, 90, -TMath::Pi()/2, TMath::Pi()/2, 512, -padMax, padMax);
	      inout[ipt][k][io][v][npads-1]->SetXTitle(Form("phiL_{%s}",RMC[k]));
	    } else if (v == 2) {
	      inout[ipt][k][io][v][npads-1] =  new TH2F(Name,Title, 90, -TMath::Pi()/2, TMath::Pi()/2, 512, -padMax, padMax);
	      inout[ipt][k][io][v][npads-1]->SetXTitle(Form("eta_{%s}",RMC[k]));
	    } else if (v == 3) {
	      inout[ipt][k][io][v][npads-1] =  new TH2F(Name,Title, 230, -10, 220, 512, -padMax, padMax);
	      inout[ipt][k][io][v][npads-1]->SetXTitle(Form("zL_{%s}",RMC[k]));
	    } else if (v == 4) {
	      inout[ipt][k][io][v][npads-1] =  new TH2F(Name,Title, 70, 2, 9, 512, -padMax, padMax);
	      inout[ipt][k][io][v][npads-1]->SetXTitle(Form("Log(AdcSum)_{%s}",RMC[k]));
	    }
	    inout[ipt][k][io][v][npads-1]->SetMarkerStyle(20);
	    inout[ipt][k][io][v][npads-1]->SetMarkerColor(npads);
	    inout[ipt][k][io][v][npads-1]->SetYTitle(Form("%s_{%s} - %s_{%s}",padtime[ipt],RMC[l],padtime[ipt],RMC[k]));
	  }
	}
      }
    }
  }
  while (iter.Next()) {
    if (fNoRcHit != 1) continue;
    if (fNoMcHit != 1 && fNoMcHit != 3) continue;
    if (fAdcSum <= 0) continue;
    Double_t AdcL = TMath::Log(fAdcSum);
    Int_t k1 = 0; 
    Int_t k2 = fNoMcHit;
    Int_t l = 0;
    for (Int_t k = k1; k < k2; k++) { 
      //    Double_t zRc = fRcHit_mPosition_mX3[0];
      if (fMcHit_mVolumeId[k] > 10000) continue; 
      //???????      if (fRcHit_mFlag[l]) continue;
      if (fMcHit_mKey[k] != fRcHit_mIdTruth[l]) continue;
      if (fRcHit_mQuality[l] < 95) continue;
      Int_t row = fMcHit_mVolumeId[k]%100;
      Int_t io = 0;
      if (row > NoInnerRows) io = 1;
      Int_t npads = fRcHit_mMaxpad[l] + fRcHit_mMinpad[l] + 1;
      if (npads > NP) npads = NP;
      Int_t pad = TMath::Nint(fMcHit_mMcl_x[k]);
      Double_t xMC = fMcHit_mMcl_x[k] - pad;
      Double_t xRC = (fRcHit_mMcl_x[l])/64. - pad;
      Double_t dx = xRC - xMC;

      Int_t ntmbks = (fRcHit_mMaxtmbk[l] + fRcHit_mMintmbk[l] + 1)/NTbPerBin + 1;
      if (ntmbks > NP) ntmbks = NP;
      //      Int_t tmbk   = TMath::Nint(fMcHit_mMcl_t[k]);
      Double_t zMC = fMcHit_mMcl_t[k] + 1e6*Frequency*(fMcHit_mTof[k] + 3*55.e-9); // - tmbk
      Double_t zRC = (fRcHit_mMcl_t[l])/64.;// - tmbk;
      Double_t dz  = zRC - zMC;
      Double_t phiL = TMath::DegToRad()*LocalPhi(fMcHit_mLocalMomentum_mX1[k],fMcHit_mLocalMomentum_mX2[k],fMcHit_mVolumeId[k]);
      if (phiL < -TMath::Pi()/2) phiL += TMath::Pi();
      if (phiL >  TMath::Pi()/2) phiL -= TMath::Pi();
      Double_t eta = Eta(fMcHit_mLocalMomentum_mX1[k],fMcHit_mLocalMomentum_mX2[k],fMcHit_mLocalMomentum_mX3[k]);
      Double_t ZL   = fMcHit_mPosition_mX3[k];
      if (TMath::Nint(fMcHit_mVolumeId[k]/100.)%100>12) {
	eta = -eta;
	ZL   = - ZL;
      }
      Double_t dxz[2] = {// [ipt]
	dx,  dz  // xRC - xMC
      };
      Int_t n[2][2]   = { //[ipt][np]
	{0, npads-1}, 
	{0,ntmbks-1}
      };
      Double_t vars[NV] = {
	0, phiL, eta, ZL, AdcL
      };
      Double_t XZ[2][2] = {// [ipt][k]
	{xMC,xRC},
	{zMC,zRC}
      };
#ifdef __PRINCIPLE__
      xP.Pad = XZ[0][1] - TMath::Nint(XZ[0][1]);
      xP.TB  = XZ[1][1] - TMath::Nint(XZ[1][1]);
      xP.phiL = phiL;
      xP.eta = eta;
      xP.zL   = ZL;
      xP.AdcL = AdcL;
#endif
      for (Int_t ipt = 0; ipt < NPT; ipt++) { // pad, time
#ifdef __PRINCIPLE__
	xP.dX  = dxz[ipt];
	princ[ipt]->AddRow(&xP.dX);
#endif
	for (Int_t v = 0; v < NV; v++) {// vars Int_t k1 = 1;
	  Int_t McRc2 = NK;
	  Int_t McRc1 =  1;
	  if (v == 0) McRc1 = 0; // MC only for pad and tmbk
	  for (Int_t McRc = McRc1; McRc < McRc2; McRc++) {// MC RC
	    if (v == 0) vars[0] = XZ[ipt][McRc] - TMath::Nint(XZ[ipt][McRc]);
	    for (Int_t np = 0; np < 2; np++) {
	      TH2 *hist = inout[ipt][McRc][io][v][n[ipt][np]];// inout[ipt][McRc][io][v][n[ipt][i]];
	      if (hist) {
		if (McRc == 0) hist->Fill(vars[v],-dxz[ipt]);
		else           hist->Fill(vars[v], dxz[ipt]);
	      }
	    }
	  }
	}
      }
#ifdef __PADCorrection__
      Int_t ioC = io + 1;
      Int_t np = 1;
      Int_t MuOrSigma = 0;
      Int_t indx = 2*(7*(ioC-1) + np-1)+MuOrSigma;
      Double_t xRCcor = xRC + fFunc[indx]->Eval(xRC);
      dx =  xMC - xRCcor;
      inout[1][io][4][0]->Fill(xRC, dx);
      if (npads > 1) {
	Int_t np = npads;
	indx = 2*(7*(ioC-1) + np-1)+MuOrSigma;
	xRCcor = xRC + fFunc[indx]->Eval(xRC);
	dx =  xMC - xRCcor;
	inout[1][io][4][npads-1]->Fill(xRC, dx);
      }
#endif /*  __PADCorrection__ */
    }
  }
#ifdef __PRINCIPLE__
  for (Int_t ipt = 0; ipt < 2; ipt++) {
    princ[ipt]->MakePrincipals(); // Do the actual analysis
    princ[ipt]->Print();	      // Print out the result on
    //    princ[ipt]->Test();           // Test the PCA 
    princ[ipt]->MakeHistograms(padtime[ipt]); // Make some histograms of the orginal, principal, residue, etc data 
    princ[ipt]->MakeCode(padtime[ipt]);       // Make two functions to map between feature and pattern space 
    princ[ipt]->Write();
  }
#endif
  fOut->Write();
}
//________________________________________________________________________________
Double_t fun2(Double_t *x, Double_t *par) {
  /*
    innerM
    FCN=49665 FROM MINOS     STATUS=UNCHANGED       0 CALLS         387 TOTAL
    EDM=3.82662e-11    STRATEGY= 1      ERROR MATRIX ACCURATE 
    EXT PARAMETER                                   STEP         FIRST   
    NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
    1  offset       6.51171e+01   9.45741e-02  -5.95917e-05   1.25163e-04
    2  z            3.50041e-01   5.17444e-03   6.60756e-06   8.73189e-03
    3  zxAdcL      -8.60050e-02   9.37586e-04  -1.19618e-06   5.04555e-02
    4  adcL         1.11909e+00   1.87552e-04   2.80116e-08   2.56762e-01
    5  adcL2       -1.31037e-02   2.45839e-05   2.45839e-05   1.88516e+00
    6  adcL3        0.00000e+00     fixed    
    
    f2->SetParameters(6.52745e+01,  3.29340e-01, -8.19982e-02,  1.11882e+00, -1.30767e-02,  0.00000e+00)
    //                    9.54303e-02,  5.28211e-03,  9.59268e-04,  1.90611e-04,  2.50096e-05,   fixed    
    .L TpcT.C+
    TF2 *f2 = new TF2("f2",fun2,3.5,8.5,-210,210, 6);
    f2->SetParameters(6.52745e+01,  3.29340e-01, -8.19982e-02,  1.11882e+00, -1.30767e-02,  0.00000e+00)
    f2->SetParNames("offset","z","zxAdcL","adcL","adcL2","adcL3");
    f2->FixParameter(5,0);
    innerM->Fit("f2","er");
    innerM->Draw("colz");
    f2->Draw("cont1 same");
  */
  Double_t adcL   = x[0];
  Double_t Z      = TMath::Abs(x[1]);
  Double_t offset = par[0] + Z*par[1] + Z*adcL*par[2];
  Double_t adc    = adcL*(par[3] + adcL*par[4]) + par[5];
  Double_t adcS   = offset + TMath::Exp(adc);
  Double_t result = 0;
  if (adcS > 0) result = TMath::Log(adcS);
  return result;
}
//________________________________________________________________________________
Double_t fun2r(Double_t *x, Double_t *par) {
  return fun2(x,par) - x[0];
}
//________________________________________________________________________________
Double_t fun1(Double_t *x, Double_t *par) {
  /*
    Int_t i;
    TAxis *y = innerM->GetYaxis();
    TF1 *f1 = new TF1("f1",fun1,3,9,7);
    f1->SetParNames("offset","z","zxAdcL","adcL","adcL2","adcL3","Z");
    Double_t params[7];
    f2->GetParameters(params);
    i = 111; TH1D *h = innerM->ProjectionX(Form("i%i",i),i,i);  params[6] = y->GetBinCenter(i); 
    h->SetTitle(Form("%s at z = %5.2f",innerM->GetTitle(),params[6]));
    h->SetStats(0); h->Draw();
    f1->SetParameters(params);  f1->Draw("same");
  */
  Double_t xz[2] = {x[0], par[6]};
  return fun2(xz,par);
}
//________________________________________________________________________________
Double_t fun1r(Double_t *x, Double_t *par) {
  Double_t xz[2] = {x[0], par[6]};
  return fun2r(xz,par);
}
//________________________________________________________________________________
void DrawF1(const Char_t *name = "innerM", Int_t i = 111, const Char_t *opt="") {
  TProfile2D *hist = (TProfile2D *) gDirectory->Get(name);
  if (! hist) return;
  TF2 *f2 = (TF2 *) hist->GetListOfFunctions()->FindObject("f2");
  if (! f2) return;
  TAxis *y = hist->GetYaxis();
  TF1 *f1 = new TF1("f1",fun1,3,9,7);
  f1->SetParNames("offset","z","zxAdcL","adcL","adcL2","scale","Z");
  Double_t params[7];
  f2->GetParameters(params);
  TH1D *h = hist->ProjectionX(Form("i%i",i),i,i);  
  params[6] = y->GetBinCenter(i); 
  h->SetTitle(Form("%s at z = %5.2f",hist->GetTitle(),params[6]));
  h->SetStats(0); h->Draw(opt);
  f1->SetParameters(params);  f1->Draw("same");
}
//________________________________________________________________________________
void DrawF1r(const Char_t *name = "innerM", Int_t i = 111, const Char_t *opt="") {
  TProfile2D *hist = (TProfile2D *) gDirectory->Get(name);
  if (! hist) return;
  TF2 *f2 = (TF2 *) hist->GetListOfFunctions()->FindObject("f2r");
  if (! f2) return;
  TAxis *y = hist->GetYaxis();
  TF1 *f1 = new TF1("f1r",fun1r,3,9,7);
  f1->SetParNames("offset","z","zxAdcL","adcL","adcL2","scale","Z");
  Double_t params[7];
  f2->GetParameters(params);
  TH1D *h = hist->ProjectionX(Form("i%i",i),i,i);  
  params[6] = y->GetBinCenter(i); 
  h->SetTitle(Form("%s at z = %5.2f",hist->GetTitle(),params[6]));
  h->SetStats(0); h->Draw(opt);
  f1->SetParameters(params);  f1->Draw("same");
}
//________________________________________________________________________________
TProfile2D *CleanAdc(const Char_t *name) {
  TProfile2D *hist = (TProfile2D *) gDirectory->Get(name);
  if (! hist) return 0;
  TProfile2D *histM = new TProfile2D(*hist);
  histM->SetName(Form("%sM",hist->GetName()));
  histM->SetXTitle("log(ADC)");
  histM->SetYTitle("Z (cm)");
  Int_t nx = hist->GetNbinsX();
  Int_t ny = hist->GetNbinsY();
#if 0
  TNtuple *FitP = new TNtuple(Form("FitP%s",hist->GetName()),"Fit results","x:y:entries:mean:rms");
#endif
  Float_t xx[5];
  TAxis *ax = hist->GetXaxis();
  TAxis *ay = hist->GetYaxis();
  for (Int_t i = 1; i <= nx; i++) {
    xx[0] = ax->GetBinCenter(i);
    for (Int_t j = 1; j <= ny; j++) {
      Int_t bin = hist->GetBin(i,j);
      xx[1] = ay->GetBinCenter(j);
      xx[2] = hist->GetBinEntries(bin);
      Double_t er = hist->GetBinError(bin);
      if (xx[2] <= 20.0 || er < 1e-3) {
	histM->SetBinEntries(bin,0.);
	histM->SetBinContent(bin,0.);
	histM->SetBinError(bin,0.);
	continue;
      }
      xx[3] = hist->GetBinContent(bin);
      xx[4] = hist->GetBinError(bin);
#if 0
      FitP->Fill(xx);
#endif
    }
  }
  return histM;
}
//________________________________________________________________________________
void AdcCorrections() {
  /*
root.exe [7] AdcCorrections()
Inner
 PARAMETER NUMBER   6 NOT A VARIABLE. IGNORED.
 THERE ARE NO MINOS ERRORS TO CALCULATE.
 FCN=49665 FROM MINOS     STATUS=UNCHANGED       0 CALLS         387 TOTAL
                     EDM=3.82662e-11    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  offset       6.51171e+01   9.45741e-02  -5.95917e-05   1.25163e-04
   2  z            3.50041e-01   5.17444e-03   6.60756e-06   8.73189e-03
   3  zxAdcL      -8.60050e-02   9.37586e-04  -1.19618e-06   5.04555e-02
   4  adcL         1.11909e+00   1.87552e-04   2.80116e-08   2.56762e-01
   5  adcL2       -1.31037e-02   2.45839e-05   2.45839e-05   1.88516e+00
   6  adcL3        0.00000e+00     fixed    

Outer
 PARAMETER NUMBER   6 NOT A VARIABLE. IGNORED.
 THERE ARE NO MINOS ERRORS TO CALCULATE.
 FCN=26465.4 FROM MINOS     STATUS=UNCHANGED       0 CALLS         417 TOTAL
                     EDM=1.04284e-10    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  offset       4.73062e+01   1.18038e-01  -6.21885e-05   2.75367e-04
   2  z            5.90289e-02   4.19343e-03   3.29939e-06   2.92729e-02
   3  zxAdcL      -2.07588e-02   8.13723e-04  -6.46022e-07   1.60206e-01
   4  adcL         1.12948e+00   3.57744e-04   3.51452e-08   2.74937e-01
   5  adcL2       -1.46575e-02   5.06778e-05   5.06778e-05   3.90317e+00
   6  adcL3        0.00000e+00     fixed    
 Font metric w =  133  h =  21 points= 9 pixels= 12 QFont( "Arial,9,12,0,80,0,0,0,0,0" ) 
  */
  const Char_t *histNames[4] = {"inner","outer","innerR","outerR"};
  const Char_t *parNames[7] = {"offset","z","zxAdcL","adcL","adcL2","scale"};
  TF2 *f2;
  TF1 *f1;
  TCanvas *c1, *c2;
  for (Int_t k = 0; k < 1; k++) {
    if (k == 0) {
      f2 = new TF2("f2",fun2,3.5,8.5,-210,210, 6);
      c1 = new TCanvas("c1","Adc Corrections");
      c2 = new TCanvas("c2","Adc Corrections Projections");
    }
    else {
      f2 = new TF2("f2r",fun2r,3.5,8.5,-210,210, 6);
      c1 = new TCanvas("c1r","Relative Adc Corrections");
      c2 = new TCanvas("c2r","Relative Adc Corrections Projections");
    }
    f2->SetParNames(parNames[0],parNames[1],parNames[2],parNames[3],parNames[4],parNames[5]);
    c1->Divide(2,1);
    c2->Divide(2,1);
    for (Int_t i = 0; i < 2; i++) {
      TProfile2D *hist = (TProfile2D *) gDirectory->Get(histNames[2*k+i]);
      if (! hist) continue;
      if (k == 0) hist->SetMinimum(3);
      else        hist->SetMinimum(-0.5);
      //    Int_t nx = hist->GetNbinsX();
      Int_t ny = hist->GetNbinsY();
      TProfile2D *histM = (TProfile2D *) gDirectory->Get(Form("%sM",histNames[i]));
      if (! histM) histM = CleanAdc(histNames[i]);
      c1->cd(i+1);
      f2->SetParameters(6.52745e+01,  0, 0,  1.11882e+00, -1.30767e-02,  0.00000e+00);
      f2->FixParameter(1,0);
      f2->FixParameter(2,0);
      f2->FixParameter(4,0);
      f2->FixParameter(5,0);
      cout << "Fit " << histM->GetName() << "\t" << histM->GetTitle() << endl;
      histM->Fit(f2,"er");
      f2->ReleaseParameter(1);
      //      f2->SetParLimits(1,0,100);
      f2->ReleaseParameter(2);
      f2->ReleaseParameter(4);
      cout << "Fit " << histM->GetName() << "\t" << histM->GetTitle() << endl;
      histM->Fit(f2,"er");
      histM->Draw("colz");
      f2->Draw("cont1 same");
      c2->cd(i+1);
      TLegend *leg = new TLegend(0.6,0.1,0.9,0.4);
      TAxis *y = histM->GetYaxis();
      Double_t params[7];
      f2->GetParameters(params);
      Int_t color = 0;
      for (Int_t j = 5; j <= ny; j += 40) {
	TH1D *h = histM->ProjectionX(Form("%sj%i",histM->GetName(),j),j,j);  
	params[6] = y->GetBinCenter(j); 
	h->SetTitle(Form("%s at z = %5.2f",histM->GetTitle(),params[6]));
	cout << h->GetTitle() << endl;
	color++;
	h->SetMarkerStyle(20);
	h->SetMarkerColor(color);
	h->SetStats(0); 
	if (k == 0)  f1 = new TF1(Form("%s%i_%i",f2->GetName(),i,j),fun1,3,9,7);
	else         f1 = new TF1(Form("%s%i_%i",f2->GetName(),i,j),fun1r,3,9,7);
	f1->SetParNames(parNames[0],parNames[1],parNames[2],parNames[3],parNames[4],parNames[5],parNames[6]);
	f1->SetParameters(params); 
	f1->SetLineColor(color);
	h->GetListOfFunctions()->Add(f1);
	if (color == 1) h->Draw();
	else            h->Draw("same");
	f1->Draw("same");
	leg->AddEntry(h,Form("z = %5.2f",params[6]));
      }
      leg->Draw();
    }
  }
}
//________________________________________________________________________________
void MakeFunctions() {
  Double_t timeBinMin = -0.5;
  Double_t timeBinMax = 10.5;
  if (! mShaperResponse) 
    mShaperResponse = new TF1("ShaperResponse",ShaperFunc,timeBinMin,timeBinMax,5);  
  //  Double_t mTau                       = tau; //1.e-9*gStTpcDb->Electronics()->tau();// s 
  //  mShaperResponse->SetParameters(mTau,mTimeBinWidth,FWHM,   0,     1);
  mShaperResponse->SetParameters(0.5,        1.,         FWHM,   0,     1);
  mShaperResponse->SetParNames("#tau","tbWidth",        "FWHM","t0","Norm");
  Double_t params[12];
  params[0] = 2.85e-01;//gStTpcDb->PadPlaneGeometry()->innerSectorPadWidth();                     // w = width of pad       
  params[1] = 0.2;//gStTpcDb->WirePlaneGeometry()->innerSectorAnodeWirePadPlaneSeparation(); // h = Anode-Cathode gap   
  params[2] = 0.4;//gStTpcDb->WirePlaneGeometry()->anodeWirePitch();                         // s = wire spacing       
  //    params[3] = anodeWireRadius;                                                         // a = Anode wire radius  
  params[4] = 0;
  params[5] = 0;
  Double_t xmin = 0; 
  Double_t xmax = 5.0;//4.5*gStTpcDb->PadPlaneGeometry()->innerSectorPadWidth();// 4.5 
  if (! mPadResponseFunctionInner)
    mPadResponseFunctionInner = new TF1("PadResponseFunctionInner",
					PadResponseFunc,xmin,xmax,6); 
  params[3] =  K3IP;  
  params[4] =  0; // CrossTalk
  params[5] = innerSectorPadPitch;
  mPadResponseFunctionInner->SetParameters(params);
  mPadResponseFunctionInner->SetParNames("innerSectorPadWidth","innerSectorAnodeWirePadPlaneSeparation",
					 "K3IP","anodeWirePitch",
					 "CrossTalk","innerSectorPadPitch");
  //      mPadResponseFunctionInner->Save(xmin,xmax,0,0,0,0);
  xmax = 5.0;//5*gStTpcDb->PadPlaneGeometry()->innerSectorPadLength(); // 1.42
  if (! mChargeFractionInner) 
    mChargeFractionInner = new TF1("ChargeFractionInner",
				   PadResponseFunc,xmin,xmax,5);
  params[0] = 1.15;//gStTpcDb->PadPlaneGeometry()->innerSectorPadLength();
  params[3] = K3OR;
  params[4] =  0; // CrossTalk
  mChargeFractionInner->SetParameters(params);
  mChargeFractionInner->SetParNames("innerSectorPadLength","innerSectorAnodeWirePadPlaneSeparation",
				    "anodeWirePitch","K3OR",
				    "CrossTalk","innerSectorPadPitch");
  //  mChargeFractionInner->Save(xmin,xmax,0,0,0,0);
  xmax = 5;//5.*gStTpcDb->PadPlaneGeometry()->outerSectorPadWidth(); // 3.
  if (! mPadResponseFunctionOuter) 
    mPadResponseFunctionOuter = new TF1("PadResponseFunctionOuter",
					PadResponseFunc,xmin,xmax,6); 
  params[0] = 6.2e-01;//gStTpcDb->PadPlaneGeometry()->outerSectorPadWidth();                    // w = width of pad       
  params[1] = 0.4;//gStTpcDb->WirePlaneGeometry()->outerSectorAnodeWirePadPlaneSeparation(); // h = Anode-Cathode gap   
  params[2] = 0.4;//gStTpcDb->WirePlaneGeometry()->anodeWirePitch();                         // s = wire spacing       
  //      params[3] = gStTpcDb->WirePlaneGeometry()->anodeWireRadius();                        // a = Anode wire radius  
  params[3] = K3OP;    // K3 from E.Mathieson, Fig. 5.3b (pads) for a/s = 2.5e-3 and h/s = 1
  params[4] =  0; // CrossTalk
  params[5] = outerSectorPadPitch;
  mPadResponseFunctionOuter->SetParameters(params);
  mPadResponseFunctionOuter->SetParNames("outerSectorPadWidth","outerSectorAnodeWirePadPlaneSeparation",
					 "anodeWirePitch","K3OP",
					 "CrossTalk","outerSectorPadPitch");
  //    mPadResponseFunctionOuter->Save(xmin,xmax,0,0,0,0);
  xmax = 5;//5*gStTpcDb->PadPlaneGeometry()->outerSectorPadLength(); // 1.26
  if (! mChargeFractionOuter) 
    mChargeFractionOuter = new TF1("ChargeFractionOuter",
				   PadResponseFunc,xmin,xmax,5);
  params[0] = 1.95; //gStTpcDb->PadPlaneGeometry()->outerSectorPadLength();
  params[3] =  K3OR;    // K3 from E.Mathieson, Fig. 5.3a (row) for a/s = 2.5e-3 and h/s = 1.0
  params[4] =  0; // CrossTalk
  mChargeFractionOuter->SetParameters(params);
  mChargeFractionOuter->SetParNames("outerSectorPadLength","outerSectorAnodeWirePadPlaneSeparation",
				    "anodeWirePitch","K3OR",
				    "CrossTalk","outerSectorPadPitch");
  //  mChargeFractionOuter->Save(xmin,xmax,0,0,0,0);
  if (! mConvolution) 
    mConvolution = new TF1("Convolution",ConvolutionF,-8,8,19);
  mConvolution->SetParName(0,"icase"); mConvolution->FixParameter(0,0);
  mConvolution->SetParName(1,"Area");  mConvolution->FixParameter(1,1); mConvolution->SetParLimits(1,0,1e5);
  mConvolution->SetParName(2,"#sigma^{2}");  mConvolution->SetParLimits(2,0,10);
  mConvolution->SetParName(3,"K3I");   mConvolution->FixParameter(3,K3IP);
  mConvolution->SetParName(4,"K3O");   mConvolution->FixParameter(4,K3OP);
  mConvolution->SetParName(5,"Shift"); mConvolution->FixParameter(5,0);
  mConvolution->SetParName(6,"Noise"); mConvolution->FixParameter(6,0);
  mConvolution->SetParName(7,"FWHM"); mConvolution->FixParameter(7,FWHM);
  mConvolution->SetParName(8,"#tau"); mConvolution->FixParameter(8,1);// tau);
  mConvolution->SetParName(9,"CrossTalk"); mConvolution->FixParameter(9,0);
  mConvolution->SetParName(10,"frac"); mConvolution->FixParameter(10,0);  
  mConvolution->SetParName(11,"#Sigma"); mConvolution->FixParameter(11,0); 
  mConvolution->SetParName(12,"anodeWirePitch"); mConvolution->FixParameter(12,0.4); 
  mConvolution->SetParName(13,"innerSectorAnodeWirePadPlaneSeparation"); mConvolution->FixParameter(13,0.2); 
  mConvolution->SetParName(14,"innerSectorPadWidth"); mConvolution->FixParameter(14,2.85e-01); 
  mConvolution->SetParName(15,"innerSectorPadPitch"); mConvolution->FixParameter(15,innerSectorPadPitch);
  
  mConvolution->SetParName(16,"outerSectorAnodeWirePadPlaneSeparation"); mConvolution->FixParameter(16,0.4); 
  mConvolution->SetParName(17,"outerSectorPadWidth"); mConvolution->FixParameter(17,6.2e-01); 
  mConvolution->SetParName(18,"outerSectorPadPitch"); mConvolution->FixParameter(18,outerSectorPadPitch);
}
//________________________________________________________________________________
void FitSlices(const Char_t *name="OuterPadRc", const Char_t *opt="K3", Int_t iy=0) {
  TDirectory *fIn = gDirectory;
  if (! fIn) return;
  TProfile2D *h = (TProfile2D *) fIn->Get(name);
  if (! h) return;
  TString Opt(opt);
  Int_t nx = h->GetNbinsX();
  Int_t ny = h->GetNbinsY();
  TString Name(h->GetName());
  Name += opt;
  if (! h) return;
  TF1 *gd = 0;
  if (Name.Contains("Pad",TString::kIgnoreCase)) {
    if (Name.Contains("Inner",TString::kIgnoreCase)) 
      gd = new TF1("gdI",Form("([0]*[0]+[1]*[1]*(209.3-abs(x)))/(%f*%f)",pPI,pPI),-210,210); // to Ground wires
    if (Name.Contains("Outer",TString::kIgnoreCase)) 
      gd = new TF1("gdO",Form("([0]*[0]+[1]*[1]*(209.3-abs(x)))/(%f*%f)",pPO,pPO),-210,210);
    if (gd) {
      gd->SetParName(0,"#sigma_{C}");
      gd->SetParName(1,"#sigma_{D}");
      gd->SetParLimits(0,0,10.);
      gd->SetParLimits(1,0,10.);
    }
  }
  if (Name.Contains("Time",TString::kIgnoreCase)) {
    if (Name.Contains("Outer",TString::kIgnoreCase)) 
      gd = new TF1("gdOT",Form("([0]*[0]+[1]*[1]*(209.3-abs(x)) + %f*x*x)/(%f*%f)",lO*lO/(rO*rO)/12.,timeBin,timeBin),-210,210);
    else 
      gd = new TF1("gdIT",Form("([0]*[0]+[1]*[1]*(209.3-abs(x)) + %f*x*x)/(%f*%f)",lI*lI/(rI*rI)/12.,timeBin,timeBin),-210,210);
    if (gd) {
      gd->SetParName(0,"#sigma_{C}");
      gd->SetParName(1,"#sigma_{D}");
      gd->SetParLimits(0,0,10.);
      gd->SetParLimits(1,0,10.);
      gd->SetParName(2,"#sigma_{B}");
    }
  }
  if (! mConvolution) MakeFunctions();
  TF1 *ga = mConvolution;
  // default parameters
  if (Opt.Contains("SigmaSQ",TString::kIgnoreCase)) {
    ga->FixParameter(0,3);
    if (Name.Contains("Outer",TString::kIgnoreCase))         ga->FixParameter(0,4);
    ga->SetParameter(1,1.);
    ga->SetParameter(2,1.); ga->SetParLimits(2,0,10);
    ga->FixParameter(3,-1);
    ga->FixParameter(4,-1);
    ga->FixParameter(5,0);
    ga->SetParameter(6,0);
    ga->FixParameter(7,0);
    ga->FixParameter(8,0);
    ga->FixParameter(9,0);
    ga->FixParameter(10,0);
    ga->FixParameter(11,0);
  } else {
    if (Name.Contains("Pad",TString::kIgnoreCase)) 
      if (Name.Contains("Inner",TString::kIgnoreCase))         ga->FixParameter(0,0);
      else  {if (Name.Contains("Outer", TString::kIgnoreCase)) ga->FixParameter(0,1);}
    else                                                       ga->FixParameter(0,2); // Time
    ga->FixParameter(2,0);
    if (Opt.Contains("Conv",TString::kIgnoreCase)) {
      ga->ReleaseParameter(2); ga->SetParLimits(2,0,10); ga->SetParameter(2,1e-5); //#sigma^{2}
    }
    ga->FixParameter(3,K3IP); 
    ga->FixParameter(4,K3OP);
    ga->FixParameter(5,0); // shift
    ga->FixParameter(6,0); // noise
    ga->FixParameter(7,FWHM);  
    ga->FixParameter(8,1);//tau);
    if (Name.Contains("Inner",TString::kIgnoreCase)) ga->FixParameter(9,CrossTalkInner); 
    else                                             ga->FixParameter(9,CrossTalkOuter);
    ga->FixParameter(10,tau); // frac
    ga->FixParameter(11,0);   // Sigma for the 2-nd Gauss 
    // Pads
    if (Name.Contains("Pad",TString::kIgnoreCase)) {
      // Cross Talk fit
      if (Opt.Contains("Cross",TString::kIgnoreCase)) {
	ga->FixParameter(2,0);
	ga->ReleaseParameter(9); 
	ga->SetParLimits(9,0,0.5);
      }
      if (Opt.Contains("K3",TString::kIgnoreCase)) {
	Int_t k = 3;
	if (Name.Contains("Outer",TString::kIgnoreCase)) k = 4; 
	Double_t K3 = ga->GetParameter(k);
	ga->SetParLimits(k,0.01*K3,100*K3);
	if (! Opt.Contains("K3S")) ga->FixParameter(2,0);
      }
    }
    if (Opt.Contains("Noise",TString::kIgnoreCase)) {
      ga->ReleaseParameter(6);  ga->SetParameter(6,0);  ga->SetParLimits(6, 0,1); // noise
    }
    if (Opt.Contains("Sigma",TString::kIgnoreCase)) {
      ga->ReleaseParameter(10);
      ga->SetParLimits(10,0,1);
      ga->ReleaseParameter(11);
      ga->SetParameter(11,5);
      ga->SetParLimits(11,1,100);
    }
    if (Name.Contains("Time",TString::kIgnoreCase)) {
      ga->ReleaseParameter(5);   ga->SetParameter(5,0);  ga->SetParLimits(5,-15,5); // shift
      ga->FixParameter(7,FWHM);
      ga->FixParameter(8,0.5545);
    }
    if (Opt.Contains("FWHM",TString::kIgnoreCase)) {
      ga->ReleaseParameter(7); // 
      ga->SetParameter(7,FWHM);
      ga->SetParLimits(7,2.5,4.5);
      ga->ReleaseParameter(8); // 
      ga->SetParameter(8,0.5);
      ga->SetParLimits(8,0.2,1.0);
    }
  }
  TString OutFile(fIn->GetName());
  OutFile.ReplaceAll(".root",".Fit.root");
  if (! fOut) fOut = new TFile(OutFile,"recreate");
  fOut->cd();
  Int_t Npar = ga->GetNpar();
  TH1D **out = new TH1D*[Npar+1];
  memset(out, 0, (Npar+1)*sizeof(TH1D*));
  Double_t pmin, pmax;
  TString pName("");
  TString pTitle("");
  for (Int_t i = 1; i < Npar; i++) {
    pmin = -1.;
    pmax =  1.;
    if (i < Npar) {
      ga->GetParLimits(i,pmin,pmax);
      pName = Form("%s_p%i",Name.Data(),i);
      pTitle = Form("Fit result for %s",ga->GetParName(i));
    }
    else {
      pName = Form("%s_Chisq",Name.Data());
      pTitle = Form("Chisq for %s",ga->GetParName(i));
    }
    if (pmin < pmax) { 
      out[i] = (TH1D *) fIn->Get(pName.Data());
      if (! out[i]) {
	out[i] = new TH1D(pName.Data(),pTitle.Data(),
			  ny,h->GetYaxis()->GetXmin(),h->GetYaxis()->GetXmax());
	out[i]->SetXTitle("Z (cm)");
	out[i]->SetStats(1000000001);
      }
    }
  }
  TCanvas *c1 = new TCanvas(Name.Data(),Name.Data());
  Int_t iy1 = 1;
  Int_t iy2 = ny;
  if (iy) {iy1 = iy2 = iy;}
  for (Int_t i = iy1; i <= iy2; i++) {
    TH1D *proj = h->ProjectionX(Form("%s_%i%s",Name.Data(),i,opt),i,i);
    proj->SetTitle(Form("Projection in [%4.0f,%4.0f] z range",h->GetYaxis()->GetBinLowEdge(i),h->GetYaxis()->GetBinUpEdge(i)));
    if (proj->GetEntries() < 10) {
      delete proj;
      continue;
    }
    Double_t xFmin = proj->GetXaxis()->GetXmin();
    Double_t xFmax = proj->GetXaxis()->GetXmax();
    if (Name.Contains("Pad",TString::kIgnoreCase)) {
      xFmin = -2; 
      xFmax =  2;
    }
    Double_t sum = proj->GetSum();
    proj->Reset();
    Double_t xmin =  9999.;
    Double_t xmax = -9999.;
    
    for (Int_t j = 2; j <= nx; j++) {
      Int_t bin    = h->GetBin(j,i);
      Double_t ent = h->GetBinEntries(bin); 
      Double_t val = h->GetBinContent(bin);
      Double_t err = h->GetBinError(bin);
      Double_t xle = h->GetXaxis()->GetBinLowEdge(j);
      Double_t xue = h->GetXaxis()->GetBinUpEdge(j);
      //      cout << "j" << "\t" << xce << "\t" << ent << "\t" << val << "\t+/-\t" << err << endl;
      if (ent > 2) {
	//      if (ent > 10 && val > 3*err && err > 0) {
	val /= (sum*(xue-xle));
	err /= (sum*(xue-xle));
	proj->SetBinContent(j,val);
	proj->SetBinError(j,err);
	if (val > 0.01) {
	  if (xmin > xle) xmin = xle;
	  if (xmax < xue) xmax = xue;
	}
      }
    }
    //    proj->SetAxisRange(xmin,xmax);
    proj->Fit(ga,"rev","",xFmin,xFmax);
    c1->Update();
    //    Double_t prob = ga->GetProb();
    //    Double_t chisq = ga->GetChisquare(); 
    //    out[Npar]->SetBinContent(i,chisq);
    //    if (chisq < 1.e5) {
      for (Int_t j = 2; j < Npar; j++) {
	if (out[j]) {
	  out[j]->SetBinContent(i,ga->GetParameter(j));
	  out[j]->SetBinError(i,ga->GetParError(j));
	}
      } 
      //    }
  }
  if (! iy && out[2]) {
    out[2]->SetMarkerStyle(20);
    if (Name.Contains("Outer",TString::kIgnoreCase)) out[2]->SetMarkerColor(2);
    out[2]->Fit(gd);
  }
  for (Int_t j = 2; j < Npar; j++) {
    if (out[j]) {
      Double_t ymin = 1e9;
      Double_t ymax = -1e9;
      for (Int_t i = iy1; i <= iy2; i++) {
	Double_t y = out[j]->GetBinContent(i);
	Double_t dy = out[j]->GetBinError(i);
	if (dy > 0) {
	  if (y > ymax) ymax = y;
	  if (y < ymin) ymin = y;
	}
      }
      if (ymin < ymax) {
	out[j]->SetMinimum(ymin);
	out[j]->SetMaximum(ymax);
      }
    }
  } 
  fIn->cd();
}
//________________________________________________________________________________
void FitSlicesT(const Char_t *name="OuterTimeRc", Int_t iy=0, const Char_t *opt="SigmaSqSpread") {
  TDirectory *fIn = gDirectory;
  if (! fIn) return;
  TProfile2D *h = (TProfile2D *) fIn->Get(name);
  if (! h) return;
  Int_t nx = h->GetNbinsX();
  Int_t ny = h->GetNbinsY();
  TString Name(h->GetName());
  Name += opt;
  if (! h) return;
  TF1 *gd = new TF1("gd","[0]*[0]+[1]*[1]*(209.3-abs(x))",-210,210);
  if (gd) {
    gd->SetParName(0,"#sigma_{C}");
    gd->SetParName(1,"#sigma_{D}");
    gd->SetParLimits(0,0,10.);
    gd->SetParLimits(1,0,10.);
  }
  Double_t lH = lI;
  TProfile *prof = (TProfile *) gDirectory->Get("dLInner");
  if (Name.Contains("Outer")) {
    lH = lO;
    prof = (TProfile *) gDirectory->Get("dLOuter");
  }
  if (! prof) return;
  TF1 *ga = new TF1("ga",ConvGausShaperF,-2,5,5);
  ga->SetParameters(0,1,1,0,1);
  ga->SetParNames("shift","Area","sigmaSQ","noise","spread");
  ga->SetParLimits(2,0.,10.);
  ga->SetParLimits(3,0.,10.);
  TString OutFile(fIn->GetName());
  OutFile.ReplaceAll(".root",".Fit.root");
  if (! fOut) fOut = new TFile(OutFile,"recreate");
  fOut->cd();
  Int_t Npar = ga->GetNpar();
  TH1D **out = new TH1D*[Npar+1];
  memset(out, 0, (Npar+1)*sizeof(TH1D*));
  Double_t pmin, pmax;
  TString pName("");
  TString pTitle("");
  for (Int_t i = 0; i < Npar; i++) {
    pmin = -1.;
    pmax =  1.;
    if (i < Npar) {
      ga->GetParLimits(i,pmin,pmax);
      pName = Form("%s_p%i",Name.Data(),i);
      pTitle = Form("Fit result for %s",ga->GetParName(i));
    }
    else {
      pName = Form("%s_Chisq",Name.Data());
      pTitle = Form("Chisq for %s",ga->GetParName(i));
    }
    if (pmin < pmax) { 
      out[i] = (TH1D *) fIn->Get(pName.Data());
      if (! out[i]) {
	out[i] = new TH1D(pName.Data(),pTitle.Data(),
			  ny,h->GetYaxis()->GetXmin(),h->GetYaxis()->GetXmax());
	out[i]->SetXTitle("Z (cm)");
	out[i]->SetStats(1000000001);
      }
    }
  }
  TCanvas *c1 = new TCanvas(Name.Data(),Name.Data());
  Int_t iy1 = 1;
  Int_t iy2 = ny;
  if (iy) {iy1 = iy2 = iy;}
  for (Int_t i = iy1; i <= iy2; i++) {
    Double_t cosL2I = prof->GetBinContent(i);
    if (cosL2I <= 0) continue;
    Double_t tanL   = TMath::Sqrt(cosL2I-1);
    Double_t spread = lH*tanL/timeBin;
    ga->SetParameters(0,1,1,0,1);
    ga->FixParameter(4,spread);

    TH1D *proj = h->ProjectionX(Form("%s_%i%s",Name.Data(),i,opt),i,i);
    proj->SetTitle(Form("Projection in [%4.0f,%4.0f] z range",h->GetYaxis()->GetBinLowEdge(i),h->GetYaxis()->GetBinUpEdge(i)));
    if (proj->GetEntries() < 10) {
      delete proj;
      continue;
    }
    Double_t xFmin = -2.0;
    Double_t xFmax =  2.5;
    Double_t sum = proj->GetSum();
    proj->Reset();
    Double_t xmin =  9999.;
    Double_t xmax = -9999.;
    Double_t fractionI = 0;
    for (Int_t j = 2; j <= nx; j++) {
      Int_t bin    = h->GetBin(j,i);
      Double_t ent = h->GetBinEntries(bin); 
      Double_t val = h->GetBinContent(bin);
      fractionI += val/sum;
      Double_t err = h->GetBinError(bin);
      Double_t xle = h->GetXaxis()->GetBinLowEdge(j);
      Double_t xue = h->GetXaxis()->GetBinUpEdge(j);
      //      cout << "j" << "\t" << xce << "\t" << ent << "\t" << val << "\t+/-\t" << err << endl;
      if (ent > 2) {
	//      if (ent > 10 && val > 3*err && err > 0) {
	val /= (sum*(xue-xle));
	err /= (sum*(xue-xle));
	proj->SetBinContent(j,val);
	proj->SetBinError(j,err);
	if (fractionI > 0.10 && fractionI < 0.80) {
	  if (xmin > xle) xmin = xle;
	  if (xmax < xue) xmax = xue;
	}
      }
    }
    xFmin = xmin;
    xFmax = xmax;
    //    proj->SetAxisRange(xmin,xmax);
    cout << "Fit: " << proj->GetTitle() << endl;
    //    proj->Fit(ga,"rev","",xFmin,xFmax);
    proj->Fit(ga,"re","",xFmin,xFmax);
    c1->Update();
    //    Double_t prob = ga->GetProb();
    //    Double_t chisq = ga->GetChisquare(); 
    //    out[Npar]->SetBinContent(i,chisq);
    //    if (chisq < 1.e5) {
      for (Int_t j = 2; j < Npar; j++) {
	if (out[j]) {
	  out[j]->SetBinContent(i,ga->GetParameter(j));
	  out[j]->SetBinError(i,ga->GetParError(j));
	}
      } 
      //    }
  }
  if (! iy && out[2]) {
    out[2]->SetMarkerStyle(20);
    if (Name.Contains("Outer",TString::kIgnoreCase)) out[2]->SetMarkerColor(2);
    out[2]->Fit(gd);
  }
  for (Int_t j = 2; j < Npar; j++) {
    if (out[j]) {
      Double_t ymin = 1e9;
      Double_t ymax = -1e9;
      for (Int_t i = iy1; i <= iy2; i++) {
	Double_t y = out[j]->GetBinContent(i);
	Double_t dy = out[j]->GetBinError(i);
	if (dy > 0) {
	  if (y > ymax) ymax = y;
	  if (y < ymin) ymin = y;
	}
      }
      if (ymin < ymax) {
	out[j]->SetMinimum(ymin);
	out[j]->SetMaximum(ymax);
      }
    }
  } 
  fIn->cd();
}
//________________________________________________________________________________
TH1D *RMSSlices(const Char_t *name="InnerPadRc", const Char_t *opt="") {
  TProfile2D *h = (TProfile2D *) gDirectory->Get(name);
  Int_t nx = h->GetNbinsX();
  Int_t ny = h->GetNbinsY();
  TString Name(h->GetName());
  Name += opt;
  TH1D *out = new TH1D(Name.Data(),"RMS**2 result",ny,h->GetYaxis()->GetXmin(),h->GetYaxis()->GetXmax());
  Int_t iy1 = 1;
  Int_t iy2 = ny;
  for (Int_t i = iy1; i <= iy2; i++) {
    TH1D *proj = h->ProjectionX(Form("%s_%i",Name.Data(),i),i,i);
    proj->SetTitle(Form("Projection in [%4.0f,%4.0f] z range",h->GetYaxis()->GetBinLowEdge(i),h->GetYaxis()->GetBinUpEdge(i)));
    if (proj->GetEntries() < 10) continue;
    proj->Reset();
    Double_t xmin =  9999.;
    Double_t xmax = -9999.;
    for (Int_t j = 1; j <= nx; j++) {
      Int_t bin    = h->GetBin(j,i);
      Double_t ent = h->GetBinEntries(bin); 
      Double_t val = h->GetBinContent(bin);
      Double_t err = h->GetBinError(bin);
      Double_t xce = h->GetXaxis()->GetBinCenter(j);
      //      cout << "j" << "\t" << xce << "\t" << ent << "\t" << val << "\t+/-\t" << err << endl;
      if (ent > 2) {
	//      if (ent > 10 && val > 3*err && err > 0) {
	proj->SetBinContent(j,val);
	proj->SetBinError(j,err);
	if (val > 0.01) {
	  if (xmin > xce) xmin = xce;
	  if (xmax < xce) xmax = xce;
	}
      }
    }
    Double_t rms = proj->GetRMS()*proj->GetRMS();
    out->SetBinContent(i,rms);
  } 
  
  out->SetMarkerStyle(20);
  return out;
}
//________________________________________________________________________________
Double_t rc(Double_t s=0.4, Double_t h=0.4, Double_t ra=1e-3, Double_t Va=1390) {// outer
  //                 s=0.4, Double_t h=0.2, Double_t ra=1e-3, Double_t Va=1170   // inner
  //  const Double_t mu = 2.26; // cm**2/V/sec CH4+ mobility 
  const Double_t mu = 1.87; // cm**2/V/sec CH4+ mobility, W.Blum and L. Rolandi, "Particle Decection with Drift Chambers", Table 2.2
  TF1 *Ln0 = new TF1("Ln0","TMath::Log(TMath::TanH(x*TMath::Pi()*[0]/(4*[1])))",1,10);
  Ln0->SetParameters(s,h);  
  Double_t rc = s/(2*TMath::Pi())*TMath::Exp(TMath::Pi()*h/s); cout << "s >> h : s = " << s << " h = " << h << " rc = " << rc << " cm" << endl;
  Double_t C  = 1./(2*TMath::Log(rc/ra)); cout << "C = " << C << endl;
  Double_t E  = 2*TMath::Pi()*C*Va/s; cout << "E = " << E << " V/cm" << endl;
  Double_t t0 = ra*ra/(4*mu*C*Va); cout << "t0 = " << 1e9*t0 << " ns" << endl;  
  // E.Mathieson (4.3), no valid for our case
  rc = 4*h/TMath::Pi(); cout << "TMath::CosH(2*TMath::Pi()*h/s) = " << TMath::CosH(2*TMath::Pi()*h/s) << " >> 1 : rc = " << rc << " cm" << endl;   
  C  = 1./(2*TMath::Log(rc/ra)); cout << "C = " << C << endl;
  t0 = ra*ra/(4*mu*C*Va); cout << "t0 = " << 1e9*t0 << " ns" << endl;  
  Double_t rcLog = TMath::Log(4*h/TMath::Pi());
  for (Double_t x = 1; x <= 10; x++) {
    rcLog -= 2*Ln0->Eval(x);
  }
  rc = TMath::Exp(rcLog);
  C  = 1./(2*TMath::Log(rc/ra)); cout << "C = " << C << endl;
  t0 = ra*ra/(4*mu*C*Va); cout << "t0 = " << 1e9*t0 << " ns" << endl;  
  cout << "Exact formula : rc = " << rc << endl;
  return rc;
}
#if 0
//________________________________________________________________________________
Double_t gain(GainDouble_t s=0.4, Double_t h=0.4, Double_t ra=1e-3, Double_t Va=1390, Double_t alpha = -26, Double_t r = 0) {// outer
  // E.Mathieson and T.J.Harris, "Modulation of anode signal in multiwire proportional chambers", NIM 157 (1978) 563
  Double sOverh =  s/(TMath::Pi()*h);
  Double_t Ca = sOverh/(1 - sOverH*TMath::Log(2*TMath::Pi()*ra/s));
  if (r < ra) r = ra;
  
}
#endif
//________________________________________________________________________________
Double_t InducedCharge(Double_t s, Double_t h, Double_t ra, Double_t Va) {
  // Calculate variation of induced charge due to different arrived angles 
  // alpha = -26 and -70 degrees
  cout << "wire spacing = " << s << " cm"
       << "\tcathode anode gap = " << h << " cm"
       << "\tanode wire radius = " << ra << " cm"
       << "\tpotential on anode wire = " << Va << " V" << endl;
  const Double_t B  = 30e-3; // 1/V
  const Double_t E0 = 20e3; // V/cm
  const Double_t mu = 2.26; // cm**2/V/sec CH4+ mobility 
  // const Double_t mu = 1.87; // cm**2/V/sec Ar+ mobility 
  Double_t alpha[2] = {-26., -70.};
  Double_t pi = TMath::Pi();
  Double_t rc = s/(2*pi)*TMath::Exp(pi*h/s); cout << "rc = " << rc << " cm" << endl;
  Double_t C  = 1./(2*TMath::Log(rc/ra)); cout << "C = " << C << endl;
  Double_t E  = 2*pi*C*Va/s; cout << "E = " << E << " V/cm" << endl;
  // Gain variation: M = M0*(1 - k*cos(2*alpha))
  Double_t k = 2*B/3.*TMath::Power((pi/E0/s),2)*TMath::Power(C*Va,3); cout << "k = " << k << endl;
  // Induced charge variation
  Double_t t0 = ra*ra/(4*mu*C*Va); cout << "t0 = " << 1e9*t0 << " ns" << endl;
  Double_t Tav = t0*h/s/(2*pi*C);  cout << "Tav = " << 1e9*Tav << " ns" << endl;
  //  Double_t t = 5*55e-9;             cout << "t = " << 1e9*t << " ns" << endl;
  Double_t t = 180e-9;             cout << "t = " << 1e9*t << " ns" << endl;
  Double_t rp = TMath::Sqrt(1. + t/t0); cout << "r' = " << rp << endl;
  // qc = rp*ra*sin(alpha)/(2*h) + C/2*log(1 + t/t0) = A*sin(alpha) + B
  Double_t Aconstant = rp*ra/(2*h);        cout << "Aconstant = " << Aconstant << endl;
  Double_t Bconstant = C/2*TMath::Log(1 + t/t0); cout << "Bconstant = " << Bconstant << endl;
  Double_t Gains[2];
  for (Int_t i = 0; i < 2; i++) {
    Gains[i] = Aconstant*TMath::Sin(pi/180*alpha[i]) + Bconstant; 
    cout << "Gain = " << Gains[i] << " at alpha = " << alpha[i] << " degree" << endl;
  }
  Double_t GainsAv = TMath::Sqrt(Gains[0]*Gains[1]);
  Double_t r = 0;
  for (Int_t i = 0; i < 2; i++) {
    r = TMath::Log(Gains[i]/GainsAv); cout << "Relative gain " << r << " at alpha = " << alpha[i] << endl;
  }
  return r;
}
#if 0
//________________________________________________________________________________
void AlphaVariation() {
  
  cout << "Outer Sector ======================" << endl;
  Double_t OuterAlphaVariation = InducedCharge(anodeWirePitch,
					       outerSectorPadWidth,
					       anodeWireRadius,
					       outerSectorAnodeVoltage);
  cout << "Inner Sector ======================" << endl;
  Double_t InnerAlphaVariation = InducedCharge(anodeWirePitch,
					       innerSectorPadWidth,
					       anodeWireRadius,
					       innerSectorAnodeVoltage);
}
#endif
//______________ EXPONENTIAL INTEGRALS En __________________________________________________________________
// define: E_n(x) = \int_1^infty{exp(-xt)/t^n}dt, x>0, n=0,1,...
Double_t expint(Int_t n, Double_t x) {
  // based on Numerical Recipes in C
  const Double_t euler = 0.57721566; // Euler's constant, gamma
  const Int_t maxit = 100;           // max. no. of iterations allowed
  const Double_t fpmin = 1.0e-30;    // close to smallest floating-point   number
  const Double_t eps = 6.0e-8;       // relative error, or absolute error near
  // the zero of Ei at x=0.3725
  
  Int_t i, ii, nm1;
  Double_t a,b,c,d,del,fact,h,psi,ans;
  
  nm1=n-1;
  if(n<0 || x<0 || (x==0 && (n==0 || n==1))) {
    cout << "Bad argument for expint(n,x)" << endl; return -1;
  }
  else {
    if(n==0) ans=exp(-x)/x;
    else {
      if(x==0) ans=1.0/nm1;
      else {
	if(x>1) {
	  b=x+n;
	  c=1.0/fpmin;
	  d=1.0/b;
	  h=d;
	  for(i=1; i<maxit; i++) {
	    a = -i*(nm1+i);
	    b += 2.0;
	    d=1.0/(a*d+b);
	    c=b+a/c;
	    del=c*d;
	    h *= del;
	    if(fabs(del-1.0)<eps) {
	      ans=h*exp(-x);
	      return ans;
	    }
	  }
	  cout << "***continued fraction failed in expint(n,x)!!!" << endl;
	  return -1;
	} else {
	  ans = (nm1!=0 ? 1.0/nm1 : -log(x)-euler);
	  fact=1;
	  for(i=1; i<=maxit; i++) {
	    fact *= -x/i;
	    if(i!=nm1) del = -fact/(i-nm1);
	    else {
	      psi = -euler;
	      for(ii=1; ii<=nm1; ii++) psi += 1.0/ii;
	      del = fact*(-log(x)+psi);
	    }
	    ans += del;
	    if(fabs(del)<fabs(ans)*eps) return ans;
	  }
	  cout << "***series failed in expint!!!" << endl;
	  return -1;
	}
      }
    }
  }
  
  return ans;
}
//______________ EXPONENTIAL INTEGRAL Ei __________________________________________________________________
// define: ei(x) = -\int_{-x}^{\infty}{exp(-t)/t}dt,  for x>0
// power series: ei(x) = eulerconst + ln(x) + x/(1*1!) + x^2/(2*2!) + ...
Double_t ei(Double_t x)
{ // taken from Numerical Recipes in C
  const Double_t euler = 0.57721566; // Euler's constant, gamma
  const Int_t maxit = 100;           // max. no. of iterations allowed
  const Double_t fpmin = 1.e-7; //1.0e-40;    // close to smallest floating-point number
  const Double_t eps = 1.e-7; //1.0e-30;       // relative error, or absolute error  near
  // the zero of Ei at x=0.3725
  //  I actually changed fpmin and eps into smaller values than in NR
  
  Int_t k;
  Double_t fact, prev, sum, term;
  
  // special case
  if(x < 0) return -expint(1,-x);
  
  if(x == 0.0) { cout << "Bad argument for ei(x)" << endl; return -1; }
  if(x < fpmin) return log(x)+euler;
  if(x <= -log(eps)) {
    sum = 0;
    fact = 1;
    for(k=1; k<=maxit; k++) {
      fact *= x/k;
      term = fact/k;
      sum += term;
      if(term < eps*sum) break;
    }
    if(k>maxit) { cout << "Series failed in ei(x)" << endl; return -1; }
    return sum+log(x)+euler;
  } else {
    sum = 0;
    term = 1;
    for(k=1; k<=maxit; k++) {
      prev = term;
      term *= k/x;
      if(term<eps) break;
      if(term<prev) sum+=term;
      else {
	sum -= prev;
	break;
      }
    }
    return exp(x)*(1.0+sum)/x;
  }
}
//________________________________________________________________________________
Double_t shapeEI(Double_t *x, Double_t *par) {
  Int_t                          k = 0; // Inner
  if (TMath::Abs(par[0]) > 1e-3) k = 1; // Outer
  Double_t t0 = t0IO[k];
  Double_t d =   1./tauP;
  Double_t a[2] = {- 1./tauIntegraton, - 1./tauF};
  Double_t A[2] = {(a[0]+d)/(a[0]-a[1]), (a[1]+d)/(a[1]-a[0])};
  Double_t t  = x[0];
  Double_t value = 0;
  if (t <= 0) return value;
  for (Int_t i = 0; i < 2; i++) {
    value += A[i]*TMath::Exp(a[i]*(t+t0))*(ei(-a[i]*(t+t0))-ei(-a[i]*t0));
  }
  return value;
}
//________________________________________________________________________________
Double_t shapeEI1(Double_t *x, Double_t *par) {
  Int_t                          k = 0; // Inner
  if (TMath::Abs(par[0]) > 1e-3) k = 1; // Outer
  Double_t t0 = t0IO[k];
  Double_t a = - 1./tauIntegraton;
  Double_t t  = x[0];
  Double_t value = 0;
  if (t <= 0) return value;
  value = TMath::Exp(a*(t+t0))*(ei(-a*(t+t0))-ei(-a*t0));
  return value;
}
#if 0
//________________________________________________________________________________
Double_t shape(Double_t *x, Double_t *par=0) {
  Double_t d =   1./tauP;
  Double_t a[2] = {- 1./tauIntegraton, - 1./tauF};
  Double_t A[2] = {(a[0]+d)/(a[0]-a[1]), (a[1]+d)/(a[1]-a[0])};
  Double_t t  = x[0];
  Double_t value = 0;
  for (Int_t i = 0; i < 2; i++) {
    value += A[i]*TMath::Exp(a[i]*t);
  }
  return value;
}
#endif
//________________________________________________________________________________
Double_t shapeEI3(Double_t *x, Double_t *par) {// does not work. It is needed to 1/s
  Int_t                          k = 0; // Inner
  if (TMath::Abs(par[0]) > 1e-3) k = 1; // Outer
  Double_t t0 = t0IO[k];
  Double_t d =   1./tauP;
  Double_t a[3] = {- 1./tauIntegraton, - 1./tauF, - 1./tauC[k]};
  Double_t A[3] = {(a[0] + d)/a[0]/(a[0] - a[1])/(a[0] - a[2]),
		   (a[1] + d)/a[1]/(a[1] - a[0])/(a[1] - a[2]),
		   (a[2] + d)/a[2]/(a[2] - a[0])/(a[2] - a[1])}; 
  Double_t t  = x[0];
  Double_t value = - d/(a[0]*a[1]*a[2])/(t + t0);
  for (Int_t i = 0; i < 3; i++) {
    value += A[i]*TMath::Exp(a[i]*(t+t0))*(ei(-a[i]*(t+t0))-ei(-a[i]*t0));
  }
  return value;
}
#if 0
//________________________________________________________________________________
Double_t shapeEI_Inner(Double_t *x, Double_t *par=0) {
  static TF1* sI = 0;
  static Double_t norm = 0;
  if (! sI) {sI = new TF1("sI",shapeEI,0,50e-6,1); sI->SetParameter(0,0); norm = sI->Integral(0,250*mTimeBinWidth);}
  Double_t t1 = mTimeBinWidth*(x[0] - 0.5);
  Double_t t2 = t1 + mTimeBinWidth;
  return sI->Integral(t1,t2)/norm;
}
//________________________________________________________________________________
Double_t shapeEI_Outer(Double_t *x, Double_t *par=0) {
  static TF1* sO = 0;
  static Double_t norm = 0;
  if (! sO) {sO = new TF1("sO",shapeEI,0,50e-6,1); sO->SetParameter(0,1);norm = sO->Integral(0,250*mTimeBinWidth);}
  Double_t t1 = mTimeBinWidth*(x[0] - 0.5);
  Double_t t2 = t1 + mTimeBinWidth;
  return sO->Integral(t1,t2)/norm;
}
#endif
//________________________________________________________________________________
void TpcTFitSlices(const Char_t *name = "OuterPadRc") {
  TProfile2D *h = (TProfile2D *) gDirectory->Get(name);
  if (! h) return;
  TF1 *ga = new TF1("ga","exp([0]-x*x/(2*[1]))+[2]",-2.,2.);
  ga->SetParameters(0,1,0);
  ga->SetParLimits(1,0.1,100);
  h->FitSlicesX(ga,0,0,10,"r");
}
#if 0
//____________________________________________________________
void CrossTalk(const Char_t *fN = "PadResponseFunctionInner") {
  TpcT t;
  t.MakeFunctions();
  TF1 *func = (TF1 *) gROOT->GetFunction(fN);
  if (! func ) return;
  TProfile *hist = new TProfile("hist","hist",100,-func->GetXmax(),func->GetXmax());
  TF1 *ga = t.GetConvolution();
  ga->FixParameter(0,0);
  ga->ReleaseParameter(2); ga->SetParLimits(2,0,10);
  ga->FixParameter(3,K3IP);
  ga->FixParameter(4,K3OP);
  ga->FixParameter(5,0);
  ga->FixParameter(6,0);
  ga->FixParameter(7,3);
  ga->FixParameter(8,55e-9);
  TString fName(fN);
  Double_t padP = padPitchI;
  if (fName.EndsWith("Outer")) {ga->FixParameter(0,1); padP = padPitchO;}
  const Int_t N = 20;
  const Double_t CTmin = 0.0;
  const Double_t CTmax = 0.2;
  Double_t dCT = (CTmax - CTmin)/N;
  TH1D *out = new TH1D(Form("h%s",fN),fN, N, CTmin - 0.5*dCT, CTmax+0.5*dCT);
  const Int_t no = 2000;
  const Int_t Npads = 3;
  const Int_t NN  = 2*Npads + 1;
  Double_t pads[NN];
  for (Int_t i = 0; i < N; i++) {
    hist->Reset();
    Double_t CT = CTmin + dCT*i;
    for (Int_t iev = 0; iev < no; iev++) {
      Double_t x = gRandom->Rndm() - 0.5;
      memset (pads, 0, NN*sizeof(Double_t));
      for (Int_t k = -Npads ; k <=Npads ; k++) {
	Double_t xx = x + k;
	Double_t res = func->Eval(xx);
	if (k - 1 >= -Npads) pads[k-1+Npads] += CT*res;
	if (k + 1 <=  Npads) pads[k+1+Npads] += CT*res;
	pads[k+Npads] += (1 - 2*CT)*res;
      }
      for (Int_t k = -Npads ; k <=Npads ; k++) {
	hist->Fill(x+k,pads[k+Npads]);
      }
    }
    hist->Fit(ga);
    Double_t val = TMath::Sqrt(ga->GetParameter(2));
    out->SetBinContent(i+1,padP*val);
    if (val > 0) 
      out->SetBinError(i+1,padP*ga->GetParError(2)/(2*val));
  }
  cout << " ========================================================= " << endl;
}
#endif
//________________________________________________________________________________
void Fits(const Char_t *opt="Rc", Int_t tp = -1) {
  const Char_t *InOut[2] = {"Inner","Outer"};
  const Char_t *PadTime[2] =           {"Time"         ,"Pad"};
  const Char_t *PadTimeFitOptions[2] = {"SigmaSqSpread","SigmaSQ"};
  //  const Char_t *PadTimeFitOptions[2] = {"FWHMNoise","SigmaSQ"};
  //  const Char_t *PadTimeFitOptions[2] = {"FWHMNoise","NoiseConv"};
  //  const Char_t *PadTimeFitOptions[2] = {"ConvNoise","NoiseConv"};
  //  const Char_t *PadTimeFitOptions[2] = {"ConvFWHM","Conv"};
  //  const Char_t *PadTimeFitOptions[2] = {"ConvFWHM","ConvNoise"};
  const Char_t *X[2]                 = {"","X"};
  Int_t ip1 = 0;
  Int_t ip2 = 1;
  if (tp > 0 && tp < 2) ip1 = ip2 = tp;
  for (Int_t ix = 0; ix < 2; ix++) {
    for (Int_t ip = ip1; ip <= ip2; ip++) {
      for (Int_t io = 0; io < 2; io++) {
	TString Fit(PadTimeFitOptions[ip]);
	TString Name(Form("%s%s%s%s",InOut[io],X[ix],PadTime[ip],opt));
	if (Fit == "SigmaSqSpread") {FitSlicesT(Name);}
	else                        {FitSlices (Name,PadTimeFitOptions[ip]);}
	
	//  FitSlices(Form("InnerTime%s",opt),"FWHMNoiseConv");
	//  FitSlices(Form("OuterTime%s",opt),"FWHMNoiseConv");
	//  FitSlices(Form("InnerXTime%s",opt),"FWHMNoiseConv");
	//  FitSlices(Form("OuterXTime%s",opt),"FWHMNoiseConv");
	//  FitSlices(Form("InnerPad%s",opt),"NoiseConv");
	//  FitSlices(Form("OuterPad%s",opt),"NoiseConv");
	//   FitSlices(Form("InnerXPad%s",opt),"NoiseConv");
	//   FitSlices(Form("OuterXPad%s",opt),"NoiseConv");
	// 	Name += "_*";
	// 	DrawList(Name);
      }
    }
  }
  if (fOut) {
    fOut->Write();
    delete fOut; fOut = 0;
  }
}
//________________________________________________________________________________
Double_t exp2f(Double_t *x, Double_t *p) {
  Double_t t = x[0] - p[0];
  Double_t Value = 0;
  if (t <= 0.0) return Value;
  Double_t t1 = t - 0.5;
  Double_t t2 = t1 + 1;
  if (t1 < 0) t1 = 0;
  Double_t Norm = p[1];
  Double_t a    = p[2];
  Double_t b    = p[3];
  if (a != b) {
    Value = 1./(a-b)*((TMath::Exp(a*t2)-TMath::Exp(a*t1))/a-(TMath::Exp(b*t2)-TMath::Exp(b*t1))/b);
  } else {
    Value = TMath::Gamma(2,t2) - TMath::Gamma(2,t1);
  }
  return Norm*Value;
}
//________________________________________________________________________________
TF1 *Exp2F() {
  TF1 *f = new TF1("Exp2F",exp2f,-5,15,4);
  f->SetParNames("t_0","Norm","a","b");
  f->SetParameters(-5,1,-1,-2);
  f->SetParLimits(0,-10,5);
  f->SetParLimits(2,-100,0);
  f->SetParLimits(3,-100,0);
  return f;
}
//________________________________________________________________________________
void MakeProjections(const Char_t *name="OuterXTimeMc") {
  TDirectory *fIn = gDirectory;
  if (! fIn) return;
  TProfile2D *h = (TProfile2D *) fIn->Get(name);
  if (! h) return;
  TString Name(name);
  Name += ".root";
  fOut = new TFile(Name,"recreate");
  h->Write();
#if 0
  Int_t nx = h->GetNbinsX();
#endif
  Int_t ny = h->GetNbinsY();
  new TCanvas(name,name);
  for (Int_t i = 1; i <= ny; i++) {
    TH1D *proj = h->ProjectionX(Form("%s_%i",name,i),i,i);
    proj->SetTitle(Form("Projection in [%4.0f,%4.0f] z range",h->GetYaxis()->GetBinLowEdge(i),h->GetYaxis()->GetBinUpEdge(i)));
    if (proj->GetEntries() < 10) {
      delete proj;
      continue;
    }
    TString O(proj->GetName());
    O += ".txt";
    ofstream out(O.Data());
    proj->SavePrimitive(out);
    proj->Draw();
    proj->Write();
  }
  delete fOut;
  fIn->cd();
}
//________________________________________________________________________________
void TpcTPadSp(const Char_t *Out = "SpXSpZ", const Char_t *files="*.root") {
  TDirIter Dir(files);
  Char_t *file = 0;
  Char_t *file1 = 0;
  Int_t NFiles = 0;
  TTreeIter iter("TpcT");
  while ((file = (Char_t *) Dir.NextFile())) {
    TString File(file);
    if (File.Contains("Plot") || File.Contains("Fit") || File.Contains("ADC") || File.Contains("Pads") || 
	File.Contains("hist") || File.Contains("tags") || File.Contains("MuMc") || File.Contains("minimc") ||
	File.Contains("minimc") || File.Contains("event") ||
	File.Contains("Sparse") ||
	File.Contains("All") ||
	File.Contains("MuDst.root")) continue;
    TFile *f = new TFile (File);
    if (f) {
      TTree *tree = (TTree *) f->Get("TpcT");
      if (! tree ) continue;
      //    tree->Show(0);
      iter.AddFile(file); 
      NFiles++; 
      file1 = file;
      SetInnerPadrows();
    }
    delete f;
  }
  cout << files << "\twith " << NFiles << " files" << endl; 
  if (! file1 ) return;
  TString output(Out);
  if (output == "") {
    output = file1;
    output.ReplaceAll(".root","Sparse.root");
  } else {
    output += ".Sparse.root";
  }
  cout << "Output for " << output << endl;
  const Int_t&       fNoRcHit                                 = iter("fNoRcHit");
  const Int_t&       fNoMcHit                                 = iter("fNoMcHit");
  const Int_t&       fAdcSum                                  = iter("fAdcSum");
  const Float_t&     Frequency                                = iter("Frequency");
#if 0
  const Float_t&     DVelWest                                 = iter("DVelWest");
  const Float_t&     DVelEast                                 = iter("DVelEast");
#endif
#if 0  
  const UChar_t*&    fPixels_mSector                          = iter("fPixels.mSector");
  const UChar_t*&    fPixels_mRow                             = iter("fPixels.mRow");
  const UChar_t*&    fPixels_mPad                             = iter("fPixels.mPad");
  const UShort_t*&   fPixels_mTimeBin                         = iter("fPixels.mTimeBin");
  const UShort_t*&   fPixels_mAdc                             = iter("fPixels.mAdc");
#ifdef PRINT
  const Int_t*&   fPixels_mIdTruth                         = iter("fPixels.mIdTruth");
#endif
#endif
  const Float_t*&    fMcHit_mLocalMomentum_mX1                = iter("fMcHit.mLocalMomentum.mX1");
  const Float_t*&    fMcHit_mLocalMomentum_mX2                = iter("fMcHit.mLocalMomentum.mX2");
  const Float_t*&    fMcHit_mLocalMomentum_mX3                = iter("fMcHit.mLocalMomentum.mX3");
  const Float_t*&    fMcHit_mPosition_mX3                     = iter("fMcHit.mPosition.mX3");
  //  const Float_t*&    fMcHit_mdE                               = iter("fMcHit.mdE");
  //  const Float_t*&    fMcHit_mdS                               = iter("fMcHit.mdS");
  const Float_t*&    fMcHit_mTof                              = iter("fMcHit.mTof");
  const Long_t*&     fMcHit_mKey                              = iter("fMcHit.mKey");
  const Long_t*&     fMcHit_mVolumeId                         = iter("fMcHit.mVolumeId");
  //  const Float_t*&    fMcHit_mAdc                           = iter("fMcHit.mAdc");
  const Float_t*&    fMcHit_mMcl_x                            = iter("fMcHit.mMcl_x");
  const Float_t*&    fMcHit_mMcl_t                            = iter("fMcHit.mMcl_t");
  //  const Int_t*&      fRcHit_mId                               = iter("fRcHit.mId");
#ifdef PRINT0
  const Float_t*&    fRcHit_mPosition_mX3                     = iter("fRcHit.mPosition.mX3");
  const Float_t*&    fRcHit_mCharge                           = iter("fRcHit.mCharge");
#endif
  const UChar_t*&    fRcHit_mMinpad                           = iter("fRcHit.mMinpad");
  const UChar_t*&    fRcHit_mMaxpad                           = iter("fRcHit.mMaxpad");
  const UChar_t*&    fRcHit_mMintmbk                          = iter("fRcHit.mMintmbk");
  const UChar_t*&    fRcHit_mMaxtmbk                          = iter("fRcHit.mMaxtmbk");
  const Short_t*&    fRcHit_mMcl_x                            = iter("fRcHit.mMcl_x");
  const Short_t*&    fRcHit_mMcl_t                            = iter("fRcHit.mMcl_t");
  const UShort_t*&    fRcHit_mFlag                             = iter("fRcHit.mFlag");
  const Int_t*&   fRcHit_mIdTruth                          = iter("fRcHit.mIdTruth");
  const UShort_t*&   fRcHit_mQuality                          = iter("fRcHit.mQuality");
#if 0
  const Float_t*&    fRcTrack_fpx                             = iter("fRcTrack.fpx");
  const Float_t*&    fRcTrack_fpy                             = iter("fRcTrack.fpy");
#endif
  TString TitleX("dX versus ");
  TString TitleZ("dZ versus ");
  for (Int_t i = 0; i < NoDim; i++) {
    if (i) {TitleX += " "; TitleZ += " ";}
    TitleX += Names[i]; TitleZ += Names[i];
  }
  THnSparseC *spX = 0;
  THnSparseC *spZ = 0;
  if (output.Contains("SpX")) spX = new THnSparseC("SpX", TitleX, NoDim, nBins, &xMin.sec, &xMax.sec);
  if (output.Contains("SpZ")) spZ = new THnSparseC("SpZ", TitleZ, NoDim, nBins, &xMin.sec, &xMax.sec);
  if (! spX && ! spZ) return;
  for (Int_t i = 0; i < NoDim; i++) {
    if (spX) spX->GetAxis(i)->SetName(Names[i]);
    if (spZ) spZ->GetAxis(i)->SetName(Names[i]);
  }
  Var_t x;
  Int_t noTTree = -1;
  while (iter.Next()) {
    Int_t cTTree = iter.Chain()->GetTreeNumber();
    if (noTTree > 0 && cTTree != noTTree) {
      TString OutF(output);
      OutF += Form("%03i",noTTree);
      fOut = new TFile(OutF,"recreate");
      fOut->cd();
      if (spX) spX->Write(); 
      if (spZ) spZ->Write();
      delete fOut; fOut = 0;
      cout << OutF.Data() << " saved" << endl;
    }
    noTTree = cTTree;
    if (fNoRcHit != 1) continue;
    if (fNoMcHit != 1 && fNoMcHit != 3) continue;
    if (fAdcSum <= 0) continue;
    Double_t AdcL = TMath::Log(fAdcSum);
    x.AdcL = AdcL;
    for (Int_t k = 0; k < fNoMcHit; k++) { 
      if (fRcHit_mFlag[k]) continue;
      if (fMcHit_mKey[k] != fRcHit_mIdTruth[k]) continue;
      if (fMcHit_mTof[k] > 50e-9) continue;
      if (fRcHit_mQuality[k] < 95) continue;
      Int_t sec = (fMcHit_mVolumeId[k]/100)%100;
      x.sec = sec;
      Int_t row = fMcHit_mVolumeId[k]%100;
      if (row > xMax.row) row = xMax.row;
      x.row = row;
      Double_t phiL = TMath::DegToRad()*LocalPhi(fMcHit_mLocalMomentum_mX1[k],fMcHit_mLocalMomentum_mX2[k],fMcHit_mVolumeId[k]);
      if (phiL < -TMath::Pi()/2) phiL += TMath::Pi();
      if (phiL >  TMath::Pi()/2) phiL -= TMath::Pi();
      x.phiL = phiL;
      Double_t eta = Eta(fMcHit_mLocalMomentum_mX1[k],fMcHit_mLocalMomentum_mX2[k],fMcHit_mLocalMomentum_mX3[k]);
      x.eta = eta;
      Double_t ZL   = fMcHit_mPosition_mX3[k];
      if (TMath::Nint(fMcHit_mVolumeId[k]/100.)%100>12) {
	eta = -eta;
	ZL   = - ZL;
      }
      x.zL = ZL;
      Int_t npads = fRcHit_mMaxpad[k] + fRcHit_mMinpad[k] + 1;
      if (npads > xMax.Npads) npads = xMax.Npads;
      x.Npads = npads;
      Int_t pad = TMath::Nint((fRcHit_mMcl_x[k])/64.);
      Double_t xMC = fMcHit_mMcl_x[k] - pad;
      Double_t xRC = (fRcHit_mMcl_x[k])/64. - pad;
      x.xPad = xRC;
      Double_t dx = xRC - xMC;
      Int_t ntmbks = fRcHit_mMaxtmbk[k] + fRcHit_mMintmbk[k] + 1;
      if (ntmbks > xMax.Ntmbks) ntmbks = xMax.Ntmbks;
      x.Ntmbks = ntmbks;
      Int_t tbin = TMath::Nint((fRcHit_mMcl_t[k])/64.);
      Double_t zMC = fMcHit_mMcl_t[k] + Frequency*fMcHit_mTof[k] + 1.66168e+00 - tbin;
      Double_t zRC = (fRcHit_mMcl_t[k])/64. - tbin;
      x.zTbk = zRC;
      x.dX = dx;
      if (spX) spX->Fill(&x.sec);
      Double_t dz = zRC - zMC;
      x.dX = dz;
      if (spZ) spZ->Fill(&x.sec);
    }
  }
  fOut = new TFile(output,"recreate");
  fOut->cd();
  if (spX) spX->Write();
  if (spZ) spZ->Write();
}
//________________________________________________________________________________
void AnalysisSparse(const Char_t *histN = "SpX",
		    Int_t sec = 1, Int_t row = 1, 
		    Int_t Npads = 1, Int_t Ntbks = 1,
		    Int_t border=1, 
		  const Char_t *fn="") {
  THnSparseC *h = ( THnSparseC *) gDirectory->Get(histN);
  if (! h) return;
  const static Int_t Ndim = h->GetNdimensions();
  // Output Tree
  TString out(fn);
  if (out == "") out = Form("Fit_%s_s_%i_r_%i_p_%i_t_%i_B_%i.root",histN,sec,row,Npads,Ntbks,border);
  TFile *fout = new TFile(out,"recreate");
  TTree* treeOut = new TTree(Form("%s_tree",histN), "Tree for fit results");
  TString branchCoord;
  for (Int_t d = 0; d < Ndim - 1; d++) {
    if (d) branchCoord += ":";
    branchCoord += h->GetAxis(d)->GetName(); branchCoord += "/D";
  }
  Var_t xC;     Double_t *XC = &xC.sec; memset(XC, 0, sizeof(Var_t));
  treeOut->Branch("coord", &xC.sec, branchCoord);
  struct Fit_t {
    Double_t A; Double_t dA;
    Double_t mu; Double_t dmu;
    Double_t sigma; Double_t dsigma;
    Double_t chi2; 
  };
  Fit_t fit;
  treeOut->Branch("fit",&fit.A,"A/D:dA/D:mu/D:dmu/D:sigma/D:dsigma/D:chi2/D");
  StCloseFileOnTerminate::Instantiate();
  //
  Int_t bins[Ndim]; memset(bins, 0, sizeof(bins));
  bins[0] = sec;
  bins[1] = row;
  bins[2] = Npads;
  bins[3] = Ntbks;
  const Int_t firstN = 4;
  Int_t N = 1;
  TArrayI Nbins(Ndim);
  for (Int_t d = 0; d < Ndim; d++) {
    TAxis *ax = h->GetAxis(d);
    Int_t nb = ax->GetNbins();
    Nbins[d] = nb+2;
    if (d >= firstN && d <= Ndim - 4) N *= Nbins[d];
  }
  TArrayI cont(N);
  Int_t *Bins = new Int_t[Ndim];
  Double_t entries = 0;
  for (Long64_t i = 0; i < h->GetNbins(); ++i) {
    entries = h->GetBinContent(i, Bins);
    //    Int_t jentry = h->GetBin(Bins,kFALSE);
    if (entries <= 0) continue;
    Int_t iok = 1;
    for (Int_t j = 0; j < firstN; j++) {
      if (Bins[j] != bins[j]) {iok = 0; break;}
    }
    if (! iok) continue;
    Int_t bin = 0;
    for (Int_t d = firstN; d <= Ndim - 4; d++) {
      if (d > firstN) bin = bin*Nbins[d];
      bin += Bins[d];
    }
    cont[bin] += entries;
  }
  for (Long64_t n = 0; n < N ; n++) {
    if (cont[n] < 10) continue; 
    Int_t l = n;
    for (Int_t d = Ndim - 4; d >= firstN; d--) {
      bins[d] = l%Nbins[d];
      l /= Nbins[d];
    }
    for (Int_t d = 0; d <= Ndim - 4; d++) {
      TAxis *ax = h->GetAxis(d);
      if (bins[d]) {
	if (d < firstN) ax->SetRange(        bins[d],                            bins[d]);
	else 	   ax->SetRange(TMath::Max(1,bins[d]-border),TMath::Min(Nbins[d],bins[d]+border));
	XC[d] = 0.5*(ax->GetBinLowEdge(ax->GetFirst()) + ax->GetBinUpEdge(ax->GetLast()));
      }
    }
#if 0
    TH3D *h3 = h->Projection(Ndim-3,Ndim-2,Ndim-1,"E");
#else
    TH2D *h3 = h->Projection(Ndim-3,Ndim-1,"E");
#endif
#if 1
    for (Int_t k = 0; k < Ndim - 3; k++) {
      cout << Form("%5s %2i %6.2f ", Names[k],bins[k],XC[k]);
    }
    cout << "\tEntries = " << h3->GetEntries() << endl;
#endif
    if (h3->GetEntries() < 10) {delete h3; continue;}
#if 0
    h3->FitSlicesZ(0,0,-1,0,-1,10);
#else
    h3->FitSlicesX(0,0,-1,10);
#endif
    TH2D *hs[4] = {0,0,0,0};
    for (Int_t i = 0; i < 4; i++) {
      if (i < 3) 
	hs[i] = (TH2D *) gDirectory->Get(Form("%s_%i",h3->GetName(),i));
      else 
	hs[i] = (TH2D *) gDirectory->Get(Form("%s_chi2",h3->GetName()));
    }
    if (! hs[1] || ! hs[2]) return; 
    Int_t nx = h3->GetNbinsX();
    //    Int_t ny = h3->GetNbinsY();
    TAxis *xax = h3->GetXaxis();
    TAxis *yax = h3->GetYaxis();
    for (Int_t i = 1; i <= nx; i++) {
      XC[Ndim-3] = xax->GetBinCenter(i);
#if 0      
      for (Int_t j = 1; j <= ny; j++) {
#else 
	Int_t j = 0;
#endif
	Int_t bin = h3->GetBin(i,j);

	XC[Ndim-2] = yax->GetBinCenter(j);
	fit.A     = hs[0]->GetBinContent(bin); fit.dA     = hs[0]->GetBinError(bin);
	if (fit.dA <= 0) continue;
	fit.mu    = hs[1]->GetBinContent(bin); fit.dmu    = hs[1]->GetBinError(bin);
	fit.sigma = hs[2]->GetBinContent(bin); fit.dsigma = hs[2]->GetBinError(bin);
	fit.chi2  = hs[3]->GetBinContent(bin); 
	treeOut->Fill();
	for (Int_t k = 0; k < Ndim - 1; k++) {
	  cout << Form("%5s", Names[k]);
	  if (k < Ndim - 3) cout << Form(" %2i",bins[k]);
	  else {
	    if (k == Ndim - 3) cout << Form(" %2i",i);
	    if (k == Ndim - 2) cout << Form(" %2i",j);
	  }
	  cout << Form(" %6.2f ",XC[k]);
	}
	cout << Form("\tA = %8.3f +/- %7.3f",fit.A,fit.dA)
	     << Form(" mu = %8.3f +/- %7.3f",fit.mu,fit.dmu)
	     << Form(" sigma = %8.3f +/- %7.3f",fit.sigma,fit.dsigma) 
	     << "\tchi2 = " << fit.chi2 << endl;
#if 0
      }
#endif
    }
    delete h3;
    for (Int_t i = 0; i < 4; i++) delete hs[i];
  }
  fout->Write();
}
//________________________________________________________________________________
const Char_t *PrintLine(Int_t N,const Int_t *Array) {
  static TString line;
  line = "{";
  for (Int_t i = 0; i < N; i++) {
    if (i) line += ",";
    if (N > 10 && i%10 == 0) line += "\n";
    line += Form("%10i",Array[i]);
  }
  line += "};";
  return line.Data();
}
//________________________________________________________________________________
const Char_t *PrintLine(Int_t N,const Double_t *Array) {
  static TString line;
  line = "{";
  for (Int_t i = 0; i < N; i++) {
    if (i) line += ",";
    if (N > 10 && i%10 == 0) line += "\n";
    line += Form("%10.3g",Array[i]);
  }
  line += "};";
  return line.Data();
}
//________________________________________________________________________________
const Char_t *PrintLine(Int_t N,const TVectorD &Array) {
  static TString line;
  line = "{";
  for (Int_t i = 0; i < N; i++) {
    if (i) line += ",";
    if (N > 10 && i%10 == 0) line += "\n";
    line += Form("%10.3g",Array(i));
  }
  line += "};";
  return line.Data();
}
//________________________________________________________________________________
void MDFerrorParameterization(const Char_t *treeName = "SpX", Int_t sec = 0, Int_t row = 0, Int_t N = 3, Int_t MS = 1, Int_t prompt = 0) {
  const Double_t secs[3] = {0.5, 12.5, 24.5};
  const Double_t rows[3] = {0.5, NoInnerRows + 0.5, NoOfRows + 0.5};
  TTree *tree = (TTree *) gDirectory->Get(Form("%s_tree",treeName));
  if (! tree) return;
  Int_t iXZ = -1;
#if 0
  Char_t **names = (Char_t **) &Names;
  Var_t  *xMin  = (Var_t  *)  &xMin;
  Var_t  *xMax  = (Var_t  *)  &xMax;
#endif
  //  Int_t  *nbins = (Int_t *)   &nBins;
  if (TString(treeName).BeginsWith("SpZ")) {
    iXZ = 1;
  } else {
    iXZ = 0;
  }
  if (iXZ < 0) return;
  // List of branches
  // Init
  tree->SetMakeClass(1);
  TBranch        *b_coord;   //!
  Var_t xC;
  tree->SetBranchAddress("coord", &xC.sec, &b_coord);
  struct Fit_t {
    Double_t A; Double_t dA;
    Double_t mu; Double_t dmu;
    Double_t sigma; Double_t dsigma;
    Double_t chi2; 
  };
  Fit_t fit;
  TBranch        *b_fit;   //!
  tree->SetBranchAddress("fit", &fit.A, &b_fit);
  // Global data parameters 
  Int_t nVars       = 4;
  //  TMultiDimFit* MDfit = new TMultiDimFit(nVars, TMultiDimFit::kMonomials,"v");
  MDfit = new TMultiDimFit(nVars, TMultiDimFit::kChebyshev,"v");
  //                  phiL,eta,zL, X 
  Int_t mPowers[]   = { 6 ,   6, 3, 6 };
  if (prompt) mPowers[2] = 1;
  MDfit->SetMaxPowers(mPowers);
  MDfit->SetMaxFunctions(10000);
  MDfit->SetMaxStudy(10000);
  MDfit->SetMaxTerms(30);
  MDfit->SetPowerLimit(1);
  MDfit->SetMinAngle(10);
  MDfit->SetMaxAngle(10);
  MDfit->SetMinRelativeError(.01);
  // Print out the start parameters
  MDfit->Print("p");
  Long64_t nentries = tree->GetEntriesFast();
  Long64_t nbytes = 0, nb = 0;
  Int_t nAccepted = 0;
  Double_t X[4];
  for (Long64_t jentry=0; jentry<nentries;jentry++) {
    Long64_t ientry = tree->LoadTree(jentry);
    if (ientry < 0) break;
    nb = tree->GetEntry(jentry);   nbytes += nb;
    if ((sec >= 0 && sec < 2) && (xC.sec < secs[sec] || xC.sec > secs[sec+1])) continue;
    if ((row >= 0 && row < 2) && (xC.row < rows[row] || xC.row > rows[row+1])) continue;
    if (N > 0 && TMath::Abs(xC.Npads - N) > 0.5)                               continue;
    if (  prompt && xC.zL <  195)                                              continue;
    if (! prompt && xC.zL >= 195)                                              continue;
    if (fit.dA             >= 0.5*fit.A)                                       continue;
    if (TMath::Abs(fit.mu) >= 0.5)                                             continue;
    if (fit.dsigma         >= 0.05)                                            continue;
    nAccepted++;
    X[0] = TMath::Tan(xC.phiL);
    X[1] = xC.eta;
    X[2] = xC.zL;
    if (iXZ) X[3] =            xC.xPad;
    else     X[3] = TMath::Abs(xC.xPad);
    Double_t D = fit.mu;
    if (! iXZ) D = TMath::Sign(D,xC.xPad); // abs(xPad)
    Double_t E = fit.dmu*fit.dmu;
    if (MS) {D = TMath::Log(fit.sigma); E = TMath::Power(fit.dsigma/fit.sigma,2);}
    // Add the row to the fit object
    MDfit->AddRow(X,D,E);
    MDfit->AddTestRow(X,D,E); // to make chi2
  }
  cout << "Accepted " << nAccepted << " entries and sample size " << MDfit->GetSampleSize() << endl;
  if (nAccepted < 100) return;
  // Reset variable limits
  TVectorD max(4), min(4);
  max(0) = TMath::Tan(1.1); min(0) = - max(0);
  max(1) = TMath::Tan(1.1); min(1) = - max(1);
  if (! prompt) {max(2) = 195; min(2) =   0;}
  else          {max(2) = 210; min(2) = 195;}
  max(3) = 0.5; min(3) = -0.5; 
  if (iXZ == 0) min(3) =  0.0; // Abs(xPad) 
  MDfit->Print("s");
#ifdef __MUDIFI_EXT__
  MDfit->SetMaxVariables(max);
  MDfit->SetMinVariables(min);
#endif
 // Print out the statistics
  MDfit->Print("s");
#ifdef __MUDIFI_EXT__
  // Book histograms 
  MDfit->MakeHistograms();
#endif
  // Find the parameterization 
  MDfit->FindParameterization();
  // Print coefficents 
  MDfit->Print("pscr");
  Double_t chi2 = MDfit->MakeChi2();
  Int_t    NDF  = MDfit->GetSampleSize() - MDfit->GetNCoefficients();
  cout << "Chi2 " << MDfit->MakeChi2() << " and Chi2/NDF = " << chi2/NDF << endl;
  // Write code to file  

  TString output(Form("MDF%s_s_%i_r_%i_N_%i_B_%i_p_%i",treeName,sec,row,N,MS,prompt));
  //  MDfit->MakeMethod(output);
  MDfit->MakeCode(output);
  Int_t nV = MDfit->GetNVariables();
  Int_t nCoef = MDfit->GetNCoefficients();
  TArrayI nMaxP(nV);
  TArrayI Code(nCoef);
  TArrayD Coef(nCoef);
  TArrayD dCoef(nCoef);
  for (Int_t i = 0; i < nCoef; i++) {
    Int_t code = 0;
    for (Int_t j = 0; j < nV; j++) {
      code *= 10;
      Int_t p  =  MDfit->GetPowers()[MDfit->GetPowerIndex()[i]*nV + j];
      if (nMaxP[j] < p) nMaxP[j] = p;
      code += p;
    }
    Code[i] = code;
    Coef[i] = (*MDfit->GetCoefficients())(i);
#ifdef __MUDIFI_EXT__
    dCoef[i] = (*MDfit->GetCoefficientsRMS())(i);
#endif
  }
  TString Out("");
  Out += gSystem->BaseName(gDirectory->GetName());
  Out.ReplaceAll(".root","");
  Out += ".C";
  ofstream out;
  if (gSystem->AccessPathName(Out)) out.open(Out, ios::out); //"Results.list",ios::out | ios::app);
  else                              out.open(Out, ios::app);
  out << "  { //" << output << "  parameterization with chi2/ndf = " <<  chi2/NDF << endl;
  out << "    Double_t  minV[" << nV << "] = " << PrintLine(nV,(*MDfit->GetMinVariables())) << endl;
  out << "    Double_t  maxV[" << nV << "] = " << PrintLine(nV,(*MDfit->GetMaxVariables())) << endl;
  out << "    Double_t meanV[" << nV << "] = " << PrintLine(nV,(*MDfit->GetMeanVariables())) << endl;
#ifdef __MUDIFI_EXT__
  out << "    Int_t   MaxPow[" << nV    << "] = " << PrintLine(nV,MDfit->GetMaxPowersFinal()) << endl;
#endif
  out << "    Int_t     Code[" << nCoef << "] = " << PrintLine(nCoef,Code.GetArray()) << endl;
  out << "    Double_t  Coef[" << nCoef << "] = " << PrintLine(nCoef,Coef.GetArray()) << endl;
  out << "    Double_t dCoef[" << nCoef << "] = " << PrintLine(nCoef,dCoef.GetArray()) << endl;
  out << "    MDFp" << treeName 
      << "[" << sec << "][" << row << "][" << N << "][" << MS << "][" << prompt << "] = "
      << "new TMDFParameters("<< MDfit->GetMeanQuantity() << "," << nV << ",minV,maxV,meanV,MaxPow," << nCoef << ",Code,Coef,dCoef);" << endl; 
  out << "  }" << endl;
  out.close();
  delete MDfit;
}
//________________________________________________________________________________
void MDFerrorParameterization2(const Char_t *treeName = "FitP", Int_t iXZ = 0, Int_t sec = 0, Int_t row = 0, Int_t MS = 1, Int_t prompt = 0) {
#ifdef __TEST__
  //void TpcHitErrors() {
  //                      xz s  r  S  p
  TMDFParameters *MDFpFitP[2][2][8][2][2]; memset(MDFpFitP, 0, sizeof(MDFpFitP));
  { //MDFFitP_xz_0_s_0_r_0_B_0_p_0  parameterization with chi2/ndf = 3.77379
    Double_t  minV[7] = {         2,         5,     -1.96,     -1.96,         0,         3,      -0.5};
    Double_t  maxV[7] = {         7,        17,      1.96,      1.96,       195,         9,       0.5};
    Double_t meanV[7] = {    0.0655,    -0.587,    -0.133,     0.401,   -0.0825,    -0.052,   0.00444};
    Int_t   MaxPow[7] = {         4,         3,         3,         1,         2,         1,         3};
    Int_t     Code[30] = {
   1111111,   2112122,   5111112,   1111112,   1111122,   1111212,   2111112,   1221111,   2111122,   4111112,
   4211112,   4121112,   3131112,   2141112,   3111114,   1131114,   1211112,   2121111,   1141111,   1211212,
   1222111,   2121122,   2211122,   1221212,   2411112,   4111212,   2121312,   1131312,   2121114,   2111214};
    Double_t  Coef[30] = {
   0.00218,    -0.145,    0.0376,    0.0349,  -0.00548,   -0.0135,     0.089,   -0.0353,     0.143,    0.0593,
    0.0613,    -0.285,    -0.061,     0.159,    0.0316,   -0.0228,    0.0119,    0.0289,   0.00836,    -0.031,
   -0.0397,    -0.254,     0.168,   -0.0255,    0.0137,   -0.0243,   -0.0795,  -0.00601,   -0.0602,    0.0142};
    Double_t dCoef[30] = {
  2.11e-05,    0.0018,  0.000193,  0.000263,  0.000271,  0.000237,  0.000465,  0.000432,   0.00188,  0.000251,
  0.000347,   0.00139,    0.0004,  0.000966,  0.000167,  0.000161,  0.000198,  0.000384,  0.000134,  0.000451,
  0.000817,   0.00513,   0.00246,   0.00151,   0.00022,  0.000261,   0.00169,  0.000104,  0.000798,  0.000239};
    MDFpFitP[0][0][0][0][0] = new TMDFParameters(-0.002498,7,minV,maxV,meanV,MaxPow,30,Code,Coef,dCoef);
  }
  { //MDFFitP_xz_0_s_0_r_0_B_0_p_1  parameterization with chi2/ndf = 2.65169
    Double_t  minV[7] = {         2,         5,     -1.96,     -1.96,       195,         3,      -0.5};
    Double_t  maxV[7] = {         7,        17,      1.96,      1.96,       210,         9,       0.5};
    Double_t meanV[7] = {    -0.484,    -0.593,    -0.102,     0.754,    -0.328,   -0.0757,   0.00986};
    Int_t   MaxPow[7] = {         3,         3,         2,         5,         0,         2,         4};
    Int_t     Code[30] = {
   1111111,   1111114,   2221111,   4111112,   3131112,   1112111,   1121111,   3111111,   1213111,   4121111,
   2111131,   1212121,   1131121,   1222131,   2421111,   1314111,   1111122,   1321111,   1121122,   1123111,
   1223121,   1322121,   1431111,   1116111,   1114113,   1111125,   1121112,   1111123,   2112112,   2211112};
    Double_t  Coef[30] = {
   -0.0998,  0.000499,    -0.492,   -0.0842,    0.0632,     0.109,    -0.117,   -0.0163,    0.0215,    0.0592,
     0.037,     -0.13,    0.0768,     0.242,    -0.171,    0.0399,    -0.022,   -0.0403,    -0.183,   -0.0232,
      -1.1,     0.453,   0.00757,    0.0124,  -0.00856,   -0.0257,    -0.015,   -0.0226,    0.0189,    0.0152};
    Double_t dCoef[30] = {
   0.00622,  0.000286,    0.0267,   0.00117,   0.00106,   0.00906,    0.0153,    0.0026,   0.00418,    0.0174,
   0.00432,    0.0154,   0.00663,    0.0249,    0.0149,   0.00193,   0.00205,    0.0118,    0.0184,   0.00641,
    0.0629,    0.0296,   0.00119,  0.000783,  0.000551,   0.00164,   0.00325,   0.00297,     0.003,   0.00297};
    MDFpFitP[0][0][0][0][1] = new TMDFParameters(0.00383095,7,minV,maxV,meanV,MaxPow,30,Code,Coef,dCoef);
  }
  { //MDFFitP_xz_0_s_0_r_0_B_1_p_0  parameterization with chi2/ndf = 3.43851
    Double_t  minV[7] = {         2,         5,     -1.96,     -1.96,         0,         3,      -0.5};
    Double_t  maxV[7] = {         7,        17,      1.96,      1.96,       195,         9,       0.5};
    Double_t meanV[7] = {    0.0655,    -0.587,    -0.133,     0.401,   -0.0825,    -0.052,   0.00444};
    Int_t   MaxPow[7] = {         3,         3,         4,         2,         2,         3,         2};
    Int_t     Code[30] = {
   1111111,   1151111,   1111211,   1113121,   2111111,   1121121,   2111211,   1113111,   1231111,   1141111,
   1111121,   1122111,   3211111,   1122121,   2221111,   1321111,   1331111,   1421111,   3121121,   1322111,
   3111231,   2112141,   2151111,   4121211,   4211211,   1111113,   1111311,   2112111,   1312111,   1411111};
    Double_t  Coef[30] = {
      0.88,     -1.13,    -0.203,     0.339,    -0.326,      -0.3,     0.346,    0.0199,     0.193,   -0.0199,
    -0.187,      1.44,    0.0108,     0.744,     0.852,      1.01,   -0.0959,    -0.157,      1.89,     -1.07,
    0.0568,    -0.215,     0.531,    -0.751,     0.152,    0.0143,    0.0312,     0.128,   -0.0418,   -0.0407};
    Double_t dCoef[30] = {
   0.00186,   0.00137,   0.00117,   0.00274,   0.00296,    0.0199,   0.00226,  0.000818,   0.00293,   0.00192,
   0.00198,   0.00853,   0.00129,    0.0261,   0.00723,   0.00875,   0.00198,   0.00491,    0.0158,    0.0118,
   0.00131,   0.00192,   0.00277,   0.00444,   0.00134,   0.00025,  0.000311,   0.00276,   0.00241,  0.000838};
    MDFpFitP[0][0][0][1][0] = new TMDFParameters(-1.96395,7,minV,maxV,meanV,MaxPow,30,Code,Coef,dCoef);
  }
  { //MDFFitP_xz_0_s_0_r_0_B_1_p_1  parameterization with chi2/ndf = 3.51555
    Double_t  minV[7] = {         2,         5,     -1.96,     -1.96,       195,         3,      -0.5};
    Double_t  maxV[7] = {         7,        17,      1.96,      1.96,       210,         9,       0.5};
    Double_t meanV[7] = {    -0.484,    -0.593,    -0.102,     0.754,    -0.328,   -0.0757,   0.00986};
    Int_t   MaxPow[7] = {         3,         4,         4,         5,         0,         2,         4};
    Int_t     Code[30] = {
   1111111,   1151111,   1311111,   2412111,   1121111,   1113111,   2131111,   2212121,   4121111,   1124121,
   1111121,   1111113,   1111115,   1421111,   3211121,   1511111,   3121131,   1341111,   3221121,   1314111,
   1111122,   1121112,   1113121,   1221113,   1321121,   4121112,   1313112,   4121121,   3222111,   1116111};
    Double_t  Coef[30] = {
         2,     -1.68,    0.0984,     0.461,     0.914,      0.13,     -1.02,    -0.422,     -1.46,      2.31,
    -0.169,   -0.0325,    0.0312,      1.57,      1.27,    -0.135,      2.03,    -0.412,         3,     0.156,
    0.0721,     0.273,   0.00366,     0.365,      1.34,    -0.498,   -0.0584,     0.984,     -1.13,    0.0504};
    Double_t dCoef[30] = {
    0.0241,    0.0138,    0.0238,    0.0323,     0.155,    0.0127,     0.031,      0.27,     0.138,      0.24,
     0.071,    0.0065,   0.00285,     0.145,     0.122,    0.0161,     0.166,    0.0735,     0.492,    0.0168,
    0.0163,    0.0662,    0.0549,    0.0595,     0.356,       0.1,    0.0134,     0.213,     0.275,   0.00767};
    MDFpFitP[0][0][0][1][1] = new TMDFParameters(-2.36276,7,minV,maxV,meanV,MaxPow,30,Code,Coef,dCoef);
  }
  { //MDFFitP_xz_0_s_0_r_1_B_0_p_0  parameterization with chi2/ndf = 2.94159e+12
    Double_t  minV[7] = {         2,         5,     -1.96,     -1.96,         0,         3,      -0.5};
    Double_t  maxV[7] = {         7,        17,      1.96,      1.96,       195,         9,       0.5};
    Double_t meanV[7] = {    -0.113,    -0.373,    -0.167,     0.235,    0.0484,   -0.0397,   0.00684};
    Int_t   MaxPow[7] = {         4,         1,         2,         3,         1,         3,         3};
    Int_t     Code[30] = {
   1111111,   5111112,   1121111,   3111112,   4111112,   3111114,   1111112,   2121111,   1111114,   2111114,
   2111132,   1112112,   2111112,   1221111,   1222111,   1211131,   2212211,   1111142,   1131122,   1132112,
   4121112,   4112112,   1112122,   1212121,   1212211,   1121114,   1124111,   1121132,   1231112,   2221112};
    Double_t  Coef[30] = {
   0.00334,    0.0746,   0.00125,   -0.0156,   -0.0369,   -0.0292,    -0.021,    0.0375,   -0.0125,    0.0376,
    0.0478,  -0.00921,    0.0817,   -0.0109,    -0.129,  -0.00499,    0.0493,   -0.0161,    0.0684,   -0.0257,
    0.0255,    0.0263,   -0.0344,    0.0361,    0.0153,    0.0101,    0.0182,   -0.0261,    0.0128,    0.0234};
    Double_t dCoef[30] = {
  1.03e-05,   5.8e-05,  0.000101,  0.000137,  0.000122,  4.65e-05,  0.000111,    0.0001,  5.77e-05,  9.86e-05,
  4.22e-08,  8.59e-05,  0.000181,   0.00013,  3.99e-08,   1.5e-08,  0.000211,  2.69e-08,  9.82e-08,  5.63e-05,
  0.000161,  9.07e-05,  1.41e-08,  2.47e-08,  0.000211,  1.69e-05,  1.29e-08,  1.49e-08,  6.53e-05,  0.000345};
    MDFpFitP[0][0][1][0][0] = new TMDFParameters(-0.00144127,7,minV,maxV,meanV,MaxPow,30,Code,Coef,dCoef);
  }
  { //MDFFitP_xz_0_s_0_r_1_B_0_p_1  parameterization with chi2/ndf = 19.2178
    Double_t  minV[7] = {         2,         5,     -1.96,     -1.96,       195,         3,      -0.5};
    Double_t  maxV[7] = {         7,        17,      1.96,      1.96,       210,         9,       0.5};
    Double_t meanV[7] = {    -0.474,    -0.342,    -0.186,     0.551,    -0.313,   -0.0705, -0.000552};
    Int_t   MaxPow[7] = {         3,         3,         1,         3,         0,         2,         3};
    Int_t     Code[30] = {
   1111111,   2211111,   1212121,   2222121,   4111112,   1111121,   3111112,   2121121,   2111114,   1412111,
   2212131,   1214121,   2321121,   1211121,   1111114,   1221121,   2222111,   3113121,   2421111,   1224111,
   3321111,   2313111,   3111114,   2121114,   1122114,   1211111,   3111111,   1121121,   2112111,   1221111};
    Double_t  Coef[30] = {
   -0.0367,    -0.084,   -0.0143,      1.91,   -0.0664,  -0.00927,    -0.042,       0.4,    0.0179,   -0.0212,
   -0.0495,    -0.112,    -0.441,   -0.0598,  -0.00452,    0.0678,     -0.96,    0.0967,    -0.151,     0.158,
   -0.0794,   -0.0404,   -0.0259,     0.106,    0.0639,   -0.0297,    0.0102,    0.0791,   -0.0287,    0.0868};
    Double_t dCoef[30] = {
  0.000666,   0.00205,   0.00574,    0.0317,  0.000367,  0.000625,  0.000373,    0.0142,   0.00155,  0.000572,
   0.00212,   0.00257,    0.0136,   0.00371,   0.00101,    0.0112,    0.0106,   0.00176,   0.00322,   0.00378,
   0.00142,  0.000633,  0.000565,   0.00215,   0.00211,   0.00128,  0.000388,   0.00664,   0.00141,   0.00414};
    MDFpFitP[0][0][1][0][1] = new TMDFParameters(0.0271297,7,minV,maxV,meanV,MaxPow,30,Code,Coef,dCoef);
  }
  { //MDFFitP_xz_0_s_0_r_1_B_1_p_0  parameterization with chi2/ndf = 5.55053e+15
    Double_t  minV[7] = {         2,         5,     -1.96,     -1.96,         0,         3,      -0.5};
    Double_t  maxV[7] = {         7,        17,      1.96,      1.96,       195,         9,       0.5};
    Double_t meanV[7] = {    -0.113,    -0.373,    -0.167,     0.235,    0.0484,   -0.0397,   0.00684};
    Int_t   MaxPow[7] = {         3,         2,         4,         3,         1,         3,         2};
    Int_t     Code[30] = {
   1111111,   1131111,   1151111,   1231111,   2111131,   1121111,   1111131,   1111221,   2121111,   1341111,
   1112121,   1211211,   2131111,   1112113,   1141111,   1132111,   1131121,   1311121,   2211121,   4111111,
   1123111,   2114111,   2141111,   3211121,   1321121,   3121131,   4112121,   3111141,   1251111,   2151111};
    Double_t  Coef[30] = {
     0.549,     0.559,    -0.542,      0.13,    -0.185,      2.58,    -0.152,    0.0192,     -2.63,   -0.0899,
     0.753,    0.0858,    -0.212,    -0.102,     0.803,    -0.141,     0.523,     0.145,    -0.234,    0.0659,
     0.525,     0.129,    -0.929,     0.162,     0.508,    -0.405,     0.553,    0.0846,    -0.193,     0.597};
    Double_t dCoef[30] = {
  5.38e-12,   5.4e-12,  5.45e-12,  1.07e-11,   6.3e-12,  1.35e-10,  6.29e-12,  2.59e-11,  1.35e-10,  9.05e-11,
  3.38e-11,  1.41e-11,  5.42e-12,  1.58e-11,  4.53e-11,  9.28e-12,  1.95e-11,  3.77e-11,  3.87e-11,  5.39e-12,
  3.61e-10,  5.91e-12,  4.53e-11,  3.91e-11,  1.02e-09,  1.55e-10,  3.38e-11,  7.91e-12,  1.08e-11,  5.47e-12};
    MDFpFitP[0][0][1][1][0] = new TMDFParameters(-2.05937,7,minV,maxV,meanV,MaxPow,30,Code,Coef,dCoef);
  }
  { //MDFFitP_xz_0_s_0_r_1_B_1_p_1  parameterization with chi2/ndf = 2.90869e+14
    Double_t  minV[7] = {         2,         5,     -1.96,     -1.96,       195,         3,      -0.5};
    Double_t  maxV[7] = {         7,        17,      1.96,      1.96,       210,         9,       0.5};
    Double_t meanV[7] = {    -0.474,    -0.342,    -0.186,     0.551,    -0.313,   -0.0705, -0.000552};
    Int_t   MaxPow[7] = {         4,         3,         5,         3,         0,         2,         4};
    Int_t     Code[30] = {
   1111111,   1121111,   2111131,   1211111,   1151111,   3311111,   2121131,   2151111,   2211111,   1312111,
   1322111,   3211121,   1411131,   1112111,   1311111,   1112121,   1112113,   3121111,   1222111,   2114111,
   2221121,   1161111,   1313121,   1212111,   3211111,   2221111,   3111121,   1111115,   1421111,   5111111};
    Double_t  Coef[30] = {
     0.869,     0.816,   -0.0305,    -0.265,    -0.941,     0.147,      1.37,      1.21,   -0.0509,     0.837,
      1.19,     0.108,     0.117,     0.722,    0.0121,    0.0893,   -0.0541,     -1.44,     -3.42,      0.27,
     -3.28,    -0.172,     0.593,    -0.443,     0.275,      0.46,    -0.243,    0.0243,    -0.276,    0.0332};
    Double_t dCoef[30] = {
  7.07e-11,  2.78e-09,  7.59e-11,  1.41e-10,  7.11e-11,  1.41e-10,  2.98e-09,  7.11e-11,  1.41e-10,  2.44e-10,
  9.58e-09,  7.59e-10,  7.59e-11,  1.22e-10,  1.41e-10,  6.55e-10,  1.76e-10,  2.78e-09,  9.58e-09,  7.36e-11,
  2.98e-08,  5.57e-10,  2.31e-09,  2.44e-10,  1.41e-10,  5.55e-09,  3.79e-10,  1.77e-09,  2.78e-09,  7.07e-11};
    MDFpFitP[0][0][1][1][1] = new TMDFParameters(-2.2467,7,minV,maxV,meanV,MaxPow,30,Code,Coef,dCoef);
  }
  { //MDFFitP_xz_0_s_1_r_0_B_0_p_0  parameterization with chi2/ndf = 3.9527
    Double_t  minV[7] = {         2,         5,     -1.96,     -1.96,         0,         3,      -0.5};
    Double_t  maxV[7] = {         7,        17,      1.96,      1.96,       195,         9,       0.5};
    Double_t meanV[7] = {    0.0303,     -0.69,    -0.122,     -0.36,     -0.16,   -0.0988,   0.00413};
    Int_t   MaxPow[7] = {         4,         1,         3,         2,         1,         2,         3};
    Int_t     Code[30] = {
   1111111,   2111122,   2111222,   5111112,   1111122,   2111112,   1111132,   3111122,   4111112,   4211112,
   3131112,   3121122,   1141122,   3111114,   1131114,   1111212,   1121112,   1222111,   3121112,   3122111,
   1222121,   4121112,   2141112,   3122112,   2213112,   2122212,   4111122,   2211222,   2121222,   1211112};
    Double_t  Coef[30] = {
 -0.000328,     0.136,    -0.126,    0.0562,    0.0278,     0.168,   -0.0301,     0.027,    0.0876,     0.025,
   -0.0799,   -0.0481,   -0.0347,     0.036,    -0.021,  -0.00615,   -0.0288,    -0.116,    -0.164,    0.0643,
     -0.15,    -0.315,     0.199,    -0.136,   -0.0215,     0.268,    0.0528,     -0.22,     0.536,  -0.00639};
    Double_t dCoef[30] = {
  2.16e-05,   0.00157,    0.0032,  0.000251,  0.000908,  0.000713,   0.00033,   0.00108,  0.000359,  0.000371,
  0.000498,   0.00885,   0.00275,  0.000163,  0.000146,  0.000106,   0.00224,   0.00124,   0.00268,   0.00082,
   0.00264,   0.00244,   0.00194,   0.00174,  0.000565,   0.00685,  0.000889,    0.0035,    0.0111,  0.000212};
    MDFpFitP[0][1][0][0][0] = new TMDFParameters(0.00115137,7,minV,maxV,meanV,MaxPow,30,Code,Coef,dCoef);
  }
  { //MDFFitP_xz_0_s_1_r_0_B_0_p_1  parameterization with chi2/ndf = 1.43212
    Double_t  minV[7] = {         2,         5,     -1.96,     -1.96,       195,         3,      -0.5};
    Double_t  maxV[7] = {         7,        17,      1.96,      1.96,       210,         9,       0.5};
    Double_t meanV[7] = {     -0.53,    -0.705,   -0.0781,    -0.766,    -0.333,   -0.0861,   -0.0193};
    Int_t   MaxPow[7] = {         2,         2,         2,         3,         0,         2,         5};
    Int_t     Code[30] = {
   1111111,   2111112,   1111112,   1112112,   1121122,   1212112,   1211114,   2121122,   1231131,   2111111,
   1111113,   1311111,   1112121,   1311121,   1121123,   1223111,   1214111,   1121132,   1111116,   2213112,
   3211113,   1113123,   1121121,   1122111,   1121113,   1111115,   2311113,   3112113,   1311123,   3111121};
    Double_t  Coef[30] = {
    -0.133,     0.243,     0.155,     0.128,     0.178,   -0.0139,    0.0223,      1.45,    -0.227,    0.0338,
   -0.0325,   -0.0631,     0.171,   -0.0517,    -0.985,    -0.199,    0.0677,   -0.0653,  -0.00322,     0.178,
    0.0737,    -0.381,    -0.478,     0.136,    -0.111,   -0.0036,  0.000104,   -0.0289,    0.0581,   -0.0342};
    Double_t dCoef[30] = {
    0.0334,    0.0206,     0.041,    0.0424,     0.351,    0.0279,   0.00553,      0.83,    0.0489,    0.0153,
    0.0149,     0.019,     0.046,    0.0438,     0.275,    0.0736,    0.0229,    0.0304,   0.00152,     0.103,
    0.0397,     0.124,     0.237,    0.0465,    0.0392,   0.00315,    0.0173,    0.0244,    0.0463,    0.0744};
    MDFpFitP[0][1][0][0][1] = new TMDFParameters(0.00408299,7,minV,maxV,meanV,MaxPow,30,Code,Coef,dCoef);
  }
  { //MDFFitP_xz_0_s_1_r_0_B_1_p_0  parameterization with chi2/ndf = 2.9514
    Double_t  minV[7] = {         2,         5,     -1.96,     -1.96,         0,         3,      -0.5};
    Double_t  maxV[7] = {         7,        17,      1.96,      1.96,       195,         9,       0.5};
    Double_t meanV[7] = {    0.0303,     -0.69,    -0.122,     -0.36,     -0.16,   -0.0988,   0.00413};
    Int_t   MaxPow[7] = {         3,         3,         4,         3,         2,         1,         0};
    Int_t     Code[30] = {
   1111111,   1151111,   1111211,   1111121,   1311111,   1111221,   1121121,   2112111,   1421111,   3111111,
   2111121,   2111211,   1122111,   1411111,   1123111,   1221211,   2231111,   3113121,   1242111,   4121211,
   2111111,   1112211,   2212111,   1312111,   1121221,   1211221,   3121111,   1112311,   1124111,   2311211};
    Double_t  Coef[30] = {
     0.783,    -0.876,    -0.307,    -0.326,    0.0589,     0.225,     -1.79,     -0.78,    -0.746,      0.22,
     0.102,     0.414,     -2.16,   -0.0861,     0.526,     0.608,     0.705,    -0.256,      1.18,    -0.392,
    -0.432,   -0.0236,    -0.797,     0.135,     -1.11,      0.44,       0.3,   -0.0463,     0.303,    0.0824};
    Double_t dCoef[30] = {
   0.00177,   0.00175,   0.00109,   0.00136,   0.00145,   0.00582,   0.00908,   0.00826,   0.00335,   0.00113,
   0.00307,   0.00209,    0.0195,  0.000882,   0.00593,   0.00521,   0.00549,   0.00255,    0.0104,   0.00444,
   0.00469,   0.00184,   0.00981,   0.00176,    0.0198,   0.00728,    0.0059,  0.000808,   0.00646,   0.00182};
    MDFpFitP[0][1][0][1][0] = new TMDFParameters(-1.9867,7,minV,maxV,meanV,MaxPow,30,Code,Coef,dCoef);
  }
  { //MDFFitP_xz_0_s_1_r_0_B_1_p_1  parameterization with chi2/ndf = 3.96049
    Double_t  minV[7] = {         2,         5,     -1.96,     -1.96,       195,         3,      -0.5};
    Double_t  maxV[7] = {         7,        17,      1.96,      1.96,       210,         9,       0.5};
    Double_t meanV[7] = {     -0.53,    -0.705,   -0.0781,    -0.766,    -0.333,   -0.0861,   -0.0193};
    Int_t   MaxPow[7] = {         2,         2,         1,         2,         0,         2,         4};
    Int_t     Code[30] = {
   1111111,   2111111,   1111121,   1111122,   1113111,   1221111,   1121111,   1112112,   1121121,   1111123,
   1111132,   1211122,   2311111,   1113121,   1311113,   1321112,   2111112,   1121113,   1211113,   3111113,
   2311121,   1321121,   1111124,   1121124,   1121115,   1121112,   3111121,   1111115,   1211114,   1113113};
    Double_t  Coef[30] = {
  -0.00203,     0.513,     -1.38,     0.353,     0.379,       5.5,       1.6,     -1.56,     -9.72,     0.232,
      1.06,     0.153,    -0.169,      2.09,     0.109,     -2.58,     0.226,    -0.556,    -0.259,     0.416,
     -1.45,      6.58,   -0.0528,      2.03,    -0.371,    -0.347,     0.443,   -0.0285,    0.0196,   -0.0513};
    Double_t dCoef[30] = {
     0.128,     0.206,     0.692,     0.798,     0.237,      2.16,       1.3,     0.434,      1.98,     0.379,
      0.36,      1.26,     0.137,      1.66,     0.127,      0.88,     0.194,     0.965,     0.307,     0.239,
      0.57,      3.42,      0.23,      1.79,     0.384,     0.598,     0.782,    0.0565,    0.0463,      0.17};
    MDFpFitP[0][1][0][1][1] = new TMDFParameters(-2.54197,7,minV,maxV,meanV,MaxPow,30,Code,Coef,dCoef);
  }
  { //MDFFitP_xz_0_s_1_r_1_B_0_p_0  parameterization with chi2/ndf = 3.23349e+12
    Double_t  minV[7] = {         2,         5,     -1.96,     -1.96,         0,         3,      -0.5};
    Double_t  maxV[7] = {         7,        17,      1.96,      1.96,       195,         9,       0.5};
    Double_t meanV[7] = {    -0.107,    -0.375,    -0.146,    -0.236,    0.0635,   -0.0319,   0.00607};
    Int_t   MaxPow[7] = {         4,         2,         4,         2,         0,         2,         3};
    Int_t     Code[30] = {
   1111111,   5111112,   1121111,   1131112,   4111112,   2111124,   2121111,   1322111,   4121112,   1211122,
   1222111,   1111124,   2111114,   2111132,   2113112,   1151112,   4112112,   3111114,   1131114,   2112114,
   1113114,   1211111,   1121112,   1221111,   1131111,   1111132,   1212112,   2112112,   2121112,   3111112};
    Double_t  Coef[30] = {
  -0.00724,    0.0693, -0.000511,   0.00555,   -0.0392,   0.00282,   -0.0349,    0.0246,    0.0228,   -0.0293,
   -0.0674,    0.0183,    0.0257,   0.00397,   -0.0386,   0.00463,   -0.0349,   -0.0261,    0.0282,    0.0429,
  -0.00664,  -0.00523,   -0.0193,    0.0129,  -0.00447,    0.0225,    0.0184,  -0.00729,    0.0243,  -0.00949};
    Double_t dCoef[30] = {
  1.74e-11,  1.94e-11,  5.18e-10,  1.95e-11,  1.94e-11,  2.12e-10,  5.18e-10,   1.8e-09,  5.72e-10,  1.69e-10,
   1.8e-09,  2.12e-10,  4.92e-11,  2.16e-11,  5.29e-11,  1.96e-11,  3.38e-11,  4.92e-11,  4.93e-11,  8.54e-11,
  1.34e-10,  3.48e-11,  5.72e-10,  1.04e-09,  1.74e-11,  2.16e-11,  6.76e-11,  3.38e-11,  5.72e-10,  1.94e-11};
    MDFpFitP[0][1][1][0][0] = new TMDFParameters(0.00258197,7,minV,maxV,meanV,MaxPow,30,Code,Coef,dCoef);
  }
  { //MDFFitP_xz_0_s_1_r_1_B_0_p_1  parameterization with chi2/ndf = 25.1603
    Double_t  minV[7] = {         2,         5,     -1.96,     -1.96,       195,         3,      -0.5};
    Double_t  maxV[7] = {         7,        17,      1.96,      1.96,       210,         9,       0.5};
    Double_t meanV[7] = {    -0.551,    -0.486,     -0.18,    -0.546,    -0.281,    -0.148,   0.00913};
    Int_t   MaxPow[7] = {         4,         3,         4,         4,         0,         2,         3};
    Int_t     Code[30] = {
   1111111,   2222111,   1322121,   2111122,   4111112,   2121131,   2111124,   1122121,   5111112,   3321111,
   1422111,   1311111,   1111131,   2221111,   1113121,   1114111,   1132111,   1223111,   2321111,   2114111,
   1115111,   1322111,   3121121,   1151111,   1123121,   1141131,   2113131,   2421111,   1211113,   2121112};
    Double_t  Coef[30] = {
    -0.073,      -1.4,         1,   -0.0572,   -0.0313,   -0.0221,   -0.0971,     0.745,    0.0218,     0.153,
     0.136,     0.024,    0.0189,    -0.293,   -0.0321,    0.0777,      0.34,    0.0353,     0.123,    0.0483,
    0.0778,    -0.298,   -0.0309,   -0.0579,    -0.211,     0.034,   -0.0787,     0.131,  -0.00568,   -0.0428};
    Double_t dCoef[30] = {
    0.0013,    0.0171,    0.0157,  0.000428,  0.000257,   0.00659,  0.000363,    0.0116,  0.000262,   0.00185,
   0.00344,  0.000226,  0.000516,    0.0104,   0.00102,  0.000915,    0.0024,   0.00499,   0.00644,  0.000578,
  0.000727,    0.0052,   0.00406,  0.000681,    0.0091,   0.00106,   0.00107,   0.00382,  0.000196,  0.000861};
    MDFpFitP[0][1][1][0][1] = new TMDFParameters(-0.0291836,7,minV,maxV,meanV,MaxPow,30,Code,Coef,dCoef);
  }
  { //MDFFitP_xz_0_s_1_r_1_B_1_p_0  parameterization with chi2/ndf = 1.24986e+15
    Double_t  minV[7] = {         2,         5,     -1.96,     -1.96,         0,         3,      -0.5};
    Double_t  maxV[7] = {         7,        17,      1.96,      1.96,       195,         9,       0.5};
    Double_t meanV[7] = {    -0.107,    -0.375,    -0.146,    -0.236,    0.0635,   -0.0319,   0.00607};
    Int_t   MaxPow[7] = {         2,         3,         5,         2,         1,         2,         2};
    Int_t     Code[30] = {
   1111111,   1131111,   1151111,   1213131,   2151111,   2111111,   1111121,   1112111,   1111221,   1121121,
   1411111,   2111131,   1133121,   1161111,   1111211,   3111111,   2121121,   3122111,   1211111,   2121111,
   1122111,   1112113,   3211111,   2122111,   1121131,   1121221,   1122121,   3111211,   3131111,   2141111};
    Double_t  Coef[30] = {
     0.408,     0.041,    -0.694,    -0.208,     0.822,    -0.155,     0.237,    -0.298,     0.121,    -0.713,
      0.04,    -0.266,    -0.758,    -0.277,    -0.111,     0.172,      1.31,    -0.361,     0.049,     -1.06,
    -0.663,    0.0696,   -0.0809,     0.707,    -0.394,     0.579,      1.19,   -0.0308,    0.0654,    -0.343};
    Double_t dCoef[30] = {
   5.9e-12,  5.92e-12,  5.97e-12,  3.51e-11,  5.97e-12,   5.9e-12,  2.15e-11,  1.02e-11,  2.87e-11,  6.04e-10,
   5.9e-12,  6.87e-12,  5.41e-11,  3.22e-11,  7.76e-12,   5.9e-12,  6.04e-10,  2.77e-10,  1.17e-11,  1.59e-10,
  2.77e-10,  1.73e-11,  1.17e-11,  2.77e-10,  1.82e-10,  7.93e-10,  1.05e-09,  7.76e-12,  5.92e-12,  5.32e-11};
    MDFpFitP[0][1][1][1][0] = new TMDFParameters(-2.07811,7,minV,maxV,meanV,MaxPow,30,Code,Coef,dCoef);
  }
  { //MDFFitP_xz_0_s_1_r_1_B_1_p_1  parameterization with chi2/ndf = 9.21872
    Double_t  minV[7] = {         2,         5,     -1.96,     -1.96,       195,         3,      -0.5};
    Double_t  maxV[7] = {         7,        17,      1.96,      1.96,       210,         9,       0.5};
    Double_t meanV[7] = {    -0.551,    -0.486,     -0.18,    -0.546,    -0.281,    -0.148,   0.00913};
    Int_t   MaxPow[7] = {         4,         3,         5,         4,         0,         3,         2};
    Int_t     Code[30] = {
   1111111,   1121111,   2311111,   2111111,   2131131,   2151111,   2115111,   1211111,   1311111,   2122111,
   3211121,   1151111,   1161111,   3141111,   1111113,   1112121,   1312111,   3113111,   1421111,   2121122,
   4122111,   1311123,   3111111,   1321111,   1113112,   1111141,   1111133,   2123111,   4211111,   5121111};
    Double_t  Coef[30] = {
     0.804,     0.674,    -0.256,    -0.383,       0.4,      1.11,     0.474,    -0.337,    -0.313,      6.98,
      0.54,    -0.955,    -0.642,      1.03,     0.105,    -0.666,     -0.59,   -0.0067,       0.4,     -1.14,
      1.71,     0.282,    -0.158,     0.419,    0.0432,    0.0885,     0.152,      1.12,     0.045,     -0.33};
    Double_t dCoef[30] = {
     0.012,     0.041,   0.00908,    0.0204,    0.0143,    0.0174,   0.00502,     0.015,   0.00702,     0.113,
    0.0195,    0.0131,   0.00762,    0.0172,   0.00644,    0.0289,   0.00893,   0.00486,    0.0117,    0.0344,
    0.0449,   0.00702,   0.00968,    0.0217,    0.0025,   0.00594,    0.0072,    0.0456,   0.00808,     0.012};
    MDFpFitP[0][1][1][1][1] = new TMDFParameters(-2.2394,7,minV,maxV,meanV,MaxPow,30,Code,Coef,dCoef);
  }
  { //MDFFitP_xz_1_s_0_r_0_B_0_p_0  parameterization with chi2/ndf = 2.17071
    Double_t  minV[7] = {         2,         5,     -1.96,     -1.96,         0,         3,      -0.5};
    Double_t  maxV[7] = {         7,        17,      1.96,      1.96,       195,         9,       0.5};
    Double_t meanV[7] = {    0.0451,    -0.779,   -0.0672,     0.298,     -0.23,    -0.126,   0.00508};
    Int_t   MaxPow[7] = {         2,         3,         4,         4,         3,         1,         1};
    Int_t     Code[30] = {
   1111111,   1112111,   1211111,   1211211,   1112211,   1212111,   3211111,   1211221,   2113111,   1211311,
   2211211,   1413111,   1251111,   1115211,   2111121,   2112111,   1111411,   1112311,   1212211,   1114111,
   2114111,   1114211,   1114311,   2121312,   1151211,   1215111,   1211121,   2112121,   2121121,   2121211};
    Double_t  Coef[30] = {
    0.0716,   -0.0552,     0.142,    0.0476,    0.0349,    -0.039,    0.0054,    0.0233,    0.0293,    0.0046,
    0.0241,   0.00798,   -0.0314,   -0.0432,   -0.0424,   -0.0136,   0.00692,   -0.0158,  -0.00193,    -0.013,
   -0.0204,    0.0242,    0.0144,   -0.0707,    0.0186,    0.0236,   -0.0131,    0.0402,    -0.107,    0.0194};
    Double_t dCoef[30] = {
   0.00139,   0.00192,   0.00169,  0.000349,    0.0012,   0.00182,  0.000151,  0.000447,  0.000292,  0.000127,
  0.000373,  0.000413,  0.000196,  0.000247,  0.000597,  0.000715,  6.47e-05,  0.000573,   0.00121,  0.000298,
  0.000427,  0.000453,  0.000272,   0.00129,  0.000271,  0.000267,  0.000296,   0.00173,   0.00246,  0.000789};
    MDFpFitP[1][0][0][0][0] = new TMDFParameters(-0.070464,7,minV,maxV,meanV,MaxPow,30,Code,Coef,dCoef);
  }
  { //MDFFitP_xz_1_s_0_r_0_B_0_p_1  parameterization with chi2/ndf = 1.20145
    Double_t  minV[7] = {         2,         5,     -1.96,     -1.96,       195,         3,      -0.5};
    Double_t  maxV[7] = {         7,        17,      1.96,      1.96,       210,         9,       0.5};
    Double_t meanV[7] = {    -0.467,    -0.558,   -0.0612,     0.788,    -0.333,   -0.0598,    0.0125};
    Int_t   MaxPow[7] = {         2,         2,         1,         5,         0,         2,         5};
    Int_t     Code[30] = {
   1111111,   1223111,   1211111,   2121112,   3111112,   1123111,   1122113,   1121132,   2111111,   1112114,
   2122113,   1111112,   1121121,   2121111,   1221111,   1112113,   1321111,   1111115,   2111114,   1121123,
   3111113,   1211132,   1111116,   2113131,   1321122,   1116111,   1213113,   1311123,   2121114,   1122114};
    Double_t  Coef[30] = {
    0.0517,      1.22,    0.0578,     -1.13,    -0.158,     0.609,     0.357,     0.238,    0.0117,    0.0437,
     0.904,     -0.13,     0.207,     0.675,    -0.291,    0.0758,    -0.147,    0.0182,     0.186,     0.264,
    0.0708,   -0.0542,   -0.0223,  0.000948,     0.455,  -0.00767,     0.047,    0.0333,     0.279,     0.202};
    Double_t dCoef[30] = {
   0.00875,     0.179,    0.0111,     0.275,    0.0184,     0.106,    0.0765,    0.0627,    0.0106,   0.00803,
     0.181,     0.019,    0.0921,    0.0984,    0.0591,    0.0168,    0.0371,    0.0029,    0.0208,     0.124,
    0.0174,    0.0165,   0.00255,   0.00984,     0.236,   0.00447,    0.0158,    0.0139,     0.136,    0.0561};
    MDFpFitP[1][0][0][0][1] = new TMDFParameters(-0.0498539,7,minV,maxV,meanV,MaxPow,30,Code,Coef,dCoef);
  }
  { //MDFFitP_xz_1_s_0_r_0_B_1_p_0  parameterization with chi2/ndf = 1.70019
    Double_t  minV[7] = {         2,         5,     -1.96,     -1.96,         0,         3,      -0.5};
    Double_t  maxV[7] = {         7,        17,      1.96,      1.96,       195,         9,       0.5};
    Double_t meanV[7] = {    0.0451,    -0.779,   -0.0672,     0.298,     -0.23,    -0.126,   0.00508};
    Int_t   MaxPow[7] = {         2,         3,         1,         5,         2,         2,         0};
    Int_t     Code[30] = {
   1111111,   1112111,   1111221,   1112121,   1112221,   1114111,   1115111,   1412111,   1116111,   1111121,
   1111211,   1112211,   2111211,   3211111,   1411111,   1121111,   1121121,   1111311,   1221121,   1112311,
   2221111,   1311211,   1124111,   3211211,   2112311,   2311121,   3122111,   1212131,   3111131,   1114121};
    Double_t  Coef[30] = {
    0.0237,     0.579,    0.0119,     0.508,   -0.0373,     0.432,    -0.575,     0.152,     0.358,    -0.368,
    -0.281,     0.359,    0.0685,   -0.0786,   -0.0539,    -0.255,     -1.21,   -0.0594,    -0.958,    0.0947,
    -0.281,    0.0254,   -0.0796,    0.0803,    -0.125,     0.102,    -0.315,    -0.211,     0.063,     -0.26};
    Double_t dCoef[30] = {
   0.00195,    0.0038,   0.00415,   0.00605,   0.00943,   0.00215,   0.00144,   0.00174,   0.00107,    0.0035,
   0.00154,   0.00298,   0.00136,   0.00153,  0.000751,   0.00334,    0.0256,  0.000548,    0.0283,    0.0014,
   0.00443,  0.000874,   0.00406,   0.00127,   0.00173,   0.00288,   0.00753,   0.00439,   0.00126,   0.00407};
    MDFpFitP[1][0][0][1][0] = new TMDFParameters(-1.71301,7,minV,maxV,meanV,MaxPow,30,Code,Coef,dCoef);
  }
  { //MDFFitP_xz_1_s_0_r_0_B_1_p_1  parameterization with chi2/ndf = 1.01753
    Double_t  minV[7] = {         2,         5,     -1.96,     -1.96,       195,         3,      -0.5};
    Double_t  maxV[7] = {         7,        17,      1.96,      1.96,       210,         9,       0.5};
    Double_t meanV[7] = {    -0.467,    -0.558,   -0.0612,     0.788,    -0.333,   -0.0598,    0.0125};
    Int_t   MaxPow[7] = {         2,         2,         2,         3,         0,         1,         4};
    Int_t     Code[30] = {
   1111111,   1211111,   2311111,   2111111,   1211112,   2111115,   1111113,   1111114,   1321111,   1213111,
   1111115,   1112123,   1311113,   3112113,   1121111,   2111112,   2121111,   1114111,   1221113,   1321113,
   1121112,   2121112,   1123111,   2113111,   1121114,   2111123,   1113113,   2311121,   3121121,   2131121};
    Double_t  Coef[30] = {
     0.759,      1.56,    -0.981,     -0.35,     0.124,    -0.279,  -0.00892,   -0.0457,   -0.0959,    -0.238,
     -0.12,     0.118,     0.175,    -0.257,    -0.182,    0.0795,    -0.474,   -0.0874,    -0.545,     0.398,
    -0.181,   -0.0416,     0.105,    -0.135,    -0.107,      0.64,    0.0268,   -0.0646,     0.541,    -0.167};
    Double_t dCoef[30] = {
     0.304,     0.582,     0.298,     0.164,    0.0321,    0.0543,    0.0633,     0.011,     0.429,       0.1,
    0.0229,     0.208,    0.0471,    0.0895,     0.253,    0.0585,      0.33,    0.0374,     0.366,     0.362,
     0.127,     0.487,     0.141,      0.12,    0.0825,     0.367,    0.0418,     0.142,     0.431,     0.143};
    MDFpFitP[1][0][0][1][1] = new TMDFParameters(-1.2949,7,minV,maxV,meanV,MaxPow,30,Code,Coef,dCoef);
  }
  { //MDFFitP_xz_1_s_0_r_1_B_0_p_0  parameterization with chi2/ndf = 3.86959
    Double_t  minV[7] = {         2,         5,     -1.96,     -1.96,         0,         3,      -0.5};
    Double_t  maxV[7] = {         7,        17,      1.96,      1.96,       195,         9,       0.5};
    Double_t meanV[7] = {   -0.0808,    -0.663,   -0.0933,     0.185,   -0.0994,     -0.12,   0.00639};
    Int_t   MaxPow[7] = {         3,         3,         4,         4,         3,         2,         1};
    Int_t     Code[30] = {
   1111111,   1211111,   2111131,   1113211,   1213211,   1112111,   2212111,   1313121,   4112112,   2111111,
   1311111,   2111112,   1111221,   2111121,   1111311,   2111211,   1231111,   2112112,   1411111,   1111321,
   1111411,   1112311,   1114111,   1115111,   1132211,   1133121,   1412121,   1152111,   1121122,   1211122};
    Double_t  Coef[30] = {
    0.0464,      0.12,   0.00296,   0.00215,    -0.063,   0.00477,   0.00217,   -0.0397,   -0.0467,   -0.0436,
    0.0145,   -0.0027,   -0.0392,   -0.0507,   0.00412,  -0.00751,   0.00758,    0.0405,   0.00646,    0.0115,
   0.00633,   -0.0382,   -0.0134,    0.0174,   -0.0377,   -0.0402,    0.0628,    0.0263,    0.0486,   -0.0122};
    Double_t dCoef[30] = {
  0.000189,  0.000197,  0.000236,  0.000142,  0.000165,  0.000764,  0.000481,  0.000166,  0.000154,  0.000251,
   0.00013,  8.88e-05,  0.000247,  0.000317,  6.06e-05,  0.000103,  0.000131,  0.000261,  8.13e-05,   0.00014,
  3.53e-05,  0.000236,  0.000241,  0.000162,  0.000307,  0.000242,  0.000442,  0.000252,   0.00046,  0.000144};
    MDFpFitP[1][0][1][0][0] = new TMDFParameters(0.0256261,7,minV,maxV,meanV,MaxPow,30,Code,Coef,dCoef);
  }
  { //MDFFitP_xz_1_s_0_r_1_B_0_p_1  parameterization with chi2/ndf = 1.77406
    Double_t  minV[7] = {         2,         5,     -1.96,     -1.96,       195,         3,      -0.5};
    Double_t  maxV[7] = {         7,        17,      1.96,      1.96,       210,         9,       0.5};
    Double_t meanV[7] = {    -0.548,    -0.532,     -0.16,     0.535,    -0.333,    -0.128,    0.0167};
    Int_t   MaxPow[7] = {         3,         1,         1,         2,         0,         4,         3};
    Int_t     Code[30] = {
   1111111,   1211131,   2212131,   4121112,   1111121,   1121112,   2112112,   3121111,   4111111,   2121113,
   4111112,   1111112,   1112111,   1112121,   1121121,   3113111,   2222111,   2221112,   1113141,   1112151,
   3122121,   2221122,   3211113,   3111114,   2121114,   1212114,   1112112,   1111114,   1211113,   2111113};
    Double_t  Coef[30] = {
    0.0595,    -0.132,    -0.225,     0.233,     0.369,   -0.0776,     0.111,   -0.0265,      0.02,     0.066,
    -0.023,     0.013,   -0.0563,    -0.581,    0.0707,   -0.0147,     0.352,    -0.195,   -0.0642,    0.0263,
      0.43,    -0.647,   0.00268,   -0.0194,    -0.134,   -0.0415,    0.0346,  -0.00687,   -0.0204,    0.0191};
    Double_t dCoef[30] = {
   0.00278,   0.00363,   0.00771,   0.00956,    0.0204,   0.00676,   0.00634,   0.00396,  0.000888,   0.00884,
   0.00146,   0.00261,    0.0057,    0.0281,     0.022,   0.00189,    0.0297,    0.0325,   0.00575,   0.00356,
    0.0454,      0.08,    0.0027,   0.00101,    0.0112,   0.00433,   0.00546,   0.00143,   0.00256,   0.00264};
    MDFpFitP[1][0][1][0][1] = new TMDFParameters(0.0470938,7,minV,maxV,meanV,MaxPow,30,Code,Coef,dCoef);
  }
  { //MDFFitP_xz_1_s_0_r_1_B_1_p_0  parameterization with chi2/ndf = 3.0262
    Double_t  minV[7] = {         2,         5,     -1.96,     -1.96,         0,         3,      -0.5};
    Double_t  maxV[7] = {         7,        17,      1.96,      1.96,       195,         9,       0.5};
    Double_t meanV[7] = {   -0.0808,    -0.663,   -0.0933,     0.185,   -0.0994,     -0.12,   0.00639};
    Int_t   MaxPow[7] = {         2,         3,         5,         5,         2,         1,         0};
    Int_t     Code[30] = {
   1111111,   1112111,   1111311,   2112111,   2131111,   1312111,   1115111,   1214111,   1116111,   3111111,
   1111221,   1112211,   3211111,   1411111,   1211221,   2111221,   1113211,   2112211,   2211211,   1331111,
   1133111,   1114211,   2311121,   1141121,   1332111,   3132111,   1161111,   1331211,   1211211,   1221121};
    Double_t  Coef[30] = {
     0.596,     -2.04,   -0.0873,    -0.134,   -0.0974,    -0.173,    -0.888,     0.141,     0.525,    0.0458,
   -0.0272,      1.93,  -0.00316,    0.0613,    -0.207,   -0.0173,  -0.00656,    -0.366,    -0.191,    -0.102,
    -0.125,     0.596,     0.206,     0.278,     0.376,     0.199,    -0.038,    -0.113,    0.0871,    -0.295};
    Double_t dCoef[30] = {
   0.00153,    0.0062,  0.000228,   0.00261,  0.000766,   0.00528,   0.00137,   0.00299,     0.001,  0.000752,
   0.00298,   0.00568,  0.000826,  0.000504,   0.00352,   0.00237,   0.00167,   0.00267,   0.00105,  0.000882,
  0.000736,   0.00207,   0.00112,   0.00207,   0.00487,   0.00135,  0.000255,   0.00106,   0.00223,   0.00542};
    MDFpFitP[1][0][1][1][0] = new TMDFParameters(-1.8265,7,minV,maxV,meanV,MaxPow,30,Code,Coef,dCoef);
  }
  { //MDFFitP_xz_1_s_0_r_1_B_1_p_1  parameterization with chi2/ndf = 1.25745
    Double_t  minV[7] = {         2,         5,     -1.96,     -1.96,       195,         3,      -0.5};
    Double_t  maxV[7] = {         7,        17,      1.96,      1.96,       210,         9,       0.5};
    Double_t meanV[7] = {    -0.548,    -0.532,     -0.16,     0.535,    -0.333,    -0.128,    0.0167};
    Int_t   MaxPow[7] = {         4,         3,         4,         5,         0,         4,         4};
    Int_t     Code[30] = {
   1111111,   1112111,   1113111,   2111111,   1211121,   1211113,   2111113,   2212111,   1222112,   2211131,
   2151111,   3411111,   1116111,   1221121,   1111115,   2321111,   1113122,   1111151,   5121111,   1124121,
   4311111,   1215111,   1224111,   1143111,   1233111,   1311123,   2211114,   1111112,   1111121,   1121111};
    Double_t  Coef[30] = {
     -1.07,       1.8,    -0.163,    -0.141,    -0.468,    -0.159,    0.0777,     0.431,     0.489,     0.482,
    0.0988,   -0.0387,     0.219,     -1.08,    0.0212,     0.287,     -0.14,    0.0848,    0.0839,      0.39,
   -0.0171,     0.195,     0.405,    -0.055,   -0.0991,     0.181,   -0.0372,   -0.0087,   -0.0972,    -0.102};
    Double_t dCoef[30] = {
    0.0932,     0.131,    0.0774,    0.0321,    0.0592,    0.0124,   0.00998,    0.0694,    0.0508,    0.0558,
    0.0168,    0.0119,    0.0139,     0.286,   0.00217,    0.0605,    0.0269,    0.0106,     0.015,      0.18,
    0.0104,     0.023,    0.0992,    0.0189,    0.0323,     0.023,   0.00573,   0.00292,    0.0302,    0.0443};
    MDFpFitP[1][0][1][1][1] = new TMDFParameters(-1.33781,7,minV,maxV,meanV,MaxPow,30,Code,Coef,dCoef);
  }
  { //MDFFitP_xz_1_s_1_r_0_B_0_p_0  parameterization with chi2/ndf = 1.94197
    Double_t  minV[7] = {         2,         5,     -1.96,     -1.96,         0,         3,      -0.5};
    Double_t  maxV[7] = {         7,        17,      1.96,      1.96,       195,         9,       0.5};
    Double_t meanV[7] = {    0.0519,    -0.788,   -0.0529,    -0.294,    -0.271,     -0.13,   0.00682};
    Int_t   MaxPow[7] = {         3,         2,         4,         4,         3,         2,         1};
    Int_t     Code[30] = {
   1111111,   1112111,   1211111,   1211211,   1111311,   1112211,   1212111,   1114111,   3111211,   1151111,
   1211231,   1121111,   2111211,   2113111,   1111411,   4111211,   1223111,   1212311,   1112411,   1121231,
   1114311,   1123311,   1213311,   1115211,   1223211,   1313211,   1215111,   3111111,   2112112,   2121112};
    Double_t  Coef[30] = {
    0.0301,    -0.029,    0.0919,    -0.035,    0.0384,    0.0695,   -0.0102,  -0.00033,  -0.00307,    0.0202,
   -0.0303,   -0.0324,  -0.00568,    0.0151,    0.0163,   0.00946,    0.0779,    -0.018,     0.039,   0.00492,
   -0.0371,   -0.0292,   -0.0349,   -0.0363,    0.0881,    0.0303,    0.0173,  -0.00553,    0.0178,   -0.0261};
    Double_t dCoef[30] = {
  0.000762,  0.000915,  0.000825,  0.000705,  0.000452,   0.00114,   0.00152,  0.000409,  0.000277,   0.00021,
  0.000282,  0.000645,  0.000543,  0.000281,  0.000122,  0.000189,   0.00115,   0.00114,  0.000414,   0.00134,
  0.000486,  0.000758,  0.000414,  0.000357,   0.00185,  0.000487,  0.000279,  0.000201,  0.000583,  0.000973};
    MDFpFitP[1][1][0][0][0] = new TMDFParameters(-0.0737039,7,minV,maxV,meanV,MaxPow,30,Code,Coef,dCoef);
  }
  { //MDFFitP_xz_1_s_1_r_0_B_1_p_0  parameterization with chi2/ndf = 1.67939
    Double_t  minV[7] = {         2,         5,     -1.96,     -1.96,         0,         3,      -0.5};
    Double_t  maxV[7] = {         7,        17,      1.96,      1.96,       195,         9,       0.5};
    Double_t meanV[7] = {    0.0519,    -0.788,   -0.0529,    -0.294,    -0.271,     -0.13,   0.00682};
    Int_t   MaxPow[7] = {         2,         3,         4,         4,         3,         4,         0};
    Int_t     Code[30] = {
   1111111,   1112111,   1111221,   1115111,   1112121,   1114111,   1214111,   3112221,   1111121,   1111211,
   2111211,   1212211,   2113211,   1151111,   1412111,   2111151,   1113311,   2122121,   1213121,   1114121,
   1212221,   3111321,   1412121,   3111231,   3111411,   1115211,   2114211,   1412211,   2312211,   1143111};
    Double_t  Coef[30] = {
    -0.238,    -0.314,    -0.323,    -0.182,     -1.17,     0.182,     -0.11,    -0.409,     -1.01,    -0.459,
    -0.186,      1.08,    -0.437,   -0.0407,    -0.202,    0.0426,    0.0503,      2.11,     0.568,     0.298,
      1.52,    0.0925,    -0.286,    0.0651,    0.0242,     0.213,    -0.103,    -0.212,     0.248,    -0.029};
    Double_t dCoef[30] = {
   0.00237,   0.00493,   0.00662,   0.00146,    0.0147,   0.00288,   0.00432,     0.016,    0.0113,   0.00309,
   0.00554,   0.00973,   0.00552,   0.00112,   0.00221,  0.000965,  0.000954,    0.0576,   0.00923,   0.00565,
     0.022,   0.00237,   0.00395,   0.00165,   0.00053,   0.00207,   0.00359,   0.00272,   0.00636,  0.000757};
    MDFpFitP[1][1][0][1][0] = new TMDFParameters(-1.70482,7,minV,maxV,meanV,MaxPow,30,Code,Coef,dCoef);
  }
  { //MDFFitP_xz_1_s_1_r_1_B_0_p_0  parameterization with chi2/ndf = 3.86645
    Double_t  minV[7] = {         2,         5,     -1.96,     -1.96,         0,         3,      -0.5};
    Double_t  maxV[7] = {         7,        17,      1.96,      1.96,       195,         9,       0.5};
    Double_t meanV[7] = {   -0.0795,    -0.683,    -0.094,    -0.179,   -0.0873,    -0.124,   0.00613};
    Int_t   MaxPow[7] = {         3,         3,         4,         3,         3,         4,         1};
    Int_t     Code[30] = {
   1111111,   1211111,   2111131,   1113211,   1213211,   1112111,   1111231,   4112112,   2111111,   1311111,
   2111121,   2111211,   1141111,   1411111,   1111321,   4111111,   1114111,   1132211,   1212311,   1211411,
   1141121,   1152111,   1151121,   1111251,   1111122,   3111111,   1112122,   1121122,   2112112,   1112221};
    Double_t  Coef[30] = {
    0.0585,     0.122,    0.0112,    0.0622,     -0.04,     0.144,   -0.0656,   -0.0484,   -0.0336,    0.0235,
    -0.063,   -0.0138,   0.00521,    0.0114,    0.0118,   0.00424,    0.0666,     0.019,   -0.0561,  -0.00737,
    0.0194,   -0.0265,   -0.0398,   -0.0252,  -0.00351,  -0.00276,    0.0172,   -0.0309,    0.0196,    0.0707};
    Double_t dCoef[30] = {
  0.000124,  0.000186,  0.000306,  0.000384,  0.000141,  0.000626,  0.000532,  0.000151,  0.000322,   0.00014,
  0.000461,  9.88e-05,  4.66e-05,  8.25e-05,  0.000135,  2.74e-05,  0.000232,  0.000443,  0.000249,  3.54e-05,
  0.000186,   0.00024,  0.000199,  0.000197,  0.000132,  4.31e-05,  0.000826,  0.000517,  0.000327,  0.000968};
    MDFpFitP[1][1][1][0][0] = new TMDFParameters(0.0224068,7,minV,maxV,meanV,MaxPow,30,Code,Coef,dCoef);
  }
  { //MDFFitP_xz_1_s_1_r_1_B_0_p_1  parameterization with chi2/ndf = 1.83237
    Double_t  minV[7] = {         2,         5,     -1.96,     -1.96,       195,         3,      -0.5};
    Double_t  maxV[7] = {         7,        17,      1.96,      1.96,       210,         9,       0.5};
    Double_t meanV[7] = {    -0.568,    -0.623,    -0.166,    -0.479,    -0.333,    -0.145,   0.00666};
    Int_t   MaxPow[7] = {         4,         2,         1,         2,         0,         2,         5};
    Int_t     Code[30] = {
   1111111,   1211111,   2212111,   4121112,   1121112,   1312111,   4111111,   1111133,   3113111,   5111111,
   3111114,   2121114,   1111112,   1112112,   2111112,   2121111,   1111114,   2121112,   1121114,   1111116,
   1321111,   1121113,   1121122,   1113112,   2311111,   3112111,   1311121,   1222111,   1111115,   2213111};
    Double_t  Coef[30] = {
     0.116,     0.162,   -0.0426,    -0.549,     0.395,   -0.0693,    0.0324,   -0.0188,   -0.0449,   -0.0044,
   0.00757,     0.607,   -0.0509,   -0.0704,   -0.0146,    -0.108,  -0.00023,     0.549,     0.145,   0.00646,
    0.0364,   -0.0145,     0.101,    -0.025,  -0.00404,   -0.0553,     0.018,   -0.0946,   0.00461,    0.0398};
    Double_t dCoef[30] = {
   0.00355,   0.00291,    0.0105,    0.0121,    0.0171,   0.00434,   0.00342,   0.00124,   0.00358,   0.00173,
    0.0016,    0.0203,   0.00717,   0.00727,   0.00456,    0.0094,   0.00125,    0.0251,   0.00945,  0.000715,
   0.00424,   0.00402,    0.0231,   0.00472,   0.00233,   0.00603,   0.00355,    0.0214,  0.000536,   0.00661};
    MDFpFitP[1][1][1][0][1] = new TMDFParameters(0.0338431,7,minV,maxV,meanV,MaxPow,30,Code,Coef,dCoef);
  }
  { //MDFFitP_xz_1_s_1_r_1_B_1_p_0  parameterization with chi2/ndf = 3.56451
    Double_t  minV[7] = {         2,         5,     -1.96,     -1.96,         0,         3,      -0.5};
    Double_t  maxV[7] = {         7,        17,      1.96,      1.96,       195,         9,       0.5};
    Double_t meanV[7] = {   -0.0795,    -0.683,    -0.094,    -0.179,   -0.0873,    -0.124,   0.00613};
    Int_t   MaxPow[7] = {         1,         3,         5,         4,         3,         2,         0};
    Int_t     Code[30] = {
   1111111,   1112111,   1111311,   2112111,   2131111,   1312111,   1411111,   1212211,   1115111,   1214111,
   1115121,   1114131,   2111111,   1311111,   2111121,   1112211,   2111221,   2311211,   1212131,   1112321,
   1214121,   1161111,   1121121,   2121111,   1213111,   2112211,   1131211,   1141211,   2113211,   1211411};
    Double_t  Coef[30] = {
     0.208,     0.398,   -0.0692,    -0.101,    0.0556,   0.00448,    0.0894,      0.13,    -0.537,    -0.542,
    -0.231,    -0.445,     0.112,    0.0856,    0.0902,   -0.0844,    -0.105,     0.074,      1.64,     0.312,
    -0.348,   -0.0582,    -0.208,   -0.0599,    -0.058,     0.321,    0.0322,    0.0317,   -0.0613,    0.0217};
    Double_t dCoef[30] = {
   0.00142,   0.00564,  0.000295,   0.00243,   0.00087,   0.00337,  0.000778,   0.00413,     0.001,   0.00305,
   0.00107,   0.00167,  0.000841,   0.00127,    0.0014,   0.00345,   0.00204,  0.000725,   0.00582,   0.00314,
    0.0035,  0.000283,   0.00223,  0.000864,   0.00193,   0.00275,  0.000702,  0.000421,   0.00111,  0.000206};
    MDFpFitP[1][1][1][1][0] = new TMDFParameters(-1.81515,7,minV,maxV,meanV,MaxPow,30,Code,Coef,dCoef);
  }
  { //MDFFitP_xz_1_s_1_r_1_B_1_p_1  parameterization with chi2/ndf = 1.29515
    Double_t  minV[7] = {         2,         5,     -1.96,     -1.96,       195,         3,      -0.5};
    Double_t  maxV[7] = {         7,        17,      1.96,      1.96,       210,         9,       0.5};
    Double_t meanV[7] = {    -0.568,    -0.623,    -0.166,    -0.479,    -0.333,    -0.145,   0.00666};
    Int_t   MaxPow[7] = {         2,         3,         2,         5,         0,         2,         3};
    Int_t     Code[30] = {
   1111111,   1112111,   1113111,   1111121,   1321111,   1313111,   1214111,   2112111,   1411111,   1211131,
   1232121,   1116111,   1111123,   2221111,   1113121,   1211114,   2111114,   2112113,   2412111,   2411121,
   3311121,   1215111,   2115111,   1231113,   2313111,   3132111,   1111112,   1111113,   1121121,   2121111};
    Double_t  Coef[30] = {
      1.13,     0.816,      1.46,    -0.395,   -0.0613,     0.481,    -0.318,    -0.651,    -0.226,    -0.216,
     -1.47,    -0.404,    -0.113,    0.0524,    -0.228,    0.0656,   -0.0568,     -0.05,     0.443,   -0.0629,
    -0.113,     0.199,     0.287,    0.0955,     0.294,    -0.111,   -0.0122,   -0.0306,    -0.599,     0.234};
    Double_t dCoef[30] = {
     0.145,     0.206,     0.117,    0.0704,    0.0488,    0.0368,    0.0816,    0.0635,    0.0232,     0.036,
     0.154,     0.023,    0.0177,     0.194,    0.0526,   0.00696,   0.00648,    0.0178,    0.0488,    0.0335,
     0.035,    0.0302,    0.0258,    0.0181,     0.029,    0.0203,   0.00245,    0.0103,    0.0945,     0.126};
    MDFpFitP[1][1][1][1][1] = new TMDFParameters(-1.41288,7,minV,maxV,meanV,MaxPow,30,Code,Coef,dCoef);
  }
#else
  const Double_t secs[3] = {0.5, 12.5, 24.5};
  const Double_t rows[3] = {0.5, 13.5, 45.5};
#endif
  TTree *tree = (TTree *) gDirectory->Get(treeName);
  if (! tree) return;
  cout << "MDFerrorParameterization2(" << treeName << "," << iXZ << "," << sec << "," << row << "," << MS << "," << prompt << ")" << endl;
#if 0
  Char_t **names = (Char_t **) &Names;
  Var_t  *xMin  = (Var_t  *)  &xMin;
  Var_t  *xMax  = (Var_t  *)  &xMax;
#endif
  //  Int_t  *nbins = (Int_t *)   &nBins;
  // List of branches
  // Init
  struct var_t {
    Float_t    sec;
    Float_t    row; // 
    Float_t  Npads; // npads or ntmbks
    Float_t Ntmbks; // npads or ntmbks
    Float_t   phiL;
    Float_t   eta;
    Float_t     zL;
    Float_t   AdcL;
    Float_t   xPad; // xRC hit position within pad
    Float_t   zTbk; // zRC hit position within time bucket
    Float_t      A; 
    Float_t     dA;
    Float_t     mu; 
    Float_t    dmu;
    Float_t  sigma; 
    Float_t dsigma;
    Float_t   chi2; 
    Int_t      iXZ;
  };
  //  const Char_t *vars = "sec/F:row/F:Npads/F:Ntmbks/F:phiL/F:eta/F:zL/F:AdcL/F:xPad/F:zTbk/F:A/F:dA/F:mu/F:dmu/F:sigma/F:dsigma/F:chi2/F:iXZ/I";
  tree->SetMakeClass(1);
  var_t fit;
  TBranch        *b_fit;   //!
  tree->SetBranchAddress("Fit", &fit.sec, &b_fit);
#ifndef __TEST__
  // Global data parameters 
  Int_t nVars       = 7;
  //  TMultiDimFit* MDfit = new TMultiDimFit(nVars, TMultiDimFit::kMonomials,"v");
  MDfit = new TMultiDimFit(nVars, TMultiDimFit::kChebyshev,"v");
  //                  Npads, Ntmbks, phiL, eta, zL, AdcL, xPad 
  Int_t mPowers[]   = {   6,      6,    6,    6,  6,    6,    6 };
  if (prompt) mPowers[4] = 1;
  MDfit->SetMaxPowers(mPowers);
  MDfit->SetMaxFunctions(10000);
  MDfit->SetMaxStudy(10000);
  MDfit->SetMaxTerms(30);
  MDfit->SetPowerLimit(1);
  MDfit->SetMinAngle(10);
  MDfit->SetMaxAngle(10);
  MDfit->SetMinRelativeError(.01);
  // Print out the start parameters
  MDfit->Print("p");
#else
#if 0
  TFile *fout = 
#endif
    new TFile("SpCheck.root","recreate");
  TTree* treeOut = new TTree("FitC", "Tree to check fit results");
  const Char_t *vars = "sec/F:row/F:Npads/F:Ntmbks/F:phiL/F:eta/F:zL/F:AdcL/F:xPad/F:zTbk/F:A/F:dA/F:mu/F:dmu/F:sigma/F:dsigma/F:chi2/F:iXZ/I";
  treeOut->Branch("Fit", &fit.sec, vars);
  TString branchCoord;
  struct FitCheck_t {
    Double_t muF; Double_t dmuF;
    Double_t sigmaF; Double_t dsigmaF;
    Double_t devMu; Double_t devSigma;
  };
  FitCheck_t fCheck;
  treeOut->Branch("fitC",&fCheck.muF,"muF/D:dmuF/D:sigmaF/D:dsigmaF/D:devMu/D:devSigma/D");
#endif
  Long64_t nentries = tree->GetEntriesFast();
  Long64_t nbytes = 0, nb = 0;
  Int_t nAccepted = 0;
  Double_t X[7];
  for (Long64_t jentry=0; jentry<nentries;jentry++) {
    Long64_t ientry = tree->LoadTree(jentry);
    if (ientry < 0) break;
    nb = tree->GetEntry(jentry);   nbytes += nb;
#ifndef __TEST__
    if (fit.iXZ != iXZ)                                                          continue;
    if ((sec >= 0 && sec < 2) && (fit.sec < secs[sec] || fit.sec > secs[sec+1])) continue;
    if ((row >= 0 && row < 2) && (fit.row < rows[row] || fit.row > rows[row+1])) continue;
    if (  prompt && fit.zL <  195)                                               continue;
    if (! prompt && fit.zL >= 195)                                               continue;
    if (fit.dA             >= 0.5*fit.A)                                         continue;
    if (TMath::Abs(fit.mu) >= 0.5)                                               continue;
    if (fit.dsigma         >= 0.05)                                              continue;
#endif
    nAccepted++;
    X[0] =            fit.Npads;
    X[1] =            fit.Ntmbks;
    X[2] = TMath::Tan(fit.phiL);
    X[3] =            fit.eta;
    X[4] =            fit.zL;
    X[5] =            fit.AdcL;
    X[6] =            fit.xPad;
#ifndef __TEST__
    Double_t D = fit.mu;
    //    if (! iXZ) D = TMath::Sign(D,fit.xPad); // abs(xPad)
    Double_t E = fit.dmu*fit.dmu;
    if (MS) {D = TMath::Log(fit.sigma); E = TMath::Power(fit.dsigma/fit.sigma,2);}
    // Add the row to the fit object
    MDfit->AddRow(X,D,E);
    MDfit->AddTestRow(X,D,E); // to make chi2
#else /* __TEST__ */
    fCheck.muF = -1; fCheck.dmuF = -1;
    fCheck.sigmaF = -1; fCheck.dsigmaF = -1;
    Int_t prmpt = 0;
    if (fit.zL >= 195)  prmpt = 1;
    if (MDFpFitP[iXZ][sec][row][0][prmpt]) {
      //      MDFpFitP[iXZ][sec][row][0][prmpt]->Print();
      fCheck.muF = MDFpFitP[iXZ][sec][row][0][prmpt]->Eval(X);
      fCheck.dmuF = MDFpFitP[iXZ][sec][row][0][prmpt]->dEval(X);
    } 
    if (MDFpFitP[iXZ][sec][row][1][prmpt]) {
      //      MDFpFitP[iXZ][sec][row][1][prmpt]->Print();
      fCheck.sigmaF = MDFpFitP[iXZ][sec][row][1][prmpt]->Eval(X); fCheck.sigmaF = TMath::Exp(fCheck.sigmaF);
      fCheck.dsigmaF = MDFpFitP[iXZ][sec][row][1][prmpt]->dEval(X); fCheck.dsigmaF *= fCheck.sigmaF;
    }
    Int_t s = 0; if (fit.sec >= 13) s = 1;
    Int_t r = 0; if (fit.row >  NoInnerRows) r = 1;
    cout << "MDF[" << fit.iXZ << "][" << s << "][" << r << "][" << prmpt << "] = ";
    const Char_t *namesV[7] = {"Npads","Ntmbks","tanPhiL","eta","zL","AdcL","xPad"};
    for (Int_t k = 0; k < 7; k++) {
      if (k < 2) cout << Form("%s %5.0f ",namesV[k],X[k]);
      else       cout << Form("%s %7.2f ",namesV[k],X[k]);
    }
    cout << Form(" mu = %8.3f +/- %7.3f muF = %8.3f +/- %7.3f",fit.mu,fit.dmu,fCheck.muF,fCheck.dmuF);
    cout << Form(" sigma = %8.3f +/- %7.3f sigmaF = %8.3f +/- %7.3f",fit.sigma,fit.dsigma,fCheck.sigmaF,fCheck.dsigmaF);
    fCheck.devMu = (fit.mu - fCheck.muF)/TMath::Sqrt(fit.dmu*fit.dmu + fCheck.dmuF*fCheck.dmuF);
    fCheck.devSigma = (fit.sigma - fCheck.sigmaF)/TMath::Sqrt(fit.dsigma*fit.dsigma + fCheck.dsigmaF*fCheck.dsigmaF);
    cout << " dm " << fCheck.devMu << " ds " << fCheck.devSigma;
    if (TMath::Abs(fCheck.devSigma) > 5) cout << " ==========================";
    cout << endl;
    treeOut->Fill();
#endif
  }
#ifndef __TEST__
  cout << "Accepted " << nAccepted << " entries and sample size " << MDfit->GetSampleSize() << endl;
  if (nAccepted < 100) return;
  // Reset variable limits
  TVectorD max(nVars), min(nVars);
  max(0) =  7; min(0) = 2;
  max(1) = 17; min(1) = 5;
  max(2) = TMath::Tan(1.1); min(2) = - max(2);
  max(3) = TMath::Tan(1.1); min(3) = - max(3);
  if (! prompt) {max(4) = 195; min(4) =   0;}
  else          {max(4) = 210; min(4) = 195;}
  max(5) = 9.0; min(5) =  3.0; // AdcL
  max(6) = 0.5; min(6) = -0.5; 
  //  if (iXZ == 0) min(3) =  0.0; // Abs(xPad) 
  MDfit->Print("s");
  MDfit->SetMaxVariables(max);
  MDfit->SetMinVariables(min);
 // Print out the statistics
  MDfit->Print("s");
#if 0
  // Book histograms 
  MDfit->MakeHistograms();
#endif
  // Find the parameterization 
  MDfit->FindParameterization();
  // Print coefficents 
  MDfit->Print("pscr");
  Double_t chi2 = MDfit->MakeChi2();
  Int_t    NDF  = MDfit->GetSampleSize() - MDfit->GetNCoefficients();
  cout << "Chi2 " << MDfit->MakeChi2() << " and Chi2/NDF = " << chi2/NDF << endl;
  // Write code to file  

  TString output(Form("MDF%s_xz_%i_s_%i_r_%i_B_%i_p_%i",treeName,iXZ,sec,row,MS,prompt));
  //  MDfit->MakeMethod(output);
  MDfit->MakeCode(output);
  Int_t nV = MDfit->GetNVariables();
  Int_t nCoef = MDfit->GetNCoefficients();
  TArrayI nMaxP(nV);
  TArrayI Code(nCoef);
  TArrayD Coef(nCoef);
  TArrayD dCoef(nCoef);
  for (Int_t i = 0; i < nCoef; i++) {
    Int_t code = 0;
    for (Int_t j = 0; j < nV; j++) {
      code *= 10;
      Int_t p  =  MDfit->GetPowers()[MDfit->GetPowerIndex()[i]*nV + j];
      if (nMaxP[j] < p) nMaxP[j] = p;
      code += p;
    }
    Code[i] = code;
    Coef[i] = (*MDfit->GetCoefficients())(i);
    dCoef[i] = (*MDfit->GetCoefficientsRMS())(i);
  }
  TString Out("");
  Out += gSystem->BaseName(gDirectory->GetName());
  Out.ReplaceAll(".root","");
  Out += ".C";
  ofstream out;
  if (gSystem->AccessPathName(Out)) out.open(Out, ios::out); //"Results.list",ios::out | ios::app);
  else                              out.open(Out, ios::app);
  out << "  { //" << output << "  parameterization with chi2/ndf = " <<  chi2/NDF << endl;
  out << "    Double_t  minV[" << nV << "] = " << PrintLine(nV,(*MDfit->GetMinVariables())) << endl;
  out << "    Double_t  maxV[" << nV << "] = " << PrintLine(nV,(*MDfit->GetMaxVariables())) << endl;
  out << "    Double_t meanV[" << nV << "] = " << PrintLine(nV,(*MDfit->GetMeanVariables())) << endl;
#ifdef __MUDIFI_EXT__
  out << "    Int_t   MaxPow[" << nV    << "] = " << PrintLine(nV,MDfit->GetMaxPowersFinal()) << endl;
#endif
  out << "    Int_t     Code[" << nCoef << "] = " << PrintLine(nCoef,Code.GetArray()) << endl;
  out << "    Double_t  Coef[" << nCoef << "] = " << PrintLine(nCoef,Coef.GetArray()) << endl;
  out << "    Double_t dCoef[" << nCoef << "] = " << PrintLine(nCoef,dCoef.GetArray()) << endl;
  out << "    MDFp" << treeName 
      << "[" << iXZ << "]"
      << "[" << sec << "][" << row << "][" << MS << "][" << prompt << "] = "
      << "new TMDFParameters("<< MDfit->GetMeanQuantity() << "," << nV << ",minV,maxV,meanV,MaxPow," << nCoef << ",Code,Coef,dCoef);" << endl; 
  out << "  }" << endl;
  out.close();
  delete MDfit;
#endif
}
//________________________________________________________________________________
void T0Offsets(const Char_t *files="*.root", const Char_t *Out = "") {
  TDirIter Dir(files);
  Char_t *file = 0;
  Char_t *file1 = 0;
  Int_t NFiles = 0;
  TTreeIter iter("TpcT");
  while ((file = (Char_t *) Dir.NextFile())) {
    TString File(file);
    if (File.Contains("Plot") || File.Contains("Fit") || File.Contains("ADC") || File.Contains("Pads") || 
	File.Contains("hist") || 
	//	File.Contains("tags") || 
	File.Contains("MuMc") || File.Contains("minimc") ||
	File.Contains("minimc") || File.Contains("event") ||
	File.Contains("Sparse") ||
	File.Contains("All") ||
	File.Contains("MuDst.root")) continue;
    TFile *f = new TFile (File);
    if (f) {
      TTree *tree = (TTree *) f->Get("TpcT");
      if (! tree ) continue;
      //    tree->Show(0);
      iter.AddFile(file); 
      NFiles++; 
      file1 = file;
      SetInnerPadrows();
    }
    delete f;
  }
  cout << files << "\twith " << NFiles << " files" << endl; 
  if (! file1 ) return;
  TString output(Out);
  if (output == "") {
    output = file1;
    output.ReplaceAll(".root","T0.root");
  } else {
    output += ".T0.root";
  }
  cout << "Output for " << output << endl;
  if (! fOut) fOut = new TFile(output,"recreate");
  TH2D *T[2] = {
    new TH2D("TI","time bucket difference for Inner sector versus Z",210,-210,210,800,-2,2),
    new TH2D("TO","time bucket difference for Outer sector versus Z",210,-210,210,800,-2,2)
  };
  TH2D *D[2] = {
    new TH2D("DI","time bucket difference for Inner sector versus tanL",200,-2,2,800,-2,2),
    new TH2D("DO","time bucket difference for Outer sector versus tanL",200,-2,2,800,-2,2)
  };
  TH2D *B[2] = {
    new TH2D("BI","time bucket difference for Inner sector versus no. of time buckets",50,0.5,50.5,800,-2,2),
    new TH2D("BO","time bucket difference for Outer sector versus no. of time buckets",50,0.5,50.5,800,-2,2)
  };
  // TpcT->Draw("fMcHit.mMcl_t+0.165*Frequency-fRcHit.mMcl_t/64:fMcHit.mPosition.mX3>>TI(210,-210,210,100,-2,3)","fNoMcHit==1&&fNoRcHit==1&&fRcHit.mQuality>90&&fMcHit.mVolumeId%100<=13","colz"); TI->FitSlicesY(); TI_1->Fit("pol2","er","",-100,100);
  // TpcT->Draw("fMcHit.mMcl_t+0.165*Frequency-fRcHit.mMcl_t/64:fMcHit.mPosition.mX3>>TO(210,-210,210,100,-2,3)","fNoMcHit==1&&fNoRcHit==1&&fRcHit.mQuality>90&&fMcHit.mVolumeId%100>13","colz"); TO->FitSlicesY(); TO_1->Fit("pol2","er","",-100,100);
  const Float_t&     Frequency                                = iter("Frequency");
  const Int_t&       fNoMcHit                                 = iter("fNoMcHit");
  const Int_t&       fNoRcHit                                 = iter("fNoRcHit");
  const Long_t*&     fMcHit_mVolumeId                         = iter("fMcHit.mVolumeId");
  const Float_t*&    fMcHit_mMcl_t                            = iter("fMcHit.mMcl_t");
  const Float_t*&    fMcHit_mPosition_mX3                     = iter("fMcHit.mPosition.mX3");
  const Float_t*&    fMcHit_mLocalMomentum_mX1                = iter("fMcHit.mLocalMomentum.mX1");
  const Float_t*&    fMcHit_mLocalMomentum_mX2                = iter("fMcHit.mLocalMomentum.mX2");
  const Float_t*&    fMcHit_mLocalMomentum_mX3                = iter("fMcHit.mLocalMomentum.mX3");
  const Float_t*&    fMcHit_mTof                              = iter("fMcHit.mTof");
  const Long_t*&     fMcHit_mKey                              = iter("fMcHit.mKey");
  const Int_t*&   fRcHit_mIdTruth                          = iter("fRcHit.mIdTruth");
  const Short_t*&    fRcHit_mMcl_t                            = iter("fRcHit.mMcl_t");
  const UChar_t*&    fRcHit_mMintmbk                          = iter("fRcHit.mMintmbk");
  const UChar_t*&    fRcHit_mMaxtmbk                          = iter("fRcHit.mMaxtmbk");
  const UShort_t*&   fRcHit_mQuality                          = iter("fRcHit.mQuality");
  const Int_t&       fNoRcTrack                               = iter("fNoRcTrack");
  const Float_t*&    fRcTrack_fpx                             = iter("fRcTrack.fpx");
  const Float_t*&    fRcTrack_fpy                             = iter("fRcTrack.fpy");
  const Float_t*&    fRcTrack_fpz                             = iter("fRcTrack.fpz");
  const UInt_t*&     fRcHit_mHardwarePosition                 = iter("fRcHit.mHardwarePosition");
  Int_t ev = 0;
  while (iter.Next()) {
    for (Int_t l = 0; l < fNoRcHit; l++) {
      Int_t IdTruth = fRcHit_mIdTruth[l];
      Int_t row = padrow(fRcHit_mHardwarePosition[l]);
      Int_t sec = sector(fRcHit_mHardwarePosition[l]);
      Int_t k = -1;
      for (Int_t k1 = 0; k1 < fNoMcHit; k1++) {
	Int_t key = fMcHit_mKey[k1];
	if (key != IdTruth) continue;
	if (fMcHit_mVolumeId[k1] > 10000) continue;
	if (sec != fMcHit_mVolumeId[k1]/100) continue;
	if (row != fMcHit_mVolumeId[k1]%100) continue;
	k = k1;
	break;
      }
      if (k < 0) continue;
      Int_t io = 0;
      if (row > NoInnerRows) io = 1;
      if (fRcHit_mQuality[l] < 90) continue;
      Double_t dT = fMcHit_mMcl_t[k]+(0.165+1e6*fMcHit_mTof[k])*Frequency-fRcHit_mMcl_t[l]/64.;
      T[io]->Fill(fMcHit_mPosition_mX3[k],dT);
      Int_t ntbk = fRcHit_mMaxtmbk[l] + fRcHit_mMintmbk[l] + 1;
      B[io]->Fill(ntbk,dT);
      Double_t pT = TMath::Sqrt(fMcHit_mLocalMomentum_mX1[k]*fMcHit_mLocalMomentum_mX1[k] + fMcHit_mLocalMomentum_mX2[k]*fMcHit_mLocalMomentum_mX2[k]);
      if (pT < 0.1) continue;
      Double_t tanL = fMcHit_mLocalMomentum_mX3[k]/pT;
      D[io]->Fill(tanL,dT);
      if (! ev%1000) cout << "Processed event " << ev << endl;
      ev++;
    }
  }
  TH1D *T1[2] = {0,0};
  for (Int_t io = 0; io < 2; io++) {
    cout << T[io]->GetTitle() << endl;
    T[io]->FitSlicesY();
    T1[io] = (TH1D *) gDirectory->Get(Form("%s_1",T[io]->GetName()));
    if (! T1[io]) continue;
    T1[io]->Fit("pol2","er","",-100,100);
  }
  fOut->Write();
}
