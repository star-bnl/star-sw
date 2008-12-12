#define PRINT
//#define __LASER__
//#define __REAL_DATA__
#if !defined(__CINT__)
// code that should be seen ONLY by the compiler
#include "TH1.h"
#include "TH2.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TProfile2D.h"
#include "TF1.h"
#include "TLegend.h"
#include "Riostream.h"
#include "TSystem.h"
#include "TMath.h"
#include "TGraphErrors.h"
#include "TDirIter.h"
#include "TTreeIter.h"
#include "TDirectory.h"
#include "TROOT.h"
#include "TFile.h"
#include "TVector3.h"
#include "TRMatrix.h"
#endif
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
*/
//                                    Inner        Outer
static const Double_t t0IO[2]   = {1.20868e-9, 1.43615e-9};
static const Double_t tauC[2]   = {999.655e-9, 919.183e-9}; 
/*1.11
  (s+d)/(s-a)/(s-b)/(s-c) => A*exp(a*t) + B*exp(b*t) + C*exp(c*t)
  A = (a + d)/(a - b)/(a - c); B = (b + d)/(b - a)/(b - c);  C = (c + d)/(c - a)/(c - b); 
  (s+d)/(s*(s-a)*(s-b)*(s-c)) => -d/(a*b*c) + (a+d)/(a*(a-b)*(a-c))*exp(a*t) + (b+d)/(b*(b-a)*(b-c))*exp(b*t) + (c+d)/(c*(c-a)*(c-b))*exp(c*t);  
*/
static const  Double_t mTimeBinWidth              = 1.06580379191673078e-07;//1.e-6/gStTpcDb->Electronics()->samplingFrequency();
#include "HardWarePosition.C"
#include "tables/St_tpcGain_Table.h"
#include "tables/St_tpcT0_Table.h"
//#include "DrawList.C"
//--------------------------------------------------------------------------------
void TpcT(const Char_t *opt = "A", const Char_t *files="*.root", const Char_t *Out = "", const Char_t *Time = "20080208.174701") {
  //	   Int_t ev, Double_t tanCut, Int_t NpadCut, Double_t pMomin, Double_t pMomax) {
  gSystem->Load("libStDb_Tables.so");
  TFile *f = new TFile(Form("$STAR/StarDb/Calibrations/tpc/tpcGain.%s.root",Time));
  if (! f) return;
  St_tpcGain *G = (St_tpcGain*) f->Get("tpcGain");
  delete f;
  if (! G) return;
  cout << "Got " << G->GetName() << endl;
  f = new TFile(Form("$STAR/StarDb/Calibrations/tpc/tpcT0.%s.root",Time));
  if (! f) return;
  St_tpcT0 *T = (St_tpcT0*) f->Get("tpcT0");
  delete f;
  if (! T) return;
  cout << "Got " << T->GetName() << endl;
  tpcGain_st *gains = G->GetTable();
  tpcT0_st   *t0s   = T->GetTable();
  TDirIter Dir(files);
  Char_t *file = 0;
  Char_t *file1 = 0;
  Int_t NFiles = 0;
  TTreeIter iter("TpcT");
  while ((file = (Char_t *) Dir.NextFile())) {
    TString File(file);
    if (File.Contains("Plot") || File.Contains("Fit")|| File.Contains("hist")) continue;
    iter.AddFile(file); 
    NFiles++; 
    file1 = file;
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
	const Int_t&       fNoTracksAtBestPV                        = iter("fNoTracksAtBestPV");
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
	const UShort_t*&   fPixels_mIdTruth                         = iter("fPixels.mIdTruth");
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
#ifdef PRINT
	const Long_t*&     fMcHit_mVolumeId                         = iter("fMcHit.mVolumeId");
#endif
#if 0
	const Float_t*&    fMcHit_mLgamma                           = iter("fMcHit.mLgamma");
#endif
	const Short_t*&    fMcHit_mMcl_x                            = iter("fMcHit.mMcl_x");
	const Short_t*&    fMcHit_mMcl_t                            = iter("fMcHit.mMcl_t");
#endif
#if 0
	const Float_t*&    fRcHit_mPosition_mX1                     = iter("fRcHit.mPosition.mX1");
	const Float_t*&    fRcHit_mPosition_mX2                     = iter("fRcHit.mPosition.mX2");
#endif
	const Float_t*&    fRcHit_mPosition_mX3                     = iter("fRcHit.mPosition.mX3");
#ifdef PRINT
	const UInt_t*&     fRcHit_mHardwarePosition                 = iter("fRcHit.mHardwarePosition");
#endif
	const Float_t*&    fRcHit_mCharge                           = iter("fRcHit.mCharge");
#if 0
	const Int_t*&      fRcHit_mId                               = iter("fRcHit.mId");
	const UShort_t*&   fRcHit_mIdTruth                          = iter("fRcHit.mIdTruth");
#endif
#ifndef __REAL_DATA__
	const UShort_t*&   fRcHit_mQuality                          = iter("fRcHit.mQuality");
#endif
#if 0
	const UChar_t*&    fRcHit_mFitFlag                          = iter("fRcHit.mFitFlag");
	const UChar_t*&    fRcHit_mTrackRefCount                    = iter("fRcHit.mTrackRefCount");
	const UChar_t*&    fRcHit_mFlag                             = iter("fRcHit.mFlag");
	const UChar_t*&    fRcHit_mMinpad                           = iter("fRcHit.mMinpad");
	const UChar_t*&    fRcHit_mMaxpad                           = iter("fRcHit.mMaxpad");
	const UChar_t*&    fRcHit_mMintmbk                          = iter("fRcHit.mMintmbk");
	const UChar_t*&    fRcHit_mMaxtmbk                          = iter("fRcHit.mMaxtmbk");
#endif
	const Short_t*&    fRcHit_mMcl_x                            = iter("fRcHit.mMcl_x");
	const Short_t*&    fRcHit_mMcl_t                            = iter("fRcHit.mMcl_t");
#ifdef PRINT
	const Int_t*&      fRcTrack_fSector                         = iter("fRcTrack.fSector");
	const Int_t*&      fRcTrack_fRow                            = iter("fRcTrack.fRow");
#endif
#if 1
	const Short_t*&    fRcTrack_mMcl_x                          = iter("fRcTrack.mMcl_x");
	const Short_t*&    fRcTrack_mMcl_t                          = iter("fRcTrack.mMcl_t");
#endif
#if 0
	const Int_t*&      fRcTrack_fNpoints                        = iter("fRcTrack.fNpoints");
#endif
	const Int_t*&      fRcTrack_fNfitpoints                     = iter("fRcTrack.fNfitpoints");
	const Int_t*&      fRcTrack_fifPrim                         = iter("fRcTrack.fifPrim");
	const Float_t*&    fRcTrack_fpx                             = iter("fRcTrack.fpx");
	const Float_t*&    fRcTrack_fpy                             = iter("fRcTrack.fpy");
	const Float_t*&    fRcTrack_fpz                             = iter("fRcTrack.fpz");
	const Float_t*&    fRcTrack_fdEdx                           = iter("fRcTrack.fdEdx");
	const Float_t*&    fRcTrack_fTrackLength70                  = iter("fRcTrack.fTrackLength70");
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
  if (! fOut) fOut = new TFile(output,"update");
  fOut->cd();
  struct Name_t {
    Char_t *Name;
    Char_t *Title;
  };
  Name_t InOut[4] = {
    {"Inner","Inner for all except 16"},
    {"Outer","Outer for all except 16"},
    {"InnerX","Inner for Sector 16"},
    {"OuterX","Outer for Sector 16"}
  };
  Name_t RcMcTk[3] = {
    {"Rc","Rc"},
    {"Mc","Mc"},
    {"Tk","Tk"}
  };
  Name_t PadTime[2] = {
    {"Pad","Pad"},
    {"Time","Time"}
  };
  //               io r  pt
  TProfile2D *hist[4][3][2];
  TH1D       *histA[4][2];
  TH2D       *histB[4];
  memset (hist, 0, sizeof(hist));
  for (Int_t io = 0; io < 4; io++) {// TPC + TPX
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
  }
#ifdef PRINT
  Int_t entry = 0;
#endif
#ifdef __LASER__
  static   Double_t offset = -178.3;
  Double_t x[7] = {offset  -0.947, offset +  26.752, offset +  57.808, 
		   offset + 87.667, offset + 118.674, offset + 146.232, offset + 178.3};
  Double_t zShift[2] = {- (6.576 + 0.5), (7.346 - 0.5)};
  
#endif
  while (iter.Next()) {
    if (! fNoPixels ) continue;
    TVector3 mom(fRcTrack_fpx[0],fRcTrack_fpy[0],fRcTrack_fpz[0]);
#if defined(__REAL_DATA__) && ! defined(__LASER__)
    if (fNofPV == 0) continue;
    if (TMath::Abs(fzV) > 20) continue;
    if (TMath::Abs(fxV) > 1 || TMath::Abs(fyV) > 1) continue;
    if (mom.Pt() < 0.4 || mom.Pt() > 0.6) continue;
    if (fRcTrack_fTrackLength70[0] < 40) continue;
    if (fRcTrack_fdEdx[0] < 2e-6 || fRcTrack_fdEdx[0] > 3e-6) continue;
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
#ifndef __REAL_DATA__
    if (fRcHit_mQuality[0] < 95) continue;
#endif
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
    if (row > 13) io = 1;
    if (sector  == 16) io += 2;
    for (Int_t i = 0; i < fNoPixels; i++) {
      pad = fPixels_mPad[i];
      tb  = fPixels_mTimeBin[i];
      if (tb  >= 512) continue;
      if (pad > 182) continue;
      if (pad < kPadMin) kPadMin = pad;
      if (pad > kPadMax) kPadMax = pad;
      if (tb  < kTbMin) kTbMin = tb;
      if (tb  > kTbMax) kTbMax = tb;
    }
    const Int_t NP = kPadMax - kPadMin + 3;
    const Int_t NT = kTbMax  - kTbMin  + 2;
    TRMatrix adcs(NP,NT);
    Int_t tMax = -1;
    Int_t pMax = -1;
    Double_t AtMax = -1; 
    for  (Int_t i = 0; i < fNoPixels; i++) {
      pad = fPixels_mPad[i];
      tb  = fPixels_mTimeBin[i];
      if (tb  < kTbMin  || tb  > kTbMax) continue;
      if (pad < kPadMin || pad > kPadMax) continue;
      Double_t ADC = fPixels_mAdc[i]*gains[sector-1].Gain[row-1][pad-1];
      adcs(pad     - kPadMin,tb - kTbMin) = ADC;
      adcs(pad     - kPadMin,NT-1)       += ADC;
      adcs(NP-2,tb - kTbMin)             += ADC;
      adcs(NP-1,tb - kTbMin)             += (tb+t0s[sector-1].T0[row-1][pad-1])*ADC;
      adcs(NP-1,NT-1)                    += ADC;
      pav                                += pad*ADC;
      tav                                += (tb+t0s[sector-1].T0[row-1][pad-1])*ADC;
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
    Double_t pax  = fRcHit_mMcl_x[0]/64.;
    Double_t tax  = fRcHit_mMcl_t[0]/64.;
    histB[io]->Fill(pav-pax,tav-tax);
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
#ifdef PRINT
    entry++;
    if (entry%1000 == 1) {
      cout << entry << "\t =======================================================" << endl;
      if (sector == 16) {
	cout << "\t =======================================================" << endl;
      }
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
      cout << "Mc:Pad\t" << fMcHit_mMcl_x[0]/64.
	   << "\tMc:Time\t" << fMcHit_mMcl_t[0]/64. << endl;
#endif
      cout << "\tsum = " << sum << "\tpav = " << pav << "\ttav = " << tav << endl;
      cout << "\tsum = " << sum << "\tpax = " << pax << "\ttax = " << tax << endl;
      cout << "kPadMin/kPadMax\t" << kPadMin << "/" << kPadMax
	   << "\tkTbMin/kTbMax\t" << kTbMin << "/" << kTbMax << endl;
      adcs.Print();
      cout << " ";
      for (tb = kTbMin; tb <= kTbMax + 1; tb++) cout << "\t|" << tb;
      cout << endl;
      cout << "_";
      for (tb = kTbMin; tb <= kTbMax + 1; tb++) cout << "\t|____";
      cout << endl;
      for (pad = kPadMin; pad <= kPadMax + 2; pad++) {
	if (pad >= kPadMax + 1) cout << "---------------------------------------------" << endl;
#if 0
	cout << pad << "|" << "G:" << gains[sector-1].Gain[row-1][pad-1] 
	     << "|T0:" << t0s[sector-1].T0[row-1][pad-1] << "|";
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
	     << "\tPad\t" << fRcHit_mMcl_x[i]/64.
	     << "\tTimeBin\t" << fRcHit_mMcl_t[i]/64.
	     << endl;
#ifndef __REAL_DATA__
      for (Int_t i = 0; i < fNoMcHit; i++) 
	cout << "Mc\t" << i << "\tRow\t" << fMcHit_mVolumeId[i]%100
	     << "\tPad\t" << fMcHit_mMcl_x[i]/64.
	     << "\tTimeBin\t" << fMcHit_mMcl_t[i]/64.
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
    for (Int_t i = 0; i < NP - 2; i++) {
      Double_t ratio = adcs(i,NT-1);
      Double_t delPadRc = i + kPadMin - pax;
      Double_t delPadTk = i + kPadMin - (fRcTrack_mMcl_x[0]/64.);
      hist[io][0][0]->Fill(delPadRc, zRc, ratio);
      if (fNoRcTrack == 1)
	hist[io][2][0]->Fill(delPadTk, zRc,ratio);
#ifndef __REAL_DATA__
      Double_t delPadMc = i + kPadMin - (fMcHit_mMcl_x[0]/64.);
      if (fNoMcHit == 1)
	hist[io][1][0]->Fill(delPadMc, zMc,ratio);
#endif
    }
    for (Int_t j = 0; j < NT - 1; j++) {
      Double_t ratio = adcs(NP-2,j);
      Double_t tb    = adcs(NP-1,j);
      Double_t delTbRc = tb + 0.5 - tax;
      Double_t delTbTk = tb + 0.5 - (fRcTrack_mMcl_t[0]/64.);
      hist[io][0][1]->Fill(delTbRc, zRc,ratio);
      if (fNoRcTrack == 1)
	hist[io][2][1]->Fill(delTbTk, zRc,ratio);
#ifndef __REAL_DATA__
      Double_t delTbMc = tb  + 0.5 - (fMcHit_mMcl_t[0]/64.);
      if (fNoMcHit == 1)
	hist[io][1][1]->Fill(delTbMc,   zMc,ratio);
#endif
    }
  }
  fOut->Write();
}
//________________________________________________________________________________
void DrawTpcTPlots(const Char_t *histname="OuterPad",const Char_t *Option="") {
  if (Option);
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
  Double_t tau = par[0];
  Double_t width = par[1];
  Double_t FWHM = par[2];
  Double_t t = (x[0]-par[3])*width/tau;
  Double_t Delta = width/tau;
  Double_t t1 = t - Delta/2.;
  Double_t t2 = t1 + Delta;
  if (t1 < 0) t1 = 0;
  if (t2 < 0) t2 = 0;
  Double_t p = TMath::Power(FWHM/((2*TMath::Sqrt(2*TMath::Log(2.)))*tau),2);
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
void TpcTAdc() {
  TTree *TpcT = (TTree *) gDirectory->Get("TpcT");
  if (! TpcT) return;
  //  TF1* off = new TF1("off","exp(log(1.+[0]/exp(x)))",3,10);
  TpcT->SetMarkerStyle(20);
  TpcT->SetMarkerColor(1);
  TpcT->Draw("fMcHit.mlgam/fAdcSum:log(fAdcSum)>>I(70,3.,10.)",
	     "fNoMcHit==1&&fNoRcHit==1&&fAdcSum/fMcHit.mlgam>0.5&& fAdcSum/fMcHit.mlgam<1.4&&fMcPad.fRow<14",
	     "prof");
  TProfile *I = (TProfile *) gDirectory->Get("I");
  if (! I) return;
  I->Fit("off","r","",3,10);
  TpcT->SetMarkerColor(2);
  TpcT->Draw("fMcHit.mlgam/fAdcSum:log(fAdcSum)>>O(70,3.,10.)",
	     "fNoMcHit==1&&fNoRcHit==1&&fAdcSum/fMcHit.mlgam>0.5&& fAdcSum/fMcHit.mlgam<1.4&&fMcPad.fRow>=14",
	     "prof");
  TProfile *O = (TProfile *) gDirectory->Get("O");
  if (! O) return;
  O->Fit("off","r","",3,10);
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
  const Double_t pPI = 0.335;
  const Double_t pPO = 0.670;
  const Double_t rI  =  89.9;
  const Double_t rO  = 156.2;
  const Double_t lI  = 1.15;
  const Double_t lO  = 1.95;
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
    Double_t timeBin = 5.78602945878541108e-01;// (cm)
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
      ga->SetParLimits(7,1,100);
      ga->ReleaseParameter(8); // 
      ga->SetParameter(8,0.5);
      ga->SetParLimits(8,0.1,10);
    }
  }
  TString OutFile(fIn->GetName());
  OutFile.ReplaceAll(".root",".Fit.root");
  if (! iy) {
    if (! fOut) fOut = new TFile(OutFile,"update");
    fOut->cd();
  }
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
    Double_t chisq = ga->GetChisquare(); 
    //    out[Npar]->SetBinContent(i,chisq);
    if (chisq < 1.e5) {
      for (Int_t j = 2; j < Npar; j++) {
	if (out[j]) {
	  out[j]->SetBinContent(i,ga->GetParameter(j));
	  out[j]->SetBinError(i,ga->GetParError(j));
	}
      } 
    }
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
  if (! iy) {
    fOut->Write();
    delete fOut; fOut = 0;
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
  Char_t *InOut[2] = {"Inner","Outer"};
  Char_t *PadTime[2] =           {"Time"         ,"Pad"};
  Char_t *PadTimeFitOptions[2] = {"FWHMNoise","SigmaSQ"};
  //  Char_t *PadTimeFitOptions[2] = {"FWHMNoise","NoiseConv"};
  //  Char_t *PadTimeFitOptions[2] = {"ConvNoise","NoiseConv"};
  //  Char_t *PadTimeFitOptions[2] = {"ConvFWHM","Conv"};
  //  Char_t *PadTimeFitOptions[2] = {"ConvFWHM","ConvNoise"};
  Char_t *X[2]                 = {"","X"};
  Int_t ip1 = 0;
  Int_t ip2 = 1;
  if (tp > 0 && tp < 2) ip1 = ip2 = tp;
  for (Int_t ix = 0; ix < 2; ix++) {
    for (Int_t ip = ip1; ip <= ip2; ip++) {
      for (Int_t io = 0; io < 2; io++) {
	TString Name(Form("%s%s%s%s",InOut[io],X[ix],PadTime[ip],opt));
	FitSlices(Name,PadTimeFitOptions[ip]);

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
