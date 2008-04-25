//#define __REAL_DATA__
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
static const Double_t tau     = 71e-9; // from pulser fit
static const Double_t pShaper = 5.15;  //     -"-         ?
static const Double_t CrossTalkInner = 0.004;
static const Double_t CrossTalkOuter = 0.004;
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
//--------------------------------------------------------------------------------
void TpcT(const Char_t *files="rcf1207_01_*.root", const Char_t *Out = "") {
  //	   Int_t ev, Double_t tanCut, Int_t NpadCut, Double_t pMomin, Double_t pMomax) {
  
  TDirIter Dir(files);
  Char_t *file = 0;
  Char_t *file1 = 0;
  Int_t NFiles = 0;
  TTreeIter iter("TpcT");
  while ((file = (Char_t *) Dir.NextFile())) {iter.AddFile(file); NFiles++; file1 = file;}
  cout << files << "\twith " << NFiles << " files" << endl; 
  if (! file1 ) return;
  TString output(Out);
  if (output == "") {
    output = file1;
    output.ReplaceAll(".root",".Plots.root");
  }
  cout << "Output for " << output << endl;
	const Int_t&       fNoPixels                                = iter("fNoPixels");
#ifndef __REAL_DATA__
	const Int_t&       fNoMcHit                                 = iter("fNoMcHit");
#endif
	const Int_t&       fNoRcHit                                 = iter("fNoRcHit");
	const Int_t&       fNoRcTrack                               = iter("fNoRcTrack");
	const Int_t&       fAdcSum                                  = iter("fAdcSum");
#if 0
	const Int_t&       fPixels_                                 = iter("fPixels");
	const UChar_t*&    fPixels_mDetector                        = iter("fPixels.mDetector");
	const UChar_t*&    fPixels_mSector                          = iter("fPixels.mSector");
#endif
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
	const UShort_t*&   fRcHit_mQuality                          = iter("fRcHit.mQuality");
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
#if 0
	const Int_t*&      fRcTrack_fifPrim                         = iter("fRcTrack.fifPrim");
#endif
#ifdef PRINT
	const Float_t*&    fRcTrack_fpx                             = iter("fRcTrack.fpx");
	const Float_t*&    fRcTrack_fpy                             = iter("fRcTrack.fpy");
#endif
#if 0
	const Float_t*&    fRcTrack_fpz                             = iter("fRcTrack.fpz");
#endif
  static Int_t nx = 200;
  static Double_t xmin =  -10.;
  static Double_t xmax =   10.;
  static Int_t nz = 42;
  static Double_t zmin = -210;
  static Double_t zmax = -zmin;
#if 0
  static Int_t ny = 140;
  static Double_t ymin = -4;
  static Double_t ymax = 10;
#endif
  TFile *fOut = new TFile(output,"update");
  struct Name_t {
    Char_t *Name;
    Char_t *Title;
  };
  Name_t InOut[2] = {
    {"Inner","Inner"},
    {"Outer","Outer"}
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
  TProfile2D *hist[2][3][2];
  memset (hist, 0, sizeof(hist));
  Int_t color = 1;
  for (Int_t io = 0; io < 2; io++) {
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
  }
  while (iter.Next()) {
#ifdef PRINT
    entry++;
    if (entry%1000 == 1) {
      cout << entry << "\t =======================================================" << endl;
      cout << "fNoPixels \t" << fNoPixels << "\tNPads " << endl; // fNoRcHit << endl;
      cout << "Charge(keV) \t" << 1e6*fRcHit_mCharge[0];
      if (TMath::Abs(fRcTrack_fpy[0]) > 1e-7) 
	cout << "\tpx/py\t" << fRcTrack_fpx[0]/fRcTrack_fpy[0] 
	     << "\tpT\t" << TMath::Sqrt(fRcTrack_fpx[0]*fRcTrack_fpx[0]+fRcTrack_fpy[0]*fRcTrack_fpy[0]);
      cout << endl;
      cout << "\tfAdcSum " << fAdcSum << endl;
      cout << "Rc:Pad\t" << fRcHit_mMcl_x[0]/64        << "\tRc:dX\t" << (fRcHit_mMcl_x[0]%64)/64.
	   << "\tRc:Time\t" << fRcHit_mMcl_t[0]/64 << "\tRc:dZ\t" << (fRcHit_mMcl_t[0]%64)/64. << endl;
#ifndef __REAL_DATA__
      cout << "Mc:Pad\t" << fMcHit_mMcl_x[0]/64        << "\tMc:dX\t" << (fMcHit_mMcl_x[0]%64)/64.
	   << "\tMc:Time\t" << fMcHit_mMcl_t[0]/64 << "\tMc:dZ\t" << (fMcHit_mMcl_t[0]%64)/64. << endl;
#endif
      Int_t kPadMin = 999;
      Int_t kPadMax =   0;
      Int_t kTbMin  = 999;
      Int_t kTbMax  =   0;
      Int_t pad  = 0;
      Int_t tb   = 0;
      Int_t indx = 0;
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
      Int_t *adcs = new Int_t[(kPadMax-kPadMin+2)*(kTbMax-kTbMin+2)];
      cout << "kPadMin/kPadMax\t" << kPadMin << "/" << kPadMax
	   << "\tkTbMin/kTbMax\t" << kTbMin << "/" << kTbMax << endl;
      memset (adcs, 0, (kPadMax-kPadMin+2)*(kTbMax-kTbMin+2)*sizeof(Int_t));
      for  (Int_t i = 0; i < fNoPixels; i++) {
	pad = fPixels_mPad[i];
	tb  = fPixels_mTimeBin[i];
	if (tb >= 512) tb = kTbMax + 1;
	if (pad > 182) pad = kPadMax + 1;
	indx = (kTbMax-kTbMin+2)*(pad - kPadMin) + tb - kTbMin;
	adcs[indx] = fPixels_mAdc[i];
      }
      cout << " ";
      for (tb = kTbMin; tb <= kTbMax + 1; tb++) cout << "\t|" << tb;
      cout << endl;
      cout << "_";
      for (tb = kTbMin; tb <= kTbMax + 1; tb++) cout << "\t|____";
      cout << endl;
      for (pad = kPadMin; pad <= kPadMax + 1; pad++) {
	if (pad == kPadMax + 1) cout << "---------------------------------------------" << endl;
	cout << pad << "|";
	for (tb = kTbMin; tb <= kTbMax + 1; tb++) {
	  indx = (kTbMax-kTbMin+2)*(pad - kPadMin) + tb - kTbMin;
	  if (tb <= kTbMax) cout << "\t" << adcs[indx];
	  else              cout << "\t|" << adcs[indx];
	}
	cout << endl;
      }
      delete [] adcs;
#if 1
      for (Int_t i = 0; i < fNoPixels; i++) 
	cout << i 
	     << "\tRow\t" << (int) fPixels_mRow[i]
	     << "\tPad\t" << fPixels_mPad[i]
	     << "\tTimeBin\t" << fPixels_mTimeBin[i]
	     << "\tadc\t" << (int) fPixels_mAdc[i]
	     << "\tId\t" << (int) fPixels_mIdTruth[i]
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
    if (! fNoPixels ) continue;
    if (fRcTrack_fNfitpoints[0] < 25) continue;
    if (fNoPixels > 80) continue;
    //    if (fNoRcHit < NpadCut) continue;
    if (fRcHit_mCharge[0] < 1.e-6 || fRcHit_mCharge[0] > 100.e-6) continue;
#if 0
    Double_t pmom = TMath::Sqrt(fRcTrack_fpx[0]*fRcTrack_fpx[0]+fRcTrack_fpy[0]*fRcTrack_fpy[0]);//+fRcTrack_fpz[0]*fRcTrack_fpz[0]);
#endif
    //    if (pMomin > 0 && pmom < pMomin || pMomax >0 && pmom > pMomax) continue;
    //    if (tanCut > 0 && TMath::Abs(fRcTrack_fpy[0]) > 1e-7 && TMath::Abs(fRcTrack_fpx[0]/fRcTrack_fpy[0]) > tanCut) continue;
    // if (Cut(ientry) < 0) continue;
    if (fNoRcHit != 1) continue;
#ifndef __REAL_DATA__
    if (fRcHit_mQuality[0] < 95) continue;
#endif
    if (fAdcSum < 100 || fAdcSum > 2.e3) continue;
    // Sector 1; RDO 4;
    //    if (fRcTrack_fSector[0] == 1 && fRcTrack_fRow[0] >= 22 && fRcTrack_fRow[0] <= 29) continue;
    for (Int_t i = 0; i < fNoPixels; i++) {
      Int_t io = 0;
      if (fPixels_mRow[i] > 13) io = 1;
      Double_t ratio = ((Double_t) fPixels_mAdc[i])/((Double_t) fAdcSum);
      if (fPixels_mTimeBin[i] > 512 && fPixels_mPad[i] < 200) {
	  hist[io][0][0]->Fill(fPixels_mPad[i]-(fRcHit_mMcl_x[0]/64.), fRcHit_mPosition_mX3[0], ratio);
#ifndef __REAL_DATA__
	if (fNoMcHit == 1)
	  hist[io][1][0]->Fill(fPixels_mPad[i]-(fMcHit_mMcl_x[0]/64.), fMcHit_mPosition_mX3[0],ratio);
#endif
	if (fNoRcTrack == 1)
	  hist[io][2][0]->Fill(fPixels_mPad[i]-(fRcTrack_mMcl_x[0]/64.), fRcHit_mPosition_mX3[0],ratio);
      }
      if (fPixels_mPad[i] > 200 && fPixels_mTimeBin[i] <= 512) {
	  hist[io][0][1]->Fill(fPixels_mTimeBin[i]-(fRcHit_mMcl_t[0]/64.), fRcHit_mPosition_mX3[0],ratio);
#ifndef __REAL_DATA__
	if (fNoMcHit == 1)
	  hist[io][1][1]->Fill(fPixels_mTimeBin[i]-(fMcHit_mMcl_t[0]/64.),   fMcHit_mPosition_mX3[0],ratio);
#endif
	if (fNoRcTrack == 1)
	  hist[io][2][1]->Fill(fPixels_mTimeBin[i]-(fRcTrack_mMcl_t[0]/64.), fRcHit_mPosition_mX3[0],ratio);
      }
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
  Double_t p = par[2];
  Double_t t = x[0]*width/tau;
  Double_t Delta = width/tau;
  Double_t t1 = t - Delta/2.;
  Double_t t2 = t1 + Delta;
  Double_t val = TMath::Gamma(p,t2) - TMath::Gamma(p,t1);
  //  return (val > 0) ? TMath::Log(val) : -999.;
  return val;
}
//________________________________________________________________________________
Double_t Gatti(Double_t *x, Double_t *par) {
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
  Double_t w = par[0]; // w = width of pad       
  Double_t y = w*TMath::Abs(x[0]);   // distance to center of strip
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
  if (CrossTalk > 0) {
    for (Int_t i = -1; i <= 1; i++) {
      Double_t xx = x[0] + i;
      if (i == 0) Value += (1. - 2.*CrossTalk)*Gatti(&xx,par);
      else        Value +=          CrossTalk *Gatti(&xx,par);
    }
  } else   Value = Gatti(x,par);
  return Value;
}
//________________________________________________________________________________
Double_t ConvolutionF(Double_t *x, Double_t *par) {
  Int_t icase = (Int_t) par[0]; // 0 - Inner, 1 - Outer, 2 - Time Shape
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
    pfunc->SetParameter(3,par[7]); // p
    pfunc->SetParameter(0,par[8]); // tau
  }
  if (! pfunc) return 0;
  Double_t Area   = par[1];
  Double_t sigma2 = TMath::Abs(par[2]);
  Double_t sigma  = TMath::Sqrt(sigma2);
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
      value += pfunc->Eval(xa)*TMath::Gaus(xx, xc, sigma, 1);
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
    mShaperResponse = new TF1("ShaperFunc",ShaperFunc,timeBinMin,timeBinMax,3);  
  Double_t mTau                       = tau; //1.e-9*gStTpcDb->Electronics()->tau();// s 
  mShaperResponse->SetParameters(mTau,mTimeBinWidth,pShaper);
  Double_t params[10];
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
				      PadResponseFunc,xmin,xmax,5); 
  params[3] =  K3IP;  
  params[4] =  0; // CrossTalk
  mPadResponseFunctionInner->SetParameters(params);
  //      mPadResponseFunctionInner->Save(xmin,xmax,0,0,0,0);
  xmax = 5.0;//5*gStTpcDb->PadPlaneGeometry()->innerSectorPadLength(); // 1.42
  if (! mChargeFractionInner) 
    mChargeFractionInner = new TF1("ChargeFractionInner",
				 PadResponseFunc,xmin,xmax,5);
  params[0] = 1.15;//gStTpcDb->PadPlaneGeometry()->innerSectorPadLength();
  params[3] = K3OP;
  params[4] =  0; // CrossTalk
  mChargeFractionInner->SetParameters(params);
  //  mChargeFractionInner->Save(xmin,xmax,0,0,0,0);
  xmax = 5;//5.*gStTpcDb->PadPlaneGeometry()->outerSectorPadWidth(); // 3.
  if (! mPadResponseFunctionOuter) 
    mPadResponseFunctionOuter = new TF1("PadResponseFunctionOuter",
				      PadResponseFunc,xmin,xmax,5); 
  params[0] = 6.2e-01;//gStTpcDb->PadPlaneGeometry()->outerSectorPadWidth();                    // w = width of pad       
  params[1] = 0.4;//gStTpcDb->WirePlaneGeometry()->outerSectorAnodeWirePadPlaneSeparation(); // h = Anode-Cathode gap   
  params[2] = 0.4;//gStTpcDb->WirePlaneGeometry()->anodeWirePitch();                         // s = wire spacing       
  //      params[3] = gStTpcDb->WirePlaneGeometry()->anodeWireRadius();                        // a = Anode wire radius  
  params[3] = K3OP;    // K3 from E.Mathieson, Fig. 5.3b (pads) for a/s = 2.5e-3 and h/s = 1
  params[4] =  0; // CrossTalk
  params[5] = 0;
  mPadResponseFunctionOuter->SetParameters(params);
  //    mPadResponseFunctionOuter->Save(xmin,xmax,0,0,0,0);
  xmax = 5;//5*gStTpcDb->PadPlaneGeometry()->outerSectorPadLength(); // 1.26
  if (! mChargeFractionOuter) 
    mChargeFractionOuter = new TF1("ChargeFractionOuter",
				 PadResponseFunc,xmin,xmax,5);
  params[0] = 1.95; //gStTpcDb->PadPlaneGeometry()->outerSectorPadLength();
  params[3] =  0.61;    // K3 from E.Mathieson, Fig. 5.3a (row) for a/s = 2.5e-3 and h/s = 1.0
  params[4] =  0; // CrossTalk
  mChargeFractionOuter->SetParameters(params);
  //  mChargeFractionOuter->Save(xmin,xmax,0,0,0,0);
  if (! mConvolution) 
    mConvolution = new TF1("Convolution",ConvolutionF,-8,8,10);
  mConvolution->SetParName(0,"icase"); mConvolution->FixParameter(0,0);
  mConvolution->SetParName(1,"Area");  mConvolution->FixParameter(1,1); mConvolution->SetParLimits(1,0,1e5);
  mConvolution->SetParName(2,"#sigma^{2}");  mConvolution->SetParLimits(2,0,10);
  mConvolution->SetParName(3,"K3I");   mConvolution->FixParameter(3,K3IP);
  mConvolution->SetParName(4,"K3O");   mConvolution->FixParameter(4,K3OP);
  mConvolution->SetParName(5,"Shift"); mConvolution->FixParameter(5,0);
  mConvolution->SetParName(6,"Noise"); mConvolution->FixParameter(6,0);
  mConvolution->SetParName(7,"p"); mConvolution->FixParameter(7,pShaper);
  mConvolution->SetParName(8,"#tau"); mConvolution->FixParameter(8,tau);
  mConvolution->SetParName(9,"CrossTalk"); mConvolution->FixParameter(9,0);
}
//________________________________________________________________________________
void FitSlices(const Char_t *name, const Char_t *opt, Int_t iy) {
  TProfile2D *h = (TProfile2D *) gDirectory->Get(name);
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
  if (Name.Contains("Pad",TString::kIgnoreCase)) 
    if (Name.Contains("Inner",TString::kIgnoreCase))         ga->FixParameter(0,0);
    else  {if (Name.Contains("Outer", TString::kIgnoreCase)) ga->FixParameter(0,1);}
  else                                                       ga->FixParameter(0,2); // Time
  ga->ReleaseParameter(2); ga->SetParLimits(2,0,10); ga->SetParameter(2,1e-5);
  ga->FixParameter(3,K3IP); 
  ga->FixParameter(4,K3OP);
  ga->FixParameter(5,0); // shift
  ga->FixParameter(6,0); // noise
  ga->FixParameter(7,pShaper);
  ga->FixParameter(8,tau);
  if (Name.Contains("Inner",TString::kIgnoreCase)) ga->FixParameter(9,CrossTalkInner); 
  else                                             ga->FixParameter(9,CrossTalkOuter);
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
      ga->SetParLimits(k,0.5*K3,10*K3);
      if (! Opt.Contains("K3S")) ga->FixParameter(2,0);
    }
  }
  if (Name.Contains("Time",TString::kIgnoreCase)) {
    ga->ReleaseParameter(5);    ga->SetParLimits(5,-5,5); // shift
    ga->ReleaseParameter(6);    ga->SetParLimits(6, 0,1); // noise
    if (Opt.Contains("pShape",TString::kIgnoreCase)) {
      ga->ReleaseParameter(7); // 
      ga->SetParLimits(7,2,10);
    }
    if (Opt.Contains("Tau",TString::kIgnoreCase)) {
      ga->SetParLimits(8,20e-9,100e-9);
    }
  }
  Int_t Npar = ga->GetNpar();
  TH1D **out = new TH1D*[Npar+1];
  memset(out, 0, (Npar+1)*sizeof(TH1D*));
  Double_t pmin, pmax;
  TString pName("");
  TString pTitle("");
  for (Int_t i = 0; i <= Npar; i++) {
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
      out[i] = (TH1D *) gDirectory->Get(pName.Data());
      if (! out[i]) 
	out[i] = new TH1D(pName.Data(),pTitle.Data(),
			  ny,h->GetYaxis()->GetXmin(),h->GetYaxis()->GetXmax());
    }
  }
  Int_t iy1 = 1;
  Int_t iy2 = ny;
  if (iy) {iy1 = iy2 = iy;}
  for (Int_t i = iy1; i <= iy2; i++) {
    TH1D *proj = h->ProjectionX(Form("%s_%i%s",Name.Data(),i,opt),i,i);
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
    proj->Fit(ga);
    //    Double_t prob = ga->GetProb();
    Double_t chisq = ga->GetChisquare(); 
    out[Npar]->SetBinContent(i,chisq);
    if (chisq < 1.e5) {
      for (Int_t j = 0; j < Npar; j++) {
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
//================================================================================
void TpcTFitSlices(const Char_t *name = "OuterPadRc") {
  TProfile2D *h = (TProfile2D *) gDirectory->Get(name);
  if (! h) return;
  TF1 *ga = new TF1("ga","exp([0]-x*x/(2*[1]))+[2]",-2.,2.);
  ga->SetParameters(0,1,0);
  ga->SetParLimits(1,0.1,100);
  h->FitSlicesX(ga,0,0,10,"r");
}
//================================================================================
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
