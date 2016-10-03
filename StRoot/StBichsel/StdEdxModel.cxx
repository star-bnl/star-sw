#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TF1.h"
#include "TF2.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TArrayD.h"
#include "TStyle.h"
#include "TFile.h"
#include "TCanvas.h"
#include "TString.h"
#include "TLegend.h"
#include "TRandom.h"
#include "TMultiDimFit.h"
#include "StdEdxModel.h"
#include "StMessMgr.h" 
using namespace std;
ClassImp(StdEdxModel)
StdEdxModel  *StdEdxModel::fgStdEdxModel = 0;
TH1D         *StdEdxModel::mdNdx = 0;   
TH1D         *StdEdxModel::mdNdE = 0;   
static        TCanvas *c1           = 0;
Int_t         StdEdxModel::_debug   = 1;
#ifndef __HEED_MODEL__
Double_t      StdEdxModel::mnPL10min   = 0.7; // log10(nP)
Double_t      StdEdxModel::mnPL10max   = 4.0;
TMultiDimFit *StdEdxModel::mDFit = 0;   
TH2D         *StdEdxModel::mdEdxMPV = 0;
TH3F         *StdEdxModel::mdEdxFun = 0;
Double_t      StdEdxModel::mzMin    = 0;
Double_t      StdEdxModel::mzMax    = 0;
Double_t      StdEdxModel::mdZ      = 0;
#else /* __HEED_MODEL__ */
Double_t      StdEdxModel::mnPLmin   = 0.4; // log(nP)
Double_t      StdEdxModel::mnPLmax   = 8.3;
TMultiDimFit *StdEdxModel::mDFit[2] = {0};   
TH2D         *StdEdxModel::mdEdxMPV[2] = {0};
TH3F         *StdEdxModel::mdEdxFun[2] = {0};
Double_t      StdEdxModel::mzMin[2]    = {0};
Double_t      StdEdxModel::mzMax[2]    = {0};
Double_t      StdEdxModel::mdZ[2]      = {0};
Char_t *StdEdxModel::namesOI[2] = {"Outer","Inner"};
Char_t *StdEdxModel::nOI[2] = {"O","I"};
#endif /* __HEED_MODEL__ */
//________________________________________________________________________________
StdEdxModel* StdEdxModel::instance() {
  if (! fgStdEdxModel) new StdEdxModel();
  return fgStdEdxModel;
}
//________________________________________________________________________________
StdEdxModel::StdEdxModel() {
  LOG_INFO << "StdEdxModel:: use StTpcRSMaker model for dE/dx calculations" << endm;
  if (! fgStdEdxModel) {
    TDirectory *dir = gDirectory;
    fgStdEdxModel = this;
    const Char_t *path  = ".:./StarDb/dEdxModel:$STAR/StarDb/dEdxModel";
    const Char_t *Files[3] = {"dEdxModel.root","dNdx_Bichsel.root","dNdE_Bichsel.root"};
    for (Int_t i = -1; i < 3; i++) {
      if (i == -1) {
#ifndef __HEED_MODEL__
	mdEdxMPV = (TH2D *)      gDirectory->Get("dEdxMPV_MDFpar"); 
	mdEdxFun = (TH3F *)      gDirectory->Get("dEdxFun");        
	if (mdEdxFun) {
	  i = 0; 
	  Warning("StdEdxModel","Histograms %s has been found im memory", mdEdxFun->GetName());
	}
#else /* __HEED_MODEL__ */
	//	mdEdxMPV = (TH2D *)      gDirectory->Get("dEdxMPV_MDFpar"); 
	mdEdxMPV[kTpcOuter] = (TH2D *)      gDirectory->Get("dEdxMPVOuter"); 
	mdEdxMPV[kTpcInner] = (TH2D *)      gDirectory->Get("dEdxMPVInner"); 
	mdEdxFun[kTpcOuter] = (TH3F *)      gDirectory->Get("dEdxFunOuter");        
	mdEdxFun[kTpcInner] = (TH3F *)      gDirectory->Get("dEdxFunInner");        
	if (mdEdxFun[kTpcOuter]) {
	  i = 0; 
	  Warning("StdEdxModel","Histograms %s has been found im memory", mdEdxFun[kTpcOuter]->GetName());
	}
#endif /* __HEED_MODEL__ */
	continue;
      }
      Char_t *file = gSystem->Which(path,Files[i],kReadPermission);
#ifndef __HEED_MODEL__
      if (! file) Fatal("StdEdxModel","File %s has not been found in path %s",Files[i],path);
      else        Warning("StdEdxModel","File %s has been found as %s",Files[i],file);
#else /* __HEED_MODEL__ */
      if (! file) Fatal("StdEdxModel::","File %s has not been found in path %s",Files[i],path);
      else        Warning("StdEdxModel::","File %s has been found as %s",Files[i],file);
#endif /* __HEED_MODEL__ */
      TFile       *pFile = new TFile(file);
      if (i == 0) {
	//	mDFit = (TMultiDimFit *) pFile->Get("MDF_dEdxMPV");    assert(mDFit);    pFile->Remove(mDFit);
#ifndef __HEED_MODEL__
	mdEdxMPV = (TH2D *)      pFile->Get("dEdxMPV_MDFpar"); assert(mdEdxMPV); mdEdxMPV->SetDirectory(0);
	mdEdxFun = (TH3F *)      pFile->Get("dEdxFun");        assert(mdEdxFun); mdEdxFun->SetDirectory(0);
#else /* __HEED_MODEL__ */
	//	mdEdxMPV = (TH2D *)      pFile->Get("dEdxMPV_MDFpar"); assert(mdEdxMPV); mdEdxMPV->SetDirectory(0);
	mdEdxMPV[kTpcOuter] = (TH2D *)      pFile->Get("dEdxMPVOuter"); assert(mdEdxMPV[kTpcOuter]); mdEdxMPV[kTpcOuter]->SetDirectory(0);
	mdEdxMPV[kTpcInner] = (TH2D *)      pFile->Get("dEdxMPVInner"); assert(mdEdxMPV[kTpcInner]); mdEdxMPV[kTpcInner]->SetDirectory(0);
	mdEdxFun[kTpcOuter] = (TH3F *)      pFile->Get("dEdxFunOuter"); assert(mdEdxFun[kTpcOuter]); mdEdxFun[kTpcOuter]->SetDirectory(0);
	mdEdxFun[kTpcInner] = (TH3F *)      pFile->Get("dEdxFunInner"); assert(mdEdxFun[kTpcInner]); mdEdxFun[kTpcInner]->SetDirectory(0);
#endif /* __HEED_MODEL__ */
      } else if (i == 1) {
	mdNdx = (TH1D *)         pFile->Get("dNdx");           assert(mdNdx);    mdNdx->SetDirectory(0);
      } else if (i == 2) {
	mdNdE = (TH1D *)         pFile->Get("dNdE");           assert(mdNdE);    mdNdE->SetDirectory(0);
      }
      delete pFile;
      delete [] file;
    }
#ifndef __HEED_MODEL__
    if (mdEdxFun) {
      mzMin = mdEdxFun->GetZaxis()->GetXmin();
      mzMax = mdEdxFun->GetZaxis()->GetXmax();
      mdZ   = mdEdxFun->GetZaxis()->GetBinWidth(1);
#else /* __HEED_MODEL__ */
    if (mdEdxFun[kTpcOuter]) {
      mzMin[kTpcOuter] = mdEdxFun[kTpcOuter]->GetZaxis()->GetXmin();
      mzMax[kTpcOuter] = mdEdxFun[kTpcOuter]->GetZaxis()->GetXmax();
      mdZ[kTpcOuter]   = mdEdxFun[kTpcOuter]->GetZaxis()->GetBinWidth(1);
    }
    if (mdEdxFun[kTpcInner]) {
      mzMin[kTpcInner] = mdEdxFun[kTpcInner]->GetZaxis()->GetXmin();
      mzMax[kTpcInner] = mdEdxFun[kTpcInner]->GetZaxis()->GetXmax();
      mdZ[kTpcInner]   = mdEdxFun[kTpcInner]->GetZaxis()->GetBinWidth(1);
#endif /* __HEED_MODEL__ */
    }
    dir->cd();
  }
}
//________________________________________________________________________________
StdEdxModel::~StdEdxModel() {
  fgStdEdxModel = 0;
  SafeDelete(mdNdx);
  SafeDelete(mdNdE);
#ifndef __HEED_MODEL__
  SafeDelete(mDFit);
  SafeDelete(mdEdxMPV);
  SafeDelete(mdEdxFun);
#else /* __HEED_MODEL__ */
  for (Int_t i = 0; i < 2; i++) {
    SafeDelete(mDFit[i]);
    SafeDelete(mdEdxMPV[i]);
    SafeDelete(mdEdxFun[i]);
  }
#endif /* __HEED_MODEL__ */
}
//____________________________________________________________________________
Double_t StdEdxModel::zMPVFunc(Double_t *x, Double_t *p) {
#ifndef __HEED_MODEL__
  // most probable log(dE) versus log10(n_P) and sigma
  Double_t n_PL10   = x[0];
  if (n_PL10 > 4) n_PL10 = mnPL10max;
  if (n_PL10 < 0.7) n_PL10 = mnPL10min;
#else /* __HEED_MODEL__ */
  // most probable log(dE) versus log(n_P) and sigma
  Double_t n_PL   = x[0];
  if (n_PL > mnPLmax) n_PL = mnPLmax;
  if (n_PL < mnPLmin) n_PL = mnPLmin;
  //  Double_t n_P      = TMath::Exp(n_PL*ln10);
#endif /* __HEED_MODEL__ */
  Double_t sigma = x[1];
  //  Double_t Sigma = TMath::Sqrt(sigma*sigma + 1./n_P);
  Double_t Sigma = sigma;
  if (Sigma < 0.0) Sigma = 0.0;
  if (Sigma > 0.50) Sigma = 0.50;
#ifndef __HEED_MODEL__
  return mdEdxMPV->Interpolate(n_PL10, Sigma);
#else /* __HEED_MODEL__ */
  ESector kTpcOuterInner = kTpcOuter;
  if (p[0] > 0.5) kTpcOuterInner = kTpcInner;
  return mdEdxMPV[kTpcOuterInner]->Interpolate(n_PL, Sigma);
#endif /* __HEED_MODEL__ */
}
//________________________________________________________________________________
#ifndef __HEED_MODEL__
TF2 *StdEdxModel::zMPV() {
  static TF2 *f = 0;
  if (! f) {
    f = new TF2("zFunc",StdEdxModel::zMPVFunc, 0.3,4, 0.0,0.50, 0);
  }
  return f;
#else /* __HEED_MODEL__ */
TF2 *StdEdxModel::zMPV(ESector kTpcOuterInner) {
  static TF2 *f[2] = {0};
  if (! f[kTpcOuterInner]) {
    f[kTpcOuterInner] = new TF2(Form("zMPVFunc%s",namesOI[kTpcOuterInner]),StdEdxModel::zMPVFunc, mnPLmin, mnPLmax, 0.0,0.50, 1);
    f[kTpcOuterInner]->SetParameter(0, kTpcOuterInner);
  }
  return f[kTpcOuterInner];
#endif /* __HEED_MODEL__ */
}
//________________________________________________________________________________
#ifndef __HEED_MODEL__
Double_t StdEdxModel::dLogNtpernPdP(Double_t *x, Double_t *p) {
  static Double_t ln10 = TMath::Log(10.);
  Double_t z        = x[0]; // log (dE (keV))
  Double_t n_PL10   = p[0];
//   if (n_PL10 > 4) n_PL10 = 4;
//   if (n_PL10 < 0.7) n_PL10 = 0.7;
  Double_t n_P      = TMath::Exp(n_PL10*ln10);
  Double_t sigma = p[1];
  Double_t Sigma = sigma;
  if (Sigma < 0.00) Sigma = 0.00;
  if (Sigma > 0.50) Sigma = 0.50;
  Double_t n_T   = n_Tz(z)*p[2]; // TMath::Exp(z)/W(); n_T from log(dE[keV])
  if (n_T < 1.) return 0;
  Double_t w     = TMath::Log(n_T/n_P);
  if (w <= mzMin+mdZ/2 || w >= mzMax-mdZ/2) return 0;
  return mdEdxFun->Interpolate(n_PL10, Sigma, w);
}
#else /* __HEED_MODEL__ */
Double_t StdEdxModel::dLogdEpernPdP(Double_t *x, Double_t *p) {
  Double_t z        = x[0]; // log (dE (eV))
  Double_t n_PL     = p[0];
  if (n_PL > mnPLmax) n_PL = mnPLmax;
  if (n_PL < mnPLmin) n_PL = mnPLmin;
  Double_t n_P      = TMath::Exp(n_PL);
  Double_t sigma = p[1];
  Double_t Sigma = sigma;
  if (Sigma < 0.00) Sigma = 0.00;
  if (Sigma > 0.50) Sigma = 0.50;
  Double_t dE   = TMath::Exp(z)*p[2]; // eV
  if (dE < 1.) return 0;
  Double_t w     = TMath::Log(dE/n_P);
  ESector kTpcOuterInner = kTpcOuter;
  if (p[3] > 0.5) kTpcOuterInner = kTpcInner;
  if (w <= mzMin[kTpcOuterInner]+mdZ[kTpcOuterInner]/2 || w >= mzMax[kTpcOuterInner]-mdZ[kTpcOuterInner]/2) return 0;
  return mdEdxFun[kTpcOuterInner]->Interpolate(n_PL, Sigma, w);
}
#endif /* __HEED_MODEL__ */
#ifndef __HEED_MODEL__
//________________________________________________________________________________
TF1 *StdEdxModel::zFunc() {
  static TF1 *f = 0;
  if (! f) f = new TF1("zFunc",StdEdxModel::dLogNtpernPdP,-5,15.,3);
  f->SetNpx(1000);
  f->SetParName(0,"n_PL10"); f->SetParLimits(0,0,4);
  f->SetParName(1,"sigma");  f->SetParLimits(1,0,1);
  f->SetParName(2,"scale");
  f->SetParameters(TMath::Log10(30.),0.25,1.0);
  //  f->FixParameter(2,1.0);
  return f;
#else /* __HEED_MODEL__ */
//________________________________________________________________________________
TF1 *StdEdxModel::zFunc(ESector kTpcOuterInner) {
  static TF1 *f[2] = {0};
  if (! f[kTpcOuterInner]) f[kTpcOuterInner] = new TF1(Form("zFunc%s",namesOI[kTpcOuterInner]),StdEdxModel::dLogdEpernPdP,0,10.,4);
  f[kTpcOuterInner]->SetNpx(1000);
  f[kTpcOuterInner]->SetParName(0,"n_PL");   f[kTpcOuterInner]->SetParLimits(0,mnPLmin,mnPLmax);
  f[kTpcOuterInner]->SetParName(1,"sigma");  f[kTpcOuterInner]->SetParLimits(1,0,1);
  f[kTpcOuterInner]->SetParName(2,"scale");
  f[kTpcOuterInner]->SetParName(3,"OutIn");
  f[kTpcOuterInner]->SetParameters(TMath::Log(30.),0.25,1.0,kTpcOuterInner);
  //  f[kTpcOuterInner]->FixParameter(2,1.0);
  return f[kTpcOuterInner];
#endif /* __HEED_MODEL__ */
}
//________________________________________________________________________________
Double_t StdEdxModel::dEdxFunc(Double_t *x, Double_t *p) {
  // Probability (
#ifndef __HEED_MODEL__
  TF1 *f = instance()->zFunc();
  Double_t n_PL10 = TMath::Log10(p[1]);
  if (n_PL10 > 4) n_PL10 = 4;
  if (n_PL10 < 0.7) n_PL10 = 0.7;
  f->SetParameter(0,n_PL10);
  static TF1 *fMPV = 0;
  if (! fMPV) fMPV = instance()->zMPV();
  Double_t zMPV = fMPV->Eval(n_PL10,p[3]);
#else /* __HEED_MODEL__ */
  ESector kTpcOuterInner = kTpcOuter;
  if (p[5] > 0.5) kTpcOuterInner = kTpcInner;
  TF1 *f = instance()->zFunc(kTpcOuterInner);
  Double_t n_PL = TMath::Log(p[1]);
  if (n_PL > mnPLmax) n_PL = mnPLmax;
  if (n_PL < mnPLmin) n_PL = mnPLmin;
  f->SetParameter(0,n_PL);
  static TF1 *fMPV[2] = {0};
  if (! fMPV[kTpcOuterInner]) fMPV[kTpcOuterInner] = instance()->zMPV(kTpcOuterInner);
  Double_t zMPV = fMPV[kTpcOuterInner]->Eval(n_PL,p[3]);
#endif /* __HEED_MODEL__ */
  f->SetParameter(1,p[3]);
  f->SetParameter(2,p[4]);
  Double_t z = x[0]+zMPV-p[2];
  return TMath::Exp(p[0])*f->Eval(z);
}
//________________________________________________________________________________
#ifndef __HEED_MODEL__
TF1 *StdEdxModel::zdEdx() {
  static TF1 *f = 0;
  if (!f) f = new TF1("zdEdx",StdEdxModel::dEdxFunc,-5,15.,5);
  f->SetNpx(1000);
  f->SetParName(0,"norm");
  f->SetParName(1,"n_P"); f->SetParLimits(1,2,1e4);
  f->SetParName(2,"mu");  f->SetParLimits(2,-10,10);
  f->SetParName(3,"sigma"); f->SetParLimits(3,0.00,0.50);
  f->SetParName(4,"scale"); //f->SetParLimits(3,0.01,0.99);
  f->SetParameters(0.,30.,0.0,0.25, 1.0);
  return f;
 }
#else /* __HEED_MODEL__ */
TF1 *StdEdxModel::zdEdx(ESector kTpcOuterInner) {
  static TF1 *f[2] = {0};
  if (!f[kTpcOuterInner]) f[kTpcOuterInner] = new TF1(Form("zdEdx%s",namesOI[kTpcOuterInner]),StdEdxModel::dEdxFunc,-5,15.,6);
  f[kTpcOuterInner]->SetNpx(1000);
  f[kTpcOuterInner]->SetParName(0,"norm");
  f[kTpcOuterInner]->SetParName(1,"n_P"); f[kTpcOuterInner]->SetParLimits(1,2,1e4);
  f[kTpcOuterInner]->SetParName(2,"mu");  f[kTpcOuterInner]->SetParLimits(2,-10,10);
  f[kTpcOuterInner]->SetParName(3,"sigma"); f[kTpcOuterInner]->SetParLimits(3,0.00,0.50);
  f[kTpcOuterInner]->SetParName(4,"scale");
  f[kTpcOuterInner]->SetParName(5,"OI"); 
  f[kTpcOuterInner]->SetParameters(0.,30.,0.0,0.25, 1.0,kTpcOuterInner);
  return f[kTpcOuterInner];
}
#endif /* __HEED_MODEL__ */
//________________________________________________________________________________
#ifndef __HEED_MODEL__
Double_t StdEdxModel::zdE(Double_t n_P, Double_t sigma) {
  // Most probable log(n_T) 
  return instance()->zMPV()->Eval(TMath::Log10(n_P), sigma);// ? - zGeVkeV;
#else /* __HEED_MODEL__ */
Double_t StdEdxModel::zdE(Double_t n_P, Double_t sigma, ESector kTpcOuterInner) {
  // Most probable log(dE) 
  return instance()->zMPV(kTpcOuterInner)->Eval(TMath::Log(n_P), sigma);// ? - zGeVkeV;
#endif /* __HEED_MODEL__ */
}
#ifndef __HEED_MODEL__
//________________________________________________________________________________
void StdEdxModel::h2MDF(const Char_t  *total, Int_t max, Int_t maxTerm){
#else /* __HEED_MODEL__ */
//________________________________________________________________________________
TMultiDimFit *StdEdxModel::h2MDF(const Char_t  *total, Int_t max, Int_t maxTerm){
#endif /* __HEED_MODEL__ */
  TH2D *total2D = (TH2D *) gDirectory->Get(total);
  if (! total2D) {
    cout << "Histogram  has not been found " << endl;
#ifndef __HEED_MODEL__
    return;
#else /* __HEED_MODEL__ */
    return 0;
#endif /* __HEED_MODEL__ */
  }
  // Global data parameters 
  Int_t nVars       = 2;
  
  // make fit object and set parameters on it. 
  //  fit = new TMultiDimFit(nVars, TMultiDimFit::kMonomials,"vk");
#ifndef __HEED_MODEL__
  mDFit = new TMultiDimFit(nVars, TMultiDimFit::kChebyshev,"vk");
#else /* __HEED_MODEL__ */
  TMultiDimFit *mDFit = new TMultiDimFit(nVars, TMultiDimFit::kChebyshev,"vk");
#endif /* __HEED_MODEL__ */
  mDFit->SetName(Form("MDF_%s",total));
  gDirectory->Append(mDFit);
  Int_t mPowers[]   = {max , 3};
  mDFit->SetMaxPowers(mPowers);
  mDFit->SetMaxFunctions(1000);
  mDFit->SetMaxStudy(1000);
  mDFit->SetMaxTerms(maxTerm);
  mDFit->SetPowerLimit(max);
  mDFit->SetMinAngle(10);
  mDFit->SetMaxAngle(10);
  mDFit->SetMinRelativeError(.01);

  // variables to hold the temporary input data 
  TArrayD X(2);
  Double_t *x = X.GetArray(); 
  
  // Print out the start parameters
  mDFit->Print("p");
  
  TAxis *xa = total2D->GetXaxis();
  TAxis *ya = total2D->GetYaxis();
  Int_t nx = xa->GetNbins();
  Int_t ny = ya->GetNbins();
  Int_t iy, ix;
  for (iy = 1; iy <= ny; iy++) {
    for (ix = 1; ix <= nx; ix++) {
      Double_t error = total2D->GetBinError(ix,iy);
      if (error <= 0) continue;
      Double_t value = total2D->GetBinContent(ix,iy);
      x[0]           = xa->GetBinCenter(ix);
      x[1]           = ya->GetBinCenter(iy);
      if (x[0] < 0.6) continue;
      if (x[0] > 3.9) continue;
      Double_t yy = value;
      Double_t ee = 1e-4; //error*error;
      mDFit->AddRow(x,yy,ee);
    }
  }
  // Print out the statistics
  mDFit->Print("s");
  
  // Book histograms 
  mDFit->SetBinVarX(1000);
  mDFit->SetBinVarY(1000);
  mDFit->MakeHistograms();

  // Find the parameterization 
  mDFit->FindParameterization();

  // Print coefficents 
  mDFit->Print("rc");
#if 0
  //
   // Now for the data
   //
  Int_t i, j;
 // Assignment to coefficients vector.
  cout << "  row.PolyType = \t"      << mDFit->GetPolyType() << ";" << endl;
  cout << "  row.NVariables = \t"    << mDFit->GetNVariables() << ";" << endl;
  cout << "  row.NCoefficients = \t" << mDFit->GetNCoefficients() << ";" << endl;
  for (i = 0; i < mDFit->GetNVariables(); i++) {
    cout << Form("  row.XMin[%2i] = %10.5g;", i,mDFit->GetMinVariables()->operator()(i));
  }
  cout << endl;
  for (i = 0; i < mDFit->GetNVariables(); i++) {
    cout << Form("  row.XMax[%2i] = %10.5g;", i,mDFit->GetMaxVariables()->operator()(i));
  }
  cout << endl;
  for (i = 0; i < mDFit->GetNCoefficients(); i++) {
    for (j = 0; j < mDFit->GetNVariables(); j++) {
      cout << Form("  row.Power[%2i] = %2i;",i * mDFit->GetNVariables() + j,
		   mDFit->GetPowers()[mDFit->GetPowerIndex()[i] * mDFit->GetNVariables() + j]);
    }
    cout << endl;
  }
  cout << "  row.DMean = \t"          << mDFit->GetMeanQuantity() << ";" << endl;
  for (i = 0; i < mDFit->GetNCoefficients(); i++) {
    cout << Form("  row.Coefficients[%2i]    = %15.5g;", i, mDFit->GetCoefficients()->operator()(i));
    if ((i+1) %2 == 0) cout << endl;
  }
  if (mDFit->GetNCoefficients()%2) cout << endl;
  for (i = 0; i < mDFit->GetNCoefficients(); i++) {
    cout << Form("  row.CoefficientsRMS[%2i] = %15.5g;", i, mDFit->GetCoefficientsRMS()->operator()(i));
    if ((i+1) %2 == 0) cout << endl;
  }
  if (mDFit->GetNCoefficients()%2) cout << endl;
#endif
  TH2D *total2Dr = new TH2D(*total2D);
  total2Dr->SetName(Form("%s_MDFres",total2D->GetName()));
  total2Dr->Reset();
  TH2D *total2Df = new TH2D(*total2D);
  total2Df->SetName(Form("%s_MDFpar",total2D->GetName()));
  total2Df->Reset();
  for (iy = 1; iy <= ny; iy++) {
    for (ix = 1; ix <= nx; ix++) {
      Double_t error = total2D->GetBinError(ix,iy);
      if (error <= 0) continue;
      Double_t value = total2D->GetBinContent(ix,iy);
      x[0]           = xa->GetBinCenter(ix);
      x[1]           = ya->GetBinCenter(iy);
      Double_t par = mDFit->Eval(x);
      total2Df->SetBinContent(ix,iy,par);
      value -= par;
      total2Dr->SetBinContent(ix,iy,value);
    }
  }  
#ifdef __HEED_MODEL__
  return mDFit;
#endif /* __HEED_MODEL__ */
}
//________________________________________________________________________________
void StdEdxModel::MakedEdxModel() {
  
  if (_debug) {
    c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
    if (! c1 ) c1 = new TCanvas("c1","c1");
  }
  cout << "Make dEdxModel" << endl;
#ifndef __HEED_MODEL__
  /* nPdTO/nPdTI - log(Total no. of conducting electrons) - log(no. of primary one) versus no. primary electrons:Outer/Inner
     n_e - Total no. of conducting electrons
     n_P - no. primary electrons; n_p = dX * BB(beta*gamma), dX track segment length, BB is Bether Bloch function = no. of primary clusters per cm
     w = log(n_e/n_P); 
     dN/dw = f(w;n_P); // histogram
     Y = n_e = n_P*exp(w);
     dn_e/dw = n_e; 
     dN/dY = dN/dw*dw/Y = f(w;n_P)/Y;
     Log Normal distribution:
     ========================  
      g(X) = 1/((2*pi)*sigma)/X*exp(-(log(X - mu)**2/(2*sigma**2)));
      z = log(X);
      mu => 0
     ======================= 
      U  = w (*) Gaus(0,sigma); convolution 
      F(U;n_P,sigma);
      dE = W * n_P * exp(U):
      log(dE) = log(W) + log(n_P) + U;  mu = log(W);
      
      N_p = n_P/dX;

      dE/dx 
  */
#else /* __HEED_MODEL__ */
  /* nPdTO/nPdTI - log(Total dE[eV]) - log(no. of primary one) versus no. primary electrons:Outer/Inner
     dE - Total dE[eV]
     n_P - no. primary electrons; n_p = dX * BB(beta*gamma), dX track segment length, BB is Bether Bloch function = no. of primary clusters per cm
     w = log(dE/n_P); 
     dN/dw = f(w;n_P); // histogram
     Y = dE = n_P*exp(w);
     ddE/dw = dE; 
     Log Normal distribution:
     ========================  
      g(X) = 1/((2*pi)*sigma)/X*exp(-(log(X - mu)**2/(2*sigma**2)));
      z = log(X);
      mu => 0
     ======================= 
      U  = w (*) Gaus(0,sigma); convolution 
      F(U;n_P,sigma);
      dE = W * n_P * exp(U):
      log(dE) = log(W) + log(n_P) + U;  mu = log(W);
      
      N_p = n_P/dX;

      dE/dx 
  */
#endif /* __HEED_MODEL__ */
#ifndef __HEED_MODEL__
  TH2D *nPdTI = (TH2D *) gDirectory->Get("nPdTI");
  TH2D *nPdTO = (TH2D *) gDirectory->Get("nPdTO");
  if (! nPdTI || ! nPdTO) {
    if (! nPdTI) cout << "nPdTI is missing" << endl;
    if (! nPdTO) cout << "nPdTO is missing" << endl;
  } else {
    cout << "Found nPdTI and nPdTO" << endl;
  }
#else /* __HEED_MODEL__ */
  TFile *fIn = gFile;
#endif /* __HEED_MODEL__ */
  TFile *fOut = new TFile("dEdxModel.root","update");
  if (! fOut) return;
#ifndef __HEED_MODEL__
  TH2D *nPdT  = (TH2D *) fOut->Get("nPdT");
  if (! nPdT) {
    if (! nPdTI && ! nPdTO) return;
    nPdT  = new TH2D(*nPdTI);
    nPdT->SetName("nPdT");
    nPdT->Add(nPdTO);
  }
  TAxis *x = nPdT->GetXaxis();
  Int_t Nxbins = x->GetNbins();
  Nxbins = 157;
  const TArrayD     &xBins = *x->GetXbins();
  TArrayD XBins(Nxbins+1);
  for (Int_t ix = 0; ix <= Nxbins; ix++) XBins[ix] = TMath::Log10(xBins[ix+1]);
  //  TAxis *y = nPdT->GetYaxis();
  //  Int_t Nybins = y->GetNbins();
  Int_t    Nsigma = 51;
  Double_t sigmaMin = -.005;
  Double_t sigmaMax = 0.505;
  TArrayD     yBins(Nsigma+1);
  Double_t dsigma = (sigmaMax - sigmaMin)/Nsigma;
  yBins[0] = sigmaMin;
  for (Int_t i = 1; i <= Nsigma; i++) yBins[i] = yBins[i-1] + dsigma;
  Int_t Nzbins = 100;
  TArrayD     zBins(Nzbins+1);
  zBins[0]      = -2.5;
  zBins[Nzbins] =  7.5;
  Double_t dZ = ( zBins[Nzbins] -  zBins[0])/Nzbins;
  for (Int_t i = 1; i < Nzbins; i++) zBins[i] = zBins[i-1] + dZ;
  TH3F *dEdxFun = (TH3F *) fOut->Get("dEdxFun");
  if (! dEdxFun) dEdxFun = new TH3F("dEdxFun","w = log(n_T/n_P) versus log_10(n_P) and sigma",
				    Nxbins, XBins.GetArray(),
				    Nsigma, yBins.GetArray(),
				    Nzbins, zBins.GetArray());
  TH2D *dEdxMPV = (TH2D *) fOut->Get("dEdxMPV");
  if (! dEdxMPV)   dEdxMPV = new TH2D("dEdxMPV","most probable value of w = log(n_T/n_P) versus log_10(n_P) and sigma",
				      Nxbins, XBins.GetArray(),
				      Nsigma, yBins.GetArray());
  TH2D *dEdxMean = (TH2D *) fOut->Get("dEdxMean");
  if (! dEdxMean) dEdxMean = new TH2D("dEdxMean","mean value of w = log(n_T/n_P) versus log_10(n_P) and sigma",
				      Nxbins, XBins.GetArray(),
				      Nsigma, yBins.GetArray());
  TH2D *dEdxRMS = (TH2D *) fOut->Get("dEdxRMS");
  if (! dEdxRMS) dEdxRMS = new TH2D("dEdxRMS","RMS value of w = log(n_T/n_P) versus log_10(n_P) and sigma",
				    Nxbins, XBins.GetArray(),
				    Nsigma, yBins.GetArray());
  if (! dEdxFun->GetEntries()) return;
#else /* __HEED_MODEL__ */
  for (Int_t kkk = 0; kkk < 2; kkk++) {
    ESector kTpcOutIn = static_cast<ESector>(kkk);
    TH2D *nPdT = (TH2D *) fIn->Get(Form("nPdT%s",nOI[kTpcOutIn]));
    if (! nPdT) {cout << "nPdT" << nOI[kTpcOutIn] << " is missing" << endl; continue;}
    else        {cout << "nPdT" << nOI[kTpcOutIn] << " is found" << endl;}
    nPdT->Write();
    //    nPdT  = (TH2D *) fOut->Get("nPdT");
    TAxis *x = nPdT->GetXaxis();
    Int_t Nxbins = x->GetNbins();
    Nxbins = x->FindBin(mnPLmax);
    const TArrayD     &XBins = *x->GetXbins();
    Int_t    Nsigma = 51;
    Double_t sigmaMin = -.005;
    Double_t sigmaMax = 0.505;
    TArrayD     yBins(Nsigma+1);
    Double_t dsigma = (sigmaMax - sigmaMin)/Nsigma;
    yBins[0] = sigmaMin;
    for (Int_t i = 1; i <= Nsigma; i++) yBins[i] = yBins[i-1] + dsigma;
    Int_t Nzbins = 100;
    TArrayD     zBins(Nzbins+1);
    zBins[0]      =  0;
    zBins[Nzbins] = 10;
    Double_t dZ = ( zBins[Nzbins] -  zBins[0])/Nzbins;
    for (Int_t i = 1; i < Nzbins; i++) zBins[i] = zBins[i-1] + dZ;
    mdEdxFun[kTpcOutIn] = (TH3F *) fOut->Get(Form("dEdxFun%s",namesOI[kTpcOutIn]));
    if (! mdEdxFun[kTpcOutIn]) mdEdxFun[kTpcOutIn] = new TH3F(Form("dEdxFun%s",namesOI[kTpcOutIn]),
							      Form("w = log(dE/n_P) versus log(n_P) and sigma for %s",namesOI[kTpcOutIn]),
							      Nxbins, XBins.GetArray(),
							      Nsigma, yBins.GetArray(),
							      Nzbins, zBins.GetArray());
    mzMin[kTpcOutIn] = mdEdxFun[kTpcOutIn]->GetZaxis()->GetXmin();
    mzMax[kTpcOutIn] = mdEdxFun[kTpcOutIn]->GetZaxis()->GetXmax();
    mdZ[kTpcOutIn]   = mdEdxFun[kTpcOutIn]->GetZaxis()->GetBinWidth(1);
    mdEdxMPV[kTpcOutIn] = (TH2D *) fOut->Get(Form("dEdxMPV%s",namesOI[kTpcOutIn]));
    if (! mdEdxMPV[kTpcOutIn])   mdEdxMPV[kTpcOutIn] = new TH2D(Form("dEdxMPV%s",namesOI[kTpcOutIn]),
					Form("most probable value of w = log(dE/n_P) versus log(n_P) and sigma for %s",namesOI[kTpcOutIn]),
					Nxbins, XBins.GetArray(),
					Nsigma, yBins.GetArray());
    TH2D *dEdxMean = (TH2D *) fOut->Get(Form("dEdxMean%s",namesOI[kTpcOutIn]));
    if (! dEdxMean) dEdxMean = new TH2D(Form("dEdxMean%s",namesOI[kTpcOutIn]),
					Form("mean value of w = log(dE/n_P) versus log(n_P) and sigma for %s",namesOI[kTpcOutIn]),
					Nxbins, XBins.GetArray(),
					Nsigma, yBins.GetArray());
    TH2D *dEdxRMS = (TH2D *) fOut->Get(Form("dEdxRMS%s",namesOI[kTpcOutIn]));
    if (! dEdxRMS) dEdxRMS = new TH2D(Form("dEdxRMS%s",namesOI[kTpcOutIn]),
				      Form("RMS value of w = log(dE/n_P) versus log(n_P) and sigma for %s",namesOI[kTpcOutIn]),
					Nxbins, XBins.GetArray(),
					Nsigma, yBins.GetArray());
    if (mdEdxFun[kTpcOutIn]->GetEntries()) continue;
#endif /* __HEED_MODEL__ */
    for (Int_t iX = 1; iX <= Nxbins; iX++) {
#ifndef __HEED_MODEL__
      Double_t n_pL10 = dEdxFun->GetXaxis()->GetBinCenter(iX);
      Double_t n_p    = TMath::Power(10.,n_pL10);
      Int_t bin = x->FindBin(n_p);
#else /* __HEED_MODEL__ */
      Double_t n_pL = mdEdxFun[kTpcOutIn]->GetXaxis()->GetBinCenter(iX);
      Double_t n_p  = TMath::Exp(n_pL);
      Int_t bin = x->FindBin(n_pL);
#endif /* __HEED_MODEL__ */
      TH1D *proj = nPdT->ProjectionY("_y",bin,bin);
      if (proj->GetEntries() < 10) {delete proj; continue;}
      for (Int_t iY = 1; iY <= Nsigma; iY++) {
#ifndef __HEED_MODEL__
	Double_t sigma = dEdxFun->GetYaxis()->GetBinCenter(iY);
	TH1D *hist = dEdxFun->ProjectionZ("RnDM",iX,iX,iY,iY);
#else /* __HEED_MODEL__ */
	Double_t sigma = mdEdxFun[kTpcOutIn]->GetYaxis()->GetBinCenter(iY);
	TH1D *hist = mdEdxFun[kTpcOutIn]->ProjectionZ("RnDM",iX,iX,iY,iY);
#endif /* __HEED_MODEL__ */
	hist->SetName("RnDM");
	hist->Reset();
	Int_t NT = 100000;
	for (Int_t k = 0; k < NT; k++) {
	  Double_t u = proj->GetRandom();
	  Double_t w = 1.;
	  if (sigma > 0) {
	    u += gRandom->Gaus(0.,sigma);
	  }
	  hist->Fill(u, w);
	}
	hist->Smooth(5);
	Double_t norm = hist->Integral();
	hist->Scale(1./norm,"width");
	Int_t nZ = hist->GetNbinsX();
#ifndef __HEED_MODEL__
	for (Int_t iZ = 1; iZ <= nZ; iZ++) {
	  dEdxFun->SetBinContent(iX,iY,iZ,hist->GetBinContent(iZ));
	}
#else /* __HEED_MODEL__ */
	for (Int_t iZ = 1; iZ <= nZ; iZ++) {
	  mdEdxFun[kTpcOutIn]->SetBinContent(iX,iY,iZ,hist->GetBinContent(iZ));
	}
#endif /* __HEED_MODEL__ */
	dEdxMean->SetBinContent(iX,iY,hist->GetMean());
	dEdxRMS->SetBinContent(iX,iY,hist->GetRMS());
	if (c1) {
	  hist->Draw();
	  TH1 *test = dEdxFun->ProjectionZ("Test",iX,iX,iY,iY);
	  test->SetLineColor(2);
	  test->Draw("samel");
	  c1->Update();
	  delete test;
	}
	delete hist;
      }
      delete proj;
      cout << "Done with iX = " << iX << endl;
    }
#ifndef __HEED_MODEL__
  TF1 *f = StdEdxModel::instance()->zFunc();
  for (Int_t iY = 1; iY <= Nsigma; iY++) {
    Double_t sigma = dEdxFun->GetYaxis()->GetBinCenter(iY);
    f->SetParameter(1,sigma);
    for (Int_t iX = 1; iX <= Nxbins; iX++) {
      Double_t n_pL10 = dEdxFun->GetXaxis()->GetBinCenter(iX);
      f->SetParameter(0,n_pL10);
      Double_t mpv = f->GetMaximumX();
      dEdxMPV->SetBinContent(iX,iY,mpv);
    }
    h2MDF(dEdxMPV->GetName(),7,200);
#else /* __HEED_MODEL__ */
    TF1 *f = StdEdxModel::instance()->zFunc(kTpcOutIn);
    for (Int_t iY = 1; iY <= Nsigma; iY++) {
      Double_t sigma = mdEdxFun[kTpcOutIn]->GetYaxis()->GetBinCenter(iY);
      f->SetParameter(1,sigma);
      for (Int_t iX = 1; iX <= Nxbins; iX++) {
	Double_t n_pL = mdEdxFun[kTpcOutIn]->GetXaxis()->GetBinCenter(iX);
	f->SetParameter(0,n_pL);
	Double_t mpv = f->GetMaximumX(mnPLmin, mnPLmax);
	mdEdxMPV[kTpcOutIn]->SetBinContent(iX,iY,mpv);
      }
    }
    mDFit[kTpcOutIn] = h2MDF(mdEdxMPV[kTpcOutIn]->GetName(),7,200);
#endif /* __HEED_MODEL__ */
  }
  fOut->Write();
}
// $Id: StdEdxModel.cxx,v 1.2 2016/06/10 19:55:45 fisyak Exp $
// $Log: StdEdxModel.cxx,v $
// Revision 1.2  2016/06/10 19:55:45  fisyak
// Fix mem. leak (covertry)
//
// Revision 1.1  2015/12/24 00:16:25  fisyak
// Add TpcRS model and macros
//
