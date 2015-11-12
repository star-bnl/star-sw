#include "StdEdxModel.h"
#include "StMessMgr.h" 
using namespace std;
ClassImp(StdEdxModel)
StdEdxModel  *StdEdxModel::fgStdEdxModel = 0;
TMultiDimFit *StdEdxModel::mDFit = 0;   
TH1D         *StdEdxModel::mdNdx = 0;   
TH2D         *StdEdxModel::mdEdxMPV = 0;
TH3F         *StdEdxModel::mdEdxFun = 0;
static Double_t dEdxShift = 0;
//________________________________________________________________________________
StdEdxModel* StdEdxModel::instance() {
  if (! fgStdEdxModel) new StdEdxModel();
  return fgStdEdxModel;
}
//________________________________________________________________________________
StdEdxModel::StdEdxModel() {
  LOG_INFO << "StdEdxModel:: use StTpcRSMaker model for dE/dx calculations" << endm;
  if (! fgStdEdxModel) {
    fgStdEdxModel = this;
    const Char_t *path  = ".:./StarDb/dEdxModel:$STAR/StarDb/dEdxModel";
    const Char_t *Files[2] = {"dEdxModel.root","dNdx_Bichsel.root"};
    for (Int_t i = 0; i < 2; i++) {
      Char_t *file = gSystem->Which(path,Files[i],kReadPermission);
      if (! file) Fatal("StdEdxModel","File %s has not been found in path %s",Files[i],path);
      else        Warning("StdEdxModel","File %s has been found as %s",Files[i],file);
      TFile       *pFile = new TFile(file);
      if (i == 0) {
	//	mDFit = (TMultiDimFit *) pFile->Get("MDF_dEdxMPV");    assert(mDFit);    pFile->Remove(mDFit);
	mdEdxMPV = (TH2D *)      pFile->Get("dEdxMPV_MDFpar"); assert(mdEdxMPV); mdEdxMPV->SetDirectory(0);
	mdEdxFun = (TH3F *)      pFile->Get("dEdxFun");        assert(mdEdxFun); mdEdxFun->SetDirectory(0);
      } else  {
	mdNdx = (TH1D *)         pFile->Get("dNdx");           assert(mdNdx);    mdNdx->SetDirectory(0);
      }
      delete pFile;
      delete [] file;
    }
    
  }
}
//________________________________________________________________________________
StdEdxModel::~StdEdxModel() {
  fgStdEdxModel = 0;
  SafeDelete(mDFit);
  SafeDelete(mdNdx);
  SafeDelete(mdEdxMPV);
  SafeDelete(mdEdxFun);
}
//____________________________________________________________________________
Double_t StdEdxModel::zMPVFunc(Double_t *x, Double_t *p) {
  static Double_t ln10 = TMath::Log(10.);
  Double_t n_PL10   = x[0];
  if (n_PL10 > 4) n_PL10 = 4;
  if (n_PL10 < 0.301) n_PL10 = 0.301;
  Double_t n_P      = TMath::Exp(n_PL10*ln10);
  Double_t sigma = x[1];
  Double_t Sigma = TMath::Sqrt(sigma*sigma + 1./n_P);
  if (Sigma < 0.01) Sigma = 0.01;
  if (Sigma > 0.99) Sigma = 0.99;
  return mdEdxMPV->Interpolate(n_PL10, Sigma);
}
//________________________________________________________________________________
TF2 *StdEdxModel::zMPV() {
  static TF2 *f = 0;
  if (! f) {
    f = new TF2("zFunc",StdEdxModel::zMPVFunc,0.3,4,0.01,0.99,0);
  }
  return f;
}
//________________________________________________________________________________
Double_t StdEdxModel::dLogNtpernPdP(Double_t *x, Double_t *p) {
  static Double_t ln10 = TMath::Log(10.);
  Double_t z        = x[0]; // log (dE (keV))
  Double_t n_PL10   = p[0];
  if (n_PL10 > 4) n_PL10 = 4;
  if (n_PL10 < 0.301) n_PL10 = 0.301;
  Double_t n_P      = TMath::Exp(n_PL10*ln10);
  Double_t sigma = p[1];
  Double_t Sigma = TMath::Sqrt(sigma*sigma + 1./n_P);
  if (Sigma < 0.01) Sigma = 0.01;
  if (Sigma > 0.99) Sigma = 0.99;
  Double_t n_T   = n_Tz(z); // TMath::Exp(z)/W();
  Double_t w     = TMath::Log(n_T/n_P);
  if (w <-1.95) w = -1.95;
  if (w > 7.95) w =  7.95;
  return mdEdxFun->Interpolate(n_PL10, Sigma, w);
}
//________________________________________________________________________________
TF1 *StdEdxModel::zFunc() {
  static TF1 *f = 0;
  if (! f) f = new TF1("zFunc",StdEdxModel::dLogNtpernPdP,-5,15.,3);
  f->SetNpx(1000);
  f->SetParName(0,"n_PL10");
  f->SetParName(1,"sigma");
  f->SetParameters(TMath::Log10(30.),0.25,0.0);
  return f;
}
//________________________________________________________________________________
Double_t StdEdxModel::dEdxFunc(Double_t *x, Double_t *p) {
  static TF1 *f = 0;
  if (! f) f = instance()->zFunc();
  Double_t n_PL10 = TMath::Log10(p[1]);
  if (n_PL10 > 4) n_PL10 = 4;
  if (n_PL10 < 0.301) n_PL10 = 0.301;
  f->SetParameter(0,n_PL10);
  f->SetParameter(1,p[3]);
  static TF1 *fMPV = 0;
  if (! fMPV) fMPV = instance()->zMPV();
  Double_t zMPV = fMPV->Eval(n_PL10,p[2]);
  return TMath::Exp(p[0])*f->Eval(x[0]+zMPV-p[2])/2000.;
}
//________________________________________________________________________________
TF1 *StdEdxModel::zdEdx() {
  static TF1 *f = 0;
  if (!f) f = new TF1("zdEdx",StdEdxModel::dEdxFunc,-5,15.,4);
  f->SetNpx(1000);
  f->SetParName(0,"scale");
  f->SetParName(1,"n_P"); f->SetParLimits(1,2,1e4);
  f->SetParName(2,"mu");  f->SetParLimits(2,-10,10);
  f->SetParName(3,"sigma"); f->SetParLimits(3,0.01,0.99);
  f->SetParameters(0.,30.,0.0,0.25);
  return f;
}
//________________________________________________________________________________
Double_t StdEdxModel::zdE(Double_t n_P, Double_t sigma) {
  // Most probable log(n_T) 
  static Double_t zGeVkeV = TMath::Log(1e6);
  return instance()->zMPV()->Eval(TMath::Log10(n_P), sigma);// ? - zGeVkeV;
}
// $Id: $
// $Log: $
