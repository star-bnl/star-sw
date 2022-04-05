#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TAxis.h"
#include "TF1.h"
#include "TH1.h"
#include "TH2.h"
#include "TSystem.h"
#include "TMath.h"
#include "StdEdxModel.h"
//#include "StMessMgr.h" 
using namespace std;
ClassImp(StdEdxModel)
StdEdxModel  *StdEdxModel::fgStdEdxModel = 0;
TH1D         *StdEdxModel::mdNdxL10 = 0;  
TH1D         *StdEdxModel::mdNdx = 0;  
TH2F         *StdEdxModel::mdEdNModel[3][3] = {0};
TH1F         *StdEdxModel::mdEdNMPV[3] = {0};
TH2F         *StdEdxModel::mLogdEdxModel[3][3] = {0};
TH1F         *StdEdxModel::mLogdEdNMPV[3] = {0};
//Double_t      StdEdxModel::fScale = TMath::Exp(2.06656e-01+3.01711e-02); // Heed
Double_t      StdEdxModel::fScale = TMath::Exp(9.12015e-02); // Bichsel
Int_t         StdEdxModel::_debug   = 1;
//________________________________________________________________________________
StdEdxModel* StdEdxModel::instance() {
  if (! fgStdEdxModel) new StdEdxModel();
  return fgStdEdxModel;
}
//________________________________________________________________________________
StdEdxModel::StdEdxModel() {
  //  LOG_INFO << "StdEdxModel:: use StTpcRSMaker model for dE/dx calculations" << endm;
  cout << "StdEdxModel:: use StTpcRSMaker model for dE/dx calculations" << endl;
  if (! fgStdEdxModel) {
    TDirectory *dir = gDirectory;
    fgStdEdxModel = this;
    const Char_t *path  = ".:./StarDb/dEdxModel:$STAR/StarDb/dEdxModel";
    //    const Char_t *Files[2] = {"dEdxModel.root","dNdx_Heed.root"};
    const Char_t *Files[2] = {"dEdxModel.root","dNdx_Bichsel.root"};
    for (Int_t i = 0; i < 2; i++) { // files
      Char_t *file = gSystem->Which(path,Files[i],kReadPermission);
      if (! file) Fatal("StdEdxModel","File %s has not been found in path %s",Files[i],path);
      else        Warning("StdEdxModel","File %s has been found as %s",Files[i],file);
      TFile       *pFile = new TFile(file);
      if (i == 0) {
	const Char_t *TpcName[3] = {"O","I",""};
	const Char_t *DerName[3] = {"","X","Y"};
	for (Int_t l = 0; l <=  kTpcAll; l++) {
	  for (Int_t j = kProb; j <= kdProbdY; j++) {
	    TString name(Form("dEdN%sNorm%s",TpcName[l],DerName[j]));
	    mdEdNModel[l][j] = (TH2F *) pFile->Get(name);
	    assert(mdEdNModel[l][j]);    mdEdNModel[l][j]->SetDirectory(0);
	    name = Form("LogdEdN%sNorm%s",TpcName[l],DerName[j]);
	    mLogdEdxModel[l][j] = (TH2F *) pFile->Get(name);
	    assert(mLogdEdxModel[l][j]);    mLogdEdxModel[l][j]->SetDirectory(0);
	  }
	  TString name(Form("dEdN%sMPV",TpcName[l]));
	  mdEdNMPV[l] = (TH1F *) pFile->Get(name);
	  assert(mdEdNMPV[l]);    mdEdNMPV[l]->SetDirectory(0);
	  name = Form("LogdEdN%sMPV",TpcName[l]);
	  mLogdEdNMPV[l] = (TH1F *) pFile->Get(name);
	  assert(mLogdEdNMPV[l]);    mLogdEdNMPV[l]->SetDirectory(0);
	}
      } else if (i == 1) {
	mdNdx    = (TH1D *)         pFile->Get("dNdx");     if (mdNdx)    mdNdx->SetDirectory(0);
	mdNdxL10 = (TH1D *)         pFile->Get("dNdxL10");  if (mdNdxL10) mdNdxL10->SetDirectory(0);
	assert(mdNdx || mdNdxL10);
      }
      delete pFile;
      delete [] file;
    }
    dir->cd();
  }
}
//________________________________________________________________________________
StdEdxModel::~StdEdxModel() {
  fgStdEdxModel = 0;
  SafeDelete(mdNdxL10);
  for (Int_t i = 0; i <= kTpcAll; i++) 
    for (Int_t j = 0; j <= kdProbdY; j++) 
      SafeDelete(mdEdNModel[i][j]);
}
//________________________________________________________________________________
Double_t StdEdxModel::dNdx(Double_t poverm, Double_t charge) {
  if (!fgStdEdxModel) instance();
  if (mdNdx)    return fScale*charge*charge*mdNdx->Interpolate(poverm);
  if (mdNdxL10) return fScale*charge*charge*mdNdxL10->Interpolate(TMath::Log10(poverm));
  return 0;
}
//________________________________________________________________________________
Double_t StdEdxModel::zMPVFunc(Double_t *x, Double_t *p) {
  Double_t n_PL = x[0]; // log(n_P);
  Int_t kTpc = (Int_t) p[0];
  if (kTpc != kTpcOuter && kTpc != kTpcInner) kTpc = kTpcAll;
  return TMath::Log(instance()->GetdEdNMPV((ETpcType) kTpc)->Interpolate(x[0])) + n_PL;
}
//________________________________________________________________________________
TF1 *StdEdxModel::zMPV() {
  static TF1* f = 0;
  if (! f) {
    f = new TF1("zMPV",StdEdxModel::zMPVFunc, 3., 11., 1);
    f->SetParNames( "IO");
    f->FixParameter(0, 2);
  }
  return f;
}
//________________________________________________________________________________
Double_t StdEdxModel::zdEFunc(Double_t *x, Double_t *p) {
  Int_t kTpc = (Int_t) p[0];
  if (kTpc != kTpcOuter && kTpc != kTpcInner) kTpc = kTpcAll;
  Double_t n_PL = p[1]; // log(n_P);
  Double_t zMPVpion =  p[2];
  Double_t logdE = x[0] + zMPVpion;
  Double_t dEovern_P = TMath::Exp(logdE - n_PL);
  static Double_t xmin, xmax, ymin, ymax;
  static TH2 *histOld = 0;
  TH2 *hist = instance()->GetdEdN(kProb,(ETpcType) kTpc);
  if (hist != histOld) {
    histOld = hist;
    xmin = hist->GetXaxis()->GetXmin();
    xmax = hist->GetXaxis()->GetXmax();
    ymin = hist->GetYaxis()->GetXmin();
    ymax = hist->GetYaxis()->GetXmax();
  }
  Double_t val = 0;
  if (xmin < n_PL      && n_PL      < xmax &&
      ymin < dEovern_P && dEovern_P < ymax) {
    val = instance()->GetdEdN(kProb,(ETpcType) kTpc)->Interpolate(n_PL, dEovern_P);
  }
  return val;
}
//________________________________________________________________________________
TF1 *StdEdxModel::zdE() {
  static TF1* f = 0;
  if (! f) {
    f = new TF1("zMPV",StdEdxModel::zdEFunc, -5., 5., 3);
    f->SetParNames("kTpc", "n_PL", "zMPVpion"); 
    f->FixParameter(0, kTpcAll);
    f->SetParameter(1,TMath::Log(10)*2.17548); // n_PL_proton
    Double_t z = zMPV()->Eval(TMath::Log(10)*1.7231);
    f->SetParameter(2,z); // zMPVpion
  }
  return f;
}
// $Id: StdEdxModel.cxx,v 1.6 2019/11/19 14:44:41 fisyak Exp $
// $Log: StdEdxModel.cxx,v $
// Revision 1.6  2019/11/19 14:44:41  fisyak
// new dEdxModel for dN/dx, calibration for Run XVIII fixed Target
//
