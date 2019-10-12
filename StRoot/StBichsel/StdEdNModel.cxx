#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "StdEdNModel.h"
//#include "StMessMgr.h" 
using namespace std;
ClassImp(StdEdNModel)
StdEdNModel  *StdEdNModel::fgStdEdNModel = 0;
TH1D         *StdEdNModel::mdNdxL10 = 0;  
TH2F         *StdEdNModel::mdEdNModel[3][3] = {0};
TH1F         *StdEdNModel::mdEdNMPV[3] = {0};
TH2F         *StdEdNModel::mLogdEdNModel[3][3] = {0};
TH1F         *StdEdNModel::mLogdEdNMPV[3] = {0};
Double_t      StdEdNModel::fScale = 1.0;
Int_t         StdEdNModel::_debug   = 1;
//________________________________________________________________________________
StdEdNModel* StdEdNModel::instance() {
  if (! fgStdEdNModel) new StdEdNModel();
  return fgStdEdNModel;
}
//________________________________________________________________________________
StdEdNModel::StdEdNModel() {
  //  LOG_INFO << "StdEdNModel:: use StTpcRSMaker model for dE/dx calculations" << endm;
  cout << "StdEdNModel:: use StTpcRSMaker model for dE/dx calculations" << endl;
  if (! fgStdEdNModel) {
    TDirectory *dir = gDirectory;
    fgStdEdNModel = this;
    const Char_t *path  = ".:./StarDb/dEdxModel:$STAR/StarDb/dEdxModel";
    const Char_t *Files[2] = {"dEdNModel.root","dNdx_Heed.root"};
    for (Int_t i = 0; i < 2; i++) { // files
      Char_t *file = gSystem->Which(path,Files[i],kReadPermission);
      if (! file) Fatal("StdEdNModel","File %s has not been found in path %s",Files[i],path);
      else        Warning("StdEdNModel","File %s has been found as %s",Files[i],file);
      TFile       *pFile = new TFile(file);
      if (i == 0) {
	const Char_t *TpcName[3] = {"I","O",""};
	const Char_t *DerName[3] = {"","X","Y"};
	for (Int_t l = 0; l <=  kTpcAll; l++) {
	  for (Int_t j = kProb; j <= kdProbdY; j++) {
	    TString name(Form("dEdN%sNorm%s",TpcName[l],DerName[j]));
	    mdEdNModel[l][j] = (TH2F *) pFile->Get(name);
	    assert(mdEdNModel[l][j]);    mdEdNModel[l][j]->SetDirectory(0);
	    name = Form("LogdEdN%sNorm%s",TpcName[l],DerName[j]);
	    mLogdEdNModel[l][j] = (TH2F *) pFile->Get(name);
	    assert(mLogdEdNModel[l][j]);    mLogdEdNModel[l][j]->SetDirectory(0);
	  }
	  TString name(Form("dEdN%sMPV",TpcName[l]));
	  mdEdNMPV[l] = (TH1F *) pFile->Get(name);
	  assert(mdEdNMPV[l]);    mdEdNMPV[l]->SetDirectory(0);
	  name = Form("LogdEdN%sMPV",TpcName[l]);
	  mLogdEdNMPV[l] = (TH1F *) pFile->Get(name);
	  assert(mLogdEdNMPV[l]);    mLogdEdNMPV[l]->SetDirectory(0);
	}
      } else if (i == 1) {
	mdNdxL10 = (TH1D *)         pFile->Get("dNdxL10");           assert(mdNdxL10);    mdNdxL10->SetDirectory(0);
      }
      delete pFile;
      delete [] file;
    }
    dir->cd();
  }
}
//________________________________________________________________________________
StdEdNModel::~StdEdNModel() {
  fgStdEdNModel = 0;
  SafeDelete(mdNdxL10);
  for (Int_t i = 0; i <= kTpcAll; i++) 
    for (Int_t j = 0; j <= kdProbdY; j++) 
      SafeDelete(mdEdNModel[i][j]);
}
//________________________________________________________________________________
Double_t StdEdNModel::dNdx(Double_t poverm, Double_t charge) {
  return fScale*charge*charge*instance()->GetdNdxL10()->Interpolate(TMath::Log10(poverm));
}
// $Id: StdEdNModel.cxx,v 1.5 2018/10/17 20:45:23 fisyak Exp $
// $Log: StdEdNModel.cxx,v $
