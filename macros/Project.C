#ifndef __CINT__
#include "TH2.h"
#include "TH3.h"
#endif
//________________________________________________________________________________
TH2F *Project(TH3F *hFee) {
  if (!hFee) return 0;
  Int_t nx = hFee->GetNbinsX();
  Int_t ny = hFee->GetNbinsY();
  TH2F *h = new TH2F("profxy",hFee->GetTitle(),nx,0,nx,ny,0,ny);
  for (int i=0;i<nx;i++){
    for (int j=0;j<ny;j++){
      TH1D *proj = hFee->ProjectionZ("f_11",i+1,i+1,j+1,j+1);
      Double_t mean = proj->GetMean();
      Double_t rms = proj->GetRMS();
      delete proj;
      if (rms > 0.6) printf("i=%i j=%i mean=%f rms=%f\n",i,j,mean,rms);
      h->Fill(i,j,mean);
      
    }
  }
  return h;
} 
//________________________________________________________________________________
void Empty(TH3F *hist,Double_t Cut=500 ){
  if (!hist) return 0;
  Int_t nx = hist->GetNbinsX();
  Int_t ny = hist->GetNbinsY();
  Int_t nz = hist->GetNbinsZ();
  Int_t k = 0;
  for (int i=1;i<=nx;i++){
    for (int j=1;j<=ny;j++){
      Double_t stat = hist->Integral(i,i,j,j,1,nz);
      if (stat < Cut) {
	k++;
	printf("%i: i=%i j=%i stat=%f\n",k,i,j,stat);
      }
    }
  }
}
//________________________________________________________________________________
void Fit(TH3F *hFee) {
  if (!hFee) return;
  TFile *f = new TFile("fee2.root","RECREATE");
  Int_t nx = hFee->GetNbinsX();
  Int_t ny = hFee->GetNbinsY();
  TH2D *mean = new TH2D("mean",hFee->GetTitle(),nx,0,nx,ny,0,ny);
  TH2D *rms = new TH2D("rms",hFee->GetTitle(),nx,0,nx,ny,0,ny);
  TH2D *entries = new TH2D("entries",hFee->GetTitle(),nx,0,nx,ny,0,ny);
  TH2D *mu = new TH2D("mu",hFee->GetTitle(),nx,0,nx,ny,0,ny);
  TH2D *sigma = new TH2D("mu",hFee->GetTitle(),nx,0,nx,ny,0,ny);
  Double_t params[9];
  TF1 *g = new TF1("g","gaus");
  TF1 *g2 = new TF1("g2","gaus(0)+gaus(3)");
  TF1 *g3 = new TF1("g3","gaus(0)+gaus(3)+gaus(6)");
  for (int i=0;i<nx;i++){
    for (int j=0;j<ny;j++){
      TH1D *proj = hFee->ProjectionZ("f_11",i+1,i+1,j+1,j+1);
      mean->Fill(i,j,proj->GetMean());
      rms->Fill(i,j,proj->GetRMS());
      proj->Fit("g");
      g->GetParameters(params);
      params[3]=1;
      params[4]=0;
      params[5]=1;
      g2->SetParameters(params);
      proj->Fit("g2");
      g2->GetParameters(params);
      params[6]=1;
      params[7]=0;
      params[8]=1;
      g3->SetParameters(params);
      proj->Fit("g3");
      g3->GetParameters(params);
      mu->Fill(i,j,params[1]);
      sigma->Fill(i,j,params[2]);
      delete proj;
    }
  }
  mean->Write();
  rms->Write();
  entries->Write();
  mu->Write();
  sigma->Write()
  delete f;
} 
