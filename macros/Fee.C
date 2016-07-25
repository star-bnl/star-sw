#ifndef __CINT__
#include "iostream.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TH2.h"
#include "TH3.h"
#include "TF1.h"
#include "TProfile.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TCanvas.h"
#include "TClassTable.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#endif
void Fee(const Char_t *topDir = "/star/rcf/scratch/fisyak/NTuples2/",
	 const Char_t *TreeName = "DeDxTree") {
  Int_t NoSector = 24;
  Int_t NoFee = 82;
  TH3F *Fee = new TH3F("Fee","dEdx versus sector and Fee",
		       NoSector,1,NoSector+1,
		       NoFee,0,NoFee,
		       200,-5.,5.);
  if (gClassTable->GetID("TDataSet")<0) gSystem->Load("libStar");
  TFileSet dirs(topDir);
  TDataSetIter next(&dirs,0);
  TDataSet *set = 0; 
  Int_t NFiles = 0;
  while ( (set = next()) ) {           
    if (strcmp(set->GetTitle(),"file") || 
	!strstr(set->GetName(),".root")) continue;
    TString File(gSystem->ConcatFileName(topDir,set->Path()));
    
    TString HName("F");
    HName += set->Path();
    HName.ReplaceAll("/","");
    HName.ReplaceAll(".root","");
    TFile f(File.Data());
    cout << " ================== Opened " <<  File.Data() << endl;
    NFiles++;
    TTree *DeDxTree = (TTree *) f.Get(TreeName);
    if (DeDxTree) {
      //      if (NFiles > 2) break;
      TH3F *fee = new TH3F(HName.Data(),"dEdx versus sector and Fee",
                           NoSector,1,NoSector+1,
			   NoFee,0,NoFee,
			   200,-5.,5.);
      TString plot("log(m_de/m_dx/m_zPi):m_fee:m_sector>>");
      plot += HName.Data();
      DeDxTree->Draw(plot.Data(),"m_de>0 && m_de<2e-4");
//       Int_t nentries = Int_t(DeDxTree->GetEntries());
      
//       Int_t nbytes = 0, nb = 0;
//       for (Int_t jentry=0; jentry<nentries;jentry++) {
// 	Int_t ientry = LoadTree(jentry); //in case of a TChain, ientry is the entry number in the current file
// 	nb = DeDxTree->GetEntry(jentry);   nbytes += nb;
// 	// if (Cut(ientry) < 0) continue;
//       }
      Fee->Add(fee);
      TFile *ff = new TFile("fee.root","UPDATE");
      fee->Write();
      delete ff;
      delete fee;
    }
    else {
      cout << "  --------------------  File is empty" << endl;
      //      break;
    }
    Fee->Draw("colz");
    gPad->Update();
    TFile *ff = new TFile("fee.root","UPDATE");
    Fee->Write();
    delete ff;
  }
}
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
  TFile *f = new TFile("fee3.root","RECREATE");
  Int_t nx = hFee->GetNbinsX();
  Int_t ny = hFee->GetNbinsY();
  TH2D *mean = new TH2D("mean",hFee->GetTitle(),nx,0,nx,ny,0,ny);
  TH2D *rms = new TH2D("rms",hFee->GetTitle(),nx,0,nx,ny,0,ny);
  TH2D *entries = new TH2D("entries",hFee->GetTitle(),nx,0,nx,ny,0,ny);
  TH2D *mu = new TH2D("mu",hFee->GetTitle(),nx,0,nx,ny,0,ny);
  TH2D *sigma = new TH2D("mu",hFee->GetTitle(),nx,0,nx,ny,0,ny);
  TH2D *chisq = new TH2D("chisq",hFee->GetTitle(),nx,0,nx,ny,0,ny);
  Double_t params[9];
  TF1 *g = new TF1("g","gaus");
  TF1 *g2 = new TF1("g2","gaus(0)+gaus(3)");
  TF1 *g3 = new TF1("g3","gaus(0)+gaus(3)+gaus(6)");
  for (int i=0;i<nx;i++){
    for (int j=0;j<ny;j++){
      TH1D *proj = hFee->ProjectionZ("f_11",i+1,i+1,j+1,j+1);
      mean->Fill(i,j,proj->GetMean());
      rms->Fill(i,j,proj->GetRMS());
      entries->Fill(i,j,proj->Integral());
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
      chisq->Fill(i,j,g3->GetChisquare());
      delete proj;
    }
  }
  mean->Write();
  rms->Write();
  entries->Write();
  mu->Write();
  sigma->Write()
  chisq->Write()
  delete f;
} 
//________________________________________________________________________________
void FillTable(TH2D *hist) {
}
