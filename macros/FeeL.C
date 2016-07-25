#define TABLE
#ifndef __CINT__
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
void FeeL(const Char_t *topDir = "/star/rcf/disk1/star/fisyak/NTuples3/",
	 const Char_t *TreeName = "DeDxTree") {
  Int_t NoSector = 24;
  Int_t NoFee = 182;
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
      TFile *ff = new TFile("FeeCorrected.root","UPDATE");
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
    TFile *ff = new TFile("feeL.root","UPDATE");
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
      Double_t sum  = proj->Integral();
      Double_t rms = proj->GetRMS();
      if (sum > 0) rms /= sum;
      else         rms = 0;
      delete proj;
      h->SetCellContent(i,j,mean);
      h->SetCellError(i,j,rms);
      
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
void Fit() {
  
  TFile *fRootFile = (TFile *) gDirectory->GetFile();
  if (! fRootFile ) {printf("Cannot find/opne %s",RootFile); return;}
  TH3F *hFee = (TH3F *) fRootFile->Get("Fee");
  if (!hFee) {printf("Cannot find Fee\n"); return;}
  struct Fit_t {
    Float_t i;
    Float_t j;
    Float_t mean;
    Float_t rms;
    Float_t mu;
    Float_t sigma;
    Float_t entries;
    Float_t chisq;
  };
  Fit_t Fit;
  TString NewRootFile("fee_");
  NewRootFile += fRootFile->GetName();
  TFile *f = new TFile(NewRootFile.Data(),"RECREATE");
  TNtuple *FitP = new TNtuple("FitP","Fit results",
		     "i:j:mean:rms:mu:sigma:entries:chisq");//:mean1:rms1:mu1:entries1:chisq1");
  Int_t nx = hFee->GetNbinsX(); printf ("nx = %i\n",nx);
  Int_t ny = hFee->GetNbinsY(); printf ("ny = %i\n",ny);
  TH2D *mean = new TH2D("mean",hFee->GetTitle(),nx,0,nx,ny,0,ny);
  TH2D *rms = new TH2D("rms",hFee->GetTitle(),nx,0,nx,ny,0,ny);
  TH2D *entries = new TH2D("entries",hFee->GetTitle(),nx,0,nx,ny,0,ny);
  TH2D *mu = new TH2D("mu",hFee->GetTitle(),nx,0,nx,ny,0,ny);
  TH2D *sigma = new TH2D("sigma",hFee->GetTitle(),nx,0,nx,ny,0,ny);
  TH2D *chisq = new TH2D("chisq",hFee->GetTitle(),nx,0,nx,ny,0,ny);
  Double_t params[9];
  TF1 *g = new TF1("g","gaus",-0.2,0.2);
//   TF1 *g2 = new TF1("g2","gaus(0)+gaus(3)");
//   TF1 *g3 = new TF1("g3","gaus(0)+gaus(3)+gaus(6)");
  for (int i=0;i<nx;i++){
    for (int j=0;j<ny;j++){
      TH1D *proj = hFee->ProjectionZ("f_11",i+1,i+1,j+1,j+1);
      Fit.i = i;
      Fit.j = j;
      Fit.mean = proj->GetMean();
      Fit.rms  = proj->GetRMS();
      Fit.entries = proj->Integral();
      mean->Fill(i,j,Fit.mean);
      rms->Fill(i,j,Fit.rms);
      entries->Fill(i,j,Fit.entries);
      params[0] = 100;
      params[1] = 0.;
      params[2] = 0.11;
      Fit.chisq = -100;
      g->SetParameters(params);
      g->SetRange(Fit.mean-2*Fit.rms,Fit.mean+2*Fit.rms);
      if (Fit.entries > 100) {
	proj->Fit("g","RQ");
	g->GetParameters(params);
//       params[3]=1;
//       params[4]=0;
//       params[5]=1;
//       g2->SetParameters(params);
//       proj->Fit("g2");
//       g2->GetParameters(params);
//       params[6]=1;
//       params[7]=0;
//       params[8]=1;
//       g3->SetParameters(params);
//       proj->Fit("g3");
//       g3->GetParameters(params);
	Fit.chisq = g->GetChisquare();
      }
      Fit.mu = params[1];
      Fit.sigma = params[2];
      //      Fit.chisq = g3->GetChisquare();
      mu->Fill(i,j,Fit.mu);
      sigma->Fill(i,j,Fit.sigma);
      chisq->Fill(i,j,Fit.chisq);
      printf("%i/%i mean %f rms = %f entries = %f mu = %f sigma = %f chisq = %f\n",
	    i,j,Fit.mean,Fit.rms,Fit.entries,Fit.mu,Fit.sigma,Fit.chisq);
      FitP->Fill(&Fit.i);
      delete proj;
    }
  }
  f->Write();
  delete f;
} 
//________________________________________________________________________________
void FillTable(TH2D *mean,TH2D *entries) {
  Int_t nx = mean->GetNbinsX(); printf ("nx = %i\n",nx);
  Int_t ny = mean->GetNbinsY(); printf ("ny = %i\n",ny);
  FILE *fp = fopen("TABLE","w");
  fprintf(fp,"{\n");
  for (int j=1;j<=ny;j++){
    fprintf(fp,"{");
    for (int i=1;i<=nx;i++){
      Double_t res = -1;
      if (entries->GetCellContent(i,j) > 1.e4) {
	res = TMath::Exp(mean->GetCellContent(i,j));
      }
      if(i != 1)  fprintf(fp,",");
      fprintf(fp,"%6.3f",res);
      if (i==12) fprintf(fp,"\n");
    }
    fprintf(fp,"},  // fee%i\n",j);
  }
  fprintf(fp,"}\n");
  fclose(fp);
}
//________________________________________________________________________________
void FillNtuple(TH2D *mean,TH2D *rms,TH2D *mu,TH2D *entries, TH2D *chisq) {
  struct Fit_t {
    Float_t i;
    Float_t j;
    Float_t mean;
    Float_t rms;
    Float_t mu;
    Float_t entries;
    Float_t chisq;
//     Float_t mean1;
//     Float_t rms1;
//     Float_t mu1;
//     Float_t entries1;
//     Float_t chisq1;
  };
  Fit_t Fit;
  FitP = new TNtuple("FitP","Fit results",
		     "i:j:mean:rms:mu:entries:chisq");//:mean1:rms1:mu1:entries1:chisq1");
  Int_t nx = mean->GetNbinsX(); printf ("nx = %i\n",nx);
  Int_t ny = mean->GetNbinsY(); printf ("ny = %i\n",ny);
#ifdef TABLE
  FILE *fp = fopen("TABLE","w");
  fprintf(fp,"{\n");
  for (int j=1;j<=ny;j++){
    fprintf(fp,"{");
    for (int i=1;i<=nx;i++){
#else
  for (int i=1;i<=nx;i++){
    for (int j=1;j<=ny;j++){
#endif
      Fit.i = i;
      Fit.j = j;
      //      if (j%2 == 0) {
	Fit.mean = mean->GetCellContent(i,j);
	Fit.rms = rms->GetCellContent(i,j);
	Fit.mu = mu->GetCellContent(i,j);
	Fit.entries = entries->GetCellContent(i,j);
	Fit.chisq = chisq->GetCellContent(i,j);
//       }
//       else  {
// 	Fit.mean1 = mean->GetCellContent(i,j);
// 	Fit.rms1 = rms->GetCellContent(i,j);
// 	Fit.mu1 = mu->GetCellContent(i,j);
// 	Fit.entries1 = entries->GetCellContent(i,j);
// 	Fit.chisq1 = chisq->GetCellContent(i,j);
	FitP->Fill(&Fit.i);
#ifdef TABLE
      Double_t res = -1;
      if (Fit.entries > 1.e4 && 
	  (j <=250 &&  Fit.rms > 0.5 && Fit.rms < 0.6) ||
	  (j > 254 &&  Fit.rms > 0.5 && Fit.rms < 0.8)) {
	res = TMath::Exp(mean->GetCellContent(i,j));
      }
      if(i != 1)  fprintf(fp,",");
      fprintf(fp,"%6.3f",res);
      if (i==12) fprintf(fp,"\n");
#endif
	//      }
    }
#ifdef TABLE
    fprintf(fp,"},  // fee%i even/odd %i\n",(j-1)/2,(j-1)%2);
  }
  fprintf(fp,"}\n");
  fclose(fp);
#else
  }
#endif
}
