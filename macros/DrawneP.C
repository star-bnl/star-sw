#include "TF1.h"
#include "TCanvas.h"
#include "TH2.h"
#include "TFile.h"
#include "TROOT.h"
#include "TLegend.h"
#include "TString.h"
//________________________________________________________________________________
TH2F *Norm(TH2F *hxz, TH2F *newh=0)
{
  if (! newh) {
    newh = new TH2F(*hxz);
    newh->SetName(Form("%sNorm",hxz->GetName()));
  }
  Int_t nx = hxz->GetNbinsX();
  Int_t ny = hxz->GetNbinsY();
  Double_t Sum;
  Int_t i,j;
  for (i=1;i<=nx;i++) {
    Sum = 0.;
    for (j=1;j<=ny;j++) {
      Sum += hxz->GetCellContent(i,j);// printf("%i %i %f\n",i,j,Sum);
    }
    if (Sum > 10.0) {
      for (j=1;j<=ny;j++) {
	Double_t cont = hxz->GetCellContent(i,j)/Sum;
	Double_t err  = sqrt(cont*(1.-cont)/Sum);
	//	 printf("%i %i %f +/- %f\n",i,j,cont,err);
	if (newh) {
	  newh->SetCellContent(i,j,cont);
	  newh->SetCellError(i,j,err);
	}
      }
    }
  }
  return newh;
}
//________________________________________________________________________________
void DrawneP() {
  TFile *files[3] = {TFile::Open("ADCut32COL.root"),
		     TFile::Open("ADCut32FXT.root"),
		     TFile::Open("ADCut32.root")};
  const Char_t *histNames[3] = {"nePI","nePO","nePI+O"};
  TF1::InitStandardFunctions();
  TF1 *fpol7 = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol7");;
  fpol7->SetRange(1,12);
  Double_t parsM[8] = {   -4.3432,     4.6327,    -1.9522,     0.4691,  -0.066615,  0.0055111, -0.00024531, 4.5394e-06}; //mu pol7
  fpol7->SetParameters(parsM);
  fpol7->SetLineColor(2);
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (c1) c1->Clear();
  else    c1 = new TCanvas("c1","c1",800,1200);
  c1->Divide(3,3);
  TH2F *hist[3] = {0};
  for (Int_t i = 0; i < 3; i++) {
    for (Int_t j = 0; j < 3; j++) {
      c1->cd(3*j + i + 1)->SetLogz(1);
      if (j < 2) {
	hist[j] = (TH2F *) files[i]->Get(histNames[j]);
      } else {
	hist[j] = new TH2F(*hist[0]);
	hist[j]->SetName(histNames[j]);
	hist[j]->Add(hist[1]);
      }
      TH2F *normH = Norm(hist[j]);
      normH->SetMaximum(0.1);
      normH->Draw("colz");
      fpol7->Draw("same");
      TLegend *l = new TLegend(0.2,0.7,0.8,0.8);
      TString Name;
      if      (i == 0) Name = "COL";
      else if (i == 1) Name = "FXT";
      else             Name = "COL+FXT";
      if      (j == 0) Name += " I";
      else if (j == 1) Name += " O";
      else             Name += " I+O";
      l->AddEntry(normH, Name);
      l->Draw();
    }
  }
  c1->Update();
}
