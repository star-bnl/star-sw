#include "TFile.h"
#include "TCanvas.h"
#include "TH1.h"
#include "TLegend.h"
#include "TEfficiency.h"
#include "Riostream.h"
#include "TStyle.h"
#include "TROOT.h"
#include "TPolyMarker.h"
using namespace std;
const Char_t *files[4] = {"pileup_emc.SL15StiCAPPV/MuMcPrV53.root",
			  "pileup_emc.SL15StiCAPPV/MuMcPrV53TMVARank.root",
			  "pileup_emc.SL15StiCAKFV/MuMcPrV54.root",
			  "pileup_emc.SL15StiCAKFV/MuMcPrV54TMVARank.root"};
const Char_t *V[2] = {"PPV","KFV"};
const Char_t *M[2] = {"old","TMVA"};
//________________________________________________________________________________
void VxEff() {
  TCanvas *c1  = new TCanvas("Eff","PPV and KFV efficiencies, old and TMVA ranking",1200,800);
  c1->Divide(2,1);
  TLegend *l[2];
  for (Int_t i = 0; i < 2; i++) { // PPV and KFV
    TH1F *frame = c1->cd(i+1)->DrawFrame(0,0,40,1.1);
    frame->SetTitle(Form("%s efficiencies",V[i]));
    frame->SetYTitle("Efficiency/Impurity");
    frame->SetXTitle("Reconstructible multiplicity");
    //    frame->SetXTitle("ToF hit multiplicity");
    l[i] = new TLegend(0.4,0.30,0.9,0.50);
    for (Int_t j = 0; j < 2; j++) { // old and TMVA
      TFile *f = new TFile(files[2*i+j]);
      if (! f) {
	cout << "File " << files[2*i+j] << " is missing" << endl;
	continue;
      }
      f->cd(); cout << "File " << gDirectory->GetName() << endl;
      TH1D *McRecMulT    = (TH1D *) f->Get("McRecMulT"); 
      if (! McRecMulT) {
	cout << "Histogram McRecMulT is missing." << endl;
	continue;
      }
      Double_t T = McRecMulT->GetEntries(); cout << "T = " << T;
      TH1D *McRecMulAny  = (TH1D *) f->Get("McRecMulAny");  Double_t A = McRecMulAny->GetEntries();  cout << "\tA = " << A;
      TH1D *McRecMulGood = (TH1D *) f->Get("McRecMulGood"); Double_t G = McRecMulGood->GetEntries(); cout << "\tG = " << G;
      TH1D *McRecMulBad  = (TH1D *) f->Get("McRecMulBad");  Double_t B = McRecMulBad->GetEntries();  cout << "\tB = " << B;
      TH1D *McRecMulBadT = (TH1D *) f->Get("McRecMulBadT"); Double_t P = McRecMulBadT->GetEntries();  cout << "\tP = " << P << endl;
      if (j == 0) {
	cout << "Efficiency for all " << McRecMulAny->GetName() << " and " << McRecMulT->GetName() << endl;
	TEfficiency *a = new TEfficiency(*McRecMulAny,*McRecMulT); 
	a->SetMarkerColor(4*j+1);
	a->Draw("same");
	l[i]->AddEntry(a,Form("Overall Efficiency (a) = %4.2f", A/T)); 
      }
      cout << "Efficiency for good " << McRecMulGood->GetName() << " and " << McRecMulT->GetName() << endl;
      TEfficiency *g = new TEfficiency(*McRecMulGood,*McRecMulT); 
      g->SetMarkerColor(4*j+2);
      l[i]->AddEntry(g,Form("%s Efficiency (b) = %4.2f",M[j],G/T)); 
      g->Draw("same");
      cout << "Efficiency for bad " << McRecMulBad->GetName() << " and " << McRecMulT->GetName() << endl;
      TEfficiency *b = new TEfficiency(*McRecMulBad,*McRecMulT); b->SetMarkerColor(4*j+3); b->Draw("same");
      l[i]->AddEntry(b,Form("%s Impurity (c) = %4.2f",M[j],B/T)); 
      if (P > 0) {
	cout << "Efficiency for true pile-up" << McRecMulBadT->GetName() << " and " << McRecMulT->GetName() << endl;
	TEfficiency *p = new TEfficiency(*McRecMulBadT,*McRecMulT); p->SetMarkerColor(4*j+4); p->Draw("same");
	l[i]->AddEntry(p,Form("%s Pile-up (p) = %4.2f",M[j],P/T)); 
      }
      delete f;
    }
    l[i]->Draw();
  }
}
//________________________________________________________________________________
void DrawRank(Int_t rebin=0, Double_t cut = 0) {
  const Char_t *names[4] = {"RankAny", "RankGood", "RankBad", "RankBadT"};
  const Char_t *title[4] = {"All", "Triggered","Out banch crossing","In banch crossing"};
  TLegend *l = new TLegend(0.5,0.7,0.9,0.9);
  for (Int_t i = 0; i < 4; i++) {
    TH1D *rank = (TH1D *) gDirectory->Get(names[i]);
    if (!i && ! rank) break;
    if (! rank) continue;
    if (!i) {
      Double_t X = cut;
      Double_t Y = 0;
      TPolyMarker *pm = new TPolyMarker(1, &X, &Y);
      rank->GetListOfFunctions()->Add(pm);
      pm->SetMarkerStyle(23);
      pm->SetMarkerColor(kRed);
      pm->SetMarkerSize(1.3);
    }
    rank->SetLineColor(i+1);
    rank->SetMarkerColor(i+1);
    rank->SetLineWidth(2);
    if (rebin) rank->Rebin(rebin);
    rank->Draw("same");
    l->AddEntry(rank,title[i]);
  }
  l->Draw();
}
//________________________________________________________________________________
void DrawRanks(Double_t ymax = 1e3, Int_t j1 = 0) {
  gStyle->SetOptStat(0);
  gStyle->SetLineWidth(2);
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("Ranks");
  if (c1) c1->Clear();
  else    c1  = new TCanvas("Ranks","PPV and KFV Ranks, old and TMVA ranking",1200,800);
  if (! j1)  c1->Divide(2,2);
  else       c1->Divide(2,2-j1);
  const Double_t cut[4] = {0, 0.250, 0., 0.183};
  for (Int_t i = 0; i < 2; i++) { // PPV and KFV
    for (Int_t j = j1; j < 2; j++) { // old and TMVA
      TFile *f = new TFile(files[2*i+j]);
      if (! f) continue;
      f->cd(); cout << "File " << gDirectory->GetName() << endl;
      TString title(V[i]);
      title += " "; title += M[j];
      TH1F *frame = c1->cd(2*(j-j1) + i + 1)->DrawFrame(-1,0,1,ymax);
      frame->SetTitle(title);
      frame->SetXTitle("Rank");
      DrawRank(5, cut[2*i+j]);
     }  
  }
}
