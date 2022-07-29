/*/hlt/cephfs/fisyak/TpcRS_MIP4/dEdx4/Fit
  root.exe pion.root proton.root kaon.root electron.root deuteron.root lBichsel.C dEdxFit.C+ FitMIP.C+

*/
#include "Riostream.h"
#include "TSeqCollection.h"
#include "TROOT.h"
#include "TString.h"
#include "TFile.h"
#include "TF1.h"
#include "TH3.h"
#include "TCanvas.h"
#include "TSystem.h"
#include "TMath.h"
#include "Ask.h"
TF1 *GG();
TF1 *GausExp();
//________________________________________________________________________________
TH1D *AddZ(TH1D *h1, TH1D *h2) {
  if (! h1 || ! h2) return 0;
  TH1D *h3 = new TH1D(*h1);
  h3->Reset();
  h3->SetName(Form("%s+%s",h1->GetName(),h2->GetName()));
  h3->SetTitle(Form("%s+%s",h1->GetTitle(),h2->GetName()));
  Int_t N1 = h1->GetEntries();
  Int_t N2 = h2->GetEntries();
  if (N1 < 1000 || N2 < 1000) return 0;
  for (Int_t ev = 0; ev < N1; ev++) {
    Double_t z1 = h1->GetRandom();
    Double_t z2 = h2->GetRandom();
    Double_t z  = TMath::Log(TMath::Exp(z1) + TMath::Exp(z2));
    h3->Fill(z);
  }
  return h3;
}
//________________________________________________________________________________
void FitMIP() {
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t NF = files->GetSize(); cout << "found " << NF << " files" << endl;
  if (! NF) return;
  TIter next(files);
  //  TF1 *g = GG();
  TF1 *g = GausExp();
  g->SetParLimits(1,-5.,5.);
  g->SetParLimits(3,0.01,5.0);
  TString Out = "FitMIP.data";
  ofstream out;
  if (gSystem->AccessPathName(Out)) out.open(Out, ios::out); //"Results.list",ios::out | ios::app);
  else                              out.open(Out, ios::app);
  out << "  static Double_t parMIP[6][3][3][4] = {" << endl << "    {{" << endl << "// particle, norml, mu, sigma, alpha" << endl; 

  TFile *f = 0;
  Int_t np = 0;
  Int_t NI = 3; // -,+,All
  Int_t NJ = 2; // I,O,All
  TH1D *hists[10][3][3] = {0};
  TString F[NF];
  TCanvas *c = new TCanvas("c1","c1",600*NI,400*NJ);
  c->Divide(NI,NJ);
  while ( (f = (TFile *) next()) ) { 
    f->cd();
    F[np] = gSystem->BaseName(f->GetName());
    F[np].ReplaceAll(".root","");
    cout << "Open " << F[np].Data() << endl;
    const Char_t *HistName[3] = {"SecRow3C","SecRow3PC","SecRow3+PC"};
    c->SetTitle(F[np]);
    TH3F *SecRow[3] = {0};
    for (Int_t i = 0; i < NI; i++) {
      TString N(F[np]);
      if      (i == 0) N += "N";
      else if (i == 1) N += "P";
      if (i < 2) SecRow[i] = (TH3F *) f->Get(HistName[i]);
      else {
	if (SecRow[0] && SecRow[1]) {
	  SecRow[i] = new TH3F(*SecRow[0]);
	  SecRow[i]->SetName(N);
	  SecRow[i]->Add(SecRow[1]);
	}
      }
      if (! SecRow[i]) continue;
      for (Int_t j = 0; j < NJ; j++) {// Inner/Outer
        if      (j == 0) hists[np][i][j] = SecRow[i]->ProjectionZ("zI"+N,1,24,1,40); else if (j == 1) hists[np][i][j] = SecRow[i]->ProjectionZ("zO"+N,1,24,41,72);
	else             hists[np][i][j] = SecRow[i]->ProjectionZ("zAll"+N,1,24,1,72);
	if (hists[np][i][j]->GetEntries() < 1e3) continue;
	c->cd(NI*j+i+1);
	g->SetParameters(0,hists[np][i][j]->GetMean(),1,1);
	//	hists[np][i][j]->Fit(g,"m");
	hists[np][i][j]->Fit(g,"m");
	out << Form("  /* %10s */ {%10.5f, %10.5f, %10.5f, %10.5f}", hists[np][i][j]->GetName(),g->GetParameter(0),g->GetParameter(1),g->GetParameter(2),g->GetParameter(3));
	if      (j != NJ - 1)   out << ",";
	else if (i != NI - 1)   out << " },{";
	else                    out << " }},{{";
	out << endl;
	c->Update();
      }
    }
    c->SaveAs(F[np] + ".png");
    if (Ask()) break;
    np++;
    c->Clear();
    c->Divide(NI,NJ);
  }
#if 1
  for (Int_t p1 = 0; p1 < np; p1++) {
    for (Int_t p2 = p1; p2 < np; p2++) {
      c->SetTitle(F[p1] + F[p2]);
      for (Int_t i = 0; i < NI; i++) {
	for (Int_t j = 0; j < NJ; j++) {
	  if (!hists[p1][i][j] || !hists[p2][i][j]) continue;
	  TH1D *h3 = AddZ(hists[p1][i][j],hists[p2][i][j]);
	  if (! h3) continue;
	  if (h3->GetEntries() < 1e3) continue;
	  c->cd(NI*j+i+1);
	  g->SetParameters(0,h3->GetMean(),1,1);
	  //	h3->Fit(g,"m");
	  h3->Fit(g,"m");
	  out << Form("  /* %10s */ {%10.5f, %10.5f, %10.5f, %10.5f}", h3->GetName(),g->GetParameter(0),g->GetParameter(1),g->GetParameter(2),g->GetParameter(3));
	  if      (j != NJ - 1)   out << ",";
	  else if (i != NI - 1)   out << " },{";
	  else if (p1 != np - 1)  out << " }},{{";
	  else                    out << " }}";
	  out << endl;
	  c->Update();
	}
      } 
      c->SaveAs(F[p1] + F[p2] + ".png");
      if (Ask()) break;
      c->Clear();
      c->Divide(NI,NJ);
    }
  }
#endif  
  out << "  };" << endl;
  out.close();
}
