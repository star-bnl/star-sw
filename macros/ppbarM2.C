#ifndef __CINT__
#include "Riostream.h"
#include "TDirectory.h"
#include "TObjArray.h"
#include "TH1.h"
#include "TH2.h"
#include "TF1.h"
#include "TLine.h"
#include "TAxis.h"
#include "TCanvas.h"
#include "TROOT.h"
#include "TLegend.h"
#include "TStyle.h"
#endif
void ppbarM2() {
  //  gStyle->SetOptStat(0);
  TF1 *func = new TF1("func","[0]*(1+[1]+[2]*x)",-0.5,0.5);
  Double_t m2P = 0.9382723*0.9382723;
  func->SetParameters(0,0,0);
  func->FixParameter(0,m2P);
  TLegend *l = new TLegend(0.2,0.2,0.6,0.4);
  TH2* p2 = (TH2 *) gDirectory->Get("/Tracks/p/hTofPID");
  if (! p2) return;
  TObjArray* arr =  new TObjArray(4);
  p2->FitSlicesY(0, 0, -1, 0, "QNR", arr);
  TH1D *m2p = (TH1D *) (*arr)[1]; m2p->SetMarkerColor(1); m2p->SetMarkerStyle(20);
  m2p->SetName("m2p");
  m2p->SetStats(0);
  m2p->Fit("func");
  //  TF1 *func = (TF1*) m2p->GetListOfFunctions()->FindObject("func");
  TString Title = Form("p   : %6.4f + %6.4f log_{10}P(%%)",100*func->GetParameter(1), 100*func->GetParameter(2));
  l->AddEntry(m2p,Title);
#if 1
  TH2* pbar2 = (TH2* )gDirectory->Get("/Tracks/p-/hTofPID");
  TObjArray* arr2 =  new TObjArray(4);
  pbar2->FitSlicesY(0, 0, -1, 0, "QNR", arr2);
  TH1D *m2pbar = (TH1D *) (*arr2)[1]; m2pbar->SetMarkerColor(2); m2pbar->SetMarkerStyle(20);
  m2pbar->SetName("m2pbar");
  m2pbar->SetStats(0);
  m2pbar->Fit("func");
  //  func = (TF1*) m2pbar->GetListOfFunctions()->FindObject("func");
  cout << Title.Data() << endl;
  Title = Form("pbar: %6.4f + %6.4f log_{10}P(%%)",100*func->GetParameter(1), 100*func->GetParameter(2));
  cout << Title.Data() << endl;
  l->AddEntry(m2pbar,Title);
  TLine *line = new TLine(-0.5, m2P, 0.5, m2P);
#endif
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (c1) c1->Clear();
  else    c1 = new TCanvas("c1","c1");
  TH1F *frame = c1->DrawFrame(-0.5,0.70,0.5,1.00);
  frame->SetTitle("p/pbar Mass^{2} versus log_{10}P");
  frame->SetXTitle("log_{10}(p[GeV/c])");
  frame->SetYTitle("M^{2}"); //[GeV/c^{2}]^{2}");
  m2p->Draw("same");
  m2pbar->Draw("sames");
  line->Draw();
  l->Draw();
}
