#ifndef __CINT__
#include "Riostream.h"
#include "TDirectory.h"
#include "TObjArray.h"
#include "TH1.h"
#include "TH2.h"
#include "TLine.h"
#include "TAxis.h"
#include "TCanvas.h"
#include "TROOT.h"
#include "TLegend.h"
#endif
void ppbarM2() {
  TH2* p2 = (TH2 *) gDirectory->Get("Tracks/p/hTofPID");
  TObjArray* arr =  new TObjArray(4);
  p2->FitSlicesY(0, 0, -1, 0, "QNR", arr);
  TH1D *m2p = (TH1D *) (*arr)[1]; m2p->SetMarkerColor(1); m2p->SetMarkerStyle(20);
  m2p->SetName("m2p");
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (c1) c1->Clear();
  else    c1 = new TCanvas("c1","c1");
  TLegend *l = new TLegend(0.2,0.2,0.4,0.4);
  TH1F *frame = c1->DrawFrame(-0.5,0.70,0.5,1.00);
  frame->SetTitle("p/pbar Mass^{2} versus log_{10}P");
  frame->SetXTitle("log_{10}(p[GeV/c])");
  frame->SetYTitle("M^{2}"); //[GeV/c^{2}]^{2}");
  m2p->Draw("same");
  l->AddEntry(m2p,"p");
#if 1
  TH2* pbar2 = (TH2* )gDirectory->Get("Tracks/p-/hTofPID");
  TObjArray* arr2 =  new TObjArray(4);
  pbar2->FitSlicesY(0, 0, -1, 0, "QNR", arr2);
  TH1D *m2pbar = (TH1D *) (*arr2)[1]; m2pbar->SetMarkerColor(2); m2pbar->SetMarkerStyle(20);
  m2pbar->SetName("m2pbar");
  m2pbar->Draw("same");
  l->AddEntry(m2pbar,"pbar");
  Double_t m2P = 0.9382723*0.9382723;
  TLine *line = new TLine(-0.5, m2P, 0.5, m2P);
  line->Draw();
  l->Draw();
#endif
}
