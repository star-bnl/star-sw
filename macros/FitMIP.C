/* /hlt/cephfs/fisyak/TpcRS_MIP4/dEdx4/Fit
   root.exe pion.root proton.root kaon.root electron.root deuteron.root lBichsel.C dEdxFit.C+ FitMIP.C+
   root.exe  pion.root proton.root kaon.root electron.root deuteron.root muon.root triton.root He3.root alpha.root HE6.root Li5.root Li6.root Li7.root lBichsel.C dEdxFit.C+ FitMIP.C+
   root.exe  pion.root proton.root kaon.root electron.root deuteron.root muon.root triton.root He3.root alpha.root lBichsel.C dEdxFit.C+ FitMIP.C+
   root.exe  pion.root proton.root kaon.root electron.root deuteron.root muon.root triton.root He3.root alpha.root HE6.root Li5.root Li6.root Li7.root Be10.root B11.root  lBichsel.C dEdxFit.C+ FitMIP.C+
   root.exe pion.root proton.root kaon.root electron.root deuteron.root muon.root lBichsel.C dEdxFit.C+ FitMIP.C+
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
#include "StBichsel/StdEdxModel.h"
#include "Ask.h"
#include "TMath.h"
#define __FITGG__
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
#ifdef __FITGG__
  //  TF1 *g = GG();
  TF1 *g = StdEdxModel::instance()->GausExp();
#endif
  return h3;
}
//________________________________________________________________________________
TF1 *FitD(TH1D *h, Int_t p1, Int_t p2, Int_t i, Int_t j, TString &Line ) {
  TF1 *g = 0;
#ifdef __FITGG__
  Line = Form("\t{%2i,%2i,%2i,%2i,%-32s, %10.5f, %10.5f, %10.5f, %10.5f}", 
	      p1, p2, i, j, "\"\"", 0, 0, 0, 0);
#else
  Line = Form("\t{%2i,%2i,%2i,%2i,%-32s, %10.5f, %10.5f, %10.5f, %10.5f, %10.5f}", 
	      p1, p2, i, j, "\"\"", 0, 0, 0, 0, 0);
#endif
  if (! h) return g;
  TString histName(h->GetName());
  histName.Prepend("\"");
  histName.Append("\"");
  if (h->GetEntries() < 1e3) return g;
  Int_t kx1 = h->FindFirstBinAbove(1e-5);
  Int_t kx2 = h->FindLastBinAbove(1e-5);
  h->GetXaxis()->SetRange(kx1,kx2);
  if (h->Integral() < 1e3) return g;
#ifdef __FITGG__
  g = StdEdxModel::instance()->GausExp();
  g->SetParLimits(1,-5.,5.);
  g->SetParLimits(2,0.1,0.5);
  g->SetParLimits(3,0.5,1.3);
  g->SetParameters(TMath::Log(h->Integral()),h->GetMean(),h->GetRMS(),0.8);
  h->Fit(g,"r");
  h->Fit(g,"mr");
  Line = Form("\t{%2i,%2i,%2i,%2i,%-32s, %10.5f, %10.5f, %10.5f, %10.5f}", 
	      p1,p2,i,j,histName.Data(),g->GetParameter(0),g->GetParameter(1),g->GetParameter(2),g->GetParameter(3));
#else
  g = StdEdxModel::instance()->ExValG();
  g->SetParameter(1,h->GetMean());
  g->SetParameter(2,1./h->GetRMS());
  g->FixParameter(3, TMath::PiOver2());
  h->Fit(g,"mr");
  g->ReleaseParameter(3);
  g->SetParameter(3, TMath::PiOver4());
  h->Fit(g,"r");
  h->Fit(g,"mr");
  Line = Form("\t{%2i,%2i,%2i,%2i,%-32s, %10.5f, %10.5f, %10.5f, %10.5f, %10.5f}", 
	      p1,p2,i,j,histName.Data(),g->GetParameter(0),g->GetParameter(1),g->GetParameter(2),g->GetParameter(3),g->GetParameter(4));
  
#endif
  return g;
}
//________________________________________________________________________________
void FitMIP() {
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t NF = files->GetSize(); cout << "found " << NF << " files" << endl;
  if (! NF) return;
  TIter next(files);
  TString Out = "FitMIP.data";
  ofstream out;
  if (gSystem->AccessPathName(Out)) out.open(Out, ios::out); //"Results.list",ios::out | ios::app);
  else                              out.open(Out, ios::app);
  
  TFile *f = 0;
  Int_t np = 0;
  Int_t NI = 3; // -,+,All
  Int_t NJ = 2; // I,O,All
#ifdef __FITGG__
  out << "  static MIPFitParX_t parMIPs[" << NF + NF*(NF+1)/2 << "][" << NI << "][" << NJ << "] = {" << endl;
#else
  out << "  static MIPFitParG_t parMIPs[" << NF + NF*(NF+1)/2 << "][" << NI << "][" << NJ << "] = {" << endl;
#endif
  //               i  j
  TH1D *hists[120][3][2] = {0};
  TString F[NF];
  TCanvas *c = new TCanvas("c1","c1",600*NI,400*NJ);
  c->Divide(NI,NJ);
  TString Line;
  while ( (f = (TFile *) next()) ) { 
    f->cd();
    F[np] = gSystem->BaseName(f->GetName());
    F[np].ReplaceAll(".root","");
    cout << "Open " << F[np].Data() << endl;
    const Char_t *HistName[3] = {"SecRow3C","SecRow3PC","SecRow3+PC"};
    c->SetTitle(F[np]);
    TH3F *SecRow[3] = {0};
#ifdef __FITGG__
  out << "    {{ // particle, norml, mu, sigma, alpha" << endl; 
#else
  out << "    {{ // particle, norml, mu, sigmaI, phase, sigmaG" << endl; 
#endif
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
      for (Int_t j = 0; j < NJ; j++) {// Inner/Outer
	if (SecRow[i]) {
	  if      (j == 0) hists[np][i][j] = SecRow[i]->ProjectionZ("zI"+N,1,24,1,40); 
	  else if (j == 1) hists[np][i][j] = SecRow[i]->ProjectionZ("zO"+N,1,24,41,72);
	  //	  else             hists[np][i][j] = SecRow[i]->ProjectionZ("zAll"+N,1,24,1,72);
	  c->cd(NI*j+i+1);
	  FitD(hists[np][i][j], np, -1, i, j, Line);
	  if (i == NI - 1 && j == NJ - 1) Line +=  " }},";
	  else           if (j == NJ - 1) Line +=  " },{";
	  else                            Line +=  ",";
	  cout << Line << endl;
	  out << Line << endl;
	  c->Update();
	}
      }
    }
    //    out << "    }}," << endl;
    c->SaveAs(F[np] + ".png");
    if (Ask()) goto BREAK;
    np++;
    c->Clear();
    c->Divide(NI,NJ);
  }
#if 1
  for (Int_t p1 = 0; p1 < np; p1++) {
    for (Int_t p2 = 0; p2 <= p1; p2++) {
#ifdef __FITGG__
      out << "    {{ // particle, norml, mu, sigma, alpha" << endl; 
#else
      out << "    {{ // particle, norml, mu, sigmaI, phase, sigmaG" << endl; 
#endif
      c->SetTitle(F[p1] + F[p2]);
      for (Int_t i = 0; i < NI; i++) {
	for (Int_t j = 0; j < NJ; j++) {
	  c->cd(NI*j+i+1);
	  TH1D *h3 = 0;
	  if (hists[p1][i][j] && hists[p2][i][j]) {
	    h3 = AddZ(hists[p1][i][j],hists[p2][i][j]);
	  }
	  TF1 *g = FitD(h3, p1, p2, i, j, Line); 
	  if (i == NI - 1 && j == NJ - 1) Line +=  " }},";
	  else           if (j == NJ - 1) Line +=  " },{";
	  else                            Line +=  ",";
	  cout << Line << endl;
	  out << Line << endl;
	  c->Update();
	}
      }
      //      out << "    }}," << endl;
      c->SaveAs(F[p1] + F[p2] + ".png");
      if (Ask()) goto BREAK;
      c->Clear();
      c->Divide(NI,NJ);
    } 
  }
#endif  
 BREAK:
  out << "  };" << endl;
  out.close();
}
//________________________________________________________________________________
void SecRow() {
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t NF = files->GetSize(); cout << "found " << NF << " files" << endl;
  TFile *f = 0;
  TString F;
  TIter next(files);
  while ( (f = (TFile *) next()) ) { 
    f->cd();
    F = gSystem->BaseName(f->GetName());
    F.ReplaceAll(".root","");
    //    cout << "Open " << F.Data() << endl;
    const Char_t *HistName[4] = {"SecRow3C","SecRow3PC","SecRow3","Secrow3P"};
    cout << "file " << F.Data();
    for (Int_t i = 0; i < 4; i++) {
      TH1 *h = (TH1 *) gDirectory->Get(HistName[i]);
      if (h) {
	cout << "\t" << HistName[i] << " = " << h->GetEntries();
      }
    }
    cout << endl;
  }
}
