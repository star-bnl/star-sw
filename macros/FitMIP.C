/*/hlt/cephfs/fisyak/TpcRS_MIP4/dEdx4/Fit
  root.exe pion.root proton.root kaon.root electron.root deuteron.root lBichsel.C dEdxFit.C+ FitMIP.C

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
#include "Ask.h"
TF1 *GG();
TF1 *GausExp();
void FitMIP() {
  Int_t NF = 0;
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t nn = files->GetSize(); cout << "found " << nn << " files" << endl;
  if (! nn) return;
  TIter next(files);
  //  TF1 *g = GG();
  TF1 *g = GausExp();
  g->SetParLimits(1,-5.,5.);
  g->SetParLimits(3,0.01,5.0);
  TString Out = "FitMIP.data";
  ofstream out;
  if (gSystem->AccessPathName(Out)) out.open(Out, ios::out); //"Results.list",ios::out | ios::app);
  else                              out.open(Out, ios::app);
  out << "// particle, norml, mu, sigma, alpha" << endl; 
  TFile *f = 0;
  while ( (f = (TFile *) next()) ) { 
    f->cd();
    TString F(gSystem->BaseName(f->GetName()));
    F.ReplaceAll(".root","");
    cout << "Open " << F.Data() << endl;
    const Char_t *HistName[3] = {"SecRow3C","SecRow3PC","SecRow3+PC"};
    TH3F *SecRow[3] = {0};
    TCanvas *c = new TCanvas("c"+F,"c"+F,1800,1200);
    c->Divide(3,3);
    for (Int_t i = 0; i < 3; i++) {
      TString N(F);
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
      for (Int_t j = 0; j < 3; j++) {// Inner/Outer
	TH1D *h = 0;
        if      (j == 0) h = SecRow[i]->ProjectionZ("zI"+N,1,24,1,40);
	else if (j == 1) h = SecRow[i]->ProjectionZ("zO"+N,1,24,41,72);
	else             h = SecRow[i]->ProjectionZ("zAll"+N,1,24,1,72);
	if (h->GetEntries() < 1e3) continue;
	c->cd(3*i+j+1);
	g->SetParameters(0,h->GetMean(),1,1);
	//	h->Fit(g,"m");
	h->Fit(g,"m");
	out << Form("  /* %10s */ {%10.5f, %10.5f, %10.5f, %10.5f},", h->GetName(),g->GetParameter(0),g->GetParameter(1),g->GetParameter(2),g->GetParameter(3)) << endl;
	c->Update();
      }
    }
    if (Ask()) break;
  }
  out.close();
}
