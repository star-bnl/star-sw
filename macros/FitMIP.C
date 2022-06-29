/*/hlt/cephfs/fisyak/TpcRS_MIP4/dEdx3/Fit
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
TF1 *GG();
void FitMIP() {
  Int_t NF = 0;
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t nn = files->GetSize(); cout << "found " << nn << " files" << endl;
  if (! nn) return;
  TIter next(files);
  TF1 *g = GG();
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
    TH3F  *SecRow3N = (TH3F *) f->Get("SecRow3N");
    if (! SecRow3N) continue;
    TH1 *h = SecRow3N->Project3D("z");
    h->SetName("z"+F);
    TCanvas *c = new TCanvas(F,F);
    g->SetParameters(0,h->GetMean(),1,1);
    h->Fit(g,"m");
    h->Fit(g,"m");
    out << Form("  /* %10s */ {%10.5f, %10.5f, %10.5f, %10.5f},", F.Data(),g->GetParameter(0),g->GetParameter(1),g->GetParameter(2),g->GetParameter(3)) << endl;
  }
  out.close();
}
