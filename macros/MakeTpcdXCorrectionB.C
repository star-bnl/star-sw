/*
  root.exe dX3GF*.root MakeTpcdXCorrectionB.C+
  root.exe dX3G4E*.root MakeTpcdXCorrectionB.C+
*/
#if !defined(__CINT__)
// code that should be seen ONLY by the compiler
#else
#if !defined(__CINT__) || defined(__MAKECINT__)
// code that should be seen by the compiler AND rootcint
#else
// code that should always be seen
#endif
#endif
//________________________________________________________________________________
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "THnSparse.h"
#include "TStyle.h"
#include "TF1.h"
#include "TProfile.h"
#include "TTree.h"
#include "TChain.h"
#include "TDirectory.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TFitResult.h"
#include "TCanvas.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "TDataSet.h"
#include "TClassTable.h"
#include "TMinuit.h"
#include "TSpectrum.h"
#include "StBichsel/Bichsel.h"
#include "StBichsel/StdEdxModel.h"
#include "TString.h"
#include "TLine.h"
#include "TText.h"
#include "TList.h"
#include "TPolyMarker.h"
#include "TKey.h"
#include "TLegend.h"
#include "TObjectTable.h"
#include "TObjArray.h"
//#include "tables/St_tpcCorrection_Table.h"
#include "TROOT.h"
#endif
#include "Ask.h"
TCanvas *c1 = 0;
TCanvas *c2 = 0;
TF1 *f[2] = {0};
//
Double_t satf(Double_t *x, Double_t *p) {
  return p[0]+p[1]*TMath::TanH(p[2]*(x[0]-p[3]));
}
//________________________________________________________________________________
void MakeTpcdXCorrection1() {
  const Char_t *tableName = "TpcdXCorrectionB";
  TString fileIn(gDirectory->GetName());
  if (! fileIn.BeginsWith("dX3")) return;
  if (! f[0]) {
    f[0] = new TF1("satO",satf,0,3,4);
    f[0]->SetParameters(0.1,-0.1,1.0,1.0);
    //    f[0]->SetParLimits(0,-1,1);
    f[0]->SetParLimits(2,-1,1);
    f[1] = new TF1(*f[0]);
    f[1]->SetName("satI");
  }
  TNtuple *FitP = (TNtuple *) gDirectory->Get("FitP");
  if (! FitP) return;
  fileIn.ReplaceAll("dX3C+dX3PCG4EY","");
  fileIn.ReplaceAll("dX3+dX3PG4EY","");
  fileIn.ReplaceAll("dX3CG4EY","");
  fileIn.ReplaceAll("dX3G4EY","");

  fileIn.ReplaceAll(".root","");
  TString fOut =  Form("%s.%s.C", tableName, fileIn.Data());
  Int_t nrows = 4; // for separate West and East
  if      (fileIn == "")                                  {nrows = 0;}
  else if (fileIn.Contains("col"))                        {nrows = 2;}
  else if (fileIn.Contains("OO"))                         {nrows = 2;}
  else if (fileIn.Contains("dAu"))                        {nrows = 2;}
  else if (fileIn.Contains("AuAu"))                       {nrows = 2;}
  else if (fileIn.Contains("pp500"))                      {nrows = 1;}
  else if (fileIn.Contains("GeV_"))                       {nrows = 2;}
  if (! nrows) {
    cout << "Reject " << fileIn.Data() << endl;
    return;
  }
  ofstream out;
  cout << "Create " << fOut.Data() << endl;
  out.open(fOut.Data());
  out << "TDataSet *CreateTable() {" << endl;
  out << "  if (!gROOT->GetClass(\"St_tpcCorrection\")) return 0;" << endl;
  out << "  Int_t nrows = " << nrows << ";" << endl;
  out << "  St_tpcCorrection *tableSet = new St_tpcCorrection(\"TpcdXCorrectionB\",nrows);" << endl;
  out << "  tpcCorrection_st row;" << endl; 
  const Char_t *histN[] = {"OW", "IW", "OE", "IE"};
  TH1 *hists[4] = {0};
  Double_t ymin =  9999;
  Double_t ymax = -9999;
  for (Int_t idx = 1; idx <= nrows; idx++) {
    Int_t io = (idx - 1)%2;
    out << "  memset(&row,0,tableSet->GetRowSize());" << endl;
    out << "  row.idx   = " << idx << ";" << endl;
    out << "  row.nrows = nrows;" << endl;
    out << "  row.type = 30;" << endl;
    TString prof("prof");
    TString same("");
    if (idx != 1) same = "same";
    TString cut("i&&j&&dmu>0&&dmu<0.01&&y<2.9");
    if (nrows > 1) {
      if (! io) cut += "&&abs(x)>40.5";
      else      cut += "&&abs(x)<40.5";
    } else {
      cut = "i&&j&&dmu>0&&dmu<0.01&&y<2.0"; // pp500 case
    }
    if (nrows >= 4) {
      if (idx <= 2) cut += "&&x>0";
      else          cut += "&&x<0";
    }
    FitP->SetMarkerColor(idx+1);
    if (! c2 ) c2 =  new TCanvas("cTemp","cTemp");
    c2->cd(); 
    c2->Clear();
#define __muJ__
#ifdef __muJ__
    cout << "FitP->Draw(\"" << Form("mu-muJ:y>>%s(24,0.3,2.7)\"",histN[idx-1]) << ",\"" << cut << "\",\"" << prof << "\");" << endl;
    FitP->Draw(Form("mu-muJ:y>>%s(24,0.3,2.7)",histN[idx-1]),cut,prof);
#else
    cout << "FitP->Draw(\"" << Form("mu:y>>%s(24,0.3,2.7)\"",histN[idx-1]) << ",\"" << cut << "\",\"" << prof << "\");" << endl;
    FitP->Draw(Form("mu:y>>%s(24,0.3,2.7)",histN[idx-1]),cut,prof);
#endif
    c2->Update();
    hists[idx-1] = (TH1 *) gDirectory->Get(histN[idx-1]);
    if (! hists[idx-1]) continue;
    if (hists[idx-1]->GetMaximum() > ymax) ymax = hists[idx-1]->GetMaximum();
    if (hists[idx-1]->GetMinimum() < ymin) ymin = hists[idx-1]->GetMinimum();
    c1->cd();
    hists[idx-1]->Draw(same);
    f[io]->SetLineColor(idx+1);
    hists[idx-1]->Fit(f[io],"m");
    Double_t prob = f[io]->GetProb();
    cout << "Fit " << hists[idx-1]->GetName() << " with " << f[io]->GetName() << " prob = " << prob << endl;
    //    if (f[io]->GetProb() > 1e-3) break;
    Int_t npar = f[io]->GetNpar();
    out << "  row.npar = " << Form("%12i",npar) << ";// " << fileIn.Data()  << endl;
    for (Int_t p = 0; p < npar; p++) { 
      out << "  row.a[" << p << "] = " << Form("%12.5g",f[io]->GetParameter(p)) << ";" << endl;
    }
    out << "  tableSet->AddAt(&row);" << endl;
    if (hists[0] && ymax > ymin) {hists[0]->SetMinimum(ymin); hists[0]->SetMaximum(ymax);}
    c1->Update();
    cout << "________________________________________________________________________________" << endl;
  }
  out << "  return (TDataSet *)tableSet;" << endl;
  out << "}" << endl;
  out.close(); 
  cout << "=================================<" << fOut.Data() << "===============================================" << endl;
}
//________________________________________________________________________________
void MakeTpcdXCorrectionB() {
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  TF1::InitStandardFunctions();
  TIter next(files);
  TFile *f = 0;
  Int_t n = 0;
  while ( (f = (TFile *) next()) ) { 
    TString F(f->GetName());
    if (! F.Contains("dX3")) continue;
    F.ReplaceAll(".root","");
    f->cd();
    TNtuple *FitP = (TNtuple *) gDirectory->Get("FitP");
    if (! FitP) continue;
    n++;
    TString cn = Form("c%i",n,1200,1200);
    c1 = new TCanvas(cn,F);
    MakeTpcdXCorrection1();
    if (Ask()) return;
  }
}
