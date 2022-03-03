/*
  root.exe Z3GF*.root lDb.C MakeTpcZCorrectionC.C+
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
#include "tables/St_tpcCorrection_Table.h"
#include "TROOT.h"
#endif
#include "Ask.h"
#if 0
//________________________________________________________________________________
void MakeRow(TH2 *mu, Int_t row = 1, Int_t p = 2, Double_t xmin = 20, Double_t xmax = 210) {
  if (! mu) return;
  if (row < 1 || row > 45) return;
  TH1D *proj = mu->ProjectionY(Form("R%i",row),row,row);
  TF1 *f = (TF1 *) gROOT->GetListOfFunctions()->FindObject(Form("pol%i",p));
  if (! f) {
    TF1::InitStandardFunctions();
    f = (TF1 *) gROOT->GetListOfFunctions()->FindObject(Form("pol%i",p));
    if (! f) return;
  }
  Int_t iok = proj->Fit(f,"er","",xmin,xmax);
  if (iok < 0) return;
  ofstream out;
  TString fOut =  Form("row.%02i.C",row);
  TString Line;
  cout << "Create " << fOut << endl;
  out.open(fOut.Data());
  Line = Form("  memset(&row,0,tableSet->GetRowSize()); // %s",gDirectory->GetName()); cout << Line.Data() << endl;  out << Line.Data() << endl;
  Line = Form("  row.idx   = %3i;",row); cout << Line.Data() << endl;  out << Line.Data() << endl;
  Line = "  row.nrows = 45;"; cout << Line.Data() << endl;  out << Line.Data() << endl;
  Line = Form("  row.min =  %5.2f;",xmin); cout << Line.Data() << endl;  out << Line.Data() << endl;
  Line = Form("  row.max =  %5.2f;",xmax); cout << Line.Data() << endl;  out << Line.Data() << endl;
  Line = Form("  row.npar       =           1%02i;",f->GetNpar()); cout << Line.Data() << endl;  out << Line.Data() << endl;
  for (Int_t i = 0; i < f->GetNpar(); i++) {
    Line = Form("  row.a[%i]       = %13.7g;", i, f->GetParameter(i)); cout << Line.Data() << endl;  out << Line.Data() << endl;
  }
  Line = Form("  tableSet->AddAt(&row); // row %2i",row); cout << Line.Data() << endl;  out << Line.Data() << endl;  
}
#endif
//________________________________________________________________________________
void MakeTpcZCorrection1() {
  const Char_t *tableName = "TpcZCorrectionC";
  TString fileIn(gDirectory->GetName());
  if (! fileIn.BeginsWith("Z3")) return;
  TNtuple *FitP = (TNtuple *) gDirectory->Get("FitP");
  if (! FitP) return;
  fileIn.ReplaceAll("Z3PCGF","");
  fileIn.ReplaceAll("Z3PGF","");
  fileIn.ReplaceAll("Z3CGF","");
  fileIn.ReplaceAll("Z3GF","");
  fileIn.ReplaceAll(".root","");
  TString fOut =  Form("%s.%s.C", tableName, fileIn.Data());
  Double_t min =  15.0;
  Double_t max = 210.0;
  TF1* f[2] = {(TF1 *) gROOT->GetFunction("pol2"), (TF1 *) gROOT->GetFunction("pol5")};
  ofstream out;
  cout << "Create " << fOut.Data() << endl;
  out.open(fOut.Data());
#ifdef __noAuAu200_2019__
  Int_t nrows = 4; // for separate West and East
  Int_t np = 5;
  Int_t nextra = 0;
#else
  Int_t nrows = 2; 
  Int_t np = 5;
  Int_t nextra = 0; //2;
  TF1 *fc = new TF1("fc",Form("pol%i(0)+expo(%i)",np-1,np),0.,210.);
#endif
  out << "TDataSet *CreateTable() {" << endl;
  out << "  if (!gROOT->GetClass(\"St_tpcCorrection\")) return 0;" << endl;
  out << "  Int_t nrows = " << nrows << ";" << endl;
  out << "  St_tpcCorrection *tableSet = new St_tpcCorrection(\"TpcZCorrectionC\",nrows);" << endl;
  out << "  tpcCorrection_st row;" << endl; 
  const Char_t *histN[] = {"OW", "IW", "OE", "IE"};
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (! c1) c1 = new TCanvas("c1","c1");
  else      c1->Clear();
  for (Int_t idx = 1; idx <= nrows; idx++) {
    Int_t io = (idx - 1)%2;
    out << "  memset(&row,0,tableSet->GetRowSize());" << endl;
    out << "  row.idx   = " << idx << ";" << endl;
    out << "  row.nrows = nrows;" << endl;
#ifdef __noAuAu200_2019__
    if (idx % 2 == 1) { // Outer
      min = 18;
      max = 208;
    } else {            // Inner
      min =  18;
      max = 208;
    }
#else /* AuAu200_2019 */
    if (idx % 2 == 1) { // Outer
      min = 18;
      max = 220;
      if (nextra) f[io] =  fc;
    } else {            // Inner
      min =  18;
      max = 220;
      if (nextra) f[io] =  fc;
    }
#endif
    out << "  row.min = " << Form("%5.1f", min)  << ";" << endl;
    out << "  row.max = " << Form("%5.1f", max)  << ";" << endl;
    TString prof("prof");
    if (idx != 1) prof += "same";
    TString cut("i&&j");
    if (! io) cut += "&&abs(x)>40.5";
    else      cut += "&&abs(x)<40.5";
    if (nrows >= 4) {
      if (idx <= 2) cut += "&&x>0";
      else          cut += "&&x<0";
    }
    FitP->SetMarkerColor(idx+1);
    cout << "FitP->Draw(\"" << Form("mu:y>>%s(105,0,210)\"",histN[idx-1]) << ",\"" << cut << "\",\"" << prof << "\");" << endl;
    FitP->Draw(Form("mu:y>>%s(110,0,220)",histN[idx-1]),cut,prof);
    TH1 *hist = (TH1 *) gDirectory->Get(histN[idx-1]);
    if (! hist) continue;
    Double_t params[10] = {0};
    if (nextra) {
      hist->Fit("expo","er","",12,26);
      TF1 *expo = (TF1 *) hist->GetListOfFunctions()->FindObject("expo");
      params[np] = expo->GetParameter(0); 
      params[np+1] = expo->GetParameter(1); 
    }
    f[io]->SetParameters(params);
    hist->Fit(f[io],"er","",min,max);
    Int_t npar = f[io]->GetNpar();
    out << "  row.npar = " << Form("%12i",npar-nextra) << ";// " << fileIn.Data()  << endl;
    if (nextra) {
      out << "  row.type = 20;" << endl;
    }
    for (Int_t p = 0; p < npar; p++) { 
      out << "  row.a[" << p << "] = " << Form("%12.5g",f[io]->GetParameter(p)) << ";" << endl;
    }
    out << "  tableSet->AddAt(&row);" << endl;
    c1->Update();
  }
  out << "  return (TDataSet *)tableSet;" << endl;
  out << "}" << endl;
  out.close(); 
}
//________________________________________________________________________________
void MakeTpcZCorrectionC() {
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  TIter next(files);
  TFile *f = 0;
  while ( (f = (TFile *) next()) ) { 
    TString F(f->GetName());
    if (! F.Contains("Z3")) continue;
    f->cd();
    TNtuple *FitP = (TNtuple *) gDirectory->Get("FitP");
    if (! FitP) continue;
    MakeTpcZCorrection1();
    //    if (Ask()) return;
  }
}
