/*
root.exe Z3CGFRunXVIAuAu200p14.root lDb.C
  .L MakeTpcZCorrection.C++
  Double_t xmin = 25;
  Double_t xmax = 210;
  mu->ProjectionY("bin1",1,1,"e")->Draw(); 
  bin1->Fit("pol2","re","",xmin,xmax); 
  TF1* Pol = pol2;
  Pol->SetRange(xmin,xmax); 
  TH2 *Mu = mu;
  MakeRow(Mu,1,2,0,1);
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
//#include "DeDxTree.C"
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
#if 0
//________________________________________________________________________________
void MakeTpcZCorrection1(TH2 *hist = 0, TF1 *func = 0, const Char_t *TableName = "TpcZCorrection", Int_t d=20160207,Int_t t=14 ){
  if (! hist) return;
  if (! func) return;
  TObjArray *arr = new TObjArray();
  hist->FitSlicesY(func,-1,-1,10,"re", arr);
  Int_t Npar = func->GetNpar();
  TH1D **fitPar = new TH1D*[Npar];
  Double_t xmin = func->GetXmin();
  Double_t xmax = func->GetXmax();
  Int_t p = 0;
  for (p = 0; p < Npar; p++) {
    fitPar[p] = (TH1D*) arr->At(p);
    if (! fitPar[p]) return;
    cout << "Histogram " << fitPar[p]->GetName() << " has been found" << endl;
  }
  if (gClassTable->GetID("TTable") < 0) gSystem->Load("libTable");
  if (gClassTable->GetID("St_tpcCorrection") < 0) gSystem->Load("libStDb_Tables");
  St_tpcCorrection *zCor = new St_tpcCorrection(TableName,45);
  Int_t NbinsX = hist->GetNbinsX(); cout << "NbinsX = " << NbinsX << endl;
  Int_t NbinsY = hist->GetNbinsY(); cout << "NbinsY = " << NbinsY << endl;
  tpcCorrection_st row;
  for (Int_t i=1; i<=NbinsX; i++) {
    memset(&row,0,zCor->GetRowSize());
    row.idx   = i;
    row.nrows = 45;
    row.min   = xmin;
    row.max   = xmax;
    row.npar  = Npar;
    for (p = 0; p < Npar; p++) {
      row.a[p] = fitPar[p]->GetBinContent(i);
    }
    zCor->AddAt(&row);
  }
  zCor->Print(0,NbinsX);
  //  TDatime  time(20010701,120000);
  TDatime  time(d,t);
  TString filename(Form("%s.%08d.%06d",TableName,time.GetDate(),time.GetTime()));
  //  sprintf(filename,"./StarDb/Calibrations/tpc/TpcZCorTest.%08d.%06d.C",time.GetDate(),time.GetTime());
  //  sprintf(filename,"TpcZCor.%08d.%06d.root",time.GetDate(),time.GetTime());
  printf("Create %s\n",filename.Data());
#if 1
  filename += ".C";
  TString dirname = gSystem->DirName(filename);
  if (gSystem->OpenDirectory(dirname.Data())==0) { 
    if (gSystem->mkdir(dirname.Data())) {
      cout << "Directory " << dirname << " creation failed" << endl;
      cout << "Putting " << TableName << ".C in current directory" << endl;
    }
  }
  ofstream *out = new ofstream(filename.Data());
  zCor->SavePrimitive(*out,"");
  delete out;
#else
  filename += ".root";
  TFile *f = new TFile(filename.Data(),"recreate");
  zCor->Write();
  //  delete f;
#endif
  //    break;
}
#endif
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
//________________________________________________________________________________
void MakeRows() {
  TH2D *mu = (TH2D *) gDirectory->Get("mu");
  if (! mu) return;
  struct P_t {
    Int_t row;
    Int_t pow;
    Double_t xmin;
    Double_t xmax;
  };
  P_t P[45] = {
    {  1, 5,  95, 210},
    {  2, 5,  90, 210},
    {  3, 5,  77, 210},
    {  4, 5,  65, 210},
    {  5, 6,  45, 210},
    {  6, 5,  20, 210},
    {  7, 4,  20, 210},
    {  8, 4,  20, 210},
    {  9, 4,  20, 210},
    { 10, 6,  20, 210},
    { 11, 6,  20, 210},
    { 12, 6,  20, 210},
    { 13, 6,  20, 210},
    { 14, 2,  20, 210},
    { 15, 2,  20, 210},
    { 16, 2,  20, 210},
    { 17, 2,  20, 210},
    { 18, 2,  20, 210},
    { 19, 2,  20, 210},
    { 20, 2,  20, 210},
    { 21, 2,  20, 210},
    { 22, 2,  20, 210},
    { 23, 2,  20, 210},
    { 24, 2,  20, 210},
    { 25, 2,  20, 210},
    { 26, 2,  20, 210},
    { 27, 2,  20, 210},
    { 28, 2,  20, 210},
    { 29, 2,  20, 210},
    { 30, 2,  20, 210},
    { 31, 2,  20, 210},
    { 32, 2,  20, 210},
    { 33, 2,  20, 210},
    { 34, 2,  20, 210},
    { 35, 2,  20, 210},
    { 36, 2,  20, 210},
    { 37, 2,  20, 210},
    { 38, 2,  20, 210},
    { 39, 2,  20, 210},
    { 40, 2,  20, 210},
    { 41, 2,  20, 210},
    { 42, 2,  20, 210},
    { 43, 2,  20, 210},
    { 44, 2,  20, 210},
    { 45, 2,  20, 210}
  };
  for (Int_t r = 0; r <= 45; r++) {
    MakeRow(mu, r, P[r-1].pow, P[r-1].xmin, P[r-1].xmax);
  }
}
//________________________________________________________________________________
void MakeTpcZCorrection() {
  const Char_t *tableName = "TpcZCorrectionB";
  TString fileIn(gDirectory->GetName());
  if (! fileIn.BeginsWith("Z3CGF")) return;
  TNtuple *FitP = (TNtuple *) gDirectory->Get("FitP");
  if (! FitP) return;
  fileIn.ReplaceAll("Z3CGF","");
  fileIn.ReplaceAll(".root","");
  TString fOut =  Form("%s.%s.C", tableName, fileIn.Data());
  Double_t min =  22.0;
  Double_t max = 210.0;
  TF1* f[2] = {(TF1 *) gROOT->GetFunction("pol2"), (TF1 *) gROOT->GetFunction("pol5")};
  ofstream out;
  cout << "Create " << fOut.Data() << endl;
  out.open(fOut.Data());
  out << "TDataSet *CreateTable() {" << endl;
  out << "  if (!gROOT->GetClass(\"St_tpcCorrection\")) return 0;" << endl;
  out << "  Int_t nrows = 4;" << endl;
  out << "  St_tpcCorrection *tableSet = new St_tpcCorrection(\"TpcZCorrectionB\",nrows);" << endl;
  out << "  tpcCorrection_st row;" << endl; 
  const Char_t *histN[] = {"OW", "IW", "OE", "IE"};
  for (Int_t idx = 1; idx <= 4; idx++) {
    Int_t io = (idx - 1)%2;
    out << "  memset(&row,0,tableSet->GetRowSize());" << endl;
    out << "  row.idx   = " << idx << ";" << endl;
    out << "  row.nrows = nrows;" << endl;
    if (idx % 2 == 1) { // Outer
      min = 22;
      max = 210;
    } else {            // Inner
      min =  20;
      max = 208;
    }
    out << "  row.min = " << Form("%5.1f", min)  << ";" << endl;
    out << "  row.max = " << Form("%5.1f", max)  << ";" << endl;
    TString prof("prof");
    //    if (idx != 1) prof += "same";
    TString cut("i&&j");
    if (! io) cut += "&&abs(x)>40.5";
    else      cut += "&&abs(x)<40.5";
    if (idx <= 2) cut += "&&x>0";
    else          cut += "&&x<0";
    FitP->SetMarkerColor(idx+1);
    cout << "FitP->Draw(\"" << Form("mu:y>>%s(105,0,210)\"",histN[idx-1]) << ",\"" << cut << "\",\"" << prof << "\");" << endl;
    FitP->Draw(Form("mu:y>>%s(105,0,210)",histN[idx-1]),cut,prof);
    TH1 *hist = (TH1 *) gDirectory->Get(histN[idx-1]);
    hist->Fit(f[io],"er","",min,max);
    Int_t npar = f[io]->GetNpar();
    out << "  row.npar = " << Form("%12i",npar) << ";//  FitP->SetMarkerColor(" << idx+1 << "); FitP->Draw(\"mu:y>> " << histN[idx-1] << "(105,0,210)\",\"" << cut.Data() << "\",\"" << prof.Data() << "\');" << endl;
    for (Int_t p = 0; p < npar; p++) { 
      out << "  row.a[" << p << "] = " << Form("%12.5g",f[io]->GetParameter(p)) << ";" << endl;
    }
    out << "  tableSet->AddAt(&row);" << endl;
  }
  out << "  return (TDataSet *)tableSet;" << endl;
  out << "}" << endl;
  out.close(); 
}
