/*
  root.exe TimeCGFP21ib02_24.root 
  FitP->Draw("mu:x>>T(360,762e6,771e6)","(i&&j&&dmu>0&&dmu<3e-4&&mu<0.06)/dmu**2","profg")
  .x 'MaketpcTimeDependence.C+(T)'
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
//________________________________________________________________________________
void  MaketpcTimeDependence(TH1 *T, Int_t bin1 = 1, Int_t bin2 = 50) {
  if (! T) return;
  TDatime t; t.Set(T->GetXaxis()->GetBinLowEdge(bin1)+788936400); t.Print();
  TString fOut = Form("tpcTimeDependence.%8i.%06i.C",t.GetDate(),t.GetTime());
  Double_t xmin = T->GetXaxis()->GetBinLowEdge(bin1);
  Double_t xmax = T->GetXaxis()->GetBinUpEdge(bin2); cout << "bin1 = " << bin1 << ", xmin = " << xmin << "\tbin2 = " << bin2 << ", xmax = " << xmax << endl;
  TF1 *f = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol1");
  if (! f) {
    TF1::InitStandardFunctions();
    f = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol1");
    if (! f) return;
  }
  //  Int_t iok = T->Fit(f,"er rob=0.75","",xmin,xmax);
  //  f->Draw("same");
  Int_t iok = T->Fit(f,"er","",xmin,xmax);
  if (iok < 0) return;
  ofstream out;
  TString Line;
  cout << "Create " << fOut << endl;
  out.open(fOut.Data());
  out << "TDataSet *CreateTable() {" << endl;
  out << "  if (!gROOT->GetClass(\"St_tpcCorrection\")) return 0;" << endl;
  out << "  Int_t nrows = 1;" << endl;
  out << "  St_tpcCorrection *tableSet = new St_tpcCorrection(\"tpcTimeDependenceB\",nrows);" << endl;
  out << "  tpcCorrection_st row;" << endl; 
  out << "  memset(&row,0,tableSet->GetRowSize());" << endl;
  out << "  row.idx   = 1;" << endl;
  out << "  row.nrows = nrows;" << endl;
  Line = Form("  row.min =  %f; // bin = %i",xmin,bin1); cout << Line.Data() << endl;  out << Line.Data() << endl;
  Line = Form("  row.max =  %f; // bin = %i",xmax,bin2); cout << Line.Data() << endl;  out << Line.Data() << endl;
  Line = Form("  row.npar       =             %i;",f->GetNpar()); cout << Line.Data() << endl;  out << Line.Data() << endl;
  for (Int_t i = 0; i < f->GetNpar(); i++) {
    Line = Form("  row.a[%i]       = %13.7g;", i, f->GetParameter(i)); cout << Line.Data() << endl;  out << Line.Data() << endl;
  }
  Line = Form("  tableSet->AddAt(&row);"); cout << Line.Data() << endl;  out << Line.Data() << endl;  
  out << "  // ----------------- end of code ---------------" <<endl;
  out << "  return (TDataSet *)tableSet;" << endl;
  out << "}" << endl;
}
