/*
  root.exe Eta3GF*.root MakeTpcEtaCorrection.C+
  root.exe Eta3G4E*.root MakeTpcEtaCorrection.C+
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
//________________________________________________________________________________
void MakeTpcEtaCorrection1() {
  const Char_t *tableName = "TpcEtaCorrection";
  TString fileIn(gDirectory->GetName());
  if (! fileIn.BeginsWith("Eta3")) return;
  TNtuple *FitP = (TNtuple *) gDirectory->Get("FitP");
  if (! FitP) return;
  fileIn.ReplaceAll("Eta3C+Eta3PCG4EY","");
  fileIn.ReplaceAll("Eta3+Eta3PG4EY","");
  fileIn.ReplaceAll("Eta3CG4EY","");
  fileIn.ReplaceAll("Eta3G4EY","");

  fileIn.ReplaceAll(".root","");
  TString fOut =  Form("%s.%s.C", tableName, fileIn.Data());
  TF1* f[2] = {(TF1 *) gROOT->GetFunction("pol2"), (TF1 *) gROOT->GetFunction("pol5")};
  Int_t nrows = 4; // for separate West and East
  Int_t np = 5;
  Int_t npO = -1;
//   Double_t min      =  -2.5;
//   Double_t max      =   2.5;
  Double_t min      =   0.0;
  Double_t max      =  6.25;
  Double_t minOuter =  min;
  Double_t maxOuter =  max;
  Double_t minInner =  min;
  Double_t maxInner =  max;
#if 1
  if      (fileIn == "")                                  {nrows = 0;}
  else if (fileIn.Contains("GeV_20"))                     {nrows = 2;}           //ok 
  else if (fileIn.Contains("GeVb_20"))                    {nrows = 2;}           //ok 
  else if (fileIn.Contains("GeVc_20"))                    {nrows = 2;}           //ok 
  //  else if (fileIn.Contains("100GeV_fixedTarget_2021"))    {nrows = 0;} // np = 3; npO = 3;} 
  //  else if (fileIn.Contains("11p5GeV_2020"))               {nrows = 2;}           //ok 
  //  else if (fileIn.Contains("13p5GeV_fixedTarget_2020"))   {}  
  //  else if (fileIn.Contains("14p5GeV_2019"))               {nrows = 2;}
  //  else if (fileIn.Contains("17p3GeV_2021")) 		  {}
  //  else if (fileIn.Contains("19GeV_2019"))   		  {}
  //  else if (fileIn.Contains("19p5GeV_fixedTarget_2020"))   {}
  //  else if (fileIn.Contains("26p5GeV_fixedTarget_2020"))   {}
  //  else if (fileIn.Contains("26p5GeV_fixedTarget_2021"))   {nrows = 0;} 
  //  else if (fileIn.Contains("31GeV_fixedTarget_2019"))     {nrows = 0;}
  //  else if (fileIn.Contains("31p2GeV_fixedTarget_2020"))   {} 
  //  else if (fileIn.Contains("3p85GeV_fixedTarget_2019"))   {nrows = 0;} //
  //  else if (fileIn.Contains("3p85GeV_fixedTarget_2021"))   {} // ?
  //  else if (fileIn.Contains("44p5GeV_fixedTarget_2021"))   {nrows = 0;} // 
  //  else if (fileIn.Contains("4p59GeV_fixedTarget_2019"))   {}
  //  else if (fileIn.Contains("5p75GeV_fixedTarget_2020"))   {}
  //  else if (fileIn.Contains("70GeV_fixedTarget_2021"))     {nrows = 0;}              
  //  else if (fileIn.Contains("7.3GeV_fixedTarget_2019"))    {} 
  //  else if (fileIn.Contains("7p3GeV_fixedTarget_2020"))    {} 
  //  else if (fileIn.Contains("7p7GeV_2019"))                {} 
  //  else if (fileIn.Contains("7p7GeV_2020"))  		  {}
  //  else if (fileIn.Contains("7p7GeV_2021"))  		  {}
  //  else if (fileIn.Contains("9p2GeV_2019"))  		  {}
  //  else if (fileIn.Contains("9p2GeV_2020"))  		  {}
  //  else if (fileIn.Contains("9p2GeVb_2020")) 		  {}
  //  else if (fileIn.Contains("9p2GeVc_2020")) 		  {}
  //  else if (fileIn.Contains("9p8GeV_fixedTarget_2020"))    {} 
  //  else if (fileIn.Contains("AuAu200GeV_2019"))            {}
  //  else if (fileIn.Contains("colGeV_2019"))                {}
  //  else if (fileIn.Contains("colGeV_2020")) 		  {}
  //  else if (fileIn.Contains("colGeV_2021")) 		  {}
  //  else if (fileIn.Contains("dAu200GeV_2021"))             {} // ?
  //  else if (fileIn.Contains("FF_OO_200GeV_2021"))          {}  // not in the game
  //  else if (fileIn.Contains("fixedTarget_2019"))                   {} 
  //  else if (fileIn.Contains("fixedTarget_2020")) 		  {} 
  //  else if (fileIn.Contains("fixedTarget_2021")) 		  {} 
  //  else if (fileIn.Contains("OO_200GeV_2021"))             {nrows = 0;}
  //  else if (fileIn.Contains("pp500GeV_2022"))              {}
  //  else if (fileIn.Contains("ps_OO_200GeV_2021"))          {} 
#endif
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
  out << "  St_tpcCorrection *tableSet = new St_tpcCorrection(\"TpcEtaCorrection\",nrows);" << endl;
  out << "  tpcCorrection_st row;" << endl; 
  const Char_t *histN[] = {"OW", "IW", "OE", "IE"};
  TH1 *hists[4] = {0};
  Double_t ymin =  9999;
  Double_t ymax = -9999;
  TLegend *l = new TLegend(0.7,0.6,0.9,0.8);
  for (Int_t idx = 1; idx <= nrows; idx++) {
    Int_t io = (idx - 1)%2;
    out << "  memset(&row,0,tableSet->GetRowSize());" << endl;
    out << "  row.idx   = " << idx << ";" << endl;
    out << "  row.nrows = nrows;" << endl;
    Int_t Np = np;
    if (idx % 2 == 1) { // Outer
      min = minOuter;
      max = maxOuter;
      if (npO > 0) Np = npO;
    } else {            // Inner
      min = minInner;
      max = maxInner;
    }
    TString prof("prof");
    TString same("");
    if (idx != 1) same = "same";
    TString cut("i&&j&&dmu>0&&dmu<0.1&&abs(mu)<0.4");
    if (! io) cut += "&&abs(x)>40.5";
    else      cut += "&&abs(x)<40.5";
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
    //    cout << "FitP->Draw(\"" << Form("mu-muJ:y>>%s(50,-2.5,2.5)\"",histN[idx-1]) << ",\"" << cut << "\",\"" << prof << "\");" << endl;
    //    FitP->Draw(Form("mu-muJ:y>>%s(50,-2.5,2.5)",histN[idx-1]),cut,prof);
    cout << "FitP->Draw(\"" << Form("mu-muJ:y*y>>%s(50,0,6.25)\"",histN[idx-1]) << ",\"" << cut << "\",\"" << prof << "\");" << endl;
    FitP->Draw(Form("mu-muJ:y*y>>%s(50,0.0,6.25)",histN[idx-1]),cut,prof);
#else
    cout << "FitP->Draw(\"" << Form("mu:y>>%s(50,-2.5,2.5)\"",histN[idx-1]) << ",\"" << cut << "\",\"" << prof << "\");" << endl;
    //    FitP->Draw(Form("mu:TMath::Sign(y,x)>>%s(50,-2.5,2.5)",histN[idx-1]),cut,prof);
    FitP->Draw(Form("mu:y*y>>%s(50,0.0,6.25)",histN[idx-1]),cut,prof);
#endif
    c2->Update();
    hists[idx-1] = (TH1 *) gDirectory->Get(histN[idx-1]);
    if (! hists[idx-1]) continue;
    l->AddEntry(hists[idx-1],hists[idx-1]->GetName());
    Int_t nxbin = hists[idx-1]->GetXaxis()->GetNbins();
    Int_t k1 = nxbin;
    Int_t k2 = 1;
    for (Int_t ib = 1; ib <= nxbin; ib++) {
      if (hists[idx-1]->GetBinError(ib) > 0) {
	k1 = ib;
        break;
      }   
    }
    for (Int_t ib = nxbin; ib > 0; ib--) {
      if (hists[idx-1]->GetBinError(ib) > 0) {
	k2 = ib;
        break;
      }   
    }
    Double_t minX = min;
    Double_t maxX = max;
    if (k1 < k2) {
      minX = hists[idx-1]->GetXaxis()->GetBinLowEdge(k1);
      maxX = hists[idx-1]->GetXaxis()->GetBinUpEdge(k2);
    }
    if (hists[idx-1]->GetMaximum() > ymax) ymax = hists[idx-1]->GetMaximum();
    if (hists[idx-1]->GetMinimum() < ymin) ymin = hists[idx-1]->GetMinimum();
    c1->cd();
    hists[idx-1]->Draw(same);
    Int_t iok = 0;
    for (Int_t p = 1; p <= Np; p++) {
      f[io] = (TF1 *) gROOT->GetFunction(Form("pol%i",p-1));
      f[io]->SetLineColor(idx+1);
      iok = hists[idx-1]->Fit(f[io],"er","",minX,maxX);
      if (iok) {cout << "Fit Failed with iok = " << iok << endl; f[io] = 0; continue;}
      Double_t prob = f[io]->GetProb();
      cout << "Fit " << hists[idx-1]->GetName() << " with " << f[io]->GetName() << " prob = " << prob << endl;
      if (f[io]->GetProb() > 1e-3) break;
    }
    Int_t npar = 0;
    if (f[io]) npar = f[io]->GetNpar();
    out << "  row.min = " << Form("%5.1f", minX)  << ";" << endl;
    out << "  row.max = " << Form("%5.1f", maxX)  << ";" << endl;
    out << "  row.npar = " << Form("%12i",npar) << ";// " << fileIn.Data()  << endl;
    for (Int_t p = 0; p < npar; p++) { 
      out << "  row.a[" << p << "] = " << Form("%12.5g",f[io]->GetParameter(p)) << ";" << endl;
    }
    out << "  tableSet->AddAt(&row);" << endl;
    if (hists[0] && ymax > ymin) {hists[0]->SetMinimum(ymin); hists[0]->SetMaximum(ymax);}
    l->Draw();
    c1->Update();
    cout << "________________________________________________________________________________" << endl;
  }
  out << "  return (TDataSet *)tableSet;" << endl;
  out << "}" << endl;
  out.close(); 
  cout << "=================================<" << fOut.Data() << "===============================================" << endl;
}
//________________________________________________________________________________
void MakeTpcEtaCorrection() {
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  TF1::InitStandardFunctions();
  TIter next(files);
  TFile *f = 0;
  Int_t n = 0;
  while ( (f = (TFile *) next()) ) { 
    TString F(f->GetName());
    if (! F.Contains("Eta3")) continue;
    F.ReplaceAll(".root","");
    f->cd();
    TNtuple *FitP = (TNtuple *) gDirectory->Get("FitP");
    if (! FitP) continue;
    n++;
    TString cn = Form("c%i",n,1200,1200);
    c1 = new TCanvas(cn,F);
    MakeTpcEtaCorrection1();
    if (Ask()) return;
  }
}
