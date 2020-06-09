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
#include "TStyle.h"
#include "TF1.h"
#include "TProfile.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TCanvas.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "TDataSet.h"
#include "TClassTable.h"
#include "TString.h"
#include "TRegexp.h"
#include "TLine.h"
#include "TText.h"
#include "TList.h"
#include "TPolyMarker.h"
#include "TKey.h"
#include "TLegend.h"
#include "TStyle.h"
#endif
#ifdef __RUNXIX__
static  Int_t N = 8;
static  const Char_t *Names[8] = {
  "AuAu200_production_FullFieldLL",
  "AuAu200_production_FullField",
  "LowLuminosity_2010_ReversedFullField",
  "AuAu200_production_ReversedFullField",
  "AuAu62_production_ReversedFullField",
  "AuAu39_production_ReversedFullField",
  "AuAu7_production_ReversedFullField",
  "AuAu11_production_ReversedFullField"
};
#else /* RunXX */
static Int_t N = 10;
static const Char_t *Names[10] = {
  "RunXX",
  "11p5GeV",
  "9p2GeV",
  "9p2GeVb",
  "5p75GeV_fixedTarget",
  "31p2GeV_fixedTarget",
  "9p8GeV_fixedTarget",
  "19p5GeV_fixedTarget",
  "13p5GeV_fixedTarget",
  "7p3GeV_fixedTarget"
};
/*                                                       DB                                                              calc     laser
11p5GeV              20191208.091308  20191208.091308:   9.0833e+06 : 11p5GeV              9.3374e+06	            ?  : 9.292252 run = 20346051, Freq = 9.3374e+06 11p5GeV	       
5p75GeV_fixedTarget  20191221.154021  20191221.154021:   9.3374e+06 : 5p75GeV_fixedTarget  9.3374e+06	            ok		   
11p5GeV              20191221.190032  20191221.190032:   9.3374e+06 : 11p5GeV              9.3374e+06	            ok		   
31p2GeV_fixedTarget  20200128.182912  20200128.182912:   9.4000e+06 : 31p2GeV_fixedTarget  9.3794e+06	            ?		  run = 21028011, Freq = 9.3794e+06 31p2GeV_fixedTarget 
9p8GeV_fixedTarget   20200130.005840  20200130.005840:   9.3612e+06 : 9p8GeV_fixedTarget   9.3411e+06	            ?		  run = 21029052, Freq = 9.3411e+06 9p8GeV_fixedTarget   
9p2GeV               20200131.012112  20200131.012112:   9.1887e+06 : 9p2GeV               9.1887e+06  	            ok		  run = 21030030, Freq = 9.1887e+06 9p2GeV	       	
9p8GeV_fixedTarget   20200131.050328  20200131.050328:   9.1887e+06 : 9p8GeV_fixedTarget   9.3411e+06	            ?		  run = 21031007, Freq = 9.3411e+06 9p8GeV_fixedTarget   
19p5GeV_fixedTarget  20200201.191904  20200201.191904:   9.1887e+06 : 19p5GeV_fixedTarget  9.3729e+06	            ?		  run = 21032039, Freq = 9.3729e+06 19p5GeV_fixedTarget Junk	       	
13p5GeV_fixedTarget  20200202.160409  20200202.160409:   9.3729e+06 : 13p5GeV_fixedTarget  9.3612e+06               ?	    	  run = 21033026, Freq = 9.3612e+06 13p5GeV_fixedTarget  
9p2GeV               20200203.202534  20200203.202534:   9.3612e+06 : 9p2GeV               9.1887e+06  	            ?		  run = 21034023, Freq = 9.1887e+06 9p2GeV	       	
7p3GeV_fixedTarget   20200204.053518  20200204.053518:   9.1887e+06 : 7p3GeV_fixedTarget   9.3071e+06  9.3374e+06   ?		  run = 21035004, Freq = 9.3071e+06 7p3GeV_fixedTarget  
9p2GeV               20200205.144626  20200205.144626:   9.3071e+06 : 9p2GeV               9.1887e+06  	            ?  : 9.249251 run = 21036032, Freq = 9.1887e+06 9p2GeV	       	 
11p5GeV              20200210.220428  20200210.220428:   9.1887e+06 : 11p5GeV              9.3374e+06	            ?		  run = 21041027, Freq = 9.3374e+06 11p5GeV	       	
5p75GeV_fixedTarget  20200213.152339  20200213.152339:   9.3374e+06 : 5p75GeV_fixedTarget  9.3374e+06	            ok		  
11p5GeV              20200214.143742  20200214.143742:   9.3374e+06 : 11p5GeV              9.3374e+06	            ok
9p2GeVb              20200224.230740  20200224.230740:   9.3374e+06 : 9p2GeVb              1e+07       9.1887e+06   ?             run = 21055035, Freq = 9.1887e+06 9p2GeVb             

*/
#endif
TFile **F = 0;
//________________________________________________________________________________
Double_t rowsigned(Int_t row, Int_t sector) {
  Double_t y = row;
  if (sector > 12) y = - row;
  return y;
}
//________________________________________________________________________________
Int_t SetFileList() {
  Int_t NF = 0;
  TSeqCollection *fs = gROOT->GetListOfFiles();
  if (! fs) {
    cout << "No root files " << endl; 
    return NF;
  }
  if (F) delete F;
  F = new TFile*[N]; memset(F, 0, N*sizeof(TFile*));
  TFile *f = 0;  
  for (Int_t k = 0; k < N; k++) {
    TIter  iter(fs);
    while ((f = (TFile *) iter())) {
      TNtuple *FitP = (TNtuple *) f->Get("FitP");
      if (! FitP) continue;
      TString name(gSystem->BaseName(f->GetName()));
      name.ReplaceAll(".root","");
      if (k == 0 && ! name.Contains(Names[k])) continue;
      if (k != 0 && ! name.EndsWith(Names[k])) continue;
      F[k] = f;
      NF++;
      break;
    }
  }
  return NF;
}
//________________________________________________________________________________
void FitPDraw(const Char_t *draw="mu:rowsigned(y,x)", 
	      const Char_t *ext = "P",
	      Int_t    nx = 0,   // 45,
	      Double_t xMin = 0, //  0.5,
	      Double_t xMax = 0, // 45.5,
	      const Char_t *cut = "(i&&j&&abs(mu)<1)/(dmu**2)", 
	      const Char_t *opt = "profg",
	      Double_t ymin = -1,
	      Double_t ymax =  1) {
  Int_t NF = SetFileList();
  if (! NF) return;
  TString Current(gDirectory->GetName());
  gStyle->SetOptStat(0);
  Int_t icol = 0;
  TLegend *leg = new TLegend(0.5,0.7,1.0,1.0);
  //  gStyle->SetMarkerSize(0.4);
  for (Int_t k = 0; k < N; k++) {
    if (! F[k]) continue;
    F[k]->cd();
    TNtuple *FitP = (TNtuple *) gDirectory->Get("FitP");
    if (! FitP) continue;
    icol = k + 1;
    Int_t kM = 20;
    if (icol == 3) kM = 24;
    if      (icol == 1) kM = 20;
    else if (icol == 5) kM = 20;
    else if (icol >  7) {kM = 27; icol -= 6;}
    FitP->SetMarkerStyle(kM);
    FitP->SetMarkerColor(icol);
    FitP->SetLineColor(icol);
    FitP->SetMarkerSize(1);
    TString same(opt);
    if (k != 0) same += "same";
    TString Draw(draw);
    TString histN(ext); histN += k;
    Draw += " >> "; Draw += histN;
    TProfile *p = 0;
    if (xMin == 0 && xMax == 0) {
      TH2 *mu = (TH2 *) gDirectory->Get("mu");
      if (mu) {
	if (Draw.Index("y") >= 0) {
	  //      Draw += Form("(%i,%f,%f)",mu->GetYaxis()->GetNbins(), mu->GetYaxis()->GetXmin(), mu->GetYaxis()->GetXmax());
	  p = new TProfile(histN,"#mu versus pad row",2*mu->GetYaxis()->GetNbins()+1, -mu->GetYaxis()->GetXmax(), mu->GetYaxis()->GetXmax());
	} else {
	  p = new TProfile(histN,"#mu versus pad row",2*mu->GetXaxis()->GetNbins()+1, -mu->GetXaxis()->GetXmax(), mu->GetXaxis()->GetXmax());
	}
      }
    } else {
      p = new TProfile(histN,"#mu versus pad row",nx, xMin, xMax);
    }
    p->SetMarkerColor(icol);
    p->SetLineColor(icol);
    p->SetMarkerStyle(kM);
    p->SetMarkerSize(1);
    cout << k << "\t" << F[k]->GetName() << endl;
    cout << "FitP->Draw(\"" << Draw << "\",\"" << cut << "\",\"" << same << "\")" << endl;
    FitP->Draw(Draw,cut,same);
    TH1 *hist = (TH1 *) gDirectory->Get(histN);
    if (hist) {
      if (ymin < ymax) {hist->SetMinimum(ymin); hist->SetMaximum(ymax);}
      TString name(gSystem->BaseName(gDirectory->GetName()));
      name.ReplaceAll(".root","");
      name.ReplaceAll("SecRow3CGF","");
      name.ReplaceAll("AvCurrentCGF","");
      name.ReplaceAll("Z3CGF","");
      leg->AddEntry(hist,name.Data());
    }
  }
  leg->Draw();
}
//________________________________________________________________________________
void DrawHist(const Char_t *dir="TPoints70BG", 
	      const Char_t *histN="mu") {
  TString Current(gDirectory->GetName());
  TSeqCollection *fs = gROOT->GetListOfFiles();
  if (! fs) {cout << "No root files " << endl; return;}
  TFile *F[9]; memset(F, 0, sizeof(F));
  TString Dir(dir);
  TRegexp reg(dir);
  Int_t icol = 0;
  TLegend *leg = new TLegend(0.5,0.7,1.0,1.0);
  TIter  iter(fs);
  TFile *f = 0;  
  while ((f = (TFile *) iter())) {
    TString name(gSystem->BaseName(f->GetName()));
    name.ReplaceAll(".root","");
    if (Dir != "" && Dir != "*" && ! name.Contains(reg)) continue; 
    Int_t l = 0;
    for (Int_t k = 0; k < N; k++) {
      //      cout << name << " and " << Names[k] << endl;
      if (name.EndsWith(Names[k])) {l = k+1;}
    }
    F[l] = f;
    cout << l << "\t" << F[l]->GetName() << endl;
  }
  //  gStyle->SetMarkerSize(0.4);
  for (Int_t k = 0; k <= N; k++) {
    if (! F[k]) continue;
    F[k]->cd();
    TString name(gSystem->BaseName(gDirectory->GetName()));
    name.ReplaceAll(".root","");
    cout << name << endl;
    TH1 *hist = (TH1 *) gDirectory->Get(histN);
    if (! hist ) continue;
    icol = k + 1;
    Int_t kM = 20;
    if (icol == 3) kM = 24;
#if 0
    if      (icol == 1) kM = 20;
    else if (icol == 5) kM = 20;
    else if (icol >  7) kM = 27;
#endif
    hist->SetMarkerStyle(kM);
    hist->SetMarkerColor(icol);
    hist->SetLineColor(icol);
    TString same("");
    if (k != 0) same += "same";
    hist->Draw(same);
    Int_t index = name.Index("Run");
    if (index < 0) index = 0;
    else           index += 3;
    leg->AddEntry(hist,name.Data()+index);
  }
  leg->Draw();
}
