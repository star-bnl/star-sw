/*
  root.exe SecRow3C*.root FitPDraw.C+
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
#define __RunXIX_RunXII__
#ifdef __RunXIX_RunXII__
static  Int_t N = 34;
static  const Char_t *Names[34] = {
  "19GeV_2019",
  "14p5GeV_2019",
  "7.3GeV_fixedTarget_2019",
  "7p7GeV_2019",
  "3p85GeV_fixedTarget_2019",
  "9p2GeV_2019",
  "4p59GeV_fixedTarget_2019",
  "9p2GeV_2019",
  "31GeV_fixedTarget_2019",
  "AuAu200_2019",

  "11p5GeV_2020",
  "5p75GeV_fixedTarget_2020",
  "31p2GeV_fixedTarget_2020",
  "9p8GeV_fixedTarget_2020",
  "9p2GeV_2020",
  "19p5GeV_fixedTarget_2020",
  "13p5GeV_fixedTarget_2020",
  "7p3GeV_fixedTarget_2020",
  "9p2GeVb_2020",
  "9p2GeVc_2020",
  "26p5GeV_fixedTarget_2020",
  "7p7GeV_2020",

  "7p7GeV_2021",
  "3p85GeV_fixedTarget_2021",
  "44p5GeV_fixedTarget_2021",
  "70GeV_fixedTarget_2021",
  "100GeV_fixedTarget_2021",
  "OO_200GeV_2021",
  "ps_OO_200GeV_2021",
  "FF_OO_200GeV_2021",
  "17p3GeV_2021",
  "26p5GeV_fixedTarget_2021",
  "dAu200_2021",

  "pp500GeV_2022"
};
#else
#define __RUNXXI__
#if defined(__RUNXIX__)
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
#elif  defined(__RUNXX__)
static Int_t N = 14;
static const Char_t *Names[14] = {
  "COL", // "RunXX22B",
  "FXT",
  "11p5GeV",
  "13p5GeV_fixedTarget",
  "19p5GeV_fixedTarget",
  "26p5GeV_fixedTarget",
  "31p2GeV_fixedTarget",
  "5p75GeV_fixedTarget",
  "7p3GeV_fixedTarget",
  "7p7GeV",
  "9p2GeVb",
  "9p2GeVc",
  "9p2GeV",
  "9p8GeV_fixedTarget"
  //  "Cosmic"
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
#elif  defined(__RUNXXI__)
static Int_t N = 9;
static const Char_t *Names[9] = {
  "All",
  "100GeV_fixedTarget_2021",  
  "3p85GeV_fixedTarget_2021",  
  "44p5GeV_fixedTarget_2021",  
  "70GeV_fixedTarget_2021",  
  "7p7GeV_2021",  
  "Cosmic",  
  "OO_200GeV_2021",
  "ps_OO_200GeV_2021"};
  //  "Cosmic"
#endif
#endif
TFile *F[100]  = {0};
//________________________________________________________________________________
Double_t rowsigned(Int_t row, Int_t sector) {
  Double_t y = row;
  if (sector > 12) y = - row;
  return y;
}
//________________________________________________________________________________
Int_t SetFileList() {
  TSeqCollection *fs = gROOT->GetListOfFiles();
  Int_t NF = fs->GetEntries();
  if (! fs) {
    cout << "No root files " << endl; 
    return NF;
  }
  //  if (F) delete F;
  //  F = new TFile*[NF]; 
  memset(F, 0, NF*sizeof(TFile*));
  TFile *f = 0;  
  TIter  iter(fs);
  Int_t k = 0;
  while ((f = (TFile *) iter())) {
    TNtuple *FitP = (TNtuple *) f->Get("FitP");
    if (! FitP) continue;
    F[k] = f;
    cout << k << "\tAdd " << F[k]->GetName() << endl;
    k++;
  }
  if (NF != k) {cout << "NF = " << NF << " k = " << k << " mismatched" << endl;}
  return NF;
}
//________________________________________________________________________________
void MuDraw(const Char_t *draw="mu:rowsigned(y,x)", 
	    const Char_t *ext = "P",
	    Int_t    nx = 0,   // 45,
	    Double_t xMin = 0, //  0.5,
	    Double_t xMax = 0, // 45.5,
	    const Char_t *cut = "(i&&j&&abs(mu)<1&&dmu>0&&dmu<0.02)", // "(i&&j&&abs(mu)<1)/(dmu**2)", 
	    const Char_t *opt = "prof", // "profg",
	    Double_t yMin = -1,
	    Double_t yMax =  1,
	    const Char_t *side = "All",
	    const Char_t *var  = "sector*side", // log_{10}(#Sigma Adc)"
	    const Char_t *title = "#mu versus pad row"
	    ) {
  cout << "MuDraw(\"" << draw << "\",\"" << ext << "\"," << nx << "," << xMin << "," << xMax << ",\"" << cut << ",\"" << opt << "\"," << yMin << "," << yMax 
       << ",\"" <<  side << "\",\"" << var << "\",\"" << title << "\")" << endl;
  TCanvas *c1 = (TCanvas*)gROOT->GetListOfCanvases()->FindObject("c1");
  if (! c1)  c1 = new TCanvas("c1","c1",1400,1200);
  else       c1->Clear();
  Int_t NF = SetFileList();  
  if (! NF) return;
  TString Current(gDirectory->GetName());
  //  gStyle->SetOptStat(0);
  Int_t icol = 0;
  TLegend *leg = new TLegend(0.6,0.1,0.9,0.3);
  //  gStyle->SetMarkerSize(0.4);
  for (Int_t k = 0; k < NF; k++) {
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
    TString Drawh(Draw);
    Drawh += " >> "; Drawh += histN;
    TProfile *p = new TProfile(histN,title,nx, xMin, xMax, yMin, yMax);
    if (p) {
      p->SetMarkerColor(icol);
      p->SetLineColor(icol);
      p->SetMarkerStyle(kM);
      p->SetMarkerSize(1);
    }
#if 1
    cout << Form("%2i %-52s\t", k, F[k]->GetName()); //  << endl;
    cout << "FitP->Draw(\"" << Drawh << "\",\"" << cut << "\",\"" << same << "\")" << "\t"; //  endl;
#endif
    FitP->Draw("mu>>muH",cut,"goff");
    TH1 *muH = (TH1 *) gDirectory->Get("muH");
    Double_t RMS = -99;
    if (muH) RMS = 100*muH->GetRMS();
    c1->cd();
    FitP->Draw(Drawh,cut,same);
    TH1 *hist = (TH1 *) gDirectory->Get(histN);
    if (hist) {
      //      TString name(gSystem->BaseName(gDirectory->GetName()));
      TString ddir(gSystem->DirName(gDirectory->GetName()));
      TString dir(gSystem->BaseName(ddir));
      if (dir == "./") dir = "";
      TString name = gSystem->BaseName(gDirectory->GetName());
      name.ReplaceAll(".root","");
#if 0
      name.ReplaceAll("SecRow3C+SecRow3PC","cor");
      name.ReplaceAll("SecRow3+SecRow3P","");
      name.ReplaceAll("SecRow3C","");
      name.ReplaceAll("SecRow3","");
      name.ReplaceAll("Z3C+Z3PC","");
      name.ReplaceAll("Z3+Z3P","cor");
      name.ReplaceAll("Z3C","");
      name.ReplaceAll("Z3","");
      name.ReplaceAll("xyPad3C+xyPad3PC","cor");
      name.ReplaceAll("xyPad3+xyPad3P","");
      name.ReplaceAll("xyPad3C","");
      name.ReplaceAll("xyPad3","");
      name.ReplaceAll("_y3","");
      name.ReplaceAll("G4E","");
#endif
      leg->AddEntry(hist,Form("%s%s %s",dir.Data(), name.Data(),side));
      hist->SetTitle(Form("%s : %s",hist->GetTitle(), side));
      hist->SetXTitle(var);
      //      cout << k << "\t" << name.Data() << "\tmin = " << 100*hist->GetMinimum() << "\tmax = " <<  100*hist->GetMaximum() << " %" << endl;
      TString nn(name);
      cout << Form("%3i %-45s%4s: min/max = %7.2f/%7.2f rms %7.2f (\%)", k, nn.Data(), side, 100*hist->GetMinimum(), 100*hist->GetMaximum(), RMS) << endl;
      if (yMin < yMax) {hist->SetMinimum(yMin); hist->SetMaximum(yMax);}
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
//________________________________________________________________________________
void FitPMu(const Char_t *draw="mu", 
	      const Char_t *cut = "i&&j", 
	      const Char_t *opt = ""
// 	      const Char_t *ext = "P",
// 	      Int_t    nx = 0,   // 45,
// 	      Double_t xMin = 0, //  0.5,
// 	      Double_t xMax = 0, // 45.5,
// 	      Double_t ymin = -1,
// 	      Double_t ymax =  1
	    ) {
  Int_t NF = SetFileList();
  if (! NF) return;
  TString Current(gDirectory->GetName());
  //  gStyle->SetOptStat(0);
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
    Draw += ">> Mu";
    FitP->Draw(Draw,cut,same);
    TString name(gSystem->BaseName(gDirectory->GetName()));
    name.ReplaceAll(".root","");
    name.ReplaceAll("SecRow3CGF","");
    name.ReplaceAll("AvCurrentCGF","");
    name.ReplaceAll("Z3CGF","");
    name.ReplaceAll("xyPad3CGF","");
    TH1 *hist = (TH1 *) gDirectory->Get("Mu");
    leg->AddEntry(hist,name.Data());
  }
  leg->Draw();
}
//________________________________________________________________________________
void FitPDraw(TString Opt = "I", TString plot = "") {
  if (! gDirectory) {return;}
  Int_t nx = 0;
  Int_t ny = 0;
  Double_t xMin = 0;
  Double_t xMax = 0;
  Double_t yMin = -1;
  Double_t yMax = 1;
  TH2 *mu = (TH2 *) gDirectory->Get("mu");
  if (mu) {
    nx = mu->GetXaxis()->GetNbins();
    xMin = mu->GetXaxis()->GetXmin();
    xMax = mu->GetXaxis()->GetXmax();
    if (mu->GetDimension() == 2) {
      ny = mu->GetYaxis()->GetNbins();
      yMin = mu->GetYaxis()->GetXmin();
      yMax = mu->GetYaxis()->GetXmax();
    }
  }
  TString Name(gSystem->BaseName(gDirectory->GetName()));
  cout << Name << "\t";
  TString muPlot("mu-muJ");
  if (plot.Contains("nomuJ",TString::kIgnoreCase)) muPlot = "mu";
  if        (Name.BeginsWith("SecRow3")) { 
    MuDraw("mu:rowsigned(y,x)", "P", 2*nx+1, -yMax, yMax, "(i&&j&&abs(mu)<1&&dmu>0&&dmu<0.02)", "prof", -1, 1, "All", "sector&side", "#mu versus pad row");
  } else if (Name.BeginsWith("Z3"))      {
    muPlot += ":TMath::Sign(208.707-y,x)";
    if (Opt == "") {
      MuDraw(muPlot.Data(),"ZI", 2*ny, -yMax, yMax, "(i&&j&&dmu>0&&dmu<0.1&&abs(mu)<0.4)", "prof", -0.4,  0.4, "All", "Z", "#mu versus Z");
    } else if (Opt == "I") {
      MuDraw(muPlot.Data(),"ZI", 2*ny, -yMax, yMax, "(i&&j&&abs(x)<40.5&&dmu>0&&dmu<0.1&&abs(mu)<0.4)", "prof", -0.4,  0.4, "Inner", "Z", "#mu versus Z");
    } else {
      MuDraw(muPlot.Data(),"ZO", 2*ny, -yMax, yMax, "(i&&j&&abs(x)>40.5&&dmu>0&&dmu<0.1&&abs(mu)<0.4)", "prof", -0.4,  0.4, "Outer", "Z", "#mu versus Z");
    }
  } else if (Name.BeginsWith("dX3"))      {
    muPlot += ":TMath::Sign(y,x)";
    if (Opt == "") {
      MuDraw(muPlot.Data(),"dXI", 2*ny, -yMax, yMax, "(i&&j&&dmu>0&&dmu<0.1&&abs(mu)<0.4)", "prof", -0.4,  0.4, "All", "dX", "#mu versus log2(dX)");
    } else if (Opt == "I") {
      MuDraw(muPlot.Data(),"dXI", 2*ny, -yMax, yMax, "(i&&j&&abs(x)<40.5&&dmu>0&&dmu<0.1&&abs(mu)<0.4)", "prof", -0.4,  0.4, "Inner", "dX", "#mu versus log2(dX)");
    } else {
      MuDraw(muPlot.Data(),"dXO", 2*ny, -yMax, yMax, "(i&&j&&abs(x)>40.5&&dmu>0&&dmu<0.1&&abs(mu)<0.4)", "prof", -0.4,  0.4, "Outer", "dX", "#mu versus log2(dX)");
    }
  } else if (Name.BeginsWith("Eta3") || Name.BeginsWith("EtaB3" ))      {
    muPlot += ":y";
    if (Opt == "") {
      MuDraw(muPlot.Data(),"EtaI", ny, yMin, yMax, "(i&&j&&dmu>0&&dmu<0.1&&abs(mu)<0.4)", "prof", -0.4,  0.4, "All", "Eta", "#mu versus #eta");
    } else if (Opt == "I") {
      MuDraw(muPlot.Data(),"EtaI", ny, yMin, yMax, "(i&&j&&abs(x)<40.5&&dmu>0&&dmu<0.1&&abs(mu)<0.4)", "prof", -0.4,  0.4, "Inner", "Eta", "#mu versus #eta");
    } else {
      MuDraw(muPlot.Data(),"EtaO", ny, yMin, yMax, "(i&&j&&abs(x)>40.5&&dmu>0&&dmu<0.1&&abs(mu)<0.4)", "prof", -0.4,  0.4, "Outer", "Eta", "#mu versus #eta");
    }
  } else if (Name.BeginsWith("xyPad3qB"))      {
    muPlot += ":0.5*y+TMath::Nint(x)";
    if (Opt == "") {
      MuDraw(muPlot.Data(),"xy", 48*32,   0.5, 48.5, "(i&&j&&dmu>0&&dmu<0.1&&abs(mu)<0.4)", "prof", -0.45,  0.15, "All", "xy", "#mu versus sector phi");
    } else if (Opt == "I") {
      MuDraw(muPlot.Data(),"xyI", 48*32,   0.5, 48.5, "(i&&j&&dmu>0&&dmu<0.1&&abs(mu)<0.4&&(x-TMath::Nint(x))<0)", "prof", -0.45,  0.15, "Inner", "xy", "#mu versus sector phi");
    } else if (Opt == "O") {
      MuDraw(muPlot.Data(),"xyO", 48*32,   0.5, 48.5, "(i&&j&&dmu>0&&dmu<0.1&&abs(mu)<0.4&&(x-TMath::Nint(x))>0)", "prof", -0.45,  0.15, "Outer", "xy", "#mu versus sector phi");
    }
  } else if (Name.BeginsWith("xyPad3"))      {
    muPlot += ":0.5*y+TMath::Nint(x)";
    if (Opt == "") {
      MuDraw(muPlot.Data(),"xy", 24*32,   0.5, 24.5, "(i&&j&&dmu>0&&dmu<0.1&&abs(mu)<0.4)", "prof", -0.45,  0.15, "All", "xy", "#mu versus sector phi");
    } else if (Opt == "I") {
      MuDraw(muPlot.Data(),"xyI", 24*32,   0.5, 24.5, "(i&&j&&dmu>0&&dmu<0.1&&abs(mu)<0.4&&(x-TMath::Nint(x))<0)", "prof", -0.45,  0.15, "Inner", "xy", "#mu versus sector phi");
    } else if (Opt == "O") {
      MuDraw(muPlot.Data(),"xyO", 24*32,   0.5, 24.5, "(i&&j&&dmu>0&&dmu<0.1&&abs(mu)<0.4&&(x-TMath::Nint(x))>0)", "prof", -0.45,  0.15, "Outer", "xy", "#mu versus sector phi");
    }
  } else if (Name.BeginsWith("Pressure"))      {
    muPlot += ":y";
    if (Opt == "I") {
      MuDraw(muPlot.Data(),"PI", nx, xMin, xMax, "(i&&j&&abs(x)<40.5&&dmu>0&&dmu<0.1&&abs(mu)<0.4)", "prof", -0.4,  0.4, "Inner", "log(P)", "#mu versus log(P)");
    } else if (Opt == "O") {
      MuDraw(muPlot.Data(),"PO", nx, xMin, xMax, "(i&&j&&abs(x)>40.5&&dmu>0&&dmu<0.1&&abs(mu)<0.4)", "prof", -0.4,  0.4, "Outer", "log(P)", "#mu versus log(P)");
    } else if (Opt == "IJ") {
      MuDraw(muPlot.Data(),"PI", nx, xMin, xMax, "(i&&j&&abs(x)<40.5&&dmu>0&&dmu<0.1&&abs(mu)<0.4)", "prof", -0.4,  0.4, "Inner", "log(P)", "#mu versus log(P)");
    }
  } else if (Name.BeginsWith("neP"))      {
    if (plot == "sigma") {
      MuDraw("sigma:x","S", 35,   3.0,  10.0, "(i&&j&&mu>0&&dmu>0&&dmu<0.01)", "prof", 0.0,  0.6, "All", "neP","#sigma versus log(nP)");
    }  else if (plot == "alpha") {
      MuDraw("p3:x","A", 35,   3.0,  10.0, "(i&&j&&mu>0&&dmu>0&&dmu<0.01)", "prof", -3.0,  4.0, "All", "neP","#sigma versus log(nP)");
    } else {
      MuDraw("mu:x","M", 35,   3.0,  10.0, "(i&&j&&mu>0&&dmu>0&&dmu<0.01)", "prof", 0.0,  1.0, "All", "neP","#mu versus log(nP)");
    }
  } else if (Name.BeginsWith("Adcne"))      {
    if (plot == "sigma") {
      MuDraw("sigma:x","S", 35,   3.0,  10.0, "(i&&j&&mu>0&&dmu>0&&dmu<0.01)", "prof", 0.0,  0.6, "All", "neP","#sigma versus log(nP)");
    }  else if (plot == "alpha") {
      MuDraw("p3:x","A", 35,   3.0,  10.0, "(i&&j&&mu>0&&dmu>0&&dmu<0.01)", "prof", -3.0,  4.0, "All", "neP","#sigma versus log(nP)");
    } else {
      MuDraw("mu:x","M", 35,   3.0,  10.0, "(i&&j&&mu>0&&dmu>0&&dmu<0.01)", "prof", 0.0,  1.0, "All", "neP","#mu versus log(nP)");
    }
  } else if (Name.BeginsWith("neN"))      {
    MuDraw("mu:x","PI", nx, xMin, xMax, "(i&&j&&dmu<0.1&&dsigma<0.04&&dp3<1&&mu<1&&p3>0)", "prof", 0.5,  1.0, "All", "neN","#mu versus log(nP)");
  } else if (Name.Contains("GEX"))      {
    if (Opt == "sigma") {
      MuDraw("sigma:x","znpL",nx, xMin, xMax, "(i&&dmu<2e-2&&dsigma<2e-2&&da0<1&&x>3)", "prof", 0.0,  1.0, "All", "npL","#mu versus Log(n_{P}");
    } else if (Opt == "k") {
      MuDraw("a0:x","znpL",nx, xMin, xMax, "(i&&dmu<2e-2&&dsigma<2e-2&&da0<1&&x>3)", "prof", -10.0,  10.0, "All", "npL","#mu versus Log(n_{P})");
    } else {
      if (plot == "") {
	MuDraw("mu:x","znpL",nx, xMin, xMax, "(i&&dmu>0&&dmu<2e-2&&dsigma<2e-2&&da0<1)", "prof", -0.4,  1.6, "All", "npL","#mu versus Log(n_{P})");
      } else {
	if (plot.Contains("sigma")) 
	  MuDraw(plot.Data(),"znpL", 75,   3.0, 10.5, "(i&&dmu<2e-2&&dsigma<2e-2&&da0<1&&x>3)", "", 0.0,  0.6, "All", "npL","#mu versus Log(n_{P})");
	else if (plot.Contains("a0")) 
	  MuDraw(plot.Data(),"znpL", 75,   3.0, 10.5, "(i&&dmu<2e-2&&dsigma<2e-2&&da0<1&&x>3)", "", -10.,  10., "All", "npL","#mu versus Log(n_{P})");
	else 
	  MuDraw(plot.Data(),"znpL", 75,   3.0, 10.5, "(i&&dmu<2e-2&&dsigma<2e-2&&da0<1&&x>3)", "", -0.4,  6.2, "All", "npL","#mu versus Log(n_{P})");
      }
    }
  } else if (Name.BeginsWith("dNdxVsBg"))      {
    MuDraw("mu:x","PI", 60, -2, 5, "(i&&j&&dmu>0&&dmu<0.01&&dsigma<0.01&&mu<0)", "prof", -0.5,  0.0, "All", "neN","#mu versus log_{10}(#beta #gamma)");
  } else if (Name.BeginsWith("NpdN"))      {
    MuDraw("mu:x","PI", 70, 3, 10, "(i&&j&&dmu>0&&dmu<0.01&&dsigma<0.01)", "prof", -0.1,  0.6, "All", "log(nP)","#mu of log(((dN/dx)*dX)/(nP)) versus log(nP)");
  } else if (Name.BeginsWith("dNNp"))      {
    MuDraw("mu:x","PI", 120, 4, 10, "(i&&j&&dmu>0&&dmu<0.01&&dsigma<0.01)/dmu**2", "profg", -0.2,  -0.05, "All", "log(dN/dx*dx)","#mu of log(Np/(dN/dx*dx)) versus log((dN/dx)*dX))");
  } else if (Name.BeginsWith("NPoin"))      {
    muPlot = "mu"; 
    TString muTitle = "#mu versus ";
    Double_t min = -0.2;
    Double_t max =  0.2;
    if (plot.Contains("sigma",TString::kIgnoreCase)) {muPlot = "sigma"; muTitle = "#sigma versus "; min = 0;}
    if (plot.Contains("y",TString::kIgnoreCase)) {muPlot += ":y"; muTitle += "#eta";}
    else                                         {muPlot += ":x"; muTitle += "No. dEdx Points";}
    MuDraw(muPlot,"P", 100, 0, 100, "i&&j&&dmu<0.01&&dsigma<0.01", "profg", min,  max, "All", "No. dE/dx points",muTitle);
  } else if (Name.BeginsWith("Time"))      {
    MuDraw("mu:x","T", 280, 7.6e8, 9.0e8, "(i&&j&&dmu>0&&dmu<0.01)", "profg", -0.4,  1.4, "All", "Time","#mu");
  }
}
