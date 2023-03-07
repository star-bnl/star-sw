#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
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
#include "TClassTable.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "TDataSet.h"
#include "TDataSetIter.h"
#include "TClassTable.h"
//#include "DeDxTree.C"
#include "TMinuit.h"
#include "TSpectrum.h"
#include "StBichsel/Bichsel.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TString.h"
#include "TLine.h"
#include "TText.h"
#include "TROOT.h"
#include "TList.h"
#include "TPolyMarker.h"
#include "TLegend.h"
#include "TPRegexp.h"
#else
class TMinuit;
class TF1;
class TH1F;
class TH2F;
class TH3F;
class TProfile;
class TH2D;
class TCanvas;
class TSpectrum;
class TSystem;
class Bichsel;
class TPRegexp;
#endif
using namespace std;
#if 0
void DrawdEdx();
void bichselG10(const Char_t *type, Int_t Nhyps=9);
#endif
//________________________________________________________________________________
void SetTitle(TString &Title) {
  //      if (Title.Contains("602P03ih"))  Title.ReplaceAll("602P03ih"," default ");
  //  cout << "Set Title: " << Title.Data();
  Title.ReplaceAll("TPoints70U+TPoints70UP", "U70+-");
  Title.ReplaceAll("TPointsNU+TPointsNUP",   "UN+-");
  Title.ReplaceAll("TPointsFU+TPointsFUP",   "UF+-");
  Title.ReplaceAll("TPoints70+TPoints70P" ,  "70+-");
  Title.ReplaceAll("TPointsN+TPointsNP" ,    "N+-");
  Title.ReplaceAll("TPointsF+TPointsFP" ,    "F+-");

  Title.ReplaceAll("TPoints70U", "U70-");
  Title.ReplaceAll("TPointsNU",   "UN-");
  Title.ReplaceAll("TPointsFU",   "UF-");
  Title.ReplaceAll("TPoints70" ,  "70-");
  Title.ReplaceAll("TPointsN" ,    "N-");
  Title.ReplaceAll("TPointsF" ,    "F-");

  Title.ReplaceAll("TPoints70UP", "U70+");
  Title.ReplaceAll("TPointsNUP",   "UN+");
  Title.ReplaceAll("TPointsFUP",   "UF+");
  Title.ReplaceAll("TPoints70P" ,  "70+");
  Title.ReplaceAll("TPointsNP" ,    "N+");
  Title.ReplaceAll("TPointsFP" ,    "F+");

  Title.ReplaceAll("NPoints70U+NPoints70UP", "U70+-");
  Title.ReplaceAll("NPointsNU+NPointsNUP",   "UN+-");
  Title.ReplaceAll("NPointsFU+NPointsFUP",   "UF+-");
  Title.ReplaceAll("NPoints70+NPoints70P" ,  "70+-");
  Title.ReplaceAll("NPointsN+NPointsNP" ,    "N+-");
  Title.ReplaceAll("NPointsF+NPointsFP" ,    "F+-");

  Title.ReplaceAll("NPoints70U", "U70-");
  Title.ReplaceAll("NPointsNU",   "UN-");
  Title.ReplaceAll("NPointsFU",   "UF-");
  Title.ReplaceAll("NPoints70" ,  "70-");
  Title.ReplaceAll("NPointsN" ,    "N-");
  Title.ReplaceAll("NPointsF" ,    "F-");

  Title.ReplaceAll("NPoints70UP", "U70+");
  Title.ReplaceAll("NPointsNUP",   "UN+");
  Title.ReplaceAll("NPointsFUP",   "UF+");
  Title.ReplaceAll("NPoints70P" ,  "70+");
  Title.ReplaceAll("NPointsNP" ,    "N+");
  Title.ReplaceAll("NPointsFP" ,    "F+");

  Title.ReplaceAll("production_","");
  Title.ReplaceAll("_ReversedFullField","");
  Title.ReplaceAll("_FullField","");
  Title.ReplaceAll("GP","");
  Title.ReplaceAll("P11ic_dedx_AuAu19_production_ReversedFullField","");
  Title.ReplaceAll("dev_calib_pp500_production_2011_ReversedFullField","");
  Title.ReplaceAll("AlAu200_P15ic_TOF_dEdx","CuAu200");
  //       Title.ReplaceAll("TPointsBAGPHist" ,"Ancorr"); if (Title.Contains("Ancorr")) continue;
  //       Title.ReplaceAll("TPointsBUGPHist" ,"Uncorr"); if (Title.Contains("Uncorr")) continue;
  //       Title.ReplaceAll("MPoints70BGPHist","M70   "); if (Title.Contains("M70")) continue;
  //       Title.ReplaceAll("MPointsBGPHist"  ,"Mfit  "); if (Title.Contains("MFit")) continue;
  //       Title.ReplaceAll("MPointsBUGPHist" ,"MUncorr");if (Title.Contains("MUncorr")) continue;
  if (Title.Contains("509P03iF"))  Title = "Fabrice's gain correction (Aug. 2003)";
  if (Title.Contains("510P03if"))  Title = "Tonko's gain corection, (Feb. 2003)";
  if (Title.Contains("511P03iF"))  Title = "Tonko's gain correction (Oct. 2003)";
  if (Title.Contains("512P03iT"))  Title = "Tonko's gain corection, (Feb. 2003) SecRow.Contains(1";
  if (Title.Contains("513P03iF"))  Title = "Fabrice's gain correction (Aug. 2003) SecRow.Contains(1";
  if (Title.Contains("516P03iFT")) Title = "TonkoAndMe's gain correction (Oct. 2003) SecRow.Contains(1";
  if (Title.Contains("527P03iT2")) Title = "Tonko's 'equalization' gain correction (Oct. 2003) SecRow.Contains(1";
  //  root.exe TPoints70GPHist312DEV.root TPoints70BGPRunII08P07id.root TPoints70BGPHist543P03ih.root TPoints70BGPHist970P05ic.root TPoints70BGPHist032P05if_dedx.root TPoints70BGPHist128P06id_dedx.root TPoints70BGPRunVII69P07ie.root TPoints70BGPRunVIII20P08ic_p*.root Res.C
  //  root.exe TPoints70BGPRunII08P07id.root TPoints70BGPHist543P03ih.root TPoints70BGPHist970P05ic.root TPoints70BGPHist032P05if_dedx.root TPoints70BGPHist128P06id_dedx.root TPoints70BGPRunVII69P07ie.root TPoints70BGPRunVIII20P08ic_p*.root Res.C
  if (Title.Contains("312DEV"))            Title = "AuAu 200 (Run II)";
  if (Title.Contains("RunII08P07id"))      Title = "AuAu 29 (Run II)";
  if (Title.Contains("543P03ih"))          Title = "dAu 200 (Run III)";
  //      if (Title.Contains("815P04if.ittf"))     Title = "dAu 200 (Run III ITTF)";
  if (Title.Contains("969P05ia_dedx"))     Title = "AuAu 200 (Run IV)";
  if (Title.Contains("970P05ic"))          Title = "AuAu 200 (Run IV)";
  if (Title.Contains("032P05if_dedx"))     Title = "CuCu 200 (Run V )";
  if (Title.Contains("128P06id_dedx"))     Title = "pp 200   (Run VI)";
  //      if (Title.Contains("RunVII46dEdx3"))     Title = "AuAu 200 (Run VII)";
  if (Title.Contains("RunVII69P07ie"))     Title = "AuAu 200 (Run VII)";
  if (Title.Contains("RunVIII20P08ic_pp")) Title = "pp 200 (Run VIII)";
  if (Title.Contains("RunVIII20P08ic_pr")) Title = "dAu 200 (Run VIII)";
  if (Title.Contains("GPRunIX65P09ig_calibAB")) Title = "pp200 (Run IX, data)";
  if (Title.Contains("GPrcf9108.zC"))      Title = " pp200 (Run IX, TpcRS)";
  Title.ReplaceAll("Ifit 270","I_{70} ");
  Title.ReplaceAll("Ifit 2F","I_{fit} ");
  Title.ReplaceAll("Ifit 2N","dN/dx ");
  //  cout << "\t" << Title.Data() << endl;
}
//________________________________________________________________________________
void Set(Int_t color=1) {
  cout << "Set color = " << color << endl;
  TTree *FitP = (TTree *) gDirectory->Get("FitP");
  if (FitP) {
    FitP->SetMarkerStyle(20);
    FitP->SetMarkerColor(color);
    FitP->SetLineColor(color);
  } else {cout << "FitP is not found" << endl;}
  TH1 *mu = (TH1 *) gDirectory->Get("mu");
  if (mu) {
    mu->SetMarkerStyle(20);
    mu->SetMarkerColor(color);
    mu->SetLineColor(color);
    mu->SetMarkerSize(0.3);
  } else {cout << "mu is not found" << endl;}
  TH1 *sigma = (TH1 *) gDirectory->Get("sigma");
  if (sigma) {
    sigma->SetMarkerStyle(20);
    sigma->SetMarkerColor(color);
    sigma->SetLineColor(color);
    sigma->SetMarkerSize(0.8);
  } else {cout << "sigma is not found" << endl;}
  TH1 *Mu = (TH1 *) gDirectory->Get("Mu");
  if (Mu) {
    Mu->SetMarkerStyle(20);
    Mu->SetMarkerColor(color);
    Mu->SetLineColor(color);
    Mu->SetMarkerSize(0.3);
  } else {cout << "Mu is not found" << endl;}
  TH1 *Sigma = (TH1 *) gDirectory->Get("Sigma");
  if (Sigma) {
    Sigma->SetMarkerStyle(20);
    Sigma->SetMarkerColor(color);
    Sigma->SetLineColor(color);
    Sigma->SetMarkerSize(0.8);
  } else {cout << "Sigma is not found" << endl;}
}
//________________________________________________________________________________
void Res(const Char_t *select="x", const Char_t *name="sigma", const Char_t *pattern = "") {
  TString Summary;
  //  const Char_t *FitNames[3] = {"Fit","I70","I60"};
  TPRegexp Pattern(pattern);
  TString plot(name);
  TString Plot;
  Bool_t IsLength = kTRUE;
  if (plot == "sigma") Plot = "Sigma";
  if (plot == "mu")    Plot = "Mu";
  Int_t NF = 0;
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t nn = files->GetSize();
  if (! nn) return;
  TFile **FitFiles = new TFile *[nn];
  TIter next(files);
  TFile *f = 0;
  TString Select(select);
  TString Title;
  Int_t all = 0;
  if (Select.Contains("all",TString::kIgnoreCase)) all = 1;
  while ( (f = (TFile *) next()) ) { 
    TString F(f->GetName());
    TTree *FitP = (TTree *) gDirectory->Get("FitP");
    if (! F.Contains("TPoints") && ! F.Contains("MPoints") && ! F.Contains("NPoints")) continue;
    if (Pattern.GetPattern() != ""  &&  F.Contains(Pattern)) continue;
    if (! all && (F.Contains("MPoints") || F.Contains("BUGP") || F.Contains("BAGP"))) continue;
    Int_t indx = 0;
    if ( F.Contains("70")) indx = 1;
    cout << "File \t" << F.Data() << endl;
    if (F.Contains("NPoint")) IsLength = kFALSE;
    FitFiles[NF] = f; 
    NF++;
  }
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (! c1 ) c1 = new TCanvas("c1","Resolution versus Track Length");
  c1->Clear();
  c1->SetTitle("Resolution versus Track Length");
  c1->SetGrid(); //x(9);
  //  c1->SetGridy(30);
  TCanvas *c2 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c2");
  if (! c2 ) c2 = new TCanvas("c2","c2");
  c1->cd();
  TH1F *frame = 0;
  Double_t xmin =  10;
  Double_t xmax = 210;
  if (! IsLength) {xmin = 10.5; xmax = 72.5;}
  TString PlotH(Plot); PlotH += Form("(100,%f,%f)",xmin,xmax);
  cout << "plot = " << plot.Data() << "\tPlot = " << PlotH.Data() << " pattern " << Pattern.GetPattern().Data() << endl;
  if (plot == "sigma") {
    frame = c1->DrawFrame(xmin,0.04,xmax,0.16);
    if (IsLength) frame->SetTitle("Resolution versus Track Length");
    else          frame->SetTitle("Resolution versus No. dEdx/dx points");
    //    frame->SetYTitle("Resolution");
  } else {
    frame = c1->DrawFrame(xmin,-0.05,xmax,0.10);
    if (IsLength) frame->SetTitle("Shift versus Track Length");
    else          frame->SetTitle("Shift versus No. dEdx/dx points");
    //    frame->SetYTitle("Sift");
  }
  if (IsLength) frame->SetXTitle("Track Length (cm)                   ");
  else          frame->SetXTitle("No. dEdx/dx points                  ");
  TLegend *leg = new TLegend(0.55,0.6,0.9,0.9,"");
  Int_t c = 0;
  TF1 *powfit = 0;
  if (plot == "sigma") {
    //    powfit = new TF1("powfit","[0]*TMath::Power(x,[1])",xmin,xmax);
    //    powfit = new TF1("powfit","TMath::Exp([0]+TMath::Log(x)*([1]+TMath::Log(x)*[2]))",xmin,xmax);
    powfit = new TF1("powfit","TMath::Exp([0]+TMath::Log(x)*([1]+TMath::Log(x)*([2]+TMath::Log(x)*[3])))",xmin,xmax);
    //    powfit = new TF1("powfit","TMath::Exp([0]+TMath::Log(x)*[1])",xmin,xmax);
    powfit->SetParameters(-0.7,-0.5,0.0);
  } else {
    powfit = new TF1("powfit","pol0",xmin,xmax);
    //	powfit->SetParameter(0,0.);
  }
  for (int i = 0; i<NF; i++) {
    c = i + 1;
    if (c == 10) c = 11;
    if (FitFiles[i]) { 
      FitFiles[i]->cd();
      TH1 *Hist = 0;
#if 0
      TH1 *hist = (TH1 *) FitFiles[i]->Get(plot);
      if (! hist) continue;
      if (hist->GetDimension() == 2) {
	Hist = ((TH2 *) hist)->ProjectionX(Form("%s_%i",Plot.Data(),i),0,0);
      } else {
	Hist = hist; 
      }
#endif
      Title = gSystem->BaseName(FitFiles[i]->GetName());
      Title.ReplaceAll(".root","");
      if (! (Title.Contains("TPoints") ||  Title.Contains("NPoints"))) continue;
      if (! Hist || Hist->GetEntries() < 1.e-4) {
	TTree *FitP = (TTree *) gDirectory->Get("FitP");
	if (! FitP) continue;
	FitP->SetMarkerColor(c);
	powfit->SetLineColor(c);
	if (plot != "sigma") {
	  //	    FitP->Draw(Form("%s:x>>%s",plot.Data(),PlotH.Data()),"(sigma<0.2&&i&&j==0&&chisq<200&&dmu<0.01&&sigma>0.03)/dmu**2","profsameggoff");
	    FitP->Draw(Form("%s:x>>%s",plot.Data(),PlotH.Data()),"(sigma<0.2&&i&&j&&chisq<200&&dmu<0.01&&sigma>0.03)","profsame");
	} else {
	  //	    FitP->Draw(Form("%s:x>>%s",plot.Data(),PlotH.Data()),"(i&&j==0&&chisq<200&&dmu<0.01&&sigma>0.03)/dsigma**2","profsameggoff");
	  if (IsLength)
	    FitP->Draw(Form("%s:x>>%s",plot.Data(),PlotH.Data()),"(sigma<0.2&&i&&j&&chisq<200&&dmu<0.01&&sigma>0.03)","profsame");
	  else 
	    //	    FitP->Draw(Form("%s:x>>%s",plot.Data(),PlotH.Data()),"i&&j&&prob>0.01&&dmu<0.01&&dsigma<0.01&&sigma<6/x","profsame");
	    FitP->Draw(Form("%s:x>>%s",plot.Data(),PlotH.Data()),"i&&j&&chisq<80&&dmu<0.01&&dsigma<0.01","profsame");
	}
	c1->Update();
	Hist = (TProfile *) FitFiles[i]->Get(Plot);
      }
      if (! Hist) continue;
      Hist->SetMarkerColor(c); Hist->SetMarkerStyle(20); Hist->SetLineColor(c);
      if (Hist->GetEntries() < 1.e-4) continue;
      //      Hist->Rebin();
      //      Hist->Scale(0.5);
      powfit->SetLineColor(c);
      c2->cd();
      Hist->Fit(powfit,"r","",xmin,xmax);
      //      Hist->Fit("powfit","r");
      c1->cd();
      Hist->SetStats(0);
      Hist->Draw("same");
      //      leg->AddEntry(sigma,Form("%s:  #sigma(@76cm) = %5.2f%%",FitNames[i],100*powfit->Eval(76)));
      SetTitle(Title);
      TString ltit;
      if (plot == "sigma") {
	if (IsLength ) {
	  //	ltit += Form(" : #sigma(@76cm) = %5.2f%%\%",100*powfit->Eval(76));
	  //	  Double_t L = 77.34;
	  //	  ltit = Form("#sigma(@%5.1fcm) = %5.2f%\%",L,100*powfit->Eval(L)); 
	  Double_t L = 124.4;
	  ltit = Form("#sigma(@%5.1fcm) = %5.2f%\%",Title.Data(),L,100*powfit->Eval(L));
	} else {
	  Double_t L = 72.0 * 0.85; // 
	  if (Title.Contains("70")) L *= 2./3.;
	  ltit = Form("#sigma(@%2.0f npts) = %5.2f%\%",L,100*powfit->Eval(L));
	}
      } else {
	ltit = Form("#mu = %5.2f%\%",100*powfit->GetParameter(0));
      }
      //      cout << "ltit " << ltit.Data() << endl;
      ltit += " for ";
      ltit += Title;
      //      ltit += Form(" : #sigma(@128cm) = %5.2f%%\%",100*powfit->Eval(128));
      //      ltit.Strip();
      cout << gDirectory->GetName() << endl << ltit << endl;
      leg->AddEntry(Hist,ltit.Data());
      Summary += ltit; Summary += "\n";
    }
  }
  if (plot == "sigma" ) {
    if (IsLength) {
      //    Double_t PositionX = 76.2, PositionY = 0.076;
      Double_t PositionX = 77.34, PositionY = 0.0742; // TPC
      TPolyMarker *pm = new TPolyMarker(1, &PositionX, &PositionY);
      frame->GetListOfFunctions()->Add(pm);
      pm->SetMarkerStyle(20);
      pm->SetMarkerColor(kRed);
      pm->SetMarkerSize(2.3);
      PositionX = 124.4; PositionY = 0.0598;
      pm = new TPolyMarker(1, &PositionX, &PositionY);
      frame->GetListOfFunctions()->Add(pm);
      pm->SetMarkerStyle(20);
      pm->SetMarkerColor(kBlue);
      pm->SetMarkerSize(2.3);
    } else {
      Double_t PositionX = 72,  PositionY = 0.0598;
      TPolyMarker *pm = new TPolyMarker(1, &PositionX, &PositionY);
      frame->GetListOfFunctions()->Add(pm);
      pm->SetMarkerStyle(20);
      pm->SetMarkerColor(kRed);
      pm->SetMarkerSize(2.3);
    }
  }
  leg->Draw();
  delete [] FitFiles;
  cout << Summary.Data() << endl;
  //  DrawdEdx();
}
//________________________________________________________________________________
void Mu(const Char_t *select="x", const Char_t *name="mu", const Char_t *pattern = "") {
  Res(select,name,pattern);
}
//________________________________________________________________________________
void DrawFitP(const Char_t *pattern = "SecRow3C", const Char_t *plot="mu:rowSigned(y,x)", const Char_t *select = "i&&j", const Char_t *opt = "prof",
	      Int_t nx = 145, Double_t xmin = -72.5, Double_t xmax = 72.5, Double_t ymin = -0.5, Double_t ymax = 0.5) {
  TPRegexp Pattern(pattern);
  Int_t NF = 0;
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t nn = files->GetSize();
  if (! nn) return;
  TFile **FitFiles = new TFile *[nn];
  TIter next(files);
  TFile *f = 0;
  TString Select(select);
  while ( (f = (TFile *) next()) ) { 
    TString F(f->GetName());
    if (! F.Contains(Pattern)) continue;

    FitFiles[NF] = f; NF++;
  }
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (! c1 ) c1 = new TCanvas("c1","c1");
  c1->Clear();
  //  c1->SetTitle("Resolution versus Track Length");
  c1->SetGrid(); //x(9);
  //  c1->SetGridy(30);
  TH1F *frame = 0;
  frame = c1->DrawFrame(xmin,ymin,xmax,ymax);
  //    frame->SetTitle("Shift versus Track Length");
  //    frame->SetYTitle("Sift");
  //  frame->SetXTitle("Track Length (cm)                   ");
  TLegend *leg = new TLegend(0.25,0.6,0.9,0.9,"");
  Int_t c = 0;
  for (int i = 0; i<NF; i++) {
    c = i + 1;
    if (c == 10) c = 11;
    if (FitFiles[i]) { 
      FitFiles[i]->cd();
      cout << "File " << gDirectory->GetName() << endl;
      TNtuple *FitP = (TNtuple *) gDirectory->Get("FitP");
      if (! FitP) continue;
      if (FitP->GetEntriesFast() < 10) continue;
      TString Opt(opt);
      Opt += "same";
      TString Plot(plot);
      Plot += Form(" >> Hist(%i,%f,%f)",nx,xmin,xmax);
      FitP->Draw(Plot,select,Opt);
      TH1 *Hist = (TH1 *) gDirectory->Get("Hist");
      Hist->SetMarkerColor(c); Hist->SetMarkerStyle(20); Hist->SetLineColor(c);
      TString Title(gSystem->BaseName(FitFiles[i]->GetName()));
      Title.ReplaceAll(".root","");
      Title.ReplaceAll(pattern,"");
      Title.ReplaceAll("GF","");
      leg->AddEntry(Hist,Title);
    }
  }
  leg->Draw();
}
#if 0
//________________________________________________________________________________
void DrawdEdx() {
  //  _file0->cd();
  TString Name(gSystem->BaseName(gDirectory->GetName()));
  Name.ReplaceAll(".root","");
  TH2 *TdEdxF = (TH2 *) gDirectory->Get("TdEdxF");
  TH2 *TdEdxI70 = (TH2 *) gDirectory->Get("TdEdxI70");
  TH2 *TdEdxN = (TH2 *) gDirectory->Get("TdEdxN");
  if (! TdEdxF || ! TdEdxI70 || ! TdEdxN) return;
#if 0
  TString Res("Res_");
  Res += Name;
  Res += ".png";
  //  TVirtualX::Instance()->WritePixmap(c1->GetCanvasID(),-1,-1,(Char_t *)Res.Data());
  c1->SaveAs(Res);
  cout << "Draw #\t" << Res << endl;
#endif
  Double_t w = 800;
  Double_t h = 3*500;
  TCanvas *c2 = new TCanvas(Name,Name,w,h);
  c2->Divide(1,3);
  c2->cd(1)->SetLogz(1);
  TdEdxI70->Draw("colz");
  gROOT->LoadMacro("bichselG10.C");
  bichselG10("I70");
  c2->cd(2)->SetLogz(1);
  if (TdEdxF) {
    TdEdxF->Draw("colz");
    bichselG10("z");
  }
  c2->cd(3)->SetLogz(1);
  TdEdxN->Draw("colz");
  bichselG10("dNdx");
  //  TVirtualX::Instance()->WritePixmap(c2->GetCanvasID(),-1,-1,(Char_t *)pngName.Data());
  TString pngName(Name);
  pngName += ".png";
  c2->SaveAs(pngName);
  cout << "Draw #\t" << pngName << endl;
}
/* ________________________________________________________________________________
   cd  ~/work/Histograms/RunXIX/dEdx928
   foreach p (` ls -qd  [0-9]*GeV*.root dEdx*.root`)
     echo ${p};
     root.exe ${p} TPoints70GP${p} TPointsFGP${p} TPointsNGP${p} Res.C
   end
 */
#endif
