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
#include "TCanvas.h"
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
#endif
using namespace std;
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
void Res(const Char_t *select="x") {
  //  const Char_t *FitNames[3] = {"Fit","I70","I60"};
  Int_t NF = 0;
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t nn = files->GetSize();
  if (! nn) return;
  TFile **FitFiles = new TFile *[nn];
  TIter next(files);
  TFile *f = 0;
  TString Select(select);
  Int_t all = 0;
  if (Select.Contains("all",TString::kIgnoreCase)) all = 1;
  while ( (f = (TFile *) next()) ) { 
    TString F(f->GetName());
    if (! F.Contains("TPoints") && ! F.Contains("MPoints")) continue;
    if (! all && (F.Contains("MPoints") || F.Contains("BUGP") || F.Contains("BAGP"))) continue;
    Int_t indx = 0;
    if ( F.Contains("70")) indx = 1;
    FitFiles[NF] = f; NF++;
  }
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (! c1 ) c1 = new TCanvas("c1","Resolution versus Track Length");
  c1->Clear();
  c1->SetTitle("Resolution versus Track Length");
  c1->SetGrid(); //x(9);
  //  c1->SetGridy(30);
  TH1F *frame = c1->DrawFrame(10,0.04,160,0.20);
  frame->SetTitle("Resolution versus Track Length");
  frame->SetXTitle("Track Length (cm)                   ");
  frame->SetYTitle("Resolution");
  TLegend *leg = new TLegend(0.25,0.6,0.9,0.9,"");
  Int_t c = 0;
  for (int i = 0; i<NF; i++) {
    c = i + 1;
    if (c == 10) c = 11;
    if (FitFiles[i]) { 
      FitFiles[i]->cd();
      TH1 *Sigma = 0;
      TH1 *sigma = (TH1 *) FitFiles[i]->Get("sigma");
      if (! sigma) continue;
      if (sigma->GetDimension() == 2) {
	Sigma = ((TH2 *) sigma)->ProjectionX(Form("Sigma_%i",i),0,0);
      } else {
	Sigma = sigma; 
      }
      Sigma->SetMarkerColor(c); Sigma->SetMarkerStyle(20); Sigma->SetLineColor(c);
      TString Title(gSystem->BaseName(FitFiles[i]->GetName()));
      Title.ReplaceAll(".root","");
      if (! Title.Contains("TPoints70") && ! Title.Contains("TPoints")) continue;
      if (!Sigma || Sigma->GetEntries() < 1.) {
	TTree *FitP = (TTree *) gDirectory->Get("FitP");
	if (FitP) FitP->Draw("sigma:x>>Sigma","sigma<0.2&&j==0","profgoff");
	Sigma = (TProfile *) FitFiles[i]->Get("Sigma");
      }
      if (! Sigma) continue;
      if (Sigma->GetEntries() < 1.) continue;
      Sigma->Rebin();
      Sigma->Scale(0.5);
      TF1 *powfit = new TF1("powfit","[0]*TMath::Power(x,[1])",40,160);
      powfit->SetParameters(0.5,-0.5);
      powfit->SetLineColor(c);
      Sigma->Fit("powfit","rn");
      powfit->Draw("same");
      //      Sigma->Fit("powfit","r");
      Sigma->Draw("same");
      //      leg->AddEntry(sigma,Form("%s:  #sigma(@76cm) = %5.2f%%",FitNames[i],100*powfit->Eval(76)));
      Title.ReplaceAll("GPHist","");
      Title.ReplaceAll("TPoints70BE","I70  East");
      Title.ReplaceAll("TPointsBE"  ,"Ifit East  ");
      Title.ReplaceAll("TPoints70BW","I70  West ");
      Title.ReplaceAll("TPointsBW"  ,"Ifit West ");
      Title.ReplaceAll("TPoints70B", "I70  ");
      Title.ReplaceAll("TPoints70U", "U70  ");
      Title.ReplaceAll("TPoints70" , "I70  ");
      Title.ReplaceAll("TPointsN"  , "Nfit ");
      Title.ReplaceAll("TPointsNU" , "NfitU");
      Title.ReplaceAll("TPointsB"  , "Ifit ");
      Title.ReplaceAll("TPointsU"  , "Ufit ");
      Title.ReplaceAll("TPointsF"  , "Ifit ");
      Title.ReplaceAll("TPoints"   , "Ifit ");
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
      //      if (Title.Contains("602P03ih"))  Title.ReplaceAll("602P03ih"," default ");
      Title += Form(" : #sigma(@76cm) = %5.2f%\%",100*powfit->Eval(76));
      //      Title += Form(" : #sigma(@128cm) = %5.2f%\%",100*powfit->Eval(128));
      Title.Strip();
      cout << Title << endl;
      leg->AddEntry(Sigma,Title.Data());
    }
  }
  Double_t PositionX = 76.2, PositionY = 0.076;
  TPolyMarker *pm = new TPolyMarker(1, &PositionX, &PositionY);
  frame->GetListOfFunctions()->Add(pm);
  pm->SetMarkerStyle(20);
  pm->SetMarkerColor(kRed);
  pm->SetMarkerSize(2.3);
  PositionX = 62.4; PositionY = 0.091;
//   pm = new TPolyMarker(1, &PositionX, &PositionY);
//   frame->GetListOfFunctions()->Add(pm);
//   pm->SetMarkerStyle(20);
//   pm->SetMarkerColor(kBlue);
//   pm->SetMarkerSize(2.3);
  leg->Draw();
  delete [] FitFiles;
}
//________________________________________________________________________________
void Mu() {
  Int_t NF = 0;
  TList *files = (TList *) gROOT->GetListOfFiles();
  Int_t nn = files->GetSize();
  if (! nn) return;
  TFile **FitFiles = new TFile *[nn];
  if (! files) return;
  TIter next(files);
  TFile *f = 0;
  while ( (f = (TFile *) next()) ) { 
    TString F(f->GetName());
    if (! F.Contains("TPoints") && ! F.Contains("MPoints")) continue;
    //    if (! F.Contains("BGP") && ! F.Contains("BUGP") && ! F.Contains("BAGP")) continue;
    if (! F.Contains("GP")) continue;
    FitFiles[NF] = f; 
    cout << "Found file[" << NF << "] = " << FitFiles[NF]->GetName() << endl;
    NF++;
  }
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (! c1 ) c1 = new TCanvas("c1","Resolution versus Track Length");
  c1->Clear();
  c1->SetTitle("Shift versus Track Length");
  c1->SetGrid(); //x(9);
  //  c1->SetGridy(30);
  TH1F *frame = c1->DrawFrame(10,-0.05,100,0.05);
  frame->SetTitle("Shift versus Track Length");
  frame->SetXTitle("Track Length (cm)                   ");
  frame->SetYTitle("Resolution");
  TLegend *leg = new TLegend(0.25,0.6,0.9,0.9,"");
  for (int i = 0; i < NF; i++) {
    if (FitFiles[i]) { 
      FitFiles[i]->cd();
      TH1 *Mu = 0;
      TH2 *mu = (TH2 *) FitFiles[i]->Get("mu");
      if (mu) {
	if (mu->GetDimension() == 2) {
	  Mu = ((TH2 *) mu)->ProjectionX("Mu",0,0);
	} else Mu = mu;
      }
      if (! Mu) continue;
      Int_t c = i + 1;
      if (c == 10) c = 11;
      Mu->SetMarkerColor(c); // Set(c);
      //      if (Mu->GetEntries() < 1.) continue;
      TF1 *powfit = new TF1("powfit","[0]",40,80);
      powfit->SetParameters(0.5,-0.5);
      powfit->SetLineColor(c);
      Mu->Fit("powfit","rn");
      powfit->Draw("same");
      //      Mu->Fit("powfit","r");
      Mu->Draw("same");
      //      leg->AddEntry(mu,Form("%s:  #mu(@76cm) = %5.2f%%",FitNames[i],100*powfit->Eval(76)));
      TString Title(gSystem->BaseName(FitFiles[i]->GetName()));
      Title.ReplaceAll(".root","");
      Title.ReplaceAll("Hist","");
      Title.ReplaceAll("TPoints70BGP","I70   ");
      Title.ReplaceAll("TPoints70BUGP","Uncorrected I70   ");
      Title.ReplaceAll("TPointsBGP"  ,"Ifit  ");
      Title.ReplaceAll("TPointsNGP"  ,"Nfit  ");
      Title.ReplaceAll("TPointsBUGP" ,"Uncorrected Ifit ");
      Title.ReplaceAll("MPoints70BGP","M70   ");
      Title.ReplaceAll("MPointsBGP"  ,"Mfit  ");
      Title.ReplaceAll("MPointsBUGP" ,"MUncorr");
      Title += Form(": #mu = %5.2f%\%",100*powfit->Eval(76));
      Title.Strip();
      cout << Title << endl;
      leg->AddEntry(Mu,Title.Data());
    }
  }
  leg->Draw();
  delete [] FitFiles;
}
