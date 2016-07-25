#if !defined(__CINT__)
// code that should be seen ONLY by the compiler
#else
#if !defined(__CINT__) || defined(__MAKECINT__)
// code that should be seen by the compiler AND rootcint
#else
// code that should always be seen
#endif
#endif
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,34,18)
//#define __USE_ROOFIT__
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
#include "TFile.h"
#include "TNtuple.h"
#include "TCanvas.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "TDataSet.h"
#include "TClassTable.h"
//#include "DeDxTree.C"
#include "TMinuit.h"
#include "TVirtualFitter.h"
#include "TSpectrum.h"
#include "StBichsel/Bichsel.h"
#include "TString.h"
#include "TLine.h"
#include "TText.h"
#include "TList.h"
#include "TPolyMarker.h"
#include "TKey.h"
#include "TLegend.h"
#ifdef __USE_ROOFIT__
#include "RooRealVar.h"
#include "RooDataSet.h"
#include "RooGaussian.h"
#include "RooFFTConvPdf.h"
#include "RooPlot.h"
#include "RooCFunction1Binding.h" 
#include "RooCFunction3Binding.h"
#include "RooTFnBinding.h" 
#include "RooDataHist.h"
#include "RooAbsPdf.h"
#include "RooRealProxy.h"
#include "RooFit.h"
#include "RooRandom.h"
#include "RooFitResult.h"
#include "RooWorkspace.h"
using namespace RooFit ;
#endif /* __USE_ROOFIT__ */
#include "TObjectTable.h"
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
// Refer to a class implemented in libRooFit to force its loading
// via the autoloader.
#ifdef __USE_ROOFIT__
class Roo2DKeysPdf;
#endif /* __USE_ROOFIT__ */
#endif
//________________________________________________________________________________
void Draw(const Char_t *tag = "New") {
  TNtuple *TpcHit = (TNtuple *) gDirectory->Get("TpcHit");
  if (! TpcHit) return;
  const Char_t *Sides[2] = {"W","E"};
  const Char_t *Cuts[2]  = {"sector<=12","sector>12"};
  for (Int_t i = 0; i < 2; i++) {
    TpcHit->Draw(Form("abs(z):row>>%sR%s(45,0.5,45.5,60,208,213)",tag,Sides[i]),Cuts[i],"colz");
    TH2 *R = (TH2 *) gDirectory->Get(Form("%sR%s",tag,Sides[i]));
    if (R) {
      R->FitSlicesY();
      TH1 *R1 = (TH1 *) gDirectory->Get(Form("%s_1",R->GetName()));
      if (R1) R1->SetMarkerColor(i+1);
    }
  }
  
}
//________________________________________________________________________________
void ClusterSize(const Char_t *f0 = "AuAu200AltroDef/st_physics_adc_15152001_raw_1000013.TpcHit.root",
		 const Char_t *f1 = "AuAu200Altro4/st_physics_adc_15152001_raw_1000013.TpcHit.root") {
  const Int_t  AltroCut[2] = {3,4};
  const Char_t *IO[3] = {"I","O","IO"};
  const Char_t *Cuts[3] = {"row<=13","row>13",""};
  TFile *files[2] = {TFile::Open(f0), TFile::Open(f1)};
  if (! files[0] || ! files[1]) {
    cout << "Can't open file " << f0 << " or " << f1 << endl;
    return;
  } else {
    for (UInt_t f = 0; f < 2; f++) {
      cout << "Opened file " << files[f]->GetName() << " for Altro Cut " << AltroCut[f] << endl;
    }
  }
  TCanvas *c1 = new TCanvas("padtime","pad versus time buckets");
  c1->Divide(3,4);
  //        io f
  TH2 *hist[3][2] = {{ 0, 0}, {0, 0}, {0, 0}};
  UInt_t k;
  TLegend *lx[3], *ly[3];
  for (k = 0; k < 3; k++) {
    lx[k] = new TLegend(0.6,0.6,0.8,0.8);
    ly[k] = new TLegend(0.6,0.6,0.8,0.8);
  }
  for (UInt_t f = 2; f--;) {
    files[f]->cd();
    TNtuple *TpcHit = (TNtuple *) gDirectory->Get("TpcHit");
    if (! TpcHit) {
      cout << "Missing TpcHit in file " << gDirectory->GetName() << endl;
      return;
    }
    for (k = 0; k < 3; k++) {
      Int_t i = f;
      Int_t kf = 3*i + k + 1;
      c1->cd(kf)->SetLogz();
      TpcHit->Draw(Form("npads:ntbks>>A%i%s(40,0.5,40.5,20,0.5,20.5)",AltroCut[f],IO[k]),Cuts[k],"colz");
      hist[k][f] = (TH2 *) gDirectory->Get(Form("A%i%s",AltroCut[f],IO[k]));
      if (! hist[k][f]) {
	cout << "Cannot find histogram " << Form("A%i%s",AltroCut[f],IO[k]) << " in file " << gDirectory->GetName() << endl;
	continue;
      }
      hist[k][f]->SetXTitle("no. of time buckets");
      hist[k][f]->SetYTitle("no. of pads");
      TH1 *projx = 	hist[k][f]->ProjectionX();
      projx->SetNormFactor(1.);
      projx->SetLineColor(f+1);
      i = 2;
      kf = 3*i + k + 1;
      c1->cd(kf);
      lx[k]->AddEntry(projx,Form("Altro Cut = %i",AltroCut[f]));
      if ( f) {projx->Draw(); projx->SetXTitle("no. of time buckets");}
      else     {projx->Draw("sames"); lx[k]->Draw();}
      TH1 *projy = 	hist[k][f]->ProjectionY();
      projy->SetNormFactor(1.);
      projy->SetLineColor(f+1);
      i = 3;
      kf = 3*i + k + 1;
      c1->cd(kf);
      ly[k]->AddEntry(projy,Form("Altro Cut = %i",AltroCut[f]));
      if ( f) {projy->Draw(); projy->SetXTitle("no. of pads");}
      else    {projy->Draw("sames"); ly[k]->Draw();}
    }
  }
}
/*
  TpcHit->Draw("timebucket:(sector <= 12) ? row : -row >> D(100,-45.5,54.5,100,0.5,5.5)","trigId==0","colz");
*/
//________________________________________________________________________________
Double_t gmp(Double_t *x, Double_t *p) {
  Double_t normL = p[0];
  Double_t nu    = p[1];
  Double_t sigma = p[2];
  Double_t gamma = p[3];
  Double_t grass = p[4];
  Double_t val   = grass;
  if (sigma > 0 && gamma > 1) {
    Double_t t = (x[0] - nu)/sigma + (gamma - 1);
    if (t > 0) { 
      val += TMath::Exp(normL)*TMath::GammaDist(t,gamma,0.,1.);
    }
  }
  return val;
}
//________________________________________________________________________________
TF1 *GMP() {
  TF1 * f = new TF1("GMP",gmp,0.5,15.5,5);
  f->SetParNames("normL", "nu", "sigma", "gamma", "grass");
  f->SetParameters(4.17880, 2.86452, 0.2, 6.0, 2.1);
  f->SetParLimits(0,0,50);
  f->SetParLimits(1,2,6);
  f->SetParLimits(2,0.1,2);
  f->SetParLimits(3,5,20);
  f->SetParLimits(4,0,1e3);
  return f;
}
//________________________________________________________________________________
void T0Fit(TChain *TpcHit = 0) {
  TH2D *D = (TH2D *) gDirectory->Get("D");
  if (! D) {
    if (! TpcHit) {
      TpcHit = (TChain *) gDirectory->Get("TpcHit");
    }
    if (! TpcHit) return;
    TString Out(gSystem->BaseName(gDirectory->GetName()));
    Out.ReplaceAll(".root",".Fit.root");
    TFile *fOut = new TFile(Out,"recreate");
    TH2D *D = new TH2D("D","z of Cluster versus row",91,-45.5,45.5,300,0.5,15.5);
    TpcHit->Draw("timebucket:(sector <= 12) ? row : -row >> D","trigId==0","colz");
    fOut->Write();
  }
  TString Out(gSystem->BaseName(gDirectory->GetName()));
  delete gDirectory;
  TFile *fOut = new TFile(Out,"update");
  D = (TH2D *) gDirectory->Get("D");
  if (! D) return;
  //  TVirtualFitter::SetDefaultFitter("Fumili");
  //  TVirtualFitter::SetDefaultFitter("Minuit2");
  TF1 *f = GMP();
  Double_t params[6];
  f->GetParameters(params);
  Int_t np = f->GetNpar();
  Int_t nx = D->GetXaxis()->GetNbins();
  Int_t ny = D->GetXaxis()->GetNbins();
  Double_t xmin = D->GetXaxis()->GetXmin();
  Double_t xmax = D->GetXaxis()->GetXmax();
  TH1D **hist = new TH1D*[np];
  for (Int_t p = 0; p <= np; p++) {
    TString name("chisq");
    if (p < np) name = f->GetParName(p);
    hist[p] = new TH1D(Form("%s_%s",D->GetName(),name.Data()),D->GetTitle(),nx,xmin,xmax);
  }
  TCanvas *c1 = new TCanvas();
  for (Int_t ix = 1; ix <= nx; ix++) {
    TH1D *proj = D->ProjectionY(Form("%s_%i",D->GetName(),ix),ix,ix);
    if (proj->GetEntries() < 100) continue;
    cout << "Fit: " << proj->GetName() << endl;
    f->FixParameter(0,params[0]);
    f->FixParameter(1,params[1]);
    f->FixParameter(2,params[2]);
    f->FixParameter(3,params[3]);
    f->ReleaseParameter(4);         f->SetParLimits(4,0,1e3);
    Int_t res = proj->Fit(f,"er","",5.5,15.5);
    //    if (res) continue;
    c1->Modified();
    c1->Update();
    params[4] = f->GetParameter(4);
    f->FixParameter(4,params[4]);
    f->ReleaseParameter(0);         f->SetParLimits(0,0,50);
    f->ReleaseParameter(1);         f->SetParLimits(1,2,6);
    f->ReleaseParameter(2);         f->SetParLimits(2,0.1,2);
    f->ReleaseParameter(3);         f->SetParLimits(3,5,20);
    res = proj->Fit(f,"erm","",0.5,5.5);
    //    if (res) continue;
    c1->Modified();
    c1->Update();
    f->ReleaseParameter(4);         f->SetParLimits(4,0,1e3);
    res = proj->Fit(f,"erm","",0.5,15.5);
    //    if (res) continue;
    c1->Modified();
    c1->Update();
    hist[np]->SetBinContent(ix,f->GetChisquare());
    for (Int_t p = 0; p < np; p++) {
      hist[p]->SetBinContent(ix,f->GetParameter(p));
      hist[p]->SetBinError(ix,f->GetParError(p));
    }
  }
  TH1D *mu = hist[1];
  if (! mu) return;
  TF1 *pol0 = 0;
  Double_t xmx[6] = {-45.5, -13.5, -0.5, 0.5, 13.5, 45.5};
  Double_t t0[4], dt0[4], w[4];
  for (Int_t i = 0; i < 4; i++) {
    Int_t i1 = i;
    if (i >= 2) i1++;
    Int_t i2 = i1+1;
    mu->Fit("pol0","er+","",xmx[i1],xmx[i2]);
    pol0 = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol0"); 
    t0[i] = pol0->GetParameter(0);
    dt0[i] = pol0->GetParError(0);
    w[i] = 1./(dt0[i]*dt0[i]);
    cout << gDirectory->GetName() << "\trange = [" << xmx[i1] << "," << xmx[i2] << "] t0 " << t0[i] << " +/- " << dt0[i] << endl;
  }
  Double_t ww = w[0] + w[3];
  Double_t t0O = (t0[0]*w[0] + t0[3]*w[3])/ww; 
  Double_t dt0O = 1./TMath::Sqrt(ww); 
  cout << gDirectory->GetName();
  cout << "\tOuter t0 = " << t0O << " +/- " << dt0O;
  ww = w[1] + w[2];
  Double_t t0I = (t0[1]*w[1] + t0[2]*w[2])/ww; 
  Double_t dt0I = 1./TMath::Sqrt(ww); 
  cout << "\tInner t0 = " << t0I << " +/- " << dt0I << endl;
}
#if defined(__CINT__) && !defined(__MAKECINT__)
//________________________________________________________________________________
void TpcPrompt(Int_t Nevents = 9999999, 
	       const Char_t *daqfile = "/star/data03/daq/2014/100/15100085/st_physics_15100085_raw_2500013.daq",
	       const Char_t *treefile = "") {
  gROOT->LoadMacro("bfc.C");

  //  TString Chain("in,StEvent,tpcDb,analysis,magF,NoDefault,tpcHitMover,OSpaceZ2,OGridLeak3D,Corr4,mysql");
  TString Chain("in,StEvent,trgD,tpcDb,analysis,magF,NoDefault,mysql");
  //  TString Chain("in,StEvent,tpcDb,analysis,magF,NoDefault,tpcHitMover,OSpaceZ2,OGridLeak3D,CorrX");
  if (TString(daqfile).EndsWith(".daq")) Chain += ",tpx,TpcHitMover,CorrX";
  TString TreeFile(treefile);
  if (TreeFile == "") {
    TreeFile = gSystem->BaseName(daqfile);
    TreeFile.ReplaceAll(".daq",".TpcHit.root");
    TreeFile.ReplaceAll(".event.root",".TpcHit.root");
  }
  chain = bfc(0,Chain.Data(),daqfile,0,TreeFile.Data());
  for (Int_t ev = 0; ev < Nevents; ev++) {
    Int_t iMake = chain->MakeEvent();
    if (iMake%10 == kStEOF || iMake%10==kStFatal)	break;
    //    StAnalysisMaker::PrintTpcHits(0,0,2);
    StAnalysisMaker::PrintTpcHits(0,0,1);
  }
}
#else
void TpcPrompt(Int_t Nevents= 0, const Char_t *daqfile = "", const Char_t *treefile = "") {
  T0Fit();
}
#endif 
