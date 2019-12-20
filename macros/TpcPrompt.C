/* 
   root.exe -q -b TpcPrompt.C >& TpcPrompt.log &
   root.exe -q -b 'Chain.C+("../*.root","TpcHit")' 'TpcPrompt.C+(tChain)' >& TpcPrompt.log &

   Fit
   root.exe -q -b TpcHit.root TpcPrompt.C+
Draw();
 ln -s ~/macros/.sl* .
 root.exe -q -b lBichsel.C TpcHitZTMfl0.root  'dEdxFit.C+("Z","GP","R",-1,-1,1,1,10,1,206,212)' >& Z.log &
 root.exe -q -b lBichsel.C TpcHitZTMfl0.root  'dEdxFit.C+("ZL","GP","R",-1,-1,1,1,10,1,0,3.0)' >& ZL.log &
 root.exe -q -b lBichsel.C TpcHitZTMfl0.root  'dEdxFit.C+("T","GP","R",-1,-1,1,1,10,1,0,12.0)' >& T.log &
 root.exe -q -b lBichsel.C TpcHitZTMfl0.root  'dEdxFit.C+("ZLM","Freq","R",-1,-1,1,1,10,1,200,220.0)' >& TFreq.log &
 root.exe -q -b lBichsel.C TpcHitZTMfl0.root  'dEdxFit.C+("Time","GP","R",-1,-1,1,1,10,1,-0.2,0.6)' >& Time.log &
 root.exe -q -b lBichsel.C TpcHitZTMfl0.root  'dEdxFit.C+("TimeB","GP","R",-1,-1,1,1,10,1,-2,6)' >& TimeB.log &
 root.exe -q -b lBichsel.C TpcHitZTMfl0.root  'dEdxFit.C+("TimeM","Freq","R",-1,-1,1,1,10,1,37,39)' >& TimeM.log &
 root.exe -q -b lBichsel.C TpcHitZTMfl0.root  'dEdxFit.C+("TimeMB","Freq","R",-1,-1,1,1,10,1,340,370)' >& TimeMB.log &
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
#if !defined(__CINT__)
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
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,34,18)
//#define __USE_ROOFIT__
#endif
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
#endif
#define TpcHit_cxx
#include "TpcHit.h"
void  TpcHit::Fill(Long64_t entry) {
  static TH3F *hist3DZ = 0, *hist3DT = 0, *hist3DZL = 0;
  static TH3F *hist3DMZ = 0, *hist3DMT = 0, *hist3DMZL = 0;
  static TH3F *histTime = 0, *histTimeB = 0;
  static TH3F *histTimeM = 0, *histTimeBM = 0;
  if (! hist3DZ) {
    TDirectory *old = gDirectory;
    TString newF("TpcHitZTMfl0.root");
    fOut = new TFile(newF,"recreate");
    hist3DZ  = new TH3F("Z","|z| versus sector and row",24,0.5,24.5,72,0.5,72.5,260,200,213);
    hist3DZL = new TH3F("ZL","Drift distance sector local versus sector and row",24,0.5,24.5,72,0.5,72.5,500,-10,10);
    hist3DT  = new TH3F("T","time bucket versus sector and row",24,0.5,24.5,72,0.5,72.5,500,0,20);
    hist3DMZ  = new TH3F("ZM","Membrane |z| versus sector and row",24,0.5,24.5,72,0.5,72.5,400,-10,10);
    hist3DMZL = new TH3F("ZLM","Membrane Drift distance sector local versus sector and row",24,0.5,24.5,72,0.5,72.5,400,200,220);
    hist3DMT  = new TH3F("TM","Membrane time bucket versus sector and row",24,0.5,24.5,72,0.5,72.5,400,320,360);
    histTime = new  TH3F("Time","Prompt time (usec) versus sector and row",24,0.5,24.5,72,0.5,72.5,400,-1,1);
    histTimeB = new  TH3F("TimeB","Prompt time (backets) versus sector and row",24,0.5,24.5,72,0.5,72.5,400,-10,10);
    histTimeM = new  TH3F("TimeM","Membrane time (usec) versus sector and row",24,0.5,24.5,72,0.5,72.5,400,30,40);
    histTimeBM = new  TH3F("TimeBM","Membrane time (backets) versus sector and row",24,0.5,24.5,72,0.5,72.5,400,300,400);
    gDirectory = old;
  }
  if (! fl) {
    hist3DZ->Fill(sector,row,TMath::Abs(z));
    hist3DZL->Fill(sector,row,zL);
    hist3DT->Fill(sector,row,timebucket);
    hist3DMZ->Fill(sector,row,z);
    hist3DMZL->Fill(sector,row,zL);
    hist3DMT->Fill(sector,row,timebucket);
    histTime->Fill(sector,row,time);
    histTimeB->Fill(sector,row,timeb);
    histTimeM->Fill(sector,row,time);
    histTimeBM->Fill(sector,row,timeb);
  }
}
//________________________________________________________________________________
void TpcHit::Loop() {
  //   In a ROOT session, you can do:
  //      Root > .L TpcHit.C
  //      Root > TpcHit t
  //      Root > t.GetEntry(12); // Fill t data members with entry number 12
  //      Root > t.Show();       // Show values of entry 12
  //      Root > t.Show(16);     // Read and show values of entry 16
  //      Root > t.Loop();       // Loop on all entries
  //
  
  //     This is the loop skeleton where:
  //    jentry is the global entry number in the chain
  //    ientry is the entry number in the current Tree
  //  Note that the argument to GetEntry must be:
  //    jentry for TChain::GetEntry
  //    ientry for TTree::GetEntry and TBranch::GetEntry
  //
  //       To read only selected branches, Insert statements like:
  // METHOD1:
  //    fChain->SetBranchStatus("*",0);  // disable all branches
  //    fChain->SetBranchStatus("branchname",1);  // activate branchname
  // METHOD2: replace line
  //    fChain->GetEntry(jentry);       //read all branches
  //by  b_branchname->GetEntry(ientry); //read only this branch
  if (fChain == 0) return;
  
  Long64_t nentries = fChain->GetEntriesFast();
  cout << "Total no. of events = " << nentries << endl;
  Long64_t nbytes = 0, nb = 0;
  for (Long64_t jentry=0; jentry<nentries;jentry++) {
    Long64_t ientry = LoadTree(jentry);
    if (ientry < 0) break;
    nb = fChain->GetEntry(jentry);   nbytes += nb;
    // if (Cut(ientry) < 0) continue;
    Fill(ientry);
    if (ientry%100000 == 0) { cout << "read event " << ientry << endl;}
  }
}
#ifdef __CINT__
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
class TpcHit;
#endif
#if !defined(__CINT__) || defined(__MAKECINT__)
//________________________________________________________________________________
void Draw(TChain *tree) {
  TpcHit hit(tree);
  hit.Loop();
}
//________________________________________________________________________________
void Draw(const Char_t *tag = "New") {
  TChain *tree = (TChain *) gDirectory->Get("TpcHit");
  if (! tree) {
    cout << "No TpcHit tree has been found" << endl;
    return;
  }
  Draw(tree);
  
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
#include "Gamma.C"
//________________________________________________________________________________
void T0Fit(TChain *tree) {
  TH2D *D = (TH2D *) gDirectory->Get("D");
  if (! D) {
    if (! tree) {
      tree = (TChain *) gDirectory->Get("TpcHit");
    }
    if (! tree) return;
    TString Out(gSystem->BaseName(gDirectory->GetName()));
    Out.ReplaceAll(".root",".Fit.root");
    TFile *fOut = new TFile(Out,"recreate");
    TH2D *D = new TH2D("D","z of Cluster versus row",91,-45.5,45.5,300,0.5,15.5);
    tree->Draw("timebucket:(sector <= 12) ? row : -row >> D","trigId==0","colz");
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
    if (ix != 20) f->SetParameter(1,  3.);
    else          f->SetParameter(1, 10.);
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
    f->ReleaseParameter(1); //      f->SetParLimits(1,2,26);
    f->ReleaseParameter(2);         f->SetParLimits(2,0.1,2);
    f->ReleaseParameter(3);         f->SetParLimits(3,2,50);
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
//________________________________________________________________________________
void T0Fit(TH3F *D = 0, Int_t iX = 0, Int_t iY = 0) {
  if (! D) return;
  TString Out(D->GetDirectory()->GetName());
  Out.ReplaceAll(".root","");
  Out += D->GetName();
  if (iX) {Out += "_x"; Out += iX;}
  if (iY) {Out += "_y"; Out += iY;}
  Double_t sign = 1;
  //  if (TString(D->GetName()).BeginsWith("Z")) sign = -1;
  
  Out += ".root";
  TFile *fOut = new TFile(Out,"recreate");
  struct BPoint_t {
    Float_t sector, row, norm, mu, sigma, gamma, grass,dnorm, sign, dmu, dsigma, dgamma, dgrass, dsign, chisq;
  };
  TNtuple *FitP = new TNtuple("FitP","FitP","sector:row:norm:mu:sigma:gamma:grass:dnorm:sign:dmu:dsigma:dgamma:dgrass:dsign:chisq");
  BPoint_t B;
  TF1 *f = GMP();
  Double_t params[6];
  f->GetParameters(params);
  Int_t np = f->GetNpar();
  Int_t nx = D->GetXaxis()->GetNbins();
  Int_t ny = D->GetYaxis()->GetNbins();
  Int_t nz = D->GetZaxis()->GetNbins();
  Double_t xmin = D->GetXaxis()->GetXmin();
  Double_t xmax = D->GetXaxis()->GetXmax();
  Double_t ymin = D->GetYaxis()->GetXmin();
  Double_t ymax = D->GetYaxis()->GetXmax();
  TCanvas *c1 = new TCanvas();
  Int_t ix1 = 1; Int_t ix2 = nx;
  Int_t iy1 = 1; Int_t iy2 = ny;
  if (iX > 0 && iX <= nx) {ix1 = ix2 = iX;}
  if (iY > 0 && iY <= ny) {iy1 = iy2 = iY;}
  for (Int_t ix = ix1; ix <= ix2; ix++) {
    D->GetXaxis()->SetRange(ix,ix);
    for (Int_t iy = iy1; iy <= iy2; iy++) {
      D->GetYaxis()->SetRange(iy,iy);
      TH1 *proj = D->Project3D(Form("z_%i_%i",ix,iy));
      if (proj->GetEntries() < 100) continue;
      cout << "Fit: " << proj->GetName() << endl;
      Int_t bin = proj->GetMaximumBin();
      params[1] = proj->GetXaxis()->GetBinCenter(bin);
      f->FixParameter(0,params[0]);
      f->FixParameter(1,params[1]);
      f->FixParameter(2,params[2]);
      f->FixParameter(3,params[3]);
      f->FixParameter(5,sign);
      f->ReleaseParameter(4);         f->SetParLimits(4,0,1e3);
      Int_t res = proj->Fit(f,"er","",params[1]-20,params[1]+20);
      //    if (res) continue;
      //      c1->Modified();
      //      c1->Update();
      params[4] = f->GetParameter(4);
      f->FixParameter(4,params[4]);
      f->ReleaseParameter(0);         f->SetParLimits(0,0,50);
      f->ReleaseParameter(1);         f->SetParLimits(1,params[1]-20,params[1]+20);
      f->ReleaseParameter(2);         f->SetParLimits(2,0.1,2);
      f->ReleaseParameter(3);         f->SetParLimits(3,2,50);
      //      res = proj->Fit(f,"erm","",0.5,5.5);
      res = proj->Fit(f,"em");
      //    if (res) continue;
      //      c1->Modified();
      //      c1->Update();
      f->ReleaseParameter(4);         f->SetParLimits(4,0,1e3);
      res = proj->Fit(f,"erm","",params[1]-20,params[1]+20);
      //    if (res) continue;
      c1->Modified();
      c1->Update();
      B.sector = ix;
      B.row    = iy;
      B.chisq = f->GetChisquare();
      Float_t *xx = &B.norm;
      for (Int_t p = 0; p < np; p++) {
	xx[p]    = f->GetParameter(p);
	xx[np+p] = f->GetParError(p);
      }
      FitP->Fill(&B.sector);
    }
  }
#if 0
  TH2D *mu = hist[1];
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
#endif
  fOut->Write();
}
//________________________________________________________________________________
void TpcPrompt(const Char_t *chainN = "TpcHit") {
  TChain *chain = (TChain *) gDirectory->Get(chainN);
  if (! chain) return;
  Draw(chain);
}
//________________________________________________________________________________
void TpcPrompt(TChain *chain) {
  if (! chain) return;
  Draw(chain);
}
#else /* __CINT__ */
//________________________________________________________________________________
void TpcPrompt(Int_t Nevents = 1000000, 
	       //	       const Char_t *daqfile = "/star/data03/daq/2014/100/15100085/st_physics_15100085_raw_2500013.daq",
	       const Char_t *daqfile = "./*.*event.root",
	       const Char_t *treefile = "TpcHit.root") {
  gROOT->LoadMacro("bfc.C");

  //  TString Chain("in,StEvent,tpcDb,analysis,magF,NoDefault,tpcHitMover,OSpaceZ2,OGridLeakFull,Corr4,mysql");
  //  TString Chain("in,StEvent,trgD,tpcDb,analysis,magF,NoDefault,mysql");
  //  TString Chain("in,TpcHitMover,StEvent,tpcDb,detDb,CorrX,OSpaceZ2,OGridLeakFull,quiet,analysis,mysql,NoDefault");
  TString Chain("in");
  if (TString(daqfile).Contains("daq")) {
    Chain += ",tpx";
  }
  Chain += ",TpcHitMover,StEvent,tpcDb,detDb,CorrY,OSpaceZ2,OGridLeakFull,quiet,analysis,mysql,NoDefault";
  //  TString Chain("in,StEvent,tpcDb,analysis,magF,NoDefault,tpcHitMover,OSpaceZ2,OGridLeakFull,CorrX");
  TString TreeFile(treefile);
  if (TreeFile == "") {
    TreeFile = gSystem->BaseName(daqfile);
    TreeFile.ReplaceAll("*","_");
    TreeFile.ReplaceAll(".daq",".TpcHit.root");
    TreeFile.ReplaceAll(".event.root",".TpcHit.root");
  }
  chain = bfc(0,Chain.Data(),daqfile,0,TreeFile.Data());
  for (Int_t ev = 0; ev < Nevents; ev++) {
    Int_t iMake = chain->MakeEvent();
    if (iMake%10 == kStEOF || iMake%10==kStFatal)	break;
    StAnalysisMaker::PrintTpcHits(0,0,2);
    //    StAnalysisMaker::PrintTpcHits(0,0,1);
  }
}
#endif 
/* 
   foreach f (`ls -1d *event.root `)
      set run = `echo $f | awk -F\_ '{print $3}'`;
      root.exe -q -b 'TpcPrompt.C(100000,"'${f}'","TpcHit'${run}'.root")' >& ${run}.log &
   end
--------------------------------------------------------------------------------
Membrane:  ZLMFreqTpcHitZTMfl0.root
1. match Inner and Outer: 
FitP->Draw("(mu-208.707):y>>Z(72,0.5,72.5)","i&&j","prof")
Z->Fit("pol2","er","",0.5,40.5)
pol2->Eval(40.5);// => -1.42279472751408842e-01
Z->Fit("pol2","er","",40.5,72.5);
pol2->Eval(40.5);// => -4.34622712046779991e-01
FitP->Draw("(mu-208.707)-(-1.42279472751408842e-01):y>>ZI(72,0.5,72.5)","i&&j&&j<=40","prof")
FitP->Draw("(mu-208.707)-(-4.34622712046779991e-01):y>>ZO(72,0.5,72.5)","i&&j&&j>40","prof")
TProfile *z = new TProfile(*ZI)
z->Add(ZO)

FitP->Draw("((mu-208.707)-(-1.42279472751408842e-01))/208.707:y>>zI(72,0.5,72.5)","i&&j&&j<=40","prof")
FitP->Draw("((mu-208.707)-(-4.34622712046779991e-01))/208.07:y>>zO(72,0.5,72.5)","i&&j&&j>40","prof")
TProfile *z = new TProfile(*zI)
z->Add(zO)
z->Fit("pol2","e")
 */
