//#define TSPECTRA
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
#include "TString.h"
#include "TObjString.h"
#include "TLine.h"
#include "TText.h"
#include "TROOT.h"
#include "TList.h"
#include "TPolyMarker.h"
#include "TLegend.h"
#include "TArrayI.h"
#include "TArrayF.h"
#include "TArrayD.h"
#include "TRVector.h"
#include "TRMatrix.h"
//#include "TQtCanvas2Html.h"
#include "TInterpreter.h"
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
Int_t npeaks = 1;
static const Double_t nSigMax = 10;
TCanvas *c1, *c2;
TPad *selold = 0;
static const Double_t WaferLength = 2.9928;

/*
static const Char_t *plotName[] = {
  "duuH", "duvP", "dutuP", "duOvertuPuP", "duOvertuPvP",  
  "dvuP", "duvH", "dvtvP", "dvOvertvPuP", "dvOvertvPvP", // 10
};
*/
static const Char_t *plotName[] = {
  "duuH", "duvP", "dutuP", "duOvertuPuP", "duOvertuPvP",  
  "dvuP", "duvH", "dvtvP", "dvOvertvPuP", "dvOvertvPvP", // 10
};
const Int_t noHist = sizeof(plotName)/sizeof(Char_t *);//34;// 26;
const Int_t firstHL = 0;
const Int_t firstHG = 10;
//const Int_t firstHG = 9;
const Int_t firstHP = firstHG + 3;
Int_t firstH = firstHG;
Int_t lastH  = noHist - 1; 

struct Rot_t {
  Double_t dX, dY, dZ;
  Double_t alpha, beta, gamma;
  Char_t  *Comment;
};
static const Int_t NPOL = 8;
//________________________________________________________________________________
Double_t PolN(Double_t *x,Double_t *par) {
  Double_t sum = par[NPOL-1];
  for (Int_t i = NPOL-2; i >= 0; i--) sum = x[0]*sum + par[i];
  return sum;
}
//________________________________________________________________________________
Double_t Fit(TH1 *hist, Double_t xmin=-99, Double_t xmax=99, const Char_t *opt = "qer") {
  Double_t prob = 0;
  if (! hist) return prob;
  TF1 *f = (TF1 *) gROOT->GetFunction("PolN");
  if (! f) {
    f = new TF1("PolN",PolN,xmin,xmax,NPOL+1);
  }
  for (Int_t i = 0; i <= NPOL; i++) {if (i <=0) f->SetParameter(i,0.0); else f->FixParameter(i,0);}
  hist->Fit("PolN",opt,"",xmin,xmax);
  f->ReleaseParameter(1);
  hist->Fit("PolN",opt,"",xmin,xmax);
  f = hist->GetFunction("PolN");
  if (! f) return prob;
  f->GetRange(xmin,xmax);
  prob = f->GetProb();
  Int_t NfitP = f->GetNumberFitPoints();
  //  if (prob > 1e-3 || NfitP < 10) return prob;
  if (prob > 0 || NfitP < 10) return prob;
  for (Int_t k = 2; k <= NPOL; k++) {
    f->ReleaseParameter(k);
    hist->Fit("PolN",opt,"",xmin,xmax);
    f = hist->GetFunction("PolN");
    prob = f->GetProb();
    if (prob > 1e-3) break;
  }
  return prob;
}
//________________________________________________________________________________
void FitPolN(TH1 *hist, Double_t Xmin=-99, Double_t Xmax=99, const Char_t *opt = "qer") {
  Double_t xmin = Xmax;
  Double_t xmax = Xmin;
  const Int_t nxbin = hist->GetNbinsX();
  Int_t np = 0;
  Double_t x = 0;
  for (Int_t i = 1; i <= nxbin; i++) {
    Double_t err = hist->GetBinError(i);
    if (err <= 1e-6) {hist->SetBinError(i,0); continue;}
    np++;
    x = hist->GetBinLowEdge(i);
    if (xmin > x) xmin = x;
    x = hist->GetBinLowEdge(i+1);
    if (xmax < x) xmax = x;
  }
  if (np < 5) return;
  Double_t prob = Fit(hist,xmin,xmax,opt);
  TF1 *f = hist->GetFunction("PolN");
  Int_t NfitP = f->GetNumberFitPoints();
  if (NfitP < 5) return;
  Int_t NMaxCut = NfitP/2;
  Int_t NfitPCut = NMaxCut;
  if (NfitPCut < 15) NfitPCut = 15;
  Int_t NCut = 0;
  Double_t dev[1000];
  Int_t indx[1000];
  Int_t iter = 0;
  while (prob < 1e-3 || iter < 5) {
    f = hist->GetFunction("PolN");
    prob = f->GetProb();
    iter++;
    Double_t chisq = f->GetChisquare();
    NfitP = f->GetNumberFitPoints();
    if (prob >= 1e-3 || chisq < 0.0 || NfitP < NfitPCut || NCut > NMaxCut) break;
    Double_t devCut = 3*chisq/NfitP;
    //    cout << hist->GetName() << "\t min/max " << xmin << "/" << xmax;
    Int_t np = 0;
    for (Int_t i = 1; i <= nxbin; i++) {
      dev[i] = 0;
      Double_t err = hist->GetBinError(i);
      if (err <= 0.0) continue;
      Double_t val = hist->GetBinContent(i);
      dev[i] = (val - f->Eval(hist->GetBinCenter(i)))/err;
      dev[i] *= dev[i];
      np++;
    }
    TMath::Sort(nxbin, dev, indx, kTRUE); // decreasing order
    Int_t npp = (Int_t) (0.1*np) + 1;
    Int_t nskipped = 0;
    for (Int_t j = 0; j < npp; j++) {
      Int_t i = indx[j];
      if (dev[i] < devCut && nskipped > 0) break;
      hist->SetBinError(i,0);
      nskipped++;
      NCut++;
    }
    prob = Fit(hist,xmin,xmax,opt);
    if (prob > 1e-3) break;
  }
}
//________________________________________________________________________________
Double_t GetRMS(TH1 *h, Double_t x1, Double_t x2) {
  if (! h) return 0;
  TAxis *fXaxis = h->GetXaxis();
  Int_t i1 = h->FindBin(x1); if (i1 < fXaxis->GetFirst()) i1 = fXaxis->GetFirst();
  Int_t i2 = h->FindBin(x2); if (i2 > fXaxis->GetLast())  i2 = fXaxis->GetLast();
  Double_t w = 0, x = 0, rms = 0;
  Double_t sumW = 0; Double_t sumX = 0; Double_t sumX2 = 0;
  for (Int_t binx = i1; binx <= i2; binx++) {
    x = fXaxis->GetBinCenter(binx);
    w   = TMath::Abs(h->GetBinContent(binx));
    sumW += w;
    sumX += w*x;
    sumX2 += w*x*x;
  }
  if (sumW == 0.0) return 0;
  x = sumX/sumW;
  rms = TMath::Sqrt(sumX2/sumW - x*x);
  return rms;
}
//________________________________________________________________________________
Double_t g2g(Double_t *xx, Double_t *par) {
  Double_t x = xx[0];
  Double_t A = TMath::Exp(par[0]);
  Double_t mu1  = par[1];
  Double_t sig1 = par[2];
  if (A < 1 && TMath::Abs(x -mu1) < 3*sig1) {TF1::RejectPoint(); return 0;}
  Double_t B    = TMath::Exp(par[3]);
  Double_t mu2  = par[4];
  Double_t sig2 = par[5];
  Double_t gra  = par[6];
  Double_t dev1 = (x - mu1)/sig1;
  Double_t dev2 = (x - mu2)/sig2;
  Double_t value = A*TMath::Exp(-0.5*dev1*dev1) + B*TMath::Exp(-0.5*dev2*dev2) + gra;
  return value;
}
//________________________________________________________________________________
TF1 *InitGP() {
  struct Par_t {
    const Char_t *Name;
    Double_t p, pmin, pmax;
  };

  TF1 *gp = new TF1("gp",g2g,-100,100,7);
  const Par_t par[7] = {
    {"logN1",    5.,    0.,   25.},
    {"mu1",      0.,   -1.,    1.},
    {"sigma1",0.01, 0.001,   0.10},
    {"logN2",    1.,    0.,   25.},
    {"mu2",      0.,   -1.,    1.},
    {"sigma2", 0.10,  0.01,    1.},
    {"grass",   0.0,  0.00,    1.}
  };
  for (Int_t i = 0; i < 7; i++) {
    gp->SetParName(i,par[i].Name);
    gp->SetParameter(i,par[i].p);
    gp->SetParLimits(i,par[i].pmin, par[i].pmax);
    gp->FixParameter(3, 1e-10);
    gp->FixParameter(4, 1e-10);
    gp->FixParameter(5, 1e-10);
    gp->FixParameter(6, 1e-10);
  }

  return gp;
}
//________________________________________________________________________________
Double_t fbackgr(Double_t *x, Double_t *par) {
  //  return TMath::Exp(par[0])*TMath::Gaus(x[0],par[1],par[2])+par[3];
  Double_t bw = TMath::BreitWigner(x[0],par[1],par[2]);
  Double_t bwl = -99;
  if (bw > 0) bwl = TMath::Log(bw);
  return TMath::Exp(par[0] + (1. + par[4])*bwl) + par[3];
}
//________________________________________________________________________________
Double_t fpeaks(Double_t *x, Double_t *par) {
  Double_t result = 0;
  for (Int_t p=0;p<npeaks;p++) {
    Double_t norm  = TMath::Exp(par[3*p]);
    Double_t mean  = par[3*p+1];
    Double_t sigma = par[3*p+2];
    //    result += norm*TMath::Gaus(x[0],mean,sigma);
    result += norm*TMath::BreitWigner(x[0],mean,sigma);
  }
  return result + fbackgr(x,&par[3*npeaks]);
}
//________________________________________________________________________________
TF1 *Peaks(TH1 *h=0) {
  if (! h) return 0;
  Double_t allcha, sumx, sumx2, x, val, rms, mean, xmin, xmax;
  Int_t bin;
  const Double_t sqrtpi = 2.506628;
  TAxis *xax = h->GetXaxis();
  Int_t hxfirst = xax->GetFirst();
  Int_t hxlast  = xax->GetLast();
  xmin = xax->GetXmin();
  xmax = xax->GetXmax();
  Double_t valmax  = h->GetBinContent(hxfirst);
  Double_t binwidx = h->GetBinWidth(hxfirst);
  allcha = sumx = sumx2 = 0;
  Int_t np = 0;
  for (bin=hxfirst;bin<=hxlast;bin++) {
    x       = h->GetBinCenter(bin);
    val     = TMath::Abs(h->GetBinContent(bin));
    if (val <= 0) continue;
    np++;
    if (val > valmax) valmax = val;
    sumx   += val*x;
    sumx2  += val*x*x;
    allcha += val;
  }
  if (allcha == 0 || np <= 5) return 0;
  mean = sumx/allcha;
  rms  = sumx2/allcha - mean*mean;
  if (rms > 0) rms  = TMath::Sqrt(rms);
  else         rms  = 0;
  if (rms == 0) rms = binwidx*(hxlast-hxfirst+1)/4;
  Double_t constant = TMath::Log(0.5*(valmax+binwidx*allcha/(sqrtpi*rms)));
  TF1 *fback = (TF1 *) gROOT->GetFunction("back");
  if (! fback) fback = new TF1("back",fbackgr,xmin,xmax,5);
  fback->SetRange(mean-rms,mean+rms);
  //  fback->SetRange(xmin,xmax);
  //  fback->SetNpx(1000);
  fback->SetParameter(0,constant); fback->SetParLimits(0,
						       constant-10,
						       constant+30);
  fback->SetParameter(1,mean); fback->SetParLimits(1,-1,1);
  fback->SetParameter(2,rms/2.);
  fback->SetParLimits(2,20e-4,rms);
  fback->FixParameter(3,0);
  fback->FixParameter(4,0);
  h->Fit(fback,"q");
  if (fback->GetProb() > 1e-3) return fback;
  fback->SetParameter(1,mean);
  fback->ReleaseParameter(3);
  h->Fit(fback,"q");
  if (fback->GetProb() > 1e-3) return fback;
  fback->ReleaseParameter(4);
  fback->SetParLimits(4,0.,10.);
  h->Fit(fback,"q");
#ifndef TSPECTRA
  return fback;
#else
  //________________________________________________________________________________
  npeaks = np;
  TSpectrum s(2*npeaks);
  Int_t nfound = s.Search(h,2,"");
  //  cout << "Found " << nfound << " candidate peaks to fit" << endl;
  if (! nfound || nfound > 10) return fback;
  npeaks = 0;
  Float_t *xpeaks = s.GetPositionX();
  Float_t *ypeaks = s.GetPositionY();
  TArrayI idxT(nfound);        Int_t *idx = idxT.GetArray();
  TArrayF dT(nfound,ypeaks);   Float_t *d = dT.GetArray();
  TMath::Sort(nfound,d,idx,1);
  TArrayD Par(3*nfound+4);
  Double_t *par = Par.GetArray();
  Int_t p;
  for (p=0;p<nfound;p++) {
    Double_t xp = xpeaks[idx[p]];
    Int_t bin = xax->FindBin(xp);
    Double_t yp = h->GetBinContent(bin);
    if (yp-TMath::Sqrt(yp) < fback->Eval(xp)) continue;
    par[3*npeaks] = TMath::Log(yp);  
    par[3*npeaks+1] = xp;
    par[3*npeaks+2] = 3*binwidx;
    npeaks++;
  }
  for (p = 0; p < 4; p++) {
    par[3*npeaks+p] = fback->GetParameter(p);
  }
  TF1 *fit   = new TF1("fit",fpeaks,xmin,xmax,3*npeaks+1);
  for (int p = 0; p < npeaks; p++) {
    fit->SetParName(3*p,Form("Norm%i",p)); fit->SetParLimits(3*p,par[3*p]-10,par[3*p]+30);
    fit->SetParName(3*p+1,Form("Mu%i",p));
    fit->SetParName(3*p+2,Form("Sigma%i",p));
  }
  fit->SetParName(3*npeaks,"NormB"); fit->SetParLimits(3*npeaks,par[3*npeaks]-10,par[3*npeaks]+30);
  fit->SetParName(3*npeaks+1,"MuB");
  fit->SetParName(3*npeaks+2,"SigmaB");
  fit->SetParName(3*npeaks+3,"grass");
  
  fit->SetNpx(1000);
  fit->SetParameters(par);
  //  TVirtualFitter::Fitter(h2,10+3*npeaks); //we may have more than the default 25 parameters
  fit->SetParameters(par);
  fit->SetLineColor(2);
  h->Fit("fit","q");             
  return fit;
#endif
}
//______________________________________________________________________________
void SlicesYFit(TH2* h=0, Int_t binmin=0, Int_t binmax=0, Int_t cut=10, Option_t *option="QNRI") {
  if (! h) return;
  Int_t nbins  = h->GetXaxis()->GetNbins();
  if (binmin < 1) binmin = 1;
  if (binmax > nbins) binmax = nbins;
  if (binmax < binmin) {binmin = 1; binmax = nbins;}
  TString opt = option;
  opt.ToLower();
  Int_t ngroup = 1;
  if (opt.Contains("g2")) {ngroup = 2; opt.ReplaceAll("g2","");}
  if (opt.Contains("g3")) {ngroup = 3; opt.ReplaceAll("g3","");}
  if (opt.Contains("g4")) {ngroup = 4; opt.ReplaceAll("g4","");}
  if (opt.Contains("g5")) {ngroup = 5; opt.ReplaceAll("g5","");}
  Int_t npar = 3;
  if (npar <= 0) return;
  Double_t *parsave = new Double_t[npar];
  
  //Create one histogram for each function parameter
  Int_t ipar;
  TH1D **hlist = new TH1D*[npar];
  char *name   = new char[2000];
  char *title  = new char[2000];
  const TArrayD *bins = h->GetXaxis()->GetXbins();
  for (ipar=0;ipar<npar;ipar++) {
    sprintf(name,"%s_%d",h->GetName(),ipar);
    sprintf(title,"Fitted value of par[%d]",ipar);
    delete gDirectory->FindObject(name);
    if (bins->fN == 0) {
      hlist[ipar] = new TH1D(name,title, nbins, 
			     h->GetXaxis()->GetXmin(), h->GetXaxis()->GetXmax());
    } else {
      hlist[ipar] = new TH1D(name,title, nbins,bins->fArray);
    }
    hlist[ipar]->GetXaxis()->SetTitle(h->GetXaxis()->GetTitle());
  }
  sprintf(name,"%s_chi2",h->GetName());
  delete gDirectory->FindObject(name);
  TH1D *hchi2 = new TH1D(name,"chisquare", nbins, 
			 h->GetXaxis()->GetXmin(), h->GetXaxis()->GetXmax());
  hchi2->GetXaxis()->SetTitle(h->GetXaxis()->GetTitle());
  
  //Loop on all bins in X, generate a projection along Y
  Int_t bin;
  Int_t nentries;
  for (bin=binmin;bin<=binmax-ngroup+1;bin++) {
    TH1D *hpy = h->ProjectionY(Form("%s_bin_%i",h->GetName(),bin),bin,bin+ngroup-1,"e");
    if (hpy == 0) continue;
    nentries = Int_t(hpy->GetEntries());
    if (nentries == 0 || nentries < cut) {delete hpy; continue;}
    TF1 *f1 = Peaks(hpy);
    if (f1) {
      Int_t npfits = f1->GetNumberFitPoints();
      //      if (npfits > npar && npfits >= cut && f1->GetProb() > 1.e-3 && f1->GetParameter(1) < 0.5) {
      Double_t mu = f1->GetParameter(1);
      if (TMath::Abs(mu) < 0.5 && 
	  npfits > npar && npfits >= cut && 
	  (f1->GetProb() > 1.e-3 || 
	   f1->GetChisquare()/(npfits-npar) < 5.)) {
	Int_t biny = bin + ngroup/2;
	for (ipar=0;ipar<npar;ipar++) {
	  //	hlist[ipar]->Fill(h->GetXaxis()->GetBinCenter(biny),f1->GetParameter(ipar));
	  hlist[ipar]->SetBinContent(biny,f1->GetParameter(ipar));
	  hlist[ipar]->SetBinError(biny,f1->GetParError(ipar));
	}
	hchi2->Fill(h->GetXaxis()->GetBinCenter(biny),
		    f1->GetChisquare()/(npfits-npar));
      }
#if 0 
      hpy->Draw();
      c1->Update();
      cout << "type something" << endl;
      Int_t i;
      cin >> i;
#endif
      delete hpy;
    }
  }
  delete [] parsave;
  delete [] name;
  delete [] title;
  delete [] hlist;
}
//________________________________________________________________________________
Double_t STcheb(Int_t N, Double_t *par, Double_t x) {// N polynome degree, dimension is par[N+1]
  if (N < 0 || N > 12) return 0;
  Double_t T0 = 1;
  Double_t T1 = 2*x - 1;
  Double_t T2;
  Double_t Sum = par[0]*T0;
  if (N >= 1) {
    T1 = 2*x - 1;
    Sum += par[1]*T1;
    for (int n = 2; n <= N; n++) {
      T2 = 2*(2*x - 1)*T1 - T0;
      Sum += par[n]*T2;
      T0 = T1;
      T1 = T2;
    }
  }
  return Sum;
}
//________________________________________________________________________________
Double_t STchebN(Double_t *x, Double_t *par) {
  Int_t N = (Int_t) par[0];
  return STcheb(N,par+1,-x[0]-0.1);
}
//________________________________________________________________________________
Double_t STchebP(Double_t *x, Double_t *par) {
  Int_t N = (Int_t) par[0];
  return STcheb(N,par+1,x[0]-0.1);
}

//________________________________________________________________________________
void TDrawL(Int_t iHist=-1, Int_t barrel = -1) {

  //Barrels (GMT modules) from l1 to l2
  Int_t l1 = 0;
  Int_t l2 = 7;
  if (barrel>=0) {l1 = l2 = barrel;}
  Int_t nx = l2 - l1 + 1;
  firstH = firstHL; lastH = firstHG - 1;
  if (iHist >= 0) {firstH = lastH = iHist;}
  Int_t ny = lastH - firstH + 1;
  Int_t scaleX = 60; //600/nx;
  Int_t scaleY = 40; //800/ny;
  //  Int_t scale  = TMath::Min(scaleX,scaleY);
  c1 = new TCanvas(Form("Barrel_%i",barrel),
		   Form("Barrel %i",barrel) ,
		   10,10,10+scaleX*nx,10+scaleY*ny);
  cout << "nx/ny = " << nx << "/" << ny << endl;
  c1->Divide(nx,ny);

  //Create output files with fit values
  ofstream out;
  ofstream outC;
  TString Out("Results.Barrel_");
  Out += Form("%i_",barrel);
  Out += gSystem->BaseName(gDirectory->GetName());
  Out.ReplaceAll(".root","");
  TDatime t;
  //  Out += t.AsString();
  Out.ReplaceAll(" ","");
  if (gSystem->AccessPathName(Out)) {
    out.open(Out, ios::out); //"Results.list",ios::out | ios::app);
  }
  else {
    out.open(Out, ios::app);
  }
  Out += ".h";
  if (gSystem->AccessPathName(Out)){
    outC.open(Out, ios::out); //"Results.list",ios::out | ios::app);
  }
  else {
    outC.open(Out, ios::app);
  }
  TF1 *gp = InitGP();
  TH2 *h = 0;
  Int_t head = 0;
  for (Int_t i = 0; i < nx; i++) {
    if (! head) {
      out <<  "________________________________________________________________________________________________________"  << endl;
      out <<  "|du mkm         |dv mkm         |dw mkm         |alpha mrad     |beta mrad      |gamma mrad     |Comment" << endl;
      cout << "________________________________________________________________________________________________________"  << endl;
      cout <<  "|du mkm         |dv mkm         |dw mkm         |alpha mrad     |beta mrad      |gamma mrad     |Comment" << endl;
      outC << "struct data_t {" << endl;
      outC << "\tInt_t barrel;" << endl;
      outC << "\tDouble_t u, Du, v, Dv, w, Dw, alpha, Dalpha, beta, Dbeta, gamma, Dgamma;" << endl;
      outC << "\tChar_t *Comment;" << endl;
      outC << "};" << endl;
      outC << "data_t Data[] = {" << endl;
    }
    head++;
    out  << "__________________________________________________________________________________________________ " << endl;
    cout << "__________________________________________________________________________________________________ " << endl;
    //Ladder = barrel from here
    Int_t ladder = i+1;
    
    Double_t FitR[6], dFitR[6], LSFit[6], dLSFit[6];
    memset (FitR, 0, 6*sizeof(Double_t));
    memset (dFitR, 0, 6*sizeof(Double_t));
    memset (LSFit, 0, 6*sizeof(Double_t));
    memset (dLSFit, 0, 6*sizeof(Double_t));
    TString line("");
    TString lTitle("");
    TString lineC("");
    TH1D *LSFB = (TH1D *) gDirectory->Get(Form("LSFB%i",ladder));
    if (LSFB) {
      Double_t *array = LSFB->GetArray();
      Int_t im = 1 + 28*(ladder-1);
      Int_t is = im + 6;
      TRVector AmX(6,array+im);// cout << "AmX " << AmX << endl;
      TRSymMatrix S(6,array+is);// cout << "S " << S << endl;
      TRSymMatrix SInv(S,TRArray::kInverted);// cout << "SInv " << SInv << endl;
      TRVector  X(SInv,TRArray::kSxA,AmX); //cout << "X " << X << endl;
      TString line("");
      for (Int_t l = 0; l < 6; l++) {
	//	if (l == 4) X(l) = -X(l);  // switch sign
	Double_t scale = 1e4;
	if (l > 2) scale = 1e3;
	LSFit[l] = scale*X(l);
	dLSFit[l] = scale*TMath::Sqrt(SInv(l,l));
	line += Form("|%7.2f+-%5.2f ",LSFit[l],dLSFit[l]); 
	if (SInv(l,l) > 0) {
	  FitR[l]  += X(l)/SInv(l,l);
	  dFitR[l] +=  1./SInv(l,l);
	}
      }
      line += "|"; line += LSFB->GetName(); line += "/"; line += LSFB->GetTitle();// line += "\t"; line += f->GetName();
      cout << line << endl;
      out << line << endl;
   } 
    for (Int_t j = firstH; j <= lastH; j++) {
      Int_t Id = ladder-1;
      h = (TH2 *) gDirectory->Get(Form("%s%04i",plotName[j],Id));
      if (! h) continue;
      h->SetMinimum(1);
      h->SetXTitle(gDirectory->GetName());
      Int_t ij = i + nx*(j-firstH) + 1;
      c1->cd(ij)->SetLogz(1);
      TH1 *fit = 0;
      TProfile *prof = h->ProfileX();
      prof->SetMarkerStyle(24);
      prof->SetMarkerColor(6);
      TH1 *sp = h->ProjectionY("_py",-1,-1,"e");
      sp->Fit("gaus","q");
      Double_t Mu = 0;
      Double_t dMu = 0;
      TF1 *gaus = sp->GetFunction("gaus");
      if(gaus==0) continue;
      gp->SetParameter(0,TMath::Log(gaus->GetParameter(0)));
      gp->SetParameter(1,gaus->GetParameter(1));
      gp->SetParameter(2,TMath::Abs(gaus->GetParameter(2)));
      sp->Fit(gp,"q");
      Mu = sp->GetFunction("gp")->GetParameter(1);
      dMu = sp->GetFunction("gp")->GetParError(1);
      
      Double_t *params = gp->GetParameters();
      params[0] -= TMath::Log(100.);
      gp->SetParameters(params);
      //      SlicesYFit(h,0,0,10,"qnig3"); // g3
      SlicesYFit(h,0,0,10,"qnig3"); // g3
      fit = (TH1 *) gDirectory->Get(Form("%s_1",h->GetName()));
      //       TH1 *sig = (TH1 *) gDirectory->Get(Form("%s_2",h->GetName()));
      //       TH1 *gra = (TH1 *) gDirectory->Get(Form("%s_3",h->GetName()));
      Double_t slope = 0;
      Double_t dslope = 0;
      TLegend *leg = new TLegend(0.1,0.2,0.9,0.3,"");
      leg->SetTextSize(0.025);
      if (fit) {
	fit->SetTitle(h->GetTitle());
	fit->SetMarkerStyle(20);
	fit->SetMarkerColor(1);
	fit->SetMaximum(0.2);
	fit->SetMinimum(-.2);
	fit->SetStats(1);
	Double_t zmax = 99;
	FitPolN(fit,-zmax,zmax);
	//	fit->Fit("PolN","eqr","",-zmax,zmax);
	TF1 *pol1 = fit->GetFunction("PolN");
	if (! pol1 ) goto endhLoop;
	Double_t prob = pol1->GetProb();
// 	if (prob <= 1e-3) {
// 	  fit = prof;
// 	  FitPolN(fit,-zmax,zmax);
// 	  pol1 = fit->GetFunction("PolN");
// 	  prob = pol1->GetProb();
// 	}
	if (prob >= 0) {
	  Mu     = pol1->GetParameter(0);
	  dMu    = pol1->GetParError(0);
	  if (dMu > 99.99e-4) dMu=  99.99e-4;
	  slope  = pol1->GetParameter(1);
	  dslope = pol1->GetParError(1);
	  if (dslope > 99.99e-3) dslope = 99.99e-3;
	} else {
	  Mu = slope = 0;
	  dMu = dslope = 0;
	}
	static const Char_t *duv[2] = {"du", "dv"};
	TString Name(h->GetName());
	TString Title(h->GetTitle());
	line = "";
	lTitle = "";
	lineC = Form("\t{%1i, %2i",ladder, j);
	for (Int_t m = 0; m < 2; m++) {
	  if (Name.BeginsWith(duv[m]) && ! Name.Contains("Over") && dMu > 0) {
	    Double_t mu = -1e4*Mu;
	    Double_t dmu = 1e4*dMu;
	    line += Form("|%7.2f+-%5.2f",mu,dmu);
	    lineC += Form(",%7.2f,%5.2f",mu,dmu);
	    lTitle += Form("%s = %7.2f +- %5.2f (mkm)", duv[m], -mu,dmu);
	    Double_t dev = mu - LSFit[m];
	    Double_t sdev = TMath::Sqrt(dmu*dmu+dLSFit[m]*dLSFit[m]);
	    if (dLSFit[m] == 0 || sdev > 0 && TMath::Abs(dev/sdev) < nSigMax) {
	      Double_t dMu2 = dMu*dMu;
	      FitR[m]  += -Mu/dMu2;
	      dFitR[m] +=  1./dMu2;
		  line += "A";
	    } else line +="R";
	  }
	  else {
	    line += Form("|               ");
	    lineC += ",      0,-9.99";
	  }
	}	  
	Int_t index = Title.Index("=>");
	TString tag("");
	if (index >= 0) {
	  index = index+2;
	  static TString separator("[^ ;,]+");
	  TString t(Title.Data()+index);
	  TObjArray *array = t.Tokenize(separator);
	  tag = ((TObjString *) array->At(0))->GetString();
	  delete array;
	}
	for (Int_t m = 2; m < 6; m++) {
	  Double_t dslope2, scale;
	  Double_t mu, dmu;
	  if (dslope <= 0) goto Empty;
	  scale = 1e3;
	  switch (m) {
	  case 2:
	    if (! tag.Contains("dw")) goto Empty;
	    scale = 1e4;
	    mu = scale*slope;
	    dmu = scale*dslope;
	    line += Form("|%7.2f+-%5.2f",mu,dmu); 
	    lineC += Form(",%7.2f,%5.2f",mu,dmu); 
	    lTitle += Form(" dw = %7.2f +- %5.2f (mkm)", mu, dmu); 
	    break;
	  case 3: 
	    if (! tag.Contains("alpha")) goto Empty;
	    //	    if (tag.Contains("-alpha")) slope = - slope;
	    mu = scale*slope;
	    dmu = scale*dslope;
	    line += Form("|%7.2f+-%5.2f",mu,dmu);
	    lineC += Form(",%7.2f,%5.2f",mu,dmu);
	    lTitle += Form(" alpha = %7.2f +- %5.2f (mrad)", mu, dmu); 
	    break;
	  case 4: 
	    if (! tag.Contains("beta")) goto Empty;
	    //	    if (! tag.Contains("-beta")) slope = - slope;
	    mu = scale*slope;
	    dmu = scale*dslope;
	    line += Form("|%7.2f+-%5.2f", mu,dmu); 
	    lineC += Form(",%7.2f,%5.2f", mu,dmu); 
	    lTitle += Form(" beta = %7.2f +- %5.2f (mrad)",  mu, dmu); 
	    break;
	  case 5:
	    if (! tag.Contains("gamma")) goto Empty;
	    //	    if (tag.Contains("-gamma")) slope = - slope;
	    mu = scale*slope;
	    dmu = scale*dslope;
	    line += Form("|%7.2f+-%5.2f", mu,dmu); 
	    lineC += Form(",%7.2f,%5.2f", mu,dmu); 
	    lTitle += Form(" gamma = %7.2f +- %5.2f (mrad)",  mu, dmu); 
	    break;
	  default:  
	    goto Empty; 
	  };
          if (! Name.Contains("duOvertuP") && dslope > 0) {
	    Double_t dev = mu - LSFit[m];
	    Double_t sdev = TMath::Sqrt(dmu*dmu+dLSFit[m]*dLSFit[m]);
	    if (dLSFit[m] == 0 || sdev > 0 && TMath::Abs(dev/sdev) < nSigMax) {
	      dslope2 = dslope*dslope;
	      FitR[m]  += slope/dslope2;
	      dFitR[m] +=  1./dslope2;
		  line += "A";
	    } else line +="R";
	  } else line +="R";
	  continue;
	Empty:
	  line += Form("|               ");
	  lineC +=     ",      0,-9.99";
	}
	lTitle += Form(" prob = %5.3f",prob);
	leg->AddEntry(pol1,lTitle);
	line += "|"; line += fit->GetName(); line += "/"; line += h->GetTitle(); 
	lineC += ",\""; lineC += fit->GetName(); lineC += "\"},";
	cout << line << endl;
	out << line << endl;
	outC << lineC << endl;
      } 
    endhLoop:
#if 0
      if (h) h->DrawCopy("colz");
      if (prof) prof->DrawCopy("same");
#else
      if (h) h->Draw("colz");
      if (prof) prof->Draw("same");
#endif
      if (fit) {
#if 0
	fit->DrawCopy("same"); 
#else
	fit->Draw("same"); 
#endif
	TF1 *pol1 = fit->GetFunction("PolN"); 
	if (pol1) {pol1->SetLineColor(2); pol1->Draw("same");}
      }
      leg->Draw();
    }
    line = ""; lineC = "";
    lineC = Form("\t{%1i, %2i",ladder, -1);
    for (Int_t m = 0; m < 6; m++) {
      if (dFitR[m] > 0) {
	Double_t scale = 1e4;
	if (m > 2) scale = 1e3;
	FitR[m] = scale*FitR[m]/dFitR[m]; 
	dFitR[m] = scale/TMath::Sqrt(dFitR[m]);
	line += Form("|%7.2f+-%5.2f ", FitR[m],dFitR[m]); 
	lineC += Form(",%7.2f,%5.2f", FitR[m],dFitR[m]); 
      } else {
	line += Form("|               ");
	lineC +=     ",      0,-9.99";
      }
    }
    line += "| Average for "; line += Form("barrel %1i,",ladder);
    lineC += ",\"Average\"},";
    cout << line << endl;
    out << line << endl;
    outC << lineC << endl;
  }
  out.close();
  outC.close();
}

//________________________________________________________________________________
void TDraw(Int_t k = -1) {
 
  TDrawL(-1,k);
}
