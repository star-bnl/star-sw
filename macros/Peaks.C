#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TMath.h"
#include "TH1.h"
#include "TF1.h"
#include "TSpectrum.h"
#include "TArrayD.h"
#endif
Int_t npeaks = 30;
//________________________________________________________________________________
Double_t fbackgr(Double_t *x, Double_t *par) {
  return TMath::Exp(par[0])*TMath::Gaus(x[0],par[1],par[2])+par[3];
}
//________________________________________________________________________________
Double_t fpeaks(Double_t *x, Double_t *par) {
  Double_t result = 0;
   for (Int_t p=0;p<npeaks;p++) {
     Double_t norm  = TMath::Exp(par[3*p]);
     Double_t mean  = par[3*p+1];
     Double_t sigma = par[3*p+2];
     result += norm*TMath::Gaus(x[0],mean,sigma);
   }
   return result + fbackgr(x,&par[3*npeaks]);
}
//________________________________________________________________________________
TF1 *Peaks(TH1 *h=0, Int_t np=10) {
  if (! h) return 0;
  Double_t allcha, sumx, sumx2, x, val, rms, mean;
  Int_t bin;
  const Double_t sqrtpi = 2.506628;
  TAxis *xax = h->GetXaxis();
  Int_t hxfirst = xax->GetFirst();
  Int_t hxlast  = xax->GetLast();
  Double_t valmax  = h->GetBinContent(hxfirst);
  Double_t binwidx = h->GetBinWidth(hxfirst);
  allcha = sumx = sumx2 = 0;
  for (bin=hxfirst;bin<=hxlast;bin++) {
    x       = h->GetBinCenter(bin);
    val     = TMath::Abs(h->GetBinContent(bin));
    if (val > valmax) valmax = val;
    sumx   += val*x;
    sumx2  += val*x*x;
    allcha += val;
  }
  if (allcha == 0) return 0;
  mean = sumx/allcha;
  rms  = sumx2/allcha - mean*mean;
  if (rms > 0) rms  = TMath::Sqrt(rms);
  else         rms  = 0;
  if (rms == 0) rms = binwidx*(hxlast-hxfirst+1)/4;
  Double_t constant = TMath::Log(0.5*(valmax+binwidx*allcha/(sqrtpi*rms)));
  TF1 *fback = new TF1("back",fbackgr,xax->GetXmin(),xax->GetXmax(),4);
  fback->SetNpx(1000);
  fback->SetParameter(0,constant);
  fback->SetParameter(1,mean);
  fback->SetParameter(2,rms);
  fback->SetParLimits(2,0,10*rms);
  fback->FixParameter(3,0);
  h->Fit(fback,"q");
  if (fback->GetProb() > 1e-2) return fback;
  fback->ReleaseParameter(3);
  h->Fit(fback,"q");
  if (fback->GetProb() > 1e-2) return fback;
  //________________________________________________________________________________
  npeaks = np;
  TSpectrum s(2*npeaks);
  Int_t nfound = s.Search(h,2,"");
  cout << "Found " << nfound << " candidate peaks to fit" << endl;
  if (! nfound) return 0;
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
  TF1 *fit   = new TF1("fit",fpeaks,xax->GetXmin(),xax->GetXmax(),3*npeaks+4);
  for (int p = 0; p < npeaks; p++) {
    fit->SetParName(3*p,Form("Norm%i",p));
    fit->SetParName(3*p+1,Form("Mu%i",p));
    fit->SetParName(3*p+2,Form("Sigma%i",p));
  }
  fit->SetParName(3*npeaks,"NormB");
  fit->SetParName(3*npeaks+1,"MuB");
  fit->SetParName(3*npeaks+2,"SigmaB");
  fit->SetParName(3*npeaks+3,"grass");
  
  fit->SetNpx(1000);
  fit->SetParameters(par);
  //  TVirtualFitter::Fitter(h2,10+3*npeaks); //we may have more than the default 25 parameters
  fit->SetParameters(par);
  fit->SetLineColor(2);
  h->Fit("fit");             
  return fit;
}
