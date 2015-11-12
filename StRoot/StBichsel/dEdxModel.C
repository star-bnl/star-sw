/* 
   root.exe SL15AuAu200Z6cmBLStiCAKFV.root dEdxModel.C+
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
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TF1.h"
#include "TF2.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "THnSparse.h"
#include "TStyle.h"
#include "TFile.h"
#include "TCanvas.h"
#include "TString.h"
#include "TLegend.h"
#include "TRandom.h"
#include "TMultiDimFit.h"
#endif
#include <assert.h>
#include "Ask.h"
static Int_t _debug = 0;
static TCanvas *c1 = 0;
TMultiDimFit* fit = 0;
 //________________________________________________________________________________
void h2MDF(const Char_t  *total = "mu", Int_t max=5, Int_t maxTerm = 20){
  TH2D *total2D = (TH2D *) gDirectory->Get(total);
  if (! total2D) {
    cout << "Histogram  has not been found " << endl;
    return;
  }
  // Global data parameters 
  Int_t nVars       = 2;
  
  // make fit object and set parameters on it. 
  //  fit = new TMultiDimFit(nVars, TMultiDimFit::kMonomials,"vk");
  fit = new TMultiDimFit(nVars, TMultiDimFit::kChebyshev,"vk");
  fit->SetName(Form("MDF_%s",total));
  gDirectory->Append(fit);
  Int_t mPowers[]   = {max , 3};
  fit->SetMaxPowers(mPowers);
  fit->SetMaxFunctions(1000);
  fit->SetMaxStudy(1000);
  fit->SetMaxTerms(maxTerm);
  fit->SetPowerLimit(max);
  fit->SetMinAngle(10);
  fit->SetMaxAngle(10);
  fit->SetMinRelativeError(.01);

  // variables to hold the temporary input data 
  Double_t *x = new Double_t[nVars];
  
  // Print out the start parameters
  fit->Print("p");
  
  TAxis *xa = total2D->GetXaxis();
  TAxis *ya = total2D->GetYaxis();
  Int_t nx = xa->GetNbins();
  Int_t ny = ya->GetNbins();
  Int_t iy, ix;
  for (iy = 1; iy <= ny; iy++) {
    for (ix = 1; ix <= nx; ix++) {
      Double_t error = total2D->GetBinError(ix,iy);
      if (error <= 0) continue;
      Double_t value = total2D->GetBinContent(ix,iy);
      x[0]           = xa->GetBinCenter(ix);
      x[1]           = ya->GetBinCenter(iy);
      if (x[0] < 0.6) continue;
      if (x[0] > 3.9) continue;
      Double_t yy = value;
      Double_t ee = 1e-4; //error*error;
      fit->AddRow(x,yy,ee);
    }
  }
  // Print out the statistics
  fit->Print("s");
  
  // Book histograms 
  fit->SetBinVarX(1000);
  fit->SetBinVarY(1000);
  fit->MakeHistograms();

  // Find the parameterization 
  fit->FindParameterization();

  // Print coefficents 
  fit->Print("rc");
#if 0
  //
   // Now for the data
   //
  Int_t i, j;
 // Assignment to coefficients vector.
  cout << "  row.PolyType = \t"      << fit->GetPolyType() << ";" << endl;
  cout << "  row.NVariables = \t"    << fit->GetNVariables() << ";" << endl;
  cout << "  row.NCoefficients = \t" << fit->GetNCoefficients() << ";" << endl;
  for (i = 0; i < fit->GetNVariables(); i++) {
    cout << Form("  row.XMin[%2i] = %10.5g;", i,fit->GetMinVariables()->operator()(i));
  }
  cout << endl;
  for (i = 0; i < fit->GetNVariables(); i++) {
    cout << Form("  row.XMax[%2i] = %10.5g;", i,fit->GetMaxVariables()->operator()(i));
  }
  cout << endl;
  for (i = 0; i < fit->GetNCoefficients(); i++) {
    for (j = 0; j < fit->GetNVariables(); j++) {
      cout << Form("  row.Power[%2i] = %2i;",i * fit->GetNVariables() + j,
		   fit->GetPowers()[fit->GetPowerIndex()[i] * fit->GetNVariables() + j]);
    }
    cout << endl;
  }
  cout << "  row.DMean = \t"          << fit->GetMeanQuantity() << ";" << endl;
  for (i = 0; i < fit->GetNCoefficients(); i++) {
    cout << Form("  row.Coefficients[%2i]    = %15.5g;", i, fit->GetCoefficients()->operator()(i));
    if ((i+1) %2 == 0) cout << endl;
  }
  if (fit->GetNCoefficients()%2) cout << endl;
  for (i = 0; i < fit->GetNCoefficients(); i++) {
    cout << Form("  row.CoefficientsRMS[%2i] = %15.5g;", i, fit->GetCoefficientsRMS()->operator()(i));
    if ((i+1) %2 == 0) cout << endl;
  }
  if (fit->GetNCoefficients()%2) cout << endl;
#endif
  TH2D *total2Dr = new TH2D(*total2D);
  total2Dr->SetName(Form("%s_MDFres",total2D->GetName()));
  total2Dr->Reset();
  TH2D *total2Df = new TH2D(*total2D);
  total2Df->SetName(Form("%s_MDFpar",total2D->GetName()));
  total2Df->Reset();
  for (iy = 1; iy <= ny; iy++) {
    for (ix = 1; ix <= nx; ix++) {
      Double_t error = total2D->GetBinError(ix,iy);
      if (error <= 0) continue;
      Double_t value = total2D->GetBinContent(ix,iy);
      x[0]           = xa->GetBinCenter(ix);
      x[1]           = ya->GetBinCenter(iy);
      Double_t par = fit->Eval(x);
      total2Df->SetBinContent(ix,iy,par);
      value -= par;
      total2Dr->SetBinContent(ix,iy,value);
    }
  }  
}
//____________________________________________________________________________
Double_t zMPVFunc(Double_t *x, Double_t *p=0) {
  // Most 
  static TH2F *dEdxMPV = 0;
  if (! dEdxMPV) {
    dEdxMPV = (TH2F *) gDirectory->Get("dEdxMPV_MDFpar");
    assert(dEdxMPV);
  }
  Double_t n_PL10   = x[0];
#if 0
  Double_t n_P      = TMath::Power(10.,x[0]);
  if (n_P < 2)  n_P = 2;
  if (n_P > 10000) n_P = 10000;
#endif
  Double_t sigma = x[1];
  if (sigma < 0.01) sigma = 0.01;
  if (sigma > 0.99) sigma = 0.99;
  return dEdxMPV->Interpolate(n_PL10, sigma);
}
//________________________________________________________________________________
TF2 *zMPV() {
  TF2 *f = new TF2("zFunc",zMPVFunc,0,4,0.01,0.99,0);
  return f;
}
//________________________________________________________________________________
Double_t dLogNtpernPdP(Double_t *x, Double_t *p) {
  static TH3F *dEdxFun = 0;
  if (! dEdxFun) {
    //    dEdxFun = (TH3F *) gDirectory->Get("dEdxFunA");
    dEdxFun = (TH3F *) gDirectory->Get("dEdxFun");
    assert(dEdxFun);
  }
  static Double_t W = 26.2e-3;// keV
  Double_t z        = x[0]; // log (dE (keV))
  Double_t n_PL10   = p[0];
  Double_t n_P      = TMath::Power(10., n_PL10);
#if 0
  Double_t n_P      = p[0];
  if (n_P < 2)  n_P = 2;
  if (n_P > 16000) n_P = 16000;
#endif
  Double_t sigma = p[1];
  if (sigma < 0.01) sigma = 0.01;
  if (sigma > 0.99) sigma = 0.99;
  Double_t n_T   = TMath::Exp(z)/W;
  Double_t w     = TMath::Log(n_T/n_P);
  if (w <-1.95) w = -1.95;
  if (w > 7.95) w =  7.95;
  return dEdxFun->Interpolate(n_PL10, sigma, w);
}
//________________________________________________________________________________
TF1 *zFunc() {
  TF1 *f = new TF1("zFunc",dLogNtpernPdP,-5,15.,2);
  f->SetNpx(1000);
  f->SetParName(0,"n_PL10");
  f->SetParName(1,"sigma");
  f->SetParameters(TMath::Log10(30.),0.25);
  return f;
}
//________________________________________________________________________________
Double_t dEdxFunc(Double_t *x, Double_t *p) {
  static TF1 *f = 0;
  if (! f) f = zFunc();
  Double_t n_PL10 = TMath::Log10(p[1]);
  if (n_PL10 > 4) n_PL10 = 4;
  if (n_PL10 < 0.301) n_PL10 = 0.301;
  f->SetParameter(0,n_PL10);
  f->SetParameter(1,p[3]);
  static TF1 *fMPV = 0;
  if (! fMPV) fMPV = zMPV();
  Double_t zMPV = fMPV->Eval(n_PL10,p[2]);
  return TMath::Exp(p[0])*f->Eval(x[0]+zMPV-p[2]);
}
//________________________________________________________________________________
TF1 *zdEdx() {
  TF1 *f = new TF1("zdEdx",dEdxFunc,-5,15.,4);
  f->SetNpx(1000);
  f->SetParName(0,"scale");
  f->SetParName(1,"n_P"); f->SetParLimits(1,2,1e4);
  f->SetParName(2,"mu");  f->SetParLimits(2,-10,10);
  f->SetParName(3,"sigma"); f->SetParLimits(3,0.01,0.99);
  f->SetParameters(0.,30.,0.0,0.25);
  return f;
}
//________________________________________________________________________________
void dEdxModel() {
  if (_debug) {
    c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
    if (! c1 ) c1 = new TCanvas("c1","c1");
  }
  cout << "dEdxModel" << endl;
  /* nPdTO/nPdTI - log(Total no. of conducting electrons) - log(no. of primary one) versus no. primary electrons:Outer/Inner
     n_e - Total no. of conducting electrons
     n_P - no. primary electrons; n_p = dX * BB(beta*gamma), dX track segment length, BB is Bether Bloch function = no. of primary clusters per cm
     w = log(n_e/n_P); 
     dN/dw = f(w;n_P); // histogram
     Y = n_e = n_P*exp(w);
     dn_e/dw = n_e; 
     dN/dY = dN/dw*dw/Y = f(w;n_P)/Y;
     
     Log Normal distribution:
     ========================  
      g(X) = 1/((2*pi)*sigma)/X*exp(-(log(X - mu)**2/(2*sigma**2)));
      z = log(X);
      mu => 0
     ======================= 
      U  = w (*) Gaus(0,sigma); convolution 
      F(U;n_P,sigma);
      dE = W * n_P * exp(U):
      log(dE) = log(W) + log(n_P) + U;  mu = log(W);
      
      N_p = n_P/dX;

      dE/dx 
   */
  TH2D *nPdTI = (TH2D *) gDirectory->Get("nPdTI");
  TH2D *nPdTO = (TH2D *) gDirectory->Get("nPdTO");
  if (! nPdTI || ! nPdTO) {
    if (! nPdTI) cout << "nPdTI is missing" << endl;
    if (! nPdTO) cout << "nPdTO is missing" << endl;
    return;
  } else {
    cout << "Found nPdTI and nPdTO" << endl;
  }
  TH2D *nPdT  = new TH2D(*nPdTI);
  nPdT->SetName("nPdT");
  nPdT->Add(nPdTO);
  TAxis *x = nPdT->GetXaxis();
  Int_t Nxbins = x->GetNbins();
  Nxbins = 157;
  const TArrayD     &xBins = *x->GetXbins();
  TArrayD XBins(Nxbins+1);
  for (Int_t ix = 0; ix <= Nxbins; ix++) XBins[ix] = TMath::Log10(xBins[ix+1]);
  TAxis *y = nPdT->GetYaxis();
  Int_t Nybins = y->GetNbins();
  TFile *fOut = new TFile("dEdxModel.root","recreate");
  Int_t    Nsigma = 100;
  Double_t sigmaMin = 0.00;
  Double_t sigmaMax = 1.00;
  TArrayD     yBins(Nsigma+1);
  Double_t dsigma = (sigmaMax - sigmaMin)/Nsigma;
  yBins[0] = sigmaMin;
  for (Int_t i = 1; i <= Nsigma; i++) yBins[i] = yBins[i-1] + dsigma;
  TArrayD     zBins(y->GetNbins()+1);
  zBins[0] = y->GetXmin();
  for (Int_t i = 1; i <= y->GetNbins(); i++) zBins[i] = y->GetBinUpEdge(i);
  Int_t Nzbins = y->GetNbins();
  TH3F *dEdxFun = new TH3F("dEdxFun","w = log(n_e/n_P) versus log_10(n_P) and sigma",
			   Nxbins, XBins.GetArray(),
			   Nsigma, yBins.GetArray(),
			   Nzbins, zBins.GetArray());
  TH3F *dEdxFunA = new TH3F("dEdxFunA","w = log(n_e/n_P) versus log_10(n_P) and sigma, smoothed",
			   Nxbins, XBins.GetArray(),
			   Nsigma, yBins.GetArray(),
			   Nzbins, zBins.GetArray());
  TH2D *dEdxMPV = new TH2D("dEdxMPV","most probable value of w = log(n_e/n_P) versus log_10(n_P) and sigma",
			   Nxbins, XBins.GetArray(),
			   Nsigma, yBins.GetArray());
  TH2D *dEdxMean = new TH2D("dEdxMean","mean value of w = log(n_e/n_P) versus log_10(n_P) and sigma",
			   Nxbins, XBins.GetArray(),
			   Nsigma, yBins.GetArray());
  TH2D *dEdxRMS = new TH2D("dEdxRMS","RMS value of w = log(n_e/n_P) versus log_10(n_P) and sigma",
			   Nxbins, XBins.GetArray(),
			   Nsigma, yBins.GetArray());
  for (Int_t iY = 1; iY <= Nsigma; iY++) {
    Double_t sigma = dEdxFun->GetYaxis()->GetBinCenter(iY);
    for (Int_t iX = 1; iX <= Nxbins; iX++) {
      Double_t n_pL10 = dEdxFun->GetXaxis()->GetBinCenter(iX);
      Double_t n_p    = TMath::Power(10.,n_pL10);
      Int_t bin = x->FindBin(n_p);
      TH1D *proj = nPdT->ProjectionY("_y",bin,bin);
      if (proj->GetEntries() > 100) {
	TH1D *hist = new TH1D(*proj);
	hist->SetName("RnDM");
	hist->Reset();
	for (Int_t k = 0; k < 100000; k++) {
	  Double_t u = proj->GetRandom();
	  u += gRandom->Gaus(0.,sigma);
	  hist->Fill(u);
	}
	hist->Smooth(25);
	for (Int_t iZ = 1; iZ <= Nzbins; iZ++) {
	  dEdxFun->SetBinContent(iX,iY,iZ,hist->GetBinContent(iZ));
	}
	dEdxMean->SetBinContent(iX,iY,hist->GetMean());
	dEdxRMS->SetBinContent(iX,iY,hist->GetRMS());
	if (c1) {
	  hist->Draw();
	  c1->Update();
	  if (! gROOT->IsBatch()) {
	    if (Ask()) return;
	  } else {_debugAsk = 0;}
	}
	delete hist;
      }
      delete proj;
    }
    cout << "Done with iY = " << iY << endl;
  }
#if 0
  for (Int_t iY = 1; iY <= Nsigma; iY++) {
    for (Int_t iX = 1; iX <= Nxbins; iX++) {
      for (Int_t iZ = 1; iZ <= Nzbins; iZ++) {
	if (iX == 1 || iX == Nxbins ||
	    iY == 1 || iY == Nybins ||
	    iZ == 1 || iZ == Nzbins) {
	  dEdxFunA->SetBinContent(iX,iY,iZ,dEdxFun->GetBinContent(iX,iY,iZ));
	} else {
	  Double_t val = 0;
	  Int_t n = 0;
	  for (Int_t ix = iX - 1; ix <= iX + 1; ix++) 
	    for (Int_t iy = iY - 1; iy <= iY + 1; iy++) 
	      for (Int_t iz = iZ - 1; iz <= iZ + 1; iz++) {
		n++;
		val += dEdxFun->GetBinContent(ix,iy,iz);
	      }
	  dEdxFunA->SetBinContent(iX,iY,iZ,val/n);
	}
      }
    }
  }
  cout << "Done  with dEdxFunA" << endl;
#endif
  TF1 *f = zFunc();
  for (Int_t iY = 1; iY <= Nsigma; iY++) {
    Double_t sigma = dEdxFun->GetYaxis()->GetBinCenter(iY);
    f->SetParameter(1,sigma);
    for (Int_t iX = 1; iX <= Nxbins; iX++) {
      Double_t n_pL10 = dEdxFun->GetXaxis()->GetBinCenter(iX);
      f->SetParameter(0,n_pL10);
      Double_t mpv = f->GetMaximumX();
      dEdxMPV->SetBinContent(iX,iY,mpv);
    }
  }
  h2MDF(dEdxMPV->GetName(),7,200);
  fOut->Write();
#if 0
  // Normalize
  for (Int_t ix = 1; ix <= Nxbins; ix++) {
    Double_t sum = 0;
    Double_t dx = x->GetBinUpEdge(ix) - x->GetBinLowEdge(ix);
    for (Int_t iy = 1; iy < Nybins; iy++) {
      sum += nPdT->GetBinContent(ix,iy);
    }
    if (sum > 0) {
      for (Int_t iy = 1; iy < Nybins; iy++) {
	Double_t val =  nPdT->GetBinContent(ix,iy)/sum;//dx;
	Double_t err =  nPdT->GetBinError(ix,iy)/sum;//dx;
	nPdT->SetBinContent(ix,iy, val);
	nPdT->SetBinError(ix,iy, err);
      }
    }
  }
#endif
}
