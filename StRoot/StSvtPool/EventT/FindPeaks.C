// This script generates a random number of gaussian peaks
// on top of a linear background.
// The position of the peaks is found via TSpectrum and injected
// as initial values of parameters to make a global fit.
// The background is computed and drawn on top of the original histogram.
//
// To execute this example, do
//  root > .x peaks.C  (generate 10 peaks by default)
//  root > .x peaks.C++ (use the compiler)
//  root > .x peaks.C++(30) (generates 30 peaks)
//
// To execute only the first part of teh script (without fitting)
// specify a negative value for the number of peaks, eg
//  root > .x peaks.C(-20)
//
//Author: Rene Brun
#include "Riostream.h"
#include <stdio.h>
#include "TSystem.h"
#include "TCanvas.h"
#include "TH1.h"
#include "TF1.h"
#include "TRandom.h"
#include "TSpectrum.h"
#include "TVirtualFitter.h"
#include "TMath.h"   
#include "TDirectory.h"
#include "TKey.h"
ofstream out;
Int_t npeaks = 30;
TCanvas *c1 = 0;
TCanvas *c2 = 0;
//________________________________________________________________________________
Double_t fpeaks(Double_t *x, Double_t *par) {
   Double_t result = par[0] + par[1]*x[0];
   for (Int_t p=0;p<npeaks;p++) {
      Double_t norm  = par[3*p+2];
      Double_t mean  = par[3*p+3];
      Double_t sigma = par[3*p+4];
      result += norm*TMath::Gaus(x[0],mean,sigma);
   }
   return result;
}
//________________________________________________________________________________
void peaks(TH1* h, Int_t np=10, Int_t ij=0) {
  if (! h) return;
  npeaks = TMath::Abs(np);
  if (! c1)  c1 = new TCanvas();
  else       c1->Clear();
  if (c2 && ij > 0) {c2->cd(ij); h->Draw(); c2->Update();}
  c1->Divide(1,2);
  c1->cd(1);
  h->Draw();
  TH1F *h2 = (TH1F*)h->Clone("h2");
  //Use TSpectrum to find the peak candidates
  TSpectrum *s = new TSpectrum(2*npeaks);
  Int_t nfound = s->Search(h,5,"",0.05);
  printf("Found %d candidate peaks to fit\n",nfound);
  if (! nfound) return;
  //Estimate background using TSpectrum::Background
  TH1 *hb = s->Background(h,20,"same");
  hb->Draw("same");
  c1->Update();
  if (c2 && ij > 0) {c2->cd(ij); h->Draw(); c2->Update();}
  if (np <0) return;

  //estimate linear background using a fitting method
  c1->cd(2);
  TF1 *fline = new TF1("fline","pol1",0,1000);
  fline->FixParameter(1,0.);
  h->Fit("fline","qnlw");
  if (c2 && ij > 0) {c2->cd(ij+1); h->Draw(); c2->Update(); c1->cd(2);}
  //Loop on all found peaks. Eliminate peaks at the background level
  Double_t par[3000];
  par[0] = fline->GetParameter(0);
  par[1] = fline->GetParameter(1);
  npeaks = 0;
  Float_t *xpeaks = s->GetPositionX();
  Float_t ymax = 0;
  for (Int_t p=0;p<nfound;p++) {
    Float_t xp = xpeaks[p];
    Int_t bin = h->GetXaxis()->FindBin(xp);
    Float_t yp = h->GetBinContent(bin);
    if (yp-3*TMath::Sqrt(yp) < fline->Eval(xp)) continue;
    par[3*npeaks+2] = yp;
    if (yp > ymax) ymax = yp;
    par[3*npeaks+3] = xp;
    par[3*npeaks+4] = 3;
    npeaks++;
  }
  cout << "Found " << npeaks << " useful peaks to fit" << endl;
  Int_t NP = 0;
  Int_t nbad = 0;
  TString LineH("  {\"");  LineH +=  h->GetName(); LineH += "\"";
  TString Line("");
  struct ParErr_t {Double_t par, err;};
  ParErr_t parErr[10];
  TF1 *fit = 0;
  if (ymax > 2*par[0]) {
    cout << "Now fitting: Be patient" << endl;
    fit = new TF1("fit",fpeaks,0,1000,2+3*npeaks);
    TVirtualFitter::Fitter(h2,10+3*npeaks); //we may have more than the default 25 parameters
    fit->SetParameter(0,par[0]);
    fit->FixParameter(1,0.);
    for (Int_t p = 0; p < npeaks; p++) {
      fit->SetParName(3*p+2,Form("A%i",p));
      fit->SetParLimits(3*p+2,0,1e6);
      fit->SetParameter(3*p+2,par[3*p+2]);
      fit->SetParName(3*p+3,Form("#mu%i",p));
      fit->SetParLimits(3*p+3,TMath::Max(0.,par[3*p+3]-2), TMath::Min(240.,par[3*p+3]+2));
      fit->SetParameter(3*p+3,par[3*p+3]);
      fit->SetParName(3*p+4,Form("#sigma%i",p));
      fit->SetParLimits(3*p+4,0.01,20);
      fit->SetParameter(3*p+4,par[3*p+4]);
    }
    fit->SetNpx(1000);
    h2->SetStats(0);
    h2->Fit("fit","l");
    if (c2 && ij > 0) {c2->cd(ij); h2->Draw("same"); c2->Update(); c1->cd(2);}
    fit->GetParameters(par);
    for (Int_t p = 0; p<np;p++) {
      if (p < npeaks && par[3*p+2] > 5*fit->GetParError(3*p+2) && 
	  par[3*p+2] > par[0]) {
	if (TMath::Abs(par[3*p+4]) > 5.0) nbad++;
	//      Line += Form(",%f,%f,%7.2f,%5.2f",par[3*p+2],fit->GetParError(3*p+2),par[3*p+3],TMath::Abs(par[3*p+4]));
	parErr[NP].par = par[3*p+3];
	parErr[NP].err = TMath::Abs(par[3*p+4]);
	for (Int_t i = 0; i < NP; i++) {
	  if (parErr[i].par > parErr[NP].par) {
	    ParErr_t temp = parErr[i];
	    parErr[i] = parErr[NP];
	    parErr[NP] = temp;
	  }
	}
	NP++;
      } 
    }
  }
  for (Int_t p = 0; p < np; p++) {
    if (p < NP) Line += Form(",%7.2f,%5.2f",parErr[p].par,parErr[p].err);
    else  Line += ",0,0";
  }
  Line += "},";
  if (nbad > 1) NP = -NP; // reject whole hybrid
  LineH +=  Form(",%3i",NP);
  cout << LineH << Line << endl;
  out  << LineH << Line << endl;
  c1->Update();
  if (c2) c2->Update();
  delete fit;
  delete s;
}
//________________________________________________________________________________
void FindPeaks(Int_t np=10) {
  TString Out("Results.");
  Out += gSystem->BaseName(gDirectory->GetName());
  Out.ReplaceAll(".root","");
  Out.ReplaceAll(" ","");
  out.open(Out, ios::out);
  TIter nextkey(gDirectory->GetListOfKeys() );
  TKey *key;
  Int_t i = 0;
  Int_t nhists = 0;
  while ((key = (TKey*) nextkey())) {
    TObject *obj = key->ReadObj();
    if (! obj->IsA()->InheritsFrom( "TH1" ) ) continue;
    TH1 *h1 = (TH1*)obj;
    if (h1->GetEntries() < 1000) continue;
    nhists++;
  }
  Int_t nxy = nhists;
  Int_t nx =  (Int_t) TMath::Sqrt(nxy) + 1;
  Int_t ny = nxy/nx + 1;
  c2 = new TCanvas("Anodes","Anodes");
  c2->Divide(nx,ny);
  nextkey.Reset();
  Int_t histNo = 1;
  while ((key = (TKey*) nextkey())) {
    TObject *obj = key->ReadObj();
    if (! obj->IsA()->InheritsFrom( "TH1" ) ) continue;
    if ( obj->IsA()->InheritsFrom( "TH1C" ) ||
	 obj->IsA()->InheritsFrom( "TH1S" ) ||
	 obj->IsA()->InheritsFrom( "TH1I" ) ||
	 obj->IsA()->InheritsFrom( "TH1F" ) ) {
      cout << "Found histogram " << obj->GetName() << endl;
      TH1 *h1 = (TH1*)obj;
      if (h1->GetEntries() < 1000) {
	TString LineH("  {\"");  LineH +=  h1->GetName(); LineH += "\"";
	TString Line("");
	LineH +=  ",-99";
	for (Int_t p = 0; p < np; p++) Line += ",0,0";
	Line += "},// dead";
	cout << LineH << Line << endl;
	out  << LineH << Line << endl;
	continue;
      }
      peaks(h1,np,histNo);
      histNo++;
      //      break;
      if (i < 99) {
	cout << "Type something" << endl;
	cin >> i;
	if (i <= 0) break;
      }
    }
  }
  out.close();
}
