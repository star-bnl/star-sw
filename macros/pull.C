#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TStyle.h"
#include "TF1.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TCanvas.h"
#include "TVector3.h"
#include "TLorentzVector.h"
#include "TDirIter.h"
#include "TTreeIter.h"
#endif
const Char_t *PlotNames[2] = {"Xpull","Ypull"};
const Char_t *PlotTitle[2] = {"X pull","Z pull"};
TCanvas *c1 = 0;
//________________________________________________________________________________
void Fit(const Char_t *dir = "") {
  TString fName = gSystem->BaseName(dir);
  ofstream out;
  TString Out("Results");
  //  Out += fName;
  if (gSystem->AccessPathName(Out)) out.open(Out, ios::out); //"Results.list",ios::out | ios::app);
  else                              out.open(Out, ios::app);
  
  cout << fName << endl;
  out << fName << endl;
  
  TString line, Name;
  TH1D *hist;
  TF1 *f = new TF1("g","gaus",-2,2);
  c1 = new TCanvas(dir,dir);
  c1->Divide(8,4);
  for (Int_t B = 1; B <= 4; B++) {
    line = Form("B%i |",B);
    for (Int_t N = 1; N <= 4; N++) {
      for (Int_t t = 0; t < 2; t++) {
	c1->cd(8*(B-1) + 2*(N-1) + t);
	Name = PlotNames[t]; Name += Form("B%iN%i",B,N);
	hist = (TH1D *) gDirectory->Get(Name);
	if (! hist) goto Empty;
	if (hist->GetEntries() < 1e3) goto Empty;
	hist->Fit(f,"");
	Double_t Sigma, dSigma, Mu;
	for (Int_t l = 0; l < 1; l++) {
	  if (! f) goto Empty;
	  Mu    =            f->GetParameter(1);
	  Sigma = TMath::Abs(f->GetParameter(2));
	  if (Sigma < 0.1) goto Empty;
	  hist->Fit(f,"r","",Mu-2*Sigma,Mu+2*Sigma);
	}
	Sigma = TMath::Abs(f->GetParameter(2));
	dSigma = f->GetParError(2);
	line += Form("%6.3f+-%5.3f|",Sigma,dSigma);
	continue;
      Empty:
	line += "             |";
      }
    }
    cout << line << endl;
    out << line << endl;
  }
  out.close();
}
//________________________________________________________________________________
void pull(const Char_t *dir =  "Svt_x_010mkm_z_030mkm_Ssd_x_030mkm_z_700mkm") {
  TString Out(gSystem->BaseName(dir));
  Out += "1.0GeV.root";
  TFile *fOut = new TFile(Out,"recreate");
  
  //        yz  B*N

  TH1D  *hist[2][4][5];

  TString Name, Title;
  for (Int_t t = 0; t < 2; t++) {
    for (Int_t B = 1; B <= 4; B++) {
      for (Int_t N = 1; N <= 5; N++) {
	Name = PlotNames[t];
	Title = PlotTitle[t];
	Name += Form("B%iN%i",B,N);
	if (N == 5)
	  Title += Form(" for barrel %i with total no. of Svt + Ssd hits >= %i",B,N);
	else 
	  Title += Form(" for barrel %i with total no. of Svt + Ssd hits = %i",B,N);
	hist[t][B-1][N-1] = new TH1D(Name,Title,200,-5,5);
      }
    }
  }
  TString Files(dir);
  Files += "/*.tags.root";
  TDirIter Dir(Files);
  Char_t *file = 0;
  Int_t NFiles = 0;
  TTreeIter iter("StiPulls");
  while ((file = (Char_t *) Dir.NextFile())) {iter.AddFile(file); NFiles++;}
  const Float_t *&mVtx          = iter("mVtx[3]");
  const Int_t    &NoHitsP       = iter("mHitsP");
  
  const Short_t *&nTpcHits      = iter("mHitsP.nTpcHits");
  const Short_t *&nSvtHits      = iter("mHitsP.nSvtHits");
  const Short_t *&nSsdHits      = iter("mHitsP.nSsdHits");
  const Short_t *&mDetector     = iter("mHitsP.mDetector");
  const UInt_t  *&mHardwarePosition = iter("mHitsP.mHardwarePosition");
  const Float_t *&lYPul         = iter("mHitsP.lYPul");
  const Float_t *&lZPul         = iter("mHitsP.lZPul");
  const Float_t *&lYPulErr      = iter("mHitsP.lYPulErr");
  const Float_t *&lZPulErr      = iter("mHitsP.lZPulErr");
  const Float_t *&pT            = iter("mHitsP.mPt");
  const Float_t *&dip            = iter("mHitsP.lDip");
  //         Now iterations
  while (iter.Next()) {
    //    if (TMath::Abs(mVtx[2]) > 20) continue;
    for (Int_t k = 0; k < NoHitsP; k++) {
      //      if (pT[k] < 1.0) continue;
      //      if (pT[k] < 0.5) continue;
      if (nTpcHits[k] < 25) continue;
      //      if (nSsdHits[k] <  1) continue;
      if (TMath::Abs(dip[k]) > 0.78) continue;
      Int_t l = -1;
      Int_t B = -1;
      if (mDetector[k] == 2) {
	l = 0;
	Int_t index = (mHardwarePosition[k]>>4)&((1L<<9)-1);
	if (index < 64)       B = 1;
	else if (index < 208) B = 2;
	else if (index < 432) B = 3;
      }
      else if (mDetector[k] == 8) {
	l = 1; 
	B = 4;
      }
      if (l < 0) continue;
      Int_t NoSvtSsd = nSvtHits[k] + nSsdHits[k];
      if (NoSvtSsd > 5) NoSvtSsd = 5;
      hist[0][B-1][NoSvtSsd-1]->Fill(lYPul[k]/lYPulErr[k]);
      hist[1][B-1][NoSvtSsd-1]->Fill(lZPul[k]/lZPulErr[k]);
    }
  }
  Fit(dir);
  fOut->Write();
}
