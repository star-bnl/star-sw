/*
  root.exe VoltageScan.C+
 */
#if !defined(__CINT__) || defined(__MAKECINT__)
#include <assert.h>
#include "Riostream.h"
#include <stdio.h>
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TProfile.h"
#include "TStyle.h"
#include "TF1.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TCanvas.h"
#include "TMinuit.h"
#include "TSpectrum.h"
#include "TString.h"
#include "TLine.h"
#include "TText.h"
#include "TROOT.h"
#include "TList.h"
#include "TPolyMarker.h"
#include "StBichsel/Bichsel.h"
#include "TDirIter.h"
#include "TTree.h"
#include "TTreeIter.h"
#include "TRandom.h"
#include "TFractionFitter.h"
#include "TLegend.h"
#include "TRVector.h"
#include "TRSymMatrix.h"
#include "StDcaGeometry.h"
#include "TROOT.h"
#include "TSystem.h"
//#include "StDedxPidTraits.h"
#include "StEnumerations.h"
#include "Ask.h"
#else
class TSystem;
class TMath;
class TH1;
class TH2;
class TH3;
class TProfile;
class TStyle;
class TF1;
class TTree;
class TChain;
class TFile;
class TNtuple;
class TCanvas;
class TMinuit;
class TSpectrum;
class TString;
class TLine;
class TText;
class TROOT;
class TList;
class TPolyMarker;
class Bichsel;
class TDirIter;
class TTreeIter;
#endif
//________________________________________________________________________________
void VoltageScan(const Char_t *files = "./VoltageCGF*.root") {
  TDirIter Dir(files);
  Char_t *file = 0;
  Int_t NFiles = 0;
  while ((file = (Char_t *) Dir.NextFile())) {
    TFile *f = TFile::Open(file);
    if (! f) {cout << "Can't open file " << file << endl; continue;}
    TNtuple *FitP = (TNtuple *) f->Get("FitP");
    if (! FitP) {cout << "No FitP in file " << file << endl; delete f; continue;}
    FitP->SetScanField(192);
    NFiles++;
    Long64_t n = FitP->Draw("mu:y>>V","i&&j&&abs(mu)>0.15","goff");
    if (n) {
      TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
      if (c1) c1->Clear();
      else    c1 = new TCanvas("c1","c1",600,600);
      TH2 *V = (TH2 *) gDirectory->Get("V");
      if (V) {
	V->Draw();
	c1->Update();
	cout << gDirectory->GetName() << endl;
	FitP->Scan("i:(i-(i-1)%8-1)/8+1:(i-1)%8+1:y:mu","i&&j&&abs(mu)>0.10","entry:sector:channel:V:mu",192);
	if (! gROOT->IsBatch()) {
	  if (Ask()) break;
	}
	delete V;
      }
    }
    delete  f;
  }
  cout << files << "\twith " << NFiles << " files" << endl; 
}
