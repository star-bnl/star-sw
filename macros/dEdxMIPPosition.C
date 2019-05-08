/* 
   root.exe -q -b lBichsel.C dEdxFit.C+ RunSummary.C+ 
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
#include "TDirIter.h"
#endif
void dEdxMIPPosition(const Char_t *files="20*.root") {
  TFile *fout = new TFile("dEdxMIPPosition.root","recreate");
  TString Tuple("Run:run:A:dA:mu:dmu:sigma:dsigma");
  cout << "Tuple: \t" << Tuple << endl;
  TNtuple *SumT = new TNtuple("FitP","dE/dx MIP position for each run",Tuple.Data()); 
  Int_t cachesize = 10000000; //this is the default value: 10 MBytes
  SumT->SetCacheSize(cachesize);
  Float_t params[10];; 
  TDataSet *set = 0; 
  Int_t run = 0;
  Int_t Run = 0;
  Int_t nFile = 0;
  TString title;
  TDirIter Dir(files);
  const Char_t *file = 0;
  while ( (file = Dir.NextFile()) ) {
    TString File = file;
    cout << "File::Name:" << File.Data() << endl;
    if (! File.BeginsWith("2") ) continue;
    cout << "Open " <<  File;
    TFile *f = new TFile(File.Data());
    if (! f) {cout << "====================== failed " << endl; continue;}
    TString F = gSystem->BaseName(File);
    sscanf(F.Data(),"%i",&run); 
    Run = run%1000000;
    cout << " for Run " << run << "/" << Run << "\t" << File << "\t" << nFile++ << endl;
    params[0] = run;
    params[1] = Run;
    if (Run <= 0) {
      cout << "Run has not bee recognized" << endl;
    } else {
      TH2F *TdEdxF = (TH2F*) f->Get("TdEdxF");
      if (! TdEdxF) {
	cout << "TdEdxF has not been found" << endl;
      } else {
	TdEdxF->GetXaxis()->SetRange(116,125);
	TH1D *proj = TdEdxF->ProjectionY();
	proj->Fit("gaus");
	TF1 *gaus = (TF1 *) proj->GetListOfFunctions()->FindObject("gaus");
	if (! gaus) {
	  cout << "gaus has not been found" << endl;
	} else {
	  for (Int_t i = 0; i < 3; i++) {
	    params[2+2*i] = gaus->GetParameter(i);    
	    params[3+2*i] = gaus->GetParError(i);    
	  }
	  SumT->Fill(params);
	}
      }
    }
    delete f;
  }
  fout->Write();
}
