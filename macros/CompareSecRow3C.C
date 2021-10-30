#include "Ask.h"
#include "Riostream.h"
#include "TDirIter.h"
#include "TSystem.h"
#include "TFile.h"
#include "TH1.h"
#include "TH3.h"
#include "TString.h"
#include "TCanvas.h"
//________________________________________________________________________________
void CompareSecRow3C(const Char_t *dir1 = "P21ic_calib_51/*GeV*/20*.root",
		     const Char_t *dir2 = "P21ic_calib_63/*GeV*/20*.root") {
  TCanvas *c1 = new TCanvas();
  TDirIter Dir1(dir1);
  TDirIter Dir2(dir2);
  TString path1 = Dir1.NextFile();  
  TString path2 = Dir2.NextFile();  
  cout << "path1 = " << path1.Data() << endl;
  cout << "path2 = " << path2.Data() << endl;
  while (1) {
    if (path1 == "" || path2 == "") return;
    cout << "path1 = " << path1.Data() << endl;
    cout << "path2 = " << path2.Data() << endl;
    if (! path1 || ! path2) break;
    TString file1(gSystem->BaseName(path1));
    TString file2(gSystem->BaseName(path2));
    Int_t iok = file1.CompareTo(file2); 
    if (iok < 0) {
      path1 = Dir1.NextFile();
    } else if (iok > 0) {
      path2 = Dir2.NextFile();
    } else {
      TFile *f1 = new TFile(path1);
      TH3F *SecRow3C1 = (TH3F *) f1->Get("SecRow3C");
      TFile *f2 = new TFile(path2);
      TH3F *SecRow3C2 = (TH3F *) f2->Get("SecRow3C");
      if (SecRow3C1 && SecRow3C2) {
	TH1 *h1 = SecRow3C1->Project3D("z1"); h1->Draw();
	TH1 *h2 = SecRow3C2->Project3D("z2"); h2->SetLineColor(2); h2->Draw("sames");
	c1->Update();
	Double_t test = h1->KolmogorovTest(h2);
	cout << "KolmogorovTest = " << test;
	if (TMath::Abs(test-1) > 1e-4) cout << "\tFailed";
	cout << endl;
	if (TMath::Abs(test-1) > 1e-4 && ! gROOT->IsBatch() && Ask()) return;
	path1 = Dir1.NextFile();
	path2 = Dir2.NextFile();
      }
      delete f1;
      delete f2;
      cout << "================================================================================" << endl;
    }
  }
}
