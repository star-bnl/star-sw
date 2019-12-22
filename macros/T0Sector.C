#include "Riostream.h"
#include "TMath.h"
#include "TDirIter.h"
#include "TFile.h"
#include "TTree.h"
#include "TString.h"
//________________________________________________________________________________
// Averge over 2019 runs
Double_t T0Sector(Double_t sector, Double_t row) {
  Double_t corrO[24] = {-0.0464,-0.0380,-0.0310,-0.0384,-0.0373,-0.0427,-0.0738,-0.0802,-0.0228,-0.0436,-0.0348,-0.0324,
			-0.0218,-0.0176,-0.0230,-0.0680,-0.1049,-0.1054,-0.0036,-0.0141,-0.0289,-0.0227,-0.0378,-0.0329};
  
  Double_t corrI[24] = { 5.5858, 5.6228, 5.6027, 6.5901, 5.6026, 5.6288, 5.5745, 5.5609, 5.6045, 6.5544, 5.5944, 5.5964, 
			 5.5831, 5.6027, 5.5887, 5.7706, 5.6311, 5.6188, 5.6331, 5.5984, 5.5914, 5.5844, 5.5833, 6.5463};
  Int_t s = TMath::Nint(sector);
  Int_t r = TMath::Nint(row);
  Double_t T0 = 3.31351;
  if (r > 40) T0 += corrO[s-1];
  else        T0 += corrI[s-1];
  //  cout << "s = " << s << "\tr = " << r << "\tT0 = " << T0 << endl;
  return T0;
}
//________________________________________________________________________________
void LoopOverFiles(const Char_t *files = "TGP20*TpcHitZTMfl0.root") {
  TDirIter Dir(files);
  Char_t *file = 0;
  Int_t NFiles = 0;
  TFile *f = 0;
  while ((file = (Char_t *) Dir.NextFile())) {
#if 0
    cout << "Try to open " << file << endl;
#endif
    f = new TFile(file,"update");// f = new TFile(file);
#if 0
    cout << "File\t" << NFiles <<"\t" << file; 
    if (! f)                cout << " Can't be opened"; 
    else if (f->IsZombie()) cout << " is Zombie";
    else                    cout << " is o.k.";
    cout << endl;
#endif
    TString Run(file);
    Run.ReplaceAll("TGP","");
    Run.ReplaceAll("TpcHitZTMfl0.root","");
    TTree *FitP = (TTree *) f->Get("FitP");
    Int_t nFound = FitP->Draw("mu-T0Sector(x,y):dmu:100*i+j","dmu>1e-3&&dmu<0.02&&mu<20&&i&&j&&chisq>0.1&&chisq<200&&sigma<0.6&&sigma>0.1","goff");
    for (Int_t i = 0; i < nFound; i++) {
      Double_t dT = FitP->GetV1()[i];
      Double_t sigmadT = FitP->GetV2()[i];
      Int_t    sec     = FitP->GetV3()[i];
      if (TMath::Abs(dT) > 10*TMath::Sqrt(sigmadT*sigmadT + 0.06*0.06)) {
	cout << Run.Data() << Form("\tsector = %2i row = %2i",sec/100,sec%100) << "\t" << dT << " +/- " << sigmadT <<endl;
      }
    }
    delete f;
    NFiles++;
  }
}
