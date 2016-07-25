#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include "TSystem.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TList.h"
#include "TIterator.h"
#include "tables/St_tpcGain_Table.h"
#else
class  St_tpcGain;
#endif 
void GainSigma(){ 
#if defined(__CINT__) && ! defined(__MAKECINT__)
  gSystem->Load("libTable");
  gSystem->Load("libStDb_Tables");
#endif
  struct Gain_t {
    Float_t sector;
    Float_t row;
    Float_t pad;
    Float_t N;
    Float_t Gain;
    Float_t Sigma;
  };
  Int_t NtpcGain = 0;
  St_tpcGain *tpcGain[100];
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  TIter  next(files);
  TFile *f = 0;
  Int_t i = 0;
  while ((f = (TFile *) next())) {
    cout << "Source file " << i++ << ": " << f->GetName() << endl;
    tpcGain[NtpcGain] = (St_tpcGain *) f->Get("tpcGain");
    if (tpcGain[NtpcGain]) NtpcGain++;
  }
  if (! NtpcGain) return;
  f = new TFile("GainSigma.root","RECREATE");
  TNtuple *ntuple = new TNtuple("GainNt","tpcGain sigmas",
		       "sector:row:pad:N:Gain:Sigma");
  tpcGain_st *g;
  Gain_t G;
  Int_t N = 0;
  for (int sector=0;sector<24; sector++) {
    for (int row = 0; row<45; row++) {
      for (int pad = 0; pad < 182; pad++) {
	G.N = 0; 
	Double_t average = 0;
	Double_t Sigma = 0;
	for (int l=0; l<NtpcGain; l++) {
	  g = tpcGain[l]->GetTable()+sector;
	  if (g->Gain[row][pad] <= 0) continue;
	  G.N++;
	  average += g->Gain[row][pad];
	  Sigma   += g->Gain[row][pad]*g->Gain[row][pad];
	}
	if (G.N <= 0.0) continue;
	G.sector = sector+1;
	G.row    = row+1;
	G.pad    = pad+1;
        average   /= G.N;
	Sigma  /= G.N;
	Sigma  -= average*average;
	G.Gain = average;
	G.Sigma = TMath::Sqrt(Sigma);
	N++;
	if (N%1000 == 1) cout << G.sector << "\t" << G.row << "\t" << G.pad 
			      << "\t" << G.N << "\t" << average << "\t" << Sigma << endl;
	ntuple->Fill(&G.sector);
      }
    }
  }
  f->Write();
}
