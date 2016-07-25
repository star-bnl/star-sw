#ifndef __CINT__
#include <iostream.h>
#include "TSystem.h"
#include "TMath.h"
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
#include "TMinuit.h"
#include "TCanvas.h"
#include "StFlowMaker/StFlowPicoEvent.h"
#include "StFlowMaker/StFlowPicoTrack.h" 
#include "BetheBloch.h"
#else
class StFlowPicoEvent;
class StFlowPicoTrack;
class StuFraction;
class TMinuit;
class TF1;
class TH1F;
class TH3F;
class TH2D;
class TChain;
class TCanvas;
#endif
TFile  *outfile = 0;
TChain *fChain = 0;
const Int_t NHYPS = 4;
Char_t *Names[5] = {"p",
		    "K",
		    "pi",
		    "e",
		    "Unknown"
		    //			"mu",
		    //			"d"
};
Double_t Masses[4] = {0.93827231,
		      0.493677,
		      0.13956995,
		      0.51099907e-3
		      //			  0.1056583568,
		      //			  1.87561339
};
//________________________________________________________________________________
void rich(Int_t nevents=10000, TChain* fChain=0)
{
  if (!fChain) return;
  Char_t line[80];
  sprintf(line,"rich%iK.root",nevents/1000);
  TString OutFile(line);
  outfile = new TFile(OutFile.Data(),"recreate");
  TProfile *mu = new TProfile("mu","average no. of photons versus length",50,0,1,0,20);
  TProfile *sigma = new TProfile("sigma","sigma of photons versus length",50,0,1,0,20,"s");
  StFlowPicoEvent *pPicoEvent = new StFlowPicoEvent();
  fChain->SetBranchAddress("pPicoEvent", &pPicoEvent);
  Int_t nbytes = 0, nb = 0, ierr = 0, nevt = 0;
  Int_t nentries = Int_t(fChain->GetEntries());
  if (nevents > 0 && nentries > nevents) nentries = nevents;
  for (Int_t jentry=0; jentry<nentries;jentry++) {
    //in case of a TChain, ientry is the entry number in the current file
    fChain->LoadTree(jentry); 
    nb = fChain->GetEntry(jentry);   nbytes += nb;
    for (Int_t kTrack=0; kTrack < pPicoEvent->GetNtrack(); kTrack++) {
      StFlowPicoTrack* pPicoTrack = (StFlowPicoTrack*)pPicoEvent->Tracks()->UncheckedAt(kTrack);
      if (pPicoTrack->RichRegistrated(0) > 0) {
#if 0
      cout << "Ev:\t" << jentry << "\tTrk:\t" << kTrack << "\tpT:\t" 
	   << pPicoTrack->Charge()* pPicoTrack->Pt() << "\tok:" << ok 
	   << "\tEta:" << pPicoTrack->Eta()   
	   << "\tp:" << prob[0] // << "/" << frac->ProtonProb()
	   << "\tK:" << prob[1] // << "/" << frac->KaonProb()
	   << "\tpi:" << prob[2] // << "/" << frac->PionProb();
	   << "\te:" << prob[3] // << "/" <<  frac->ElectronProb()
	   << "\tsum:" << frac->Sum() << endl;
#endif
      mu->Fill(pPicoTrack->RichExpected(0),pPicoTrack->RichRegistrated(0));
      sigma->Fill(pPicoTrack->RichExpected(0),pPicoTrack->RichRegistrated(0));
      }
    }
    if (jentry%1000 == 1) cout << "Read event no. " << jentry << endl;
  }
  outfile->Write();
}
