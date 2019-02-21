/* 
   root.exe -q -b MuTrackCount.C+
 */
#define __MuDst__
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
#include "TNtuple.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TCanvas.h"
#include "TVector3.h"
#include "TLorentzVector.h"
#include "TDirIter.h"
#include "TTreeIter.h"
#endif
TNtuple *FitP = 0;
//________________________________________________________________________________
void Print(Int_t RunOld, TH1F *NgT) {
  struct Fit_t {
    Float_t run;
    Float_t noevents;
    Float_t noev10;
    Float_t percent;
  };
  Fit_t Fit;
  memset(&Fit.run, 0, sizeof(Fit_t));
  Double_t NoEntries = NgT->GetEntries();
  if (NoEntries > 0) {
    Double_t NoHigh = NgT->Integral(11,500);
    cout << "Run " << RunOld << " no. of event " << NoEntries << " with No. Track > 10 : " << NoHigh/NoEntries*100 << " %%" << endl;
    Fit.run = RunOld;
    Fit.noevents = NoEntries;
    Fit.noev10 = NoHigh;
    Fit.percent = NoHigh/NoEntries*100;
    FitP->Fill(&Fit.run);
    NgT->Write();
  }
}
//________________________________________________________________________________
void MuTrackCount(const Char_t *files = "./*MuDst.root",
	   const Char_t *Out="NoTracks.root") { 
  TDirIter Dir(files);
  Char_t *file = 0;
  Int_t NFiles = 0;
  TTreeIter iter;
  while ((file = (Char_t *) Dir.NextFile())) {iter.AddFile(file); NFiles++;}
  TFile *fOut = new TFile(Out,"recreate");
  const Int_t*&      MuEvent_mRunInfo_mRunId                  = iter("MuEvent.mRunInfo.mRunId");
  TH1F *NgT = new TH1F("NgT","No. of Global tracks per event",500,0,500);
  TString VarN("run:noevents:noev10:percent");
  FitP = new TNtuple("FitP","Fit results",VarN);
  FitP->SetScanField(0);
  while ((file = (Char_t *) Dir.NextFile())) {iter.AddFile(file); NFiles++;}
  //#include "MuDst.h"
  const Int_t&       NoGlobalTracks                            = iter("GlobalTracks");
  Int_t RunOld = -1;
  while (iter.Next()) {
    if (MuEvent_mRunInfo_mRunId[0] != RunOld) {
      Print(RunOld, NgT); 
      RunOld = MuEvent_mRunInfo_mRunId[0];
      NgT->Reset();
      NgT->SetName(Form("NgT%i",RunOld));
    }
    NgT->Fill(NoGlobalTracks);
  }
  Print(RunOld, NgT); 
  fOut->Write();
}
/*
  ls -1d st_*.root | sed -e 's/st_//' -e 's/adc_//' -e 's/hltcosmic_//' -e 's/gmt_//' | awk -F_ '{print $1}' | sort -u
For Irakli
*      412 *  20044012 *      2847 *       621 * 21.812433 *
*      413 *  20044014 *      1433 *      1107 * 77.250526 *
*      417 *  20044022 *       637 *        59 * 9.2621660 *
*20044012* *20044014* *20044022*

awk '{print $4}' ~/scratch2/reco/2019/RF/BadRuns.16  | sort -u

root.exe NoTracks.17.root
FitP->Scan("*","percent>2||noevents<100"); >> BadRuns.17

awk '{print "*"$4"*"}' ~/scratch2/reco/2019/RF/BadRuns.17 | sort -u | xargs



 */
