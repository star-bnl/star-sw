#define TpcT_cxx
#include "TpcT.h"
#include <TH2.h>
#include <TStyle.h>
#include <TCanvas.h>
#include "TProfile.h"
#include "Riostream.h"
void TpcT::Loop(Int_t ev) {
//   In a ROOT session, you can do:
//      Root > .L TpcT.C
//      Root > TpcT t
//      Root > t.GetEntry(12); // Fill t data members with entry number 12
//      Root > t.Show();       // Show values of entry 12
//      Root > t.Show(16);     // Read and show values of entry 16
//      Root > t.Loop();       // Loop on all entries
//

//     This is the loop skeleton where:
//    jentry is the global entry number in the chain
//    ientry is the entry number in the current Tree
//  Note that the argument to GetEntry must be:
//    jentry for TChain::GetEntry
//    ientry for TTree::GetEntry and TBranch::GetEntry
//
//       To read only selected branches, Insert statements like:
// METHOD1:
//    fChain->SetBranchStatus("*",0);  // disable all branches
//    fChain->SetBranchStatus("branchname",1);  // activate branchname
// METHOD2: replace line
//    fChain->GetEntry(jentry);       //read all branches
//by  b_branchname->GetEntry(ientry); //read only this branch
  if (fChain == 0) return;
  //  fChain->SetMarkerStyle(20);
  //  fChain->SetMarkerColor(opt%10);
  static Int_t nx = 320;
  static Double_t xmin = -10.;
  static Double_t xmax =  10.;
  TProfile *OuterPadRc = (TProfile *) gDirectory->Get("OuterPadRc");
  Int_t color = 1;
  if (! OuterPadRc) {
    OuterPadRc = new TProfile("OuterPadRc","OuterPadRc",nx,xmin,xmax,""); 
    OuterPadRc->SetMarkerStyle(20); 
    OuterPadRc->SetMarkerColor(color++);
  }
  TProfile *OuterPadMc = (TProfile *) gDirectory->Get("OuterPadMc");
  if (! OuterPadMc) {
    OuterPadMc = new TProfile("OuterPadMc","OuterPadMc",nx,xmin,xmax,""); 
    OuterPadMc->SetMarkerStyle(20); 
    OuterPadMc->SetMarkerColor(color++);
  }
  TProfile *InnerPadRc = (TProfile *) gDirectory->Get("InnerPadRc");
  if (! InnerPadRc) {
    InnerPadRc = new TProfile("InnerPadRc","InnerPadRc",nx,xmin,xmax,""); 
    InnerPadRc->SetMarkerStyle(20); 
    InnerPadRc->SetMarkerColor(color++);
  }
  TProfile *InnerPadMc = (TProfile *) gDirectory->Get("InnerPadMc");
  if (! InnerPadMc) {
    InnerPadMc = new TProfile("InnerPadMc","InnerPadMc",nx,xmin,xmax,""); 
    InnerPadMc->SetMarkerStyle(20); 
    InnerPadMc->SetMarkerColor(color++);
  }
  TProfile *OuterTimeRc = (TProfile *) gDirectory->Get("OuterTimeRc");
  if (! OuterTimeRc) {
    OuterTimeRc = new TProfile("OuterTimeRc","OuterTimeRc",nx,xmin,xmax,""); 
    OuterTimeRc->SetMarkerStyle(20); 
    OuterTimeRc->SetMarkerColor(color++);
  }
  TProfile *OuterTimeMc = (TProfile *) gDirectory->Get("OuterTimeMc");
  if (! OuterTimeMc) {
    OuterTimeMc = new TProfile("OuterTimeMc","OuterTimeMc",nx,xmin,xmax,""); 
    OuterTimeMc->SetMarkerStyle(20); 
    OuterTimeMc->SetMarkerColor(color++);
  }
  TProfile *InnerTimeRc = (TProfile *) gDirectory->Get("InnerTimeRc");
  if (! InnerTimeRc) {
    InnerTimeRc = new TProfile("InnerTimeRc","InnerTimeRc",nx,xmin,xmax,""); 
    InnerTimeRc->SetMarkerStyle(20); 
    InnerTimeRc->SetMarkerColor(color++);
  }
  TProfile *InnerTimeMc = (TProfile *) gDirectory->Get("InnerTimeMc");
  if (! InnerTimeMc) {
    InnerTimeMc = new TProfile("InnerTimeMc","InnerTimeMc",nx,xmin,xmax,""); 
    InnerTimeMc->SetMarkerStyle(20); 
    InnerTimeMc->SetMarkerColor(color++);
  }
  Int_t nentries = Int_t(fChain->GetEntriesFast());
  Int_t jentry=0;
  if (ev >= 0) { jentry = ev; nentries = jentry + 1;}
  
  Int_t nbytes = 0, nb = 0;
  for (; jentry<nentries;jentry++) {
    Int_t ientry = LoadTree(jentry);
    if (ientry < 0) break;
    nb = fChain->GetEntry(jentry);   nbytes += nb;
    if (! fNoPixels || fNoPixels >kMaxfPixels) continue;
    if (fNoPixels > 80) continue;
    if (fRcHit_mCharge[0] < 1.e-6 || fRcHit_mCharge[0] > 100.e-6) continue;
    if (ientry%100 == 1) {
      cout << ientry << "\t =======================================================" << endl;
      Int_t kPadMin = 999;
      Int_t kPadMax =   0;
      Int_t kTbMin  = 999;
      Int_t kTbMax  =   0;
      Int_t pad  = 0;
      Int_t tb   = 0;
      Int_t indx = 0;
      for (int i = 0; i < fNoPixels; i++) {
	pad = fPixels_mPad[i];
	tb  = fPixels_mTimeBin[i];
	if (tb > 512) continue;
	if (pad > 200) continue;
	if (pad < kPadMin) kPadMin = pad;
	if (pad > kPadMax) kPadMax = pad;
	if (tb  < kTbMin) kTbMin = tb;
	if (tb  > kTbMax) kTbMax = tb;
      }
      Int_t *adcs = new Int_t[(kPadMax-kPadMin+2)*(kTbMax-kTbMin+2)];
      memset (adcs, 0, (kPadMax-kPadMin+2)*(kTbMax-kTbMin+2)*sizeof(Int_t));
      for  (int i = 0; i < fNoPixels; i++) {
	pad = fPixels_mPad[i];
	tb  = fPixels_mTimeBin[i];
	if (tb > 512) tb = kTbMax + 1;
	if (pad > 200) pad = kPadMax + 1;
	indx = (kTbMax-kTbMin+2)*(pad - kPadMin) + tb - kTbMin;
	adcs[indx] = fPixels_mAdc[i];
      }
      cout << " ";
      for (tb = kTbMin; tb <= kTbMax + 1; tb++) cout << "\t|" << tb;
      cout << endl;
      cout << "_";
      for (tb = kTbMin; tb <= kTbMax + 1; tb++) cout << "\t|____";
      cout << endl;
      for (pad = kPadMin; pad <= kPadMax + 1; pad++) {
	if (pad == kPadMax + 1) cout << "---------------------------------------------" << endl;
	cout << pad << "|";
	for (tb = kTbMin; tb <= kTbMax + 1; tb++) {
	  indx = (kTbMax-kTbMin+2)*(pad - kPadMin) + tb - kTbMin;
	  if (tb <= kTbMax) cout << "\t" << adcs[indx];
	  else              cout << "\t|" << adcs[indx];
	}
	cout << endl;
      }
      delete [] adcs;
      for (int i = 0; i < fNoPixels; i++) 
	cout << i 
	     << "\tRow\t" << (int) fPixels_mRow[i]
	     << "\tPad\t" << (int) fPixels_mPad[i]
	     << "\tTimeBin\t" << (int) fPixels_mTimeBin[i]
	     << "\tadc\t" << (int) fPixels_mAdc[i]
	     << "\tId\t" << (int) fPixels_mIdTruth[i]
	     << "\tR\t" << ((Double_t) fPixels_mAdc[i])/((Double_t) fAdcSum)
	     << endl;
      for (int i = 0; i < fNoRcPad; i++) 
	cout << "Rc\t" << i << "\tRow\t" << fRcPad_fRow[i]
	     << "\tPad\t" << fRcPad_fPad[i]
	     << "\tTimeBin\t" << fRcPad_fTimeBin[i]
	     << "\tdX\t" << fRcPad_fdX[i]
	     << "\tdZ\t" << fRcPad_fdZ[i] 
	     << endl;
      for (int i = 0; i < fNoMcPad; i++) 
	cout << "Mc\t" << i << "\tRow\t" << fMcPad_fRow[i]
	     << "\tPad\t" << fMcPad_fPad[i]
	     << "\tTimeBin\t" << fMcPad_fTimeBin[i]
	     << "\tdX\t" << fMcPad_fdX[i]
	     << "\tdZ\t" << fMcPad_fdZ[i] 
	     << endl;
    }
    // if (Cut(ientry) < 0) continue;
    if (fNoRcPad != 1) continue;
    if (fAdcSum < 100 || fAdcSum > 2.e4) continue;
    for (int i = 0; i < fNoPixels; i++) {
      if (fPixels_mTimeBin[i] > 512) {
	if (fPixels_mRow[i] > 13) {
	  OuterPadRc->Fill(fPixels_mPad[i]-(fRcPad_fPad[0]+fRcPad_fdX[0]),
			   ((Double_t) fPixels_mAdc[i])/((Double_t) fAdcSum));
	  if (fNoMcPad == 1)
	    OuterPadMc->Fill(fPixels_mPad[i]-(fMcPad_fPad[0]+fMcPad_fdX[0]),
			   ((Double_t) fPixels_mAdc[i])/((Double_t) fAdcSum));
	} else {
	  InnerPadRc->Fill(fPixels_mPad[i]-(fRcPad_fPad[0]+fRcPad_fdX[0]),
			   ((Double_t) fPixels_mAdc[i])/((Double_t) fAdcSum));
	  if (fNoMcPad == 1)
	  InnerPadMc->Fill(fPixels_mPad[i]-(fMcPad_fPad[0]+fMcPad_fdX[0]),
			   ((Double_t) fPixels_mAdc[i])/((Double_t) fAdcSum));
	}
      }
      if (fPixels_mPad[i] > 200) {
	if (fPixels_mRow[i] > 13) {
	  OuterTimeRc->Fill(fPixels_mTimeBin[i]-(fRcPad_fTimeBin[0]+fRcPad_fdZ[0]),
			    ((Double_t) fPixels_mAdc[i])/((Double_t) fAdcSum));
	  if (fNoMcPad == 1)
	  OuterTimeMc->Fill(fPixels_mTimeBin[i]-(fMcPad_fTimeBin[0]+fMcPad_fdZ[0]),
			    ((Double_t) fPixels_mAdc[i])/((Double_t) fAdcSum));
	} else {
	  InnerTimeRc->Fill(fPixels_mTimeBin[i]-(fRcPad_fTimeBin[0]+fRcPad_fdZ[0]),
			    ((Double_t) fPixels_mAdc[i])/((Double_t) fAdcSum));
	  if (fNoMcPad == 1)
	  InnerTimeMc->Fill(fPixels_mTimeBin[i]-(fMcPad_fTimeBin[0]+fMcPad_fdZ[0]),
			    ((Double_t) fPixels_mAdc[i])/((Double_t) fAdcSum));
	}
      }
    }
  }
}
