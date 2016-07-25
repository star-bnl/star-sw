// To build profile histograms: root.exe -q -b TpcT.C+
// To fit them                : root.exe -q *G.root FitTpcT.C
// To draw all of them        : root.exe */*Fit.root
//                              .L DrawList.C+
//                              DrawFAll()
#define PRINT
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TProfile2D.h"
#include "TF1.h"
#include "TF2.h"
#include "TLegend.h"
#include "Riostream.h"
#include "TSystem.h"
#include "TMath.h"
#include "TGraphErrors.h"
#include "TDirIter.h"
#include "TTreeIter.h"
#include "TDirectory.h"
#include "TROOT.h"
#include "TChain.h"
#include "TFile.h"
#include "TVector3.h"
#include "TRMatrix.h"
#include "TNtuple.h"
#endif

//--------------------------------------------------------------------------------
void TpcTonko(const Char_t *files="*.root", const Char_t *Out = ""){//, const Char_t *Time = "20090415.000000") {
  //	   Int_t ev, Double_t tanCut, Int_t NpadCut, Double_t pMomin, Double_t pMomax) {
  TDirIter Dir(files);
  Char_t *file = 0;
  Char_t *file1 = 0;
  Int_t NFiles = 0;
  TTreeIter iter("TpcT");
  while ((file = (Char_t *) Dir.NextFile())) {
    TString File(file);
    if (File.Contains("Plot") || File.Contains("Fit") || File.Contains("ADC") || File.Contains("Pads") || 
	File.Contains("hist") || File.Contains("tags") ||
	File.Contains("minimc") || File.Contains("event") ||
	File.Contains("All") ||
	File.Contains("MuDst")) continue;
    TFile *f = new TFile (File);
    if (! f) continue;
    TTree *tree = (TTree *) f->Get("TpcT");
    if (! tree ) continue;
    //    tree->Show(0);
    iter.AddFile(file); 
    NFiles++; 
    file1 = file;
    //    delete f;
  }
  cout << files << "\twith " << NFiles << " files" << endl; 
  const Int_t&       fNoPixels                                = iter("fNoPixels");
#ifndef __REAL_DATA__
  const Int_t&       fNoMcHit                                 = iter("fNoMcHit");
#endif
  const Int_t&       fNofPV                                   = iter("fNofPV");
#if 0
  const Int_t&       fNoTracksAtBestPV                        = iter("fNoTracksAtBestPV");
#endif
  const Float_t&     fxV                                      = iter("fxV");
  const Float_t&     fyV                                      = iter("fyV");
  const Float_t&     fzV                                      = iter("fzV");
  const Int_t&       fSector                                  = iter("fSector");
  const Int_t&       fRow                                     = iter("fRow");
  const Int_t&       fNoRcHit                                 = iter("fNoRcHit");
  const Int_t&       fNoRcTrack                               = iter("fNoRcTrack");
  const Int_t&       fAdcSum                                  = iter("fAdcSum");
#if 0
  const Int_t&       fPixels_                                 = iter("fPixels");
  const UChar_t*&    fPixels_mDetector                        = iter("fPixels.mDetector");
#endif
  const UChar_t*&    fPixels_mSector                          = iter("fPixels.mSector");
  const UChar_t*&    fPixels_mRow                             = iter("fPixels.mRow");
  const UChar_t*&    fPixels_mPad                             = iter("fPixels.mPad");
  const UShort_t*&   fPixels_mTimeBin                         = iter("fPixels.mTimeBin");
  const UShort_t*&   fPixels_mAdc                             = iter("fPixels.mAdc");
#ifdef PRINT
  const UShort_t*&   fPixels_mIdTruth                         = iter("fPixels.mIdTruth");
#endif
#ifndef __REAL_DATA__
  const Short_t*&    fPixels_mId                              = iter("fPixels.mId");
  const Float_t*&    fMcHit_mPosition_mX1                     = iter("fMcHit.mPosition.mX1");
  const Float_t*&    fMcHit_mPosition_mX2                     = iter("fMcHit.mPosition.mX2");
  const Float_t*&    fMcHit_mPosition_mX3                     = iter("fMcHit.mPosition.mX3");
#if 0
  const Float_t*&    fMcHit_mLocalMomentum_mX1                = iter("fMcHit.mLocalMomentum.mX1");
  const Float_t*&    fMcHit_mLocalMomentum_mX2                = iter("fMcHit.mLocalMomentum.mX2");
  const Float_t*&    fMcHit_mLocalMomentum_mX3                = iter("fMcHit.mLocalMomentum.mX3");
  const Float_t*&    fMcHit_mdE                               = iter("fMcHit.mdE");
  const Float_t*&    fMcHit_mdS                               = iter("fMcHit.mdS");
#endif
  const Long_t*&     fMcHit_mKey                              = iter("fMcHit.mKey");
#if 0
#ifdef PRINT
  const Long_t*&     fMcHit_mVolumeId                         = iter("fMcHit.mVolumeId");
#endif
#endif
  const Float_t*&    fMcHit_mAdc                           = iter("fMcHit.mAdc");
  const Float_t*&    fMcHit_mMcl_x                            = iter("fMcHit.mMcl_x");
  const Float_t*&    fMcHit_mMcl_t                            = iter("fMcHit.mMcl_t");
#endif
  const Float_t*&    fRcHit_mPosition_mX1                     = iter("fRcHit.mPosition.mX1");
  const Float_t*&    fRcHit_mPosition_mX2                     = iter("fRcHit.mPosition.mX2");
  const Float_t*&    fRcHit_mPosition_mX3                     = iter("fRcHit.mPosition.mX3");
#if 0
#ifdef PRINT
  const UInt_t*&     fRcHit_mHardwarePosition                 = iter("fRcHit.mHardwarePosition");
#endif
#endif
  const Float_t*&    fRcHit_mCharge                           = iter("fRcHit.mCharge");
  const Int_t*&      fRcHit_mId                               = iter("fRcHit.mId");
  const UShort_t*&   fRcHit_mIdTruth                          = iter("fRcHit.mIdTruth");
  const UShort_t*&   fRcHit_mQuality                          = iter("fRcHit.mQuality");
#if 0
  const UChar_t*&    fRcHit_mFitFlag                          = iter("fRcHit.mFitFlag");
  const UChar_t*&    fRcHit_mTrackRefCount                    = iter("fRcHit.mTrackRefCount");
  const UChar_t*&    fRcHit_mFlag                             = iter("fRcHit.mFlag");
#endif
  const UChar_t*&    fRcHit_mMinpad                           = iter("fRcHit.mMinpad");
  const UChar_t*&    fRcHit_mMaxpad                           = iter("fRcHit.mMaxpad");
  const UChar_t*&    fRcHit_mMintmbk                          = iter("fRcHit.mMintmbk");
  const UChar_t*&    fRcHit_mMaxtmbk                          = iter("fRcHit.mMaxtmbk");
  const Short_t*&    fRcHit_mMcl_x                            = iter("fRcHit.mMcl_x");
  const Short_t*&    fRcHit_mMcl_t                            = iter("fRcHit.mMcl_t");
  //  const Int_t*&      fRcTrack_fSector                         = iter("fRcTrack.fSector");
  //  const Int_t*&      fRcTrack_fRow                            = iter("fRcTrack.fRow");
#if 1
  const Short_t*&    fRcTrack_mMcl_x                          = iter("fRcTrack.mMcl_x");
  const Short_t*&    fRcTrack_mMcl_t                          = iter("fRcTrack.mMcl_t");
#endif
#if 0
  const Int_t*&      fRcTrack_fNpoints                        = iter("fRcTrack.fNpoints");
  const Int_t*&      fRcTrack_fNfitpoints                     = iter("fRcTrack.fNfitpoints");
  const Int_t*&      fRcTrack_fifPrim                         = iter("fRcTrack.fifPrim");
#endif
  const Float_t*&    fRcTrack_fpx                             = iter("fRcTrack.fpx");
  const Float_t*&    fRcTrack_fpy                             = iter("fRcTrack.fpy");
  const Float_t*&    fRcTrack_fpz                             = iter("fRcTrack.fpz");
#if 0
  const Float_t*&    fRcTrack_fdEdx                           = iter("fRcTrack.fdEdx");
#endif
  const Float_t*&    fRcTrack_fTrackLength70                  = iter("fRcTrack.fTrackLength70");
  while (iter.Next()) {
    if (! fNoPixels ) continue;
    cout << Form("sec: %3i row: %3i ===============================",fSector,fRow) << endl;
    for (Int_t i = 0; i < fNoPixels; i++) {
      cout << Form("pixel %5i pad: %4i time: %4i Adc: %5i Id: %5i ",
		   fPixels_mId[i],fPixels_mPad[i],fPixels_mTimeBin[i],fPixels_mAdc[i],fPixels_mIdTruth[i]) << endl;
    }
    for (Int_t i = 0; i < fNoMcHit; i++) {
      cout << Form("McHit: Id %5i x %8.2f y %8.2f z %8.2f pad %6.2f time %6.2f Adc %6.2f", 
		   (Int_t)fMcHit_mKey[i],
		   fMcHit_mPosition_mX1[i],fMcHit_mPosition_mX2[i],fMcHit_mPosition_mX3[i],
		   fMcHit_mMcl_x[i],fMcHit_mMcl_t[i],fMcHit_mAdc[i]) << endl;
    }
    for (Int_t i = 0; i < fNoRcHit; i++) {
      Float_t pad = ((Float_t) fRcHit_mMcl_x[i])/64.;
      Float_t time= ((Float_t) fRcHit_mMcl_t[i])/64.;
      cout << Form("RcHit: Id %5i x %8.2f y %8.2f z %8.2f pad %6.2f time %6.2f q %6.2f (keV)", 
		   fRcHit_mId[i],
		   fRcHit_mPosition_mX1[i],fRcHit_mPosition_mX2[i],fRcHit_mPosition_mX3[i],
		   pad,time,1e6*fRcHit_mCharge[i])
	   << Form(" IdTruth %5i QA %4i pads [%3i,%3i] tb [%3i,%3i]",
		   fRcHit_mIdTruth[i],fRcHit_mQuality[i],
		   (Int_t)(TMath::Nint(pad)-fRcHit_mMinpad[i]),
		   (Int_t)(TMath::Nint(pad)+fRcHit_mMaxpad[i]),
		   (Int_t)(TMath::Nint(time)-fRcHit_mMintmbk[i]),
		   (Int_t)(TMath::Nint(time)+fRcHit_mMaxtmbk[i]))
		   << endl;
    }
  } // end of print
}
