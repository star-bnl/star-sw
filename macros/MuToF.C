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
#define PrP(x) cout << "\t" << (#x) << " = " << (x);
#endif
#define DEBUG
//________________________________________________________________________________
void MuToF(const Char_t *files =  "./tpt/*.MuDst.root", const Char_t *Out="tof.root") {
  TFile *fOut = new TFile(Out,"recreate");
  TH2D *PrVsGl = new TH2D("PrVsGl","No. of Primary tracks versus No. of Globals",
			    100,0,5000., 100,0.,2500.);
  TH1D *K0mass = new TH1D("K0mass","mass of K0",200,.400,.600);
  TDirIter Dir(files);
  Char_t *file = 0;
  Int_t NFiles = 0;
  TTreeIter iter;
  while ((file = (Char_t *) Dir.NextFile())) {iter.AddFile(file); NFiles++;}
  const Int_t    &NoTracksGl       = iter("GlobalTracks");
  const UChar_t *&NHitsGl          = iter("GlobalTracks.mNHits");   
  const UChar_t *&NHitsDedxGl      = iter("GlobalTracks.mNHitsDedx");   
  const Float_t *&dEdxGl           = iter("GlobalTracks.mdEdx");   
  const Float_t *&dEdxFitGl        = iter("GlobalTracks.mProbPidTraits.mdEdxFit");   
  const Float_t *&pTGl             = iter("GlobalTracks.mPt");   
  const Float_t *&EtaGl            = iter("GlobalTracks.mEta");   

  const Int_t    &NoTracksPr = iter("PrimaryTracks");
  const UChar_t *&NHitsPr          = iter("PrimaryTracks.mNHits");   
  const UChar_t *&NHitsDedxPr      = iter("PrimaryTracks.mNHitsDedx");   
  const Float_t *&dEdxPr           = iter("PrimaryTracks.mdEdx");   
  const Float_t *&dEdxFitPr        = iter("PrimaryTracks.mProbPidTraits.mdEdxFit");   
  const Float_t *&pTPr             = iter("PrimaryTracks.mPt");   
  const Float_t *&EtaPr            = iter("PrimaryTracks.mEta");   
  const Float_t *&PhiPr            = iter("PrimaryTracks.mPhi");
  const Short_t *&FlagPr           = iter("PrimaryTracks.mFlag");
  const Int_t   *&InPr2Gl          = iter("PrimaryTracks.mIndex2Global");
  const Int_t   &NoV0              = iter("V0");
  const Float_t *&mMomPosX  = iter("V0.mMomPosX");
  const Float_t *&mMomPosY  = iter("V0.mMomPosY");
  const Float_t *&mMomPosZ  = iter("V0.mMomPosZ");
  const Float_t *&mMomNegX  = iter("V0.mMomNegX");
  const Float_t *&mMomNegY  = iter("V0.mMomNegY");
  const Float_t *&mMomNegZ  = iter("V0.mMomNegZ");
  const Int_t    &NoTofHit  = iter("TofHit");
  const Int_t   *&mIconf       = iter("TofHit.mIconf");
  const Int_t   *&mTrayIndex   = iter("TofHit.mTrayIndex");
  const Int_t   *&mModuleIndex = iter("TofHit.mModuleIndex");
  const Int_t   *&mCellIndex   = iter("TofHit.mCellIndex");
  const Int_t   *&mDaqIndex    = iter("TofHit.mDaqIndex");
  const Int_t   *&mADC         = iter("TofHit.mADC");
  const Float_t *&mTimeOfFlight= iter("TofHit.mTimeOfFlight");
  const Float_t *&mPathLength  = iter("TofHit.mPathLength");
  const Float_t *&mBeta        = iter("TofHit.mBeta");
  const Int_t   *&mAssociatedTrackId = iter("TofHit.mAssociatedTrackId");
  const Float_t *&mProjectedPoint_mX1 = iter("TofHit_mProjectedPoint.mX1");
  const Float_t *&mProjectedPoint_mX2 = iter("TofHit_mProjectedPoint.mX2");
  const Float_t *&mProjectedPoint_mX3 = iter("TofHit_mProjectedPoint.mX3");
  const Float_t *&mTOFExpectedAsElectron = iter("TofHit.mTOFExpectedAsElectron");
  const Float_t *&mTOFExpectedAsPion = iter("TofHit.mTOFExpectedAsPion");
  const Float_t *&mTOFExpectedAsKaon = iter("TofHit.mTOFExpectedAsKaon");
  const Float_t *&mTOFExpectedAsProton = iter("TofHit.mTOFExpectedAsProton");
  const Float_t *&mSigmaElectron = iter("TofHit.mSigmaElectron");
  const Float_t *&mSigmaPion = iter("TofHit.mSigmaPion");
  const Float_t *&mSigmaKaon = iter("TofHit.mSigmaKaon");
  const Float_t *&mSigmaProton = iter("TofHit.mSigmaProton");
  const Int_t   *&mParticleHypothesis = iter("TofHit.mParticleHypothesis");
  const Int_t    &NoTofData = iter("TofData");
  const UShort_t *&mDataIndex = iter("TofData.mDataIndex");
  const UShort_t *&mAdc = iter("TofData.mAdc");
  const UShort_t *&mTdcD = iter("TofData.mTdc");
  const Short_t  *&mTc = iter("TofData.mTc");
  const UShort_t *&mSc = iter("TofData.mSc");
  const UInt_t   *&mLeadingTdc = iter("TofData.mLeadingTdc");
  const UInt_t   *&mTrailingTdc = iter("TofData.mTrailingTdc");
  const Int_t     &NoTofRaw = iter("TofRawData");
  const UShort_t *&mLeTeFlag = iter("TofRawData.mLeTeFlag");
  const UShort_t *&mChannel = iter("TofRawData.mChannel");
  const UInt_t   *&mTdc = iter("TofRawData.mTdc");
  const UShort_t *&mQuality = iter("TofRawData.mQuality");
  
  //         Now iterations
  while (iter.Next()) {
#ifdef DEBUG
    cout << "NoTracks\t" << (int) NoTracksGl << "\t" <<  (int) NoTracksPr << endl;
#endif
    PrVsGl->Fill(NoTracksGl,NoTracksPr);
#ifdef DEBUG
    for (Int_t k = 0; k < NoTofHit; k++) {
      cout << "Hit " << k;
      PrP(mIconf);
      PrP(mTrayIndex);
      PrP(mModuleIndex);
      PrP(mCellIndex);
      PrP(mDaqIndex);
      PrP(mADC);
      PrP(mTimeOfFlight);
      PrP(mPathLength);
      PrP(mBeta);
      PrP(mAssociatedTrackId);
      PrP(mProjectedPoint_mX1);
      PrP(mProjectedPoint_mX2);
      PrP(mProjectedPoint_mX3);
      PrP(mTOFExpectedAsElectron);
      PrP(mTOFExpectedAsPion);
      PrP(mTOFExpectedAsKaon);
      PrP(mTOFExpectedAsProton);
      PrP(mSigmaElectron);
      PrP(mSigmaPion);
      PrP(mSigmaKaon);
      PrP(mSigmaProton);
      PrP(mParticleHypothesis);
      cout << endl;
    }
#if 0
    for (Int_t k = 0; k < NoTofData; k++) {
      cout << "Data " << k;
      PrP(mDataIndex);
      PrP(mAdc);
      PrP(mTdcD);
      PrP(mTc);
      PrP(mSc);
      PrP(mLeadingTdc);
      PrP(mTrailingTdc);
      cout << endl;
    }
    for (Int_t k = 0; k < NoTofRaw; k++) {
      cout << "Raw " << k;
      PrP(mLeTeFlag);
      PrP(mChannel);
      PrP(mTdc);
      PrP(mQuality);
    }
#endif
  }
#endif
  fOut->Write();
}
