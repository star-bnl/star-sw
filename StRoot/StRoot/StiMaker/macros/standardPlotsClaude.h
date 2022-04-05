/*
 * $Id: standardPlotsClaude.h,v 1.2 2003/09/02 18:00:00 perev Exp $
 * A. Rose, WSU
 *
 *
 * $Log: standardPlotsClaude.h,v $
 * Revision 1.2  2003/09/02 18:00:00  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  2002/07/11 18:21:31  pruneau
 * new plots added
 *
 * Revision 1.4  2002/07/08 14:32:49  pruneau
 * added efficiency plots
 *
 * Revision 1.3  2002/07/02 18:59:30  andrewar
 * fixed bug with constructor
 *
 * Revision 1.2  2002/06/26 14:34:13  andrewar
 * Added cut handles.
 *
 * Revision 1.1  2002/06/12 20:22:02  andrewar
 * Initial commit. Template for standard evaluation plot generation.
 * Current version includes few real plots and no cut capability.
 *
 */

#ifndef standardPlots_h
#define standardPlots_h

#include <Stiostream.h>
#include <TROOT.h>
#include <TChain.h>
#include <TFile.h>
#include <TProfile.h>
   const Int_t kMaxmMcTracks = 6000;
   const Int_t kMaxmMatchedPairs = 6000;
   const Int_t kMaxmMergedPairs = 500;
   const Int_t kMaxmSplitPairs = 1;
   const Int_t kMaxmGhostPairs = 1;
   const Int_t kMaxmContamPairs = 1;

class standardPlots {
   public :
   TTree          *fChain;   //!pointer to the analyzed TTree or TChain
   Int_t           fCurrent; //!current Tree number in a TChain
//Declaration of leaves types
   Int_t           mEventId;
   Int_t           mRunId;
   Int_t           mOriginMult;
   Int_t           mCentralMult;
   Int_t           mCentrality;
   Int_t           mNUncorrectedNegativePrimaries;
   Int_t           mNUncorrectedPrimaries;
   Int_t           mMcMult;
   Float_t         mVertexX;
   Float_t         mVertexY;
   Float_t         mVertexZ;
   Float_t         mMcVertexX;
   Float_t         mMcVertexY;
   Float_t         mMcVertexZ;
   Float_t         mMagField;
   Float_t         mCTB;
   Float_t         mZDCe;
   Float_t         mZDCw;
   Int_t           mNMcTrack;
   Int_t           mNMatchedPair;
   Int_t           mNMergedPair;
   Int_t           mNSplitPair;
   Int_t           mNGhostPair;
   Int_t           mNContamPair;
   Int_t           mMcTracks_;
   Float_t         mMcTracks_mPtMc[kMaxmMcTracks];   //[mMcTracks_]
   Float_t         mMcTracks_mPzMc[kMaxmMcTracks];   //[mMcTracks_]
   Float_t         mMcTracks_mEtaMc[kMaxmMcTracks];   //[mMcTracks_]
   Float_t         mMcTracks_mPhiMc[kMaxmMcTracks];   //[mMcTracks_]
   Short_t         mMcTracks_mNHitMc[kMaxmMcTracks];   //[mMcTracks_]
   Short_t         mMcTracks_mGeantId[kMaxmMcTracks];   //[mMcTracks_]
   Short_t         mMcTracks_mChargeMc[kMaxmMcTracks];   //[mMcTracks_]
   Float_t         mMcTracks_mStopR[kMaxmMcTracks];   //[mMcTracks_]
   Short_t         mMcTracks_mNAssocGl[kMaxmMcTracks];   //[mMcTracks_]
   Short_t         mMcTracks_mNAssocPr[kMaxmMcTracks];   //[mMcTracks_]
   UInt_t          mMcTracks_fUniqueID[kMaxmMcTracks];   //[mMcTracks_]
   UInt_t          mMcTracks_fBits[kMaxmMcTracks];   //[mMcTracks_]
   Int_t           mMatchedPairs_;
   Short_t         mMatchedPairs_mNCommonHit[kMaxmMatchedPairs];   //[mMatchedPairs_]
   UChar_t         mMatchedPairs_mIsBestContam[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Float_t         mMatchedPairs_mPtMc[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Float_t         mMatchedPairs_mPzMc[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Float_t         mMatchedPairs_mEtaMc[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Float_t         mMatchedPairs_mPhiMc[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Short_t         mMatchedPairs_mNHitMc[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Short_t         mMatchedPairs_mGeantId[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Short_t         mMatchedPairs_mChargeMc[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Float_t         mMatchedPairs_mStopR[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Short_t         mMatchedPairs_mNAssocGl[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Short_t         mMatchedPairs_mNAssocPr[kMaxmMatchedPairs];   //[mMatchedPairs_]
   UInt_t          mMatchedPairs_fUniqueID[kMaxmMatchedPairs];   //[mMatchedPairs_]
   UInt_t          mMatchedPairs_fBits[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Float_t         mMatchedPairs_mPtPr[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Float_t         mMatchedPairs_mPzPr[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Float_t         mMatchedPairs_mEtaPr[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Float_t         mMatchedPairs_mPhiPr[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Float_t         mMatchedPairs_mDcaPr[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Float_t         mMatchedPairs_mDcaXYPr[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Float_t         mMatchedPairs_mDcaZPr[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Float_t         mMatchedPairs_mChi2Pr[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Short_t         mMatchedPairs_mFlag[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Float_t         mMatchedPairs_mDedx[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Float_t         mMatchedPairs_mPtGl[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Float_t         mMatchedPairs_mPzGl[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Float_t         mMatchedPairs_mEtaGl[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Float_t         mMatchedPairs_mPhiGl[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Float_t         mMatchedPairs_mDcaGl[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Float_t         mMatchedPairs_mDcaXYGl[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Float_t         mMatchedPairs_mDcaZGl[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Float_t         mMatchedPairs_mPidPion[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Float_t         mMatchedPairs_mPidProton[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Float_t         mMatchedPairs_mPidKaon[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Float_t         mMatchedPairs_mPidElectron[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Float_t         mMatchedPairs_mFirstZ[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Float_t         mMatchedPairs_mLastZ[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Short_t         mMatchedPairs_mFirstPadrow[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Short_t         mMatchedPairs_mLastPadrow[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Short_t         mMatchedPairs_mFirstFitPadrow[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Short_t         mMatchedPairs_mLastFitPadrow[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Short_t         mMatchedPairs_mFirstSector[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Short_t         mMatchedPairs_mLastSector[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Short_t         mMatchedPairs_mFitPts[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Short_t         mMatchedPairs_mAllPts[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Short_t         mMatchedPairs_mCharge[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Short_t         mMatchedPairs_mNAssocMc[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Short_t         mMatchedPairs_mNPossible[kMaxmMatchedPairs];   //[mMatchedPairs_]
   Int_t           mMergedPairs_;
   Short_t         mMergedPairs_mNCommonHit[kMaxmMergedPairs];   //[mMergedPairs_]
   UChar_t         mMergedPairs_mIsBestContam[kMaxmMergedPairs];   //[mMergedPairs_]
   Float_t         mMergedPairs_mPtMc[kMaxmMergedPairs];   //[mMergedPairs_]
   Float_t         mMergedPairs_mPzMc[kMaxmMergedPairs];   //[mMergedPairs_]
   Float_t         mMergedPairs_mEtaMc[kMaxmMergedPairs];   //[mMergedPairs_]
   Float_t         mMergedPairs_mPhiMc[kMaxmMergedPairs];   //[mMergedPairs_]
   Short_t         mMergedPairs_mNHitMc[kMaxmMergedPairs];   //[mMergedPairs_]
   Short_t         mMergedPairs_mGeantId[kMaxmMergedPairs];   //[mMergedPairs_]
   Short_t         mMergedPairs_mChargeMc[kMaxmMergedPairs];   //[mMergedPairs_]
   Float_t         mMergedPairs_mStopR[kMaxmMergedPairs];   //[mMergedPairs_]
   Short_t         mMergedPairs_mNAssocGl[kMaxmMergedPairs];   //[mMergedPairs_]
   Short_t         mMergedPairs_mNAssocPr[kMaxmMergedPairs];   //[mMergedPairs_]
   UInt_t          mMergedPairs_fUniqueID[kMaxmMergedPairs];   //[mMergedPairs_]
   UInt_t          mMergedPairs_fBits[kMaxmMergedPairs];   //[mMergedPairs_]
   Float_t         mMergedPairs_mPtPr[kMaxmMergedPairs];   //[mMergedPairs_]
   Float_t         mMergedPairs_mPzPr[kMaxmMergedPairs];   //[mMergedPairs_]
   Float_t         mMergedPairs_mEtaPr[kMaxmMergedPairs];   //[mMergedPairs_]
   Float_t         mMergedPairs_mPhiPr[kMaxmMergedPairs];   //[mMergedPairs_]
   Float_t         mMergedPairs_mDcaPr[kMaxmMergedPairs];   //[mMergedPairs_]
   Float_t         mMergedPairs_mDcaXYPr[kMaxmMergedPairs];   //[mMergedPairs_]
   Float_t         mMergedPairs_mDcaZPr[kMaxmMergedPairs];   //[mMergedPairs_]
   Float_t         mMergedPairs_mChi2Pr[kMaxmMergedPairs];   //[mMergedPairs_]
   Short_t         mMergedPairs_mFlag[kMaxmMergedPairs];   //[mMergedPairs_]
   Float_t         mMergedPairs_mDedx[kMaxmMergedPairs];   //[mMergedPairs_]
   Float_t         mMergedPairs_mPtGl[kMaxmMergedPairs];   //[mMergedPairs_]
   Float_t         mMergedPairs_mPzGl[kMaxmMergedPairs];   //[mMergedPairs_]
   Float_t         mMergedPairs_mEtaGl[kMaxmMergedPairs];   //[mMergedPairs_]
   Float_t         mMergedPairs_mPhiGl[kMaxmMergedPairs];   //[mMergedPairs_]
   Float_t         mMergedPairs_mDcaGl[kMaxmMergedPairs];   //[mMergedPairs_]
   Float_t         mMergedPairs_mDcaXYGl[kMaxmMergedPairs];   //[mMergedPairs_]
   Float_t         mMergedPairs_mDcaZGl[kMaxmMergedPairs];   //[mMergedPairs_]
   Float_t         mMergedPairs_mPidPion[kMaxmMergedPairs];   //[mMergedPairs_]
   Float_t         mMergedPairs_mPidProton[kMaxmMergedPairs];   //[mMergedPairs_]
   Float_t         mMergedPairs_mPidKaon[kMaxmMergedPairs];   //[mMergedPairs_]
   Float_t         mMergedPairs_mPidElectron[kMaxmMergedPairs];   //[mMergedPairs_]
   Float_t         mMergedPairs_mFirstZ[kMaxmMergedPairs];   //[mMergedPairs_]
   Float_t         mMergedPairs_mLastZ[kMaxmMergedPairs];   //[mMergedPairs_]
   Short_t         mMergedPairs_mFirstPadrow[kMaxmMergedPairs];   //[mMergedPairs_]
   Short_t         mMergedPairs_mLastPadrow[kMaxmMergedPairs];   //[mMergedPairs_]
   Short_t         mMergedPairs_mFirstFitPadrow[kMaxmMergedPairs];   //[mMergedPairs_]
   Short_t         mMergedPairs_mLastFitPadrow[kMaxmMergedPairs];   //[mMergedPairs_]
   Short_t         mMergedPairs_mFirstSector[kMaxmMergedPairs];   //[mMergedPairs_]
   Short_t         mMergedPairs_mLastSector[kMaxmMergedPairs];   //[mMergedPairs_]
   Short_t         mMergedPairs_mFitPts[kMaxmMergedPairs];   //[mMergedPairs_]
   Short_t         mMergedPairs_mAllPts[kMaxmMergedPairs];   //[mMergedPairs_]
   Short_t         mMergedPairs_mCharge[kMaxmMergedPairs];   //[mMergedPairs_]
   Short_t         mMergedPairs_mNAssocMc[kMaxmMergedPairs];   //[mMergedPairs_]
   Short_t         mMergedPairs_mNPossible[kMaxmMergedPairs];   //[mMergedPairs_]
   Int_t           mSplitPairs_;
   Short_t         mSplitPairs_mNCommonHit[kMaxmSplitPairs];   //[mSplitPairs_]
   UChar_t         mSplitPairs_mIsBestContam[kMaxmSplitPairs];   //[mSplitPairs_]
   Float_t         mSplitPairs_mPtMc[kMaxmSplitPairs];   //[mSplitPairs_]
   Float_t         mSplitPairs_mPzMc[kMaxmSplitPairs];   //[mSplitPairs_]
   Float_t         mSplitPairs_mEtaMc[kMaxmSplitPairs];   //[mSplitPairs_]
   Float_t         mSplitPairs_mPhiMc[kMaxmSplitPairs];   //[mSplitPairs_]
   Short_t         mSplitPairs_mNHitMc[kMaxmSplitPairs];   //[mSplitPairs_]
   Short_t         mSplitPairs_mGeantId[kMaxmSplitPairs];   //[mSplitPairs_]
   Short_t         mSplitPairs_mChargeMc[kMaxmSplitPairs];   //[mSplitPairs_]
   Float_t         mSplitPairs_mStopR[kMaxmSplitPairs];   //[mSplitPairs_]
   Short_t         mSplitPairs_mNAssocGl[kMaxmSplitPairs];   //[mSplitPairs_]
   Short_t         mSplitPairs_mNAssocPr[kMaxmSplitPairs];   //[mSplitPairs_]
   UInt_t          mSplitPairs_fUniqueID[kMaxmSplitPairs];   //[mSplitPairs_]
   UInt_t          mSplitPairs_fBits[kMaxmSplitPairs];   //[mSplitPairs_]
   Float_t         mSplitPairs_mPtPr[kMaxmSplitPairs];   //[mSplitPairs_]
   Float_t         mSplitPairs_mPzPr[kMaxmSplitPairs];   //[mSplitPairs_]
   Float_t         mSplitPairs_mEtaPr[kMaxmSplitPairs];   //[mSplitPairs_]
   Float_t         mSplitPairs_mPhiPr[kMaxmSplitPairs];   //[mSplitPairs_]
   Float_t         mSplitPairs_mDcaPr[kMaxmSplitPairs];   //[mSplitPairs_]
   Float_t         mSplitPairs_mDcaXYPr[kMaxmSplitPairs];   //[mSplitPairs_]
   Float_t         mSplitPairs_mDcaZPr[kMaxmSplitPairs];   //[mSplitPairs_]
   Float_t         mSplitPairs_mChi2Pr[kMaxmSplitPairs];   //[mSplitPairs_]
   Short_t         mSplitPairs_mFlag[kMaxmSplitPairs];   //[mSplitPairs_]
   Float_t         mSplitPairs_mDedx[kMaxmSplitPairs];   //[mSplitPairs_]
   Float_t         mSplitPairs_mPtGl[kMaxmSplitPairs];   //[mSplitPairs_]
   Float_t         mSplitPairs_mPzGl[kMaxmSplitPairs];   //[mSplitPairs_]
   Float_t         mSplitPairs_mEtaGl[kMaxmSplitPairs];   //[mSplitPairs_]
   Float_t         mSplitPairs_mPhiGl[kMaxmSplitPairs];   //[mSplitPairs_]
   Float_t         mSplitPairs_mDcaGl[kMaxmSplitPairs];   //[mSplitPairs_]
   Float_t         mSplitPairs_mDcaXYGl[kMaxmSplitPairs];   //[mSplitPairs_]
   Float_t         mSplitPairs_mDcaZGl[kMaxmSplitPairs];   //[mSplitPairs_]
   Float_t         mSplitPairs_mPidPion[kMaxmSplitPairs];   //[mSplitPairs_]
   Float_t         mSplitPairs_mPidProton[kMaxmSplitPairs];   //[mSplitPairs_]
   Float_t         mSplitPairs_mPidKaon[kMaxmSplitPairs];   //[mSplitPairs_]
   Float_t         mSplitPairs_mPidElectron[kMaxmSplitPairs];   //[mSplitPairs_]
   Float_t         mSplitPairs_mFirstZ[kMaxmSplitPairs];   //[mSplitPairs_]
   Float_t         mSplitPairs_mLastZ[kMaxmSplitPairs];   //[mSplitPairs_]
   Short_t         mSplitPairs_mFirstPadrow[kMaxmSplitPairs];   //[mSplitPairs_]
   Short_t         mSplitPairs_mLastPadrow[kMaxmSplitPairs];   //[mSplitPairs_]
   Short_t         mSplitPairs_mFirstFitPadrow[kMaxmSplitPairs];   //[mSplitPairs_]
   Short_t         mSplitPairs_mLastFitPadrow[kMaxmSplitPairs];   //[mSplitPairs_]
   Short_t         mSplitPairs_mFirstSector[kMaxmSplitPairs];   //[mSplitPairs_]
   Short_t         mSplitPairs_mLastSector[kMaxmSplitPairs];   //[mSplitPairs_]
   Short_t         mSplitPairs_mFitPts[kMaxmSplitPairs];   //[mSplitPairs_]
   Short_t         mSplitPairs_mAllPts[kMaxmSplitPairs];   //[mSplitPairs_]
   Short_t         mSplitPairs_mCharge[kMaxmSplitPairs];   //[mSplitPairs_]
   Short_t         mSplitPairs_mNAssocMc[kMaxmSplitPairs];   //[mSplitPairs_]
   Short_t         mSplitPairs_mNPossible[kMaxmSplitPairs];   //[mSplitPairs_]
   Int_t           mGhostPairs_;
   Short_t         mGhostPairs_mNCommonHit[kMaxmGhostPairs];   //[mGhostPairs_]
   UChar_t         mGhostPairs_mIsBestContam[kMaxmGhostPairs];   //[mGhostPairs_]
   Float_t         mGhostPairs_mPtMc[kMaxmGhostPairs];   //[mGhostPairs_]
   Float_t         mGhostPairs_mPzMc[kMaxmGhostPairs];   //[mGhostPairs_]
   Float_t         mGhostPairs_mEtaMc[kMaxmGhostPairs];   //[mGhostPairs_]
   Float_t         mGhostPairs_mPhiMc[kMaxmGhostPairs];   //[mGhostPairs_]
   Short_t         mGhostPairs_mNHitMc[kMaxmGhostPairs];   //[mGhostPairs_]
   Short_t         mGhostPairs_mGeantId[kMaxmGhostPairs];   //[mGhostPairs_]
   Short_t         mGhostPairs_mChargeMc[kMaxmGhostPairs];   //[mGhostPairs_]
   Float_t         mGhostPairs_mStopR[kMaxmGhostPairs];   //[mGhostPairs_]
   Short_t         mGhostPairs_mNAssocGl[kMaxmGhostPairs];   //[mGhostPairs_]
   Short_t         mGhostPairs_mNAssocPr[kMaxmGhostPairs];   //[mGhostPairs_]
   UInt_t          mGhostPairs_fUniqueID[kMaxmGhostPairs];   //[mGhostPairs_]
   UInt_t          mGhostPairs_fBits[kMaxmGhostPairs];   //[mGhostPairs_]
   Float_t         mGhostPairs_mPtPr[kMaxmGhostPairs];   //[mGhostPairs_]
   Float_t         mGhostPairs_mPzPr[kMaxmGhostPairs];   //[mGhostPairs_]
   Float_t         mGhostPairs_mEtaPr[kMaxmGhostPairs];   //[mGhostPairs_]
   Float_t         mGhostPairs_mPhiPr[kMaxmGhostPairs];   //[mGhostPairs_]
   Float_t         mGhostPairs_mDcaPr[kMaxmGhostPairs];   //[mGhostPairs_]
   Float_t         mGhostPairs_mDcaXYPr[kMaxmGhostPairs];   //[mGhostPairs_]
   Float_t         mGhostPairs_mDcaZPr[kMaxmGhostPairs];   //[mGhostPairs_]
   Float_t         mGhostPairs_mChi2Pr[kMaxmGhostPairs];   //[mGhostPairs_]
   Short_t         mGhostPairs_mFlag[kMaxmGhostPairs];   //[mGhostPairs_]
   Float_t         mGhostPairs_mDedx[kMaxmGhostPairs];   //[mGhostPairs_]
   Float_t         mGhostPairs_mPtGl[kMaxmGhostPairs];   //[mGhostPairs_]
   Float_t         mGhostPairs_mPzGl[kMaxmGhostPairs];   //[mGhostPairs_]
   Float_t         mGhostPairs_mEtaGl[kMaxmGhostPairs];   //[mGhostPairs_]
   Float_t         mGhostPairs_mPhiGl[kMaxmGhostPairs];   //[mGhostPairs_]
   Float_t         mGhostPairs_mDcaGl[kMaxmGhostPairs];   //[mGhostPairs_]
   Float_t         mGhostPairs_mDcaXYGl[kMaxmGhostPairs];   //[mGhostPairs_]
   Float_t         mGhostPairs_mDcaZGl[kMaxmGhostPairs];   //[mGhostPairs_]
   Float_t         mGhostPairs_mPidPion[kMaxmGhostPairs];   //[mGhostPairs_]
   Float_t         mGhostPairs_mPidProton[kMaxmGhostPairs];   //[mGhostPairs_]
   Float_t         mGhostPairs_mPidKaon[kMaxmGhostPairs];   //[mGhostPairs_]
   Float_t         mGhostPairs_mPidElectron[kMaxmGhostPairs];   //[mGhostPairs_]
   Float_t         mGhostPairs_mFirstZ[kMaxmGhostPairs];   //[mGhostPairs_]
   Float_t         mGhostPairs_mLastZ[kMaxmGhostPairs];   //[mGhostPairs_]
   Short_t         mGhostPairs_mFirstPadrow[kMaxmGhostPairs];   //[mGhostPairs_]
   Short_t         mGhostPairs_mLastPadrow[kMaxmGhostPairs];   //[mGhostPairs_]
   Short_t         mGhostPairs_mFirstFitPadrow[kMaxmGhostPairs];   //[mGhostPairs_]
   Short_t         mGhostPairs_mLastFitPadrow[kMaxmGhostPairs];   //[mGhostPairs_]
   Short_t         mGhostPairs_mFirstSector[kMaxmGhostPairs];   //[mGhostPairs_]
   Short_t         mGhostPairs_mLastSector[kMaxmGhostPairs];   //[mGhostPairs_]
   Short_t         mGhostPairs_mFitPts[kMaxmGhostPairs];   //[mGhostPairs_]
   Short_t         mGhostPairs_mAllPts[kMaxmGhostPairs];   //[mGhostPairs_]
   Short_t         mGhostPairs_mCharge[kMaxmGhostPairs];   //[mGhostPairs_]
   Short_t         mGhostPairs_mNAssocMc[kMaxmGhostPairs];   //[mGhostPairs_]
   Short_t         mGhostPairs_mNPossible[kMaxmGhostPairs];   //[mGhostPairs_]
   Int_t           mContamPairs_;
   Int_t           mContamPairs_mParentGeantId[kMaxmContamPairs];   //[mContamPairs_]
   Int_t           mContamPairs_mGeantProcess[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mPtMcParent[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mEtaMcParent[kMaxmContamPairs];   //[mContamPairs_]
   Int_t           mContamPairs_mParentParentGeantId[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mPtMcParentParent[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mStartX[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mStartY[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mStartZ[kMaxmContamPairs];   //[mContamPairs_]
   Short_t         mContamPairs_mNCommonHit[kMaxmContamPairs];   //[mContamPairs_]
   UChar_t         mContamPairs_mIsBestContam[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mPtMc[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mPzMc[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mEtaMc[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mPhiMc[kMaxmContamPairs];   //[mContamPairs_]
   Short_t         mContamPairs_mNHitMc[kMaxmContamPairs];   //[mContamPairs_]
   Short_t         mContamPairs_mGeantId[kMaxmContamPairs];   //[mContamPairs_]
   Short_t         mContamPairs_mChargeMc[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mStopR[kMaxmContamPairs];   //[mContamPairs_]
   Short_t         mContamPairs_mNAssocGl[kMaxmContamPairs];   //[mContamPairs_]
   Short_t         mContamPairs_mNAssocPr[kMaxmContamPairs];   //[mContamPairs_]
   UInt_t          mContamPairs_fUniqueID[kMaxmContamPairs];   //[mContamPairs_]
   UInt_t          mContamPairs_fBits[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mPtPr[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mPzPr[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mEtaPr[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mPhiPr[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mDcaPr[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mDcaXYPr[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mDcaZPr[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mChi2Pr[kMaxmContamPairs];   //[mContamPairs_]
   Short_t         mContamPairs_mFlag[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mDedx[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mPtGl[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mPzGl[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mEtaGl[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mPhiGl[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mDcaGl[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mDcaXYGl[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mDcaZGl[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mPidPion[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mPidProton[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mPidKaon[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mPidElectron[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mFirstZ[kMaxmContamPairs];   //[mContamPairs_]
   Float_t         mContamPairs_mLastZ[kMaxmContamPairs];   //[mContamPairs_]
   Short_t         mContamPairs_mFirstPadrow[kMaxmContamPairs];   //[mContamPairs_]
   Short_t         mContamPairs_mLastPadrow[kMaxmContamPairs];   //[mContamPairs_]
   Short_t         mContamPairs_mFirstFitPadrow[kMaxmContamPairs];   //[mContamPairs_]
   Short_t         mContamPairs_mLastFitPadrow[kMaxmContamPairs];   //[mContamPairs_]
   Short_t         mContamPairs_mFirstSector[kMaxmContamPairs];   //[mContamPairs_]
   Short_t         mContamPairs_mLastSector[kMaxmContamPairs];   //[mContamPairs_]
   Short_t         mContamPairs_mFitPts[kMaxmContamPairs];   //[mContamPairs_]
   Short_t         mContamPairs_mAllPts[kMaxmContamPairs];   //[mContamPairs_]
   Short_t         mContamPairs_mCharge[kMaxmContamPairs];   //[mContamPairs_]
   Short_t         mContamPairs_mNAssocMc[kMaxmContamPairs];   //[mContamPairs_]
   Short_t         mContamPairs_mNPossible[kMaxmContamPairs];   //[mContamPairs_]
   UInt_t          fUniqueID;
   UInt_t          fBits;

//List of branches
   TBranch        *b_mEventId;   //!
   TBranch        *b_mRunId;   //!
   TBranch        *b_mOriginMult;   //!
   TBranch        *b_mCentralMult;   //!
   TBranch        *b_mCentrality;   //!
   TBranch        *b_mNUncorrectedNegativePrimaries;   //!
   TBranch        *b_mNUncorrectedPrimaries;   //!
   TBranch        *b_mMcMult;   //!
   TBranch        *b_mVertexX;   //!
   TBranch        *b_mVertexY;   //!
   TBranch        *b_mVertexZ;   //!
   TBranch        *b_mMcVertexX;   //!
   TBranch        *b_mMcVertexY;   //!
   TBranch        *b_mMcVertexZ;   //!
   TBranch        *b_mMagField;   //!
   TBranch        *b_mCTB;   //!
   TBranch        *b_mZDCe;   //!
   TBranch        *b_mZDCw;   //!
   TBranch        *b_mNMcTrack;   //!
   TBranch        *b_mNMatchedPair;   //!
   TBranch        *b_mNMergedPair;   //!
   TBranch        *b_mNSplitPair;   //!
   TBranch        *b_mNGhostPair;   //!
   TBranch        *b_mNContamPair;   //!
   TBranch        *b_mMcTracks_;   //!
   TBranch        *b_mMcTracks_mPtMc;   //!
   TBranch        *b_mMcTracks_mPzMc;   //!
   TBranch        *b_mMcTracks_mEtaMc;   //!
   TBranch        *b_mMcTracks_mPhiMc;   //!
   TBranch        *b_mMcTracks_mNHitMc;   //!
   TBranch        *b_mMcTracks_mGeantId;   //!
   TBranch        *b_mMcTracks_mChargeMc;   //!
   TBranch        *b_mMcTracks_mStopR;   //!
   TBranch        *b_mMcTracks_mNAssocGl;   //!
   TBranch        *b_mMcTracks_mNAssocPr;   //!
   TBranch        *b_mMcTracks_fUniqueID;   //!
   TBranch        *b_mMcTracks_fBits;   //!
   TBranch        *b_mMatchedPairs_;   //!
   TBranch        *b_mMatchedPairs_mNCommonHit;   //!
   TBranch        *b_mMatchedPairs_mIsBestContam;   //!
   TBranch        *b_mMatchedPairs_mPtMc;   //!
   TBranch        *b_mMatchedPairs_mPzMc;   //!
   TBranch        *b_mMatchedPairs_mEtaMc;   //!
   TBranch        *b_mMatchedPairs_mPhiMc;   //!
   TBranch        *b_mMatchedPairs_mNHitMc;   //!
   TBranch        *b_mMatchedPairs_mGeantId;   //!
   TBranch        *b_mMatchedPairs_mChargeMc;   //!
   TBranch        *b_mMatchedPairs_mStopR;   //!
   TBranch        *b_mMatchedPairs_mNAssocGl;   //!
   TBranch        *b_mMatchedPairs_mNAssocPr;   //!
   TBranch        *b_mMatchedPairs_fUniqueID;   //!
   TBranch        *b_mMatchedPairs_fBits;   //!
   TBranch        *b_mMatchedPairs_mPtPr;   //!
   TBranch        *b_mMatchedPairs_mPzPr;   //!
   TBranch        *b_mMatchedPairs_mEtaPr;   //!
   TBranch        *b_mMatchedPairs_mPhiPr;   //!
   TBranch        *b_mMatchedPairs_mDcaPr;   //!
   TBranch        *b_mMatchedPairs_mDcaXYPr;   //!
   TBranch        *b_mMatchedPairs_mDcaZPr;   //!
   TBranch        *b_mMatchedPairs_mChi2Pr;   //!
   TBranch        *b_mMatchedPairs_mFlag;   //!
   TBranch        *b_mMatchedPairs_mDedx;   //!
   TBranch        *b_mMatchedPairs_mPtGl;   //!
   TBranch        *b_mMatchedPairs_mPzGl;   //!
   TBranch        *b_mMatchedPairs_mEtaGl;   //!
   TBranch        *b_mMatchedPairs_mPhiGl;   //!
   TBranch        *b_mMatchedPairs_mDcaGl;   //!
   TBranch        *b_mMatchedPairs_mDcaXYGl;   //!
   TBranch        *b_mMatchedPairs_mDcaZGl;   //!
   TBranch        *b_mMatchedPairs_mPidPion;   //!
   TBranch        *b_mMatchedPairs_mPidProton;   //!
   TBranch        *b_mMatchedPairs_mPidKaon;   //!
   TBranch        *b_mMatchedPairs_mPidElectron;   //!
   TBranch        *b_mMatchedPairs_mFirstZ;   //!
   TBranch        *b_mMatchedPairs_mLastZ;   //!
   TBranch        *b_mMatchedPairs_mFirstPadrow;   //!
   TBranch        *b_mMatchedPairs_mLastPadrow;   //!
   TBranch        *b_mMatchedPairs_mFirstFitPadrow;   //!
   TBranch        *b_mMatchedPairs_mLastFitPadrow;   //!
   TBranch        *b_mMatchedPairs_mFirstSector;   //!
   TBranch        *b_mMatchedPairs_mLastSector;   //!
   TBranch        *b_mMatchedPairs_mFitPts;   //!
   TBranch        *b_mMatchedPairs_mAllPts;   //!
   TBranch        *b_mMatchedPairs_mCharge;   //!
   TBranch        *b_mMatchedPairs_mNAssocMc;   //!
   TBranch        *b_mMatchedPairs_mNPossible;   //!
   TBranch        *b_mMergedPairs_;   //!
   TBranch        *b_mMergedPairs_mNCommonHit;   //!
   TBranch        *b_mMergedPairs_mIsBestContam;   //!
   TBranch        *b_mMergedPairs_mPtMc;   //!
   TBranch        *b_mMergedPairs_mPzMc;   //!
   TBranch        *b_mMergedPairs_mEtaMc;   //!
   TBranch        *b_mMergedPairs_mPhiMc;   //!
   TBranch        *b_mMergedPairs_mNHitMc;   //!
   TBranch        *b_mMergedPairs_mGeantId;   //!
   TBranch        *b_mMergedPairs_mChargeMc;   //!
   TBranch        *b_mMergedPairs_mStopR;   //!
   TBranch        *b_mMergedPairs_mNAssocGl;   //!
   TBranch        *b_mMergedPairs_mNAssocPr;   //!
   TBranch        *b_mMergedPairs_fUniqueID;   //!
   TBranch        *b_mMergedPairs_fBits;   //!
   TBranch        *b_mMergedPairs_mPtPr;   //!
   TBranch        *b_mMergedPairs_mPzPr;   //!
   TBranch        *b_mMergedPairs_mEtaPr;   //!
   TBranch        *b_mMergedPairs_mPhiPr;   //!
   TBranch        *b_mMergedPairs_mDcaPr;   //!
   TBranch        *b_mMergedPairs_mDcaXYPr;   //!
   TBranch        *b_mMergedPairs_mDcaZPr;   //!
   TBranch        *b_mMergedPairs_mChi2Pr;   //!
   TBranch        *b_mMergedPairs_mFlag;   //!
   TBranch        *b_mMergedPairs_mDedx;   //!
   TBranch        *b_mMergedPairs_mPtGl;   //!
   TBranch        *b_mMergedPairs_mPzGl;   //!
   TBranch        *b_mMergedPairs_mEtaGl;   //!
   TBranch        *b_mMergedPairs_mPhiGl;   //!
   TBranch        *b_mMergedPairs_mDcaGl;   //!
   TBranch        *b_mMergedPairs_mDcaXYGl;   //!
   TBranch        *b_mMergedPairs_mDcaZGl;   //!
   TBranch        *b_mMergedPairs_mPidPion;   //!
   TBranch        *b_mMergedPairs_mPidProton;   //!
   TBranch        *b_mMergedPairs_mPidKaon;   //!
   TBranch        *b_mMergedPairs_mPidElectron;   //!
   TBranch        *b_mMergedPairs_mFirstZ;   //!
   TBranch        *b_mMergedPairs_mLastZ;   //!
   TBranch        *b_mMergedPairs_mFirstPadrow;   //!
   TBranch        *b_mMergedPairs_mLastPadrow;   //!
   TBranch        *b_mMergedPairs_mFirstFitPadrow;   //!
   TBranch        *b_mMergedPairs_mLastFitPadrow;   //!
   TBranch        *b_mMergedPairs_mFirstSector;   //!
   TBranch        *b_mMergedPairs_mLastSector;   //!
   TBranch        *b_mMergedPairs_mFitPts;   //!
   TBranch        *b_mMergedPairs_mAllPts;   //!
   TBranch        *b_mMergedPairs_mCharge;   //!
   TBranch        *b_mMergedPairs_mNAssocMc;   //!
   TBranch        *b_mMergedPairs_mNPossible;   //!
   TBranch        *b_mSplitPairs_;   //!
   TBranch        *b_mSplitPairs_mNCommonHit;   //!
   TBranch        *b_mSplitPairs_mIsBestContam;   //!
   TBranch        *b_mSplitPairs_mPtMc;   //!
   TBranch        *b_mSplitPairs_mPzMc;   //!
   TBranch        *b_mSplitPairs_mEtaMc;   //!
   TBranch        *b_mSplitPairs_mPhiMc;   //!
   TBranch        *b_mSplitPairs_mNHitMc;   //!
   TBranch        *b_mSplitPairs_mGeantId;   //!
   TBranch        *b_mSplitPairs_mChargeMc;   //!
   TBranch        *b_mSplitPairs_mStopR;   //!
   TBranch        *b_mSplitPairs_mNAssocGl;   //!
   TBranch        *b_mSplitPairs_mNAssocPr;   //!
   TBranch        *b_mSplitPairs_fUniqueID;   //!
   TBranch        *b_mSplitPairs_fBits;   //!
   TBranch        *b_mSplitPairs_mPtPr;   //!
   TBranch        *b_mSplitPairs_mPzPr;   //!
   TBranch        *b_mSplitPairs_mEtaPr;   //!
   TBranch        *b_mSplitPairs_mPhiPr;   //!
   TBranch        *b_mSplitPairs_mDcaPr;   //!
   TBranch        *b_mSplitPairs_mDcaXYPr;   //!
   TBranch        *b_mSplitPairs_mDcaZPr;   //!
   TBranch        *b_mSplitPairs_mChi2Pr;   //!
   TBranch        *b_mSplitPairs_mFlag;   //!
   TBranch        *b_mSplitPairs_mDedx;   //!
   TBranch        *b_mSplitPairs_mPtGl;   //!
   TBranch        *b_mSplitPairs_mPzGl;   //!
   TBranch        *b_mSplitPairs_mEtaGl;   //!
   TBranch        *b_mSplitPairs_mPhiGl;   //!
   TBranch        *b_mSplitPairs_mDcaGl;   //!
   TBranch        *b_mSplitPairs_mDcaXYGl;   //!
   TBranch        *b_mSplitPairs_mDcaZGl;   //!
   TBranch        *b_mSplitPairs_mPidPion;   //!
   TBranch        *b_mSplitPairs_mPidProton;   //!
   TBranch        *b_mSplitPairs_mPidKaon;   //!
   TBranch        *b_mSplitPairs_mPidElectron;   //!
   TBranch        *b_mSplitPairs_mFirstZ;   //!
   TBranch        *b_mSplitPairs_mLastZ;   //!
   TBranch        *b_mSplitPairs_mFirstPadrow;   //!
   TBranch        *b_mSplitPairs_mLastPadrow;   //!
   TBranch        *b_mSplitPairs_mFirstFitPadrow;   //!
   TBranch        *b_mSplitPairs_mLastFitPadrow;   //!
   TBranch        *b_mSplitPairs_mFirstSector;   //!
   TBranch        *b_mSplitPairs_mLastSector;   //!
   TBranch        *b_mSplitPairs_mFitPts;   //!
   TBranch        *b_mSplitPairs_mAllPts;   //!
   TBranch        *b_mSplitPairs_mCharge;   //!
   TBranch        *b_mSplitPairs_mNAssocMc;   //!
   TBranch        *b_mSplitPairs_mNPossible;   //!
   TBranch        *b_mGhostPairs_;   //!
   TBranch        *b_mGhostPairs_mNCommonHit;   //!
   TBranch        *b_mGhostPairs_mIsBestContam;   //!
   TBranch        *b_mGhostPairs_mPtMc;   //!
   TBranch        *b_mGhostPairs_mPzMc;   //!
   TBranch        *b_mGhostPairs_mEtaMc;   //!
   TBranch        *b_mGhostPairs_mPhiMc;   //!
   TBranch        *b_mGhostPairs_mNHitMc;   //!
   TBranch        *b_mGhostPairs_mGeantId;   //!
   TBranch        *b_mGhostPairs_mChargeMc;   //!
   TBranch        *b_mGhostPairs_mStopR;   //!
   TBranch        *b_mGhostPairs_mNAssocGl;   //!
   TBranch        *b_mGhostPairs_mNAssocPr;   //!
   TBranch        *b_mGhostPairs_fUniqueID;   //!
   TBranch        *b_mGhostPairs_fBits;   //!
   TBranch        *b_mGhostPairs_mPtPr;   //!
   TBranch        *b_mGhostPairs_mPzPr;   //!
   TBranch        *b_mGhostPairs_mEtaPr;   //!
   TBranch        *b_mGhostPairs_mPhiPr;   //!
   TBranch        *b_mGhostPairs_mDcaPr;   //!
   TBranch        *b_mGhostPairs_mDcaXYPr;   //!
   TBranch        *b_mGhostPairs_mDcaZPr;   //!
   TBranch        *b_mGhostPairs_mChi2Pr;   //!
   TBranch        *b_mGhostPairs_mFlag;   //!
   TBranch        *b_mGhostPairs_mDedx;   //!
   TBranch        *b_mGhostPairs_mPtGl;   //!
   TBranch        *b_mGhostPairs_mPzGl;   //!
   TBranch        *b_mGhostPairs_mEtaGl;   //!
   TBranch        *b_mGhostPairs_mPhiGl;   //!
   TBranch        *b_mGhostPairs_mDcaGl;   //!
   TBranch        *b_mGhostPairs_mDcaXYGl;   //!
   TBranch        *b_mGhostPairs_mDcaZGl;   //!
   TBranch        *b_mGhostPairs_mPidPion;   //!
   TBranch        *b_mGhostPairs_mPidProton;   //!
   TBranch        *b_mGhostPairs_mPidKaon;   //!
   TBranch        *b_mGhostPairs_mPidElectron;   //!
   TBranch        *b_mGhostPairs_mFirstZ;   //!
   TBranch        *b_mGhostPairs_mLastZ;   //!
   TBranch        *b_mGhostPairs_mFirstPadrow;   //!
   TBranch        *b_mGhostPairs_mLastPadrow;   //!
   TBranch        *b_mGhostPairs_mFirstFitPadrow;   //!
   TBranch        *b_mGhostPairs_mLastFitPadrow;   //!
   TBranch        *b_mGhostPairs_mFirstSector;   //!
   TBranch        *b_mGhostPairs_mLastSector;   //!
   TBranch        *b_mGhostPairs_mFitPts;   //!
   TBranch        *b_mGhostPairs_mAllPts;   //!
   TBranch        *b_mGhostPairs_mCharge;   //!
   TBranch        *b_mGhostPairs_mNAssocMc;   //!
   TBranch        *b_mGhostPairs_mNPossible;   //!
   TBranch        *b_mContamPairs_;   //!
   TBranch        *b_mContamPairs_mParentGeantId;   //!
   TBranch        *b_mContamPairs_mGeantProcess;   //!
   TBranch        *b_mContamPairs_mPtMcParent;   //!
   TBranch        *b_mContamPairs_mEtaMcParent;   //!
   TBranch        *b_mContamPairs_mParentParentGeantId;   //!
   TBranch        *b_mContamPairs_mPtMcParentParent;   //!
   TBranch        *b_mContamPairs_mStartX;   //!
   TBranch        *b_mContamPairs_mStartY;   //!
   TBranch        *b_mContamPairs_mStartZ;   //!
   TBranch        *b_mContamPairs_mNCommonHit;   //!
   TBranch        *b_mContamPairs_mIsBestContam;   //!
   TBranch        *b_mContamPairs_mPtMc;   //!
   TBranch        *b_mContamPairs_mPzMc;   //!
   TBranch        *b_mContamPairs_mEtaMc;   //!
   TBranch        *b_mContamPairs_mPhiMc;   //!
   TBranch        *b_mContamPairs_mNHitMc;   //!
   TBranch        *b_mContamPairs_mGeantId;   //!
   TBranch        *b_mContamPairs_mChargeMc;   //!
   TBranch        *b_mContamPairs_mStopR;   //!
   TBranch        *b_mContamPairs_mNAssocGl;   //!
   TBranch        *b_mContamPairs_mNAssocPr;   //!
   TBranch        *b_mContamPairs_fUniqueID;   //!
   TBranch        *b_mContamPairs_fBits;   //!
   TBranch        *b_mContamPairs_mPtPr;   //!
   TBranch        *b_mContamPairs_mPzPr;   //!
   TBranch        *b_mContamPairs_mEtaPr;   //!
   TBranch        *b_mContamPairs_mPhiPr;   //!
   TBranch        *b_mContamPairs_mDcaPr;   //!
   TBranch        *b_mContamPairs_mDcaXYPr;   //!
   TBranch        *b_mContamPairs_mDcaZPr;   //!
   TBranch        *b_mContamPairs_mChi2Pr;   //!
   TBranch        *b_mContamPairs_mFlag;   //!
   TBranch        *b_mContamPairs_mDedx;   //!
   TBranch        *b_mContamPairs_mPtGl;   //!
   TBranch        *b_mContamPairs_mPzGl;   //!
   TBranch        *b_mContamPairs_mEtaGl;   //!
   TBranch        *b_mContamPairs_mPhiGl;   //!
   TBranch        *b_mContamPairs_mDcaGl;   //!
   TBranch        *b_mContamPairs_mDcaXYGl;   //!
   TBranch        *b_mContamPairs_mDcaZGl;   //!
   TBranch        *b_mContamPairs_mPidPion;   //!
   TBranch        *b_mContamPairs_mPidProton;   //!
   TBranch        *b_mContamPairs_mPidKaon;   //!
   TBranch        *b_mContamPairs_mPidElectron;   //!
   TBranch        *b_mContamPairs_mFirstZ;   //!
   TBranch        *b_mContamPairs_mLastZ;   //!
   TBranch        *b_mContamPairs_mFirstPadrow;   //!
   TBranch        *b_mContamPairs_mLastPadrow;   //!
   TBranch        *b_mContamPairs_mFirstFitPadrow;   //!
   TBranch        *b_mContamPairs_mLastFitPadrow;   //!
   TBranch        *b_mContamPairs_mFirstSector;   //!
   TBranch        *b_mContamPairs_mLastSector;   //!
   TBranch        *b_mContamPairs_mFitPts;   //!
   TBranch        *b_mContamPairs_mAllPts;   //!
   TBranch        *b_mContamPairs_mCharge;   //!
   TBranch        *b_mContamPairs_mNAssocMc;   //!
   TBranch        *b_mContamPairs_mNPossible;   //!
   TBranch        *b_fUniqueID;   //!
   TBranch        *b_fBits;   //!

   standardPlots(TChain *tree=0,char* infile="/star/data22/ITTF/EvalData/MCNtuple/auau200.rcf0183_12.190.root");
   //standardPlots(char* infile="/star/data22/ITTF/EvalData/MCNtuple/auau200.rcf0183_12.190.root");
   ~standardPlots();

   //cut arrays
   float  multCut[2];
   float  zCut[2];
   float  ptCut[2];
   float  etaCut[2];
   float  nHitCut[2];
   float  dca[2];
   float  primary;
   float  global;

	 Int_t ientry;
   Int_t nentries;
   Int_t nbytes;
   Int_t nb;

   Int_t  GetEntry(Int_t entry);
   Int_t  LoadTree(Int_t entry);
   void   Init(TChain *tree);
   void   Loop();
   Bool_t Notify();
   void   Show(Int_t entry = -1);

   void    SetEventCutMult(float low, float high){multCut[0]=low;multCut[1]=high;};
   void    SetEventCutZ(float low, float high){zCut[0]=low;zCut[1]=high;};
   Int_t  Cut(Int_t entry);

   void    SetTrackCutPt(float low, float high){ptCut[0]=low;ptCut[1]=high;};
   void    SetTrackCutEta(float low, float high){etaCut[0]=low;etaCut[1]=high;};
   void    SetTrackCutNHit(float low, float high){nHitCut[0]=low;nHitCut[1]=high;};   
   void    SetTrackCutDca(float low, float high){dca[0]=low;dca[1]=high;};
   int    trackCut(int entry, int track);
   int    mcTrackCut(int entry, int track);
   void   showCuts();

   //functions to make plots
   void   makeTrackEffPlots(const TString & path,const TString & fileName);
   void   makeMomentumPlots();
   //void   makeHitEffPlots();
   void   makeFitPointsPlots();

	 void calculateRel(const TProfile *p1, const TProfile *p2, 
										 TH1D *h1, TH1D *h2);
	 void print(const TString & path,const TString &fileName);

	 TH1D * hPt[4];
	 TH1D * hMcPt[4];
	 TH1D * hPtR[4];
	 TH1D * hMcEta[4];
	 TH1D * hEta[4];
	 TH1D * hEtaR[4];

	 TProfile * pDPt[4];
	 TProfile * pDPz[4];
	 TProfile * pDEta[4];
	 TProfile * pDPt2[4];
	 TProfile * pDPz2[4];
	 TProfile * pDEta2[4];

	 TH1D * pDPtR[4];
	 TH1D * pDPzR[4];
	 TH1D * pDEtaR[4];
	 TH1D * pDPt2R[4];
	 TH1D * pDPz2R[4];
	 TH1D * pDEta2R[4];

	 TH1D     *hitEff[4];
	 TProfile *hitEffMult;
	 TProfile *hitEffPt[4];
	 TProfile *hitEffEta[4];
	 
};

#endif

#ifdef standardPlots_cxx


standardPlots::standardPlots(TChain *tree, char* infile)
{
// if parameter tree is not specified (or zero), connect the file
// used to generate this class and read the Tree.
   if (tree == 0) {
      TFile *f = (TFile*)gROOT->GetListOfFiles()->FindObject(infile);
      if (!f) {
         f = new TFile(infile);
      }
      tree = (TChain*)gDirectory->Get("StMiniMcTree");

   }
   else{
     tree->Add("EvalItTestrcf0183_10_300evts.minimc.root");
     tree->Add("EvalItTestrcf0183_20_300evts.minimc.root");
     tree->Add("EvalItTestrcf0183_21_300evts.minimc.root");
     tree->Add("EvalItTestrcf0183_22_300evts.minimc.root");
     tree->Add("EvalItTestrcf0183_23_300evts.minimc.root");
     tree->Add("EvalItTestrcf0183_24_300evts.minimc.root");
     tree->Add("EvalItTestrcf0183_25_300evts.minimc.root");
     tree->Add("EvalItTestrcf0183_30_300evts.minimc.root");
     tree->Add("EvalItTestrcf0183_31_300evts.minimc.root");
     tree->Add("EvalItTestrcf0183_32_300evts.minimc.root");
     tree->Add("EvalItTestrcf0183_33_300evts.minimc.root");
     tree->Add("EvalItTestrcf0183_34_300evts.minimc.root");
     tree->Add("EvalItTestrcf0183_35_300evts.minimc.root");
     tree->Add("EvalItTestrcf0183_37_300evts.minimc.root"); 
     tree->Add("EvalItTestrcf0183_38_300evts.minimc.root");
     tree->Add("EvalItTestrcf0183_39_300evts.minimc.root");
     tree->Add("EvalItTestrcf0183_40_300evts.minimc.root");
     tree->Add("EvalItTestrcf0183_41_300evts.minimc.root");
   }

   cout <<"There are "<<tree->GetEntries()<<" events in the tree."
	<<endl;


   Init(tree);

   SetTrackCutPt(0.,20.);
   SetTrackCutEta(-1.5,1.5);
   SetTrackCutNHit(25.,55.);
   SetTrackCutDca(0.,20.);
   SetEventCutMult(0.,10000.);
   SetEventCutZ(-30.,30.);
}

standardPlots::~standardPlots()
{
   if (!fChain) return;
   delete fChain->GetCurrentFile();
}

Int_t standardPlots::GetEntry(Int_t entry)
{
// Read contents of entry.
   if (!fChain) return 0;
   return fChain->GetEntry(entry);
}

Int_t standardPlots::LoadTree(Int_t entry)
{
// Set the environment to read one entry
   if (!fChain) return -5;
   Int_t centry = fChain->LoadTree(entry);
   if (centry < 0) return centry;
   if (fChain->IsA() != TChain::Class()) return centry;
   TChain *chain = (TChain*)fChain;
   if (chain->GetTreeNumber() != fCurrent) {
      fCurrent = chain->GetTreeNumber();
      Notify();
   }
   return centry;
}

void standardPlots::Init(TChain *tree)
{
//   Set branch addresses
   if (tree == 0) return;
   fChain    = tree;
   fCurrent = -1;
   fChain->SetMakeClass(1);

   fChain->SetBranchAddress("mEventId",&mEventId);
   fChain->SetBranchAddress("mRunId",&mRunId);
   fChain->SetBranchAddress("mOriginMult",&mOriginMult);
   fChain->SetBranchAddress("mCentralMult",&mCentralMult);
   fChain->SetBranchAddress("mCentrality",&mCentrality);
   fChain->SetBranchAddress("mNUncorrectedNegativePrimaries",&mNUncorrectedNegativePrimaries);
   fChain->SetBranchAddress("mNUncorrectedPrimaries",&mNUncorrectedPrimaries);
   fChain->SetBranchAddress("mMcMult",&mMcMult);
   fChain->SetBranchAddress("mVertexX",&mVertexX);
   fChain->SetBranchAddress("mVertexY",&mVertexY);
   fChain->SetBranchAddress("mVertexZ",&mVertexZ);
   fChain->SetBranchAddress("mMcVertexX",&mMcVertexX);
   fChain->SetBranchAddress("mMcVertexY",&mMcVertexY);
   fChain->SetBranchAddress("mMcVertexZ",&mMcVertexZ);
   fChain->SetBranchAddress("mMagField",&mMagField);
   fChain->SetBranchAddress("mCTB",&mCTB);
   fChain->SetBranchAddress("mZDCe",&mZDCe);
   fChain->SetBranchAddress("mZDCw",&mZDCw);
   fChain->SetBranchAddress("mNMcTrack",&mNMcTrack);
   fChain->SetBranchAddress("mNMatchedPair",&mNMatchedPair);
   fChain->SetBranchAddress("mNMergedPair",&mNMergedPair);
   fChain->SetBranchAddress("mNSplitPair",&mNSplitPair);
   fChain->SetBranchAddress("mNGhostPair",&mNGhostPair);
   fChain->SetBranchAddress("mNContamPair",&mNContamPair);
   fChain->SetBranchAddress("mMcTracks_",&mMcTracks_);
   fChain->SetBranchAddress("mMcTracks.mPtMc",mMcTracks_mPtMc);
   fChain->SetBranchAddress("mMcTracks.mPzMc",mMcTracks_mPzMc);
   fChain->SetBranchAddress("mMcTracks.mEtaMc",mMcTracks_mEtaMc);
   fChain->SetBranchAddress("mMcTracks.mPhiMc",mMcTracks_mPhiMc);
   fChain->SetBranchAddress("mMcTracks.mNHitMc",mMcTracks_mNHitMc);
   fChain->SetBranchAddress("mMcTracks.mGeantId",mMcTracks_mGeantId);
   fChain->SetBranchAddress("mMcTracks.mChargeMc",mMcTracks_mChargeMc);
   fChain->SetBranchAddress("mMcTracks.mStopR",mMcTracks_mStopR);
   fChain->SetBranchAddress("mMcTracks.mNAssocGl",mMcTracks_mNAssocGl);
   fChain->SetBranchAddress("mMcTracks.mNAssocPr",mMcTracks_mNAssocPr);
   fChain->SetBranchAddress("mMcTracks.fUniqueID",mMcTracks_fUniqueID);
   fChain->SetBranchAddress("mMcTracks.fBits",mMcTracks_fBits);
   fChain->SetBranchAddress("mMatchedPairs_",&mMatchedPairs_);
   fChain->SetBranchAddress("mMatchedPairs.mNCommonHit",mMatchedPairs_mNCommonHit);
   fChain->SetBranchAddress("mMatchedPairs.mIsBestContam",mMatchedPairs_mIsBestContam);
   fChain->SetBranchAddress("mMatchedPairs.mPtMc",mMatchedPairs_mPtMc);
   fChain->SetBranchAddress("mMatchedPairs.mPzMc",mMatchedPairs_mPzMc);
   fChain->SetBranchAddress("mMatchedPairs.mEtaMc",mMatchedPairs_mEtaMc);
   fChain->SetBranchAddress("mMatchedPairs.mPhiMc",mMatchedPairs_mPhiMc);
   fChain->SetBranchAddress("mMatchedPairs.mNHitMc",mMatchedPairs_mNHitMc);
   fChain->SetBranchAddress("mMatchedPairs.mGeantId",mMatchedPairs_mGeantId);
   fChain->SetBranchAddress("mMatchedPairs.mChargeMc",mMatchedPairs_mChargeMc);
   fChain->SetBranchAddress("mMatchedPairs.mStopR",mMatchedPairs_mStopR);
   fChain->SetBranchAddress("mMatchedPairs.mNAssocGl",mMatchedPairs_mNAssocGl);
   fChain->SetBranchAddress("mMatchedPairs.mNAssocPr",mMatchedPairs_mNAssocPr);
   fChain->SetBranchAddress("mMatchedPairs.fUniqueID",mMatchedPairs_fUniqueID);
   fChain->SetBranchAddress("mMatchedPairs.fBits",mMatchedPairs_fBits);
   fChain->SetBranchAddress("mMatchedPairs.mPtPr",mMatchedPairs_mPtPr);
   fChain->SetBranchAddress("mMatchedPairs.mPzPr",mMatchedPairs_mPzPr);
   fChain->SetBranchAddress("mMatchedPairs.mEtaPr",mMatchedPairs_mEtaPr);
   fChain->SetBranchAddress("mMatchedPairs.mPhiPr",mMatchedPairs_mPhiPr);
   fChain->SetBranchAddress("mMatchedPairs.mDcaPr",mMatchedPairs_mDcaPr);
   fChain->SetBranchAddress("mMatchedPairs.mDcaXYPr",mMatchedPairs_mDcaXYPr);
   fChain->SetBranchAddress("mMatchedPairs.mDcaZPr",mMatchedPairs_mDcaZPr);
   fChain->SetBranchAddress("mMatchedPairs.mChi2Pr",mMatchedPairs_mChi2Pr);
   fChain->SetBranchAddress("mMatchedPairs.mFlag",mMatchedPairs_mFlag);
   fChain->SetBranchAddress("mMatchedPairs.mDedx",mMatchedPairs_mDedx);
   fChain->SetBranchAddress("mMatchedPairs.mPtGl",mMatchedPairs_mPtGl);
   fChain->SetBranchAddress("mMatchedPairs.mPzGl",mMatchedPairs_mPzGl);
   fChain->SetBranchAddress("mMatchedPairs.mEtaGl",mMatchedPairs_mEtaGl);
   fChain->SetBranchAddress("mMatchedPairs.mPhiGl",mMatchedPairs_mPhiGl);
   fChain->SetBranchAddress("mMatchedPairs.mDcaGl",mMatchedPairs_mDcaGl);
   fChain->SetBranchAddress("mMatchedPairs.mDcaXYGl",mMatchedPairs_mDcaXYGl);
   fChain->SetBranchAddress("mMatchedPairs.mDcaZGl",mMatchedPairs_mDcaZGl);
   fChain->SetBranchAddress("mMatchedPairs.mPidPion",mMatchedPairs_mPidPion);
   fChain->SetBranchAddress("mMatchedPairs.mPidProton",mMatchedPairs_mPidProton);
   fChain->SetBranchAddress("mMatchedPairs.mPidKaon",mMatchedPairs_mPidKaon);
   fChain->SetBranchAddress("mMatchedPairs.mPidElectron",mMatchedPairs_mPidElectron);
   fChain->SetBranchAddress("mMatchedPairs.mFirstZ",mMatchedPairs_mFirstZ);
   fChain->SetBranchAddress("mMatchedPairs.mLastZ",mMatchedPairs_mLastZ);
   fChain->SetBranchAddress("mMatchedPairs.mFirstPadrow",mMatchedPairs_mFirstPadrow);
   fChain->SetBranchAddress("mMatchedPairs.mLastPadrow",mMatchedPairs_mLastPadrow);
   fChain->SetBranchAddress("mMatchedPairs.mFirstFitPadrow",mMatchedPairs_mFirstFitPadrow);
   fChain->SetBranchAddress("mMatchedPairs.mLastFitPadrow",mMatchedPairs_mLastFitPadrow);
   fChain->SetBranchAddress("mMatchedPairs.mFirstSector",mMatchedPairs_mFirstSector);
   fChain->SetBranchAddress("mMatchedPairs.mLastSector",mMatchedPairs_mLastSector);
   fChain->SetBranchAddress("mMatchedPairs.mFitPts",mMatchedPairs_mFitPts);
   fChain->SetBranchAddress("mMatchedPairs.mAllPts",mMatchedPairs_mAllPts);
   fChain->SetBranchAddress("mMatchedPairs.mCharge",mMatchedPairs_mCharge);
   fChain->SetBranchAddress("mMatchedPairs.mNAssocMc",mMatchedPairs_mNAssocMc);
   fChain->SetBranchAddress("mMatchedPairs.mNPossible",mMatchedPairs_mNPossible);
   fChain->SetBranchAddress("mMergedPairs_",&mMergedPairs_);
   fChain->SetBranchAddress("mMergedPairs.mNCommonHit",mMergedPairs_mNCommonHit);
   fChain->SetBranchAddress("mMergedPairs.mIsBestContam",mMergedPairs_mIsBestContam);
   fChain->SetBranchAddress("mMergedPairs.mPtMc",mMergedPairs_mPtMc);
   fChain->SetBranchAddress("mMergedPairs.mPzMc",mMergedPairs_mPzMc);
   fChain->SetBranchAddress("mMergedPairs.mEtaMc",mMergedPairs_mEtaMc);
   fChain->SetBranchAddress("mMergedPairs.mPhiMc",mMergedPairs_mPhiMc);
   fChain->SetBranchAddress("mMergedPairs.mNHitMc",mMergedPairs_mNHitMc);
   fChain->SetBranchAddress("mMergedPairs.mGeantId",mMergedPairs_mGeantId);
   fChain->SetBranchAddress("mMergedPairs.mChargeMc",mMergedPairs_mChargeMc);
   fChain->SetBranchAddress("mMergedPairs.mStopR",mMergedPairs_mStopR);
   fChain->SetBranchAddress("mMergedPairs.mNAssocGl",mMergedPairs_mNAssocGl);
   fChain->SetBranchAddress("mMergedPairs.mNAssocPr",mMergedPairs_mNAssocPr);
   fChain->SetBranchAddress("mMergedPairs.fUniqueID",mMergedPairs_fUniqueID);
   fChain->SetBranchAddress("mMergedPairs.fBits",mMergedPairs_fBits);
   fChain->SetBranchAddress("mMergedPairs.mPtPr",mMergedPairs_mPtPr);
   fChain->SetBranchAddress("mMergedPairs.mPzPr",mMergedPairs_mPzPr);
   fChain->SetBranchAddress("mMergedPairs.mEtaPr",mMergedPairs_mEtaPr);
   fChain->SetBranchAddress("mMergedPairs.mPhiPr",mMergedPairs_mPhiPr);
   fChain->SetBranchAddress("mMergedPairs.mDcaPr",mMergedPairs_mDcaPr);
   fChain->SetBranchAddress("mMergedPairs.mDcaXYPr",mMergedPairs_mDcaXYPr);
   fChain->SetBranchAddress("mMergedPairs.mDcaZPr",mMergedPairs_mDcaZPr);
   fChain->SetBranchAddress("mMergedPairs.mChi2Pr",mMergedPairs_mChi2Pr);
   fChain->SetBranchAddress("mMergedPairs.mFlag",mMergedPairs_mFlag);
   fChain->SetBranchAddress("mMergedPairs.mDedx",mMergedPairs_mDedx);
   fChain->SetBranchAddress("mMergedPairs.mPtGl",mMergedPairs_mPtGl);
   fChain->SetBranchAddress("mMergedPairs.mPzGl",mMergedPairs_mPzGl);
   fChain->SetBranchAddress("mMergedPairs.mEtaGl",mMergedPairs_mEtaGl);
   fChain->SetBranchAddress("mMergedPairs.mPhiGl",mMergedPairs_mPhiGl);
   fChain->SetBranchAddress("mMergedPairs.mDcaGl",mMergedPairs_mDcaGl);
   fChain->SetBranchAddress("mMergedPairs.mDcaXYGl",mMergedPairs_mDcaXYGl);
   fChain->SetBranchAddress("mMergedPairs.mDcaZGl",mMergedPairs_mDcaZGl);
   fChain->SetBranchAddress("mMergedPairs.mPidPion",mMergedPairs_mPidPion);
   fChain->SetBranchAddress("mMergedPairs.mPidProton",mMergedPairs_mPidProton);
   fChain->SetBranchAddress("mMergedPairs.mPidKaon",mMergedPairs_mPidKaon);
   fChain->SetBranchAddress("mMergedPairs.mPidElectron",mMergedPairs_mPidElectron);
   fChain->SetBranchAddress("mMergedPairs.mFirstZ",mMergedPairs_mFirstZ);
   fChain->SetBranchAddress("mMergedPairs.mLastZ",mMergedPairs_mLastZ);
   fChain->SetBranchAddress("mMergedPairs.mFirstPadrow",mMergedPairs_mFirstPadrow);
   fChain->SetBranchAddress("mMergedPairs.mLastPadrow",mMergedPairs_mLastPadrow);
   fChain->SetBranchAddress("mMergedPairs.mFirstFitPadrow",mMergedPairs_mFirstFitPadrow);
   fChain->SetBranchAddress("mMergedPairs.mLastFitPadrow",mMergedPairs_mLastFitPadrow);
   fChain->SetBranchAddress("mMergedPairs.mFirstSector",mMergedPairs_mFirstSector);
   fChain->SetBranchAddress("mMergedPairs.mLastSector",mMergedPairs_mLastSector);
   fChain->SetBranchAddress("mMergedPairs.mFitPts",mMergedPairs_mFitPts);
   fChain->SetBranchAddress("mMergedPairs.mAllPts",mMergedPairs_mAllPts);
   fChain->SetBranchAddress("mMergedPairs.mCharge",mMergedPairs_mCharge);
   fChain->SetBranchAddress("mMergedPairs.mNAssocMc",mMergedPairs_mNAssocMc);
   fChain->SetBranchAddress("mMergedPairs.mNPossible",mMergedPairs_mNPossible);
   fChain->SetBranchAddress("mSplitPairs_",&mSplitPairs_);
   fChain->SetBranchAddress("mSplitPairs.mNCommonHit",&mSplitPairs_mNCommonHit);
   fChain->SetBranchAddress("mSplitPairs.mIsBestContam",&mSplitPairs_mIsBestContam);
   fChain->SetBranchAddress("mSplitPairs.mPtMc",&mSplitPairs_mPtMc);
   fChain->SetBranchAddress("mSplitPairs.mPzMc",&mSplitPairs_mPzMc);
   fChain->SetBranchAddress("mSplitPairs.mEtaMc",&mSplitPairs_mEtaMc);
   fChain->SetBranchAddress("mSplitPairs.mPhiMc",&mSplitPairs_mPhiMc);
   fChain->SetBranchAddress("mSplitPairs.mNHitMc",&mSplitPairs_mNHitMc);
   fChain->SetBranchAddress("mSplitPairs.mGeantId",&mSplitPairs_mGeantId);
   fChain->SetBranchAddress("mSplitPairs.mChargeMc",&mSplitPairs_mChargeMc);
   fChain->SetBranchAddress("mSplitPairs.mStopR",&mSplitPairs_mStopR);
   fChain->SetBranchAddress("mSplitPairs.mNAssocGl",&mSplitPairs_mNAssocGl);
   fChain->SetBranchAddress("mSplitPairs.mNAssocPr",&mSplitPairs_mNAssocPr);
   fChain->SetBranchAddress("mSplitPairs.fUniqueID",&mSplitPairs_fUniqueID);
   fChain->SetBranchAddress("mSplitPairs.fBits",&mSplitPairs_fBits);
   fChain->SetBranchAddress("mSplitPairs.mPtPr",&mSplitPairs_mPtPr);
   fChain->SetBranchAddress("mSplitPairs.mPzPr",&mSplitPairs_mPzPr);
   fChain->SetBranchAddress("mSplitPairs.mEtaPr",&mSplitPairs_mEtaPr);
   fChain->SetBranchAddress("mSplitPairs.mPhiPr",&mSplitPairs_mPhiPr);
   fChain->SetBranchAddress("mSplitPairs.mDcaPr",&mSplitPairs_mDcaPr);
   fChain->SetBranchAddress("mSplitPairs.mDcaXYPr",&mSplitPairs_mDcaXYPr);
   fChain->SetBranchAddress("mSplitPairs.mDcaZPr",&mSplitPairs_mDcaZPr);
   fChain->SetBranchAddress("mSplitPairs.mChi2Pr",&mSplitPairs_mChi2Pr);
   fChain->SetBranchAddress("mSplitPairs.mFlag",&mSplitPairs_mFlag);
   fChain->SetBranchAddress("mSplitPairs.mDedx",&mSplitPairs_mDedx);
   fChain->SetBranchAddress("mSplitPairs.mPtGl",&mSplitPairs_mPtGl);
   fChain->SetBranchAddress("mSplitPairs.mPzGl",&mSplitPairs_mPzGl);
   fChain->SetBranchAddress("mSplitPairs.mEtaGl",&mSplitPairs_mEtaGl);
   fChain->SetBranchAddress("mSplitPairs.mPhiGl",&mSplitPairs_mPhiGl);
   fChain->SetBranchAddress("mSplitPairs.mDcaGl",&mSplitPairs_mDcaGl);
   fChain->SetBranchAddress("mSplitPairs.mDcaXYGl",&mSplitPairs_mDcaXYGl);
   fChain->SetBranchAddress("mSplitPairs.mDcaZGl",&mSplitPairs_mDcaZGl);
   fChain->SetBranchAddress("mSplitPairs.mPidPion",&mSplitPairs_mPidPion);
   fChain->SetBranchAddress("mSplitPairs.mPidProton",&mSplitPairs_mPidProton);
   fChain->SetBranchAddress("mSplitPairs.mPidKaon",&mSplitPairs_mPidKaon);
   fChain->SetBranchAddress("mSplitPairs.mPidElectron",&mSplitPairs_mPidElectron);
   fChain->SetBranchAddress("mSplitPairs.mFirstZ",&mSplitPairs_mFirstZ);
   fChain->SetBranchAddress("mSplitPairs.mLastZ",&mSplitPairs_mLastZ);
   fChain->SetBranchAddress("mSplitPairs.mFirstPadrow",&mSplitPairs_mFirstPadrow);
   fChain->SetBranchAddress("mSplitPairs.mLastPadrow",&mSplitPairs_mLastPadrow);
   fChain->SetBranchAddress("mSplitPairs.mFirstFitPadrow",&mSplitPairs_mFirstFitPadrow);
   fChain->SetBranchAddress("mSplitPairs.mLastFitPadrow",&mSplitPairs_mLastFitPadrow);
   fChain->SetBranchAddress("mSplitPairs.mFirstSector",&mSplitPairs_mFirstSector);
   fChain->SetBranchAddress("mSplitPairs.mLastSector",&mSplitPairs_mLastSector);
   fChain->SetBranchAddress("mSplitPairs.mFitPts",&mSplitPairs_mFitPts);
   fChain->SetBranchAddress("mSplitPairs.mAllPts",&mSplitPairs_mAllPts);
   fChain->SetBranchAddress("mSplitPairs.mCharge",&mSplitPairs_mCharge);
   fChain->SetBranchAddress("mSplitPairs.mNAssocMc",&mSplitPairs_mNAssocMc);
   fChain->SetBranchAddress("mSplitPairs.mNPossible",&mSplitPairs_mNPossible);
   fChain->SetBranchAddress("mGhostPairs_",&mGhostPairs_);
   fChain->SetBranchAddress("mGhostPairs.mNCommonHit",&mGhostPairs_mNCommonHit);
   fChain->SetBranchAddress("mGhostPairs.mIsBestContam",&mGhostPairs_mIsBestContam);
   fChain->SetBranchAddress("mGhostPairs.mPtMc",&mGhostPairs_mPtMc);
   fChain->SetBranchAddress("mGhostPairs.mPzMc",&mGhostPairs_mPzMc);
   fChain->SetBranchAddress("mGhostPairs.mEtaMc",&mGhostPairs_mEtaMc);
   fChain->SetBranchAddress("mGhostPairs.mPhiMc",&mGhostPairs_mPhiMc);
   fChain->SetBranchAddress("mGhostPairs.mNHitMc",&mGhostPairs_mNHitMc);
   fChain->SetBranchAddress("mGhostPairs.mGeantId",&mGhostPairs_mGeantId);
   fChain->SetBranchAddress("mGhostPairs.mChargeMc",&mGhostPairs_mChargeMc);
   fChain->SetBranchAddress("mGhostPairs.mStopR",&mGhostPairs_mStopR);
   fChain->SetBranchAddress("mGhostPairs.mNAssocGl",&mGhostPairs_mNAssocGl);
   fChain->SetBranchAddress("mGhostPairs.mNAssocPr",&mGhostPairs_mNAssocPr);
   fChain->SetBranchAddress("mGhostPairs.fUniqueID",&mGhostPairs_fUniqueID);
   fChain->SetBranchAddress("mGhostPairs.fBits",&mGhostPairs_fBits);
   fChain->SetBranchAddress("mGhostPairs.mPtPr",&mGhostPairs_mPtPr);
   fChain->SetBranchAddress("mGhostPairs.mPzPr",&mGhostPairs_mPzPr);
   fChain->SetBranchAddress("mGhostPairs.mEtaPr",&mGhostPairs_mEtaPr);
   fChain->SetBranchAddress("mGhostPairs.mPhiPr",&mGhostPairs_mPhiPr);
   fChain->SetBranchAddress("mGhostPairs.mDcaPr",&mGhostPairs_mDcaPr);
   fChain->SetBranchAddress("mGhostPairs.mDcaXYPr",&mGhostPairs_mDcaXYPr);
   fChain->SetBranchAddress("mGhostPairs.mDcaZPr",&mGhostPairs_mDcaZPr);
   fChain->SetBranchAddress("mGhostPairs.mChi2Pr",&mGhostPairs_mChi2Pr);
   fChain->SetBranchAddress("mGhostPairs.mFlag",&mGhostPairs_mFlag);
   fChain->SetBranchAddress("mGhostPairs.mDedx",&mGhostPairs_mDedx);
   fChain->SetBranchAddress("mGhostPairs.mPtGl",&mGhostPairs_mPtGl);
   fChain->SetBranchAddress("mGhostPairs.mPzGl",&mGhostPairs_mPzGl);
   fChain->SetBranchAddress("mGhostPairs.mEtaGl",&mGhostPairs_mEtaGl);
   fChain->SetBranchAddress("mGhostPairs.mPhiGl",&mGhostPairs_mPhiGl);
   fChain->SetBranchAddress("mGhostPairs.mDcaGl",&mGhostPairs_mDcaGl);
   fChain->SetBranchAddress("mGhostPairs.mDcaXYGl",&mGhostPairs_mDcaXYGl);
   fChain->SetBranchAddress("mGhostPairs.mDcaZGl",&mGhostPairs_mDcaZGl);
   fChain->SetBranchAddress("mGhostPairs.mPidPion",&mGhostPairs_mPidPion);
   fChain->SetBranchAddress("mGhostPairs.mPidProton",&mGhostPairs_mPidProton);
   fChain->SetBranchAddress("mGhostPairs.mPidKaon",&mGhostPairs_mPidKaon);
   fChain->SetBranchAddress("mGhostPairs.mPidElectron",&mGhostPairs_mPidElectron);
   fChain->SetBranchAddress("mGhostPairs.mFirstZ",&mGhostPairs_mFirstZ);
   fChain->SetBranchAddress("mGhostPairs.mLastZ",&mGhostPairs_mLastZ);
   fChain->SetBranchAddress("mGhostPairs.mFirstPadrow",&mGhostPairs_mFirstPadrow);
   fChain->SetBranchAddress("mGhostPairs.mLastPadrow",&mGhostPairs_mLastPadrow);
   fChain->SetBranchAddress("mGhostPairs.mFirstFitPadrow",&mGhostPairs_mFirstFitPadrow);
   fChain->SetBranchAddress("mGhostPairs.mLastFitPadrow",&mGhostPairs_mLastFitPadrow);
   fChain->SetBranchAddress("mGhostPairs.mFirstSector",&mGhostPairs_mFirstSector);
   fChain->SetBranchAddress("mGhostPairs.mLastSector",&mGhostPairs_mLastSector);
   fChain->SetBranchAddress("mGhostPairs.mFitPts",&mGhostPairs_mFitPts);
   fChain->SetBranchAddress("mGhostPairs.mAllPts",&mGhostPairs_mAllPts);
   fChain->SetBranchAddress("mGhostPairs.mCharge",&mGhostPairs_mCharge);
   fChain->SetBranchAddress("mGhostPairs.mNAssocMc",&mGhostPairs_mNAssocMc);
   fChain->SetBranchAddress("mGhostPairs.mNPossible",&mGhostPairs_mNPossible);
   fChain->SetBranchAddress("mContamPairs_",&mContamPairs_);
   fChain->SetBranchAddress("mContamPairs.mParentGeantId",&mContamPairs_mParentGeantId);
   fChain->SetBranchAddress("mContamPairs.mGeantProcess",&mContamPairs_mGeantProcess);
   fChain->SetBranchAddress("mContamPairs.mPtMcParent",&mContamPairs_mPtMcParent);
   fChain->SetBranchAddress("mContamPairs.mEtaMcParent",&mContamPairs_mEtaMcParent);
   fChain->SetBranchAddress("mContamPairs.mParentParentGeantId",&mContamPairs_mParentParentGeantId);
   fChain->SetBranchAddress("mContamPairs.mPtMcParentParent",&mContamPairs_mPtMcParentParent);
   fChain->SetBranchAddress("mContamPairs.mStartX",&mContamPairs_mStartX);
   fChain->SetBranchAddress("mContamPairs.mStartY",&mContamPairs_mStartY);
   fChain->SetBranchAddress("mContamPairs.mStartZ",&mContamPairs_mStartZ);
   fChain->SetBranchAddress("mContamPairs.mNCommonHit",&mContamPairs_mNCommonHit);
   fChain->SetBranchAddress("mContamPairs.mIsBestContam",&mContamPairs_mIsBestContam);
   fChain->SetBranchAddress("mContamPairs.mPtMc",&mContamPairs_mPtMc);
   fChain->SetBranchAddress("mContamPairs.mPzMc",&mContamPairs_mPzMc);
   fChain->SetBranchAddress("mContamPairs.mEtaMc",&mContamPairs_mEtaMc);
   fChain->SetBranchAddress("mContamPairs.mPhiMc",&mContamPairs_mPhiMc);
   fChain->SetBranchAddress("mContamPairs.mNHitMc",&mContamPairs_mNHitMc);
   fChain->SetBranchAddress("mContamPairs.mGeantId",&mContamPairs_mGeantId);
   fChain->SetBranchAddress("mContamPairs.mChargeMc",&mContamPairs_mChargeMc);
   fChain->SetBranchAddress("mContamPairs.mStopR",&mContamPairs_mStopR);
   fChain->SetBranchAddress("mContamPairs.mNAssocGl",&mContamPairs_mNAssocGl);
   fChain->SetBranchAddress("mContamPairs.mNAssocPr",&mContamPairs_mNAssocPr);
   fChain->SetBranchAddress("mContamPairs.fUniqueID",&mContamPairs_fUniqueID);
   fChain->SetBranchAddress("mContamPairs.fBits",&mContamPairs_fBits);
   fChain->SetBranchAddress("mContamPairs.mPtPr",&mContamPairs_mPtPr);
   fChain->SetBranchAddress("mContamPairs.mPzPr",&mContamPairs_mPzPr);
   fChain->SetBranchAddress("mContamPairs.mEtaPr",&mContamPairs_mEtaPr);
   fChain->SetBranchAddress("mContamPairs.mPhiPr",&mContamPairs_mPhiPr);
   fChain->SetBranchAddress("mContamPairs.mDcaPr",&mContamPairs_mDcaPr);
   fChain->SetBranchAddress("mContamPairs.mDcaXYPr",&mContamPairs_mDcaXYPr);
   fChain->SetBranchAddress("mContamPairs.mDcaZPr",&mContamPairs_mDcaZPr);
   fChain->SetBranchAddress("mContamPairs.mChi2Pr",&mContamPairs_mChi2Pr);
   fChain->SetBranchAddress("mContamPairs.mFlag",&mContamPairs_mFlag);
   fChain->SetBranchAddress("mContamPairs.mDedx",&mContamPairs_mDedx);
   fChain->SetBranchAddress("mContamPairs.mPtGl",&mContamPairs_mPtGl);
   fChain->SetBranchAddress("mContamPairs.mPzGl",&mContamPairs_mPzGl);
   fChain->SetBranchAddress("mContamPairs.mEtaGl",&mContamPairs_mEtaGl);
   fChain->SetBranchAddress("mContamPairs.mPhiGl",&mContamPairs_mPhiGl);
   fChain->SetBranchAddress("mContamPairs.mDcaGl",&mContamPairs_mDcaGl);
   fChain->SetBranchAddress("mContamPairs.mDcaXYGl",&mContamPairs_mDcaXYGl);
   fChain->SetBranchAddress("mContamPairs.mDcaZGl",&mContamPairs_mDcaZGl);
   fChain->SetBranchAddress("mContamPairs.mPidPion",&mContamPairs_mPidPion);
   fChain->SetBranchAddress("mContamPairs.mPidProton",&mContamPairs_mPidProton);
   fChain->SetBranchAddress("mContamPairs.mPidKaon",&mContamPairs_mPidKaon);
   fChain->SetBranchAddress("mContamPairs.mPidElectron",&mContamPairs_mPidElectron);
   fChain->SetBranchAddress("mContamPairs.mFirstZ",&mContamPairs_mFirstZ);
   fChain->SetBranchAddress("mContamPairs.mLastZ",&mContamPairs_mLastZ);
   fChain->SetBranchAddress("mContamPairs.mFirstPadrow",&mContamPairs_mFirstPadrow);
   fChain->SetBranchAddress("mContamPairs.mLastPadrow",&mContamPairs_mLastPadrow);
   fChain->SetBranchAddress("mContamPairs.mFirstFitPadrow",&mContamPairs_mFirstFitPadrow);
   fChain->SetBranchAddress("mContamPairs.mLastFitPadrow",&mContamPairs_mLastFitPadrow);
   fChain->SetBranchAddress("mContamPairs.mFirstSector",&mContamPairs_mFirstSector);
   fChain->SetBranchAddress("mContamPairs.mLastSector",&mContamPairs_mLastSector);
   fChain->SetBranchAddress("mContamPairs.mFitPts",&mContamPairs_mFitPts);
   fChain->SetBranchAddress("mContamPairs.mAllPts",&mContamPairs_mAllPts);
   fChain->SetBranchAddress("mContamPairs.mCharge",&mContamPairs_mCharge);
   fChain->SetBranchAddress("mContamPairs.mNAssocMc",&mContamPairs_mNAssocMc);
   fChain->SetBranchAddress("mContamPairs.mNPossible",&mContamPairs_mNPossible);
   fChain->SetBranchAddress("fUniqueID",&fUniqueID);
   fChain->SetBranchAddress("fBits",&fBits);
   Notify();
}

Bool_t standardPlots::Notify()
{
   // Called when loading a new file.
   // Get branch pointers.
   b_mEventId = fChain->GetBranch("mEventId");
   b_mRunId = fChain->GetBranch("mRunId");
   b_mOriginMult = fChain->GetBranch("mOriginMult");
   b_mCentralMult = fChain->GetBranch("mCentralMult");
   b_mCentrality = fChain->GetBranch("mCentrality");
   b_mNUncorrectedNegativePrimaries = fChain->GetBranch("mNUncorrectedNegativePrimaries");
   b_mNUncorrectedPrimaries = fChain->GetBranch("mNUncorrectedPrimaries");
   b_mMcMult = fChain->GetBranch("mMcMult");
   b_mVertexX = fChain->GetBranch("mVertexX");
   b_mVertexY = fChain->GetBranch("mVertexY");
   b_mVertexZ = fChain->GetBranch("mVertexZ");
   b_mMcVertexX = fChain->GetBranch("mMcVertexX");
   b_mMcVertexY = fChain->GetBranch("mMcVertexY");
   b_mMcVertexZ = fChain->GetBranch("mMcVertexZ");
   b_mMagField = fChain->GetBranch("mMagField");
   b_mCTB = fChain->GetBranch("mCTB");
   b_mZDCe = fChain->GetBranch("mZDCe");
   b_mZDCw = fChain->GetBranch("mZDCw");
   b_mNMcTrack = fChain->GetBranch("mNMcTrack");
   b_mNMatchedPair = fChain->GetBranch("mNMatchedPair");
   b_mNMergedPair = fChain->GetBranch("mNMergedPair");
   b_mNSplitPair = fChain->GetBranch("mNSplitPair");
   b_mNGhostPair = fChain->GetBranch("mNGhostPair");
   b_mNContamPair = fChain->GetBranch("mNContamPair");
   b_mMcTracks_ = fChain->GetBranch("mMcTracks_");
   b_mMcTracks_mPtMc = fChain->GetBranch("mMcTracks.mPtMc");
   b_mMcTracks_mPzMc = fChain->GetBranch("mMcTracks.mPzMc");
   b_mMcTracks_mEtaMc = fChain->GetBranch("mMcTracks.mEtaMc");
   b_mMcTracks_mPhiMc = fChain->GetBranch("mMcTracks.mPhiMc");
   b_mMcTracks_mNHitMc = fChain->GetBranch("mMcTracks.mNHitMc");
   b_mMcTracks_mGeantId = fChain->GetBranch("mMcTracks.mGeantId");
   b_mMcTracks_mChargeMc = fChain->GetBranch("mMcTracks.mChargeMc");
   b_mMcTracks_mStopR = fChain->GetBranch("mMcTracks.mStopR");
   b_mMcTracks_mNAssocGl = fChain->GetBranch("mMcTracks.mNAssocGl");
   b_mMcTracks_mNAssocPr = fChain->GetBranch("mMcTracks.mNAssocPr");
   b_mMcTracks_fUniqueID = fChain->GetBranch("mMcTracks.fUniqueID");
   b_mMcTracks_fBits = fChain->GetBranch("mMcTracks.fBits");
   b_mMatchedPairs_ = fChain->GetBranch("mMatchedPairs_");
   b_mMatchedPairs_mNCommonHit = fChain->GetBranch("mMatchedPairs.mNCommonHit");
   b_mMatchedPairs_mIsBestContam = fChain->GetBranch("mMatchedPairs.mIsBestContam");
   b_mMatchedPairs_mPtMc = fChain->GetBranch("mMatchedPairs.mPtMc");
   b_mMatchedPairs_mPzMc = fChain->GetBranch("mMatchedPairs.mPzMc");
   b_mMatchedPairs_mEtaMc = fChain->GetBranch("mMatchedPairs.mEtaMc");
   b_mMatchedPairs_mPhiMc = fChain->GetBranch("mMatchedPairs.mPhiMc");
   b_mMatchedPairs_mNHitMc = fChain->GetBranch("mMatchedPairs.mNHitMc");
   b_mMatchedPairs_mGeantId = fChain->GetBranch("mMatchedPairs.mGeantId");
   b_mMatchedPairs_mChargeMc = fChain->GetBranch("mMatchedPairs.mChargeMc");
   b_mMatchedPairs_mStopR = fChain->GetBranch("mMatchedPairs.mStopR");
   b_mMatchedPairs_mNAssocGl = fChain->GetBranch("mMatchedPairs.mNAssocGl");
   b_mMatchedPairs_mNAssocPr = fChain->GetBranch("mMatchedPairs.mNAssocPr");
   b_mMatchedPairs_fUniqueID = fChain->GetBranch("mMatchedPairs.fUniqueID");
   b_mMatchedPairs_fBits = fChain->GetBranch("mMatchedPairs.fBits");
   b_mMatchedPairs_mPtPr = fChain->GetBranch("mMatchedPairs.mPtPr");
   b_mMatchedPairs_mPzPr = fChain->GetBranch("mMatchedPairs.mPzPr");
   b_mMatchedPairs_mEtaPr = fChain->GetBranch("mMatchedPairs.mEtaPr");
   b_mMatchedPairs_mPhiPr = fChain->GetBranch("mMatchedPairs.mPhiPr");
   b_mMatchedPairs_mDcaPr = fChain->GetBranch("mMatchedPairs.mDcaPr");
   b_mMatchedPairs_mDcaXYPr = fChain->GetBranch("mMatchedPairs.mDcaXYPr");
   b_mMatchedPairs_mDcaZPr = fChain->GetBranch("mMatchedPairs.mDcaZPr");
   b_mMatchedPairs_mChi2Pr = fChain->GetBranch("mMatchedPairs.mChi2Pr");
   b_mMatchedPairs_mFlag = fChain->GetBranch("mMatchedPairs.mFlag");
   b_mMatchedPairs_mDedx = fChain->GetBranch("mMatchedPairs.mDedx");
   b_mMatchedPairs_mPtGl = fChain->GetBranch("mMatchedPairs.mPtGl");
   b_mMatchedPairs_mPzGl = fChain->GetBranch("mMatchedPairs.mPzGl");
   b_mMatchedPairs_mEtaGl = fChain->GetBranch("mMatchedPairs.mEtaGl");
   b_mMatchedPairs_mPhiGl = fChain->GetBranch("mMatchedPairs.mPhiGl");
   b_mMatchedPairs_mDcaGl = fChain->GetBranch("mMatchedPairs.mDcaGl");
   b_mMatchedPairs_mDcaXYGl = fChain->GetBranch("mMatchedPairs.mDcaXYGl");
   b_mMatchedPairs_mDcaZGl = fChain->GetBranch("mMatchedPairs.mDcaZGl");
   b_mMatchedPairs_mPidPion = fChain->GetBranch("mMatchedPairs.mPidPion");
   b_mMatchedPairs_mPidProton = fChain->GetBranch("mMatchedPairs.mPidProton");
   b_mMatchedPairs_mPidKaon = fChain->GetBranch("mMatchedPairs.mPidKaon");
   b_mMatchedPairs_mPidElectron = fChain->GetBranch("mMatchedPairs.mPidElectron");
   b_mMatchedPairs_mFirstZ = fChain->GetBranch("mMatchedPairs.mFirstZ");
   b_mMatchedPairs_mLastZ = fChain->GetBranch("mMatchedPairs.mLastZ");
   b_mMatchedPairs_mFirstPadrow = fChain->GetBranch("mMatchedPairs.mFirstPadrow");
   b_mMatchedPairs_mLastPadrow = fChain->GetBranch("mMatchedPairs.mLastPadrow");
   b_mMatchedPairs_mFirstFitPadrow = fChain->GetBranch("mMatchedPairs.mFirstFitPadrow");
   b_mMatchedPairs_mLastFitPadrow = fChain->GetBranch("mMatchedPairs.mLastFitPadrow");
   b_mMatchedPairs_mFirstSector = fChain->GetBranch("mMatchedPairs.mFirstSector");
   b_mMatchedPairs_mLastSector = fChain->GetBranch("mMatchedPairs.mLastSector");
   b_mMatchedPairs_mFitPts = fChain->GetBranch("mMatchedPairs.mFitPts");
   b_mMatchedPairs_mAllPts = fChain->GetBranch("mMatchedPairs.mAllPts");
   b_mMatchedPairs_mCharge = fChain->GetBranch("mMatchedPairs.mCharge");
   b_mMatchedPairs_mNAssocMc = fChain->GetBranch("mMatchedPairs.mNAssocMc");
   b_mMatchedPairs_mNPossible = fChain->GetBranch("mMatchedPairs.mNPossible");
   b_mMergedPairs_ = fChain->GetBranch("mMergedPairs_");
   b_mMergedPairs_mNCommonHit = fChain->GetBranch("mMergedPairs.mNCommonHit");
   b_mMergedPairs_mIsBestContam = fChain->GetBranch("mMergedPairs.mIsBestContam");
   b_mMergedPairs_mPtMc = fChain->GetBranch("mMergedPairs.mPtMc");
   b_mMergedPairs_mPzMc = fChain->GetBranch("mMergedPairs.mPzMc");
   b_mMergedPairs_mEtaMc = fChain->GetBranch("mMergedPairs.mEtaMc");
   b_mMergedPairs_mPhiMc = fChain->GetBranch("mMergedPairs.mPhiMc");
   b_mMergedPairs_mNHitMc = fChain->GetBranch("mMergedPairs.mNHitMc");
   b_mMergedPairs_mGeantId = fChain->GetBranch("mMergedPairs.mGeantId");
   b_mMergedPairs_mChargeMc = fChain->GetBranch("mMergedPairs.mChargeMc");
   b_mMergedPairs_mStopR = fChain->GetBranch("mMergedPairs.mStopR");
   b_mMergedPairs_mNAssocGl = fChain->GetBranch("mMergedPairs.mNAssocGl");
   b_mMergedPairs_mNAssocPr = fChain->GetBranch("mMergedPairs.mNAssocPr");
   b_mMergedPairs_fUniqueID = fChain->GetBranch("mMergedPairs.fUniqueID");
   b_mMergedPairs_fBits = fChain->GetBranch("mMergedPairs.fBits");
   b_mMergedPairs_mPtPr = fChain->GetBranch("mMergedPairs.mPtPr");
   b_mMergedPairs_mPzPr = fChain->GetBranch("mMergedPairs.mPzPr");
   b_mMergedPairs_mEtaPr = fChain->GetBranch("mMergedPairs.mEtaPr");
   b_mMergedPairs_mPhiPr = fChain->GetBranch("mMergedPairs.mPhiPr");
   b_mMergedPairs_mDcaPr = fChain->GetBranch("mMergedPairs.mDcaPr");
   b_mMergedPairs_mDcaXYPr = fChain->GetBranch("mMergedPairs.mDcaXYPr");
   b_mMergedPairs_mDcaZPr = fChain->GetBranch("mMergedPairs.mDcaZPr");
   b_mMergedPairs_mChi2Pr = fChain->GetBranch("mMergedPairs.mChi2Pr");
   b_mMergedPairs_mFlag = fChain->GetBranch("mMergedPairs.mFlag");
   b_mMergedPairs_mDedx = fChain->GetBranch("mMergedPairs.mDedx");
   b_mMergedPairs_mPtGl = fChain->GetBranch("mMergedPairs.mPtGl");
   b_mMergedPairs_mPzGl = fChain->GetBranch("mMergedPairs.mPzGl");
   b_mMergedPairs_mEtaGl = fChain->GetBranch("mMergedPairs.mEtaGl");
   b_mMergedPairs_mPhiGl = fChain->GetBranch("mMergedPairs.mPhiGl");
   b_mMergedPairs_mDcaGl = fChain->GetBranch("mMergedPairs.mDcaGl");
   b_mMergedPairs_mDcaXYGl = fChain->GetBranch("mMergedPairs.mDcaXYGl");
   b_mMergedPairs_mDcaZGl = fChain->GetBranch("mMergedPairs.mDcaZGl");
   b_mMergedPairs_mPidPion = fChain->GetBranch("mMergedPairs.mPidPion");
   b_mMergedPairs_mPidProton = fChain->GetBranch("mMergedPairs.mPidProton");
   b_mMergedPairs_mPidKaon = fChain->GetBranch("mMergedPairs.mPidKaon");
   b_mMergedPairs_mPidElectron = fChain->GetBranch("mMergedPairs.mPidElectron");
   b_mMergedPairs_mFirstZ = fChain->GetBranch("mMergedPairs.mFirstZ");
   b_mMergedPairs_mLastZ = fChain->GetBranch("mMergedPairs.mLastZ");
   b_mMergedPairs_mFirstPadrow = fChain->GetBranch("mMergedPairs.mFirstPadrow");
   b_mMergedPairs_mLastPadrow = fChain->GetBranch("mMergedPairs.mLastPadrow");
   b_mMergedPairs_mFirstFitPadrow = fChain->GetBranch("mMergedPairs.mFirstFitPadrow");
   b_mMergedPairs_mLastFitPadrow = fChain->GetBranch("mMergedPairs.mLastFitPadrow");
   b_mMergedPairs_mFirstSector = fChain->GetBranch("mMergedPairs.mFirstSector");
   b_mMergedPairs_mLastSector = fChain->GetBranch("mMergedPairs.mLastSector");
   b_mMergedPairs_mFitPts = fChain->GetBranch("mMergedPairs.mFitPts");
   b_mMergedPairs_mAllPts = fChain->GetBranch("mMergedPairs.mAllPts");
   b_mMergedPairs_mCharge = fChain->GetBranch("mMergedPairs.mCharge");
   b_mMergedPairs_mNAssocMc = fChain->GetBranch("mMergedPairs.mNAssocMc");
   b_mMergedPairs_mNPossible = fChain->GetBranch("mMergedPairs.mNPossible");
   b_mSplitPairs_ = fChain->GetBranch("mSplitPairs_");
   b_mSplitPairs_mNCommonHit = fChain->GetBranch("mSplitPairs.mNCommonHit");
   b_mSplitPairs_mIsBestContam = fChain->GetBranch("mSplitPairs.mIsBestContam");
   b_mSplitPairs_mPtMc = fChain->GetBranch("mSplitPairs.mPtMc");
   b_mSplitPairs_mPzMc = fChain->GetBranch("mSplitPairs.mPzMc");
   b_mSplitPairs_mEtaMc = fChain->GetBranch("mSplitPairs.mEtaMc");
   b_mSplitPairs_mPhiMc = fChain->GetBranch("mSplitPairs.mPhiMc");
   b_mSplitPairs_mNHitMc = fChain->GetBranch("mSplitPairs.mNHitMc");
   b_mSplitPairs_mGeantId = fChain->GetBranch("mSplitPairs.mGeantId");
   b_mSplitPairs_mChargeMc = fChain->GetBranch("mSplitPairs.mChargeMc");
   b_mSplitPairs_mStopR = fChain->GetBranch("mSplitPairs.mStopR");
   b_mSplitPairs_mNAssocGl = fChain->GetBranch("mSplitPairs.mNAssocGl");
   b_mSplitPairs_mNAssocPr = fChain->GetBranch("mSplitPairs.mNAssocPr");
   b_mSplitPairs_fUniqueID = fChain->GetBranch("mSplitPairs.fUniqueID");
   b_mSplitPairs_fBits = fChain->GetBranch("mSplitPairs.fBits");
   b_mSplitPairs_mPtPr = fChain->GetBranch("mSplitPairs.mPtPr");
   b_mSplitPairs_mPzPr = fChain->GetBranch("mSplitPairs.mPzPr");
   b_mSplitPairs_mEtaPr = fChain->GetBranch("mSplitPairs.mEtaPr");
   b_mSplitPairs_mPhiPr = fChain->GetBranch("mSplitPairs.mPhiPr");
   b_mSplitPairs_mDcaPr = fChain->GetBranch("mSplitPairs.mDcaPr");
   b_mSplitPairs_mDcaXYPr = fChain->GetBranch("mSplitPairs.mDcaXYPr");
   b_mSplitPairs_mDcaZPr = fChain->GetBranch("mSplitPairs.mDcaZPr");
   b_mSplitPairs_mChi2Pr = fChain->GetBranch("mSplitPairs.mChi2Pr");
   b_mSplitPairs_mFlag = fChain->GetBranch("mSplitPairs.mFlag");
   b_mSplitPairs_mDedx = fChain->GetBranch("mSplitPairs.mDedx");
   b_mSplitPairs_mPtGl = fChain->GetBranch("mSplitPairs.mPtGl");
   b_mSplitPairs_mPzGl = fChain->GetBranch("mSplitPairs.mPzGl");
   b_mSplitPairs_mEtaGl = fChain->GetBranch("mSplitPairs.mEtaGl");
   b_mSplitPairs_mPhiGl = fChain->GetBranch("mSplitPairs.mPhiGl");
   b_mSplitPairs_mDcaGl = fChain->GetBranch("mSplitPairs.mDcaGl");
   b_mSplitPairs_mDcaXYGl = fChain->GetBranch("mSplitPairs.mDcaXYGl");
   b_mSplitPairs_mDcaZGl = fChain->GetBranch("mSplitPairs.mDcaZGl");
   b_mSplitPairs_mPidPion = fChain->GetBranch("mSplitPairs.mPidPion");
   b_mSplitPairs_mPidProton = fChain->GetBranch("mSplitPairs.mPidProton");
   b_mSplitPairs_mPidKaon = fChain->GetBranch("mSplitPairs.mPidKaon");
   b_mSplitPairs_mPidElectron = fChain->GetBranch("mSplitPairs.mPidElectron");
   b_mSplitPairs_mFirstZ = fChain->GetBranch("mSplitPairs.mFirstZ");
   b_mSplitPairs_mLastZ = fChain->GetBranch("mSplitPairs.mLastZ");
   b_mSplitPairs_mFirstPadrow = fChain->GetBranch("mSplitPairs.mFirstPadrow");
   b_mSplitPairs_mLastPadrow = fChain->GetBranch("mSplitPairs.mLastPadrow");
   b_mSplitPairs_mFirstFitPadrow = fChain->GetBranch("mSplitPairs.mFirstFitPadrow");
   b_mSplitPairs_mLastFitPadrow = fChain->GetBranch("mSplitPairs.mLastFitPadrow");
   b_mSplitPairs_mFirstSector = fChain->GetBranch("mSplitPairs.mFirstSector");
   b_mSplitPairs_mLastSector = fChain->GetBranch("mSplitPairs.mLastSector");
   b_mSplitPairs_mFitPts = fChain->GetBranch("mSplitPairs.mFitPts");
   b_mSplitPairs_mAllPts = fChain->GetBranch("mSplitPairs.mAllPts");
   b_mSplitPairs_mCharge = fChain->GetBranch("mSplitPairs.mCharge");
   b_mSplitPairs_mNAssocMc = fChain->GetBranch("mSplitPairs.mNAssocMc");
   b_mSplitPairs_mNPossible = fChain->GetBranch("mSplitPairs.mNPossible");
   b_mGhostPairs_ = fChain->GetBranch("mGhostPairs_");
   b_mGhostPairs_mNCommonHit = fChain->GetBranch("mGhostPairs.mNCommonHit");
   b_mGhostPairs_mIsBestContam = fChain->GetBranch("mGhostPairs.mIsBestContam");
   b_mGhostPairs_mPtMc = fChain->GetBranch("mGhostPairs.mPtMc");
   b_mGhostPairs_mPzMc = fChain->GetBranch("mGhostPairs.mPzMc");
   b_mGhostPairs_mEtaMc = fChain->GetBranch("mGhostPairs.mEtaMc");
   b_mGhostPairs_mPhiMc = fChain->GetBranch("mGhostPairs.mPhiMc");
   b_mGhostPairs_mNHitMc = fChain->GetBranch("mGhostPairs.mNHitMc");
   b_mGhostPairs_mGeantId = fChain->GetBranch("mGhostPairs.mGeantId");
   b_mGhostPairs_mChargeMc = fChain->GetBranch("mGhostPairs.mChargeMc");
   b_mGhostPairs_mStopR = fChain->GetBranch("mGhostPairs.mStopR");
   b_mGhostPairs_mNAssocGl = fChain->GetBranch("mGhostPairs.mNAssocGl");
   b_mGhostPairs_mNAssocPr = fChain->GetBranch("mGhostPairs.mNAssocPr");
   b_mGhostPairs_fUniqueID = fChain->GetBranch("mGhostPairs.fUniqueID");
   b_mGhostPairs_fBits = fChain->GetBranch("mGhostPairs.fBits");
   b_mGhostPairs_mPtPr = fChain->GetBranch("mGhostPairs.mPtPr");
   b_mGhostPairs_mPzPr = fChain->GetBranch("mGhostPairs.mPzPr");
   b_mGhostPairs_mEtaPr = fChain->GetBranch("mGhostPairs.mEtaPr");
   b_mGhostPairs_mPhiPr = fChain->GetBranch("mGhostPairs.mPhiPr");
   b_mGhostPairs_mDcaPr = fChain->GetBranch("mGhostPairs.mDcaPr");
   b_mGhostPairs_mDcaXYPr = fChain->GetBranch("mGhostPairs.mDcaXYPr");
   b_mGhostPairs_mDcaZPr = fChain->GetBranch("mGhostPairs.mDcaZPr");
   b_mGhostPairs_mChi2Pr = fChain->GetBranch("mGhostPairs.mChi2Pr");
   b_mGhostPairs_mFlag = fChain->GetBranch("mGhostPairs.mFlag");
   b_mGhostPairs_mDedx = fChain->GetBranch("mGhostPairs.mDedx");
   b_mGhostPairs_mPtGl = fChain->GetBranch("mGhostPairs.mPtGl");
   b_mGhostPairs_mPzGl = fChain->GetBranch("mGhostPairs.mPzGl");
   b_mGhostPairs_mEtaGl = fChain->GetBranch("mGhostPairs.mEtaGl");
   b_mGhostPairs_mPhiGl = fChain->GetBranch("mGhostPairs.mPhiGl");
   b_mGhostPairs_mDcaGl = fChain->GetBranch("mGhostPairs.mDcaGl");
   b_mGhostPairs_mDcaXYGl = fChain->GetBranch("mGhostPairs.mDcaXYGl");
   b_mGhostPairs_mDcaZGl = fChain->GetBranch("mGhostPairs.mDcaZGl");
   b_mGhostPairs_mPidPion = fChain->GetBranch("mGhostPairs.mPidPion");
   b_mGhostPairs_mPidProton = fChain->GetBranch("mGhostPairs.mPidProton");
   b_mGhostPairs_mPidKaon = fChain->GetBranch("mGhostPairs.mPidKaon");
   b_mGhostPairs_mPidElectron = fChain->GetBranch("mGhostPairs.mPidElectron");
   b_mGhostPairs_mFirstZ = fChain->GetBranch("mGhostPairs.mFirstZ");
   b_mGhostPairs_mLastZ = fChain->GetBranch("mGhostPairs.mLastZ");
   b_mGhostPairs_mFirstPadrow = fChain->GetBranch("mGhostPairs.mFirstPadrow");
   b_mGhostPairs_mLastPadrow = fChain->GetBranch("mGhostPairs.mLastPadrow");
   b_mGhostPairs_mFirstFitPadrow = fChain->GetBranch("mGhostPairs.mFirstFitPadrow");
   b_mGhostPairs_mLastFitPadrow = fChain->GetBranch("mGhostPairs.mLastFitPadrow");
   b_mGhostPairs_mFirstSector = fChain->GetBranch("mGhostPairs.mFirstSector");
   b_mGhostPairs_mLastSector = fChain->GetBranch("mGhostPairs.mLastSector");
   b_mGhostPairs_mFitPts = fChain->GetBranch("mGhostPairs.mFitPts");
   b_mGhostPairs_mAllPts = fChain->GetBranch("mGhostPairs.mAllPts");
   b_mGhostPairs_mCharge = fChain->GetBranch("mGhostPairs.mCharge");
   b_mGhostPairs_mNAssocMc = fChain->GetBranch("mGhostPairs.mNAssocMc");
   b_mGhostPairs_mNPossible = fChain->GetBranch("mGhostPairs.mNPossible");
   b_mContamPairs_ = fChain->GetBranch("mContamPairs_");
   b_mContamPairs_mParentGeantId = fChain->GetBranch("mContamPairs.mParentGeantId");
   b_mContamPairs_mGeantProcess = fChain->GetBranch("mContamPairs.mGeantProcess");
   b_mContamPairs_mPtMcParent = fChain->GetBranch("mContamPairs.mPtMcParent");
   b_mContamPairs_mEtaMcParent = fChain->GetBranch("mContamPairs.mEtaMcParent");
   b_mContamPairs_mParentParentGeantId = fChain->GetBranch("mContamPairs.mParentParentGeantId");
   b_mContamPairs_mPtMcParentParent = fChain->GetBranch("mContamPairs.mPtMcParentParent");
   b_mContamPairs_mStartX = fChain->GetBranch("mContamPairs.mStartX");
   b_mContamPairs_mStartY = fChain->GetBranch("mContamPairs.mStartY");
   b_mContamPairs_mStartZ = fChain->GetBranch("mContamPairs.mStartZ");
   b_mContamPairs_mNCommonHit = fChain->GetBranch("mContamPairs.mNCommonHit");
   b_mContamPairs_mIsBestContam = fChain->GetBranch("mContamPairs.mIsBestContam");
   b_mContamPairs_mPtMc = fChain->GetBranch("mContamPairs.mPtMc");
   b_mContamPairs_mPzMc = fChain->GetBranch("mContamPairs.mPzMc");
   b_mContamPairs_mEtaMc = fChain->GetBranch("mContamPairs.mEtaMc");
   b_mContamPairs_mPhiMc = fChain->GetBranch("mContamPairs.mPhiMc");
   b_mContamPairs_mNHitMc = fChain->GetBranch("mContamPairs.mNHitMc");
   b_mContamPairs_mGeantId = fChain->GetBranch("mContamPairs.mGeantId");
   b_mContamPairs_mChargeMc = fChain->GetBranch("mContamPairs.mChargeMc");
   b_mContamPairs_mStopR = fChain->GetBranch("mContamPairs.mStopR");
   b_mContamPairs_mNAssocGl = fChain->GetBranch("mContamPairs.mNAssocGl");
   b_mContamPairs_mNAssocPr = fChain->GetBranch("mContamPairs.mNAssocPr");
   b_mContamPairs_fUniqueID = fChain->GetBranch("mContamPairs.fUniqueID");
   b_mContamPairs_fBits = fChain->GetBranch("mContamPairs.fBits");
   b_mContamPairs_mPtPr = fChain->GetBranch("mContamPairs.mPtPr");
   b_mContamPairs_mPzPr = fChain->GetBranch("mContamPairs.mPzPr");
   b_mContamPairs_mEtaPr = fChain->GetBranch("mContamPairs.mEtaPr");
   b_mContamPairs_mPhiPr = fChain->GetBranch("mContamPairs.mPhiPr");
   b_mContamPairs_mDcaPr = fChain->GetBranch("mContamPairs.mDcaPr");
   b_mContamPairs_mDcaXYPr = fChain->GetBranch("mContamPairs.mDcaXYPr");
   b_mContamPairs_mDcaZPr = fChain->GetBranch("mContamPairs.mDcaZPr");
   b_mContamPairs_mChi2Pr = fChain->GetBranch("mContamPairs.mChi2Pr");
   b_mContamPairs_mFlag = fChain->GetBranch("mContamPairs.mFlag");
   b_mContamPairs_mDedx = fChain->GetBranch("mContamPairs.mDedx");
   b_mContamPairs_mPtGl = fChain->GetBranch("mContamPairs.mPtGl");
   b_mContamPairs_mPzGl = fChain->GetBranch("mContamPairs.mPzGl");
   b_mContamPairs_mEtaGl = fChain->GetBranch("mContamPairs.mEtaGl");
   b_mContamPairs_mPhiGl = fChain->GetBranch("mContamPairs.mPhiGl");
   b_mContamPairs_mDcaGl = fChain->GetBranch("mContamPairs.mDcaGl");
   b_mContamPairs_mDcaXYGl = fChain->GetBranch("mContamPairs.mDcaXYGl");
   b_mContamPairs_mDcaZGl = fChain->GetBranch("mContamPairs.mDcaZGl");
   b_mContamPairs_mPidPion = fChain->GetBranch("mContamPairs.mPidPion");
   b_mContamPairs_mPidProton = fChain->GetBranch("mContamPairs.mPidProton");
   b_mContamPairs_mPidKaon = fChain->GetBranch("mContamPairs.mPidKaon");
   b_mContamPairs_mPidElectron = fChain->GetBranch("mContamPairs.mPidElectron");
   b_mContamPairs_mFirstZ = fChain->GetBranch("mContamPairs.mFirstZ");
   b_mContamPairs_mLastZ = fChain->GetBranch("mContamPairs.mLastZ");
   b_mContamPairs_mFirstPadrow = fChain->GetBranch("mContamPairs.mFirstPadrow");
   b_mContamPairs_mLastPadrow = fChain->GetBranch("mContamPairs.mLastPadrow");
   b_mContamPairs_mFirstFitPadrow = fChain->GetBranch("mContamPairs.mFirstFitPadrow");
   b_mContamPairs_mLastFitPadrow = fChain->GetBranch("mContamPairs.mLastFitPadrow");
   b_mContamPairs_mFirstSector = fChain->GetBranch("mContamPairs.mFirstSector");
   b_mContamPairs_mLastSector = fChain->GetBranch("mContamPairs.mLastSector");
   b_mContamPairs_mFitPts = fChain->GetBranch("mContamPairs.mFitPts");
   b_mContamPairs_mAllPts = fChain->GetBranch("mContamPairs.mAllPts");
   b_mContamPairs_mCharge = fChain->GetBranch("mContamPairs.mCharge");
   b_mContamPairs_mNAssocMc = fChain->GetBranch("mContamPairs.mNAssocMc");
   b_mContamPairs_mNPossible = fChain->GetBranch("mContamPairs.mNPossible");
   b_fUniqueID = fChain->GetBranch("fUniqueID");
   b_fBits = fChain->GetBranch("fBits");
   return kTRUE;
}

void standardPlots::Show(Int_t entry)
{
// Print contents of entry.
// If entry is not specified, print current entry
   if (!fChain) return;
   fChain->Show(entry);
}

Int_t standardPlots::Cut(Int_t entry)
{
// This function may be called from Loop.
// returns  1 if entry is accepted.
// returns -1 otherwise.
   if(mMatchedPairs_>multCut[0] && mMatchedPairs_<multCut[1]
      && mVertexZ>zCut[0] && mVertexZ<zCut[1]) return 1;

   return 0;
}

int standardPlots::trackCut(int entry, int track)
{
// This function may be called from Loop.
// returns  1 if entry is accepted.
// returns 0 otherwise.
  if(   mMatchedPairs_mPtPr[track]       > ptCut[0] 
     && mMatchedPairs_mPtPr[track]       < ptCut[1]
     && mMatchedPairs_mEtaPr[track]      > etaCut[0]
     && mMatchedPairs_mEtaPr[track]      < etaCut[1]
     && mMatchedPairs_mNCommonHit[track] > nHitCut[0] 
     && mMatchedPairs_mNCommonHit[track] < nHitCut[1]
     && mMatchedPairs_mDcaGl[track]      > dca[0] 
     && mMatchedPairs_mDcaGl[track]      < dca[1]) return 1;

   return 0;
}

int standardPlots::mcTrackCut(int entry, int track)
{
// This function may be called from Loop.
// returns  1 if entry is accepted.
// returns 0 otherwise.
  if(  mMcTracks_mPtMc[track]        > ptCut[0]
     &&mMcTracks_mPtMc[track]        < ptCut[1]
     &&(mMcTracks_mEtaMc[track]      > etaCut[0]
     &&mMcTracks_mEtaMc[track]       < etaCut[1]) 
     &&mMcTracks_mNHitMc[track]      > nHitCut[0] 
     && mMcTracks_mNHitMc[track]     < nHitCut[1] 
     &&mMcTracks_mChargeMc[track]!=0)
     return 1;

   return 0;
}

void standardPlots::showCuts()
{
  cout <<"The Event cuts currently defined are:\t\tlow\thigh"<<endl
       <<"\tVertex Z :\t"
       <<"\t\t\t"<<zCut[0]<<"\t"<<zCut[1]<<endl
       <<"\tMultiplicity :\t"
       <<"\t\t\t"<<multCut[0]<<"\t"<<multCut[1]<<endl<<endl
       <<"The Track Cuts currently defined are:\t\tlow\thigh"<<endl    
       <<"\tTransverse Momentum :\t"
       <<"\t\t"<<ptCut[0]<<"\t"<<ptCut[1]<<endl
       <<"\tEta :\t"
       <<"\t\t\t\t"<<etaCut[0]<<"\t"<<etaCut[1]<<endl
       <<"\tTpc Hits :\t"
       <<"\t\t\t"<<nHitCut[0]<<"\t"<<nHitCut[1]<<endl;
}
#endif // #ifdef standardPlots_cxx

