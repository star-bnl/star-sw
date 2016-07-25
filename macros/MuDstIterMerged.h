#ifndef __MuDstIter_H__
#include "Rtypes.h"
#define __MuDstIter_H__
#if defined(__MuCov__) || defined(__MuDst__)
  #define __PrimaryTracks__
  #define __GlobalTracks__
#endif
#ifdef __MuCov__
  #define __CovGlobTrack__
  #define __CovPrimTrack__
#endif
#define __dEdx__P(track)						\
  const UChar_t*&    track ## Tracks_mProbPidTraits_mNDF        = iter(# track "Tracks.mProbPidTraits.mNDF"); \
  const Float_t*&    track ## Tracks_mProbPidTraits_mdEdxFit    = iter(# track "Tracks.mProbPidTraits.mdEdxFit"); \
  const Float_t*&    track ## Tracks_mProbPidTraits_mdEdxErrorFit = iter(# track "Tracks.mProbPidTraits.mdEdxErrorFit"); \
  const Float_t*&    track ## Tracks_mProbPidTraits_mdEdxTruncated = iter(# track "Tracks.mProbPidTraits.mdEdxTruncated"); \
  const Float_t*&    track ## Tracks_mProbPidTraits_mdEdxErrorTruncated = iter(# track "Tracks.mProbPidTraits.mdEdxErrorTruncated"); \
  const Float_t*&    track ## Tracks_mProbPidTraits_mdEdxTrackLength = iter(# track "Tracks.mProbPidTraits.mdEdxTrackLength"); \
  const Float_t*&    track ## Tracks_mProbPidTraits_mProbabilities = iter(# track "Tracks.mProbPidTraits.mProbabilities[9]");

#define __BTof__P(track)						\
  const UChar_t*&    track ## Tracks_mBTofPidTraits_mMatchFlag  = iter(# track "Tracks.mBTofPidTraits.mMatchFlag");	\
  const Float_t*&    track ## Tracks_mBTofPidTraits_mYLocal     = iter(# track "Tracks.mBTofPidTraits.mYLocal"); \
  const Float_t*&    track ## Tracks_mBTofPidTraits_mZLocal     = iter(# track "Tracks.mBTofPidTraits.mZLocal"); \
  const Float_t*&    track ## Tracks_mBTofPidTraits_mThetaLocal = iter(# track "Tracks.mBTofPidTraits.mThetaLocal"); \
  const Float_t*&    track ## Tracks_mBTofPidTraits_mPosition_mX1 = iter(# track "Tracks.mBTofPidTraits.mPosition.mX1"); \
  const Float_t*&    track ## Tracks_mBTofPidTraits_mPosition_mX2 = iter(# track "Tracks.mBTofPidTraits.mPosition.mX2"); \
  const Float_t*&    track ## Tracks_mBTofPidTraits_mPosition_mX3 = iter(# track "Tracks.mBTofPidTraits.mPosition.mX3"); \
  const Float_t*&    track ## Tracks_mBTofPidTraits_mTimeOfFlight = iter(# track "Tracks.mBTofPidTraits.mTimeOfFlight"); \
  const Float_t*&    track ## Tracks_mBTofPidTraits_mPathLength = iter(# track "Tracks.mBTofPidTraits.mPathLength"); \
  const Float_t*&    track ## Tracks_mBTofPidTraits_mBeta       = iter(# track "Tracks.mBTofPidTraits.mBeta"); \
  const Float_t*&    track ## Tracks_mBTofPidTraits_mSigmaElectron = iter(# track "Tracks.mBTofPidTraits.mSigmaElectron"); \
  const Float_t*&    track ## Tracks_mBTofPidTraits_mSigmaPion  = iter(# track "Tracks.mBTofPidTraits.mSigmaPion");	\
  const Float_t*&    track ## Tracks_mBTofPidTraits_mSigmaKaon  = iter(# track "Tracks.mBTofPidTraits.mSigmaKaon");	\
  const Float_t*&    track ## Tracks_mBTofPidTraits_mSigmaProton = iter(# track "Tracks.mBTofPidTraits.mSigmaProton"); \
  const Float_t*&    track ## Tracks_mBTofPidTraits_mProbElectron = iter(# track "Tracks.mBTofPidTraits.mProbElectron"); \
  const Float_t*&    track ## Tracks_mBTofPidTraits_mProbPion   = iter(# track "Tracks.mBTofPidTraits.mProbPion"); \
  const Float_t*&    track ## Tracks_mBTofPidTraits_mProbKaon   = iter(# track "Tracks.mBTofPidTraits.mProbKaon"); \
  const Float_t*&    track ## Tracks_mBTofPidTraits_mProbProton = iter(# track "Tracks.mBTofPidTraits.mProbProton"); \

#ifdef __RunInfo__
const Int_t&       NoMuEvent                                = iter("MuEvent");
const Int_t*&      MuEvent_mRunInfo_mRunId                  = iter("MuEvent.mRunInfo.mRunId");
const UInt_t*&     MuEvent_mRunInfo_mProductionTime         = iter("MuEvent.mRunInfo.mProductionTime");
const TString*&    MuEvent_mRunInfo_mProductionVersion      = iter("MuEvent.mRunInfo.mProductionVersion");
const Float_t*&    MuEvent_mRunInfo_mCenterOfMassEnergy     = iter("MuEvent.mRunInfo.mCenterOfMassEnergy");
const Int_t*&      MuEvent_mRunInfo_mBeamMassNumber         = iter("MuEvent.mRunInfo.mBeamMassNumber[2]");
const Double_t*&   MuEvent_mRunInfo_mMagneticFieldZ         = iter("MuEvent.mRunInfo.mMagneticFieldZ");
const Float_t*&    MuEvent_mRunInfo_mTpcDriftVelocity       = iter("MuEvent.mRunInfo.mTpcDriftVelocity[2]");
const Float_t*&    MuEvent_mRunInfo_mSvtDriftVelocityScaler = iter("MuEvent.mRunInfo.mSvtDriftVelocityScaler");
const Double_t*&   MuEvent_mRunInfo_mZdcEastRate            = iter("MuEvent.mRunInfo.mZdcEastRate");
const Double_t*&   MuEvent_mRunInfo_mZdcWestRate            = iter("MuEvent.mRunInfo.mZdcWestRate");
const Double_t*&   MuEvent_mRunInfo_mZdcCoincidenceRate     = iter("MuEvent.mRunInfo.mZdcCoincidenceRate");
const Double_t*&   MuEvent_mRunInfo_mBackgroundRate         = iter("MuEvent.mRunInfo.mBackgroundRate");
const Double_t*&   MuEvent_mRunInfo_mL0RateToRich           = iter("MuEvent.mRunInfo.mL0RateToRich");
const Double_t*&   MuEvent_mRunInfo_mBbcCoincidenceRate     = iter("MuEvent.mRunInfo.mBbcCoincidenceRate");
const Float_t*&    MuEvent_mRunInfo_mBeamEnergy             = iter("MuEvent.mRunInfo.mBeamEnergy[2]");
const Float_t*&    MuEvent_mRunInfo_mInitialBeamIntensity   = iter("MuEvent.mRunInfo.mInitialBeamIntensity[2]");
const Float_t*&    MuEvent_mRunInfo_mBeamLifeTime           = iter("MuEvent.mRunInfo.mBeamLifeTime[2]");
const Float_t*&    MuEvent_mRunInfo_mBeamFillNumber         = iter("MuEvent.mRunInfo.mBeamFillNumber[2]");
const Double_t*&   MuEvent_mRunInfo_mBbcEastRate            = iter("MuEvent.mRunInfo.mBbcEastRate");
const Double_t*&   MuEvent_mRunInfo_mBbcWestRate            = iter("MuEvent.mRunInfo.mBbcWestRate");
const Double_t*&   MuEvent_mRunInfo_mBbcBlueBackgroundRate  = iter("MuEvent.mRunInfo.mBbcBlueBackgroundRate");
const Double_t*&   MuEvent_mRunInfo_mBbcYellowBackgroundRate = iter("MuEvent.mRunInfo.mBbcYellowBackgroundRate");
const Int_t*&      MuEvent_mRunInfo_mSpaceChargeCorrectionMode = iter("MuEvent.mRunInfo.mSpaceChargeCorrectionMode");
const Float_t*&    MuEvent_mRunInfo_mSpaceCharge            = iter("MuEvent.mRunInfo.mSpaceCharge");
#endif /* __RunInfo__ */
#ifdef    __EventInfo__
const TString*&    MuEvent_mEventInfo_mType                 = iter("MuEvent.mEventInfo.mType");
const Int_t*&      MuEvent_mEventInfo_mRunId                = iter("MuEvent.mEventInfo.mRunId");
const Int_t*&      MuEvent_mEventInfo_mId                   = iter("MuEvent.mEventInfo.mId");
const Int_t*&      MuEvent_mEventInfo_mTime                 = iter("MuEvent.mEventInfo.mTime");
const UInt_t*&     MuEvent_mEventInfo_mTriggerMask          = iter("MuEvent.mEventInfo.mTriggerMask");
const UInt_t*&     MuEvent_mEventInfo_mBunchCrossingNumber  = iter("MuEvent.mEventInfo.mBunchCrossingNumber[2]");
const UInt_t*&     MuEvent_mEventInfo_mEventSize            = iter("MuEvent.mEventInfo.mEventSize");
#endif /* __EventInfo__ */
#ifdef __EventSummary__
const Int_t*&      MuEvent_mEventSummary_mNumberOfTracks    = iter("MuEvent.mEventSummary.mNumberOfTracks");
const Int_t*&      MuEvent_mEventSummary_mNumberOfGoodTracks = iter("MuEvent.mEventSummary.mNumberOfGoodTracks");
const Int_t*&      MuEvent_mEventSummary_mNumberOfGoodPrimaryTracks = iter("MuEvent.mEventSummary.mNumberOfGoodPrimaryTracks");
const Int_t*&      MuEvent_mEventSummary_mNumberOfPositiveTracks = iter("MuEvent.mEventSummary.mNumberOfPositiveTracks");
const Int_t*&      MuEvent_mEventSummary_mNumberOfNegativeTracks = iter("MuEvent.mEventSummary.mNumberOfNegativeTracks");
const Int_t*&      MuEvent_mEventSummary_mNumberOfExoticTracks = iter("MuEvent.mEventSummary.mNumberOfExoticTracks");
const Int_t*&      MuEvent_mEventSummary_mNumberOfVertices  = iter("MuEvent.mEventSummary.mNumberOfVertices");
const TArrayL*&    MuEvent_mEventSummary_mNumberOfVertexTypes = iter("MuEvent.mEventSummary.mNumberOfVertexTypes");
const Int_t*&      MuEvent_mEventSummary_mNumberOfPileupVertices = iter("MuEvent.mEventSummary.mNumberOfPileupVertices");
const Float_t*&    MuEvent_mEventSummary_mMeanPt            = iter("MuEvent.mEventSummary.mMeanPt");
const Float_t*&    MuEvent_mEventSummary_mMeanPt2           = iter("MuEvent.mEventSummary.mMeanPt2");
const Float_t*&    MuEvent_mEventSummary_mMeanEta           = iter("MuEvent.mEventSummary.mMeanEta");
const Float_t*&    MuEvent_mEventSummary_mRmsEta            = iter("MuEvent.mEventSummary.mRmsEta");
const Float_t*&    MuEvent_mEventSummary_mPrimaryVertexPos_mX1 = iter("MuEvent.mEventSummary.mPrimaryVertexPos.mX1");
const Float_t*&    MuEvent_mEventSummary_mPrimaryVertexPos_mX2 = iter("MuEvent.mEventSummary.mPrimaryVertexPos.mX2");
const Float_t*&    MuEvent_mEventSummary_mPrimaryVertexPos_mX3 = iter("MuEvent.mEventSummary.mPrimaryVertexPos.mX3");
const UShort_t*&   MuEvent_mEventSummary_mVertexTypeArraySize = iter("MuEvent.mEventSummary.mVertexTypeArraySize");
const UShort_t*&   MuEvent_mEventSummary_mPhiBinsSize       = iter("MuEvent.mEventSummary.mPhiBinsSize");
const UShort_t*&   MuEvent_mEventSummary_mPtAndEtaBinsSize  = iter("MuEvent.mEventSummary.mPtAndEtaBinsSize");
const UShort_t*&   MuEvent_mEventSummary_mHistogramSize     = iter("MuEvent.mEventSummary.mHistogramSize");
const TArrayF*&    MuEvent_mEventSummary_mEtaBins           = iter("MuEvent.mEventSummary.mEtaBins");
const TArrayF*&    MuEvent_mEventSummary_mPtBins            = iter("MuEvent.mEventSummary.mPtBins");
const TArrayF*&    MuEvent_mEventSummary_mPhiBins           = iter("MuEvent.mEventSummary.mPhiBins");
const TArrayL*&    MuEvent_mEventSummary_mEtaOfTracksHisto  = iter("MuEvent.mEventSummary.mEtaOfTracksHisto");
const TArrayL*&    MuEvent_mEventSummary_mPtOfTracksHisto   = iter("MuEvent.mEventSummary.mPtOfTracksHisto");
const TArrayL*&    MuEvent_mEventSummary_mPhiOfTracksHisto  = iter("MuEvent.mEventSummary.mPhiOfTracksHisto");
const TArrayF*&    MuEvent_mEventSummary_mEneryVsEtaHisto   = iter("MuEvent.mEventSummary.mEneryVsEtaHisto");
const TArrayF*&    MuEvent_mEventSummary_mEnergyVsPhiHisto  = iter("MuEvent.mEventSummary.mEnergyVsPhiHisto");
const Double_t*&   MuEvent_mEventSummary_mMagneticFieldZ    = iter("MuEvent.mEventSummary.mMagneticFieldZ");
#endif /* __EventSummary__ */
#ifdef __TriggerDetectors__
#ifdef   __VpdTriggerDetector__
const UShort_t*&   MuEvent_mVpdTriggerDetector_mADC         = iter("MuEvent.mVpdTriggerDetector.mADC[2][16]");
const UShort_t*&   MuEvent_mVpdTriggerDetector_mTDC         = iter("MuEvent.mVpdTriggerDetector.mTDC[2][16]");
const UShort_t*&   MuEvent_mVpdTriggerDetector_mEarliestTDC = iter("MuEvent.mVpdTriggerDetector.mEarliestTDC[2]");
const UShort_t*&   MuEvent_mVpdTriggerDetector_mTimeDifference = iter("MuEvent.mVpdTriggerDetector.mTimeDifference");
const UShort_t*&   MuEvent_mVpdTriggerDetector_mYear        = iter("MuEvent.mVpdTriggerDetector.mYear");
#endif   /* __VpdTriggerDetector__ */
#ifdef   __MdtTriggerDetector__
const UShort_t*&   MuEvent_mMtdTriggerDetector_mADC         = iter("MuEvent.mMtdTriggerDetector.mADC[2][8]");
const UShort_t*&   MuEvent_mMtdTriggerDetector_mTDC         = iter("MuEvent.mMtdTriggerDetector.mTDC[2][8]");
#endif   /* __MdtTriggerDetector__ */
const Float_t*&    MuEvent_mCtbTriggerDetector_mMips        = iter("MuEvent.mCtbTriggerDetector.mMips[120][2][11]");
const Char_t*&     MuEvent_mCtbTriggerDetector_mTime        = iter("MuEvent.mCtbTriggerDetector.mTime[120][2][11]");
const Float_t*&    MuEvent_mCtbTriggerDetector_mAux         = iter("MuEvent.mCtbTriggerDetector.mAux[16][11]");
const Int_t*&      MuEvent_mCtbTriggerDetector_mNumberOfPreSamples = iter("MuEvent.mCtbTriggerDetector.mNumberOfPreSamples");
const Int_t*&      MuEvent_mCtbTriggerDetector_mNumberOfPostSamples = iter("MuEvent.mCtbTriggerDetector.mNumberOfPostSamples");
const Float_t*&    MuEvent_mZdcTriggerDetector_mAdc         = iter("MuEvent.mZdcTriggerDetector.mAdc[16]");
const Float_t*&    MuEvent_mZdcTriggerDetector_mTdc         = iter("MuEvent.mZdcTriggerDetector.mTdc[16]");
const Float_t*&    MuEvent_mZdcTriggerDetector_mSumAdc      = iter("MuEvent.mZdcTriggerDetector.mSumAdc[2]");
const Float_t*&    MuEvent_mZdcTriggerDetector_mSum         = iter("MuEvent.mZdcTriggerDetector.mSum");
const Float_t*&    MuEvent_mZdcTriggerDetector_mVertexZ     = iter("MuEvent.mZdcTriggerDetector.mVertexZ");
const Float_t*&    MuEvent_mZdcTriggerDetector_mZdcSmdEast  = iter("MuEvent.mZdcTriggerDetector.mZdcSmdEast[16]");
const Float_t*&    MuEvent_mZdcTriggerDetector_mZdcSmdWest  = iter("MuEvent.mZdcTriggerDetector.mZdcSmdWest[16]");
const UShort_t*&   MuEvent_mBbcTriggerDetector_mAdc         = iter("MuEvent.mBbcTriggerDetector.mAdc[48]");
const UShort_t*&   MuEvent_mBbcTriggerDetector_mTdc         = iter("MuEvent.mBbcTriggerDetector.mTdc[48]");
const UShort_t*&   MuEvent_mBbcTriggerDetector_mReg         = iter("MuEvent.mBbcTriggerDetector.mReg[2]");
const UShort_t*&   MuEvent_mBbcTriggerDetector_mPed         = iter("MuEvent.mBbcTriggerDetector.mPed[128]");
const UShort_t*&   MuEvent_mBbcTriggerDetector_mScl         = iter("MuEvent.mBbcTriggerDetector.mScl[32]");
const UInt_t*&     MuEvent_mBbcTriggerDetector_mYear        = iter("MuEvent.mBbcTriggerDetector.mYear");
const UInt_t*&     MuEvent_mBbcTriggerDetector_mDSMVTX      = iter("MuEvent.mBbcTriggerDetector.mDSMVTX");
const Char_t*&     MuEvent_mEmcTriggerDetector_mHighTower   = iter("MuEvent.mEmcTriggerDetector.mHighTower[300]");
const Char_t*&     MuEvent_mEmcTriggerDetector_mPatch       = iter("MuEvent.mEmcTriggerDetector.mPatch[300]");
const Char_t*&     MuEvent_mEmcTriggerDetector_mEHighTower  = iter("MuEvent.mEmcTriggerDetector.mEHighTower[90]");
const Char_t*&     MuEvent_mEmcTriggerDetector_mEPatch      = iter("MuEvent.mEmcTriggerDetector.mEPatch[90]");
const UShort_t*&   MuEvent_mEmcTriggerDetector_mBemcLayer1  = iter("MuEvent.mEmcTriggerDetector.mBemcLayer1[48]");
const UShort_t*&   MuEvent_mEmcTriggerDetector_mEemcLayer1  = iter("MuEvent.mEmcTriggerDetector.mEemcLayer1[16]");
const UShort_t*&   MuEvent_mEmcTriggerDetector_mEmcLayer2   = iter("MuEvent.mEmcTriggerDetector.mEmcLayer2[8]");
const UChar_t*&    MuEvent_mFpdTriggerDetector_mEN          = iter("MuEvent.mFpdTriggerDetector.mEN[49]");
const UChar_t*&    MuEvent_mFpdTriggerDetector_mES          = iter("MuEvent.mFpdTriggerDetector.mES[49]");
const UChar_t*&    MuEvent_mFpdTriggerDetector_mET          = iter("MuEvent.mFpdTriggerDetector.mET[25]");
const UChar_t*&    MuEvent_mFpdTriggerDetector_mEB          = iter("MuEvent.mFpdTriggerDetector.mEB[25]");
const UChar_t*&    MuEvent_mFpdTriggerDetector_mEPN         = iter("MuEvent.mFpdTriggerDetector.mEPN[7]");
const UChar_t*&    MuEvent_mFpdTriggerDetector_mEPS         = iter("MuEvent.mFpdTriggerDetector.mEPS[7]");
const UChar_t*&    MuEvent_mFpdTriggerDetector_mWN          = iter("MuEvent.mFpdTriggerDetector.mWN[49]");
const UChar_t*&    MuEvent_mFpdTriggerDetector_mWS          = iter("MuEvent.mFpdTriggerDetector.mWS[49]");
const UChar_t*&    MuEvent_mFpdTriggerDetector_mWT          = iter("MuEvent.mFpdTriggerDetector.mWT[25]");
const UChar_t*&    MuEvent_mFpdTriggerDetector_mWB          = iter("MuEvent.mFpdTriggerDetector.mWB[25]");
const UChar_t*&    MuEvent_mFpdTriggerDetector_mWPN         = iter("MuEvent.mFpdTriggerDetector.mWPN[7]");
const UChar_t*&    MuEvent_mFpdTriggerDetector_mWPS         = iter("MuEvent.mFpdTriggerDetector.mWPS[7]");
const UShort_t*&   MuEvent_mFpdTriggerDetector_mLayer1      = iter("MuEvent.mFpdTriggerDetector.mLayer1[2][6][4]");
const UShort_t*&   MuEvent_mFpdTriggerDetector_mLayer2      = iter("MuEvent.mFpdTriggerDetector.mLayer2[2][6]");
const UInt_t*&     MuEvent_mFmsTriggerDetector_mNumQTdata   = iter("MuEvent.mFmsTriggerDetector.mNumQTdata");
const UInt_t*&     MuEvent_mFmsTriggerDetector_mQTdata      = iter("MuEvent.mFmsTriggerDetector.mQTdata[1600]");
const Char_t*&     MuEvent_mFmsTriggerDetector_mDSM         = iter("MuEvent.mFmsTriggerDetector.mDSM[256]");
const Char_t*&     MuEvent_mFmsTriggerDetector_mDSM01       = iter("MuEvent.mFmsTriggerDetector.mDSM01[112]");
const Char_t*&     MuEvent_mFmsTriggerDetector_mDSM02       = iter("MuEvent.mFmsTriggerDetector.mDSM02[16]");
const UShort_t*&   MuEvent_mFmsTriggerDetector_mDSM1        = iter("MuEvent.mFmsTriggerDetector.mDSM1[16]");
const UShort_t*&   MuEvent_mFmsTriggerDetector_mDSM2        = iter("MuEvent.mFmsTriggerDetector.mDSM2[8]");
const UShort_t*&   MuEvent_mFpdCollection_mAdc              = iter("MuEvent.mFpdCollection.mAdc[256]");
const UShort_t*&   MuEvent_mFpdCollection_mTdc              = iter("MuEvent.mFpdCollection.mTdc[8]");
const UShort_t*&   MuEvent_mFpdCollection_mReg              = iter("MuEvent.mFpdCollection.mReg[3]");
const UShort_t*&   MuEvent_mFpdCollection_mPed              = iter("MuEvent.mFpdCollection.mPed[256]");
const UInt_t*&     MuEvent_mFpdCollection_mScl              = iter("MuEvent.mFpdCollection.mScl[128]");
const UShort_t*&   MuEvent_mFpdCollection_mToken            = iter("MuEvent.mFpdCollection.mToken");
const UInt_t*&     MuEvent_mL0Trigger_mTriggerActionWord    = iter("MuEvent.mL0Trigger.mTriggerActionWord");
const UInt_t*&     MuEvent_mL0Trigger_mTriggerWord          = iter("MuEvent.mL0Trigger.mTriggerWord");
const Int_t*&      MuEvent_mL0Trigger_mCoarsePixelArray     = iter("MuEvent.mL0Trigger.mCoarsePixelArray[32]");
const Int_t*&      MuEvent_mL0Trigger_mMwcCtbMultiplicity   = iter("MuEvent.mL0Trigger.mMwcCtbMultiplicity");
const Int_t*&      MuEvent_mL0Trigger_mMwcCtbDipole         = iter("MuEvent.mL0Trigger.mMwcCtbDipole");
const Int_t*&      MuEvent_mL0Trigger_mMwcCtbTopology       = iter("MuEvent.mL0Trigger.mMwcCtbTopology");
const Int_t*&      MuEvent_mL0Trigger_mMwcCtbMoment         = iter("MuEvent.mL0Trigger.mMwcCtbMoment");
const UShort_t*&   MuEvent_mL0Trigger_mDsmInput             = iter("MuEvent.mL0Trigger.mDsmInput");
const UChar_t*&    MuEvent_mL0Trigger_mDetectorBusy         = iter("MuEvent.mL0Trigger.mDetectorBusy");
const UShort_t*&   MuEvent_mL0Trigger_mTriggerToken         = iter("MuEvent.mL0Trigger.mTriggerToken");
const UShort_t*&   MuEvent_mL0Trigger_mDsmAddress           = iter("MuEvent.mL0Trigger.mDsmAddress");
const UChar_t*&    MuEvent_mL0Trigger_mAddBits              = iter("MuEvent.mL0Trigger.mAddBits");
const UShort_t*&   MuEvent_mL0Trigger_mLastDsmArray         = iter("MuEvent.mL0Trigger.mLastDsmArray[8]");
const UShort_t*&   MuEvent_mL0Trigger_mBcDataArray          = iter("MuEvent.mL0Trigger.mBcDataArray[16]");
const Int_t*&      MuEvent_mL3EventSummary_mNumberOfProcessedEvents = iter("MuEvent.mL3EventSummary.mNumberOfProcessedEvents");
const Int_t*&      MuEvent_mL3EventSummary_mNumberReconstructedEvents = iter("MuEvent.mL3EventSummary.mNumberReconstructedEvents");
const Int_t*&      MuEvent_mL3EventSummary_mNumberOfTracks  = iter("MuEvent.mL3EventSummary.mNumberOfTracks");
const Int_t*&      MuEvent_mL3EventSummary_mNumberOfAlgorithms = iter("MuEvent.mL3EventSummary.mNumberOfAlgorithms");
const UChar_t*&    MuEvent_mL3EventSummary_mFlags           = iter("MuEvent.mL3EventSummary.mFlags");
const UInt_t*&     MuEvent_mL3EventSummary_mL0TriggerWord   = iter("MuEvent.mL3EventSummary.mL0TriggerWord");
const Int_t*&      MuEvent_mL3EventSummary_mUnbiasedPreScale = iter("MuEvent.mL3EventSummary.mUnbiasedPreScale");
const Float_t*&    MuEvent_mL3EventSummary_mPrimaryVertex_mX1 = iter("MuEvent.mL3EventSummary.mPrimaryVertex.mX1");
const Float_t*&    MuEvent_mL3EventSummary_mPrimaryVertex_mX2 = iter("MuEvent.mL3EventSummary.mPrimaryVertex.mX2");
const Float_t*&    MuEvent_mL3EventSummary_mPrimaryVertex_mX3 = iter("MuEvent.mL3EventSummary.mPrimaryVertex.mX3");
const ULong64_t*&  MuEvent_mTriggerIdCollection_mL1TriggerId_mMask = iter("MuEvent.mTriggerIdCollection.mL1TriggerId.mMask");
const UInt_t*&     MuEvent_mTriggerIdCollection_mL1TriggerId_mId = iter("MuEvent.mTriggerIdCollection.mL1TriggerId.mId[64]");
const UInt_t*&     MuEvent_mTriggerIdCollection_mL1TriggerId_mVersion = iter("MuEvent.mTriggerIdCollection.mL1TriggerId.mVersion[64]");
const UInt_t*&     MuEvent_mTriggerIdCollection_mL1TriggerId_mNameVersion = iter("MuEvent.mTriggerIdCollection.mL1TriggerId.mNameVersion[64]");
const UInt_t*&     MuEvent_mTriggerIdCollection_mL1TriggerId_mThresholdVersion = iter("MuEvent.mTriggerIdCollection.mL1TriggerId.mThresholdVersion[64]");
const UInt_t*&     MuEvent_mTriggerIdCollection_mL1TriggerId_mPrescaleVersion = iter("MuEvent.mTriggerIdCollection.mL1TriggerId.mPrescaleVersion[64]");
const ULong64_t*&  MuEvent_mTriggerIdCollection_mL2TriggerId_mMask = iter("MuEvent.mTriggerIdCollection.mL2TriggerId.mMask");
const UInt_t*&     MuEvent_mTriggerIdCollection_mL2TriggerId_mId = iter("MuEvent.mTriggerIdCollection.mL2TriggerId.mId[64]");
const UInt_t*&     MuEvent_mTriggerIdCollection_mL2TriggerId_mVersion = iter("MuEvent.mTriggerIdCollection.mL2TriggerId.mVersion[64]");
const UInt_t*&     MuEvent_mTriggerIdCollection_mL2TriggerId_mNameVersion = iter("MuEvent.mTriggerIdCollection.mL2TriggerId.mNameVersion[64]");
const UInt_t*&     MuEvent_mTriggerIdCollection_mL2TriggerId_mThresholdVersion = iter("MuEvent.mTriggerIdCollection.mL2TriggerId.mThresholdVersion[64]");
const UInt_t*&     MuEvent_mTriggerIdCollection_mL2TriggerId_mPrescaleVersion = iter("MuEvent.mTriggerIdCollection.mL2TriggerId.mPrescaleVersion[64]");
const ULong64_t*&  MuEvent_mTriggerIdCollection_mL3TriggerId_mMask = iter("MuEvent.mTriggerIdCollection.mL3TriggerId.mMask");
const UInt_t*&     MuEvent_mTriggerIdCollection_mL3TriggerId_mId = iter("MuEvent.mTriggerIdCollection.mL3TriggerId.mId[64]");
const UInt_t*&     MuEvent_mTriggerIdCollection_mL3TriggerId_mVersion = iter("MuEvent.mTriggerIdCollection.mL3TriggerId.mVersion[64]");
const UInt_t*&     MuEvent_mTriggerIdCollection_mL3TriggerId_mNameVersion = iter("MuEvent.mTriggerIdCollection.mL3TriggerId.mNameVersion[64]");
const UInt_t*&     MuEvent_mTriggerIdCollection_mL3TriggerId_mThresholdVersion = iter("MuEvent.mTriggerIdCollection.mL3TriggerId.mThresholdVersion[64]");
const UInt_t*&     MuEvent_mTriggerIdCollection_mL3TriggerId_mPrescaleVersion = iter("MuEvent.mTriggerIdCollection.mL3TriggerId.mPrescaleVersion[64]");
const ULong64_t*&  MuEvent_mTriggerIdCollection_mNTriggerId_mMask = iter("MuEvent.mTriggerIdCollection.mNTriggerId.mMask");
const UInt_t*&     MuEvent_mTriggerIdCollection_mNTriggerId_mId = iter("MuEvent.mTriggerIdCollection.mNTriggerId.mId[64]");
const UInt_t*&     MuEvent_mTriggerIdCollection_mNTriggerId_mVersion = iter("MuEvent.mTriggerIdCollection.mNTriggerId.mVersion[64]");
const UInt_t*&     MuEvent_mTriggerIdCollection_mNTriggerId_mNameVersion = iter("MuEvent.mTriggerIdCollection.mNTriggerId.mNameVersion[64]");
const UInt_t*&     MuEvent_mTriggerIdCollection_mNTriggerId_mThresholdVersion = iter("MuEvent.mTriggerIdCollection.mNTriggerId.mThresholdVersion[64]");
const UInt_t*&     MuEvent_mTriggerIdCollection_mNTriggerId_mPrescaleVersion = iter("MuEvent.mTriggerIdCollection.mNTriggerId.mPrescaleVersion[64]");
const ULong64_t*&  MuEvent_mTriggerIdCollection_mLETriggerId_mMask = iter("MuEvent.mTriggerIdCollection.mLETriggerId.mMask");
const UInt_t*&     MuEvent_mTriggerIdCollection_mLETriggerId_mId = iter("MuEvent.mTriggerIdCollection.mLETriggerId.mId[64]");
const UInt_t*&     MuEvent_mTriggerIdCollection_mLETriggerId_mVersion = iter("MuEvent.mTriggerIdCollection.mLETriggerId.mVersion[64]");
const UInt_t*&     MuEvent_mTriggerIdCollection_mLETriggerId_mNameVersion = iter("MuEvent.mTriggerIdCollection.mLETriggerId.mNameVersion[64]");
const UInt_t*&     MuEvent_mTriggerIdCollection_mLETriggerId_mThresholdVersion = iter("MuEvent.mTriggerIdCollection.mLETriggerId.mThresholdVersion[64]");
const UInt_t*&     MuEvent_mTriggerIdCollection_mLETriggerId_mPrescaleVersion = iter("MuEvent.mTriggerIdCollection.mLETriggerId.mPrescaleVersion[64]");
#endif /* __TriggerDetectors__ */
#ifdef __RefMult__
const UShort_t*&   MuEvent_mRefMultPos                      = iter("MuEvent.mRefMultPos");
const UShort_t*&   MuEvent_mRefMultNeg                      = iter("MuEvent.mRefMultNeg");
const UShort_t*&   MuEvent_mRefMultFtpcEast                 = iter("MuEvent.mRefMultFtpcEast");
const UShort_t*&   MuEvent_mRefMultFtpcWest                 = iter("MuEvent.mRefMultFtpcWest");
const Float_t*&    MuEvent_mReactionPlane                   = iter("MuEvent.mReactionPlane[2]");
const Float_t*&    MuEvent_mReactionPlanePtWgt              = iter("MuEvent.mReactionPlanePtWgt[2]");
#endif /*  __RefMult__ */
#ifdef __Junk__
const Float_t*&    MuEvent_mPrimaryVertexError_mX1          = iter("MuEvent.mPrimaryVertexError.mX1");
const Float_t*&    MuEvent_mPrimaryVertexError_mX2          = iter("MuEvent.mPrimaryVertexError.mX2");
const Float_t*&    MuEvent_mPrimaryVertexError_mX3          = iter("MuEvent.mPrimaryVertexError.mX3");
const TArrayI*&    MuEvent_mL2Result                        = iter("MuEvent.mL2Result");
const UInt_t*&     MuEvent_mVpdEast                         = iter("MuEvent.mVpdEast");
const UInt_t*&     MuEvent_mVpdWest                         = iter("MuEvent.mVpdWest");
const Float_t*&    MuEvent_mVpdTstart                       = iter("MuEvent.mVpdTstart");
const Float_t*&    MuEvent_mVpdTdiff                        = iter("MuEvent.mVpdTdiff");
#endif /* __Junk__ */
#ifdef  __PrimaryVertices__
const Int_t&       NoPrimaryVertices                        = iter("PrimaryVertices");
const Float_t*&    MuEvent_mVpdVz                           = iter("MuEvent.mVpdVz");
const Int_t&       PrimaryVertices_                         = iter("PrimaryVertices");
const Float_t*&    PrimaryVertices_mPosition_mX1            = iter("PrimaryVertices.mPosition.mX1");
const Float_t*&    PrimaryVertices_mPosition_mX2            = iter("PrimaryVertices.mPosition.mX2");
const Float_t*&    PrimaryVertices_mPosition_mX3            = iter("PrimaryVertices.mPosition.mX3");
  #ifdef __PrimaryVerticesExtention__
const Float_t*&    PrimaryVertices_mPosError_mX1            = iter("PrimaryVertices.mPosError.mX1");
const Float_t*&    PrimaryVertices_mPosError_mX2            = iter("PrimaryVertices.mPosError.mX2");
const Float_t*&    PrimaryVertices_mPosError_mX3            = iter("PrimaryVertices.mPosError.mX3");
const Int_t*&      PrimaryVertices_mVertexFinderId          = iter("PrimaryVertices.mVertexFinderId");
const Float_t*&    PrimaryVertices_mRanking                 = iter("PrimaryVertices.mRanking");
const UShort_t*&   PrimaryVertices_mNTracksUsed             = iter("PrimaryVertices.mNTracksUsed");
const UShort_t*&   PrimaryVertices_mNBTOFMatch              = iter("PrimaryVertices.mNBTOFMatch");
const UShort_t*&   PrimaryVertices_mNCTBMatch               = iter("PrimaryVertices.mNCTBMatch");
const UShort_t*&   PrimaryVertices_mNBEMCMatch              = iter("PrimaryVertices.mNBEMCMatch");
const UShort_t*&   PrimaryVertices_mNEEMCMatch              = iter("PrimaryVertices.mNEEMCMatch");
const UShort_t*&   PrimaryVertices_mNCrossCentralMembrane   = iter("PrimaryVertices.mNCrossCentralMembrane");
const Float_t*&    PrimaryVertices_mSumTrackPt              = iter("PrimaryVertices.mSumTrackPt");
const Float_t*&    PrimaryVertices_mMeanDip                 = iter("PrimaryVertices.mMeanDip");
const Float_t*&    PrimaryVertices_mChiSquared              = iter("PrimaryVertices.mChiSquared");
const UShort_t*&   PrimaryVertices_mRefMultNeg              = iter("PrimaryVertices.mRefMultNeg");
const UShort_t*&   PrimaryVertices_mRefMultPos              = iter("PrimaryVertices.mRefMultPos");
const UShort_t*&   PrimaryVertices_mRefMultFtpcWest         = iter("PrimaryVertices.mRefMultFtpcWest");
const UShort_t*&   PrimaryVertices_mRefMultFtpcEast         = iter("PrimaryVertices.mRefMultFtpcEast");
  #endif /* __PrimaryVerticesExtention__ */
const UShort_t*&   PrimaryVertices_mRefMultNeg              = iter("PrimaryVertices.mRefMultNeg");
const UShort_t*&   PrimaryVertices_mRefMultPos              = iter("PrimaryVertices.mRefMultPos");
#endif /* __PrimaryVertices__ */
#ifdef __PrimaryTracks__
const Int_t&       NoPrimaryTracks                           = iter("PrimaryTracks");
  #ifdef __PrimaryTracksFlags__
#ifdef __Y2015__
const Int_t*&   PrimaryVertices_mIdTruth                 = iter("PrimaryVertices.mIdTruth");
const Int_t*&   PrimaryVertices_mQuality                 = iter("PrimaryVertices.mQuality");
#else
const UShort_t*&   PrimaryVertices_mIdTruth                 = iter("PrimaryVertices.mIdTruth");
const UShort_t*&   PrimaryVertices_mQuality                 = iter("PrimaryVertices.mQuality");
#endif
const Int_t*&      PrimaryVertices_mIdParent                = iter("PrimaryVertices.mIdParent");
const Int_t&       PrimaryTracks_                           = iter("PrimaryTracks");
const Int_t*&    PrimaryTracks_mId                        = iter("PrimaryTracks.mId");
const Short_t*&    PrimaryTracks_mType                      = iter("PrimaryTracks.mType");
const Short_t*&    PrimaryTracks_mFlag                      = iter("PrimaryTracks.mFlag");
  #endif /*  __PrimaryTracksFlags__ */
  #ifdef __GlobalTracks__
const Int_t*&      PrimaryTracks_mIndex2Global              = iter("PrimaryTracks.mIndex2Global");
  #endif /* __GlobalTracks__ */
  #ifdef __RichSpectra__
const Int_t*&      PrimaryTracks_mIndex2RichSpectra         = iter("PrimaryTracks.mIndex2RichSpectra");
  #endif /* __RichSpectra__ */
const Int_t*&      PrimaryTracks_mVertexIndex               = iter("PrimaryTracks.mVertexIndex");
const UChar_t*&    PrimaryTracks_mNHits                     = iter("PrimaryTracks.mNHits");
  #ifdef  __PrimaryTracksHits__
const UChar_t*&    PrimaryTracks_mNHitsPoss                 = iter("PrimaryTracks.mNHitsPoss");
  #endif /* __PrimaryTracksHits__ */
const UChar_t*&    PrimaryTracks_mNHitsDedx                 = iter("PrimaryTracks.mNHitsDedx");
const UChar_t*&    PrimaryTracks_mNHitsFit                  = iter("PrimaryTracks.mNHitsFit");
  #ifdef  __PrimaryTracksHits__
const UChar_t*&    PrimaryTracks_mNHitsPossInner            = iter("PrimaryTracks.mNHitsPossInner");
const UChar_t*&    PrimaryTracks_mNHitsFitInner             = iter("PrimaryTracks.mNHitsFitInner");
const UChar_t*&    PrimaryTracks_mNHitsPossTpc              = iter("PrimaryTracks.mNHitsPossTpc");
const UChar_t*&    PrimaryTracks_mNHitsFitTpc               = iter("PrimaryTracks.mNHitsFitTpc");
const UShort_t*&   PrimaryTracks_mPidProbElectron           = iter("PrimaryTracks.mPidProbElectron");
const UShort_t*&   PrimaryTracks_mPidProbPion               = iter("PrimaryTracks.mPidProbPion");
const UShort_t*&   PrimaryTracks_mPidProbKaon               = iter("PrimaryTracks.mPidProbKaon");
const UShort_t*&   PrimaryTracks_mPidProbProton             = iter("PrimaryTracks.mPidProbProton");
const Int_t*&      PrimaryTracks_mNSigmaElectron            = iter("PrimaryTracks.mNSigmaElectron");
const Int_t*&      PrimaryTracks_mNSigmaPion                = iter("PrimaryTracks.mNSigmaPion");
const Int_t*&      PrimaryTracks_mNSigmaKaon                = iter("PrimaryTracks.mNSigmaKaon");
const Int_t*&      PrimaryTracks_mNSigmaProton              = iter("PrimaryTracks.mNSigmaProton");
const Float_t*&    PrimaryTracks_mdEdx                      = iter("PrimaryTracks.mdEdx");
  #endif /* __PrimaryTracksHits__ */
  #if ! defined(__MuDst__)
const Float_t*&    PrimaryTracks_mChiSqXY                   = iter("PrimaryTracks.mChiSqXY");
const Float_t*&    PrimaryTracks_mChiSqZ                    = iter("PrimaryTracks.mChiSqZ");
const Float_t*&    PrimaryTracks_mPt                        = iter("PrimaryTracks.mPt");
const Float_t*&    PrimaryTracks_mEta                       = iter("PrimaryTracks.mEta");
const Float_t*&    PrimaryTracks_mPhi                       = iter("PrimaryTracks.mPhi");
  #endif /* (__MuDst__ */
  #if ! defined(__MuCov__) && ! defined(__MuDst__)
const UInt_t*&     PrimaryTracks_mTopologyMap_mMap0         = iter("PrimaryTracks.mTopologyMap.mMap0");
const UInt_t*&     PrimaryTracks_mTopologyMap_mMap1         = iter("PrimaryTracks.mTopologyMap.mMap1");
const Float_t*&    PrimaryTracks_mP_mX1                     = iter("PrimaryTracks.mP.mX1");
const Float_t*&    PrimaryTracks_mP_mX2                     = iter("PrimaryTracks.mP.mX2");
const Float_t*&    PrimaryTracks_mP_mX3                     = iter("PrimaryTracks.mP.mX3");
    #ifdef __DCA__
const Float_t*&    PrimaryTracks_mDCA_mX1                   = iter("PrimaryTracks.mDCA.mX1");
const Float_t*&    PrimaryTracks_mDCA_mX2                   = iter("PrimaryTracks.mDCA.mX2");
const Float_t*&    PrimaryTracks_mDCA_mX3                   = iter("PrimaryTracks.mDCA.mX3");
const Float_t*&    PrimaryTracks_mDCAGlobal_mX1             = iter("PrimaryTracks.mDCAGlobal.mX1");
const Float_t*&    PrimaryTracks_mDCAGlobal_mX2             = iter("PrimaryTracks.mDCAGlobal.mX2");
const Float_t*&    PrimaryTracks_mDCAGlobal_mX3             = iter("PrimaryTracks.mDCAGlobal.mX3");
    #endif /* __DCA__ */ 
    #ifdef __FirstLastPoint__
const Float_t*&    PrimaryTracks_mFirstPoint_mX1            = iter("PrimaryTracks.mFirstPoint.mX1");
const Float_t*&    PrimaryTracks_mFirstPoint_mX2            = iter("PrimaryTracks.mFirstPoint.mX2");
const Float_t*&    PrimaryTracks_mFirstPoint_mX3            = iter("PrimaryTracks.mFirstPoint.mX3");
const Float_t*&    PrimaryTracks_mLastPoint_mX1             = iter("PrimaryTracks.mLastPoint.mX1");
const Float_t*&    PrimaryTracks_mLastPoint_mX2             = iter("PrimaryTracks.mLastPoint.mX2");
const Float_t*&    PrimaryTracks_mLastPoint_mX3             = iter("PrimaryTracks.mLastPoint.mX3");
    #endif /* __FirstLastPoint__ */
const Short_t*&    PrimaryTracks_mHelix_mQ                  = iter("PrimaryTracks.mHelix.mQ");
    #ifdef __Helix__
const Float_t*&    PrimaryTracks_mHelix_mP_mX1              = iter("PrimaryTracks.mHelix.mP.mX1");
const Float_t*&    PrimaryTracks_mHelix_mP_mX2              = iter("PrimaryTracks.mHelix.mP.mX2");
const Float_t*&    PrimaryTracks_mHelix_mP_mX3              = iter("PrimaryTracks.mHelix.mP.mX3");
const Float_t*&    PrimaryTracks_mHelix_mOrigin_mX1         = iter("PrimaryTracks.mHelix.mOrigin.mX1");
const Float_t*&    PrimaryTracks_mHelix_mOrigin_mX2         = iter("PrimaryTracks.mHelix.mOrigin.mX2");
const Float_t*&    PrimaryTracks_mHelix_mOrigin_mX3         = iter("PrimaryTracks.mHelix.mOrigin.mX3");
const Float_t*&    PrimaryTracks_mHelix_mB                  = iter("PrimaryTracks.mHelix.mB");
const Float_t*&    PrimaryTracks_mOuterHelix_mP_mX1         = iter("PrimaryTracks.mOuterHelix.mP.mX1");
const Float_t*&    PrimaryTracks_mOuterHelix_mP_mX2         = iter("PrimaryTracks.mOuterHelix.mP.mX2");
const Float_t*&    PrimaryTracks_mOuterHelix_mP_mX3         = iter("PrimaryTracks.mOuterHelix.mP.mX3");
const Float_t*&    PrimaryTracks_mOuterHelix_mOrigin_mX1    = iter("PrimaryTracks.mOuterHelix.mOrigin.mX1");
const Float_t*&    PrimaryTracks_mOuterHelix_mOrigin_mX2    = iter("PrimaryTracks.mOuterHelix.mOrigin.mX2");
const Float_t*&    PrimaryTracks_mOuterHelix_mOrigin_mX3    = iter("PrimaryTracks.mOuterHelix.mOrigin.mX3");
const Short_t*&    PrimaryTracks_mOuterHelix_mQ             = iter("PrimaryTracks.mOuterHelix.mQ");
const Float_t*&    PrimaryTracks_mOuterHelix_mB             = iter("PrimaryTracks.mOuterHelix.mB");
   #endif /* __Helix__ */
   #ifdef __dEdx__
__dEdx__P(Primary);
   #endif /* __dEdx__ */
   #ifdef __BTof__
	const Int_t*&      PrimaryTracks_mIndex2BTofHit             = iter("PrimaryTracks.mIndex2BTofHit");
__BTof__P(Primary);
   #endif /*  __BTof__ */
const Int_t*&      PrimaryTracks_mIndex2Cov                 = iter("PrimaryTracks.mIndex2Cov");
  #endif /* !__MuCov__ && !__MuDst__ */
#endif /*  __PrimaryTracks__ */
#ifdef __GlobalTracks__
const Int_t&       NoGlobalTracks                            = iter("GlobalTracks");
  #if ! defined(__MuCov__) && ! defined(__MuDst__)
#if 0
#ifdef __Y2015__
	const Int_t*&   PrimaryTracks_mIdTruth                   = iter("PrimaryTracks.mIdTruth");
	const Int_t*&   PrimaryTracks_mQuality                   = iter("PrimaryTracks.mQuality");
#else
	const UShort_t*&   PrimaryTracks_mIdTruth                   = iter("PrimaryTracks.mIdTruth");
	const UShort_t*&   PrimaryTracks_mQuality                   = iter("PrimaryTracks.mQuality");
#endif
#endif
	const Int_t*&      PrimaryTracks_mIdParentVx                = iter("PrimaryTracks.mIdParentVx");
	const Int_t&       GlobalTracks_                            = iter("GlobalTracks");
const Int_t*&    GlobalTracks_mId                         = iter("GlobalTracks.mId");
//const Short_t*&    GlobalTracks_mId                         = iter("GlobalTracks.mId");
const Short_t*&    GlobalTracks_mType                       = iter("GlobalTracks.mType");
  #endif /* ! __MuCov__ && ! __MuDst__ */
  #if ! defined(__MuCov__)
const Short_t*&    GlobalTracks_mFlag                       = iter("GlobalTracks.mFlag");
  #endif /*  __MuCov__ */
  #if ! defined(__MuDst__)
const Int_t*&      GlobalTracks_mIndex2Global               = iter("GlobalTracks.mIndex2Global");
  #endif /* __MuDst__ */
  #if ! defined(__MuDst__)
const Int_t*&      GlobalTracks_mIndex2RichSpectra          = iter("GlobalTracks.mIndex2RichSpectra");
	const Int_t*&      GlobalTracks_mIndex2BTofHit              = iter("GlobalTracks.mIndex2BTofHit");
const Int_t*&      GlobalTracks_mVertexIndex                = iter("GlobalTracks.mVertexIndex");
const UChar_t*&    GlobalTracks_mNHits                      = iter("GlobalTracks.mNHits");
const UChar_t*&    GlobalTracks_mNHitsPoss                  = iter("GlobalTracks.mNHitsPoss");
const UChar_t*&    GlobalTracks_mNHitsDedx                  = iter("GlobalTracks.mNHitsDedx");
  #endif /* ! __MuDst__ */
  #if ! defined(__MuCov__)
const UChar_t*&    GlobalTracks_mNHitsFit                   = iter("GlobalTracks.mNHitsFit");
  #endif
  #if ! defined(__MuCov__) && ! defined(__MuDst__)
const UChar_t*&    GlobalTracks_mNHitsPossInner             = iter("GlobalTracks.mNHitsPossInner");
const UChar_t*&    GlobalTracks_mNHitsFitInner              = iter("GlobalTracks.mNHitsFitInner");
const UChar_t*&    GlobalTracks_mNHitsPossTpc               = iter("GlobalTracks.mNHitsPossTpc");
const UChar_t*&    GlobalTracks_mNHitsFitTpc                = iter("GlobalTracks.mNHitsFitTpc");
const UShort_t*&   GlobalTracks_mPidProbElectron            = iter("GlobalTracks.mPidProbElectron");
const UShort_t*&   GlobalTracks_mPidProbPion                = iter("GlobalTracks.mPidProbPion");
const UShort_t*&   GlobalTracks_mPidProbKaon                = iter("GlobalTracks.mPidProbKaon");
const UShort_t*&   GlobalTracks_mPidProbProton              = iter("GlobalTracks.mPidProbProton");
const Int_t*&      GlobalTracks_mNSigmaElectron             = iter("GlobalTracks.mNSigmaElectron");
const Int_t*&      GlobalTracks_mNSigmaPion                 = iter("GlobalTracks.mNSigmaPion");
const Int_t*&      GlobalTracks_mNSigmaKaon                 = iter("GlobalTracks.mNSigmaKaon");
  #endif
  #if ! defined(__MuDst__)
const Int_t*&      GlobalTracks_mNSigmaProton               = iter("GlobalTracks.mNSigmaProton");
const Float_t*&    GlobalTracks_mdEdx                       = iter("GlobalTracks.mdEdx");
const Float_t*&    GlobalTracks_mChiSqXY                    = iter("GlobalTracks.mChiSqXY");
const Float_t*&    GlobalTracks_mChiSqZ                     = iter("GlobalTracks.mChiSqZ");
  #endif
const Float_t*&    GlobalTracks_mPt                         = iter("GlobalTracks.mPt");
const Float_t*&    GlobalTracks_mEta                        = iter("GlobalTracks.mEta");
const Float_t*&    GlobalTracks_mPhi                        = iter("GlobalTracks.mPhi");
  #if ! defined(__MuCov__) && ! defined(__MuDst__)
const UInt_t*&     GlobalTracks_mTopologyMap_mMap0          = iter("GlobalTracks.mTopologyMap.mMap0");
const UInt_t*&     GlobalTracks_mTopologyMap_mMap1          = iter("GlobalTracks.mTopologyMap.mMap1");
const Float_t*&    GlobalTracks_mP_mX1                      = iter("GlobalTracks.mP.mX1");
const Float_t*&    GlobalTracks_mP_mX2                      = iter("GlobalTracks.mP.mX2");
const Float_t*&    GlobalTracks_mP_mX3                      = iter("GlobalTracks.mP.mX3");
    #ifdef __DCA__
const Float_t*&    GlobalTracks_mDCA_mX1                    = iter("GlobalTracks.mDCA.mX1");
const Float_t*&    GlobalTracks_mDCA_mX2                    = iter("GlobalTracks.mDCA.mX2");
const Float_t*&    GlobalTracks_mDCA_mX3                    = iter("GlobalTracks.mDCA.mX3");
const Float_t*&    GlobalTracks_mDCAGlobal_mX1              = iter("GlobalTracks.mDCAGlobal.mX1");
const Float_t*&    GlobalTracks_mDCAGlobal_mX2              = iter("GlobalTracks.mDCAGlobal.mX2");
const Float_t*&    GlobalTracks_mDCAGlobal_mX3              = iter("GlobalTracks.mDCAGlobal.mX3");
    #endif /* __DCA__ */
    #ifdef __FirstLastPoint__
const Float_t*&    GlobalTracks_mFirstPoint_mX1             = iter("GlobalTracks.mFirstPoint.mX1");
const Float_t*&    GlobalTracks_mFirstPoint_mX2             = iter("GlobalTracks.mFirstPoint.mX2");
const Float_t*&    GlobalTracks_mFirstPoint_mX3             = iter("GlobalTracks.mFirstPoint.mX3");
const Float_t*&    GlobalTracks_mLastPoint_mX1              = iter("GlobalTracks.mLastPoint.mX1");
const Float_t*&    GlobalTracks_mLastPoint_mX2              = iter("GlobalTracks.mLastPoint.mX2");
const Float_t*&    GlobalTracks_mLastPoint_mX3              = iter("GlobalTracks.mLastPoint.mX3");
    #endif /* __FirstLastPoint__ */
    #ifdef __Helix__
const Float_t*&    GlobalTracks_mHelix_mP_mX1               = iter("GlobalTracks.mHelix.mP.mX1");
const Float_t*&    GlobalTracks_mHelix_mP_mX2               = iter("GlobalTracks.mHelix.mP.mX2");
const Float_t*&    GlobalTracks_mHelix_mP_mX3               = iter("GlobalTracks.mHelix.mP.mX3");
const Float_t*&    GlobalTracks_mHelix_mOrigin_mX1          = iter("GlobalTracks.mHelix.mOrigin.mX1");
const Float_t*&    GlobalTracks_mHelix_mOrigin_mX2          = iter("GlobalTracks.mHelix.mOrigin.mX2");
const Float_t*&    GlobalTracks_mHelix_mOrigin_mX3          = iter("GlobalTracks.mHelix.mOrigin.mX3");
const Short_t*&    GlobalTracks_mHelix_mQ                   = iter("GlobalTracks.mHelix.mQ");
const Float_t*&    GlobalTracks_mHelix_mB                   = iter("GlobalTracks.mHelix.mB");
const Float_t*&    GlobalTracks_mOuterHelix_mP_mX1          = iter("GlobalTracks.mOuterHelix.mP.mX1");
const Float_t*&    GlobalTracks_mOuterHelix_mP_mX2          = iter("GlobalTracks.mOuterHelix.mP.mX2");
const Float_t*&    GlobalTracks_mOuterHelix_mP_mX3          = iter("GlobalTracks.mOuterHelix.mP.mX3");
const Float_t*&    GlobalTracks_mOuterHelix_mOrigin_mX1     = iter("GlobalTracks.mOuterHelix.mOrigin.mX1");
const Float_t*&    GlobalTracks_mOuterHelix_mOrigin_mX2     = iter("GlobalTracks.mOuterHelix.mOrigin.mX2");
const Float_t*&    GlobalTracks_mOuterHelix_mOrigin_mX3     = iter("GlobalTracks.mOuterHelix.mOrigin.mX3");
const Short_t*&    GlobalTracks_mOuterHelix_mQ              = iter("GlobalTracks.mOuterHelix.mQ");
const Float_t*&    GlobalTracks_mOuterHelix_mB              = iter("GlobalTracks.mOuterHelix.mB");
    #endif /* __Helix__ */
    #ifdef __dEdx__
__dEdx__P(Global);
    #endif /* __dEdx__ */
    #ifdef __DCA__
const Float_t*&    GlobalTracks_mSigmaDcaD                  = iter("GlobalTracks.mSigmaDcaD");
const Float_t*&    GlobalTracks_mSigmaDcaZ                  = iter("GlobalTracks.mSigmaDcaZ");
    #endif /* __DCA__ */
  #endif /* ! __MuCov__ && ! __MuDst__ */
  #if ! defined(__MuDst__)
    #ifdef __BTof__
__BTof__P(Global);
    #endif /*  __BTof__ */
const Int_t*&      GlobalTracks_mIndex2Cov                  = iter("GlobalTracks.mIndex2Cov");
  #endif
#endif /* __GlobalTracks__ */
#ifdef __OtherTracks__
const Int_t&       NoOtherTracks                             = iter("OtherTracks");
	const Int_t*&   GlobalTracks_mIdTruth                    = iter("GlobalTracks.mIdTruth");
	const Int_t*&   GlobalTracks_mQuality                    = iter("GlobalTracks.mQuality");
	const Int_t*&      GlobalTracks_mIdParentVx                 = iter("GlobalTracks.mIdParentVx");
	const Int_t&       OtherTracks_                             = iter("OtherTracks");
const Int_t*&    OtherTracks_mId                          = iter("OtherTracks.mId");
const Short_t*&    OtherTracks_mType                        = iter("OtherTracks.mType");
const Short_t*&    OtherTracks_mFlag                        = iter("OtherTracks.mFlag");
const Int_t*&      OtherTracks_mIndex2Global                = iter("OtherTracks.mIndex2Global");
const Int_t*&      OtherTracks_mIndex2RichSpectra           = iter("OtherTracks.mIndex2RichSpectra");
	const Int_t*&      OtherTracks_mIndex2BTofHit               = iter("OtherTracks.mIndex2BTofHit");
const Int_t*&      OtherTracks_mVertexIndex                 = iter("OtherTracks.mVertexIndex");
const UChar_t*&    OtherTracks_mNHits                       = iter("OtherTracks.mNHits");
const UChar_t*&    OtherTracks_mNHitsPoss                   = iter("OtherTracks.mNHitsPoss");
const UChar_t*&    OtherTracks_mNHitsDedx                   = iter("OtherTracks.mNHitsDedx");
const UChar_t*&    OtherTracks_mNHitsFit                    = iter("OtherTracks.mNHitsFit");
const UChar_t*&    OtherTracks_mNHitsPossInner              = iter("OtherTracks.mNHitsPossInner");
const UChar_t*&    OtherTracks_mNHitsFitInner               = iter("OtherTracks.mNHitsFitInner");
const UChar_t*&    OtherTracks_mNHitsPossTpc                = iter("OtherTracks.mNHitsPossTpc");
const UChar_t*&    OtherTracks_mNHitsFitTpc                 = iter("OtherTracks.mNHitsFitTpc");
const UShort_t*&   OtherTracks_mPidProbElectron             = iter("OtherTracks.mPidProbElectron");
const UShort_t*&   OtherTracks_mPidProbPion                 = iter("OtherTracks.mPidProbPion");
const UShort_t*&   OtherTracks_mPidProbKaon                 = iter("OtherTracks.mPidProbKaon");
const UShort_t*&   OtherTracks_mPidProbProton               = iter("OtherTracks.mPidProbProton");
const Int_t*&      OtherTracks_mNSigmaElectron              = iter("OtherTracks.mNSigmaElectron");
const Int_t*&      OtherTracks_mNSigmaPion                  = iter("OtherTracks.mNSigmaPion");
const Int_t*&      OtherTracks_mNSigmaKaon                  = iter("OtherTracks.mNSigmaKaon");
const Int_t*&      OtherTracks_mNSigmaProton                = iter("OtherTracks.mNSigmaProton");
const Float_t*&    OtherTracks_mdEdx                        = iter("OtherTracks.mdEdx");
const Float_t*&    OtherTracks_mChiSqXY                     = iter("OtherTracks.mChiSqXY");
const Float_t*&    OtherTracks_mChiSqZ                      = iter("OtherTracks.mChiSqZ");
const Float_t*&    OtherTracks_mPt                          = iter("OtherTracks.mPt");
const Float_t*&    OtherTracks_mEta                         = iter("OtherTracks.mEta");
const Float_t*&    OtherTracks_mPhi                         = iter("OtherTracks.mPhi");
const UInt_t*&     OtherTracks_mTopologyMap_mMap0           = iter("OtherTracks.mTopologyMap.mMap0");
const UInt_t*&     OtherTracks_mTopologyMap_mMap1           = iter("OtherTracks.mTopologyMap.mMap1");
const Float_t*&    OtherTracks_mP_mX1                       = iter("OtherTracks.mP.mX1");
const Float_t*&    OtherTracks_mP_mX2                       = iter("OtherTracks.mP.mX2");
const Float_t*&    OtherTracks_mP_mX3                       = iter("OtherTracks.mP.mX3");
  #ifdef __DCA__
const Float_t*&    OtherTracks_mDCA_mX1                     = iter("OtherTracks.mDCA.mX1");
const Float_t*&    OtherTracks_mDCA_mX2                     = iter("OtherTracks.mDCA.mX2");
const Float_t*&    OtherTracks_mDCA_mX3                     = iter("OtherTracks.mDCA.mX3");
const Float_t*&    OtherTracks_mDCAGlobal_mX1               = iter("OtherTracks.mDCAGlobal.mX1");
const Float_t*&    OtherTracks_mDCAGlobal_mX2               = iter("OtherTracks.mDCAGlobal.mX2");
const Float_t*&    OtherTracks_mDCAGlobal_mX3               = iter("OtherTracks.mDCAGlobal.mX3");
  #endif /* __DCA__ */
  #ifdef __FirstLastPoint__
const Float_t*&    OtherTracks_mFirstPoint_mX1              = iter("OtherTracks.mFirstPoint.mX1");
const Float_t*&    OtherTracks_mFirstPoint_mX2              = iter("OtherTracks.mFirstPoint.mX2");
const Float_t*&    OtherTracks_mFirstPoint_mX3              = iter("OtherTracks.mFirstPoint.mX3");
const Float_t*&    OtherTracks_mLastPoint_mX1               = iter("OtherTracks.mLastPoint.mX1");
const Float_t*&    OtherTracks_mLastPoint_mX2               = iter("OtherTracks.mLastPoint.mX2");
const Float_t*&    OtherTracks_mLastPoint_mX3               = iter("OtherTracks.mLastPoint.mX3");
  #endif /* __FirstLastPoint__ */
  #ifdef __Helix__
const Float_t*&    OtherTracks_mHelix_mP_mX1                = iter("OtherTracks.mHelix.mP.mX1");
const Float_t*&    OtherTracks_mHelix_mP_mX2                = iter("OtherTracks.mHelix.mP.mX2");
const Float_t*&    OtherTracks_mHelix_mP_mX3                = iter("OtherTracks.mHelix.mP.mX3");
const Float_t*&    OtherTracks_mHelix_mOrigin_mX1           = iter("OtherTracks.mHelix.mOrigin.mX1");
const Float_t*&    OtherTracks_mHelix_mOrigin_mX2           = iter("OtherTracks.mHelix.mOrigin.mX2");
const Float_t*&    OtherTracks_mHelix_mOrigin_mX3           = iter("OtherTracks.mHelix.mOrigin.mX3");
const Short_t*&    OtherTracks_mHelix_mQ                    = iter("OtherTracks.mHelix.mQ");
const Float_t*&    OtherTracks_mHelix_mB                    = iter("OtherTracks.mHelix.mB");
const Float_t*&    OtherTracks_mOuterHelix_mP_mX1           = iter("OtherTracks.mOuterHelix.mP.mX1");
const Float_t*&    OtherTracks_mOuterHelix_mP_mX2           = iter("OtherTracks.mOuterHelix.mP.mX2");
const Float_t*&    OtherTracks_mOuterHelix_mP_mX3           = iter("OtherTracks.mOuterHelix.mP.mX3");
const Float_t*&    OtherTracks_mOuterHelix_mOrigin_mX1      = iter("OtherTracks.mOuterHelix.mOrigin.mX1");
const Float_t*&    OtherTracks_mOuterHelix_mOrigin_mX2      = iter("OtherTracks.mOuterHelix.mOrigin.mX2");
const Float_t*&    OtherTracks_mOuterHelix_mOrigin_mX3      = iter("OtherTracks.mOuterHelix.mOrigin.mX3");
const Short_t*&    OtherTracks_mOuterHelix_mQ               = iter("OtherTracks.mOuterHelix.mQ");
const Float_t*&    OtherTracks_mOuterHelix_mB               = iter("OtherTracks.mOuterHelix.mB");
  #endif /* __Helix__ */
  #ifdef __dEdx__
__dEdx__P(OtherTracks);
  #endif /* __dEdx__ */
  #ifdef __DCA__
const Float_t*&    OtherTracks_mSigmaDcaD                   = iter("OtherTracks.mSigmaDcaD");
const Float_t*&    OtherTracks_mSigmaDcaZ                   = iter("OtherTracks.mSigmaDcaZ");
    #ifdef __BTof__
__BTof__P(Other);
    #endif /*  __BTof__ */
const Int_t*&      OtherTracks_mIndex2Cov                   = iter("OtherTracks.mIndex2Cov");
  #endif /* __DCA__ */
#endif /* __OtherTracks__ */
#ifdef __L3Tracks__
const Int_t&       NoL3Tracks                                = iter("L3Tracks");
	const Int_t*&   OtherTracks_mIdTruth                     = iter("OtherTracks.mIdTruth");
	const Int_t*&   OtherTracks_mQuality                     = iter("OtherTracks.mQuality");
	const Int_t*&      OtherTracks_mIdParentVx                  = iter("OtherTracks.mIdParentVx");
	const Int_t&       L3Tracks_                                = iter("L3Tracks");
const Int_t*&    L3Tracks_mId                             = iter("L3Tracks.mId");
const Short_t*&    L3Tracks_mType                           = iter("L3Tracks.mType");
const Short_t*&    L3Tracks_mFlag                           = iter("L3Tracks.mFlag");
const Int_t*&      L3Tracks_mIndex2Global                   = iter("L3Tracks.mIndex2Global");
const Int_t*&      L3Tracks_mIndex2RichSpectra              = iter("L3Tracks.mIndex2RichSpectra");
	const Int_t*&      L3Tracks_mIndex2BTofHit                  = iter("L3Tracks.mIndex2BTofHit");
const Int_t*&      L3Tracks_mVertexIndex                    = iter("L3Tracks.mVertexIndex");
const UChar_t*&    L3Tracks_mNHits                          = iter("L3Tracks.mNHits");
const UChar_t*&    L3Tracks_mNHitsPoss                      = iter("L3Tracks.mNHitsPoss");
const UChar_t*&    L3Tracks_mNHitsDedx                      = iter("L3Tracks.mNHitsDedx");
const UChar_t*&    L3Tracks_mNHitsFit                       = iter("L3Tracks.mNHitsFit");
const UChar_t*&    L3Tracks_mNHitsPossInner                 = iter("L3Tracks.mNHitsPossInner");
const UChar_t*&    L3Tracks_mNHitsFitInner                  = iter("L3Tracks.mNHitsFitInner");
const UChar_t*&    L3Tracks_mNHitsPossTpc                   = iter("L3Tracks.mNHitsPossTpc");
const UChar_t*&    L3Tracks_mNHitsFitTpc                    = iter("L3Tracks.mNHitsFitTpc");
const UShort_t*&   L3Tracks_mPidProbElectron                = iter("L3Tracks.mPidProbElectron");
const UShort_t*&   L3Tracks_mPidProbPion                    = iter("L3Tracks.mPidProbPion");
const UShort_t*&   L3Tracks_mPidProbKaon                    = iter("L3Tracks.mPidProbKaon");
const UShort_t*&   L3Tracks_mPidProbProton                  = iter("L3Tracks.mPidProbProton");
const Int_t*&      L3Tracks_mNSigmaElectron                 = iter("L3Tracks.mNSigmaElectron");
const Int_t*&      L3Tracks_mNSigmaPion                     = iter("L3Tracks.mNSigmaPion");
const Int_t*&      L3Tracks_mNSigmaKaon                     = iter("L3Tracks.mNSigmaKaon");
const Int_t*&      L3Tracks_mNSigmaProton                   = iter("L3Tracks.mNSigmaProton");
const Float_t*&    L3Tracks_mdEdx                           = iter("L3Tracks.mdEdx");
const Float_t*&    L3Tracks_mChiSqXY                        = iter("L3Tracks.mChiSqXY");
const Float_t*&    L3Tracks_mChiSqZ                         = iter("L3Tracks.mChiSqZ");
const Float_t*&    L3Tracks_mPt                             = iter("L3Tracks.mPt");
const Float_t*&    L3Tracks_mEta                            = iter("L3Tracks.mEta");
const Float_t*&    L3Tracks_mPhi                            = iter("L3Tracks.mPhi");
const UInt_t*&     L3Tracks_mTopologyMap_mMap0              = iter("L3Tracks.mTopologyMap.mMap0");
const UInt_t*&     L3Tracks_mTopologyMap_mMap1              = iter("L3Tracks.mTopologyMap.mMap1");
const Float_t*&    L3Tracks_mP_mX1                          = iter("L3Tracks.mP.mX1");
const Float_t*&    L3Tracks_mP_mX2                          = iter("L3Tracks.mP.mX2");
const Float_t*&    L3Tracks_mP_mX3                          = iter("L3Tracks.mP.mX3");
  #ifdef __DCA__
const Float_t*&    L3Tracks_mDCA_mX1                        = iter("L3Tracks.mDCA.mX1");
const Float_t*&    L3Tracks_mDCA_mX2                        = iter("L3Tracks.mDCA.mX2");
const Float_t*&    L3Tracks_mDCA_mX3                        = iter("L3Tracks.mDCA.mX3");
const Float_t*&    L3Tracks_mDCAGlobal_mX1                  = iter("L3Tracks.mDCAGlobal.mX1");
const Float_t*&    L3Tracks_mDCAGlobal_mX2                  = iter("L3Tracks.mDCAGlobal.mX2");
const Float_t*&    L3Tracks_mDCAGlobal_mX3                  = iter("L3Tracks.mDCAGlobal.mX3");  
  #endif /* __DCA__ */
  #ifdef __FirstLastPoint__
const Float_t*&    L3Tracks_mFirstPoint_mX1                 = iter("L3Tracks.mFirstPoint.mX1");
const Float_t*&    L3Tracks_mFirstPoint_mX2                 = iter("L3Tracks.mFirstPoint.mX2");
const Float_t*&    L3Tracks_mFirstPoint_mX3                 = iter("L3Tracks.mFirstPoint.mX3");
const Float_t*&    L3Tracks_mLastPoint_mX1                  = iter("L3Tracks.mLastPoint.mX1");
const Float_t*&    L3Tracks_mLastPoint_mX2                  = iter("L3Tracks.mLastPoint.mX2");
const Float_t*&    L3Tracks_mLastPoint_mX3                  = iter("L3Tracks.mLastPoint.mX3");
  #endif /* __FirstLastPoint__ */
  #ifdef __Helix__
const Float_t*&    L3Tracks_mHelix_mP_mX1                   = iter("L3Tracks.mHelix.mP.mX1");
const Float_t*&    L3Tracks_mHelix_mP_mX2                   = iter("L3Tracks.mHelix.mP.mX2");
const Float_t*&    L3Tracks_mHelix_mP_mX3                   = iter("L3Tracks.mHelix.mP.mX3");
const Float_t*&    L3Tracks_mHelix_mOrigin_mX1              = iter("L3Tracks.mHelix.mOrigin.mX1");
const Float_t*&    L3Tracks_mHelix_mOrigin_mX2              = iter("L3Tracks.mHelix.mOrigin.mX2");
const Float_t*&    L3Tracks_mHelix_mOrigin_mX3              = iter("L3Tracks.mHelix.mOrigin.mX3");
const Short_t*&    L3Tracks_mHelix_mQ                       = iter("L3Tracks.mHelix.mQ");
const Float_t*&    L3Tracks_mHelix_mB                       = iter("L3Tracks.mHelix.mB");
const Float_t*&    L3Tracks_mOuterHelix_mP_mX1              = iter("L3Tracks.mOuterHelix.mP.mX1");
const Float_t*&    L3Tracks_mOuterHelix_mP_mX2              = iter("L3Tracks.mOuterHelix.mP.mX2");
const Float_t*&    L3Tracks_mOuterHelix_mP_mX3              = iter("L3Tracks.mOuterHelix.mP.mX3");
const Float_t*&    L3Tracks_mOuterHelix_mOrigin_mX1         = iter("L3Tracks.mOuterHelix.mOrigin.mX1");
const Float_t*&    L3Tracks_mOuterHelix_mOrigin_mX2         = iter("L3Tracks.mOuterHelix.mOrigin.mX2");
const Float_t*&    L3Tracks_mOuterHelix_mOrigin_mX3         = iter("L3Tracks.mOuterHelix.mOrigin.mX3");
const Short_t*&    L3Tracks_mOuterHelix_mQ                  = iter("L3Tracks.mOuterHelix.mQ");
const Float_t*&    L3Tracks_mOuterHelix_mB                  = iter("L3Tracks.mOuterHelix.mB");
  #endif /* __Helix__ */
  #ifdef __dEdx__
__dEdx__P(L3Tracks);
  #endif /* __dEdx__ */
  #ifdef __DCA__
const Float_t*&    L3Tracks_mSigmaDcaD                      = iter("L3Tracks.mSigmaDcaD");
const Float_t*&    L3Tracks_mSigmaDcaZ                      = iter("L3Tracks.mSigmaDcaZ");
    #ifdef __BTof__
__BTof__P(L3);
    #endif /*  __BTof__ */
const Int_t*&      L3Tracks_mIndex2Cov                      = iter("L3Tracks.mIndex2Cov");
  #endif /* __DCA__ */
#endif /*  __L3Tracks__ */
#ifdef __RichSpectra__
const Int_t&       NoRichSpectra                             = iter("RichSpectra");
	const Int_t*&   L3Tracks_mIdTruth                        = iter("L3Tracks.mIdTruth");
	const Int_t*&   L3Tracks_mQuality                        = iter("L3Tracks.mQuality");
	const Int_t*&      L3Tracks_mIdParentVx                     = iter("L3Tracks.mIdParentVx");
	const Int_t&       RichSpectra_                             = iter("RichSpectra");
const Float_t*&    RichSpectra_mExtrapolatedX               = iter("RichSpectra.mExtrapolatedX");
const Float_t*&    RichSpectra_mExtrapolatedY               = iter("RichSpectra.mExtrapolatedY");
const Float_t*&    RichSpectra_mDx                          = iter("RichSpectra.mDx");
const Float_t*&    RichSpectra_mDy                          = iter("RichSpectra.mDy");
const Float_t*&    RichSpectra_mCdx                         = iter("RichSpectra.mCdx");
const Float_t*&    RichSpectra_mCdy                         = iter("RichSpectra.mCdy");
const Float_t*&    RichSpectra_mCherenkovAngle              = iter("RichSpectra.mCherenkovAngle");
const Float_t*&    RichSpectra_mCherenkovAngleSigma         = iter("RichSpectra.mCherenkovAngleSigma");
const Int_t*&      RichSpectra_mNumberOfPhotons             = iter("RichSpectra.mNumberOfPhotons");
const Float_t*&    RichSpectra_mPeakAngle                   = iter("RichSpectra.mPeakAngle");
const Int_t*&      RichSpectra_mPeakPhotons                 = iter("RichSpectra.mPeakPhotons");
const Int_t*&      RichSpectra_mTotalPhotons                = iter("RichSpectra.mTotalPhotons");
const Float_t*&    RichSpectra_mMassSquared                 = iter("RichSpectra.mMassSquared");
const Float_t*&    RichSpectra_mLineIntegralRatio           = iter("RichSpectra.mLineIntegralRatio");
const Float_t*&    RichSpectra_mLineIntegral                = iter("RichSpectra.mLineIntegral");
const Float_t*&    RichSpectra_mAlpha                       = iter("RichSpectra.mAlpha");
const Int_t*&      RichSpectra_mFlag                        = iter("RichSpectra.mFlag");
const Float_t*&    RichSpectra_mReserved                    = iter("RichSpectra.mReserved");
const Float_t*&    RichSpectra_mDpi                         = iter("RichSpectra.mDpi");
const Float_t*&    RichSpectra_mDk                          = iter("RichSpectra.mDk");
const Float_t*&    RichSpectra_mDp                          = iter("RichSpectra.mDp");
const Int_t*&      RichSpectra_mNDpi                        = iter("RichSpectra.mNDpi");
const Int_t*&      RichSpectra_mNDk                         = iter("RichSpectra.mNDk");
const Int_t*&      RichSpectra_mNDp                         = iter("RichSpectra.mNDp");
const Int_t*&      RichSpectra_mVersion                     = iter("RichSpectra.mVersion");
#endif /* __RichSpectra__ */
#ifdef __L3AlgoAccept__
const Int_t&       NoDetectorStates                          = iter("DetectorStates");
	const Int_t&       DetectorStates_                          = iter("DetectorStates");
const Int_t*&      DetectorStates_mDetectorId               = iter("DetectorStates.mDetectorId");
const Char_t*&     DetectorStates_mIsGood                   = iter("DetectorStates.mIsGood");
const Int_t&       NoL3AlgoAccept                            = iter("L3AlgoAccept");
	const Int_t&       L3AlgoAccept_                            = iter("L3AlgoAccept");
const Int_t*&      L3AlgoAccept_mId                         = iter("L3AlgoAccept.mId");
const Char_t*&     L3AlgoAccept_mOn                         = iter("L3AlgoAccept.mOn");
const Char_t*&     L3AlgoAccept_mAccept                     = iter("L3AlgoAccept.mAccept");
const Char_t*&     L3AlgoAccept_mBuild                      = iter("L3AlgoAccept.mBuild");
const Int_t*&      L3AlgoAccept_mNumberOfProcessedEvents    = iter("L3AlgoAccept.mNumberOfProcessedEvents");
const Int_t*&      L3AlgoAccept_mNumberOfAcceptedEvents     = iter("L3AlgoAccept.mNumberOfAcceptedEvents");
const Int_t*&      L3AlgoAccept_mNumberOfBuildEvents        = iter("L3AlgoAccept.mNumberOfBuildEvents");
const UShort_t*&   L3AlgoAccept_mDataSize                   = iter("L3AlgoAccept.mDataSize");
const TArrayF*&    L3AlgoAccept_mDataArray                  = iter("L3AlgoAccept.mDataArray");
const Int_t*&      L3AlgoAccept_mPreScale                   = iter("L3AlgoAccept.mPreScale");
const Int_t*&      L3AlgoAccept_mPostScale                  = iter("L3AlgoAccept.mPostScale");
const UShort_t*&   L3AlgoAccept_mIntParameterSize           = iter("L3AlgoAccept.mIntParameterSize");
const TArrayI*&    L3AlgoAccept_mIntParameterArray          = iter("L3AlgoAccept.mIntParameterArray");
const UShort_t*&   L3AlgoAccept_mFloatParameterSize         = iter("L3AlgoAccept.mFloatParameterSize");
const TArrayF*&    L3AlgoAccept_mFloatParameterArray        = iter("L3AlgoAccept.mFloatParameterArray");
const Int_t&       NoL3AlgoReject                            = iter("L3AlgoReject");
	const Int_t&       L3AlgoReject_                            = iter("L3AlgoReject");
const Int_t*&      L3AlgoReject_mId                         = iter("L3AlgoReject.mId");
const Char_t*&     L3AlgoReject_mOn                         = iter("L3AlgoReject.mOn");
const Char_t*&     L3AlgoReject_mAccept                     = iter("L3AlgoReject.mAccept");
const Char_t*&     L3AlgoReject_mBuild                      = iter("L3AlgoReject.mBuild");
const Int_t*&      L3AlgoReject_mNumberOfProcessedEvents    = iter("L3AlgoReject.mNumberOfProcessedEvents");
const Int_t*&      L3AlgoReject_mNumberOfAcceptedEvents     = iter("L3AlgoReject.mNumberOfAcceptedEvents");
const Int_t*&      L3AlgoReject_mNumberOfBuildEvents        = iter("L3AlgoReject.mNumberOfBuildEvents");
const UShort_t*&   L3AlgoReject_mDataSize                   = iter("L3AlgoReject.mDataSize");
const TArrayF*&    L3AlgoReject_mDataArray                  = iter("L3AlgoReject.mDataArray");
const Int_t*&      L3AlgoReject_mPreScale                   = iter("L3AlgoReject.mPreScale");
const Int_t*&      L3AlgoReject_mPostScale                  = iter("L3AlgoReject.mPostScale");
const UShort_t*&   L3AlgoReject_mIntParameterSize           = iter("L3AlgoReject.mIntParameterSize");
const TArrayI*&    L3AlgoReject_mIntParameterArray          = iter("L3AlgoReject.mIntParameterArray");
const UShort_t*&   L3AlgoReject_mFloatParameterSize         = iter("L3AlgoReject.mFloatParameterSize");
const TArrayF*&    L3AlgoReject_mFloatParameterArray        = iter("L3AlgoReject.mFloatParameterArray");
#endif /* __L3AlgoAccept__ */
#ifdef __CovGlobTrack__
const Int_t&       NoCovGlobTrack                           = iter("CovGlobTrack");
const Float_t*&    CovGlobTrack_mImp                        = iter("CovGlobTrack.mImp");
const Float_t*&    CovGlobTrack_mZ                          = iter("CovGlobTrack.mZ");
const Float_t*&    CovGlobTrack_mPsi                        = iter("CovGlobTrack.mPsi");
const Float_t*&    CovGlobTrack_mPti                        = iter("CovGlobTrack.mPti");
const Float_t*&    CovGlobTrack_mTan                        = iter("CovGlobTrack.mTan");
const Float_t*&    CovGlobTrack_mCurv                       = iter("CovGlobTrack.mCurv");
const Float_t*&    CovGlobTrack_mImpImp                     = iter("CovGlobTrack.mImpImp");
const Float_t*&    CovGlobTrack_mZImp                       = iter("CovGlobTrack.mZImp");
const Float_t*&    CovGlobTrack_mZZ                         = iter("CovGlobTrack.mZZ");
const Float_t*&    CovGlobTrack_mPsiImp                     = iter("CovGlobTrack.mPsiImp");
const Float_t*&    CovGlobTrack_mPsiZ                       = iter("CovGlobTrack.mPsiZ");
const Float_t*&    CovGlobTrack_mPsiPsi                     = iter("CovGlobTrack.mPsiPsi");
const Float_t*&    CovGlobTrack_mPtiImp                     = iter("CovGlobTrack.mPtiImp");
const Float_t*&    CovGlobTrack_mPtiZ                       = iter("CovGlobTrack.mPtiZ");
const Float_t*&    CovGlobTrack_mPtiPsi                     = iter("CovGlobTrack.mPtiPsi");
const Float_t*&    CovGlobTrack_mPtiPti                     = iter("CovGlobTrack.mPtiPti");
const Float_t*&    CovGlobTrack_mTanImp                     = iter("CovGlobTrack.mTanImp");
const Float_t*&    CovGlobTrack_mTanZ                       = iter("CovGlobTrack.mTanZ");
const Float_t*&    CovGlobTrack_mTanPsi                     = iter("CovGlobTrack.mTanPsi");
const Float_t*&    CovGlobTrack_mTanPti                     = iter("CovGlobTrack.mTanPti");
const Float_t*&    CovGlobTrack_mTanTan                     = iter("CovGlobTrack.mTanTan");
#endif /* __CovGlobTrack__ */
#ifdef __CovPrimTrack__
const Int_t&       NoCovPrimTrack                           = iter("CovPrimTrack");
const Float_t*&    CovPrimTrack_mTanTan                     = iter("CovPrimTrack.mTanTan");
const Float_t*&    CovPrimTrack_mPsiTan                     = iter("CovPrimTrack.mPsiTan");
const Float_t*&    CovPrimTrack_mPsiPsi                     = iter("CovPrimTrack.mPsiPsi");
const Float_t*&    CovPrimTrack_mPtiTan                     = iter("CovPrimTrack.mPtiTan");
const Float_t*&    CovPrimTrack_mPtiPsi                     = iter("CovPrimTrack.mPtiPsi");
const Float_t*&    CovPrimTrack_mPtiPti                     = iter("CovPrimTrack.mPtiPti");
#endif /* __CovPrimTrack__ */
#ifdef __Event__
const Int_t&       NoEvent                                  = iter("Event");
	const Int_t&       pp2pp_                                   = iter("pp2pp");
	const UChar_t*&    pp2pp_mSiliconBunch                      = iter("pp2pp.mSiliconBunch");
	const Int_t*&      pp2pp_mNumberPlanes                      = iter("pp2pp.mNumberPlanes[8]");
	const Int_t*&      pp2pp_mNumberPlanesWithClusters          = iter("pp2pp.mNumberPlanesWithClusters[8]");
	const UChar_t*&    pp2pp_mStatusRomanPot                    = iter("pp2pp.mStatusRomanPot[8]");
	const Int_t*&      pp2pp_mADC                               = iter("pp2pp.mADC[8][2]");
	const Int_t*&      pp2pp_mTAC                               = iter("pp2pp.mTAC[8][2]");
	const Double_t*&   pp2pp_mOffsetPlane                       = iter("pp2pp.mOffsetPlane[8][4]");
	const Double_t*&   pp2pp_mzPlane                            = iter("pp2pp.mzPlane[8][4]");
	const Double_t*&   pp2pp_mAnglePlane                        = iter("pp2pp.mAnglePlane[8][4]");
	const Short_t*&    pp2pp_mOrientationPlane                  = iter("pp2pp.mOrientationPlane[8][4]");
	const UChar_t*&    pp2pp_mStatusPlane                       = iter("pp2pp.mStatusPlane[8][4]");
	const Int_t*&      pp2pp_mNumberOfClusters                  = iter("pp2pp.mNumberOfClusters[8][4]");
	const vector<double>*& pp2pp_mPositionCluster                   = iter("pp2pp.mPositionCluster[8][4]");
	const vector<short>*& pp2pp_mLengthCluster                     = iter("pp2pp.mLengthCluster[8][4]");
	const vector<double>*& pp2pp_mEnergyCluster                     = iter("pp2pp.mEnergyCluster[8][4]");
	const vector<double>*& pp2pp_mXYCluster                         = iter("pp2pp.mXYCluster[8][4]");
	const Int_t&       mtd_                                     = iter("mtd");
	const Int_t&       Event_                                   = iter("Event");
const Int_t*&      Event_mRun                               = iter("Event.mRun");
const Int_t*&      Event_mEvent                             = iter("Event.mEvent");
const Float_t*&    Event_mPrimaryVertexX                    = iter("Event.mPrimaryVertexX");
const Float_t*&    Event_mPrimaryVertexY                    = iter("Event.mPrimaryVertexY");
const Float_t*&    Event_mPrimaryVertexZ                    = iter("Event.mPrimaryVertexZ");
const Int_t*&      Event_mGlobalTracks                      = iter("Event.mGlobalTracks");
const Int_t*&      Event_mPrimaryTracks                     = iter("Event.mPrimaryTracks");
const Int_t*&      Event_mPrimaryNegTracks                  = iter("Event.mPrimaryNegTracks");
const Float_t*&    Event_mMagneticField                     = iter("Event.mMagneticField");
const UInt_t*&     Event_mL0TriggerWord                     = iter("Event.mL0TriggerWord");
#endif /* __Event__ */
#ifdef __McEvent__
const Int_t&       NoMcEvent                                 = iter("McEvent");
const Int_t*&      McEvent_mRun                             = iter("McEvent.mRun");
const Int_t*&      McEvent_mEvent                           = iter("McEvent.mEvent");
const Float_t*&    McEvent_mPrimaryVertexX                  = iter("McEvent.mPrimaryVertexX");
const Float_t*&    McEvent_mPrimaryVertexY                  = iter("McEvent.mPrimaryVertexY");
const Float_t*&    McEvent_mPrimaryVertexZ                  = iter("McEvent.mPrimaryVertexZ");
const Int_t*&      McEvent_mGlobalTracks                    = iter("McEvent.mGlobalTracks");
const Int_t*&      McEvent_mPrimaryTracks                   = iter("McEvent.mPrimaryTracks");
const Int_t*&      McEvent_mPrimaryNegTracks                = iter("McEvent.mPrimaryNegTracks");
const Float_t*&    McEvent_mMagneticField                   = iter("McEvent.mMagneticField");
const UInt_t*&     McEvent_mL0TriggerWord                   = iter("McEvent.mL0TriggerWord");
#endif /* __McEvent__ */
#ifdef __Strange__
#ifdef __V0__
const Int_t&       NoV0                                      = iter("V0");
const Float_t*&    V0_mDecayVertexV0X                       = iter("V0.mDecayVertexV0X");
const Float_t*&    V0_mDecayVertexV0Y                       = iter("V0.mDecayVertexV0Y");
const Float_t*&    V0_mDecayVertexV0Z                       = iter("V0.mDecayVertexV0Z");
const Float_t*&    V0_mDcaV0Daughters                       = iter("V0.mDcaV0Daughters");
const Float_t*&    V0_mDcaV0ToPrimVertex                    = iter("V0.mDcaV0ToPrimVertex");
const Float_t*&    V0_mDcaPosToPrimVertex                   = iter("V0.mDcaPosToPrimVertex");
const Float_t*&    V0_mDcaNegToPrimVertex                   = iter("V0.mDcaNegToPrimVertex");
const Float_t*&    V0_mMomPosX                              = iter("V0.mMomPosX");
const Float_t*&    V0_mMomPosY                              = iter("V0.mMomPosY");
const Float_t*&    V0_mMomPosZ                              = iter("V0.mMomPosZ");
const Float_t*&    V0_mMomNegX                              = iter("V0.mMomNegX");
const Float_t*&    V0_mMomNegY                              = iter("V0.mMomNegY");
const Float_t*&    V0_mMomNegZ                              = iter("V0.mMomNegZ");
	const Int_t*&      V0_mKeyPos                               = iter("V0.mKeyPos");
	const Int_t*&      V0_mKeyNeg                               = iter("V0.mKeyNeg");
const UInt_t*&     V0_mTopologyMapPos_mMap0                 = iter("V0.mTopologyMapPos.mMap0");
const UInt_t*&     V0_mTopologyMapPos_mMap1                 = iter("V0.mTopologyMapPos.mMap1");
const UInt_t*&     V0_mTopologyMapNeg_mMap0                 = iter("V0.mTopologyMapNeg.mMap0");
const UInt_t*&     V0_mTopologyMapNeg_mMap1                 = iter("V0.mTopologyMapNeg.mMap1");
const Float_t*&    V0_mChi2V0                               = iter("V0.mChi2V0");
const Float_t*&    V0_mClV0                                 = iter("V0.mClV0");
const Float_t*&    V0_mChi2Pos                              = iter("V0.mChi2Pos");
const Float_t*&    V0_mClPos                                = iter("V0.mClPos");
const Float_t*&    V0_mChi2Neg                              = iter("V0.mChi2Neg");
const Float_t*&    V0_mClNeg                                = iter("V0.mClNeg");
const Float_t*&    V0_mDedxPos                              = iter("V0.mDedxPos");
const Float_t*&    V0_mDedxNeg                              = iter("V0.mDedxNeg");
const Float_t*&    V0_mErrDedxPos                           = iter("V0.mErrDedxPos");
const Float_t*&    V0_mErrDedxNeg                           = iter("V0.mErrDedxNeg");
const UShort_t*&   V0_mNumDedxPos                           = iter("V0.mNumDedxPos");
const UShort_t*&   V0_mNumDedxNeg                           = iter("V0.mNumDedxNeg");
const Int_t&       NoMcV0                                    = iter("McV0");
const Int_t*&      McV0_mPositiveSimTpcHits                 = iter("McV0.mPositiveSimTpcHits");
const Int_t*&      McV0_mPositiveCommonTpcHits              = iter("McV0.mPositiveCommonTpcHits");
const Int_t*&      McV0_mNegativeSimTpcHits                 = iter("McV0.mNegativeSimTpcHits");
const Int_t*&      McV0_mNegativeCommonTpcHits              = iter("McV0.mNegativeCommonTpcHits");
const Int_t*&      McV0_mDecayMode                          = iter("McV0.mDecayMode");
const Int_t*&      McV0_mParentGeantId                      = iter("McV0.mParentGeantId");
const Int_t*&      McV0_mPositiveGeantId                    = iter("McV0.mPositiveGeantId");
const Int_t*&      McV0_mNegativeGeantId                    = iter("McV0.mNegativeGeantId");
const Float_t*&    McV0_mParentMomentumX                    = iter("McV0.mParentMomentumX");
const Float_t*&    McV0_mParentMomentumY                    = iter("McV0.mParentMomentumY");
const Float_t*&    McV0_mParentMomentumZ                    = iter("McV0.mParentMomentumZ");
const Float_t*&    McV0_mPositiveMomentumX                  = iter("McV0.mPositiveMomentumX");
const Float_t*&    McV0_mPositiveMomentumY                  = iter("McV0.mPositiveMomentumY");
const Float_t*&    McV0_mPositiveMomentumZ                  = iter("McV0.mPositiveMomentumZ");
const Float_t*&    McV0_mNegativeMomentumX                  = iter("McV0.mNegativeMomentumX");
const Float_t*&    McV0_mNegativeMomentumY                  = iter("McV0.mNegativeMomentumY");
const Float_t*&    McV0_mNegativeMomentumZ                  = iter("McV0.mNegativeMomentumZ");
const Float_t*&    McV0_mPositionX                          = iter("McV0.mPositionX");
const Float_t*&    McV0_mPositionY                          = iter("McV0.mPositionY");
const Float_t*&    McV0_mPositionZ                          = iter("McV0.mPositionZ");
const Int_t&       NoV0Assoc                                 = iter("V0Assoc");
const Int_t*&      V0Assoc_mIndexRecoArray                  = iter("V0Assoc.mIndexRecoArray");
const Int_t*&      V0Assoc_mIndexMcArray                    = iter("V0Assoc.mIndexMcArray");
#endif /* __V0__ */
#ifdef __Xi__
const Int_t&       NoXi                                      = iter("Xi");
const Float_t*&    Xi_mDecayVertexV0X                       = iter("Xi.mDecayVertexV0X");
const Float_t*&    Xi_mDecayVertexV0Y                       = iter("Xi.mDecayVertexV0Y");
const Float_t*&    Xi_mDecayVertexV0Z                       = iter("Xi.mDecayVertexV0Z");
const Float_t*&    Xi_mDcaV0Daughters                       = iter("Xi.mDcaV0Daughters");
const Float_t*&    Xi_mDcaV0ToPrimVertex                    = iter("Xi.mDcaV0ToPrimVertex");
const Float_t*&    Xi_mDcaPosToPrimVertex                   = iter("Xi.mDcaPosToPrimVertex");
const Float_t*&    Xi_mDcaNegToPrimVertex                   = iter("Xi.mDcaNegToPrimVertex");
const Float_t*&    Xi_mMomPosX                              = iter("Xi.mMomPosX");
const Float_t*&    Xi_mMomPosY                              = iter("Xi.mMomPosY");
const Float_t*&    Xi_mMomPosZ                              = iter("Xi.mMomPosZ");
const Float_t*&    Xi_mMomNegX                              = iter("Xi.mMomNegX");
const Float_t*&    Xi_mMomNegY                              = iter("Xi.mMomNegY");
const Float_t*&    Xi_mMomNegZ                              = iter("Xi.mMomNegZ");
	const Int_t*&      Xi_mKeyPos                               = iter("Xi.mKeyPos");
	const Int_t*&      Xi_mKeyNeg                               = iter("Xi.mKeyNeg");
const UInt_t*&     Xi_mTopologyMapPos_mMap0                 = iter("Xi.mTopologyMapPos.mMap0");
const UInt_t*&     Xi_mTopologyMapPos_mMap1                 = iter("Xi.mTopologyMapPos.mMap1");
const UInt_t*&     Xi_mTopologyMapNeg_mMap0                 = iter("Xi.mTopologyMapNeg.mMap0");
const UInt_t*&     Xi_mTopologyMapNeg_mMap1                 = iter("Xi.mTopologyMapNeg.mMap1");
const Float_t*&    Xi_mChi2V0                               = iter("Xi.mChi2V0");
const Float_t*&    Xi_mClV0                                 = iter("Xi.mClV0");
const Float_t*&    Xi_mChi2Pos                              = iter("Xi.mChi2Pos");
const Float_t*&    Xi_mClPos                                = iter("Xi.mClPos");
const Float_t*&    Xi_mChi2Neg                              = iter("Xi.mChi2Neg");
const Float_t*&    Xi_mClNeg                                = iter("Xi.mClNeg");
const Float_t*&    Xi_mDedxPos                              = iter("Xi.mDedxPos");
const Float_t*&    Xi_mDedxNeg                              = iter("Xi.mDedxNeg");
const Float_t*&    Xi_mErrDedxPos                           = iter("Xi.mErrDedxPos");
const Float_t*&    Xi_mErrDedxNeg                           = iter("Xi.mErrDedxNeg");
const UShort_t*&   Xi_mNumDedxPos                           = iter("Xi.mNumDedxPos");
const UShort_t*&   Xi_mNumDedxNeg                           = iter("Xi.mNumDedxNeg");
const Int_t*&      Xi_mCharge                               = iter("Xi.mCharge");
const Float_t*&    Xi_mDecayVertexXiX                       = iter("Xi.mDecayVertexXiX");
const Float_t*&    Xi_mDecayVertexXiY                       = iter("Xi.mDecayVertexXiY");
const Float_t*&    Xi_mDecayVertexXiZ                       = iter("Xi.mDecayVertexXiZ");
const Float_t*&    Xi_mDcaXiDaughters                       = iter("Xi.mDcaXiDaughters");
const Float_t*&    Xi_mDcaBachelorToPrimVertex              = iter("Xi.mDcaBachelorToPrimVertex");
const Float_t*&    Xi_mDcaXiToPrimVertex                    = iter("Xi.mDcaXiToPrimVertex");
const Float_t*&    Xi_mMomBachelorX                         = iter("Xi.mMomBachelorX");
const Float_t*&    Xi_mMomBachelorY                         = iter("Xi.mMomBachelorY");
const Float_t*&    Xi_mMomBachelorZ                         = iter("Xi.mMomBachelorZ");
	const Int_t*&      Xi_mKeyBachelor                          = iter("Xi.mKeyBachelor");
const UInt_t*&     Xi_mTopologyMapBachelor_mMap0            = iter("Xi.mTopologyMapBachelor.mMap0");
const UInt_t*&     Xi_mTopologyMapBachelor_mMap1            = iter("Xi.mTopologyMapBachelor.mMap1");
const Float_t*&    Xi_mChi2Xi                               = iter("Xi.mChi2Xi");
const Float_t*&    Xi_mClXi                                 = iter("Xi.mClXi");
const Float_t*&    Xi_mChi2Bachelor                         = iter("Xi.mChi2Bachelor");
const Float_t*&    Xi_mClBachelor                           = iter("Xi.mClBachelor");
const Float_t*&    Xi_mDedxBachelor                         = iter("Xi.mDedxBachelor");
const Float_t*&    Xi_mErrDedxBachelor                      = iter("Xi.mErrDedxBachelor");
const UShort_t*&   Xi_mNumDedxBachelor                      = iter("Xi.mNumDedxBachelor");
const Int_t&       NoMcXi                                    = iter("McXi");
const Int_t*&      McXi_mParentGeantId                      = iter("McXi.mParentGeantId");
const Int_t*&      McXi_mDaughterGeantId                    = iter("McXi.mDaughterGeantId");
const Float_t*&    McXi_mParentMomentumX                    = iter("McXi.mParentMomentumX");
const Float_t*&    McXi_mParentMomentumY                    = iter("McXi.mParentMomentumY");
const Float_t*&    McXi_mParentMomentumZ                    = iter("McXi.mParentMomentumZ");
const Float_t*&    McXi_mParentPrimMomentumX                = iter("McXi.mParentPrimMomentumX");
const Float_t*&    McXi_mParentPrimMomentumY                = iter("McXi.mParentPrimMomentumY");
const Float_t*&    McXi_mParentPrimMomentumZ                = iter("McXi.mParentPrimMomentumZ");
const Float_t*&    McXi_mDaughterMomentumX                  = iter("McXi.mDaughterMomentumX");
const Float_t*&    McXi_mDaughterMomentumY                  = iter("McXi.mDaughterMomentumY");
const Float_t*&    McXi_mDaughterMomentumZ                  = iter("McXi.mDaughterMomentumZ");
const Float_t*&    McXi_mPositionX                          = iter("McXi.mPositionX");
const Float_t*&    McXi_mPositionY                          = iter("McXi.mPositionY");
const Float_t*&    McXi_mPositionZ                          = iter("McXi.mPositionZ");
const Int_t*&      McXi_mSimTpcHits                         = iter("McXi.mSimTpcHits");
const Int_t*&      McXi_mCommonTpcHits                      = iter("McXi.mCommonTpcHits");
const Int_t*&      McXi_mDecayMode                          = iter("McXi.mDecayMode");
const Int_t*&      McXi_v0                                  = iter("McXi.v0");
const Int_t&       NoXiAssoc                                 = iter("XiAssoc");
const Int_t*&      XiAssoc_mIndexRecoArray                  = iter("XiAssoc.mIndexRecoArray");
const Int_t*&      XiAssoc_mIndexMcArray                    = iter("XiAssoc.mIndexMcArray");
#endif /*  __Xi__ */
#ifdef __Kink__
const Int_t&       NoKink                                    = iter("Kink");
const Int_t*&      Kink_mParentGeantId                      = iter("Kink.mParentGeantId");
const Int_t*&      Kink_mDaughterGeantId                    = iter("Kink.mDaughterGeantId");
const Float_t*&    Kink_mParentMomentumX                    = iter("Kink.mParentMomentumX");
const Float_t*&    Kink_mParentMomentumY                    = iter("Kink.mParentMomentumY");
const Float_t*&    Kink_mParentMomentumZ                    = iter("Kink.mParentMomentumZ");
const Float_t*&    Kink_mParentPrimMomentumX                = iter("Kink.mParentPrimMomentumX");
const Float_t*&    Kink_mParentPrimMomentumY                = iter("Kink.mParentPrimMomentumY");
const Float_t*&    Kink_mParentPrimMomentumZ                = iter("Kink.mParentPrimMomentumZ");
const Float_t*&    Kink_mDaughterMomentumX                  = iter("Kink.mDaughterMomentumX");
const Float_t*&    Kink_mDaughterMomentumY                  = iter("Kink.mDaughterMomentumY");
const Float_t*&    Kink_mDaughterMomentumZ                  = iter("Kink.mDaughterMomentumZ");
const Float_t*&    Kink_mPositionX                          = iter("Kink.mPositionX");
const Float_t*&    Kink_mPositionY                          = iter("Kink.mPositionY");
const Float_t*&    Kink_mPositionZ                          = iter("Kink.mPositionZ");
const Float_t*&    Kink_mDcaParentDaughter                  = iter("Kink.mDcaParentDaughter");
const Float_t*&    Kink_mDcaDaughterPrimaryVertex           = iter("Kink.mDcaDaughterPrimaryVertex");
const Float_t*&    Kink_mDcaParentPrimaryVertex             = iter("Kink.mDcaParentPrimaryVertex");
const Float_t*&    Kink_mHitDistanceParentDaughter          = iter("Kink.mHitDistanceParentDaughter");
const Float_t*&    Kink_mHitDistanceParentVertex            = iter("Kink.mHitDistanceParentVertex");
const Float_t*&    Kink_mMinDeltaEnergy                     = iter("Kink.mMinDeltaEnergy");
const Float_t*&    Kink_mDecayAngle                         = iter("Kink.mDecayAngle");
const Float_t*&    Kink_mParentMomentum                     = iter("Kink.mParentMomentum");
const Float_t*&    Kink_mParentPrimMomentum                 = iter("Kink.mParentPrimMomentum");
const Int_t*&      Kink_mParentCharge                       = iter("Kink.mParentCharge");
const Float_t*&    Kink_mDaughterMomentum                   = iter("Kink.mDaughterMomentum");
const Int_t*&      Kink_mDaughterCharge                     = iter("Kink.mDaughterCharge");
const Float_t*&    Kink_mDecayLength                        = iter("Kink.mDecayLength");
const Float_t*&    Kink_mTransverseMomentum                 = iter("Kink.mTransverseMomentum");
const Float_t*&    Kink_mTransverseMassKaon                 = iter("Kink.mTransverseMassKaon");
const Float_t*&    Kink_mTransverseMassPion                 = iter("Kink.mTransverseMassPion");
const Float_t*&    Kink_mRapidityKaon                       = iter("Kink.mRapidityKaon");
const Float_t*&    Kink_mRapidityPion                       = iter("Kink.mRapidityPion");
const Float_t*&    Kink_mChi2Kink                           = iter("Kink.mChi2Kink");
const Float_t*&    Kink_mClKink                             = iter("Kink.mClKink");
const Float_t*&    Kink_mChi2Parent                         = iter("Kink.mChi2Parent");
const Float_t*&    Kink_mClParent                           = iter("Kink.mClParent");
const Float_t*&    Kink_mChi2Daughter                       = iter("Kink.mChi2Daughter");
const Float_t*&    Kink_mClDaughter                         = iter("Kink.mClDaughter");
const Float_t*&    Kink_mDedxParent                         = iter("Kink.mDedxParent");
const Float_t*&    Kink_mDedxDaughter                       = iter("Kink.mDedxDaughter");
const Float_t*&    Kink_mErrDedxParent                      = iter("Kink.mErrDedxParent");
const Float_t*&    Kink_mErrDedxDaughter                    = iter("Kink.mErrDedxDaughter");
const UShort_t*&   Kink_mNumDedxParent                      = iter("Kink.mNumDedxParent");
const UShort_t*&   Kink_mNumDedxDaughter                    = iter("Kink.mNumDedxDaughter");
	const Int_t*&      Kink_mKeyParent                          = iter("Kink.mKeyParent");
	const Int_t*&      Kink_mKeyDaughter                        = iter("Kink.mKeyDaughter");
	const Int_t&       NoMcKink                                 = iter("McKink");
const Int_t*&      McKink_mParentGeantId                    = iter("McKink.mParentGeantId");
const Int_t*&      McKink_mDaughterGeantId                  = iter("McKink.mDaughterGeantId");
const Float_t*&    McKink_mParentMomentumX                  = iter("McKink.mParentMomentumX");
const Float_t*&    McKink_mParentMomentumY                  = iter("McKink.mParentMomentumY");
const Float_t*&    McKink_mParentMomentumZ                  = iter("McKink.mParentMomentumZ");
const Float_t*&    McKink_mParentPrimMomentumX              = iter("McKink.mParentPrimMomentumX");
const Float_t*&    McKink_mParentPrimMomentumY              = iter("McKink.mParentPrimMomentumY");
const Float_t*&    McKink_mParentPrimMomentumZ              = iter("McKink.mParentPrimMomentumZ");
const Float_t*&    McKink_mDaughterMomentumX                = iter("McKink.mDaughterMomentumX");
const Float_t*&    McKink_mDaughterMomentumY                = iter("McKink.mDaughterMomentumY");
const Float_t*&    McKink_mDaughterMomentumZ                = iter("McKink.mDaughterMomentumZ");
const Float_t*&    McKink_mPositionX                        = iter("McKink.mPositionX");
const Float_t*&    McKink_mPositionY                        = iter("McKink.mPositionY");
const Float_t*&    McKink_mPositionZ                        = iter("McKink.mPositionZ");
const Int_t*&      McKink_mSimTpcHits                       = iter("McKink.mSimTpcHits");
const Int_t*&      McKink_mCommonTpcHits                    = iter("McKink.mCommonTpcHits");
const Int_t*&      McKink_mDecayMode                        = iter("McKink.mDecayMode");
const Int_t&       NoKinkAssoc                               = iter("KinkAssoc");
const Int_t*&      KinkAssoc_mIndexRecoArray                = iter("KinkAssoc.mIndexRecoArray");
const Int_t*&      KinkAssoc_mIndexMcArray                  = iter("KinkAssoc.mIndexMcArray");
#endif /* __Kink__ */
const Int_t&       NoStrangeCuts                             = iter("StrangeCuts");
const TString*&    StrangeCuts_fName                        = iter("StrangeCuts.fName");
const TString*&    StrangeCuts_fTitle                       = iter("StrangeCuts.fTitle");
#endif /* __Strange__ */
#ifdef __MuMc__
	const Int_t&       StMuMcVertex_                            = iter("StMuMcVertex");
	const Int_t*&      StMuMcVertex_mId                         = iter("StMuMcVertex.mId");
	const Int_t*&      StMuMcVertex_mNoDaughters                = iter("StMuMcVertex.mNoDaughters");
	const Int_t*&      StMuMcVertex_mIdParTrk                   = iter("StMuMcVertex.mIdParTrk");
	const Int_t*&      StMuMcVertex_mIsInterm                   = iter("StMuMcVertex.mIsInterm");
	const Float_t*&    StMuMcVertex_mTime                       = iter("StMuMcVertex.mTime");
	const Float_t*&    StMuMcVertex_mXyzV_mX1                   = iter("StMuMcVertex.mXyzV.mX1");
	const Float_t*&    StMuMcVertex_mXyzV_mX2                   = iter("StMuMcVertex.mXyzV.mX2");
	const Float_t*&    StMuMcVertex_mXyzV_mX3                   = iter("StMuMcVertex.mXyzV.mX3");
	const Int_t&       StMuMcTrack_                             = iter("StMuMcTrack");
	const Int_t*&      StMuMcTrack_mGePid                       = iter("StMuMcTrack.mGePid");
	const Int_t*&      StMuMcTrack_mId                          = iter("StMuMcTrack.mId");
	const Char_t*&     StMuMcTrack_mIsShower                    = iter("StMuMcTrack.mIsShower");
	const UChar_t*&    StMuMcTrack_mHits                        = iter("StMuMcTrack.mHits[22]");
	const Int_t*&      StMuMcTrack_mItrmdVertex                 = iter("StMuMcTrack.mItrmdVertex");
	const Int_t*&      StMuMcTrack_mIdVx                        = iter("StMuMcTrack.mIdVx");
	const Int_t*&      StMuMcTrack_mIdVxEnd                     = iter("StMuMcTrack.mIdVxEnd");
	const Char_t*&     StMuMcTrack_mCharge                      = iter("StMuMcTrack.mCharge");
	const Float_t*&    StMuMcTrack_mE                           = iter("StMuMcTrack.mE");
	const Float_t*&    StMuMcTrack_mEta                         = iter("StMuMcTrack.mEta");
	const Float_t*&    StMuMcTrack_mPxyz_mX1                    = iter("StMuMcTrack.mPxyz.mX1");
	const Float_t*&    StMuMcTrack_mPxyz_mX2                    = iter("StMuMcTrack.mPxyz.mX2");
	const Float_t*&    StMuMcTrack_mPxyz_mX3                    = iter("StMuMcTrack.mPxyz.mX3");
	const Float_t*&    StMuMcTrack_mpT                          = iter("StMuMcTrack.mpT");
	const Float_t*&    StMuMcTrack_mPtot                        = iter("StMuMcTrack.mPtot");
	const Float_t*&    StMuMcTrack_mRapidity                    = iter("StMuMcTrack.mRapidity");
#endif /* __MuMc__ */
#ifdef __Emc__
const Int_t&       NoEmcTow                                  = iter("EmcTow");
const UShort_t*&   EmcTow_mTowerADC                         = iter("EmcTow.mTowerADC[4800]");
const UShort_t*&   EmcTow_mEndcapTowerADC                   = iter("EmcTow.mEndcapTowerADC[720]");
const UChar_t*&    EmcTow_mBTowCrateFlags                   = iter("EmcTow.mBTowCrateFlags[30]");
const UChar_t*&    EmcTow_mBSmdCrateFlags                   = iter("EmcTow.mBSmdCrateFlags[8]");
const UChar_t*&    EmcTow_mBPrsCrateFlags                   = iter("EmcTow.mBPrsCrateFlags[4]");
const UChar_t*&    EmcTow_mETowCrateFlags                   = iter("EmcTow.mETowCrateFlags[6]");
const UChar_t*&    EmcTow_mESmdCrateFlags                   = iter("EmcTow.mESmdCrateFlags[36]");
const UChar_t*&    EmcTow_mEPrsCrateFlags                   = iter("EmcTow.mEPrsCrateFlags[12]");
const Int_t&       NoEmcPrs                                  = iter("EmcPrs");
const Float_t*&    EmcPrs_mEnergy                           = iter("EmcPrs.mEnergy");
const Short_t*&    EmcPrs_mId                               = iter("EmcPrs.mId");
const Short_t*&    EmcPrs_mAdc                              = iter("EmcPrs.mAdc");
const Char_t*&     EmcPrs_mCalType                          = iter("EmcPrs.mCalType");
const Int_t&       NoEmcSmde                                 = iter("EmcSmde");
const Float_t*&    EmcSmde_mEnergy                          = iter("EmcSmde.mEnergy");
const Short_t*&    EmcSmde_mId                              = iter("EmcSmde.mId");
const Short_t*&    EmcSmde_mAdc                             = iter("EmcSmde.mAdc");
const Char_t*&     EmcSmde_mCalType                         = iter("EmcSmde.mCalType");
const Int_t&       NoEmcSmdp                                 = iter("EmcSmdp");
const Float_t*&    EmcSmdp_mEnergy                          = iter("EmcSmdp.mEnergy");
const Short_t*&    EmcSmdp_mId                              = iter("EmcSmdp.mId");
const Short_t*&    EmcSmdp_mAdc                             = iter("EmcSmdp.mAdc");
const Char_t*&     EmcSmdp_mCalType                         = iter("EmcSmdp.mCalType");
const Int_t&       NoEEmcPrs                                 = iter("EEmcPrs");
const Float_t*&    EEmcPrs_mEnergy                          = iter("EEmcPrs.mEnergy");
const Short_t*&    EEmcPrs_mId                              = iter("EEmcPrs.mId");
const Short_t*&    EEmcPrs_mAdc                             = iter("EEmcPrs.mAdc");
const Char_t*&     EEmcPrs_mCalType                         = iter("EEmcPrs.mCalType");
const Int_t&       NoEEmcSmdu                                = iter("EEmcSmdu");
const Float_t*&    EEmcSmdu_mEnergy                         = iter("EEmcSmdu.mEnergy");
const Short_t*&    EEmcSmdu_mId                             = iter("EEmcSmdu.mId");
const Short_t*&    EEmcSmdu_mAdc                            = iter("EEmcSmdu.mAdc");
const Char_t*&     EEmcSmdu_mCalType                        = iter("EEmcSmdu.mCalType");
const Int_t&       NoEEmcSmdv                                = iter("EEmcSmdv");
const Float_t*&    EEmcSmdv_mEnergy                         = iter("EEmcSmdv.mEnergy");
const Short_t*&    EEmcSmdv_mId                             = iter("EEmcSmdv.mId");
const Short_t*&    EEmcSmdv_mAdc                            = iter("EEmcSmdv.mAdc");
const Char_t*&     EEmcSmdv_mCalType                        = iter("EEmcSmdv.mCalType");
const Int_t&       NoPmdHit                                  = iter("PmdHit");
const Short_t*&    PmdHit_mSuperModule                      = iter("PmdHit.mSuperModule");
const Short_t*&    PmdHit_mSubDetector                      = iter("PmdHit.mSubDetector");
const Short_t*&    PmdHit_mRow                              = iter("PmdHit.mRow");
const Short_t*&    PmdHit_mCol                              = iter("PmdHit.mCol");
const Float_t*&    PmdHit_mEnergy                           = iter("PmdHit.mEnergy");
const Int_t*&      PmdHit_mADC                              = iter("PmdHit.mADC");
const Int_t&       NoCpvHit                                  = iter("CpvHit");
const Short_t*&    CpvHit_mSuperModule                      = iter("CpvHit.mSuperModule");
const Short_t*&    CpvHit_mSubDetector                      = iter("CpvHit.mSubDetector");
const Short_t*&    CpvHit_mRow                              = iter("CpvHit.mRow");
const Short_t*&    CpvHit_mCol                              = iter("CpvHit.mCol");
const Float_t*&    CpvHit_mEnergy                           = iter("CpvHit.mEnergy");
const Int_t*&      CpvHit_mADC                              = iter("CpvHit.mADC");
const Int_t&       NoPmdCluster                              = iter("PmdCluster");
const Int_t*&      PmdCluster_mSuperModule                  = iter("PmdCluster.mSuperModule");
const Int_t*&      PmdCluster_mNcell                        = iter("PmdCluster.mNcell");
const Float_t*&    PmdCluster_mEta                          = iter("PmdCluster.mEta");
const Float_t*&    PmdCluster_mPhi                          = iter("PmdCluster.mPhi");
const Float_t*&    PmdCluster_mSigma                        = iter("PmdCluster.mSigma");
const Float_t*&    PmdCluster_mEnergy                       = iter("PmdCluster.mEnergy");
const Int_t*&      PmdCluster_mEnergyPID                    = iter("PmdCluster.mEnergyPID");
const Int_t*&      PmdCluster_mPID                          = iter("PmdCluster.mPID");
const Int_t*&      PmdCluster_mMcPID                        = iter("PmdCluster.mMcPID");
const Int_t&       NoCpvCluster                             = iter("CpvCluster");
const Int_t*&      CpvCluster_mSuperModule                  = iter("CpvCluster.mSuperModule");
const Int_t*&      CpvCluster_mNcell                        = iter("CpvCluster.mNcell");
const Float_t*&    CpvCluster_mEta                          = iter("CpvCluster.mEta");
const Float_t*&    CpvCluster_mPhi                          = iter("CpvCluster.mPhi");
const Float_t*&    CpvCluster_mSigma                        = iter("CpvCluster.mSigma");
const Float_t*&    CpvCluster_mEnergy                       = iter("CpvCluster.mEnergy");
const Int_t*&      CpvCluster_mEnergyPID                    = iter("CpvCluster.mEnergyPID");
const Int_t*&      CpvCluster_mPID                          = iter("CpvCluster.mPID");
const Int_t*&      CpvCluster_mMcPID                        = iter("CpvCluster.mMcPID");
#endif /* __Emc__ */
#ifdef __Fmc__
	const Int_t&       FmsHit_                                  = iter("FmsHit");
	const UShort_t*&   FmsHit_mDetectorId                       = iter("FmsHit.mDetectorId");
	const UShort_t*&   FmsHit_mChannel                          = iter("FmsHit.mChannel");
	const UShort_t*&   FmsHit_mQTCrtSlotCh                      = iter("FmsHit.mQTCrtSlotCh");
	const UShort_t*&   FmsHit_mAdc                              = iter("FmsHit.mAdc");
	const UShort_t*&   FmsHit_mTdc                              = iter("FmsHit.mTdc");
	const Float_t*&    FmsHit_mEnergy                           = iter("FmsHit.mEnergy");
#endif /* __Fmc__ */
#ifdef __Tof__
const Int_t&       NoTofHit                                  = iter("TofHit");
const Int_t*&      TofHit_mIconf                            = iter("TofHit.mIconf");
const Int_t*&      TofHit_mTrayIndex                        = iter("TofHit.mTrayIndex");
const Int_t*&      TofHit_mModuleIndex                      = iter("TofHit.mModuleIndex");
const Int_t*&      TofHit_mCellIndex                        = iter("TofHit.mCellIndex");
const Int_t*&      TofHit_mDaqIndex                         = iter("TofHit.mDaqIndex");
const Int_t*&      TofHit_mADC                              = iter("TofHit.mADC");
const Float_t*&    TofHit_mTimeOfFlight                     = iter("TofHit.mTimeOfFlight");
const Float_t*&    TofHit_mPathLength                       = iter("TofHit.mPathLength");
const Float_t*&    TofHit_mBeta                             = iter("TofHit.mBeta");
const Int_t*&      TofHit_mAssociatedTrackId                = iter("TofHit.mAssociatedTrackId");
const Float_t*&    TofHit_mProjectedPoint_mX1               = iter("TofHit.mProjectedPoint.mX1");
const Float_t*&    TofHit_mProjectedPoint_mX2               = iter("TofHit.mProjectedPoint.mX2");
const Float_t*&    TofHit_mProjectedPoint_mX3               = iter("TofHit.mProjectedPoint.mX3");
const Float_t*&    TofHit_mTOFExpectedAsElectron            = iter("TofHit.mTOFExpectedAsElectron");
const Float_t*&    TofHit_mTOFExpectedAsPion                = iter("TofHit.mTOFExpectedAsPion");
const Float_t*&    TofHit_mTOFExpectedAsKaon                = iter("TofHit.mTOFExpectedAsKaon");
const Float_t*&    TofHit_mTOFExpectedAsProton              = iter("TofHit.mTOFExpectedAsProton");
const Float_t*&    TofHit_mSigmaElectron                    = iter("TofHit.mSigmaElectron");
const Float_t*&    TofHit_mSigmaPion                        = iter("TofHit.mSigmaPion");
const Float_t*&    TofHit_mSigmaKaon                        = iter("TofHit.mSigmaKaon");
const Float_t*&    TofHit_mSigmaProton                      = iter("TofHit.mSigmaProton");
const Int_t*&      TofHit_mParticleHypothesis               = iter("TofHit.mParticleHypothesis");
const Int_t&       NoTofData                                 = iter("TofData");
const UShort_t*&   TofData_mDataIndex                       = iter("TofData.mDataIndex");
const UShort_t*&   TofData_mAdc                             = iter("TofData.mAdc");
const UShort_t*&   TofData_mTdc                             = iter("TofData.mTdc");
const Short_t*&    TofData_mTc                              = iter("TofData.mTc");
const UShort_t*&   TofData_mSc                              = iter("TofData.mSc");
const UInt_t*&     TofData_mLeadingTdc                      = iter("TofData.mLeadingTdc");
const UInt_t*&     TofData_mTrailingTdc                     = iter("TofData.mTrailingTdc");
const Int_t&       NoTofRawData                              = iter("TofRawData");
const UShort_t*&   TofRawData_mLeTeFlag                     = iter("TofRawData.mLeTeFlag");
const UShort_t*&   TofRawData_mTray                         = iter("TofRawData.mTray");
const UShort_t*&   TofRawData_mChannel                      = iter("TofRawData.mChannel");
const UInt_t*&     TofRawData_mTdc                          = iter("TofRawData.mTdc");
const UInt_t*&     TofRawData_mTriggertime                  = iter("TofRawData.mTriggertime");
const UShort_t*&   TofRawData_mQuality                      = iter("TofRawData.mQuality");
	const Int_t&       BTofHit_                                 = iter("BTofHit");
	const UChar_t*&    BTofHit_mTray                            = iter("BTofHit.mTray");
	const UChar_t*&    BTofHit_mModule                          = iter("BTofHit.mModule");
	const UChar_t*&    BTofHit_mCell                            = iter("BTofHit.mCell");
	const Double_t*&   BTofHit_mLeadingEdgeTime                 = iter("BTofHit.mLeadingEdgeTime");
	const Double_t*&   BTofHit_mTrailingEdgeTime                = iter("BTofHit.mTrailingEdgeTime");
	const Short_t*&    BTofHit_mAssociatedTrackId               = iter("BTofHit.mAssociatedTrackId");
	const Int_t*&      BTofHit_mIndex2Primary                   = iter("BTofHit.mIndex2Primary");
	const Int_t*&      BTofHit_mIndex2Global                    = iter("BTofHit.mIndex2Global");
	const Int_t*&   BTofHit_mIdTruth                         = iter("BTofHit.mIdTruth");
	const Int_t*&   BTofHit_mQuality                         = iter("BTofHit.mQuality");
	const Int_t&       BTofRawHit_                              = iter("BTofRawHit");
	const Char_t*&     BTofRawHit_mFlag                         = iter("BTofRawHit.mFlag");
	const UChar_t*&    BTofRawHit_mTray                         = iter("BTofRawHit.mTray");
	const UChar_t*&    BTofRawHit_mChannel                      = iter("BTofRawHit.mChannel");
	const UInt_t*&     BTofRawHit_mTdc                          = iter("BTofRawHit.mTdc");
	const Int_t&       BTofHeader_                              = iter("BTofHeader");
	const Short_t*&    BTofHeader_mFiberHeader                  = iter("BTofHeader.mFiberHeader[4]");
	const UInt_t*&     BTofHeader_mFiberTriggerWord             = iter("BTofHeader.mFiberTriggerWord[4]");
	const UInt_t*&     BTofHeader_mVpdHitPattern                = iter("BTofHeader.mVpdHitPattern[2]");
	const Float_t*&    BTofHeader_mVpdVz                        = iter("BTofHeader.mVpdVz[20]");
	const Double_t*&   BTofHeader_mTStart                       = iter("BTofHeader.mTStart");
	const Double_t*&   BTofHeader_mTStartErr                    = iter("BTofHeader.mTStartErr");
	const Double_t*&   BTofHeader_mTDiff                        = iter("BTofHeader.mTDiff");
	const Double_t*&   BTofHeader_mVpdTime                      = iter("BTofHeader.mVpdTime[2][19]");
	const UInt_t*&     BTofHeader_mTriggerTime                  = iter("BTofHeader.mTriggerTime[4]");
	const Short_t*&    BTofHeader_mNTzero                       = iter("BTofHeader.mNTzero");
#endif /* __Tof__ */
#endif /* __MuDstIter_H__ */
