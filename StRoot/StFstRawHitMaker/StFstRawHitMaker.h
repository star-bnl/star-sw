#ifndef StFstRawHitMaker_hh
#define StFstRawHitMaker_hh

#ifndef __CINT__
#include <array>
#include <iostream>
#endif

#include <vector>

#include "St_base/Stypes.h"
#include "StChain/StRTSBaseMaker.h"
#include "StEvent/StFstConsts.h"


class StEvent;
class StFstCollection;
class StFstEvtCollection;


/**
 * Reads/un-packs a DAQ or SFS file; Accesses calibration DBs;
 * Marks out bad channels/chips; Creates and fills the FST containers.
 * Two working modes included:
 * 1) calibration mode: save raw ADC value only
 * 2) non-calibration (physics) mode: access calibration datasets, signal-like
 *    raw hit decision, full raw hit info., ...
 *
 * \Author Shenghui Zhang
 * \date Aug 2021
 */
class StFstRawHitMaker : public StRTSBaseMaker
{
public:
   StFstRawHitMaker( const char *name = "fst_raw_hit" );
   ~StFstRawHitMaker();
   Int_t Init();
   Int_t InitRun(Int_t runNumber);
   Int_t Make();
   void Clear( Option_t *opts = "" );

   void setIsCalibrationMode( bool isCaliMode = false )  { mIsCaliMode = isCaliMode; }
   void setMinHitCut(double minhitCut = 2.5)             { mMinHitCut = minhitCut; }
   void setMedHitCut(double medhitCut = 3.5)             { mMedHitCut = medhitCut; }
   void setMaxHitCut(double maxhitCut = 4.0)             { mMaxHitCut = maxhitCut; }
   void setCmnCorrection( bool doCmnCorrection = true )	 { mDoCmnCorrection = doCmnCorrection; }
   void setCmnCut(double cmnCut = 3.)                    { mCmnCut = cmnCut; }
   /// 0 - All data; 1 - non-ZS data; 2 - ZS data; 3 - ZS first data
   void setDataType(int nDataType = 0)                   { mDataType = nDataType; };
   void setDoEmbedding(Bool_t doIt)                      { mDoEmbedding = doIt; }
   UChar_t getDataType() {return mDataType;} // 0 - All data; 1 - non-ZS data; 2 - ZS data; 3 - ZS first data

protected:
   Bool_t mIsCaliMode;
   Bool_t mDoEmbedding;
   Bool_t mDoCmnCorrection;
   //control paramters
   double mMinHitCut, mMedHitCut, mMaxHitCut, mCmnCut, mChanMinRmsNoiseLevel, mChanMaxRmsNoiseLevel, mApvMaxCmNoiseLevel;
   Int_t mALLdata, mADCdata, mZSdata, mDefaultTimeBin, mCurrentTimeBinNum;
   Int_t mMinNumOfRawHits, mMaxNumOfRawHits;

   /// Main output container with either corrected or uncorrected ADC values per
   /// channel. Usually the values come from real data DAQ records but can be
   /// merged with simulated hits or entirely provided by StFstSlowSimMaker
   StFstCollection *mFstCollectionPtr;

   /// Pointer to an FST hit container with simulated ADC values usually provided
   /// by StFstSlowSimMaker
   StFstCollection *mFstCollectionSimuPtr;

   std::vector< std::vector< std::vector< float > > > mCmnVec;   ///< APV chip geom. index, common mode (CM) noise
   std::vector< std::vector< float > > mPedVec;   ///< Channel elec. index, pedestal
   std::vector< std::vector< float > > mTotRmsVec;   ///< Channel elec. index, Total RMS noise
   std::vector< std::vector< float > > mRanRmsVec;   ///< Channel elec. index, Random RMS noise
   std::vector< float > mGainVec;  ///< Channel elec. index, gain
   std::vector< int > mMappingVec; ///< Channel elec. index to geometry ID mapping
   std::vector< int > mConfigVec; ///< APV chip configuration status indexed by geom. id

private:

   int FillRawHitCollectionFromAPVData(unsigned char dataFlag, int ntimebin, int counterAdcPerEvent[][kFstNumTimeBins], double sumAdcPerEvent[][kFstNumTimeBins], int apvElecId,
      std::array< std::array<double, kFstNumTimeBins>, kFstNumApvChannels > &signalUnCorrected,
      std::array< std::array<double, kFstNumTimeBins>, kFstNumApvChannels > &signalCorrected,
      std::array< std::array<int, kFstNumTimeBins>, kFstNumApvChannels > &seedFlag,
      std::array<int, kFstNumApvChannels> &idTruth);

   int FillRawHitCollectionFromSimData();

   StEvent* mEvent;
   StFstEvtCollection* mFstEvtCollection;

   Int_t mDataType; ///<  0=all, 1=adc only, 2=zs only
   
   ClassDef(StFstRawHitMaker, 1);
};

#endif
