/***************************************************************************
*
* $Id: StIstRawHitMaker.cxx,v 1.32 2015/08/03 14:26:03 smirnovd Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description:
* See header file.
***************************************************************************/

#include "StIstRawHitMaker.h"

#include "StEvent.h"
#include "StRoot/St_base/StMessMgr.h"
#include "RTS/src/DAQ_FGT/daq_fgt.h"
#include "RTS/src/DAQ_READER/daq_dta.h"
#include "StChain/StRtsTable.h"

#include "StRoot/StIstUtil/StIstCollection.h"
#include "StRoot/StIstUtil/StIstRawHitCollection.h"
#include "StRoot/StIstUtil/StIstRawHit.h"
#include "StRoot/StIstDbMaker/StIstDb.h"
#include "StRoot/StIstUtil/StIstConsts.h"

#include "tables/St_istPedNoise_Table.h"
#include "tables/St_istGain_Table.h"
#include "tables/St_istMapping_Table.h"
#include "tables/St_istControl_Table.h"
#include "tables/St_istChipConfig_Table.h"

#include <string.h>
#include <time.h>

StIstRawHitMaker::StIstRawHitMaker( const char *name ): StRTSBaseMaker( "ist", name ), mIsCaliMode(false), mDoCmnCorrection(0), mIstCollectionPtr(0), mDataType(2)
{
   // set all vectors to zeros
   mCmnVec.resize( kIstNumApvs );
   mPedVec.resize( kIstNumElecIds );
   mRmsVec.resize( kIstNumElecIds );
   mGainVec.resize( kIstNumElecIds );
   mMappingVec.resize( kIstNumElecIds );
   mConfigVec.resize( kIstNumApvs, 1 );
};

StIstRawHitMaker::~StIstRawHitMaker()
{
   delete mIstCollectionPtr; mIstCollectionPtr = 0;
};


/*!
 * Init(): prepare the IST raw hit collection
 * in the dataset m_DataSet (data member of StMaker)
 */
Int_t StIstRawHitMaker::Init()
{
   LOG_INFO << "Initializing StIstRawHitMaker ..." << endm;
   Int_t ierr = kStOk;

   //prepare output data collection
   m_DataSet = new TObjectSet("istRawHitAndCluster");

   mIstCollectionPtr = new StIstCollection();
   ((TObjectSet *) m_DataSet)->AddObject(mIstCollectionPtr);

   if ( ierr || !mIstCollectionPtr ) {
      LOG_WARN << "Error constructing istCollection" << endm;
      ierr = kStWarn;
   }

   return ierr;
};


/*!
 * InitRun(): access IST calibration DB and retrieve the calibration information
 * from Db tables
 */
Int_t StIstRawHitMaker::InitRun(Int_t runnumber)
{
   Int_t ierr = kStOk;

   TObjectSet *istDbDataSet = (TObjectSet *)GetDataSet("ist_db");
   StIstDb *mIstDb = NULL;

   if (istDbDataSet) {
      mIstDb = (StIstDb *)istDbDataSet->GetObject();
      assert(mIstDb);
   }
   else {
      LOG_ERROR << "InitRun : no istDb" << endm;
      return kStErr;
   }

   // IST control parameters
   const istControl_st *istControlTable = mIstDb->getControl() ;

   if (!istControlTable)  {
      LOG_ERROR << "Pointer to IST control table is null" << endm;
      ierr = kStErr;
   }
   else {
      mHitCut  = istControlTable[0].kIstHitCutDefault;
      mCmnCut  = istControlTable[0].kIstCMNCutDefault;
      mChanMinRmsNoiseLevel = istControlTable[0].kIstChanMinRmsNoiseLevel;
      mChanMaxRmsNoiseLevel = istControlTable[0].kIstChanMaxRmsNoiseLevel;
      mApvMaxCmNoiseLevel   = istControlTable[0].kIstApvMaxCmNoiseLevel;
      mALLdata = istControlTable[0].kIstAlldata;
      mADCdata = istControlTable[0].kIstADCdata;
      mZSdata  = istControlTable[0].kIstZSdata;
      mDefaultTimeBin = istControlTable[0].kIstDefaultTimeBin;
      mCurrentTimeBinNum = istControlTable[0].kIstCurrentTimeBinNum;
      mMinNumOfRawHits = istControlTable[0].kIstMinNumOfRawHits;
      mMaxNumOfRawHits = istControlTable[0].kIstMaxNumOfRawHits;
   }

   // IST pedestal/rms table
   const istPedNoise_st *gPN = mIstDb->getPedNoise();

   if ( !gPN ) {
      LOG_ERROR << "Pointer to IST pedestal/noise table is null" << endm;
      ierr = kStErr;
   }
   else {
      for (int i = 0; i < kIstNumApvs; i++) {
         LOG_DEBUG << Form(" Print entry %d : CM noise=%f ", i, (float)gPN[0].cmNoise[i] / 100.) << endm;
         mCmnVec[i] = (float)gPN[0].cmNoise[i] / 100.0;
      }

      for (int i = 0; i < kIstNumElecIds; i++) {
         LOG_DEBUG << Form(" Print entry %d : pedestal=%f ", i, (float)gPN[0].pedestal[i]) << endm;
         mPedVec[i] = (float)gPN[0].pedestal[i];
      }

      for (int i = 0; i < kIstNumElecIds; i++) {
         LOG_DEBUG << Form(" Print entry %d : RMS noise=%f ", i, (float)gPN[0].rmsNoise[i] / 100.) << endm;
         mRmsVec[i] = (float)gPN[0].rmsNoise[i] / 100.;
      }
   }

   // IST gain table
   const istGain_st *gG = mIstDb->getGain();

   if ( !gG ) {
      LOG_WARN << "Pointer to IST gain table is null" << endm;
      ierr = kStWarn;
   }
   else {
      for (int i = 0; i < kIstNumElecIds; i++) {
         LOG_DEBUG << Form(" Print entry %d : gain=%f ", i, (float)gG[0].gain[i]) << endm;
         mGainVec[i] = (float)gG[0].gain[i];
      }
   }

   // IST mapping table
   const istMapping_st *gM = mIstDb->getMapping();

   if ( !gM ) {
      LOG_ERROR << "Pointer to IST mapping table is null" << endm;
      ierr = kStErr;
   }
   else {
      for (int i = 0; i < kIstNumElecIds; i++) {
         LOG_DEBUG << Form(" Print entry %d : geoId=%d ", i, gM[0].mapping[i]) << endm;
         mMappingVec[i] = gM[0].mapping[i];
      }
   }

   // IST chip configuration status table
   const istChipConfig_st *gCS = mIstDb->getChipStatus();

   if ( !gCS ) {
      LOG_ERROR << "Pointer to IST chip configuration table is null" << endm;
      ierr = kStErr;
   }
   else {
      if (runnumber == gCS[0].run) {
         for (int i = 0; i < kIstNumApvs; i++) {
            LOG_DEBUG << Form(" Print entry %d : status=%d ", i, gCS[0].s[i]) << endm;
            mConfigVec[i] = gCS[0].s[i];  /* 0 dead;  1 good; 2-9 reserved good status; 10 mis-configured */
         }
      }
   }

   return ierr;
};


/*!
 * Make(): main functional part of the raw hit maker, which contains the below three functions:
 * (1) un-pack the IST raw ADC data (ZS or non-ZS) from daq data via daq reader;
 * (2) pedestal subtraction for the non-ZS data and dynamical common-mode noise calculation;
 * (3) raw hit decision and ADC-to-dE/dx translation, then written to corresponding collection in physics mode;
 *     While in offline calibrarion mode, ADC information was directly saved without any raw hit processing.
 */
Int_t StIstRawHitMaker::Make()
{
   Int_t ierr = kStOk;

   StRtsTable *rts_tbl = 0;
   UChar_t dataFlag = mALLdata;
   Int_t ntimebin = mCurrentTimeBinNum;

   while (1) { //loops over input raw data
      if (dataFlag == mALLdata) {
         if (mDataType == mALLdata) {
            rts_tbl = GetNextDaqElement("ist/zs"); 	dataFlag = mZSdata;

            if (!rts_tbl) {
               LOG_WARN << "NO ZS-DATA BANK FOUND!!!" << endm;
               rts_tbl = GetNextDaqElement("ist/adc"); dataFlag = mADCdata;
            }
         }
         else if (mDataType == mADCdata) {
            rts_tbl = GetNextDaqElement("ist/adc"); 	dataFlag = mADCdata;
         }
         else if (mDataType == mZSdata) {
            rts_tbl = GetNextDaqElement("ist/zs");  	dataFlag = mZSdata;
         }
      }
      else if (dataFlag == mADCdata) { rts_tbl = GetNextDaqElement("ist/adc"); }
      else if (dataFlag == mZSdata) { rts_tbl = GetNextDaqElement("ist/zs"); }

      if (!rts_tbl) break;

      apv_meta_t *meta = (apv_meta_t *)rts_tbl->Meta();

      if (meta) {
         for (int r = 1; r <= kIstNumRdos; r++) { //6 rdos needed for whole IST detector
            if (meta->arc[r].present == 0) continue ;

            for (int arm = 0; arm < kIstNumArmsPerRdo; arm++) { //6 arms per arc
               if (meta->arc[r].arm[arm].present == 0) continue ;

               for (int apv = 0; apv < kIstNumApvsPerArm; apv++) { //24 apvs per arm
                  if (meta->arc[r].arm[arm].apv[apv].present == 0) continue ;

                  int nt = meta->arc[r].arm[arm].apv[apv].ntim;

                  if (ntimebin != 0 && nt != 0 && ntimebin != nt)
                     LOG_WARN << "Different number of timebins in different APV!!! Taking larger one!!!" << endm;

                  if (ntimebin < nt)
                     ntimebin = nt;
               }
            }
         }
      }

      mIstCollectionPtr->setNumTimeBins(ntimebin);

      // arrays to store ADC information per APV chip (128 channels over all time bins)
      Int_t signalUnCorrected[kIstNumApvChannels][kIstNumTimeBins];    //signal w/o pedestal subtracted
      Float_t signalCorrected[kIstNumApvChannels][kIstNumTimeBins];    //signal w/ pedestal subtracted

      for (int l = 0; l < kIstNumApvChannels; l++)    {
         for (int m = 0; m < kIstNumTimeBins; m++)    {
            signalUnCorrected[l][m]  = 0;
            signalCorrected[l][m]    = 0.;
         }
      }

      // arrays to calculate dynamical common mode noise contribution to the APV chip in current event
      Float_t cmNoisePerChip = 0.;                              //common mode noise of the APV chip
      Float_t sumAdcPerEvent[kIstNumTimeBins];
      Int_t counterAdcPerEvent[kIstNumTimeBins];

      //for (int n = 0; n < ntimebin; n++)  {
      for (int n = 0; n < kIstNumTimeBins; n++)  {
         sumAdcPerEvent[n]     = 0.;
         counterAdcPerEvent[n] = 0 ;
      }

      // electronics coordinate info.: RDO, ARM, APV
      Int_t rdo = rts_tbl->Rdo();     // 1, 2, ..., 6
      Int_t arm = rts_tbl->Sector();  // 0, 1, ..., 5
      Int_t apv = rts_tbl->Pad();     // 0, 1, ..., 23

      Int_t flag = 0;

      if (rdo < 1     || rdo >  kIstNumRdos)                 flag = 1;

      if (arm < 0     || arm >= kIstNumArmsPerRdo)           flag = 1;

      if (apv < 0     || apv >= kIstNumApvsPerArm)           flag = 1;

      if (flag == 1) {
         LOG_INFO << "Corrupt data  rdo: " << rdo << " arm: " << arm << " apv: " << apv << endm;
         continue;
      }

      // Loop over the data in this APV to get raw hit info. (channel, timebin, adc)
      for (StRtsTable::iterator it = rts_tbl->begin(); it != rts_tbl->end(); it++) {
         // channel info.
         fgt_adc_t *f = (fgt_adc_t *)*it;
         Int_t channel   = f->ch;  //channel index  0, 1, ..., 127
         Int_t adc       = f->adc; //adc
         Short_t timebin = f->tb;  //time bin
         LOG_DEBUG << "channel: " << channel << "   adc: " << adc << "  time bin: " << timebin << endm;

         flag = 0;

         if ((dataFlag == mADCdata) && (adc < 0 || adc >= kIstMaxAdc))	flag = 1;

         if (channel < 0 || channel >= kIstNumApvChannels)        	flag = 1;

         if (timebin < 0 || timebin >= ntimebin)           		flag = 1;

         if (flag == 1) {
            LOG_INFO << "Corrupt data channel: " << channel << " tbin: " << timebin << " adc: " << adc << endm;
            continue;
         }

         signalUnCorrected[channel][timebin] = adc;

         if ( !mIsCaliMode )        {
            Int_t elecId = (rdo - 1) * kIstNumArmsPerRdo * kIstNumApvsPerArm * kIstNumApvChannels + arm * kIstNumApvsPerArm * kIstNumApvChannels + apv * kIstNumApvChannels + channel; // 0, ..., 110591

            if (elecId < 0 || elecId >= kIstNumElecIds) {
               LOG_INFO << "Wrong elecId: " << elecId  << endm;
               continue;
            }

            if ( dataFlag == mADCdata ) { // non-ZS data
               signalCorrected[channel][timebin]    = (float)signalUnCorrected[channel][timebin] - mPedVec[elecId];

               // exclude signal-related channels for common mode noise calculation
               if ( (signalCorrected[channel][timebin] > (-mCmnCut)*mRmsVec[elecId]) && ( signalCorrected[channel][timebin] < mCmnCut * mRmsVec[elecId] ) )     {
                  sumAdcPerEvent[timebin] += signalCorrected[channel][timebin];
                  counterAdcPerEvent[timebin]++;
               }
            }
            else {	// ZS data
               signalCorrected[channel][timebin]    = (float)signalUnCorrected[channel][timebin];
            }
         }
      } // end current APV loops

      // calculate the dynamical common mode noise for the current chip in this event
      Float_t commonModeNoise[kIstNumTimeBins];

      for (int tbIdx = 0; tbIdx < kIstNumTimeBins; tbIdx++)
         commonModeNoise[tbIdx] = 0.;

      if ( !mIsCaliMode && dataFlag == mADCdata ) {
         for (short iTb = 0; iTb < ntimebin; iTb++)  {
            if (counterAdcPerEvent[iTb] > 0)
               commonModeNoise[iTb] = sumAdcPerEvent[iTb] / counterAdcPerEvent[iTb];
         }
      }

      // raw hit decision and channel counter passed the hit decision
      Bool_t isPassRawHitCut[kIstNumApvChannels];
      Int_t nChanPassedCut = 0;

      for (int iChan = 0; iChan < kIstNumApvChannels; iChan++) {
         isPassRawHitCut[iChan] = kFALSE;
      }

      for (int iChan = 0; iChan < kIstNumApvChannels; iChan++) {
         Int_t elecId = (rdo - 1) * kIstNumArmsPerRdo * kIstNumApvsPerArm * kIstNumApvChannels + arm * kIstNumApvsPerArm * kIstNumApvChannels + apv * kIstNumApvChannels + iChan;

         for (int iTB = 1; iTB < ntimebin - 1; iTB++)    {
            // raw hit decision: the method is stolen from Gerrit's ARMdisplay.C
            if ( (signalUnCorrected[iChan][iTB] > 0) && (signalUnCorrected[iChan][iTB] < kIstMaxAdc) &&
                  (signalCorrected[iChan][iTB - 1] > mHitCut * mRmsVec[elecId])     &&
                  (signalCorrected[iChan][iTB]   > mHitCut * mRmsVec[elecId])     &&
                  (signalCorrected[iChan][iTB + 1] > mHitCut * mRmsVec[elecId]) ) {

               isPassRawHitCut[iChan] = kTRUE;
               nChanPassedCut++;
               iTB = 999;
            }
         }
      }

      // skip the chip filling if the signal-channel number too large (20% chip occupancy was set) to exclude hot chip
      if ( !mIsCaliMode && (nChanPassedCut > mMaxNumOfRawHits || nChanPassedCut < mMinNumOfRawHits) ) {
         LOG_DEBUG << "Skip: The APV chip could be hot with " << nChanPassedCut << " channels fired!!" << endm;
         continue;
      }

      // fill IST raw hits for current APV chip
      for (int iChan = 0; iChan < kIstNumApvChannels; iChan++) {
         //mapping info.
         Int_t elecId = (rdo - 1) * kIstNumArmsPerRdo * kIstNumApvsPerArm * kIstNumApvChannels + arm * kIstNumApvsPerArm * kIstNumApvChannels + apv * kIstNumApvChannels + iChan;
         Int_t geoId  = mMappingVec[elecId]; // channel geometry ID which is numbering from 1 to 110592
         Int_t ladder = 1 + (geoId - 1) / (kIstApvsPerLadder * kIstNumApvChannels); // ladder geometry ID: 1, 2, ..., 24
         Int_t apvId  = 1 + (geoId - 1) / kIstNumApvChannels; // APV geometry ID: 1, ..., 864 (numbering from ladder 1 to ladder 24)
         cmNoisePerChip = mCmnVec[apvId - 1];

         //store raw hits information
         StIstRawHitCollection *rawHitCollectionPtr = mIstCollectionPtr->getRawHitCollection( ladder - 1 );

         if ( rawHitCollectionPtr ) {
            if ( mIsCaliMode ) { //calibration mode (non-ZS data): only write raw ADC value
               if (dataFlag == mADCdata) {
                  StIstRawHit *rawHitPtr = rawHitCollectionPtr->getRawHit( elecId );

                  for (int iTimeBin = 0; iTimeBin < ntimebin; iTimeBin++) {
                     rawHitPtr->setCharge( (float)signalUnCorrected[iChan][iTimeBin], (unsigned char)iTimeBin );
                  }

                  rawHitPtr->setChannelId( elecId );
                  rawHitPtr->setGeoId( geoId );
               }
               else continue;
            }
            else { //physics mode: pedestal subtracted + dynamical common mode correction
               //skip dead chips and bad mis-configured chips
               if (mConfigVec[apvId - 1] < 1 || mConfigVec[apvId - 1] > 9) { //1-9 good status code
                  LOG_DEBUG << "Skip: Channel belongs to dead/bad/mis-configured APV chip geometry index: " << apvId << " on ladder " << ladder << endm;
                  continue;
               }

               //skip current channel marked as suspicious status
               if (mRmsVec[elecId] < mChanMinRmsNoiseLevel || mRmsVec[elecId] > mChanMaxRmsNoiseLevel || mRmsVec[elecId] > 99.0)  {
                  LOG_DEBUG << "Skip: Noisy/hot/dead channel electronics index: " << elecId << endm;
                  continue;
               }

               if ( isPassRawHitCut[iChan] ) {
                  UChar_t tempMaxTB = -1;
                  Float_t tempMaxCharge = -999.0;

                  StIstRawHit *rawHitPtr = rawHitCollectionPtr->getRawHit( elecId );

                  for (int iTBin = 0; iTBin < ntimebin; iTBin++)      {
                     if ( mDoCmnCorrection && dataFlag == mADCdata )
                        signalCorrected[iChan][iTBin] -= commonModeNoise[iTBin];

                     if (signalCorrected[iChan][iTBin] < 0) signalCorrected[iChan][iTBin] = 0.1;

                     rawHitPtr->setCharge(signalCorrected[iChan][iTBin] * mGainVec[elecId], (unsigned char)iTBin );
                     rawHitPtr->setChargeErr(mRmsVec[elecId] * mGainVec[elecId], (unsigned char)iTBin);

                     if (signalCorrected[iChan][iTBin] > tempMaxCharge) {
                        tempMaxCharge = signalCorrected[iChan][iTBin];
                        tempMaxTB = (unsigned char)iTBin;
                     }
                  }

                  rawHitPtr->setChannelId( elecId );
                  rawHitPtr->setGeoId( geoId );
                  rawHitPtr->setMaxTimeBin( tempMaxTB );
                  rawHitPtr->setDefaultTimeBin( mDefaultTimeBin );
               }//end raw hit decision cut
            }//end filling hit info
         }
         else {
            LOG_WARN << "StIstRawHitMaker::Make() -- Could not access rawHitCollection for ladder " << ladder << endm;
         }
      } //end single APV chip hits filling
   }//end while

   return ierr;
};

void StIstRawHitMaker::Clear( Option_t *opts )
{
   if (mIstCollectionPtr ) {
      for ( unsigned char i = 0; i < kIstNumLadders; ++i ) {
         mIstCollectionPtr->getRawHitCollection(i)->Clear( "" );
      }
   }
};

ClassImp(StIstRawHitMaker);


/***************************************************************************
*
* $Log: StIstRawHitMaker.cxx,v $
* Revision 1.32  2015/08/03 14:26:03  smirnovd
* Corrected style with 'astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f'
*
* Revision 1.31  2015/02/27 15:48:34  ypwang
* Make sure the corrected ADC value is positive
*
* Revision 1.30  2014/10/14 21:06:15  smirnovd
* No need to empty vectors in destructor as they will be destructed anyway
*
* Revision 1.29  2014/10/14 21:06:08  smirnovd
* StIstRawHitMaker: Do not protect for possible double delete. This maker is solely responsible for the StIstCollection
*
* Revision 1.28  2014/09/20 08:14:19  ypwang
* update variable ntimebin from static to normal integer and related temporary array definitions
*
* Revision 1.27  2014/09/17 20:33:32  smirnovd
* Squashed commit of the following:
*
* commit 72dc19a6663ea31c719c1a61f6d2b4752dd766aa
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Wed Sep 17 12:34:42 2014 -0400
*
*     Minor code refactoring, clean up
*
* commit e083a10a9fb60b7dcce692ef8043b9227c12768b
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Wed Sep 17 12:18:16 2014 -0400
*
*     Removed pointless comments
*
* commit 88d51857362c91c954704cec4a31a0b0fa7fccc5
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Wed Sep 17 12:17:26 2014 -0400
*
*     Updated description in doxygen comments
*
* commit eb09527489179fc7dab6aa7f23fd132b25185bb1
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Tue Sep 9 15:15:56 2014 -0400
*
*     StIstScanClusterAlgo: Removed unused variable
*
* commit 1a8df63533c71a0e2ba4d8275ebf89f4e3004765
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Fri Aug 22 16:04:47 2014 -0400
*
*     Neatened headers: Removed unused, spelled paths in includes explicitly as it slightly helps in identifying dependencies
*
* commit 972e8ed41403bd680ade5ecc509f8bca004e86ee
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Wed Sep 17 12:34:20 2014 -0400
*
*     Minor stylistic changes
*
* commit 57daf5a1e0b3246fd12f1dd1c2ca089b62930c83
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Tue Sep 16 16:29:14 2014 -0400
*
*     Improved doxygen comments
*
* Revision 1.26  2014/09/08 19:29:40  smirnovd
* Corrected syntax of comments so they can be recognized by doxygen
*
* Revision 1.25  2014/09/07 08:02:18  ypwang
* doxygen comments were added for its main member functions
*
* Revision 1.24  2014/09/07 07:40:51  ypwang
* the mIstDb was declared as a local variable in InitRun() in stead of as a data member
*
* Revision 1.23  2014/09/07 07:30:03  ypwang
* the object mIstCollectionPtr was killed in the destructor
*
* Revision 1.22  2014/09/07 06:55:51  ypwang
* remove an unnecessary ierr cut in Make() function, and formatted with astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f
*
* Revision 1.21  2014/08/22 15:55:15  smirnovd
* Fixed style with astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f
*
* Revision 1.20  2014/08/21 17:51:08  smirnovd
* Moved CVS history to the end of file
*
* Revision 1.19  2014/08/12 23:00:02  ypwang
* chip occupancy cut added to skip the chip with more than 20% channels fired; change the raw hit decision cut position in the code.
*
* Revision 1.18  2014/08/12 17:39:17  ypwang
* clean several comment-out lines
*
* Revision 1.17  2014/08/06 18:56:53  ypwang
* minor update due to coding style update of the StIstDb method
*
* Revision 1.16  2014/08/04 17:12:48  ypwang
* update chip status Db table obtain method due to the table is populated run-by-run
*
* Revision 1.15  2014/07/29 20:13:31  ypwang
* update the IST DB obtain method
*
* Revision 1.14  2014/04/15 06:47:00  ypwang
* updates for collections clear due to Clear() function removed from StIstCollection
*
* Revision 1.13  2014/04/14 02:45:56  ypwang
* update LOG_ERROR to LOG_WARN for the case when real time bin number does not equal to the value from DB
*
* Revision 1.12  2014/03/25 03:06:53  ypwang
* updates on Db table accessory method
*
* Revision 1.11  2014/03/24 15:55:08  ypwang
* minor updates due to returned const pointers in StIstDbMaker
*
* Revision 1.10  2014/03/18 02:45:19  ypwang
* update raw hit decision algorithm: removed 1st time bin restriction cut
*
* Revision 1.9  2014/02/25 01:08:30  smirnovd
* Explicit pointer type conversion
*
* Revision 1.8  2014/02/25 01:07:02  smirnovd
* Minor pointer initialization and declaration fixes
*
* Revision 1.7  2014/02/19 06:26:14  ypwang
* update raw hit decision cuts to be compatible to ZS and non-ZS data
*
* Revision 1.6  2014/02/18 07:57:09  ypwang
* add setDefaultTimeBin() while filling raw hits information
*
* Revision 1.5  2014/02/15 19:55:25  ypwang
* remove virtual type declaration from member function
*
* Revision 1.4  2014/02/08 03:34:17  ypwang
* updating scripts
*
*
****************************************************************************
* StIstRawHitMaker.cxx,v 1.0
* Revision 1.0 2013/11/04 15:55:30 Yaping
* Initial version
****************************************************************************/
