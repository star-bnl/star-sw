/***************************************************************************
*
* $Id: StIstRawHitMaker.cxx,v 1.35 2015/12/27 18:28:32 smirnovd Exp $
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

StIstRawHitMaker::StIstRawHitMaker( const char *name ): StRTSBaseMaker( "ist", name ), mIsCaliMode(false), mDoEmbedding(false), mDoCmnCorrection(0), mIstCollectionPtr(0), mIstCollectionSimuPtr(NULL), mDataType(2)
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

   //access raw ADC containers from simu data
   TObjectSet* istSimuDataSet = (TObjectSet*)GetDataSet("istRawAdcSimu");
   if ( !istSimuDataSet ) {
      LOG_WARN << "StIstRawHitMaker::Make() - No raw ADC dataset found from simu data! " << endm;
   }
   if(istSimuDataSet) {
      mIstCollectionSimuPtr = (StIstCollection*)istSimuDataSet->GetObject();
   }
   if( !mIstCollectionSimuPtr ) {
      LOG_WARN << "StIstRawHitMaker::Make() - No istCollection found in simu dataset! "<<endm;
   }


   StRtsTable *rts_tbl = 0;
   UChar_t dataFlag = mALLdata;
   Int_t ntimebin = mCurrentTimeBinNum;

   Int_t nRawAdcFromData = 0;
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
      memset(signalUnCorrected, 0, sizeof(signalUnCorrected));
      memset(signalCorrected, 0, sizeof(signalCorrected));

      // arrays to calculate dynamical common mode noise contribution to the APV chip in current event
      Float_t sumAdcPerEvent[kIstNumTimeBins];
      Int_t counterAdcPerEvent[kIstNumTimeBins];
      memset(sumAdcPerEvent, 0, sizeof(sumAdcPerEvent));
      memset(counterAdcPerEvent, 0, sizeof(counterAdcPerEvent));

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

      // Define apv Id to form full channel id later
      int apvElecId = (rdo - 1) * kIstNumArmsPerRdo * kIstNumApvsPerArm * kIstNumApvChannels + arm * kIstNumApvsPerArm * kIstNumApvChannels + apv * kIstNumApvChannels;

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
         if(adc>0) nRawAdcFromData++;

         if ( !mIsCaliMode )        {
            Int_t elecId = apvElecId + channel;

            if (elecId < 0 || elecId >= kIstNumElecIds) {
               LOG_INFO << "Wrong elecId: " << elecId  << endm;
               continue;
            }

            // This is where we get the simulated hits from the simu container if it is available
            // and merge with real data ADC values
            if (mIstCollectionSimuPtr)
            {
               Int_t geoId = mMappingVec[elecId];
               Int_t ladder = 1 + (geoId - 1) / (kIstNumSensorsPerLadder * kIstNumPadsPerSensor);

               StIstRawHitCollection *rawHitCollectionSimuPtr = mIstCollectionSimuPtr->getRawHitCollection(ladder-1);
               if(rawHitCollectionSimuPtr)
               {
                  StIstRawHit * rawHitSimu = rawHitCollectionSimuPtr->getRawHit(elecId);
                  signalUnCorrected[channel][timebin] += rawHitSimu->getCharge(timebin);
               }
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

      FillRawHitCollectionFromAPVData(dataFlag, ntimebin, counterAdcPerEvent, sumAdcPerEvent, apvElecId, signalUnCorrected, signalCorrected);

   }//end while

   if(!mDoEmbedding && !nRawAdcFromData) FillRawHitCollectionFromSimData();

   return ierr;
}


void StIstRawHitMaker::FillRawHitCollectionFromAPVData(unsigned char dataFlag, int ntimebin, int counterAdcPerEvent[], float sumAdcPerEvent[], int apvElecId,
   int (&signalUnCorrected)[kIstNumApvChannels][kIstNumTimeBins],
   float (&signalCorrected)[kIstNumApvChannels][kIstNumTimeBins])
{
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
   memset(isPassRawHitCut,0,sizeof(isPassRawHitCut));
   Int_t nChanPassedCut = 0;


   for (int iChan = 0; iChan < kIstNumApvChannels; iChan++) {
      Int_t elecId = apvElecId + iChan;

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
      return;
   }

   // fill IST raw hits for current APV chip
   for (int iChan = 0; iChan < kIstNumApvChannels; iChan++) {
      //mapping info.
      Int_t elecId = apvElecId + iChan;
      Int_t geoId  = mMappingVec[elecId]; // channel geometry ID which is numbering from 1 to 110592
      Int_t ladder = 1 + (geoId - 1) / (kIstApvsPerLadder * kIstNumApvChannels); // ladder geometry ID: 1, 2, ..., 24
      Int_t apvId  = 1 + (geoId - 1) / kIstNumApvChannels; // APV geometry ID: 1, ..., 864 (numbering from ladder 1 to ladder 24)

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
            else return;
         }
         else { //physics mode: pedestal subtracted + dynamical common mode correction
            //skip dead chips and bad mis-configured chips
            if (mConfigVec[apvId - 1] < 1 || mConfigVec[apvId - 1] > 9) { //1-9 good status code
               LOG_DEBUG << "Skip: Channel belongs to dead/bad/mis-configured APV chip geometry index: " << apvId << " on ladder " << ladder << endm;
               return;
            }

            //skip current channel marked as suspicious status
            if (mRmsVec[elecId] < mChanMinRmsNoiseLevel || mRmsVec[elecId] > mChanMaxRmsNoiseLevel || mRmsVec[elecId] > 99.0)  {
               LOG_DEBUG << "Skip: Noisy/hot/dead channel electronics index: " << elecId << endm;
               return;
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
};

void StIstRawHitMaker::FillRawHitCollectionFromSimData()
{
   if(!mIstCollectionSimuPtr) return;
   for( UChar_t ladderIdx=0; ladderIdx < kIstNumLadders; ++ladderIdx ){
      StIstRawHitCollection *rawHitCollectionDataPtr = mIstCollectionPtr->getRawHitCollection( ladderIdx );
      std::vector<StIstRawHit *> rawAdcSimuVec = mIstCollectionSimuPtr->getRawHitCollection(ladderIdx)->getRawHitVec();
      for (std::vector<StIstRawHit *>::iterator rawAdcSimuPtr = rawAdcSimuVec.begin(); rawAdcSimuPtr!=rawAdcSimuVec.end(); ++rawAdcSimuPtr) {
         Int_t eId = (*rawAdcSimuPtr)->getChannelId();
		   StIstRawHit * rawHitData = rawHitCollectionDataPtr->getRawHit(eId);
         for(Int_t iTBin=0; iTBin<mCurrentTimeBinNum; iTBin++){
            rawHitData->setCharge((*rawAdcSimuPtr)->getCharge(iTBin),(UChar_t)iTBin);
            rawHitData->setChargeErr((*rawAdcSimuPtr)->getChargeErr(iTBin),(UChar_t)iTBin);
         }
         rawHitData->setChannelId(eId);
         rawHitData->setGeoId((*rawAdcSimuPtr)->getGeoId());
         rawHitData->setMaxTimeBin((*rawAdcSimuPtr)->getMaxTimeBin());
         rawHitData->setDefaultTimeBin((*rawAdcSimuPtr)->getDefaultTimeBin());
         rawHitData->setIdTruth((*rawAdcSimuPtr)->getIdTruth());
      }
   }
}

void StIstRawHitMaker::Clear( Option_t *opts )
{
   if (mIstCollectionPtr ) {
      for ( unsigned char i = 0; i < kIstNumLadders; ++i ) {
         mIstCollectionPtr->getRawHitCollection(i)->Clear( "" );
      }
   }
};

ClassImp(StIstRawHitMaker);
