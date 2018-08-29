// $Id: StIstRawHitMaker.cxx,v 1.50 2018/02/25 03:51:57 dongx Exp $
#include "StIstRawHitMaker.h"

#include "StEvent.h"
#include "St_base/StMessMgr.h"
#include "RTS/src/DAQ_FGT/daq_fgt.h"
#include "RTS/src/DAQ_READER/daq_dta.h"
#include "StChain/StRtsTable.h"

#include "StIstUtil/StIstCollection.h"
#include "StIstUtil/StIstRawHitCollection.h"
#include "StIstUtil/StIstRawHit.h"
#include "StIstDbMaker/StIstDb.h"
#include "StIstUtil/StIstConsts.h"

#include "tables/St_istPedNoise_Table.h"
#include "tables/St_istGain_Table.h"
#include "tables/St_istMapping_Table.h"
#include "tables/St_istControl_Table.h"
#include "tables/St_istChipConfig_Table.h"


StIstRawHitMaker::StIstRawHitMaker( const char *name ): StRTSBaseMaker( "ist", name ),
   mIsCaliMode(false), mDoEmbedding(false), mDoCmnCorrection(false),
   mHitCut(5.), mCmnCut(3.),
   mIstCollectionPtr(new StIstCollection()), mIstCollectionSimuPtr(nullptr),
   mCmnVec(kIstNumApvs, 0),
   mPedVec(kIstNumElecIds, 0),
   mRmsVec(kIstNumElecIds, 0),
   mGainVec(kIstNumElecIds, 0),
   mMappingVec(kIstNumElecIds, 0),
   mConfigVec(kIstNumApvs, 1),
   mDataType(2)
{
}

StIstRawHitMaker::~StIstRawHitMaker()
{
   delete mIstCollectionPtr; mIstCollectionPtr = 0;
}


/**
 * Init(): prepare the IST raw hit collection
 * in the dataset m_DataSet (data member of StMaker)
 */
Int_t StIstRawHitMaker::Init()
{
   ToWhiteConst("istRawHitAndCluster", mIstCollectionPtr);

   return kStOk;
}


/**
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
}


/**
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
   int nIdTruth_Ist = 0;
   Bool_t printed = kFALSE;
   while (1) { //loops over input raw data
      if (dataFlag == mALLdata) {
         if (mDataType == mALLdata) {
            if(!printed) { LOG_INFO << " Trying to read ALLdata" << endm; printed = kTRUE; }
            rts_tbl = GetNextDaqElement("ist/zs");      dataFlag = mZSdata;

            if (!rts_tbl) {
               LOG_WARN << "NO ZS-DATA BANK FOUND!!!" << endm;
               rts_tbl = GetNextDaqElement("ist/adc");  dataFlag = mADCdata;
            }
         }
         else if (mDataType == mADCdata) {
            if(!printed) { LOG_INFO << " Trying to read ADCdata" << endm; printed = kTRUE; }
            rts_tbl = GetNextDaqElement("ist/adc");     dataFlag = mADCdata;
         }
         else if (mDataType == mZSdata) {
            if(!printed) { LOG_INFO << " Trying to read ZSdata" << endm; printed = kTRUE; }         
            rts_tbl = GetNextDaqElement("ist/zs");      dataFlag = mZSdata;
         }
      }
      else if (dataFlag == mADCdata) { 
            if(!printed) { LOG_INFO << " Trying to read ADCdata" << endm; printed = kTRUE; }
            rts_tbl = GetNextDaqElement("ist/adc"); 
      }
      else if (dataFlag == mZSdata) { 
            if(!printed) { LOG_INFO << " Trying to read ZSdata" << endm; printed = kTRUE; }
            rts_tbl = GetNextDaqElement("ist/zs");
      }

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

      // Arrays to store ADC information per APV chip (128 channels over all time bins)
      // Signal w/o pedestal subtracted
      std::array< std::array<double, kIstNumTimeBins>, kIstNumApvChannels > signalUnCorrected{};
      // Signal w/ pedestal subtracted
      std::array< std::array<double, kIstNumTimeBins>, kIstNumApvChannels > signalCorrected{};
      // id of mc track
      std::array<int, kIstNumApvChannels> idTruth{};

      // arrays to calculate dynamical common mode noise contribution to the APV chip in current event
      double sumAdcPerEvent[kIstNumTimeBins];
      Int_t counterAdcPerEvent[kIstNumTimeBins];
      memset(sumAdcPerEvent, 0, sizeof(sumAdcPerEvent));
      memset(counterAdcPerEvent, 0, sizeof(counterAdcPerEvent));

      // electronics coordinate info.: RDO, ARM, APV
      Int_t rdo = rts_tbl->Rdo();     // 1, 2, ..., 6
      Int_t arm = rts_tbl->Sector();  // 0, 1, ..., 5
      Int_t apv = rts_tbl->Pad();     // 0, 1, ..., 23

      Int_t flag = 0;

      if (rdo < 1 || rdo >  kIstNumRdos)       flag = 1;
      if (arm < 0 || arm >= kIstNumArmsPerRdo) flag = 1;
      if (apv < 0 || apv >= kIstNumApvsPerArm) flag = 1;

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

         if ((dataFlag == mADCdata) && (adc < 0 || adc >= kIstMaxAdc))  flag = 1;
         if (channel < 0 || channel >= kIstNumApvChannels)              flag = 1;
         if (timebin < 0 || timebin >= ntimebin)                        flag = 1;

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
                  idTruth[channel] = 0;
                  if(rawHitSimu->getCharge(timebin) > 0) {  // a valid MC hit
                    signalUnCorrected[channel][timebin] += rawHitSimu->getCharge(timebin);
                    idTruth[channel] = rawHitSimu->getIdTruth();
                  }
               }
            }

            if ( dataFlag == mADCdata ) { // non-ZS data
               signalCorrected[channel][timebin]    = signalUnCorrected[channel][timebin] - mPedVec[elecId];

               // exclude signal-related channels for common mode noise calculation
               if ( (signalCorrected[channel][timebin] > (-mCmnCut)*mRmsVec[elecId]) &&
                    (signalCorrected[channel][timebin] <   mCmnCut *mRmsVec[elecId]) )
               {
                  sumAdcPerEvent[timebin] += signalCorrected[channel][timebin];
                  counterAdcPerEvent[timebin]++;
               }
               
               LOG_DEBUG << " Corrected = " << signalCorrected[channel][timebin] << "\t sumAdcPerEvent = " << sumAdcPerEvent[timebin] << endm;
            }
            else {      // ZS data
               signalCorrected[channel][timebin]    = signalUnCorrected[channel][timebin];
            }
         }
      } // end current APV loops

      nIdTruth_Ist += FillRawHitCollectionFromAPVData(dataFlag, ntimebin, counterAdcPerEvent, sumAdcPerEvent, apvElecId, signalUnCorrected, signalCorrected, idTruth);

   }//end while
   LOG_INFO << " Total number of IST Raw Hits - Step I = " << mIstCollectionPtr->getNumRawHits() << " w/ idTruth = " << nIdTruth_Ist << endm;
   
   // In case of pure simulation mode when neither real data hits from DAQ
   // records is available nor embedding is requested fill the output container
   // with simulated hits
   if(!mDoEmbedding && !nRawAdcFromData) nIdTruth_Ist += FillRawHitCollectionFromSimData();

   LOG_INFO << " Total number of IST Raw Hits - Step II = " << mIstCollectionPtr->getNumRawHits() << " w/ idTruth = " << nIdTruth_Ist << endm;
   
   return ierr;
}


/**
 * A private helper function to actually insert StIstRawHits unpacked from DAQ
 * records into the final output container mIstCollectionPtr.
 */
int StIstRawHitMaker::FillRawHitCollectionFromAPVData(unsigned char dataFlag, int ntimebin,
   int counterAdcPerEvent[], double sumAdcPerEvent[], int apvElecId,
   std::array< std::array<double, kIstNumTimeBins>, kIstNumApvChannels > &signalUnCorrected,
   std::array< std::array<double, kIstNumTimeBins>, kIstNumApvChannels > &signalCorrected,
   std::array<int, kIstNumApvChannels> &idTruth)
{
   int nIdTruth = 0;
   // calculate the dynamical common mode noise for the current chip in this event
   double commonModeNoise[kIstNumTimeBins];

   for (int tbIdx = 0; tbIdx < kIstNumTimeBins; tbIdx++)
      commonModeNoise[tbIdx] = 0.;

   if ( !mIsCaliMode && dataFlag == mADCdata ) {
      for (short iTb = 0; iTb < ntimebin; iTb++)  {
         if (counterAdcPerEvent[iTb] > 0)
            commonModeNoise[iTb] = sumAdcPerEvent[iTb] / counterAdcPerEvent[iTb];
      }
   }

   // raw hit decision and channel counter passed the hit decision
   std::vector<bool> isPassRawHitCut(kIstNumApvChannels, false);
   Int_t nChanPassedCut = 0;


   for (int iChan = 0; iChan < kIstNumApvChannels; iChan++)
   {
      Int_t elecId = apvElecId + iChan;

      for (int iTB = 1; iTB < ntimebin - 1; iTB++)
      {
         // raw hit decision: the method is stolen from Gerrit's ARMdisplay.C
         if ( (signalUnCorrected[iChan][iTB] > 0) &&
              (signalUnCorrected[iChan][iTB] < kIstMaxAdc) &&
              (signalCorrected[iChan][iTB - 1] > mHitCut * mRmsVec[elecId]) &&
              (signalCorrected[iChan][iTB]     > mHitCut * mRmsVec[elecId]) &&
              (signalCorrected[iChan][iTB + 1] > mHitCut * mRmsVec[elecId]) )
         {
            isPassRawHitCut[iChan] = kTRUE;
            nChanPassedCut++;
            iTB = 999;
         }
      }
   }

   // skip the chip filling if the signal-channel number too large (20% chip occupancy was set) to exclude hot chip
   if ( !mIsCaliMode && (nChanPassedCut > mMaxNumOfRawHits || nChanPassedCut < mMinNumOfRawHits) ) {
      LOG_DEBUG << "Skip: The APV chip could be hot with " << nChanPassedCut << " channels fired!!" << endm;
      return 0;
   }

   // fill IST raw hits for current APV chip
   for (int iChan = 0; iChan < kIstNumApvChannels; iChan++)
   {
      //mapping info.
      Int_t elecId = apvElecId + iChan;
      Int_t geoId  = mMappingVec[elecId]; // channel geometry ID which is numbering from 1 to 110592
      Int_t ladder = 1 + (geoId - 1) / (kIstApvsPerLadder * kIstNumApvChannels); // ladder geometry ID: 1, 2, ..., 24
      Int_t apvId  = 1 + (geoId - 1) / kIstNumApvChannels; // APV geometry ID: 1, ..., 864 (numbering from ladder 1 to ladder 24)

      //store raw hits information
      StIstRawHitCollection *rawHitCollectionPtr = mIstCollectionPtr->getRawHitCollection( ladder - 1 );

      if ( !rawHitCollectionPtr ) {
         LOG_WARN << "StIstRawHitMaker::Make() -- Could not access rawHitCollection for ladder " << ladder << endm;
         continue;
      }

      if ( mIsCaliMode ) { //calibration mode (non-ZS data): only write raw ADC value
         if (dataFlag == mADCdata) {
            rawHitCollectionPtr->addRawHit( new StIstRawHit(elecId, geoId, signalUnCorrected[iChan]) );
         }
         else return 0;
      }
      else { //physics mode: pedestal subtracted + dynamical common mode correction
         //skip dead chips and bad mis-configured chips
         if (mConfigVec[apvId - 1] < 1 || mConfigVec[apvId - 1] > 9) { //1-9 good status code
            LOG_DEBUG << "Skip: Channel belongs to dead/bad/mis-configured APV chip geometry index: " << apvId << " on ladder " << ladder << endm;
            continue;
         }

         //skip current channel marked as suspicious status
         if (mRmsVec[elecId] < mChanMinRmsNoiseLevel ||
             mRmsVec[elecId] > mChanMaxRmsNoiseLevel ||
             mRmsVec[elecId] > 99.0)
         {
            LOG_DEBUG << "Skip: Noisy/hot/dead channel electronics index: " << elecId << endm;
            continue;
         }

         if ( !isPassRawHitCut[iChan] )
            continue;

         UChar_t tempMaxTB = -1;
         double tempMaxCharge = -999.0;

         StIstRawHit *rawHitPtr = rawHitCollectionPtr->getRawHit( elecId );

         // Modify signalCorrected values in some ways
         for (int iTBin = 0; iTBin < ntimebin; iTBin++)
         {
            if ( mDoCmnCorrection && dataFlag == mADCdata )
               signalCorrected[iChan][iTBin] -= commonModeNoise[iTBin];

            if (signalCorrected[iChan][iTBin] < 0)
               signalCorrected[iChan][iTBin] = 0.1;

            rawHitPtr->setChargeErr(mRmsVec[elecId] * mGainVec[elecId], (unsigned char)iTBin);

            if (signalCorrected[iChan][iTBin] > tempMaxCharge) {
               tempMaxCharge = signalCorrected[iChan][iTBin];
               tempMaxTB = (unsigned char)iTBin;
            }

            signalCorrected[iChan][iTBin] *= mGainVec[elecId];
         }

         rawHitPtr->setCharges(signalCorrected[iChan]);
         rawHitPtr->setChannelId( elecId );
         rawHitPtr->setGeoId( geoId );
         rawHitPtr->setMaxTimeBin( tempMaxTB );
         rawHitPtr->setDefaultTimeBin( mDefaultTimeBin );
         rawHitPtr->setIdTruth( idTruth[iChan] );
         if(idTruth[iChan]>0) nIdTruth++;
         
      }//end filling hit info
   } //end single APV chip hits filling
   return nIdTruth;
}


/**
 * Copies (and overwrites) StIstRawHits from the container with simulated hits
 * (usually provided by StIstSlowSimMaker and pointed to by this class' member
 * mIstCollectionSimuPtr) to the final output container with real data hits
 * (pointed to by class member mIstCollectionPtr). This method is used internaly
 * in the pure simulation mode when neither real data hits from DAQ records is
 * available nor embedding is requested.
 */
int StIstRawHitMaker::FillRawHitCollectionFromSimData()
{
   int nIdTruth = 0;
   if(!mIstCollectionSimuPtr) return 0;

   for( UChar_t ladderIdx=0; ladderIdx < kIstNumLadders; ++ladderIdx )
   {
      StIstRawHitCollection *rawHitCollectionDataPtr = mIstCollectionPtr->getRawHitCollection( ladderIdx );
      std::vector<StIstRawHit *> rawAdcSimuVec = mIstCollectionSimuPtr->getRawHitCollection(ladderIdx)->getRawHitVec();

      for (const auto rawAdcSimuPtr : rawAdcSimuVec)
      {
         if (!rawAdcSimuPtr) continue;

         rawHitCollectionDataPtr->addRawHit(new StIstRawHit(*rawAdcSimuPtr));
         if(rawAdcSimuPtr->getIdTruth()>0) nIdTruth++;
      }
   }
   return nIdTruth;
}

void StIstRawHitMaker::Clear( Option_t *opts )
{
   if (mIstCollectionPtr ) {
      for ( unsigned char i = 0; i < kIstNumLadders; ++i ) {
         mIstCollectionPtr->getRawHitCollection(i)->Clear( "" );
      }
   }
}

ClassImp(StIstRawHitMaker)
