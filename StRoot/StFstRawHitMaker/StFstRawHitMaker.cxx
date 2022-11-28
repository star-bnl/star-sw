#include "StFstRawHitMaker.h"

#include "StEvent.h"
#include "St_base/StMessMgr.h"
#include "StRoot/RTS/src/DAQ_FGT/daq_fgt.h"
#include "StRoot/RTS/src/DAQ_READER/daq_dta.h"
#include "StChain/StRtsTable.h"

#include "StFstUtil/StFstCollection.h"
#include "StFstUtil/StFstRawHitCollection.h"
#include "StEvent/StFstRawHit.h"
#include "StEvent/StFstEvtCollection.h"
#include "StFstDbMaker/StFstDb.h"
#include "StEvent/StFstConsts.h"

#include "tables/St_fstPedNoise_Table.h"
#include "tables/St_fstGain_Table.h"
#include "tables/St_fstMapping_Table.h"
#include "tables/St_fstControl_Table.h"
#include "tables/St_fstChipConfig_Table.h"


StFstRawHitMaker::StFstRawHitMaker( const char *name ): StRTSBaseMaker( "fst", name ),
    mIsCaliMode(false), mDoEmbedding(false), mDoCmnCorrection(true),
    mMinHitCut(2.5), mMedHitCut(3.5), mMaxHitCut(4.0),mCmnCut(3.),
    mFstCollectionPtr(new StFstCollection()), mFstCollectionSimuPtr(nullptr),
    mCmnVec(kFstNumApvs, std::vector<std::vector<float>>(kFstNumRStripsPerSensor, std::vector<float>(kFstNumTimeBins, 0))),
    mPedVec(kFstNumElecIds, std::vector<float>(kFstNumTimeBins, 0)),
    mTotRmsVec(kFstNumElecIds, std::vector<float>(kFstNumTimeBins, 0)),
    mRanRmsVec(kFstNumElecIds, std::vector<float>(kFstNumTimeBins, 0)),
    mGainVec(kFstNumElecIds, 0),
    mMappingVec(kFstNumElecIds, 0),
    mConfigVec(kFstNumApvs, 1),
    mDataType(0)
{
}

StFstRawHitMaker::~StFstRawHitMaker()
{
    delete mFstCollectionPtr; mFstCollectionPtr = 0;
}


/**
 * Init(): prepare the FST raw hit collection
 * in the dataset m_DataSet (data member of StMaker)
 */
Int_t StFstRawHitMaker::Init()
{
    ToWhiteConst("fstRawHitAndCluster", mFstCollectionPtr);

    return kStOk;
}


/**
 * InitRun(): access FST calibration DB and retrieve the calibration information
 * from Db tables
 */
Int_t StFstRawHitMaker::InitRun(Int_t runnumber)
{
    Int_t ierr = kStOk;

    TObjectSet *fstDbDataSet = (TObjectSet *)GetDataSet("fst_db");
    StFstDb *mFstDb = NULL;

    if (fstDbDataSet) {
        mFstDb = (StFstDb *)fstDbDataSet->GetObject();
        assert(mFstDb);
    }
    else {
        LOG_ERROR << "InitRun : no fstDb" << endm;
        return kStErr;
    }

    // FST control parameters
    const fstControl_st *fstControlTable = mFstDb->getControl() ;

    if (!fstControlTable)  {
        LOG_ERROR << "Pointer to FST control table is null" << endm;
        ierr = kStErr;
    }
    else {
        mMinHitCut            = fstControlTable[0].kFstMinHitCutDefault;
        mMedHitCut            = fstControlTable[0].kFstMedHitCutDefault;
        mMaxHitCut            = fstControlTable[0].kFstMaxHitCutDefault;
        mCmnCut               = fstControlTable[0].kFstCMNCutDefault;
        mChanMinRmsNoiseLevel = fstControlTable[0].kFstChanMinRmsNoiseLevel;
        mChanMaxRmsNoiseLevel = fstControlTable[0].kFstChanMaxRmsNoiseLevel;
        mApvMaxCmNoiseLevel   = fstControlTable[0].kFstApvMaxCmNoiseLevel;
        mALLdata              = fstControlTable[0].kFstAlldata;
        mADCdata              = fstControlTable[0].kFstADCdata;
        mZSdata               = fstControlTable[0].kFstZSdata;
        mDefaultTimeBin       = fstControlTable[0].kFstDefaultTimeBin;
        mCurrentTimeBinNum    = fstControlTable[0].kFstCurrentTimeBinNum;
        mMinNumOfRawHits      = fstControlTable[0].kFstMinNumOfRawHits;
        mMaxNumOfRawHits      = fstControlTable[0].kFstMaxNumOfRawHits;
    }

    // FST pedestal/rms table
    const fstPedNoise_st *gPN = mFstDb->getPedNoise();

    if ( !gPN ) {
        LOG_ERROR << "Pointer to FST pedestal/noise table is null" << endm;
        ierr = kStErr;
    }
    else {
        for (int i = 0; i < kFstNumApvs; i++) {
            for ( int j = 0; j < kFstNumRStripsPerSensor; j++) {
                for ( int k = 0; k < kFstNumTimeBins; k++) {
                    LOG_DEBUG << Form(" Print entry %d-%d-%d : CM noise=%f ", i, j, k, (float)gPN[0].cmNoise[(i*kFstNumRStripsPerSensor+j)*kFstNumTimeBins+k] / 100.) << endm;
                    mCmnVec[i][j][k] = (float)gPN[0].cmNoise[(i*kFstNumRStripsPerSensor+j)*kFstNumTimeBins+k] / 100.0;
                }
            }
        }

        for (int i = 0; i < kFstNumElecIds; i++) {
            for ( int j = 0; j < kFstNumTimeBins; j++) {
                LOG_DEBUG << Form(" Print entry %d-%d : pedestal=%f ", i, j, (float)gPN[0].pedestal[i*kFstNumTimeBins+j]) << endm;
                mPedVec[i][j] = (float)gPN[0].pedestal[i*kFstNumTimeBins+j];
            }
        }
        for (int i = 0; i < kFstNumElecIds; i++) {
            for ( int j = 0; j < kFstNumTimeBins; j++) {
                LOG_DEBUG << Form(" Print entry %d-%d : RMS noise=%f ", i, j, (float)gPN[0].totNoise[i*kFstNumTimeBins+j] / 100.) << endm;
                mTotRmsVec[i][j] = (float)gPN[0].totNoise[i*kFstNumTimeBins+j] / 100.;
            }
        }
        for (int i = 0; i < kFstNumElecIds; i++) {
             for ( int j = 0; j < kFstNumTimeBins; j++) {
                LOG_DEBUG << Form(" Print entry %d-%d : RMS noise=%f ", i, j, (float)gPN[0].ranNoise[i*kFstNumTimeBins+j] / 100.) << endm;
                mRanRmsVec[i][j] = (float)gPN[0].ranNoise[i*kFstNumTimeBins+j] / 100.;
             }
        }
    }

    // FST gain table
    const fstGain_st *gG = mFstDb->getGain();

    if ( !gG ) {
        LOG_WARN << "Pointer to FST gain table is null" << endm;
        ierr = kStWarn;
    }
    else {
        for (int i = 0; i < kFstNumElecIds; i++) {
            LOG_DEBUG << Form(" Print entry %d : gain=%f ", i, (float)gG[0].gain[i]) << endm;
            mGainVec[i] = (float)gG[0].gain[i];
        }
    }

    // FST mapping table
    const fstMapping_st *gM = mFstDb->getMapping();

    if ( !gM ) {
        LOG_ERROR << "Pointer to FST mapping table is null" << endm;
        ierr = kStErr;
    }
    else {
        for (int i = 0; i < kFstNumElecIds; i++) {
            LOG_DEBUG << Form(" Print entry %d : geoId=%d ", i, gM[0].mapping[i]) << endm;
            mMappingVec[i] = gM[0].mapping[i];
        }
    }

    // FST chip configuration status table
    const fstChipConfig_st *gCS = mFstDb->getChipStatus();

    if ( !gCS ) {
        LOG_ERROR << "Pointer to FST chip configuration table is null" << endm;
        ierr = kStErr;
    }
    else {
        for (int i = 0; i < kFstNumApvs; i++) {
            LOG_DEBUG << Form(" Print entry %d : status=%d ", i, gCS[0].s[i]) << endm;
            mConfigVec[i] = gCS[0].s[i];  /* 0 dead;  1 good; 2-9 reserved good status; 10 mis-configured */
        }
    }

    return ierr;
}


/**
 * Make(): main functional part of the raw hit maker, which contains the below three functions:
 * (1) un-pack the FST raw ADC data (ZS or non-ZS) from daq data via daq reader;
 * (2) pedestal subtraction for the non-ZS data and dynamical common-mode noise calculation;
 * (3) raw hit decision and ADC-to-dE/dx translation, then written to corresponding collection in physics mode;
 *     While in offline calibrarion mode, ADC information was directly saved without any raw hit processing.
 */
Int_t StFstRawHitMaker::Make()
{
    Int_t ierr = kStOk;

    //access raw ADC containers from simu data
    TObjectSet* fstSimuDataSet = (TObjectSet*)GetDataSet("fstRawAdcSimu");
    if ( !fstSimuDataSet ) {
        LOG_WARN << "StFstRawHitMaker::Make() - No raw ADC dataset found from simu data! " << endm;
    }
    if(fstSimuDataSet) {
        mFstCollectionSimuPtr = (StFstCollection*)fstSimuDataSet->GetObject();
    }
    if( !mFstCollectionSimuPtr ) {
        LOG_WARN << "StFstRawHitMaker::Make() - No fstCollection found in simu dataset! "<<endm;
    }


    StRtsTable *rts_tbl = 0;
    UChar_t dataFlag = mALLdata;
    Int_t ntimebin = mCurrentTimeBinNum;

    Int_t nRawAdcFromData = 0;
    int nIdTruth_Fst = 0;
    Bool_t printed = kFALSE;
    while (1) { //loops over input raw data
        if (dataFlag == mALLdata) {
            if (mDataType == mALLdata) {
                if(!printed) { LOG_INFO << " Trying to read ALLdata" << endm; printed = kTRUE; }
                rts_tbl = GetNextDaqElement("fst/zs");      dataFlag = mZSdata;

                if (!rts_tbl) {
                    LOG_WARN << "NO ZS-DATA BANK FOUND!!!" << endm;
                    rts_tbl = GetNextDaqElement("fst/adc");  dataFlag = mADCdata;
                }
            }
            else if (mDataType == mADCdata) {
                if(!printed) { LOG_INFO << " Trying to read ADCdata" << endm; printed = kTRUE; }
                rts_tbl = GetNextDaqElement("fst/adc");     dataFlag = mADCdata;
            }
            else if (mDataType == mZSdata) {
                if(!printed) { LOG_INFO << " Trying to read ZSdata" << endm; printed = kTRUE; }         
                rts_tbl = GetNextDaqElement("fst/zs");      dataFlag = mZSdata;
            }
        }
        else if (dataFlag == mADCdata) { 
            if(!printed) { LOG_INFO << " Trying to read ADCdata" << endm; printed = kTRUE; }
            rts_tbl = GetNextDaqElement("fst/adc"); 
        }
        else if (dataFlag == mZSdata) { 
            if(!printed) { LOG_INFO << " Trying to read ZSdata" << endm; printed = kTRUE; }
            rts_tbl = GetNextDaqElement("fst/zs");
        }

        if (!rts_tbl) break;

        apv_meta_t *meta = (apv_meta_t *)rts_tbl->Meta();

        if (meta) {
            for (int r = 1; r <= kFstNumRdos; r++) { //6 rdos needed for whole FST detector
                if (meta->arc[r].present == 0) continue ;

                for (int arm = 0; arm < kFstNumArmsPerRdo; arm++) { //3 arms per arc
                    if (meta->arc[r].arm[arm].present == 0) continue ;

                    for (int apv = 0; apv < 24; apv++) { //16 apvs per arm
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

        mFstCollectionPtr->setNumTimeBins(ntimebin);

        // Arrays to store ADC information per APV chip (128 channels over all time bins)
        // Signal w/o pedestal subtracted
        std::array< std::array<double, kFstNumTimeBins>, kFstNumApvChannels > signalUnCorrected{};
        // Signal w/ pedestal subtracted
        std::array< std::array<double, kFstNumTimeBins>, kFstNumApvChannels > signalCorrected{};
	// seed hit flag: non-zs: 0 | zs: >0 & 7 for seed hit
	std::array< std::array<int, kFstNumTimeBins>, kFstNumApvChannels > seedFlag{};
        // id of mc track
        std::array<int, kFstNumApvChannels> idTruth{};

        // arrays to calculate dynamical common mode noise contribution to the APV chip in current event
        double sumAdcPerRgroupPerEvent[kFstNumRStripsPerWedge][kFstNumTimeBins];
        Int_t counterAdcPerRgroupPerEvent[kFstNumRStripsPerWedge][kFstNumTimeBins];
        memset(sumAdcPerRgroupPerEvent, 0, sizeof(sumAdcPerRgroupPerEvent));
        memset(counterAdcPerRgroupPerEvent, 0, sizeof(counterAdcPerRgroupPerEvent));

        // electronics coordinate info.: RDO, ARM, APV
        Int_t rdo = rts_tbl->Rdo();     // 1, 2, ..., 6
        Int_t arm = rts_tbl->Sector();  // 0, 1, 2
        Int_t apvro = rts_tbl->Pad();   // 0-7&12-19
        Int_t apv = -1;
        if(apvro>7) apv = apvro-4;
        else apv = apvro;           //APV:0-15

        Int_t flag = 0;

        if (rdo < 1 || rdo >  kFstNumRdos)       flag = 1;
        if (arm < 0 || arm >= kFstNumArmsPerRdo) flag = 1;
        if (apv < 0 || apv >= kFstNumApvsPerArm) flag = 1;

        if (flag == 1) {
            LOG_INFO << "Corrupt data  rdo: " << rdo << " arm: " << arm << " apv: " << apv << endm;
            continue;
        }

        // Define apv Id to form full channel id later
        int apvElecId = (rdo - 1) * kFstNumArmsPerRdo * kFstNumApvsPerArm * kFstNumApvChannels + arm * kFstNumApvsPerArm * kFstNumApvChannels + apv * kFstNumApvChannels;

        // Loop over the data in this APV to get raw hit info. (channel, timebin, adc)
        for (StRtsTable::iterator it = rts_tbl->begin(); it != rts_tbl->end(); it++) {
            // channel info.
            fgt_adc_t *f = (fgt_adc_t *)*it;
            Int_t channel   = f->ch;  //channel index  0, 1, ..., 127
            Int_t adc       = f->adc; //adc
            Short_t timebin = f->tb;  //time bin
	    Int_t sFlag     = f->flags; // seed hit flag
            LOG_DEBUG << "channel: " << channel << "   adc: " << adc << "  time bin: " << timebin << endm;

            flag = 0;

            if ((dataFlag == mADCdata) && (adc < 0 || adc >= kFstMaxAdc))  flag = 1;
            if (channel < 0 || channel >= kFstNumApvChannels)              flag = 1;
            if (timebin < 0 || timebin >= ntimebin)                        flag = 1;

            if (flag == 1) {
                LOG_INFO << "Corrupt data channel: " << channel << " tbin: " << timebin << " adc: " << adc << endm;
                continue;
            }

            signalUnCorrected[channel][timebin] = adc;
	    seedFlag[channel][timebin] = sFlag;
            if(adc>0) nRawAdcFromData++;

            if ( !mIsCaliMode )        {
                Int_t elecId = apvElecId + channel;

                if (elecId < 0 || elecId >= kFstNumElecIds) {
                    LOG_INFO << "Wrong elecId: " << elecId  << endm;
                    continue;
                }

                // This is where we get the simulated hits from the simu container if it is available
                // and merge with real data ADC values
                if (mFstCollectionSimuPtr)
                {
                    Int_t geoId = mMappingVec[elecId];
                    Int_t wedge = 1 + geoId / (kFstNumInnerSensorsPerWedge * kFstNumStripsPerInnerSensor + kFstNumOuterSensorsPerWedge * kFstNumStripsPerOuterSensor);

                    StFstRawHitCollection *rawHitCollectionSimuPtr = mFstCollectionSimuPtr->getRawHitCollection(wedge-1);
                    if(rawHitCollectionSimuPtr)
                    {
                        StFstRawHit * rawHitSimu = rawHitCollectionSimuPtr->getRawHit(elecId);
                        idTruth[channel] = 0;
                        if(rawHitSimu->getCharge(timebin) > 0) {  // a valid MC hit
                            signalUnCorrected[channel][timebin] += rawHitSimu->getCharge(timebin);
                            idTruth[channel] = rawHitSimu->getIdTruth();
                        }
                    }
                }

                if ( dataFlag == mADCdata ) { // non-ZS data
                    signalCorrected[channel][timebin] = signalUnCorrected[channel][timebin] - mPedVec[elecId][timebin];

                    // exclude signal-related channels for common mode noise calculation
                    if ( (signalCorrected[channel][timebin] > (-mCmnCut)*mTotRmsVec[elecId][timebin]) &&
                            (signalCorrected[channel][timebin] <   mCmnCut *mTotRmsVec[elecId][timebin]) )
                    {
                        Int_t geoId = mMappingVec[elecId];
                        int rstrip = (geoId % (kFstNumInnerSensorsPerWedge * kFstNumStripsPerInnerSensor + kFstNumOuterSensorsPerWedge * kFstNumStripsPerOuterSensor))/kFstNumPhiSegPerWedge;
                        sumAdcPerRgroupPerEvent[rstrip][timebin] += signalCorrected[channel][timebin];
                        counterAdcPerRgroupPerEvent[rstrip][timebin]++;
                    }

                    LOG_DEBUG << " Corrected = " << signalCorrected[channel][timebin] << endm;
                }
                else {      // ZS data
                    signalCorrected[channel][timebin] = signalUnCorrected[channel][timebin];
                }
            }
        } // end current APV loops

	nIdTruth_Fst += FillRawHitCollectionFromAPVData(dataFlag, ntimebin, counterAdcPerRgroupPerEvent, sumAdcPerRgroupPerEvent, apvElecId, signalUnCorrected, signalCorrected, seedFlag, idTruth);

    }//end while
    LOG_INFO << " Total number of FST Raw Hits - Step I = " << mFstCollectionPtr->getNumRawHits() << " w/ idTruth = " << nIdTruth_Fst << endm;

    // In case of pure simulation mode when neither real data hits from DAQ
    // records is available nor embedding is requested fill the output container
    // with simulated hits
    if(!mDoEmbedding && !nRawAdcFromData) nIdTruth_Fst += FillRawHitCollectionFromSimData();

    LOG_INFO << " Total number of FST Raw Hits - Step II = " << mFstCollectionPtr->getNumRawHits() << " w/ idTruth = " << nIdTruth_Fst << endm;

    // Fill Fst raw hits we got so far into StEvent if the chain option is called
    if(IAttr("fstEvtRawHit")) {//True for storing FST Raw hits
        LOG_INFO << " Filling FST Raw Hits into StEvent" << endm;

        //prepare for StEvent
        mEvent = (StEvent *) GetInputDS("StEvent");

        if(mEvent) {
            LOG_DEBUG<<"Found StEvent"<<endm;
        } else {
            mEvent = new StEvent();
            AddData(mEvent);
            LOG_DEBUG <<"Added StEvent"<<endm;
        }

        // Get pointer to an existing StFstEvtCollection if any
        mFstEvtCollection = mEvent->fstEvtCollection();

        // If no fst raw hit collection, create one
        if (!mFstEvtCollection) {
            mFstEvtCollection = new StFstEvtCollection();
            mEvent->setFstEvtCollection(mFstEvtCollection);
            LOG_DEBUG << "Make() - Added new StFstEvtCollection to this StEvent" << endm;
        }

        if(mFstCollectionPtr->getNumRawHits() > 0) {
            for(int wedgeIdx=0; wedgeIdx<kFstNumWedges; ++wedgeIdx ){
                StFstRawHitCollection *rawHitCollectionPtr = mFstCollectionPtr->getRawHitCollection( wedgeIdx );
                if( rawHitCollectionPtr ){
                    std::vector<StFstRawHit*>& rawHitVec = rawHitCollectionPtr->getRawHitVec();
                    std::vector< StFstRawHit* >::iterator rawHitIter;

                    for( rawHitIter = rawHitVec.begin(); rawHitIter != rawHitVec.end(); ++rawHitIter ){
                        StFstRawHit* rawHit = *rawHitIter;
                        StFstRawHit* newRawHit = new StFstRawHit( *rawHit );
                        mFstEvtCollection->addRawHit(newRawHit);
                    }
                }
            }
        }
    }

    return ierr;
}


/**
 * A private helper function to actually insert StFstRawHits unpacked from DAQ
 * records into the final output container mFstCollectionPtr.
 */
int StFstRawHitMaker::FillRawHitCollectionFromAPVData(unsigned char dataFlag, int ntimebin,
        int counterAdcPerRgroupPerEvent[][kFstNumTimeBins], double sumAdcPerRgroupPerEvent[][kFstNumTimeBins], int apvElecId,
        std::array< std::array<double, kFstNumTimeBins>, kFstNumApvChannels > &signalUnCorrected,
        std::array< std::array<double, kFstNumTimeBins>, kFstNumApvChannels > &signalCorrected,
	std::array< std::array<int, kFstNumTimeBins>, kFstNumApvChannels > &seedFlag,
        std::array<int, kFstNumApvChannels> &idTruth)
{
    int nIdTruth = 0;
    // calculate the dynamical common mode noise for the current chip in this event
    double commonModeNoise[kFstNumRStripsPerWedge][kFstNumTimeBins];

    for (int tbIdx = 0; tbIdx < kFstNumTimeBins; tbIdx++){
        for (int rindex = 0; rindex < kFstNumRStripsPerWedge; rindex++)
            commonModeNoise[rindex][tbIdx] = 0.;
    }

    if ( !mIsCaliMode && dataFlag == mADCdata ) {
        for (short iTb = 0; iTb < ntimebin; iTb++)  {
            for (int rindex = 0; rindex < kFstNumRStripsPerWedge; rindex++){
                if (counterAdcPerRgroupPerEvent[rindex][iTb] > 0)
                    commonModeNoise[rindex][iTb] = sumAdcPerRgroupPerEvent[rindex][iTb] / counterAdcPerRgroupPerEvent[rindex][iTb];
            }
        }
    }

    // CMN correction, raw hit decision and channel counter passed the hit decision
    std::vector<bool> isPassRawHitCut(kFstNumApvChannels, false);
    Int_t nChanPassedCut = 0;

    for (int iChan = 0; iChan < kFstNumApvChannels; iChan++)
    {
        Int_t elecId = apvElecId + iChan;
        Int_t geoId  = mMappingVec[elecId];
        // Modify signalCorrected values in some ways
        for (int iTBin = 0; iTBin < ntimebin; iTBin++)
        {
            if (!mIsCaliMode && mDoCmnCorrection && dataFlag == mADCdata ){
                int rstrip = (geoId % (kFstNumInnerSensorsPerWedge * kFstNumStripsPerInnerSensor + kFstNumOuterSensorsPerWedge * kFstNumStripsPerOuterSensor))/kFstNumPhiSegPerWedge;
                signalCorrected[iChan][iTBin] -= commonModeNoise[rstrip][iTBin];
            }
        }

	for (int iTB = 0; iTB < ntimebin; iTB++)
	{
	    // raw hit decision for non-zs
	    if ( (dataFlag == mADCdata) && (signalUnCorrected[iChan][iTB] > 0) &&
		    (signalUnCorrected[iChan][iTB] < kFstMaxAdc) &&
		    ((signalCorrected[iChan][iTB]     > mMedHitCut * mRanRmsVec[elecId][iTB])||
		     (iTB >0 && (signalCorrected[iChan][iTB-1]     > mMinHitCut * mRanRmsVec[elecId][iTB]) &&
		      (signalCorrected[iChan][iTB]     > mMinHitCut * mRanRmsVec[elecId][iTB]))))
	    {
		isPassRawHitCut[iChan] = kTRUE;
		nChanPassedCut++;
		iTB = 999;
	    }
	    // zs data are all raw hits candidates
	    if( (dataFlag == mZSdata) && (seedFlag[iChan][iTB] > 0) )
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

    // fill FST raw hits for current APV chip
    for (int iChan = 0; iChan < kFstNumApvChannels; iChan++)
    {
        Int_t seedhitflag = 0;
        //mapping info.
        Int_t elecId = apvElecId + iChan;
        Int_t geoId  = mMappingVec[elecId]; // channel geometry ID which is numbering from 0 to 36863
        Int_t wedge = 1 + geoId / (kFstApvsPerWedge * kFstNumApvChannels); // wedge geometry ID: 1, 2, ..., 36
        Int_t apvId  = elecId / kFstNumApvChannels; // APV geometry ID: 0, ..., 287 (numbering from wedge 1 to wedge 36)

        //store raw hits information
        StFstRawHitCollection *rawHitCollectionPtr = mFstCollectionPtr->getRawHitCollection( wedge - 1 );

        if ( !rawHitCollectionPtr ) {
            LOG_WARN << "StFstRawHitMaker::Make() -- Could not access rawHitCollection for wedge " << wedge << endm;
            continue;
        }

        if ( mIsCaliMode ) { //calibration mode (non-ZS data): only write raw ADC value
          if (dataFlag == mADCdata) {
            // rawHitCollectionPtr->addRawHit( new StFstRawHit(elecId, geoId, signalUnCorrected[iChan]) );
            StFstRawHit *rawHitPtr = rawHitCollectionPtr->getRawHit( elecId );
            rawHitPtr->setCharges(signalUnCorrected[iChan]);
            rawHitPtr->setChannelId( elecId );
            rawHitPtr->setGeoId( geoId );
          }
          else return 0;
        }
        else { //physics mode: pedestal subtracted + dynamical common mode correction
            //skip dead chips and bad mis-configured chips
            if (mConfigVec[apvId] < 1 || mConfigVec[apvId] > 9) { //1-9 good status code
                LOG_DEBUG << "Skip: Channel belongs to dead/bad/mis-configured APV chip electronic index: " << apvId << " on wedge " << wedge << endm;
                continue;
            }

            //skip current channel marked as suspicious status
            if (mRanRmsVec[elecId][mDefaultTimeBin] < mChanMinRmsNoiseLevel ||
                    mRanRmsVec[elecId][mDefaultTimeBin] > mChanMaxRmsNoiseLevel ||
                    mRanRmsVec[elecId][mDefaultTimeBin] > 99.0)
            {
                LOG_DEBUG << "Skip: Noisy/hot/dead channel electronics index: " << elecId << endm;
                continue;
            }

            if ( !isPassRawHitCut[iChan] )
                continue;

            UChar_t tempMaxTB = -1;
            double tempMaxCharge = -999.0;

            StFstRawHit *rawHitPtr = rawHitCollectionPtr->getRawHit( elecId );

            // Modify signalCorrected values in some ways
            for (int iTBin = 0; iTBin < ntimebin; iTBin++)
            {
                if (signalCorrected[iChan][iTBin] < 0)
                    signalCorrected[iChan][iTBin] = 0.1;

                rawHitPtr->setChargeErr(mRanRmsVec[elecId][iTBin] * mGainVec[elecId], (unsigned char)iTBin);

                if (signalCorrected[iChan][iTBin] > tempMaxCharge) {
                    tempMaxCharge = signalCorrected[iChan][iTBin];
                    tempMaxTB = (unsigned char)iTBin;
                }

                signalCorrected[iChan][iTBin] *= mGainVec[elecId];
		if( (dataFlag == mADCdata) && (iTBin >0) && 
			(signalCorrected[iChan][iTBin-1] > mMaxHitCut * mRanRmsVec[elecId][iTBin-1]) &&
			(signalCorrected[iChan][iTBin] > mMaxHitCut * mRanRmsVec[elecId][iTBin]))
		{
		    seedhitflag = 1;
		}
		if( (dataFlag == mZSdata) && (seedFlag[iChan][iTBin] == 7) )
		{
		    seedhitflag = 1;
		}
            }

            rawHitPtr->setCharges(signalCorrected[iChan]);
            rawHitPtr->setChannelId( elecId );
            rawHitPtr->setGeoId( geoId );
            rawHitPtr->setSeedhitflag( seedhitflag );
            rawHitPtr->setMaxTimeBin( tempMaxTB );
            rawHitPtr->setDefaultTimeBin( mDefaultTimeBin );
            rawHitPtr->setIdTruth( idTruth[iChan] );
            if(idTruth[iChan]>0) nIdTruth++;

        }//end filling hit info
    } //end single APV chip hits filling
    return nIdTruth;
}


/**
 * Copies (and overwrites) StFstRawHits from the container with simulated hits
 * (usually provided by StFstSlowSimMaker and pointed to by this class' member
 * mFstCollectionSimuPtr) to the final output container with real data hits
 * (pointed to by class member mFstCollectionPtr). This method is used internaly
 * in the pure simulation mode when neither real data hits from DAQ records is
 * available nor embedding is requested.
 */
int StFstRawHitMaker::FillRawHitCollectionFromSimData()
{
    int nIdTruth = 0;
    if(!mFstCollectionSimuPtr) return 0;

    for( UChar_t wedgeIdx=0; wedgeIdx < kFstNumWedges; ++wedgeIdx )
    {
        StFstRawHitCollection *rawHitCollectionDataPtr = mFstCollectionPtr->getRawHitCollection( wedgeIdx );
        std::vector<StFstRawHit *> rawAdcSimuVec = mFstCollectionSimuPtr->getRawHitCollection(wedgeIdx)->getRawHitVec();

        for (const auto rawAdcSimuPtr : rawAdcSimuVec)
        {
            if (!rawAdcSimuPtr) continue;

            rawHitCollectionDataPtr->addRawHit(new StFstRawHit(*rawAdcSimuPtr));
            if(rawAdcSimuPtr->getIdTruth()>0) nIdTruth++;
        }
    }
    return nIdTruth;
}

void StFstRawHitMaker::Clear( Option_t *opts )
{
    if (mFstCollectionPtr ) {
        for ( unsigned char i = 0; i < kFstNumWedges; ++i ) {
            mFstCollectionPtr->getRawHitCollection(i)->Clear( "" );
        }
    }
}

ClassImp(StFstRawHitMaker)
