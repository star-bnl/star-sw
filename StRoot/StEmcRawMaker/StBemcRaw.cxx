//
// $Id: StBemcRaw.cxx,v 1.40 2017/06/02 16:43:06 jlzhang Exp $
// $Log: StBemcRaw.cxx,v $
// Revision 1.40  2017/06/02 16:43:06  jlzhang
// peer review for picoDst production
//
// Revision 1.39  2010/12/22 22:58:57  stevens4
// Patch for BSMDE mapping problem in P10ih and P10ij productions (RT #2043)
//
// Revision 1.38  2009/11/17 15:55:48  mattheww
// fixed a bunch of warnings
//
// Revision 1.37  2009/03/23 21:08:32  mattheww
// Update default BPRS ZS handling and fix EEMC minor bug
//
// Revision 1.36  2009/02/26 12:00:40  mattheww
// added token check to BTOW header check
//
// Revision 1.35  2009/02/11 22:50:44  mattheww
// fixed a bug in getting CAP
//
// Revision 1.34  2009/02/11 22:38:56  mattheww
// fixed a bug in getting CAP
//
// Revision 1.33  2009/02/04 21:05:42  kocolosk
// Refactor StEEmcDb(Maker), new location for StEmcDecoder. Fixes RT #1388.
//
// Revision 1.32  2009/02/02 15:55:23  mattheww
// removed some debugging prints not taken out from last commit
//
// Revision 1.31  2009/01/28 15:42:44  mattheww
// Put back some obsolete methods to satisfy StBemcData
//
// Revision 1.30  2009/01/27 19:58:36  mattheww
// Updates to StEmcRawMaker to be compatible with 2009 DAQ Format
//
// Revision 1.29  2008/12/02 19:31:46  mattheww
// fixed bug in BPRS swap logic for run 8+
//
// Revision 1.28  2008/11/07 22:37:55  mattheww
// update date for bprs swap fix
//
// Revision 1.27  2008/10/24 18:19:06  mattheww
// Added option to throw out all hits in an event if any crates are corrupted
//
// Revision 1.26  2008/07/28 12:51:45  mattheww
// Minor bug fix in StBemcRaw::createDecoder
//
// Revision 1.25  2008/07/24 15:08:30  mattheww
// Minor bug fix and exception handling in setCheckStatus
//
// Revision 1.24  2008/07/23 23:53:24  mattheww
// Changed default status check mode to off
//
// Revision 1.23  2008/07/03 20:58:49  mattheww
// Added checking of every status table for each hit. Status table checks can be toggled using an option added to setCheckStatus. Also fixed a small bug.
//
// Revision 1.22  2008/06/12 13:25:32  mattheww
// Added a check on the pedestal status in makeHit
//
// Revision 1.21  2007/09/10 22:21:41  kocolosk
// Support for new BPRS swap fixes (off by default for 06/07 production, on for analysis).
// StBemcTables now matches map fixes in case end users want to use this copy.
//
// Revision 1.20  2007/01/22 19:13:37  kocolosk
// use STAR logger for all output
//
// Revision 1.19  2006/08/07 02:29:16  kocolosk
// one more change to allow saving CAP==127||128 via control table
//
// Revision 1.18  2006/08/07 01:58:06  kocolosk
// save hits from other CAPs using control table, can't comment out code b/c ADCtoEMaker needed it
//
// Revision 1.17  2006/08/04 12:54:10  kocolosk
// don't throw away CAP==127||128 PRS and SMD hits this year
//
// Revision 1.16  2006/08/01 17:07:07  kocolosk
// save all preshower hits for 2006 productions
//
// Revision 1.15  2006/01/16 11:12:00  suaide
// tower map bug fixed and astyle run
//
// Revision 1.14  2005/05/20 01:48:59  suaide
// small bug in the capacito assignement is fixed
//
// Revision 1.13  2005/02/02 11:09:59  suaide
// crate Id check is back!
//
// Revision 1.12  2005/01/14 12:20:31  suaide
// small bug fixed for the PSD
//
// Revision 1.11  2005/01/07 20:33:18  suaide
// created a new method to correct for the PSD map problem
//
// Revision 1.10  2004/12/21 12:53:48  suaide
// moved StBemcTables to StEmcUtil
// corrected for y2005 PSD data banks
//
// Revision 1.9  2004/12/14 11:32:11  suaide
// added histograms for status tables creation
//
// Revision 1.8  2004/11/22 12:46:22  suaide
// added new flags for hit reconstruction. Status are not checked
// dureing production anymore in order to avoid bad status loaded in
// DB
//
// Revision 1.7  2004/11/12 21:17:56  suaide
// non initialization of some variables were fixed
//
// Revision 1.6  2004/11/02 03:23:09  suaide
// small changes in order to fix a bug
//
// Revision 1.5  2004/10/21 00:01:42  suaide
// small changes in histogramming and messages for BEMC
// Complete version for EEMC done by Jan Balewski
//
// Revision 1.4  2004/10/20 15:45:19  suaide
// few bugs fixed
//
// Revision 1.3  2004/10/20 14:24:21  suaide
// small fix to crateUnknown status in old files
//
// Revision 1.2  2004/10/19 17:53:00  suaide
// code clean up
//
// Revision 1.1  2004/10/18 18:20:06  suaide
// New Maker. Will replace StEmcADCtoEMaker in production.
// It reads only DAQ structures. Output is StEvent.
//
#include "StBemcRaw.h"
#include "Stiostream.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "TString.h"
#include "StEventTypes.h"
#include "StEvent.h"
// DAQ Libraries
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/EMC/EMC_Reader.hh"
#include "StDAQMaker/StDAQReader.h"
#include "DAQ_READER/daq_det.h"
#include "DAQ_BTOW/daq_btow.h"
#include "DAQ_BSMD/daq_bsmd.h"
#include "DAQ_ETOW/daq_etow.h"
#include "DAQ_ESMD/daq_esmd.h"
#include "DAQ_EMC/daq_emc.h"
#include "StEmcUtil/database/StEmcDecoder.h"
#include "StEmcRawMaker.h"
#include "StMessMgr.h"
#include "TGenericTable.h"
//#include "StChain/StRTSBaseMaker.h"
#include "StChain/StRtsTable.h"

ClassImp(StBemcRaw)

//_____________________________________________________________________________
/*
   Default constructor. Set Initial values for some variables
*/
StBemcRaw::StBemcRaw()
{
    mSaveAllStEvent = kFALSE;
    mPsdMapBug = kFALSE;
    mPsdMapBug2 = kFALSE;
    mTowerMapBug = kFALSE;
    mSmdMapBug = kFALSE;
    mProdVer = "";
    mDecoder = 0;
    mDate = 0;
    mTime = 0;
    mTables = new StBemcTables();
    mControlADCtoE = new controlADCtoE_st();
    Int_t   calib[]      = {1, 1, 1, 1, 0, 0, 0, 0};
    Int_t   pedSub[]     = {1, 0, 0, 0, 0, 0, 0, 0};//default no bsmd pedsub
    Float_t cut[]        = {-1, -1, 1.5, 1.5, -1, -1, -1, -1};
    //cutType will now specify which daq data block to use
    //cut will now do nothing
    Int_t   cutType[]    = {0, 1, 1, 1, 0, 0, 0, 0};//default zs bsmd
    Int_t   onlyCal[]    = {0, 0, 0, 0, 0, 0, 0, 0};
    Int_t   status[]     = {0, 0, 0, 0, 0, 0, 0, 0};
    Int_t   crate[]      = {1, 1, 1, 1, 0, 0, 0, 0};

    mCrateVeto = 0;
    mAnyCorrupted = kFALSE;

    for(Int_t i=0; i<MAXDETBARREL; i++)
    {
        mControlADCtoE->Calibration[i]=calib[i];
        mControlADCtoE->DeductPedestal[i]=pedSub[i];
        mControlADCtoE->CutOff[i]=cut[i];
        mControlADCtoE->CutOffType[i]=cutType[i];
        mControlADCtoE->OnlyCalibrated[i]=onlyCal[i];
        mControlADCtoE->CheckStatus[i]=status[i];
        mControlADCtoE->CheckCrate[i]=crate[i];
        mBarrelQAHisto[i] = 0;
	for(int j = 0; j < 4; j++){
	  mCheckStatus[i][j]=0;
	}
    }
    assert(mControlADCtoE->CutOffType[2]==mControlADCtoE->CutOffType[3]);

}
//_____________________________________________________________________________
/*!
   Default destructor
*/
StBemcRaw::~StBemcRaw()
{
    if(mTables)
        delete mTables;
    if(mDecoder)
        delete mDecoder;
    if(mControlADCtoE)
        delete mControlADCtoE;
}
void StBemcRaw::printConf()
{
    LOG_INFO <<"Configuration for BEMC hit reconstruction "<<endm;
    for(Int_t i=0;i<MAXDETBARREL;i++)
    {
        LOG_INFO <<"  Configuration for detector "<<detname[i].Data()<<endm;
        LOG_INFO <<"     switch for deducting pedestal     = "<<mControlADCtoE->DeductPedestal[i]<<endm;
        LOG_INFO <<"     switch for calibration            = "<<mControlADCtoE->Calibration[i]<<endm;
        LOG_INFO <<"     cutoff type                       = "<<mControlADCtoE->CutOffType[i]<<endm;
        LOG_INFO <<"     cutoff value                      = "<<mControlADCtoE->CutOff[i]<<endm;
        LOG_INFO <<"     save only calibrated hits         = "<<mControlADCtoE->OnlyCalibrated[i]<<endm;
        LOG_INFO <<"     save only if status is ok         = "<<mControlADCtoE->CheckStatus[i]<<endm;
        LOG_INFO <<"     save only if crate is ok          = "<<mControlADCtoE->CheckCrate[i]<<endm;
        LOG_INFO <<"     SAVE ALL FLAG (overwrites above)  = "<<(Int_t)mSaveAllStEvent<<endm;
    }
}
void StBemcRaw::createDecoder(Int_t date, Int_t time)
{
    if(mDecoder)
        delete mDecoder;
    mDecoder = new StEmcDecoder(date,time);
    mDate = date;
    mTime = time;
}
void StBemcRaw::initHisto()
{
    mBarrelNHitHist         = new TH2F("BarrelNHit","BarrelNHit",500,0.0,18000.0,4,0.5,4.5);
    mBarrelEtotHist         = new TH2F("BarrelEtot","BarrelEtot",500,0.0,10000.0,4,0.5,4.5);
    mBarrelAdcSumHist       = new TH2F("BarrelAdcSum","BarrelAdcSum",500,0.0,1000000.0,4,0.5,4.5);
    mBarrelNCratesHist      = new TH2F("BarrelNCrates","BarrelNCrates",31,0.0,31.0,4,0.5,4.5);
    mBarrelCrateStatusHist  = new TH2F("BarrelCrateStatus","BarrelCrateStatus",6,-0.5,5.5,30,0.5,30.5);
}
void StBemcRaw::initQAHisto()
{
    for(Int_t det = 1;det<=MAXDETBARREL; det++)
    {
        Int_t N = BTOWCH;
        if(det>2)
            N= BSMDCH;
        mBarrelQAHisto[det-1] = new TH2F(detname[det-1].Data(),detname[det-1].Data(),N,0.5,(Float_t)N+0.5,250,-0.5,249.5);
    }
}
void StBemcRaw::fillHisto()
{
    for(Int_t det = 1;det<=MAXDETBARREL; det++)
    {
        mBarrelNHitHist->Fill(getTotalSaved(det),det);
        mBarrelEtotHist->Fill(getTotalE(det),det);
        mBarrelAdcSumHist->Fill(getTotalADC(det),det);
        mBarrelNCratesHist->Fill(getNCratesOK(det),det);
    }
    for(Int_t crate = 1;crate<=MAXCRATES; crate++)
    {
        mBarrelCrateStatusHist->Fill(getCrateStatus(BTOW,crate),crate);
    }
}
Bool_t StBemcRaw::make(StEmcRawMaker * TheData, StEvent* event)
{
    if(!TheData)
        return kFALSE;
    if(!event)
        return kFALSE;
    StEmcCollection* emc = event->emcCollection();
    if(!emc)
        return kFALSE;
    StEmcRawData *bemcRaw = emc->bemcRawData();
    if(!bemcRaw)
        return kFALSE;
    if(!convertFromDaq(TheData,bemcRaw))
        return kFALSE;
    return make(bemcRaw,event);
}
Bool_t StBemcRaw::convertFromDaq(StEmcRawMaker * DAQ, StEmcRawData* RAW)
{
    if(!DAQ)
    {
      LOG_ERROR <<"Could not find DAQ DataSet "<<endm;
      return kFALSE;
    }
    if(!RAW)
    {
      LOG_ERROR <<"Could not find StEmcRawData pointer for BEMC"<<endm;
      return kFALSE;
    }

    StRtsTable* btow = DAQ->GetDaqElement("btow/adc");
    if(btow){
      btow_t* btowdata = (btow_t*)*btow->begin();
      if(RAW->header(BTOWBANK))
	RAW->deleteBank(BTOWBANK);
      RAW->createBank(0,BTOWHEADER,BTOWSIZE);

      for(int i = 0; i < BTOW_MAXFEE; i++){
	for(int j = 0; j < BTOW_DATSIZE; j++){
	  int id = -1;
	  int daqid = -1;
	  mDecoder->GetTowerIdFromTDC(i,j,id);
	  mDecoder->GetDaqIdFromTowerId(id,daqid);
	  //printf("agrdl: BTOW ADC %d %d %d %d\n",i,j,id,btowdata->adc[i][j]);
	  RAW->setData(BTOWBANK,daqid,btowdata->adc[i][j]);
	}
	for(int j = 0; j < BTOW_PRESIZE; j++){
	  RAW->setHeader(BTOWBANK,i+j*30,btowdata->preamble[i][j]);
	  int crate;
	  mDecoder->GetTowerCrateFromTDC(i,crate);
	  //printf("agrdl: BTOW HEAD %d %d %d\n",crate,j,btowdata->preamble[i][j]);
	}
      }
    }else{
      LOG_ERROR<<"BTOW Structure not found"<<endm;
    }

    while(DAQ->GetDaqElement("bsmd/adc")){
      if(mDate < 20081101)continue;//Old daq files do not have this block
      int rdo = DAQ->Rdo();
      int use = 0;
      if(rdo < 9 && mControlADCtoE->CutOffType[2])use=1;
      if(rdo >=9 && mControlADCtoE->CutOffType[1])use=1;
      bsmd_t* bsmddata = (bsmd_t*)*DAQ->Dta()->begin();
      if(use){
	if(RAW->header(rdo))
	  RAW->deleteBank(rdo);
	RAW->createBank(rdo,1,BSMD_DATSIZE);
	RAW->setHeader(rdo,0,(unsigned short)bsmddata->cap);
	for(int j = 0; j < BSMD_DATSIZE; j++){
	  RAW->setData(rdo,j,bsmddata->adc[j]);
	}
      }
    }

    while(DAQ->GetDaqElement("bsmd/adc_non_zs")){
      int rdo = DAQ->Rdo();
      int use = 0;
      if((rdo < 9 && !mControlADCtoE->CutOffType[2]) || mDate < 20081101)use=1;
      if((rdo >= 9 && !mControlADCtoE->CutOffType[1]) || mDate < 20081101)use=1;
      if(use){
	//printf("agrdl: entered smd rdo: %d\n",rdo);
	bsmd_t* bsmddata = (bsmd_t*)*DAQ->Dta()->begin();
	if(RAW->header(rdo))
	  RAW->deleteBank(rdo);
	RAW->createBank(rdo,1,BSMD_DATSIZE);
	RAW->setHeader(rdo,0,(unsigned short)bsmddata->cap);
	//printf("agrdl: BSMD %d CAP %d\n",rdo,(unsigned short)bsmddata->cap);
	for(int j = 0; j < BSMD_DATSIZE; j++){
	  RAW->setData(rdo,j,bsmddata->adc[j]);
	  //printf("agrdl: BSMD ADC %d %d %d\n",rdo,j,bsmddata->adc[j]);
	}
      }
    }
    return kTRUE;
}
//-------------------------------------------------------
Bool_t StBemcRaw::make(StEmcRawData* bemcRaw, StEvent* event)
{
    if(!bemcRaw)
        return kFALSE;
    if(!event)
        return kFALSE;
    StEmcCollection* emc = event->emcCollection();
    if(!emc)
        return kFALSE;

    checkHeaders(bemcRaw, event);
    emptyEmcCollection(emc);

    Int_t cap=0,crate=0;
    Int_t ADC=0;
    Float_t E=0;

    for(Int_t det = 1; det<=MAXDETBARREL; det++)
    {
        Int_t nch = BTOWCH;
        if(det>2)
            nch=BSMDCH;

        clearStats(det);
        for(Int_t id = 1; id<=nch; id++)
        {
            ADC = getBemcADCRaw(det,id,bemcRaw,crate,cap);
            Int_t S = makeHit(emc,det,id,ADC,crate,cap,E);
            updateStats(det,S,ADC,E);
        }
        printStats(det);
        StDetectorId did = static_cast<StDetectorId>(det+kBarrelEmcTowerId-1);
        StEmcDetector* detector=emc->detector(did);
        if(detector)
        {
            for(Int_t crate = 1;crate<=MAXCRATES;crate++)
                detector->setCrateStatus(crate,(StEmcCrateStatus)mCrateStatus[det-1][crate-1]);
        }
    }

    return kTRUE;
}
//_____________________________________________________________________________
void StBemcRaw::checkHeaders(StEmcRawData* RAW, StEvent* event)
{
    for(Int_t det=1;det<=MAXDETBARREL; det++)
    {
        for(Int_t crate = 1; crate<=MAXCRATES;crate++)
            mCrateStatus[det-1][crate-1] = crateUnknown;
        mIsCorrupted[det-1] = kFALSE;
    }
    checkBtowCrates(RAW,event);

    mNCRATESOK[BPRS-1]=mNCRATESOK[BSMDE-1]=mNCRATESOK[BSMDP-1]=0;
    // smd data
    for(Int_t i = 0; i<MAXSMDCRATES; i++)
    {
        UShort_t *header = RAW->header(i+BSMDOFFSET);
        if(header)
        {
            mCrateStatus[BSMDE-1][i] = crateOK;
            mCrateStatus[BSMDP-1][i] = crateOK;
            mNCRATESOK[BSMDE-1]++;
            mNCRATESOK[BSMDP-1]++;
        }
    }
    // PSD data
    for(Int_t i = 0; i<MAXBPRSCRATES; i++)
    {
        UShort_t *header = RAW->header(i+BPRSOFFSET);
        if(header)
        {
            mCrateStatus[BPRS-1][i] = crateOK;
            mNCRATESOK[BPRS-1]++;
        }
    }
    for(Int_t i = 0; i < MAXDETBARREL; i++){
      if(mIsCorrupted[i])mAnyCorrupted = kTRUE;
    }
}
void StBemcRaw::emptyEmcCollection(StEmcCollection *emc)
{
    if(!emc)
        return;
    StSPtrVecEmcPoint& pvec = emc->barrelPoints();
    if(pvec.size()>0)
        pvec.clear();

    for(Int_t i=0; i<MAXDETBARREL; i++)
    {
        StDetectorId id = static_cast<StDetectorId>(i+kBarrelEmcTowerId);
        StEmcDetector* detector=emc->detector(id);
        if(detector)
        {
            if(detector->cluster())
            {
                StSPtrVecEmcCluster& cluster=detector->cluster()->clusters();
                if(cluster.size()>0)
                    cluster.clear();
            }
            for(UInt_t j=1;j<=detector->numberOfModules() ;j++)
            {
                StEmcModule *module = detector->module(j);
                if(module)
                {
                    StSPtrVecEmcRawHit&  hits=module->hits();
                    hits.clear();
                }
            }
        }
    }
    return;
}
//_____________________________________________________________________________
/*!
  Check tower crates header
*/
void StBemcRaw::checkBtowCrates(StEmcRawData* RAW, StEvent* event)
{
    if(!RAW)
        return;
    if(!mDecoder)
        return;
    UShort_t *header = RAW->header(BTOWBANK);
    if(!header)
        return;
    mNCRATESOK[0] = 0;
    int trgtoken = event->l0Trigger()->triggerToken();
    for(Int_t crate = 1;crate<=MAXCRATES;crate++)
    {
        Int_t TDC;
        mDecoder->GetTowerTDCFromCrate(crate,TDC);
        Int_t sum = header[TDC];
	Int_t token = header[TDC+BTOWTOKENOFFSET];
        Int_t err = header[TDC+BTOWTDCERROFFSET];
        Int_t crateFromHeader = header[TDC+BTOWCRATEOFFSET]& 0x0FF;
        mCrateStatus[BTOW-1][crate-1] = crateUnknown;
        if(sum==BTOWBYTESUM && err == BTOWERRFLAG && crate==crateFromHeader && token == trgtoken)
            mCrateStatus[BTOW-1][crate-1] = crateOK;
        else
            mCrateStatus[BTOW-1][crate-1] = crateHeaderCorrupt;

        if(sum==BTOWNOTPRESENT && err == BTOWNOTPRESENT)
            mCrateStatus[BTOW-1][crate-1] = crateNotPresent;

        if(mCrateStatus[BTOW-1][crate-1]==crateOK)
            mNCRATESOK[BTOW-1]++;

        if(mCrateStatus[BTOW-1][crate-1]==crateHeaderCorrupt)
            mIsCorrupted[BTOW-1] = kTRUE;
    }
    return;
}
void StBemcRaw::clearStats(Int_t det)
{
    mNZ[det-1] = 0;
    mNCRATE[det-1] = 0;
    mNSTATUS[det-1] = 0;
    mNRMS[det-1] = 0;
    mNPED[det-1] = 0;
    mNOK[det-1] = 0;
    mNTOTAL[det-1] = 0;
    mADCSUM[det-1] = 0;
    mTOTALE[det-1] = 0;
}
void StBemcRaw::updateStats(Int_t det,Int_t S,Int_t ADC, Float_t E)
{
    if(S==kZero)
        mNZ[det-1]++;
    else if(S==kCrate)
        mNCRATE[det-1]++;
    else if(S==kStatus)
        mNSTATUS[det-1]++;
    else if(S==kRms)
        mNRMS[det-1]++;
    else if(S==kPed)
        mNPED[det-1]++;
    else if(S==kOK)
        mNOK[det-1]++;
    mNTOTAL[det-1]++;
    if(S==kOK)
    {
        mADCSUM[det-1]+=ADC;
        mTOTALE[det-1]+=E;
    }
}
void StBemcRaw::printStats(Int_t det)
{
    LOG_DEBUG <<"Statistics for detector  "<<detname[det-1].Data()<<endm;
    LOG_DEBUG <<"   Total number of crates with header ok = "<<mNCRATESOK[det-1]<<endm;
    LOG_DEBUG <<"   Total number of hits                  = "<<mNTOTAL[det-1]<<endm;
    LOG_DEBUG <<"   Total hits removed because of crates  = "<<mNCRATE[det-1]<<endm;
    LOG_DEBUG <<"   Total hits removed because ADC = 0    = "<<mNZ[det-1]<<endm;
    LOG_DEBUG <<"   Total hits removed by Pedestal        = "<<mNPED[det-1]+mNRMS[det-1]<<endm;
    LOG_DEBUG <<"   Total hits removed by Status          = "<<mNSTATUS[det-1]<<endm;
    LOG_DEBUG <<"   Total number of hits saved            = "<<mNOK[det-1]<<endm;
    LOG_DEBUG <<"   Total ADCSUM of valid hits            = "<<mADCSUM[det-1]<<endm;
    LOG_DEBUG <<"   Total Energy of valid hits            = "<<mTOTALE[det-1]<<endm;
}
//_____________________________________________________________________________
/*!
  Get BEMC ADC Value from StEmcRawData
*/
Int_t StBemcRaw::getBemcADCRaw(Int_t det, Int_t softId, StEmcRawData* RAW, Int_t& CRATE, Int_t& CAP)
{
    CAP = 0;
    CRATE = 0;
    if(!RAW)
    {
        LOG_WARN <<"Could not find StEmcRawData pointer for BEMC det: " << det << " softId: " << softId <<endm;
        return 0;
    }
    if(!mDecoder)
    {
        LOG_WARN <<"Could not find StEmcDecoder pointer for BEMC det: " << det << " softId: " << softId <<endm;
        return 0;
    }
    if(det==BTOW) // tower
    {
        Int_t daq;
        if(mDecoder->GetDaqIdFromTowerId(softId,daq)==1 && RAW->header(BTOWBANK))
        {
            Int_t CR,INDEX;
            mDecoder->GetTowerCrateFromDaqId(daq,CR,INDEX);
            CRATE = CR;
            CAP = 0;
            return RAW->data(BTOWBANK,daq);
        }
        return 0;
    }
    else if(det==BPRS) // PSD
    {
        Int_t RDO=0,index=0;
        Int_t S = mDecoder->GetPsdRDO(softId,RDO,index);
        CRATE = RDO+1;
        //cout <<det<<"  "<<RDO<<" "<<softId<<"   "<<RAW->header(RDO+BPRSOFFSET)<<endl;
        if(RDO<0 || RDO>=MAXBPRSCRATES)
            return 0;
        if(S==1 && RAW->header(RDO+BPRSOFFSET))
        {
            if(mDate < 20081101)CAP = RAW->header(RDO+BPRSOFFSET,SMDCAPACITOR);
	    else CAP = RAW->header(RDO+BPRSOFFSET,0);
            while(CAP>127)
                CAP-=128;
            return RAW->data(RDO+BPRSOFFSET,index);
        }
        return 0;
    }
    else if(det==BSMDE) // SMDE
    {
        StEmcGeom *geo = StEmcGeom::instance("bsmde");
        Int_t m=0,e=0,s=0;
        if(geo->getBin(softId,m,e,s)==1)
            return 0;
        Int_t RDO=0,index=0;
        Int_t S = mDecoder->GetSmdRDO(BSMDE,m,e,s,RDO,index);
        CRATE = RDO+1;
        if(S==1 && RAW->header(RDO+BSMDOFFSET) && RDO>=0 && RDO<MAXSMDCRATES)
        {
	  if(mDate < 20081101)CAP = RAW->header(RDO+BSMDOFFSET,SMDCAPACITOR);
	  else CAP = RAW->header(RDO+BSMDOFFSET,0);
            while(CAP>127)
                CAP-=128;
            return RAW->data(RDO+BSMDOFFSET,index);
        }
        return 0;
    }
    else if(det==BSMDP) // SMDP
    {
        StEmcGeom *geo = StEmcGeom::instance("bsmdp");
        Int_t m=0,e=0,s=0;
        if(geo->getBin(softId,m,e,s)==1)
            return 0;
        Int_t RDO=0,index=0;
        Int_t S = mDecoder->GetSmdRDO(BSMDP,m,e,s,RDO,index);
        CRATE = RDO+1;
        if(S==1 && RAW->header(RDO+BSMDOFFSET) && RDO>=0 && RDO<MAXSMDCRATES)
        {
 	  if(mDate < 20081101)CAP = RAW->header(RDO+BSMDOFFSET,SMDCAPACITOR);
	  else CAP = RAW->header(RDO+BSMDOFFSET,0);
             while(CAP>127)
                CAP-=128;
            return RAW->data(RDO+BSMDOFFSET,index);
        }
        return 0;
    }
    return 0;
}
//_____________________________________________________________________________
/*!
  Construct the StEmcRawHit. Checks if the hit is going to be saved, subtract
  pedestal and apply the calibration, if this is the case.
*/
Int_t StBemcRaw::makeHit(StEmcCollection* emc, Int_t det, Int_t id, Int_t ADC, Int_t CRATE, Int_t CAP,Float_t& E)
{
    E=0;
    
    if(det==BTOW && mTowerMapBug && mDate<20060101) // after this date the bug should be fixed
    {
        Int_t shift = 0;
        mDecoder->GetTowerBugCorrectionShift(id,shift);
        id+=shift;
    }
    if(det==BPRS && mPsdMapBug2 && mDate<20071101)
    {
        Int_t shift = 0;
        mDecoder->GetPreshowerBugCorrectionShift(id,shift);
        id+=shift;
    }
    if(det==BSMDE && mSmdMapBug && mDate>=20100101 && mDate<20110101 && (!mProdVer.compare("SL10h") || !mProdVer.compare("SL10i") || !mProdVer.compare("SL10j")))
    {
        Int_t shift = 0;
        mDecoder->GetSmdBugCorrectionShift(id,shift);
        if(id+shift < 0) return kZero; //mask lost channels
        id+=shift;
    }
    if(CRATE>0 && CRATE<=MAXCRATES && mControlADCtoE->CheckCrate[det-1]==1)
        if((mCrateStatus[det-1][CRATE-1]!=crateOK &&
                mCrateStatus[det-1][CRATE-1]!=crateUnknown) &&
                !mSaveAllStEvent)
            return kCrate;

    if(!mSaveAllStEvent && mCrateVeto && mAnyCorrupted)
      return kCrate;

    if(ADC==0 && !mSaveAllStEvent)
        return kZero;

    if(mControlADCtoE->CheckStatus[det-1]==1)
    {
      if(mCheckStatus[det-1][0]==1 && !mSaveAllStEvent){
        Int_t STATUS;
        mTables->getStatus(det,id,STATUS);
        if(STATUS!=STATUS_OK && !mSaveAllStEvent)
            return kStatus;
      }
      if(mCheckStatus[det-1][1]==1 && !mSaveAllStEvent){
	Int_t pedSTATUS;
	mTables->getStatus(det,id,pedSTATUS,"pedestal");
	if(pedSTATUS!=STATUS_OK && !mSaveAllStEvent)return kStatus;
      }
      if(mCheckStatus[det-1][2]==1 && !mSaveAllStEvent){
	Int_t calibSTATUS;
	mTables->getStatus(det,id,calibSTATUS,"calib");
        if(calibSTATUS!=STATUS_OK && !mSaveAllStEvent)return kStatus;
      }
      if(mCheckStatus[det-1][3]==1 && !mSaveAllStEvent){
	Int_t gainSTATUS;
	mTables->getStatus(det,id,gainSTATUS,"gain");
	if(gainSTATUS!=STATUS_OK && !mSaveAllStEvent)return kStatus;
      }

    }

    Float_t PEDESTAL = 0,RMS = 0;
    mTables->getPedestal(det,id,CAP,PEDESTAL,RMS);

    if(mControlADCtoE->DeductPedestal[det-1]>0)
    {
        // do not consider hits wih capacitor number CAP1 and CAP2 for
        // PSD and SMD as valid hits
	// for 2006 keep these hits by setting DeductPedestal == 2
        if(mControlADCtoE->DeductPedestal[det-1]==1)
		if(det>=BPRS && !mSaveAllStEvent)
	            if(CAP==CAP1 || CAP==CAP2)
                return kPed;
    }

    if(mControlADCtoE->CutOffType[det-1]==1 && !mSaveAllStEvent && mControlADCtoE->DeductPedestal[det-1] > 0) // pedestal cut
    {
        if(RMS<=0)
            return kRms;
        Float_t x = (ADC-PEDESTAL)/RMS;
        if(x<=mControlADCtoE->CutOff[det-1])
            return kPed;
    }

    if(mControlADCtoE->Calibration[det-1]==1)
    {
        Float_t ADCP = 1;
        Float_t C;
        for(Int_t i=0;i<5;i++)
        {
            mTables->getCalib(det,id,i,C);
            E+=ADCP*C;
            ADCP*=(Float_t)(ADC-PEDESTAL);
        }
        mTables->getGain(det,id,C);
        E*=C;

        if(mControlADCtoE->CutOffType[det-1]==2 && !mSaveAllStEvent) // energy cut
        {
            if(E<mControlADCtoE->CutOff[det-1])
                return kEn;
        }
    }

    if(mControlADCtoE->OnlyCalibrated[det-1]>0 && E==0 && !mSaveAllStEvent)
        return kCalib;

    StDetectorId did = static_cast<StDetectorId>(det+kBarrelEmcTowerId-1);
    StEmcDetector* detector=emc->detector(did);
    StEmcGeom *geo = StEmcGeom::instance(det);
    if(!detector)
    {
        detector = new StEmcDetector(did,BEMCMODULES);
        emc->setDetector(detector);
    }
    if(det==BPRS && mPsdMapBug && mDate<20060101) // after this date the map is fixed
    {
        Int_t PsdOffset_bug[40] = {20,21,22,23,0,1,2,3,24,25,26,27,4,5,6,7,28,29,30,31,
                                   8,9,10,11,32,33,34,35,12,13,14,15,36,37,38,39,16,17,18,19};
        Int_t PsdOffset_ok[40]  = {36,37,38,39,16,17,18,19,32,33,34,35,12,13,14,15,28,29,30,31,
                                   8,9,10,11,24,25,26,27,4,5,6,7,20,21,22,23,0,1,2,3};
        Int_t RDO,index,PMT,wire,A_value;
        mDecoder->GetPsdRDO(id,RDO,index);
        mDecoder->GetPsdId(RDO,index,id,PMT,wire,A_value);
        //int oldId = id;
        id-=PsdOffset_bug[wire-1];
        id+=PsdOffset_ok[wire-1];
        //cout <<"PSD old id = "<<oldId<<"  new id = "<<id<<"  wire = "<<wire<<endl;
    }
    Int_t m,e,s;
    geo->getBin(id,m,e,s);
    StEmcRawHit* hit=new StEmcRawHit(did,m,e,s,(UInt_t)ADC);
    if(mBarrelQAHisto[det-1] && !isCorrupted(det))
        mBarrelQAHisto[det-1]->Fill((Float_t)id,(Float_t)ADC);
    hit->setEnergy(E);
    hit->setCalibrationType(CAP);
    detector->addHit(hit);
    return kOK;
}

void StBemcRaw::setCheckStatus(Int_t det, Int_t flag, const char* option)
{
  //change the status if a particular option is chosen
  if(!strcmp(option,"status")){
    mCheckStatus[det][0]=flag;
    return;
  }
  if(!strcmp(option,"pedestal")){
    mCheckStatus[det][1]=flag;
    return;
  }
  if(!strcmp(option,"calib")){
    mCheckStatus[det][2]=flag;
    return;
  }
  if(!strcmp(option,"gain")){
    mCheckStatus[det][3]=flag;
    return;
  }

  if(strcmp(option,"")){
    LOG_WARN<<option<<" is not a valid option to setCheckStatus"<<endm;
    return;
  }

  //if no options chosen, change all status flags
  for(int i = 0; i < 4; i++){
    mCheckStatus[det][i]=flag;
  }

  getControlTable()->CheckStatus[det] = flag;
  return;

}
void StBemcRaw::setCrateVeto(Int_t flag)
{
  if(flag != 0 && flag != 1){
    LOG_ERROR<<"Invalid flag passed to StBemcRaw::setCrateVeto, only 0 and 1 accepted"<<endm;
    return;
  }
  mCrateVeto = flag;
}
Bool_t StBemcRaw::make(TDataSet* TheData, StEvent* event)
{
  LOG_WARN<<"StBemcRaw::make(TDataSet*,StEvent*) is OBSOLETE for data in Run 9 or later"<<endm;
    if(!TheData)
        return kFALSE;
    if(!event)
        return kFALSE;
    StEmcCollection* emc = event->emcCollection();
    if(!emc)
        return kFALSE;
    StEmcRawData *bemcRaw = emc->bemcRawData();
    if(!bemcRaw)
        return kFALSE;
    if(!convertFromDaq(TheData,bemcRaw))
        return kFALSE;
    return make(bemcRaw,event);
}
Bool_t StBemcRaw::convertFromDaq(TDataSet* DAQ, StEmcRawData* RAW)
{
  LOG_WARN<<"StBemcRaw::convertFromDaq(TDataSet*,StEmcRawData*) is OBSOLETE for data in Run 9 or later"<<endm; 
    if(!DAQ)
    {
        LOG_ERROR <<"Could not find DAQ DataSet "<<endm;
        return kFALSE;
    }
    if(!RAW)
    {
                LOG_ERROR <<"Could not find StEmcRawData pointer for BEMC"<<endm;
        return kFALSE;
    }

    StDAQReader* TheDataReader=(StDAQReader*)(DAQ->GetObject());
    if(!TheDataReader || !TheDataReader->EMCPresent())
    {
        LOG_ERROR <<"Data Reader is not present "<<endm;
        return kFALSE;
    }

    StEMCReader* TheEmcReader=TheDataReader->getEMCReader();
    if(!TheEmcReader)
    {
        LOG_ERROR <<"Could not find BEMC Reader "<<endm;
        return kFALSE;
    }

    EMC_Reader* reader = TheEmcReader->getBemcReader();
    if(!reader)
    {
        LOG_ERROR <<"Could not find Barrel Reader "<<endm;
        return kFALSE;
    }

    if(reader->isTowerPresent())
    {
        Bank_BTOWERADCR& tower = reader->getBTOWERADCR();
        if(RAW->header(BTOWBANK))
            RAW->deleteBank(BTOWBANK);
        RAW->createBank(0,BTOWHEADER,BTOWSIZE);
        for(Int_t i = 0; i<BTOWSIZE ;i++){
          int crate,sequence,tdc;
          int id;
          mDecoder->GetTowerIdFromDaqId(i,id);
          mDecoder->GetCrateFromTowerId(id,crate,sequence);
          mDecoder->GetTDCFromTowerId(id,tdc);
            RAW->setData(BTOWBANK,i,tower.TowerADCArray[i]);
            //printf("agrdl: BTOW ADC %d %d %d %d\n",tdc,sequence,id,tower.TowerADCArray[i]);
        }
        for(Int_t i = 0; i<BTOWHEADER  ;i++){
            RAW->setHeader(BTOWBANK,i,tower.TDCHeader[i]);
            //printf("agrdl: BTOW HEAD %d %d\n",i,tower.TDCHeader[i]);
        }
    }
    // smd data
    if(reader->isSmdPresent())
    {
        Bank_BSMDADCR& smd =  reader->getSMD_ADCR();
        Int_t NSMD = MAXSMDCRATES;
        // there is only 4 SMD Crates before that data and some
        // of them are PSD crates. For Y2004 AuAu runs PSD do
        // not have its own data format and it is being read as
        // SMD
        if(mDate<20040701)
            NSMD = 4;

        for(Int_t i = 0; i<NSMD; i++)
        {
            if(smd.HasData[i]==1)
            {
                Int_t bank = i+BSMDOFFSET;
                if(RAW->header(bank))
                    RAW->deleteBank(bank);
                RAW->createBank(bank,BSMDHEADER,BSMDSIZE);
                for(Int_t j=0; j<BSMDHEADER;  j++)
                    RAW->setHeader(bank,j,smd.SmdHeader[i][j]);
		//int CAP = RAW->header(bank,SMDCAPACITOR);
                    //printf("agrdl: BSMD %d CAP %d\n",bank,CAP);
                for(Int_t j=0; j<BSMDSIZE; j++){
                    RAW->setData(bank,j,smd.SMDADCArray[i][j]);
                    //printf("agrdl: BSMD ADC %d %d %d\n",bank,j,smd.SMDADCArray[i][j]);
                }
            }
        }
        /////////////////////////////////////////////////////////////////////
        // read Pre Shower data for Y2004 AuAu data. This year, the PSD data
        // is read as SMD data for fibers 4 and 5.
        //
        // For y2005 data, PSD data is on SMD banks 8 to 12
        //
        // This is a temporary solution while the PSD data format is not
        // decided by Tonko. He needs to have a decision on some
        // hardware issues before the data format is decided
        //
        // AAPSUAIDE 20040318
        //
        if(mDate>20040101)
        {
            for(Int_t RDO = 0; RDO<4; RDO++)
            {
                Int_t SMDRDO = RDO+NSMD;
                if(smd.HasData[SMDRDO]==1)
                {
                    Int_t bank = RDO+BPRSOFFSET;
                    if(RAW->header(bank))
                        RAW->deleteBank(bank);
                    RAW->createBank(bank,BPRSHEADER,BPRSSIZE);
                    for(Int_t i = 0; i<BPRSHEADER;  i++)
                        RAW->setHeader(bank,i,smd.SmdHeader[SMDRDO][i]);
                    //int CAP = RAW->header(bank,SMDCAPACITOR);
                    //printf("agrdl: BSMD %d CAP %d\n",bank,CAP);
                    for(Int_t i = 0; i<BPRSSIZE; i++){
                        RAW->setData(bank,i,smd.SMDADCArray[SMDRDO][i]);
                        //printf("agrdl: BSMD ADC %d %d %d\n",bank,i,smd.SMDADCArray[SMDRDO][i]);
                    }
                }
            }
        }
        /////////////////////////////////////////////////////////////////////
    }
    return kTRUE;
}
