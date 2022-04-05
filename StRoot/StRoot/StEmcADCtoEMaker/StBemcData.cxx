#include "StBemcData.h"
#include "StEmcUtil/database/StEmcDecoder.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEventTypes.h"
#include "StEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"

ClassImp(StBemcData)

//_____________________________________________________________________________
/*
   Default constructor. Set Initial values for some variables
*/
StBemcData::StBemcData()
{
    Int_t   calib[]      = {1, 1, 1, 1, 0, 0, 0, 0};
    Int_t   pedSub[]     = {1, 1, 1, 1, 0, 0, 0, 0};
    Float_t cut[]        = {-1, 1.5, 1.5, 1.5, -1, -1, -1, -1};
    Int_t   cutType[]    = {0, 1, 1, 1, 0, 0, 0, 0};
    Int_t   onlyCal[]    = {0, 0, 0, 0, 0, 0, 0, 0};
    Int_t   status[]     = {1, 1, 1, 1, 0, 0, 0, 0};
    Int_t   crate[]      = {1, 1, 1, 1, 0, 0, 0, 0};

    for(Int_t i=0; i<MAXDETBARREL; i++)
    {
        mControlADCtoE->Calibration[i]=calib[i];
        mControlADCtoE->DeductPedestal[i]=pedSub[i];
        mControlADCtoE->CutOff[i]=cut[i];
        mControlADCtoE->CutOffType[i]=cutType[i];
        mControlADCtoE->OnlyCalibrated[i]=onlyCal[i];
        mControlADCtoE->CheckStatus[i]=status[i];
        mControlADCtoE->CheckCrate[i]=crate[i];
    }
    
    mPsdMapBug2 = kTRUE;
    mTowerMapBug = kTRUE;
    mSmdMapBug = kTRUE;   
 
    // correct DB tables for swapping
    delete mTables;
    mTables = new StBemcTables(kTRUE, kTRUE);

}
//_____________________________________________________________________________
/*!
   Default destructor
*/
StBemcData::~StBemcData()
{}
Bool_t StBemcData::make(StEmcRawData* emcraw, StEvent* event)
{
    return StBemcRaw::make(emcraw,event);
}
Bool_t StBemcData::make(TDataSet* DS, StEvent* event)
{
    return StBemcRaw::make(DS,event);
}
Bool_t StBemcData::make(StEmcCollection* emc, StEvent* event)
{
    if(!emc)
        return kFALSE;
    if(!event)
        return kFALSE;
    StEmcCollection *emcN = event->emcCollection();
    if(!emcN)
    {
        emcN = emc;
        event->setEmcCollection(emcN);
    }

    Int_t ADC[MAXDETBARREL][BSMDCH];
    Int_t CRATE[MAXDETBARREL][BSMDCH];
    Int_t CAP[MAXDETBARREL][BSMDCH];
    Int_t ID[MAXDETBARREL][BSMDCH];
    Int_t NH[MAXDETBARREL];
    Int_t Crate,RDO,Index,Daq;
    Int_t S;
    Float_t E;
    for(Int_t det=1;det<=MAXDETBARREL;det++)
    {
        StDetectorId id = static_cast<StDetectorId>(det+kBarrelEmcTowerId-1);
        NH[det-1] = 0;
        StEmcDetector* detector=emc->detector(id);
        mNCRATESOK[det-1]=0;
        if(detector)
        {
            mIsCorrupted[det-1]=kFALSE;
            for(Int_t crate=1;crate<=MAXCRATES;crate++)
            {
                mCrateStatus[det-1][crate-1] = (Int_t)detector->crateStatus(crate);
                if(mCrateStatus[det-1][crate-1]==crateOK)
                    mNCRATESOK[det-1]++;
                if(mCrateStatus[det-1][crate-1]==crateHeaderCorrupt)
                    mIsCorrupted[det-1] = kTRUE;
            }

            StEmcGeom* geo = StEmcGeom::instance(det);
            for(UInt_t j=1;j<=BEMCMODULES;j++)
            {
                StEmcModule* module = detector->module(j);
                if(module)
                {
                    StSPtrVecEmcRawHit& rawHit=module->hits();
                    for(UInt_t k=0;k<rawHit.size();k++)
                    {
                        Int_t m=rawHit[k]->module();
                        Int_t e=rawHit[k]->eta();
                        Int_t s=abs(rawHit[k]->sub());
                        Int_t adc=rawHit[k]->adc();
                        Int_t cap=rawHit[k]->calibrationType();
                        while(cap>127)
                            cap-=128;
                        Int_t id;
                        geo->getId(m,e,s,id);
                        ID[det-1][NH[det-1]]  = id;
                        ADC[det-1][NH[det-1]] = adc;
                        CAP[det-1][NH[det-1]] = cap;
                        if(det==BTOW)
                        {
                            mDecoder->GetDaqIdFromTowerId(id,Daq);
                            mDecoder->GetTowerCrateFromDaqId(Daq,Crate,Index);
                        }
                        else if(det==BPRS)
                        {
                            mDecoder->GetPsdRDO(id,RDO,Index);
                            Crate = RDO+1;
                        }
                        else if(det==BSMDE || det == BSMDP)
                        {
                            mDecoder->GetSmdRDO(det,m,e,s,RDO,Index);
                            Crate = RDO+1;
                        }
                        CRATE[det-1][NH[det-1]] = Crate;
                        NH[det-1]++;
                    }
                }
            }
        }
    }
    emptyEmcCollection(emcN);
    for(Int_t det=1;det<=MAXDETBARREL;det++)
    {
        clearStats(det);
        for(Int_t i = 0;i<NH[det-1];i++)
        {
            S = makeHit(emcN,det,ID[det-1][i],ADC[det-1][i],CRATE[det-1][i],CAP[det-1][i],E);
            updateStats(det,S,ADC[det-1][i],E);
        }
        printStats(det);
    }
    return kTRUE;
}
Bool_t StBemcData::make(StMuEmcCollection* muEmc, StEvent* event)
{
    if(!muEmc)
        return kFALSE;
    if(!event)
        return kFALSE;
    StEmcCollection *emc = event->emcCollection();
    if(!emc)
        return kFALSE;
    emptyEmcCollection(emc);

    for(Int_t det=1;det<=MAXDETBARREL;det++)
    {
        clearStats(det);
        mNCRATESOK[det-1]=0;
        StDetectorId did = static_cast<StDetectorId>(det+kBarrelEmcTowerId-1);
        StEmcDetector* detector=emc->detector(did);
        mIsCorrupted[det-1]=kFALSE;
        for(Int_t crate = 1;crate<=MAXCRATES;crate++)
        {
            mCrateStatus[det-1][crate-1] = (Int_t)muEmc->getCrateStatus(crate,det);
            if(mCrateStatus[det-1][crate-1]==crateOK)
                mNCRATESOK[det-1]++;
            if(mCrateStatus[det-1][crate-1]==crateHeaderCorrupt)
                mIsCorrupted[det-1] = kTRUE;
            if(detector)
                detector->setCrateStatus(crate,(StEmcCrateStatus)mCrateStatus[det-1][crate-1]);
        }
        StEmcGeom* geo = StEmcGeom::instance(det);
        Int_t nh=0;
        Int_t ADC=0,ID=0,CRATE=0,RDO=0,INDEX=0,CAP=0,DAQ=0;
        Int_t m=0,e=0,s=0;
        Float_t E=0;
        Int_t S=0;
        if (det==BTOW)
            nh = BTOWCH;
        if (det==BPRS)
            nh=muEmc->getNPrsHits(det);
        if (det==BSMDE || det==BSMDP)
            nh=muEmc->getNSmdHits(det);
        for(Int_t j=0;j<nh;j++)
        {
            ADC = 0;
            if(det==BTOW) // towers have only ADC
            {
                ID = j+1;
                ADC = muEmc->getTowerADC(ID,det);
                mDecoder->GetDaqIdFromTowerId(ID,DAQ);
                mDecoder->GetTowerCrateFromDaqId(DAQ,CRATE,INDEX);
                CAP = 0;
            }
            if(det==BPRS)
            {
                StMuEmcHit* hit=muEmc->getPrsHit(j,det);
                ID = hit->getId();
                ADC = hit->getAdc();
                CAP = hit->getCalType();
                mDecoder->GetPsdRDO(ID,RDO,INDEX);
                CRATE = RDO+1;
            }
            if(det==BSMDE || det==BSMDP) //smd
            {
                StMuEmcHit* hit=muEmc->getSmdHit(j,det);
                ID = hit->getId();
                ADC = hit->getAdc();
                CAP = hit->getCalType();
                geo->getBin(ID,m,e,s);
                mDecoder->GetSmdRDO(det,m,e,s,RDO,INDEX);
                CRATE = RDO+1;
            }
            S = makeHit(emc,det,ID,ADC,CRATE,CAP,E);
            updateStats(det,S,ADC,E);
        }
        printStats(det);
    }
    return kTRUE;
}
