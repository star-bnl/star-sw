//////////////////////////////////////////////////////////////////////////
//
// StEmcTriggerMaker A. A. P. Suaide (C) 2001
//
//   Update: 22-Feb-2002
//	     J.L. Klay (LBNL)
//
//   This class now creates histograms of the (10-bit to 6-bit compressed)
//   DAQ data and the TRG 6-bit ADC data so that comparisons
//   can be made.
//
//   In order to run on *event.root files, just load the library and call:
//     StEmcTriggerMaker* trigger=new StEmcTriggerMaker("bemctrigger");
//     trigger->SetHistFileName(outfile);
//
//   In order to run on *.daq files, make sure to load the St_trg_Maker
//   and StEmcCalibrationMaker libraries and then to call them in this
//   order:
//   	St_trg_Maker* trg=new St_trg_Maker("trigger");
//      trg->SetMode(1);
//      StEmcPreCalibrationMaker* precalib=new StEmcPreCalibrationMaker("precalib",1);
//      StEmcTriggerMaker* trigger=new StEmcTriggerMaker("bemctrigger");
//      trigger->SetHistFileName(outfile);
//
//////////////////////////////////////////////////////////////////////////

#include <Stiostream.h>
#include <math.h>
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StMaker.h"
#include "StEmcTriggerMaker.h"
#include "StEvent/StEvent.h"
#include "StEvent/StEventTypes.h"
#include "TFile.h"
#include "StEmcUtil/database/StBemcTables.h"
#include "StMessMgr.h"

ClassImp(StEmcTriggerMaker)

//_____________________________________________________________________________
StEmcTriggerMaker::StEmcTriggerMaker(const char *name):StMaker(name)
{
    mBemcTrigger = new StBemcTrigger();
    mSaveStEvent = true;
    mPrint = false;
    mHTBefore = NULL;
    mPABefore = NULL;
    mHT = NULL;
    mPA = NULL;
    mHTCorrel = NULL;
    mPACorrel = NULL;

    mIs2003HT1=-1;
    mIs2004HT1=-1;
    mIs2004JP1=-1;
    mIs2004HT2=-1;
    mIs2004JP2=-1;
    mIs2005HT1=-1;
    mIs2005JP1=-1;
    mIs2005HT2=-1;
    mIs2005JP2=-1;
    mIs2005ADJ=-1;
    for (int i=0;i<10;i++)
    {
        isTrig[i]=-1;
    }

    HT1_ID_2003=-1;
    HT1_ID_2004=-1;
    HT2_ID_2004=-1;
    JP1_ID_2004=-1;
    JP2_ID_2004=-1;
    HT1_ID_2005=-1;
    HT2_ID_2005=-1;
    JP1_ID_2005=-1;
    JP2_ID_2005=-1;
    ADJ_ID_2005=-1;
    for (int i=0;i<10;i++)
    {
        TowJetId[i] = -1;
    }

    HT1_DSM_2003=-1;
    HT1_DSM_2004=-1;
    HT2_DSM_2004=-1;
    JP1_DSM_2004=-1;
    JP2_DSM_2004=-1;
    HT1_DSM_2005=-1;
    HT2_DSM_2005=-1;
    JP1_DSM_2005=-1;
    JP2_DSM_2005=-1;
    ADJ_DSM_2005=-1;
    for (int i=0;i<10;i++)
    {
        DsmAdc[i] = -1;
    }

   

    for (int i=0;i<12;i++){
      JP12005array[i]=-1;
      JP22005array[i]=-1;
    }
    for (int i=0; i<4800; i++){
      HT12005array[i]=-1;
      HT22005array[i]=-1;
    }
    for (int i=0; i<5; i++) {
      numHT[i]=-1;
      numJP[i]=-1;
    }

}

//____________________________________________________________________________
StEmcTriggerMaker::~StEmcTriggerMaker()
{}
//_____________________________________________________________________________
Int_t StEmcTriggerMaker::Init()
{
    tables=new StBemcTables();

    LOG_INFO <<"StEmcTriggerMaker::Init()"<<endm;
    if (IAttr(".histos"))
    {
        mHTBefore = new TH2F("HighTower_DSM","High Tower trigger in DSM",300,-0.5,299.5,64,-0.5,63.5);
        mPABefore = new TH2F("Patch_DSM","Patch trigger in DSM",300,-0.5,299.5,64,-0.5,63.5);
        mHT       = new TH2F("HighTower","High Tower trigger",300,-0.5,299.5,64,-0.5,63.5);
        mPA       = new TH2F("Patch","Patch trigger",300,-0.5,299.5,64,-0.5,63.5);
        mHTCorrel = new TH2F("HighTower_Correl","High Tower trigger correlation",64,-0.5,63.5,64,-0.5,63.5);
        mPACorrel = new TH2F("Patch_Correl","Patch trigger correlation",64,-0.5,63.5,64,-0.5,63.5);
    }
    return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StEmcTriggerMaker::Make()
{

    LOG_DEBUG<<"StEmcTriggerMaker::Make()"<<endm;

    tables->loadTables(this);
    setTableMaker(tables);

    StEvent* event=(StEvent*)GetInputDS("StEvent");
    if(!event)
        return kStOk;

    mBemcTrigger->setPrint(mPrint);
    mBemcTrigger->setEvent(event);
    if(mBemcTrigger->makeTrigger() != kStOK)
    {
        LOG_WARN << "StEmcTriggerMaker::Make() -- trigger information is not filled! Something is wrong!" << endm;
    }

    int* isTrig = mBemcTrigger->isTrigEvent();
    int* TowJetId = mBemcTrigger->getTowPatchId();
    int* DsmAdc = mBemcTrigger->getTowPatchDSM();
    int* numHT = mBemcTrigger->getNHT();
    int* numJP = mBemcTrigger->getNJP();
    int* HT12005array = mBemcTrigger->getHT12005array();
    int* HT22005array = mBemcTrigger->getHT22005array();
    int* JP12005array = mBemcTrigger->getJP12005array();
    int* JP22005array = mBemcTrigger->getJP22005array();


    //2003 HT1 ==  1101
    mIs2003HT1=isTrig[0];
    HT1_ID_2003=TowJetId[0];
    HT1_DSM_2003=DsmAdc[0];

    //2004 HT1 == 45201
    mIs2004HT1=isTrig[1];
    HT1_ID_2004=TowJetId[1];
    HT1_DSM_2004=DsmAdc[1];

    //2004 HT2 == 45202
    mIs2004HT2=isTrig[2];
    HT2_ID_2004=TowJetId[2];
    HT2_DSM_2004=DsmAdc[2];

    //2004 JP1 == 45206
    mIs2004JP1=isTrig[3];
    JP1_ID_2004=TowJetId[3];
    JP1_DSM_2004=DsmAdc[3];

    //2004 JP2 == 45207
    mIs2004JP2=isTrig[4];
    JP2_ID_2004=TowJetId[4];
    JP2_DSM_2004=DsmAdc[4];

    //2005 HT1=96201
    mIs2005HT1=isTrig[5];
    HT1_ID_2005=TowJetId[5];
    HT1_DSM_2005=DsmAdc[5];
    numHT1_2005=numHT[3];
    for (int i=0;i<numHT1_2005;i++){
      HT1_2005_array[i]=HT12005array[i];
    }      

    //2005 HT2=96211
    mIs2005HT2=isTrig[6];
    HT2_ID_2005=TowJetId[6];
    HT2_DSM_2005=DsmAdc[6];
    numHT2_2005=numHT[4];
    for (int i=0;i<numHT2_2005;i++){
      HT2_2005_array[i]=HT22005array[i];
    }      

    //2005 JP1=96221
    mIs2005JP1=isTrig[7];
    JP1_ID_2005=TowJetId[7];
    JP1_DSM_2005=DsmAdc[7];
    numJP1_2005=numJP[2];
    for (int i=0;i<numJP1_2005;i++){
      JP1_2005_array[i]=JP12005array[i];
    }

    //2005 JP2=96233
    mIs2005JP2=isTrig[8];
    JP2_ID_2005=TowJetId[8];
    JP2_DSM_2005=DsmAdc[8];
    numJP2_2005=numJP[3];
    for (int i=0;i<numJP2_2005;i++){
      JP2_2005_array[i]=JP22005array[i];
    }

    //2005 ADJP = 96241
    mIs2005ADJ=isTrig[9];
    ADJ_ID_2005=TowJetId[9];
    ADJ_DSM_2005=DsmAdc[9];


    //access TP 6 bit DSMsum
    for (int j=0;j<300;j++)
    {
        trigPatch[j]=0;
        trigPatch[j]=mBemcTrigger->trgPatch[j];
    }

    if (IAttr(".histos"))
    {
        fillHistograms(event);
    }

    if(mSaveStEvent)
        fillStEvent(event);

    return kStOK;
}
//_____________________________________________________________________________
Int_t StEmcTriggerMaker::Finish()
{
    return StMaker::Finish();
}
//_____________________________________________________________________________
void StEmcTriggerMaker::fillHistograms(StEvent *event)
{
    emcTrigger emcTrg = mBemcTrigger->getTrigger();
    for(int i=0;i<300;i++)
    {
        if (mHT)
            mHT->Fill(i,emcTrg.HT[i]);
        if (mPA)
            mPA->Fill(i,emcTrg.Patch[i]);
    }

    // comparison with existing data in StTriggerData
    if(!event)
        return;
    StTriggerData* trg=event->triggerData();
    if(trg)
    {
        for(int i=0;i<300;i++)
        {
            if (mHTBefore)
                mHTBefore->Fill(i,trg->bemcHighTower(i));
            if (mPABefore)
                mPABefore->Fill(i,trg->bemcJetPatch(i));
            if (mHTCorrel)
                mHTCorrel->Fill(emcTrg.HT[i],trg->bemcHighTower(i));
            if (mPACorrel)
                mPACorrel->Fill(emcTrg.Patch[i],trg->bemcJetPatch(i));
        }
    }
    return;
}
//_____________________________________________________________________________
void StEmcTriggerMaker::saveHistograms(char* file)
{
    TFile *f = new TFile(file,"RECREATE");
    if (mHT)
        mHT->Write();
    if (mPA)
        mPA->Write();
    if (mHTBefore)
        mHTBefore->Write();
    if (mPABefore)
        mPABefore->Write();
    if (mHTCorrel)
        mHTCorrel->Write();
    if (mPACorrel)
        mPACorrel->Write();
    f->Close();
    delete f;
    return;
}

void StEmcTriggerMaker::get2005HT1_TOWS(int index, int *id){
  *id=-1;
  if (index<4800) *id=HT1_2005_array[index];
}

void StEmcTriggerMaker::get2005HT2_TOWS(int index, int *id){
  *id=-1;
  if (index<4800) *id=HT2_2005_array[index];
}

void StEmcTriggerMaker::get2005JP1_PATCHES(int index, int *id){
  *id=-1;
  if (index<12) *id=JP1_2005_array[index];
}

void StEmcTriggerMaker::get2005JP2_PATCHES(int index, int *id){
  *id=-1;
  if (index<12) *id=JP2_2005_array[index];
}


//_____________________________________________________________________________
void StEmcTriggerMaker::fillStEvent(StEvent *event)
{
    if(!event)
        return;
    StTriggerDetectorCollection* trg = event->triggerDetectorCollection();
    if(!trg)
    {
        trg = new StTriggerDetectorCollection();
        event->setTriggerDetectorCollection(trg);
    }
    StEmcTriggerDetector emc=trg->emc();
    emcTrigger emcTrg = mBemcTrigger->getTrigger();
    for(int i=0;i<300;i++)
    {
        emc.setHighTower(i,emcTrg.HT[i]);
        emc.setPatch(i,emcTrg.Patch[i]);
    }
    return;
}



