///////////////////////////////////////////////////////////////////////////////////////
//
//
// StEmcTriggerMaker R. Fatemi (Oct 26, 2006)
//
// The structure of this class was first developed by J.Klay and A. Suaide in 2001.
// It was originally designed to fill StEvent with the simulated L0 trigger response
// but to my understanding was never fully implemented 
//
// Early in 2005, using code originally developed by Alex Stopolsky to emulate the BEMC
// FEE output, I expanded the code to return full BEMC L0 trigger emulation. This code
// was motivated by the need to run the same trigger algorithm over data and simulation.
// All DSM outputs are stored, for data only, in StTriggerDetector class. The ultimate
// design vision is that StEmcTriggerMaker serves as access to the StBemcTrigger and
// StEemcTrigger classes which mock up the BEMC/EEMC FEE + L0 DSM trigger algorithms.
// Interface to L2 should also take place in this class.
//
//
////////////////////////////////////////////////////////////////////////////////////////

#include <Stiostream.h>
#include "StChain.h"
#include "TFile.h"
#include <math.h>

#include "StMaker.h"
#include "St_DataSetIter.h"
#include "StEvent/StEvent.h"
#include "StEvent/StEventTypes.h"
#include "StEmcUtil/database/StBemcTables.h"

#include "StEmcTriggerMaker.h"


ClassImp(StEmcTriggerMaker)

//_____________________________________________________________________________
StEmcTriggerMaker::StEmcTriggerMaker(const char *name):StMaker(name)
{
    mBemcTrigger = new StBemcTrigger();
    mSaveStEvent = true;

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
    mIs2005JPSI=-1;
    mIs2006JP1=-1;
    mIs2006HT2=-1;
    mIs2006JP2=-1;
    mIs2006JPSI=-1;
    mIs2006HTTP=-1;
    for (int i=0;i<16;i++)
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
    HT2_ID_2006=-1;
    JP1_ID_2006=-1;
    JP2_ID_2006=-1;
    for (int i=0;i<13;i++)
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
    HT2_DSM_2006=-1;
    JP1_DSM_2006=-1;
    JP2_DSM_2006=-1;
    BETOT_DSM_2006=-1;
    for (int i=0;i<14;i++)
    {
        DsmAdc[i] = -1;
    }

    for (int i=0;i<kNJet;i++){
      JP12005array[i]=-1;
      JP22005array[i]=-1;
      JP12006array[i]=-1;
      JP22006array[i]=-1;
    }
    for (int i=0; i<kNTowers; i++){
      HT12005array[i]=-1;
      HT22005array[i]=-1;
      HT22006array[i]=-1;
    }

    for (int i=0;i<(kNJet/2);i++){ BL1_2006_arrayADC[i]=-1;}

    for (int i=0; i<kNPatches; i++){
      HTTP2006arrayTP[i]=-1;
      HTTP2006arrayTPADC[i]=-1;
      HTTP2006arrayHT[i]=-1;
      HTTP2006arrayHTADC[i]=-1;

    }

    for (int i=0; i<7; i++) {
      numHT[i]=-1;
      numJP[i]=-1;
      numHTTP[i]=-1;
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
  
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StEmcTriggerMaker::Make()
{

    LOG_DEBUG<<"StEmcTriggerMaker::Make()"<<endm;

    tables->loadTables(this);
    setTableMaker(tables);

    StEvent* event=(StEvent*)GetInputDS("StEvent");
    if(!event) return kStOk;

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
    int* numHTTP =mBemcTrigger->getNHTTP();
    int* HT12005array = mBemcTrigger->getHT12005array();
    int* HT22005array = mBemcTrigger->getHT22005array();
    int* JP12005array = mBemcTrigger->getJP12005array();
    int* JP22005array = mBemcTrigger->getJP22005array();
    int* JPSI2005adc  = mBemcTrigger->getJPSI2005adc();
    int* JPSI2005id   = mBemcTrigger->getJPSI2005id();
    int* HT22006array = mBemcTrigger->getHT22006array();
    int* JP12006array = mBemcTrigger->getJP12006array();
    int* JP22006array = mBemcTrigger->getJP22006array();
    int* JPSI2006adc  = mBemcTrigger->getJPSI2006adc();
    int* JPSI2006id   = mBemcTrigger->getJPSI2006id();
    int *HTTP2006arrayHT = mBemcTrigger->getHTTP2006arrayHT();
    int *HTTP2006arrayHTADC = mBemcTrigger->getHTTP2006arrayHTADC();
    int *HTTP2006arrayTP = mBemcTrigger->getHTTP2006arrayTP();
    int *HTTP2006arrayTPADC = mBemcTrigger->getHTTP2006arrayTPADC();
    int *BL12006arrayADC=mBemcTrigger->getBL12006arrayADC();


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

    //2005 JPSI = 20
    mIs2005JPSI=isTrig[10];
    for (int i=0;i<kNJet; i++){
      JPSI_2005_ADC[i]=JPSI2005adc[i];
      JPSI_2005_ID[i]=JPSI2005id[i];
    }
    
    //2006 HT2=
    mIs2006HT2=isTrig[11];
    HT2_ID_2006=TowJetId[11];
    HT2_DSM_2006=DsmAdc[11];
    numHT2_2006=numHT[5];
    for (int i=0;i<numHT2_2006;i++){
      HT2_2006_array[i]=HT22006array[i];
    }      

    //2006 JP1=
    mIs2006JP1=isTrig[12];
    JP1_ID_2006=TowJetId[12];
    JP1_DSM_2006=DsmAdc[12];
    numJP1_2006=numJP[5];
    for (int i=0;i<numJP1_2006;i++){
      JP1_2006_array[i]=JP12006array[i];
    }

    //2006 JP2=
    mIs2006JP2=isTrig[13];
    JP2_ID_2006=TowJetId[13];
    JP2_DSM_2006=DsmAdc[13];
    numJP2_2006=numJP[6];
    for (int i=0;i<numJP2_2006;i++){
      JP2_2006_array[i]=JP22006array[i];
    }

    //2006 JPSI 
    mIs2006JPSI=isTrig[14];
    for (int i=0;i<kNJet; i++){
      JPSI_2006_ADC[i]=JPSI2006adc[i];
      JPSI_2006_ID[i]=JPSI2006id[i];
    }

    //2006 HTTP && UPSILON
    mIs2006HTTP=isTrig[15];
    numHTTP_2006=numHTTP[0];
    for (int i=0; i<numHTTP_2006; i++){
      HTTP_2006_arrayTP[i]=HTTP2006arrayTP[i];
      HTTP_2006_arrayHT[i]=HTTP2006arrayHT[i];
      HTTP_2006_arrayTP_ADC[i]=HTTP2006arrayTPADC[i];
      HTTP_2006_arrayHT_ADC[i]=HTTP2006arrayHTADC[i];
    }

    //2006 BETOT
    BETOT_DSM_2006=DsmAdc[14];
    for (int i=0;i<kNJet/2;i++){
      BL1_2006_arrayADC[i]=BL12006arrayADC[i];
    }
    //access TP 6 bit DSMsum
    for (int j=0;j<300;j++)
    {
        trigPatch[j]=0;
        trigPatch[j]=mBemcTrigger->trgPatch[j];
    }


    return kStOK;
}

							  
Int_t StEmcTriggerMaker::Finish()
{
    return StMaker::Finish();
}

void StEmcTriggerMaker::get2005HT1_TOWS(int index, int *id){
  *id=-1;
  if (index<kNTowers) *id=HT1_2005_array[index];
}

void StEmcTriggerMaker::get2005HT2_TOWS(int index, int *id){
  *id=-1;
  if (index<kNTowers) *id=HT2_2005_array[index];
}

void StEmcTriggerMaker::get2005JP1_PATCHES(int index, int *id){
  *id=-1;
  if (index<kNJet) *id=JP1_2005_array[index];
}

void StEmcTriggerMaker::get2005JP2_PATCHES(int index, int *id){
  *id=-1;
  if (index<kNJet) *id=JP2_2005_array[index];
}

void StEmcTriggerMaker::get2005JPSI_ADC(int index, int *id){
  *id=-1;
  if (index<kNJet) *id=JPSI_2005_ADC[index];
}

void StEmcTriggerMaker::get2005JPSI_ID(int index, int *id){
  *id=-1;
  if (index<kNJet) *id=JPSI_2005_ID[index];
}

void StEmcTriggerMaker::get2006HT2_TOWS(int index, int *id){
  *id=-1;
  if (index<kNTowers) *id=HT2_2006_array[index];
}

void StEmcTriggerMaker::get2006JP1_PATCHES(int index, int *id){
  *id=-1;
  if (index<kNJet) *id=JP1_2006_array[index];
}

void StEmcTriggerMaker::get2006JP2_PATCHES(int index, int *id){
  *id=-1;
  if (index<kNJet) *id=JP2_2006_array[index];
}

void StEmcTriggerMaker::get2006JPSI_ADC(int index, int *id){
  *id=-1;
  if (index<kNJet) *id=JPSI_2006_ADC[index];
}

void StEmcTriggerMaker::get2006JPSI_ID(int index, int *id){
  *id=-1;
  if (index<kNJet) *id=JPSI_2006_ID[index];
}

void StEmcTriggerMaker::get2006HTTP_TP(int index, int *id){
  *id=-1;
  if (index<kNPatches) *id=HTTP_2006_arrayTP[index];
}

void StEmcTriggerMaker::get2006HTTP_TP_ADC(int index, int *id){
  *id=-1;
  if (index<kNPatches) *id=HTTP_2006_arrayTP_ADC[index];
}

void StEmcTriggerMaker::get2006HTTP_HT(int index, int *id){
  *id=-1;
  if (index<kNPatches) *id=HTTP_2006_arrayHT[index];
}

void StEmcTriggerMaker::get2006HTTP_HT_ADC(int index, int *id){
  *id=-1;
  if (index<kNPatches) *id=HTTP_2006_arrayHT_ADC[index];
}

void StEmcTriggerMaker::get2006BL1_ADC(int index, int *id){
  *id=-1;
  if (index<kNJet/2) *id=BL1_2006_arrayADC[index];
}


