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
#include "StDaqLib/EMC/StEmcDecoder.h"

#include "StEmcTriggerMaker.h"


ClassImp(StEmcTriggerMaker)

//_____________________________________________________________________________
StEmcTriggerMaker::StEmcTriggerMaker(const char *name):StMaker(name)
{
    mBemcTrigger = new StBemcTrigger();

    mIs2003HT1=-1;
    mIs2003HT2=-1;
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
    for (int matrix=0;matrix<6;matrix++){
      mIs2006JP0[matrix]=-1;
      mIs2006HT2[matrix]=-1;
      mIs2006JP1[matrix]=-1;
      mIs2006JPSI[matrix]=-1;
      mIs2006HTTP[matrix]=-1;
    }
    for (int i=0;i<50;i++) isTrig[i]=-1;
      
    
    HT1_ID_2003=-1;
    HT2_ID_2003=-1;
    HT1_ID_2004=-1;
    HT2_ID_2004=-1;
    JP1_ID_2004=-1;
    JP2_ID_2004=-1;
    HT1_ID_2005=-1;
    HT2_ID_2005=-1;
    JP1_ID_2005=-1;
    JP2_ID_2005=-1;
    ADJ_ID_2005=-1;
    for (int matrix=0;matrix<6;matrix++)
      {
	HT2_ID_2006[matrix]=-1;
	JP0_ID_2006[matrix]=-1;
	JP1_ID_2006[matrix]=-1;
      }
    
    for (int i=0;i<50;i++) TowJetId[i] = -1;
    
    HT1_DSM_2003=-1;
    HT2_DSM_2003=-1;
    HT1_DSM_2004=-1;
    HT2_DSM_2004=-1;
    JP1_DSM_2004=-1;
    JP2_DSM_2004=-1;
    HT1_DSM_2005=-1;
    HT2_DSM_2005=-1;
    JP1_DSM_2005=-1;
    JP2_DSM_2005=-1;
    ADJ_DSM_2005=-1;
    for ( int matrix=0;matrix<6;matrix++){
      HT2_DSM_2006[matrix]=-1;
      JP0_DSM_2006[matrix]=-1;
      JP1_DSM_2006[matrix]=-1;
    }
    BETOT_DSM_2006=-1;
    
    for (int i=0;i<50;i++)  DsmAdc[i] = -1;
    
    for (int i=0;i<kNJet;i++){
      JP12005array[i]=-1;
      JP22005array[i]=-1;
    }
    for (int i=0; i<kNTowers; i++){
      HT12005array[i]=-1;
      HT22005array[i]=-1;
    }

    for (int i=0;i<(kNJet/2);i++){ BL1_2006_arrayADC[i]=-1;}
    
    for (int i=0; i<12; i++) {
      numHT[i]=-1;
      numJP[i]=-1;
      if (i<6) numHTTP[i]=-1;
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
    int* BL12006arrayADC=mBemcTrigger->getBL12006arrayADC();
  

    //2003 HT1 == 1101/2201
    mIs2003HT1=isTrig[0];
    HT1_ID_2003=TowJetId[0];
    HT1_DSM_2003=DsmAdc[0];

    //2003 HT2 == 2202
    mIs2003HT2=isTrig[42];
    HT2_ID_2003=TowJetId[42];
    HT2_DSM_2003=DsmAdc[42];

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



    for (int matrix=0;matrix<6;matrix++)
      {
	//2006 HT2
	mIs2006HT2[matrix]=isTrig[11+(matrix*5)];
	HT2_ID_2006[matrix]=TowJetId[11+(matrix*5)];
	HT2_DSM_2006[matrix]=DsmAdc[11+(matrix*5)];
	numHT2_2006[matrix]=numHT[5+matrix];
	for (int i=0;i<numHT2_2006[matrix];i++){
	  HT2_2006_array[matrix][i]=mBemcTrigger->getHT22006array(matrix,i);
	}      
			
	//2006 JP0
	mIs2006JP0[matrix]=isTrig[12+(matrix*5)];
	JP0_ID_2006[matrix]=TowJetId[12+(matrix*5)];
	JP0_DSM_2006[matrix]=DsmAdc[12+(matrix*5)];
	numJP0_2006[matrix]=numJP[5+(matrix*2)];
	for (int i=0;i<numJP0_2006[matrix];i++) {
	  JP0_2006_array[matrix][i]=mBemcTrigger->getJP02006array(matrix,i);
	}

	//2006 JP1
	mIs2006JP1[matrix]=isTrig[13+(matrix*5)];
	JP1_ID_2006[matrix]=TowJetId[13+(matrix*5)];
	JP1_DSM_2006[matrix]=DsmAdc[13+(matrix*5)];
	numJP1_2006[matrix]=numJP[6+(matrix*2)];
	for (int i=0;i<numJP1_2006[matrix];i++){
	  JP1_2006_array[matrix][i]=mBemcTrigger->getJP12006array(matrix,i);
	}
	
	//2006 JPSI 
	mIs2006JPSI[matrix]=isTrig[14+(matrix*5)];
	for (int i=0;i<kNJet; i++){
	  JPSI_2006_ADC[matrix][i]=mBemcTrigger->getJPSI2006adc(matrix,i);
	  JPSI_2006_ID[matrix][i]=mBemcTrigger->getJPSI2006id(matrix,i);
	}
	
	//2006 HTTP && UPSILON
	mIs2006HTTP[matrix]=isTrig[15+(matrix*5)];
	numHTTP_2006[matrix]=numHTTP[matrix];
	for (int i=0; i<numHTTP_2006[matrix]; i++){
	  HTTP_2006_arrayTP[matrix][i]=mBemcTrigger->getHTTP2006arrayTP(matrix,i);
	  HTTP_2006_arrayHT[matrix][i]=mBemcTrigger->getHTTP2006arrayHT(matrix,i);
	  HTTP_2006_arrayTP_ADC[matrix][i]=mBemcTrigger->getHTTP2006arrayTPADC(matrix,i);
	  HTTP_2006_arrayHT_ADC[matrix][i]=mBemcTrigger->getHTTP2006arrayHTADC(matrix,i);
	}
      }

    //2006 BETOT
    BETOT_DSM_2006=DsmAdc[41];
    for (int i=0;i<kNJet/2;i++) BL1_2006_arrayADC[i]=BL12006arrayADC[i];
    
    
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


void StEmcTriggerMaker::get2006BL1_ADC(int index, int *id){
  *id=-1;
  if (index<kNJet/2) *id=BL1_2006_arrayADC[index];
}


int StEmcTriggerMaker::isTrigger(int trigId) {
  
  switch(trigId) 
    {
      
      //matrix==0
      //bemc-ht2-mb-emul  
    case(127212): return mIs2006HT2[0];
      
      //matrix==1
      //bemc-jp0-mb
    case(127501): return mIs2006JP0[1];
      //bemc-jp1-mb
    case(127221): return mIs2006JP1[1];
      //bemc-http-mb-fast 
    case(127821): return mIs2006HTTP[1];
      //bemc-http-mb-l2gamma 
    case(127611): return -1;
      //bemc-jp0-etot-mb-L2jet
    case(127622): return -1;
      //bemc-ht2-mb-emul
    case(127213): return mIs2006HT2[1];
      //jpsi-mb 
    case(117705): return mIs2006JPSI[1];
      //Upsilon
    case(117602): return mIs2006HTTP[1];

      //matrix==2 only a few triggers commissioned
      //bemc-jp1-mb
    case(137221): return mIs2006JP1[2];
      
      //matrix==3
      //bemc-jp0-mb
    case(137501): return mIs2006JP0[3];
      //bemc-jp1-mb
    case(137222): return mIs2006JP1[3];
      //bemc-http-mb-fast 
    case(137821): return mIs2006HTTP[3];
      //bemc-jp0-etot-mb-L2jet
    case(137622): return -1;
      //bemc-ht2-mb-emul
    case(137213): return mIs2006HT2[3];
      //Upsilon
    case(137602): return mIs2006HTTP[3];


      //matrix==4 change HT1 and TP th
      //bemc-http-mb-fast 
    case(137822): return mIs2006HTTP[4];

      //matrix==5  add L2 gamma trigger
      //bemc-http-mb-l2gamma 
    case(137611): return -1;

      //2003-2005 
    case(96201): return is2005HT1();
    case(96211): return is2005HT2();
    case(96221): return is2005JP1();
    case(96233): return is2005JP2();
    case(45201): return is2004HT1();
    case(45202): return is2004HT2();
    case(45206): return is2004JP1();
    case(45207): return is2004JP2();
    case(1101):  return is2003HT1();
    case(2201):  return is2003HT1();
    case(2202):  return is2003HT2();

    }
  
  //if we got here then we don't know how to interpret the trigger
  return -1;
}


int StEmcTriggerMaker::barrelTowerThreshold(int trigId, int softId) 
{
  switch(trigId) 
    {
      //high tower 
    case(127212): return (softId > 2400) ? 24:22;
    case(127213):case(137213): return 24;
      
      //jet patch
    case(127501):case(137501):
    case(127221):case(137221):case(137222): return 0;
      
      //http
    case(127821): return 12;
    case(137821): return 18;
    case(137822): return 16;
      
      //jpsi E==11, W==5
    case(117705): return (softId > 2400) ? 11:5;
      
      //2003-2005
    case(96201): return 13;
    case(96211): return 17;
    case(96221): case(96233): return 0;
    case(45201): return 10;
    case(45202): return 20;
    case(45206): case(45207): return 0;
    case(1101):  return 8;
    case(2201):  return 8;
    case(2202):  return 13;


    }
  
  //if we got here then we don't know how to interpret the trigger
  return -1;
}

int StEmcTriggerMaker::barrelTriggerPatchThreshold(int trigId, int patchId) 
{
  switch(trigId) 
    {
      //high tower
    case(127212):case(127213):case(137213): return 0;
      
    //jet patch
    case(127501):case(137501):
    case(127221):case(137221):case(137222): return 0;
      
      //http
    case(127821): return 17;
    case(137821): return 20;
    case(137822): return 19;
      
      //jpsi
    case(117705): return 0;
      
      //run 2003-2005
    case(96201): case(96211): case(96221): case(96233): return 0;
    case(45201): case(45202): case(45206): case(45207): return 0;
    case(1101): case(2201): case(2202):  return 0;

    }
  
  //if we got here then we don't know how to interpret the trigger
  return -1;
}

int StEmcTriggerMaker::barrelJetPatchThreshold(int trigId, int patchId)
{
  switch(trigId) 
    {
      //high tower
    case(127212):case(127213):case(137213): return 0;
      
      //jet patch
    case(127501):return 42;
    case(127221):case(137221): return 58;
    case(137501):return 49;
    case(137222):return 60;
      
      //http
    case(127821):case(137821):case(137822): return 0;
      
      //jpsi
    case(117705): return 0;        
      
      //run 2003+2004+2005
    case(96201): case(96211): return 0;
    case(96221): return 66;
    case(96233): return 84;
    case(45201): case(45202): return 0;
    case(45206): return 40;
    case(45207): return 60;
    case(1101): case(2201): case(2202): return 0;

      
    }
  
  //if we got here then we don't know how to interpret the trigger
  return -1;
}

map<int,int> StEmcTriggerMaker::barrelTowersAboveThreshold(int trigId) {
    map<int,int> towers;
    int counter,softId,adc;
    
    switch(trigId) 
      {

	//matrix==0
	//bemc-ht2-mb-emul  
      case(127212):
	counter = numHT2_2006[0];
	for(int i=0; i<counter; i++) {
	  adc=-1;
	  softId=HT2_2006_array[0][i];
	  if (softId == HT2_ID_2006[0]) adc = HT2_DSM_2006[0];
	  towers[softId] = adc;
	}
        break;
	
	//matrix==1
	//bemc-http-mb-fast 
      case(127821):
	counter = numHTTP_2006[1];
	for(int i=0; i<counter; i++) {
	  softId= HTTP_2006_arrayHT[1][i];
	  adc   = HTTP_2006_arrayHT_ADC[1][i];
	  towers[softId] = adc;
	}
        break;
	
	//bemc-http-mb-l2gamma 
	//case(127611): return -1;
	//bemc-jp0-etot-mb-L2jet
	//case(127622): return -1;
	
	//bemc-ht2-mb-emul	
      case(127213):
	counter = numHT2_2006[1];
	for(int i=0; i<counter; i++) {
	  softId=HT2_2006_array[1][i];
	  adc=-1;
	  if (softId == HT2_ID_2006[1]) adc = HT2_DSM_2006[1];
	  towers[softId] = adc;
	}
        break;

	//jpsi-mb 
      case(117705):
	counter = kNJet;
	for(int i=0; i<counter; i++) {
	  softId=JPSI_2006_ID[1][i];
	  adc=JPSI_2006_ADC[1][i];
	  towers[softId] = adc;
	}
        break;

	//Upsilon
      case(117602):
	counter = numHTTP_2006[1];
	for(int i=0; i<counter; i++) {
	  softId= HTTP_2006_arrayHT[1][i];
	  adc   = HTTP_2006_arrayHT_ADC[1][i];
	  towers[softId] = adc;
	}
        break;

	//matrix==2 only a few triggers commissioned
	//bemc-jp1-mb
	
	//matrix==3
	//bemc-http-mb-fast 
      case(137821):
	counter = numHTTP_2006[3];
	for(int i=0; i<counter; i++) {
	  softId= HTTP_2006_arrayHT[3][i];
	  adc   = HTTP_2006_arrayHT_ADC[3][i];
	  towers[softId] = adc;
	}
        break;

	//bemc-ht2-mb-emul
      case(137213):
	counter = numHT2_2006[3];
	for(int i=0; i<counter; i++) {
	  softId=HT2_2006_array[3][i];
	  adc=-1;
	  if (softId == HT2_ID_2006[3]) adc = HT2_DSM_2006[3];
	  towers[softId] = adc;
	}
        break;

	//Upsilon
      case(137602):
	counter = numHTTP_2006[3];
	for(int i=0; i<counter; i++) {
	  softId= HTTP_2006_arrayHT[3][i];
	  adc   = HTTP_2006_arrayHT_ADC[3][i];
	  towers[softId] = adc;
	}
        break;

	//matrix==4 change HT1 and TP th
	//bemc-http-mb-fast 
      case(137822):
	counter = numHTTP_2006[4];
	for(int i=0; i<counter; i++) {
	  softId= HTTP_2006_arrayHT[4][i];
	  adc   = HTTP_2006_arrayHT_ADC[4][i];
	  towers[softId] = adc;
	}
        break;
	
	//matrix==5  add L2 gamma trigger
	//bemc-http-mb-l2gamma 
	//case(137611): 
	
	
	//2005 ht1
      case(96201):
	counter = get2005HT1_NTOWS();
	for(int i=0; i<counter; i++) {
	  get2005HT1_TOWS(i,&softId);
	  adc = -1;
	  if(softId == get2005HT1_ID()) adc = get2005HT1_ADC();
	  towers[softId] = adc;
	}
        break;
      
	//2005 ht2
      case(96211):
	counter = get2005HT2_NTOWS();
	for(int i=0; i<counter; i++) {
	  get2005HT2_TOWS(i,&softId);
	  adc = -1;
	  if(softId == get2005HT2_ID()) adc = get2005HT2_ADC();
	  towers[softId] = adc;
	}
        break;
	
      case(1101):
	if (is2003HT1()==0) counter=0;
	if (is2003HT1()==1) counter=1;
	for(int i=0; i<counter; i++) {
	  softId=get2003HT1_ID();
	  adc = get2003HT1_ADC();
	  towers[softId] = adc;
	}
        break;
	
      case(2201):
	if (is2003HT1()==0) counter=0;
	if (is2003HT1()==1) counter=1;
	for(int i=0; i<counter; i++) {
	  softId=get2003HT1_ID();
	  adc = get2003HT1_ADC();
	  towers[softId] = adc;
	}
        break;
	
      case(2202):
	if (is2003HT2()==0) counter=0;
	if (is2003HT2()==1) counter=1;
	for(int i=0; i<counter; i++) {
	  softId=get2003HT2_ID();
	  adc = get2003HT2_ADC();
	  towers[softId] = adc;
	}
        break;

      case(45201):
	if (is2004HT1()==0) counter=0;
	if (is2004HT1()==1) counter=1;
	for(int i=0; i<counter; i++) {
	  softId=get2004HT1_ID();
	  adc = get2004HT1_ADC();
	  towers[softId] = adc;
	}
        break;
	
     case(45202):
	if (is2004HT2()==0) counter=0;
	if (is2004HT2()==1) counter=1;
	for(int i=0; i<counter; i++) {
	  softId=get2004HT2_ID();
	  adc = get2004HT2_ADC();
	  towers[softId] = adc;
	}
        break;
	


      }
    
    return towers;
}


map<int,int> StEmcTriggerMaker::barrelTriggerPatchesAboveThreshold(int trigId) {
  map<int,int> patches;
  int counter,softId,adc;
  
  switch(trigId) 
    {
      
      //bemc-http-mb-fast 
    case(127821):
      counter = numHTTP_2006[1];
      for(int i=0; i<counter; i++) {
	softId=HTTP_2006_arrayTP[1][i];
	adc=HTTP_2006_arrayTP_ADC[1][i];
	patches[softId] = adc;
      }
      break;
      
      //bemc-http-mb-fast 
    case(137821): 
      counter = numHTTP_2006[3];
      for(int i=0; i<counter; i++) {
	softId=HTTP_2006_arrayTP[3][i];
	adc=HTTP_2006_arrayTP_ADC[3][i];
	patches[softId] = adc;
      }
      break;
      
      //bemc-http-mb-fast 
    case(137822):
      counter = numHTTP_2006[4];
      for(int i=0; i<counter; i++) {
	softId=HTTP_2006_arrayTP[4][i];
	adc=HTTP_2006_arrayTP_ADC[4][i];
	patches[softId] = adc;
      }
      break;
    }
  
  return patches;
}

map<int,int> StEmcTriggerMaker::barrelJetPatchesAboveThreshold(int trigId) {
    map<int,int> patches;
    int counter,softId,adc;
    
    switch(trigId) 
      {

	//bemc-jp0-mb matrix==1
      case(127501):
	counter = numJP0_2006[1];
	for(int i=0; i<counter; i++) {
	  adc=-1;
	  softId=JP0_2006_array[1][i];
	  if(softId == JP0_ID_2006[1]) adc = JP0_DSM_2006[1];
	  patches[softId] = adc;
	}
        break;

	//bemc-jp1-mb matrix==1
      case(127221):
	counter = numJP1_2006[1];
	for(int i=0; i<counter; i++) {
	  adc=-1;
	  softId=JP1_2006_array[1][i];
	  if(softId == JP1_ID_2006[1]) adc = JP1_DSM_2006[1];
	  patches[softId] = adc;
	}
        break;

	//bemc-jp1-mb matrix==2
      case(137221):
	counter = numJP1_2006[2];
	for(int i=0; i<counter; i++) {
	  adc=-1;
	  softId=JP1_2006_array[2][i];
	  if(softId == JP1_ID_2006[2]) adc = JP1_DSM_2006[2];
	  patches[softId] = adc;
	}
        break;
 
	//bemc-jp0-mb matrix==3
      case(137501):
	counter = numJP0_2006[3];
	for(int i=0; i<counter; i++) {
	  adc=-1;
	  softId=JP0_2006_array[3][i];
	  if(softId == JP0_ID_2006[3]) adc = JP0_DSM_2006[3];
	  patches[softId] = adc;
	}
        break;

	//bemc-jp1-mb matrix==3
      case(137222):
	counter = numJP1_2006[3];
	for(int i=0; i<counter; i++) {
	  adc=-1;
	  softId=JP1_2006_array[3][i];
	  if(softId == JP1_ID_2006[3]) adc = JP1_DSM_2006[3];
	  patches[softId] = adc;
	}
        break;

        
      case(96221):
	counter = get2005JP1_NPATCHES();
	for(int i=0; i<counter; i++) {
	  get2005JP1_PATCHES(i,&softId);
	  adc = -1;
	  if(softId == get2005JP1_ID()) adc = get2005JP1_ADC();
	  patches[softId] = adc;
	}
	break;
	
      case(96233):
	counter = get2005JP2_NPATCHES();
	for(int i=0; i<counter; i++) {
	  get2005JP2_PATCHES(i,&softId);
	  adc = -1;
	  if(softId == get2005JP2_ID()) adc = get2005JP2_ADC();
	  patches[softId] = adc;
	}
	break;

	
      case(45206):
	if (is2004JP1()==0) counter=0;
	if (is2004JP1()==1) counter=1;
	for(int i=0; i<counter; i++) {
	  softId=get2004JP1_ID();
	  adc = get2004JP1_ADC();
	  patches[softId] = adc;
	}
        break;
	
      case(45207):
	if (is2004JP2()==0) counter=0;
	if (is2004JP2()==1) counter=1;
	for(int i=0; i<counter; i++) {
	  softId=get2004JP2_ID();
	  adc = get2004JP2_ADC();
	  patches[softId] = adc;
	}
        break;
   
      }
    
    return patches;
}

int StEmcTriggerMaker::barrelTriggerPatchForTower(int softId) {
    const StEmcDecoder *decoder = mBemcTrigger->decoder();
    if(decoder == NULL) {
        LOG_WARN << "Pointer to decoder is NULL! Can't get trigger patch for " << softId << endm;
        return -1;
    }
    int patchId;
    if(decoder->GetTriggerPatchFromTowerId(softId,patchId) != 0) {
        return patchId;
    }
    else {
        LOG_WARN << "Decoder encountered a problem translating tower id = " << softId << " into a trigger patch" << endm;
        return -1;
    }
}

int StEmcTriggerMaker::totalEnergyThreshold(int trigId) {
    switch(trigId) 
      {
      case(127622):case(127652):case(137622):case(137652): return 109;
      default: return 0;
      }
}

int	StEmcTriggerMaker::totalEnergy() {
  return BETOT_DSM_2006;
}

//the remaining methods still need underlying functionality before they can be implemented
int	StEmcTriggerMaker::endcapTowerThreshold(int trigId) {
	return -1;
}

int	StEmcTriggerMaker::endcapTriggerPatchThreshold(int trigId) {
	return -1;
}

int StEmcTriggerMaker::endcapJetPatchThreshold(int trigId) {
	return -1;
}

map<int,int> StEmcTriggerMaker::endcapTowersAboveThreshold(int trigId) {
	map<int,int> towers;
	return towers;
}

map<int,int> StEmcTriggerMaker::endcapTriggerPatchesAboveThreshold(int trigId) {
	map<int,int> trigPatches;
	return trigPatches;
}

map<int,int> StEmcTriggerMaker::endcapJetPatchesAboveThreshold(int trigId) {
	map<int,int> jetPatches;
	return jetPatches;
}

// $Id: StEmcTriggerMaker.cxx,v 1.23 2007/08/22 15:06:53 kocolosk Exp $
//
// $Log: StEmcTriggerMaker.cxx,v $
// Revision 1.23  2007/08/22 15:06:53  kocolosk
// added #include statements that should have always been there.  We didn't catch the problem
// because StBemcTables provided them in the past.
//
// Revision 1.22  2007/05/12 12:45:53  rfatemi
// Added BHT2 for 2003, new access scheme extends back to 2003+2004, remove all access to StEmcPedestal tables
//
// Revision 1.21  2007/05/02 17:36:22  kocolosk
// added decoder wrapper method that correlates tower and trigger patch.
// Useful for HTTP in particular.
//
// Revision 1.20  2007/04/30 01:53:14  rfatemi
// Remove cout statements
//
// Revision 1.19  2007/04/30 00:59:37  rfatemi
// Update new trigger interface
//
// Revision 1.17  2007/04/24 15:53:17  kocolosk
// added new interface methods to get trigger thresholds and decisions based on trigId
//
