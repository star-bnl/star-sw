/***************************************************************************
 * $id: EMC daq reader.
 * Author: Subhasis and Herbert Ward
 ***************************************************************************
 *  Opens Event From File, Fills Struct
 *
 **************************************************************************/

#include "EMC_Reader.hh"
#include <assert.h>
#include "EMC_BarrelReader.hh"
#include "EMC_SmdReader.hh"
#define MAX_ADC 0xFFF


void EMC_Reader::ProcessEvent(const Bank_EMCP * EmcPTR) {

  // will process the event to fill the arrays for different detectors.
  //Towers
  EMC_BarrelReader* barreltowerreader = 
  new EMC_BarrelReader(ercpy, const_cast<Bank_EMCP *>(EmcPTR));
  int towerresult=  barreltowerreader->ProcessBarrelTower(EmcPTR);
  mTheTowerAdcR=barreltowerreader->getBTOWERADCR();

  if(!towerresult)
   {cout<<" Barrel TOWER processing is not successful**"<<endl;
        TowerPresent=false;} // 0 is bad
    else{TowerPresent=true;}
  delete barreltowerreader;barreltowerreader=0;

  //SMDs
  EMC_SmdReader* barrelsmdreader= 
  new EMC_SmdReader(ercpy, const_cast<Bank_EMCP *>(EmcPTR));
  int smdresult= barrelsmdreader->ProcessBarrelSmd(EmcPTR);
  mTheSmdAdcR=barrelsmdreader->getBSMDADCR();

  if(!smdresult)
   {cout<<" Barrel SMD processing is not successful**"<<endl;
     SmdPresent=false;} // 0 is bad
    else{SmdPresent=true;}
  delete barrelsmdreader;barrelsmdreader=0;

 }

//EMC_Reader::~EMC_Reader(){}


EMC_Reader::EMC_Reader(EventReader *er, Bank_EMCP *pEMCP) {
  pBankEMCP = pEMCP; //copy into class data member for use by other methods
  ercpy = er; // squirrel away pointer eventreader for our friends
  printf("This is the EMC_Reader ctor in %s.\n",__FILE__); 
  pBankEMCP->header.BankType[7]=0;
  cout<<"header bank type "<<pBankEMCP->header.BankType<<endl;
 
  //  assert(!strcmp(pEMCP->header.BankType,"EMCP")); // Be sure that we have the correct bank.
  if (!pBankEMCP->test_CRC())  {
    printf("CRC error in EMCP: %s %d\n",__FILE__,__LINE__) ;
  }
  if (pBankEMCP->swap() < 0) {
    printf("swap error in EMCP: %s %d\n",__FILE__,__LINE__) ;
  }

  pBankEMCP->header.CRC = 0;

  int Token = pBankEMCP->header.Token;

  Bank_DATAP *dp = (Bank_DATAP *)ercpy->getDATAP();

  if(Token !=dp->header.Token){
    printf("Token mismatch between global %d and RICH %d\n",dp->header.Token,Token);
  }
  // Initialize arrays

  //  Initialize();

  // Process events and fill the structs

  ProcessEvent(pBankEMCP); 
}

int EMC_Reader::NTowerHits()
 {
  if(TowerPresent)
  {
  if(strncmp(mTheTowerAdcR.BankType,"TOWRADCR",8)) {
    cout<<" error in header name**"<<endl;return 0;
  }
   int nhits=mTheTowerAdcR.NTowerHits;
   return nhits;}
  else{return 0;}
 }
int EMC_Reader::NSmdHits()
 {
  if(SmdPresent)
  {
  if(strncmp(mTheSmdAdcR.BankType,"BSMDADCR",8)) {
    cout<<" error in header name**"<<endl;return 0;
  }
   int nhits=mTheSmdAdcR.NSmdHits;
   return nhits;}
  else{return 0;}
 }

int EMC_Reader::getTowerADC(int mod,int e, int s,unsigned short& ADC )
 {
  if(TowerPresent)
  {
  if(strncmp(mTheTowerAdcR.BankType,"TOWRADCR",8)) {
    cout<<" error in header name**"<<endl;return 0;
  }
   ADC=mTheTowerAdcR.TowerMatrix[mod-1][e-1][s-1];
   return 1;}
 // 1 is good
  else{return 0;}
 }

int EMC_Reader::getTowerADC(int index, unsigned short& ADC )
 {
  if(TowerPresent)
  {
  if(strncmp(mTheTowerAdcR.BankType,"TOWRADCR",8)) {
    cout<<" error in header name**"<<endl;return 0;
  } 
  ADC=mTheTowerAdcR.TowerADCArray[index-1];
  return 1;  }// 1 is good
  else{return 0;}
 }

/*
int EMC_Reader::getTowerADCR(unsigned short*** AdcList )
 {
  if(strncmp(mTheTowerAdcR.BankType,"TOWRADCR",8)) {
    cout<<" error in header name**"<<endl;return 0;
  } 
   **AdcList=&mTheTowerAdcR.TowerMatrix[0][0][0];
  return 1;  // 1 is good
 }
*/

int EMC_Reader::getSMDE_ADC(int mod,int strip,unsigned short& ADC )
 {
  if(SmdPresent)
  {
  if(strncmp(mTheSmdAdcR.BankType,"BSMDADCR",8)) {
    cout<<" getSMDE_ADC::error in header name**"<<endl;return 0;
  }
   ADC=mTheSmdAdcR.SmdE_ADCMatrix[mod-1][strip-1];
   return 1;}
 // 1 is good
  else{return 0;}
 }

int EMC_Reader::getSMD_ADC(int index, int fiber,unsigned short& ADC )
 {
  if(SmdPresent)
  {
  if(strncmp(mTheSmdAdcR.BankType,"BSMDADCR",8)) {
    cout<<" getSMD_ADC::error in header name**"<<endl;return 0;
  } 
  ADC=mTheSmdAdcR.SMDADCArray[fiber-1][index-1];
  return 1;}
  // 1 is good
  else{return 0;}
 }

int EMC_Reader::getSMDP_ADC(int mod,int bin,int strip,unsigned short& ADC )
 {

  if(SmdPresent)
  {
  if(strncmp(mTheSmdAdcR.BankType,"BSMDADCR",8)) {
    cout<<" getSMDE_ADC::error in header name**"<<endl;return 0;
  }
   ADC=mTheSmdAdcR.SmdP_ADCMatrix[mod-1][bin-1][strip-1];
   return 1;} // 1 is good
  else{return 0;}
 }
  
int EMC_Reader::getSMD_TIMEBIN(int fiber, unsigned int& TimeBin)
 {
  if(SmdPresent)
  {
  if(strncmp(mTheSmdAdcR.BankType,"BSMDADCR",8)) {
    cout<<" getSMDE_ADC::error in header name**"<<endl;return 0;
  }
   TimeBin=mTheSmdAdcR.TimeBin[fiber];
   return 1; }// 1 is good
  else{return 0;}
 }
  
