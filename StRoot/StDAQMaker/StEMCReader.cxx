/***************************************************************************
 *
 * $Id: 
 *
 * Author: Herbert Ward and Subhasis C.
 ***************************************************************************
 *
 * Description: Offline Wrapper for DAQ EMC reader classes
 *
 ***************************************************************************
 *
 * $Log: 
 *
 **************************************************************************/
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <assert.h>

#include "StDAQReader.h"
#include "StEMCReader.h"
#include "StDaqLib/EMC/EMC_Reader.hh"

typedef EventInfo DAQEventInfo;
  
StEMCReader::StEMCReader(StDAQReader *daqr) {
  fEMCImpReader=0;
   fDAQReader = daqr;
  delete fEMCImpReader;
  fEMCImpReader = ::getEMCReader(daqr->getEventReader());

  /*
  if(fEMCImpReader){
    Int_t towerfill=FillTowerArray();
    if(towerfill!=kStOK)printf("tower arrays are not filled)"
    Int_t prsfill=FillPRSArray();
    if(prsfill!=kStOK)printf("PRS arrays are not filled)"
    Int_t smdefill=FillSMDEArray();
    if(towerfill!=kStOK)printf("SMDE arrays are not filled)"
    Int_t towerfill=FillSMDPArray();
    if(towerfill!=kStOK)printf("SMDP arrays are not filled)"
}
  */
}


StEMCReader::~StEMCReader() {
}

int StEMCReader::close() {
//  delete fEMCImpReader; fEMCImpReader=0;
  return 1;
}

int StEMCReader::Update() {
  delete fEMCImpReader;
  fEMCImpReader = ::getEMCReader(fDAQReader->getEventReader());
 // close();
 cout<<" EMC Updated**"<<endl;
 return 1;
}

int StEMCReader::getTowerADC(int mod,int e, int s,unsigned short& ADC )
 {

   if(!fEMCImpReader->getTowerADC(mod,e,s,ADC))return 0;
   return 1; //1 is good
 }
int StEMCReader::getTowerADC(int index,unsigned short& ADC )
 {

   if(!fEMCImpReader->getTowerADC(index,ADC))return 0;
   return 1; //1 is good
 }

int StEMCReader::getSMDE_ADC(int mod,int e,unsigned short& ADC )
 {

   if(!fEMCImpReader->getSMDE_ADC(mod,e,ADC))return 0;
   return 1; //1 is good
 }

int StEMCReader::getSMDP_ADC(int mod,int bin,int s,unsigned short& ADC )
 {

   if(!fEMCImpReader->getSMDP_ADC(mod,bin,s,ADC))return 0;
   return 1; //1 is good
 }

int StEMCReader::getSMD_TIMEBIN(int fiber,unsigned int& TimeBin )
 {
   if(!fEMCImpReader->getSMD_TIMEBIN(fiber,TimeBin))return 0;
   return 1; //1 is good
 }

int StEMCReader::getSMD_ADC(int index,int fiber,unsigned short& ADC )
 {

   if(!fEMCImpReader->getSMD_ADC(index,fiber,ADC))return 0;
   return 1; //1 is good
 }

/*
int StEMCReader::getTowerADCR(int mod,unsigned short*** & AdcList)
 {

      if(!fEMCImpReader->getTowerADCR(&AdcList[mod-1]))return 0;
   //   if(!fEMCImpReader->getTowerADCR(AdcList))return 0;
   return 1; //1 is good
 }
*/


int StEMCReader::NTowerHits()
 {
   return fEMCImpReader->NTowerHits();
 }
int StEMCReader::NSmdHits()
 {
   return fEMCImpReader->NSmdHits();
 }
 
EMC_Reader* StEMCReader::getBemcReader()
{
  return fEMCImpReader;
}

