/***************************************************************************
 *
 * $Id: 
 *
 * Author: Herbert Ward
 ***************************************************************************
 *
 * Description: Offline Wrapper for DAQ EEMC reader classes
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
#include "StEEMCReader.h"
#include "StDaqLib/EEMC/EEMC_Reader.hh"

typedef EventInfo DAQEventInfo;
  
StEEMCReader::StEEMCReader(StDAQReader *daqr) {
   fEEMCImpReader=0;
   fDAQReader = daqr;
  delete fEEMCImpReader;
  fEEMCImpReader = ::getEEMCReader(daqr->getEventReader());
}


StEEMCReader::~StEEMCReader() {
}

int StEEMCReader::close() {
//  delete fEEMCImpReader; fEEMCImpReader=0;
  return 1;
}


int StEEMCReader::Update() {
  delete fEEMCImpReader;
  fEEMCImpReader = ::getEEMCReader(fDAQReader->getEventReader());
 // close();
 cout<<" EEMC Updated**"<<endl;
 return 1;
}


u_short *StEEMCReader::getEemcHeadBlock(int fiber,char type) {
  if(fEEMCImpReader) return fEEMCImpReader->getEemcHeadBlock(fiber,type);
  return 0;
} 


u_short *StEEMCReader::getEemcDataBlock(int fiber,char type) {
  if(fEEMCImpReader) return fEEMCImpReader->getEemcDataBlock(fiber,type);
  return 0;
} 


u_short StEEMCReader::getEemcHead(int fiber,int channel,char type) {
  if(fEEMCImpReader) return fEEMCImpReader->getEemcHead(fiber,channel,type);
  return 0;
}

u_short StEEMCReader::getEemcData(int fiber,int channel,char type) {
  if(fEEMCImpReader) return fEEMCImpReader->getEemcData(fiber,channel,type);
  return 0;
}

int StEEMCReader::isEemcBankIn( char type) {
  if(fEEMCImpReader) return fEEMCImpReader->isEemcBankIn( type);
  return 0;
}
