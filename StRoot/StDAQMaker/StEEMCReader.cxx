/***************************************************************************
 *
 *  
 *
 * Author: Herbert Ward
 ***************************************************************************
 *
 * Description: Offline Wrapper for DAQ EEMC reader classes
 *
 ***************************************************************************
 *
 *  
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

int StEEMCReader::getEemcData2004(int crate,int channel) {
  static int warn=0;
  if(fEEMCImpReader) return fEEMCImpReader->getEemc2004(crate,channel);
  if(!warn) {
    printf("StEEMCReader::getTowerAdc  WARNING:   no EEMC data in this event.\n");
    warn=1;
  }
  return -1;
}
int StEEMCReader::getTowerAdc(int crate,int channel) {
  static int warn=0;
  if(fEEMCImpReader) return fEEMCImpReader->getEemcTowerAdc(crate,channel);
  if(!warn) {
    printf("StEEMCReader::getTowerAdc  WARNING:   no EEMC data in this event.\n");
    warn=1;
  }
  return -1;
}


int StEEMCReader::getEEmcData(int crate,int channel, int mapping) {
  static int warn=0;
  if(fEEMCImpReader) return fEEMCImpReader->getEemc(crate,channel,mapping);
  if(!warn) {
    printf("StEEMCReader::getTowerAdc  WARNING:   no EEMC data in this event.\n");
    warn=1;
  }
  return -1;
}


int StEEMCReader::Update() {
  delete fEEMCImpReader;
  fEEMCImpReader = ::getEEMCReader(fDAQReader->getEventReader());
 // close();
 cout<<" EEMC Updated**"<<endl;
 return 1;
}
