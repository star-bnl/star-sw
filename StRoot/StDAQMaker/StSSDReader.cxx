/***************************************************************************
 *
 * $Id: 
 *
 * Author: Herbert Ward
 ***************************************************************************
 *
 * Description: Offline Wrapper for DAQ SSD reader classes
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
#include "StSSDReader.h"
#include "StDaqLib/SSD/SSD_Reader.hh"

  
int StSSDReader::getSsdData(int ladder,char eastWest,int channel,int& data,int& pedestal,int& noise) {
  int rv;
  if(!fSSDImpReader) return -1;
  if(ladder<1||ladder>20) return -2;
  if(eastWest!='E'&&eastWest!='W') return -3;
  rv = fSSDImpReader->ssdData(ladder,eastWest,channel,data,pedestal,noise);
  return rv;
}
StSSDReader::StSSDReader(StDAQReader *daqr) {
  fSSDImpReader=0;
   fDAQReader = daqr;
  delete fSSDImpReader;
  fSSDImpReader = ::getSSDReader(daqr->getEventReader());
}
StSSDReader::~StSSDReader() {
}
int StSSDReader::close() {
  //  delete fSSDImpReader; fSSDImpReader=0;
  return 1;
}
int StSSDReader::Update() {
  delete fSSDImpReader;
  fSSDImpReader = ::getSSDReader(fDAQReader->getEventReader());
  // close();
  cout<<" SSD Updated**"<<endl;
  return 1;
}

