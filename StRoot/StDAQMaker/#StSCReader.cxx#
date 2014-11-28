/***************************************************************************
 *
 * $Id: 
 *
 * Author: Herbert Ward
 ***************************************************************************
 *
 * Description: Offline Wrapper for DAQ SC reader classes
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
#include "StSCReader.h"
#include "RTS/src/DAQ_SC/sc.h"
#include "tables/St_trigDetSums_Table.h"

#ifdef sc
#    error "sc_t redefinition"
#endif
#define sc (*(fSC))

// Copy of the sc_t from DAQ_SC/daq_sc.h
//#include "DAQ_SC/daq-sc.h"

//____________________________________________________________
void StSCReader::FillTime(  unsigned int utime)
{
  //Keep BBCBkg scalers flipped as they were historically before 2009
  //Note that new DAQ reader leads to UTime = 0, or tm_year=70 (1970)
  //but new DAQ reader only gets used for 2009+ anyhow
  time_t UTime = utime; //er->getEventInfo().UnixTime;
  struct tm *time=gmtime(&UTime);
  flipBBCBkg = (time->tm_year > 95 && time->tm_year < 109 ? 1 : 0) ;
  useNoKillers = (time->tm_year > 110 ? 1 : 0);;
}

double StSCReader::getCTBWest() {
  return 0;
}

double StSCReader::getCTBEast() {
  return 0;
}

double StSCReader::getCTBOrTOFp() {
  return 0;
}

double StSCReader::getTOFp() {
  return 0;
}

double StSCReader::getZDCWest() {
  return sc.rich_scalers[6];
}

double StSCReader::getZDCEast() {
  return sc.rich_scalers[5];
}

double StSCReader::getZDCX() {
  return sc.rich_scalers[7];
}

double StSCReader::getZDCWestNoKiller() {
  return sc.rich_scalers[11];
}

double StSCReader::getZDCEastNoKiller() {
  return sc.rich_scalers[12];
}

double StSCReader::getZDCXNoKiller() {
  return sc.rich_scalers[14];
}

double StSCReader::getMult() {
  return sc.rich_scalers[10];
}

double StSCReader::getL0() {
  return 0;
}

double StSCReader::getBBCX() {
  return sc.rich_scalers[2];
}

double StSCReader::getBBCXCTB() {
  return 0;
}

double StSCReader::getBBCWest() {
  return sc.rich_scalers[1];
}

double StSCReader::getBBCEast() {
  return sc.rich_scalers[0];
}

double StSCReader::getBBCYellowBkg() {
  return sc.rich_scalers[3 + flipBBCBkg];
}

double StSCReader::getBBCBlueBkg() {
  return sc.rich_scalers[4 - flipBBCBkg];
}

double StSCReader::getPVPDWest() {
  return sc.rich_scalers[9];
}

double StSCReader::getPVPDEast() {
  return sc.rich_scalers[8];
}

unsigned int StSCReader::getValid() {
  return sc.valid;
}

unsigned int StSCReader::getTime() {
  return sc.time;
}

int StSCReader::getTimelag() {
  return sc.timelag;
}

float StSCReader::getMagField() {
  return sc.mag_field;
}

StSCReader::StSCReader(sc_t *daqsc,unsigned int utime) : fSC(daqsc)
{ FillTime(utime); }

StSCReader::~StSCReader() { }

int StSCReader::close() {
  //  delete fSCImpReader; fSCImpReader=0;
  return 1;
}

int StSCReader::Update() {
  return 1;
}

char StSCReader::thereIsSCData() {
  // Make sure at least one non-zero ZDC or BBC scaler
  //   because of missing SCPresent value in 2005-2006
  if (getZDCWest() || getZDCEast() ||
      getBBCWest() || getBBCEast())  return 7; // TRUE
  return 0; //FALSE
}

TDataSet* StSCReader::getSCTable(unsigned long runno) {
  St_trigDetSums* table = new St_trigDetSums("trigDetSums",1);
  trigDetSums_st* tb = table->GetTable();
  if (useNoKillers) { // use otherwise empty space
    tb->ctbWest      = getZDCWestNoKiller();
    tb->ctbEast      = getZDCEastNoKiller();
    tb->ctbTOFp      = getZDCXNoKiller();
  } else {
    tb->ctbWest      = getCTBWest();
    tb->ctbEast      = getCTBEast();
    tb->ctbTOFp      = getCTBOrTOFp();
  }
  tb->tofp         = getTOFp();
  tb->zdcWest      = getZDCWest();
  tb->zdcEast      = getZDCEast();
  tb->zdcX         = getZDCX();
  tb->mult         = getMult();
  tb->L0           = getL0();
  tb->bbcX         = getBBCX();
  tb->bbcXctbTOFp  = getBBCXCTB();
  tb->bbcWest      = getBBCWest();
  tb->bbcEast      = getBBCEast();
  tb->bbcYellowBkg = getBBCYellowBkg();
  tb->bbcBlueBkg   = getBBCBlueBkg();
  tb->pvpdWest     = getPVPDWest();
  tb->pvpdEast     = getPVPDEast();
  tb->runNumber    = runno;
  tb->timeOffset   = getTime();
  table->SetNRows(1);
  return table;
}
