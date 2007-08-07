/***************************************************************************
 *
 *  
 *
 * Author: Herbert Ward
 ***************************************************************************
 *
 * Description: Offline Wrapper for DAQ SC reader classes
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
#include "StSCReader.h"
#include "StDaqLib/SC/SC_Reader.hh"
#include "tables/St_trigDetSums_Table.h"


double StSCReader::getCTBWest() {
  double rv = 0;
  if(!fSCImpReader) return -1;
  rv = fSCImpReader->CTBWest();
  return rv;
}

double StSCReader::getCTBEast() {
  double rv = 0;
  if(!fSCImpReader) return -1;
  rv = fSCImpReader->CTBEast();
  return rv;
}

double StSCReader::getCTBOrTOFp() {
  double rv = 0;
  if(!fSCImpReader) return -1;
  rv = fSCImpReader->CTBOrTOFp();
  return rv;
}

double StSCReader::getTOFp() {
  double rv = 0;
  if(!fSCImpReader) return -1;
  rv = fSCImpReader->TOFp();
  return rv;
}

double StSCReader::getZDCWest() {
  double rv = 0;
  if(!fSCImpReader) return -1;
  rv = fSCImpReader->ZDCWest();
  return rv;
}

double StSCReader::getZDCEast() {
  double rv = 0;
  if(!fSCImpReader) return -1;
  rv = fSCImpReader->ZDCEast();
  return rv;
}

double StSCReader::getZDCX() {
  double rv = 0;
  if(!fSCImpReader) return -1;
  rv = fSCImpReader->ZDCX();
  return rv;
}

double StSCReader::getMult() {
  double rv = 0;
  if(!fSCImpReader) return -1;
  rv = fSCImpReader->Mult();
  return rv;
}

double StSCReader::getL0() {
  double rv = 0;
  if(!fSCImpReader) return -1;
  rv = fSCImpReader->L0();
  return rv;
}

double StSCReader::getBBCX() {
  double rv = 0;
  if(!fSCImpReader) return -1;
  rv = fSCImpReader->BBCX();
  return rv;
}

double StSCReader::getBBCXCTB() {
  double rv = 0;
  if(!fSCImpReader) return -1;
  rv = fSCImpReader->BBCXCTB();
  return rv;
}

double StSCReader::getBBCWest() {
  double rv = 0;
  if(!fSCImpReader) return -1;
  rv = fSCImpReader->BBCWest();
  return rv;
}

double StSCReader::getBBCEast() {
  double rv = 0;
  if(!fSCImpReader) return -1;
  rv = fSCImpReader->BBCEast();
  return rv;
}

double StSCReader::getBBCYellowBkg() {
  double rv = 0;
  if(!fSCImpReader) return -1;
  rv = fSCImpReader->BBCYellowBkg();
  return rv;
}

double StSCReader::getBBCBlueBkg() {
  double rv = 0;
  if(!fSCImpReader) return -1;
  rv = fSCImpReader->BBCBlueBkg();
  return rv;
}

double StSCReader::getPVPDWest() {
  double rv = 0;
  if(!fSCImpReader) return -1;
  rv = fSCImpReader->PVPDWest();
  return rv;
}

double StSCReader::getPVPDEast() {
  double rv = 0;
  if(!fSCImpReader) return -1;
  rv = fSCImpReader->PVPDEast();
  return rv;
}

StSCReader::StSCReader(StDAQReader *daqr) {
  fDAQReader = daqr;
  fSCImpReader = ::getSCReader(daqr->getEventReader());
}

StSCReader::~StSCReader() {
}

int StSCReader::close() {
  //  delete fSCImpReader; fSCImpReader=0;
  return 1;
}

int StSCReader::Update() {
  delete fSCImpReader;
  fSCImpReader = ::getSCReader(fDAQReader->getEventReader());
  // close();
  return 1;
}

char StSCReader::thereIsSCData() {
  // Make sure at least one non-zero ZDC or BBC scaler
  //   because of missing SCPresent value in 2005-2006
  if (fSCImpReader &&
     (getZDCWest() || getZDCEast() ||
      getBBCWest() || getBBCEast())) return 7; // TRUE
  return 0; //FALSE
}

TDataSet* StSCReader::getSCTable(unsigned long runno) {
  St_trigDetSums* table = new St_trigDetSums("trigDetSums",1);
  trigDetSums_st* tb = table->GetTable();
  tb->ctbWest      = getCTBWest();
  tb->ctbEast      = getCTBEast();
  tb->ctbTOFp      = getCTBOrTOFp();
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
  tb->timeOffset  = 0; // not known here?
  table->SetNRows(1);
  return table;
}
