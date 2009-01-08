#ifndef NEW_DAQ_READER_DAQLIB
#  include "StDaqLib/EVP/scReader.h"
#else
#  include "RTS/src/DAQ_READER/daqReader.h"
#  include "RTS/src/DAQ_SC/scReader.h"
#endif /* NEW_DAQ_READER */
#include "SC_Reader.hh"
#include "StDaqLib/GENERIC/EventReader.hh"

#include <assert.h>
#include <math.h>

using namespace OLDEVP;

int SC_Reader::BBCEast() {
  return sc.rich_scalers[0];
} 

int SC_Reader::BBCWest() {
  return sc.rich_scalers[1];
} 

int SC_Reader::BBCX() {
  return sc.rich_scalers[2];
} 

int SC_Reader::BBCBlueBkg() {
  return sc.rich_scalers[3];
} 

int SC_Reader::BBCYellowBkg() {
  return sc.rich_scalers[4];
} 

int SC_Reader::ZDCEast() {
  return sc.rich_scalers[5];
} 

int SC_Reader::ZDCWest() {
  return sc.rich_scalers[6];
} 

int SC_Reader::ZDCX() {
  return sc.rich_scalers[7];
} 

int SC_Reader::PVPDEast() {
  return sc.rich_scalers[8];
} 

int SC_Reader::PVPDWest() {
  return sc.rich_scalers[9];
} 

int SC_Reader::CTBWest() {
  return 0;
} 

int SC_Reader::CTBEast() {
  return 0;
} 

int SC_Reader::TOFp() {
  return 0;
} 

int SC_Reader::CTBOrTOFp() {
  return 0;
} 

int SC_Reader::Mult() {
  return sc.rich_scalers[10];
} 

int SC_Reader::L0() {
  return 0;
} 

int SC_Reader::BBCXCTB() {
  return 0;
} 

SC_Reader::SC_Reader(EventReader *er) {

  //static int call=0;


  //unsigned int UTime = er->getEventInfo().UnixTime;
  //struct tm *time=gmtime((time_t*) &UTime);

  //  LDate = (((1900+time->tm_year)*100 + 1 + time->tm_mon)*100 + time->tm_mday)*100;
  //  LTime = (time->tm_hour*100 + time->tm_min)*100 + time->tm_sec;
#ifndef NEW_DAQ_READER_DAQLIB
  char *datap; // ,ew;
  datap=er->getDATAP(); 
  if (datap) scReader(datap); // call the "event pool" code
#else 
   daqReader *rdr=er->getDaqReader(); assert(rdr);
  ::scReader((char*)rdr); // call the "event pool" code
#endif

}
