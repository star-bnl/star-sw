
#ifndef SC_READER_HH
#define SC_READER_HH

#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/EVP/scReader.h"

class SC_Reader {

public:
  SC_Reader(EventReader *er);
  int CTBWest();
  int CTBEast();
  int CTBOrTOFp();
  int TOFp();
  int ZDCWest();
  int ZDCEast();
  int ZDCX();
  int Mult();
  int L0();
  int BBCX();
  int BBCXCTB();
  int BBCWest();
  int BBCEast();
  int BBCYellowBkg();
  int BBCBlueBkg();
  int PVPDWest();
  int PVPDEast();

private:
};

SC_Reader *getSCReader(EventReader *er);

#endif
