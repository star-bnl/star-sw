#ifndef SC_READER_HH
#define SC_READER_HH
class EventReader; 

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
  int ZDCWestNoKiller();
  int ZDCEastNoKiller();
  int ZDCXNoKiller();
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
  short flipBBCBkg;
};

SC_Reader *getSCReader(EventReader *er);

#endif
