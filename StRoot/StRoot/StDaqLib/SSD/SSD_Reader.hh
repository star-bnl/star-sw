
#ifndef SSD_READER_HH
#define SSD_READER_HH

#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/EVP/ssdReader.h"

class SSD_Reader {

public:
  SSD_Reader(EventReader *er);
  int ssdData( int  ladder, 
	       char eastWest,
	       int  channel,
	       int &data  ,  // for ssd_data.mode=0 ie data 
	       int &ped   ,  // for ssd_data.mode=1 ie pedestal run
	       int &noise   // "      "        "     "       "
  );

private:
  int      SSDDaqLadder(char,int);
  unsigned int LDate;
  unsigned int LTime;

};

SSD_Reader *getSSDReader(EventReader *er);

#endif
