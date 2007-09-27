#ifndef _DAQ_FORMATX_H
#define _DAQ_FORMATX_H
// eXtended definitions of DAQ structures for offline
#include "daqFormats.h"

#define CHAR_SCD        "SCD     "
#define SCD_FORMAT_VERSION        0x000500001    // Run FY05, version 1
struct SCD {
  struct bankHeader bh ;
  unsigned int time ;    //unix time
  int mag_field ;
  unsigned int rich_scalers[17] ;
} ;




#endif

