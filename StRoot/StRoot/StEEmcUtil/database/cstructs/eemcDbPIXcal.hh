#ifndef TAB_EEMC_DB_PIXCAL__hh
#define TAB_EEMC_DB_PIXCAL__hh 
#include "eemcConstDB.hh"
/*
 * description:  EEMC calibration for every pixel ( PMT and MAPMT) 
 */
struct eemcDbPIXcal {
  char  name[EEMCDbMaxAdcName];   /*  pixel name*/
  float gain[EEMCDbMaxAdc];       /* gain of the pixel */
  float egain[EEMCDbMaxAdc];      /* error of the gain  */
  char  comment[EEMCDbMaxComment];
};

#endif

