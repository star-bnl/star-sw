#ifndef TAB_EEMC_DB_PMTPED__hh
#define TAB_EEMC_DB_PMTPED__hh
#include "eemcConstDB.hh" 
/*
 * description:  PMT+MAPMT ADC pedestals for one sector
 */  

struct eemcDbPMTped {
  char  name[EEMCDbMaxAdcName];  /*  sector/subsector/tower of strip/pre/post/tower */
  float ped[EEMCDbMaxAdc];       /* pedestal in ADC chan */
  float sig[EEMCDbMaxAdc];       /* width of pedestal in ADC chan */
  char comment[EEMCDbMaxComment];
};

#endif

 
 
