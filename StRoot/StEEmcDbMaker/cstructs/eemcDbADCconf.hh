#ifndef TAB_EEMC_DB_ADCCONF__hh
#define TAB_EEMC_DB_ADCCONF__hh
#include "eemcConstDB.hh" 
/*
 * description:  PMT+MAPMT ADC channel/slot configuration for one sector
 */  
struct eemcDbADCconf {
  char  name[EEMCDbMaxAdcName];      /* sector/subsector/tower or strip or preTower ... */
  int   crate[EEMCDbMaxAdc];         /* crate holding FEEs  */
  int   channel[EEMCDbMaxAdc];       /* channel */
  char  comment[EEMCDbMaxComment];   /* comment */
};

#endif

 

