#ifndef TAB_EEMC_DB_HVTEMP__hh
#define TAB_EEMC_DB_HVTEMP__hh
#include "eemcConstDB.hh"
/*
 * description:  online-DB, temp reported by HVsys
 */
struct eemcDbHVtemp {
  char  name[EEMCDbMaxName];      /*  PMT ID:  sector/box, eg 06TB */
  float tempC;                    /*  temp in C */  
  char  comment[EEMCDbMaxComment]; 
};

#endif
