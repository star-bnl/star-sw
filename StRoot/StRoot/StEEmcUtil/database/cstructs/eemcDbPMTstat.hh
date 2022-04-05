#ifndef TAB_EEMC_DB_PMTSTAT__hh
#define TAB_EEMC_DB_PMTSTAT__hh
#include "eemcConstDB.hh"
/*
 * description:  EEMC status of Tower/Strip/Shower
 */

struct eemcDbPMTstat {
  char            name[EEMCDbMaxAdcName];  /* sector/subsector/tower of strip/pre/post/tower */
  unsigned short  stat[EEMCDbMaxAdc];      /* status  code, see eemcConstDB.hh for definitions */
  unsigned short  fail[EEMCDbMaxAdc];      /* failure code, see eemcConstDB.hh for definitions */
  char            comment[EEMCDbMaxComment];
};
#endif




