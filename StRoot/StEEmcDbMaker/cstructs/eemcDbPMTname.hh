#ifndef TAB_EEMC_DB_PMTNAME__hh
#define TAB_EEMC_DB_PMTNAME__hh
#include "eemcConstDB.hh"
/*
 * description:  EEMC Tower and PMT names
 */
struct eemcDbPMTname {
  char  name[EEMCDbMaxPmtName];   /*  tower name  */
  char  tubeName[EEMCDbMaxPmtName];   /*  PMT  name  */
  char  comment[EEMCDbMaxComment];
};

#endif
