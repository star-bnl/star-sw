#ifndef TAB_EEMC_DB_PIXAME__hh
#define TAB_EEMC_DB_PIXNAME__hh
#include "eemcConstDB.hh"
/*
 * description:  EEMC PMT and MAPMT pixel names, 
 */
struct eemcDbPIXname {
  char  name[EEMCDbMaxAdcName];      /*logical name of  pixel */
  char  tubeName[EEMCDbMaxAdcName];   /* hardware name of pixel  */
  char  comment[EEMCDbMaxComment];
};
 
#endif
