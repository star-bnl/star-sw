#ifndef TAB_EEMC_DB_BOXCONF__hh
#define TAB_EEMC_DB_BOXCONF__hh
#include "eemcConstDB.hh" 
/*
 * description:  EEMC Tower Box configuration for one sector
 */  
struct eemcDbBoxconf {
  char name[EEMCDbMaxBoxName];        /*    PMT ID  sector/box/tower */
  int  barcode[EEMCDbMaxBox];         /*    box barcode  */
  int  hvBranch[EEMCDbMaxBox];        /*    branch+ID  of HVsys controller */
  char comment[EEMCDbMaxComment];     /*    PMT ID  sector/box/tower */
};  

#endif

 

