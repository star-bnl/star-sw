#ifndef TAB_EEMC_DB_HVSYS__hh
#define TAB_EEMC_DB_HVSYS__hh
#include "eemcConstDB.hh"
/*
 * description:  online-DB, state a _single_ CWcell reported by HVsys
 */
struct eemcDbHVsys {
  char  name[EEMCDbMaxName];    /*    PMT ID:  sector/box/tower, eg 06TB09 */
  int   hvBranch;        /*    branch+ 10*ID_of_HVsys_controller */
  int   address; /*  cw base programmable id ,ADDR from Dubna */
  float maxHV;   /*  cw HV (V) for dac=1023,  K1280 from Dubna */
  int dac;     /* pmt  dac=HV/HVmax*1023 */
  float hv;      /* pmt actual HV (V) */
  char  comment[EEMCDbMaxComment]; 
};

#endif
