#ifndef TAB_EEMC_DB_PMTCHAR__hh
#define TAB_EEMC_DB_PMTCHAR__hh
#include "eemcConstDB.hh"
/*
 * description:  PMT characterization (from Sasha)
 */
struct eemcDbPMTchar {
  int   sn;     /*    pmt base serial number*/
  float speRes; /*    single p. e.  resolution  (ch?)  */
  float nomHV;  /*  HV (V) at gain 2x10^5  */
  float darkC;  /*  dark current, (pA)    */
  char  comment[EEMCDbMaxComment];
};

#endif
