#ifndef TAB_EEMC_DB_PMTCAL__hh
#define TAB_EEMC_DB_PMTCAL__hh
#include "eemcConstDB.hh"
/*
 * description:  EEMC Tower Box  calibration for one sector
 */
struct eemcDbPMTcal {
  char  name[EEMCDbMaxPmtName];   /*    PMT ID  sector/box/tower */
  float gain[EEMCDbMaxPmt];       /* pmt effective gain of the tower */
  float hv[EEMCDbMaxPmt];         /* pmt actual HV (V), dac=HV/HVmax*1023 */
  char  comment[EEMCDbMaxComment];
};

#endif

