#ifndef TAB_EEMC_DB_PMTSTAT__hh
#define TAB_EEMC_DB_PMTSTAT__hh
#include "eemcConstDB.hh"
/*
 * description:  EEMC status of Tower
 */

struct eemcDbPMTStatus {
  char            name[EEMCDbMaxPmtName];   /* channel name, eg 06TB12 */
  unsigned short  stat[EEMCDbMaxPmt];       /* status  code, see eemcConstDB.hh for bits definition */
  unsigned short  fail[EEMCDbMaxPmt];       /* failure code, see eemcConstDB.hh for bits definition */
  char            note[EEMCDbMaxPmtName];   /* short, up to 16 char/PMT */
  char            comment[EEMCDbMaxComment];
};
#endif



-- 

