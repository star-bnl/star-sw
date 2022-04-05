#ifndef TAB_EEMC_DB_PMTCONF__hh
#define TAB_EEMC_DB_PMTCONF__hh
#include "eemcConstDB.hh" 
/*
 * description:   EEMC Tower PMT configuration for one sector
*/  
struct eemcDbPMTconf {
  char name[EEMCDbMaxPmtName];      /*    PMT ID  sector/box/tower */
  int  barcode[EEMCDbMaxPmt];       /*    PMT  shield barcode  */
  int  sn[EEMCDbMaxPmt];            /*    pmt serial number  */
  int  baseBarcode[EEMCDbMaxPmt];   /*    cw base barcode  */
  int  baseSN[EEMCDbMaxPmt];        /*    cw base serial number  */
  int  baseAddress[EEMCDbMaxPmt];   /*    cw base programmable id  */
  char comment[EEMCDbMaxComment];
};  

#endif

 

