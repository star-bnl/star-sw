#ifndef TAB_EEMC_DB_CWCHAR__hh
#define TAB_EEMC_DB_CWCHAR__hh
#include "eemcConstDB.hh"
/*
 * description:  CW characterization (from Dubna)
 */
struct eemcDbCWchar {
  int   sn;      /*  cw base serial number ,  SER from Dubna */
  int   address; /*  cw base programmable id ,ADDR from Dubna */
  float maxHV;   /*  cw HV (V) for dac=1023,  K1280 from Dubna */
  float dumm;    /*  8-byte align */
  char  comment[EEMCDbMaxComment]; 
};

#endif
