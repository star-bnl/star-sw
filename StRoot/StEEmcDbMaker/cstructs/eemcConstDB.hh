#ifndef __EEMC_CONST_DB_HH
#define __EEMC_CONST_DB_HH

// number of tower PMTs BOXes per sector
#define EEMCDbMaxBox        5

// number of tower PMTs per sector
#define EEMCDbMaxPmt       60

// maximal no. of ADC channels  (PMT+MAPMT) in one sector
#define EEMCDbMaxAdc     1000

// maximal no. entries for any QA type of data
//#define EEMCDbMaxQAbatch     200

// length of any name (string) used in EEMC DB tables
#define EEMCDbMaxName      16

// length of a comment 
#define EEMCDbMaxComment   160


//  this delimiter may _not_ be a part of any name
#define EEMCDbStringDelim  '$'


//  .............  redundant dimensions, check it always .......!

#define   EEMCDbMaxPmtName   960 
// 960 = EEMCDbMaxPmt * EEMCDbMaxName

#define  EEMCDbMaxBoxName     80 
// 80 = EEMCDbMaxBox * EEMCDbMaxName

#define    EEMCDbMaxAdcName  16000
// 16000 = EEMCDbMaxAdc * EEMCDbMaxName

//#define EEMCDbMaxQAbatchName     3200
//3200  =EEMCDbMaxQAbatch * EEMCDbMaxName


// status bits (short int) 
#define EEMCSTAT_NOSIG  0x0000
#define EEMCSTAT_STKBT  0x0001
#define EEMCSTAT_HOTHT  0x0002
#define EEMCSTAT_HOTJP  0x0004

// falure bits (short int)
#define EEMCFAIL_GARBG  0x1000
#define EEMCFAIL_HVOFF  0x0001
#define EEMCFAIL_NOFIB  0x0002

#endif
