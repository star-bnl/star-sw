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

#endif
