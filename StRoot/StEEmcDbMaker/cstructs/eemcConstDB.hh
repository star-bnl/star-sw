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


// 0.5 MB per record should be enough for now
#define    EEMCDbMaxXMLData  524288

/*
Use idividual bits of 'stat' to exclude individual
channels from a particular analysis, but let other 
analysis make a different choice.
*/

// status bits (short int)
#define EEMCSTAT_ONLPED   0x0001 // only pedestal
#define EEMCSTAT_STKBT    0x0002 // sticky lower bits
#define EEMCSTAT_HOTHT    0x0004 // hot for HT trigger
#define EEMCSTAT_HOTJP    0x0008 // hot for JP trigger
#define EEMCSTAT_OUTPI0   0x0010 // hot in pi0 analysis

//The remaing 11 bits of 'stat' are free.

/* The 'fail' 16-bits are meant as general abort of a given 
channel. If any bit is set in
'fail' all analysis should exclude this channel.
*/

// failure bits (short int)
#define EEMCFAIL_GARBG  0x0001  // exclude from any anal
#define EEMCFAIL_HVOFF  0x0002  // HV was off
#define EEMCFAIL_NOFIB  0x0004  // signal fiber is broken


#endif 
 
 
