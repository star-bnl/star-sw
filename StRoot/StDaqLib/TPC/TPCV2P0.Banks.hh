/***************************************************************************
 * $Id: TPCV2P0.Banks.hh,v 1.3 1999/07/22 17:56:27 levine Exp $
 * Author: M.W. Schulz, Jeff Landgraf and M.J. LeVine
 ***************************************************************************
 * Description:  Record Formats for Version 2.0
 *      
 *
 *   change log
 * 02-Jun-99 MJL changed HyperSector[12] to HyperSector[24]
 * 12-Jul-99 MJL add TPCMZCLD
 *
 ***************************************************************************
 * $Log: TPCV2P0.Banks.hh,v $
 * Revision 1.3  1999/07/22 17:56:27  levine
 * add TPCMZCLD (mezz cluster pointer bank) description
 *
 * Revision 1.2  1999/07/02 04:43:23  levine
 * Many changes -
 *  navigates to head of TPCP bank independent of position.
 *  move declarations out of loops where they were upsetting some compilers
 *  suppress output from class libraries with run-time switch EventReader.verbose
 *  added TPCV2P0_CPP_SR::getAsicParams()
 *
 *
 **************************************************************************/

#ifndef RECFORMATS_HH
#define RECFORMATS_HH


#define classname(x) x ## V2P0    //embed version number in bank name

#include <sys/types.h>
#include "StDaqLib/GENERIC/RecHeaderFormats.hh"

/* Used for variable length array declarations to get pointer type correct */
#define VARLENGTH 1

// default swap
struct classname(Bank_TPCP): public Bank
{
  Pointer HyperSector[24];   // Hypersectors 1->sectors 1,2
                             //              3->sectors 3,4
                             // etc... upto 23->sectors 23,24
};

// default swap
struct classname(Bank_TPCSECP) : public Bank
{
  Pointer RcvBoard[12];      // [0-5] are odd sector, rcv 1...6
                             // [6-11] are even sector, rcv 1...6
};

// override swap
struct classname(Bank_TPCRBP) : public Bank
{
  Pointer Mz[3];             // [0]=mz a, [1]=mz b, [2]=mz c
  char FiberHeader[64];

  int swap();
};

// Default swap
struct classname(Bank_TPCMZP) : public Bank
{
  Pointer TPCADCD;
  Pointer TPCSEQD;
  Pointer TPCADCX;
  Pointer TPCPADK;
  Pointer TPCCPPR;
  Pointer TPCADCR;
  Pointer TPCMZCLD;
  Pointer TPCCFGR;
  Pointer TPCPEDR;
  Pointer TPCRMSR;
  Pointer TPCGAINR;
  Pointer TPCBADR;
};
  
// TPCADCD - currently no compressed data.  This structure
// only works with uncompressed data.
//
// override swap
struct classname(Bank_TPCADCD) : public Bank  // zero suppressed data
{
  char ADC[VARLENGTH];
  int swap();
};

// override swap
struct classname(Bank_TPCADCR) : public Bank   // Mezzanine ADC Raw
{
  char ADC[VARLENGTH];
  int swap();
};

struct Coordinates
{
  u_char pad_row;    // 0xff signifies invalid
  u_char pad;
};

// override swap
struct classname(Bank_TPCPADK) : public Bank
{
  INT32 bpADC;
  INT32 bpCPP;
  INT32 bpPED;
  INT32 bpRMS;
  INT32 bpCFG;
  INT32 bpGAIN;
  Coordinates index[384];

  int swap();
};

struct ADCX_entry
{
  INT32 pad_row;
  INT32 TPCADCD_offset;  // offset to padrow in TPCADCD
  INT32 TPCSEQD_offset;  // offset to padrow in TPCSEQD
};

// default swap
struct classname(Bank_TPCADCX) : public Bank  // Mezzanine Index Bank
{
  ADCX_entry entry[VARLENGTH];    // up to 4 repeats
};


struct CPPR_entry
{
  INT16 start_time_bin;
  INT16 stop_time_bin;
};

// override swap
struct classname(Bank_TPCCPPR) : public Bank
{
  ASIC_params asic_params; 
  CPPR_entry entry[12288];

  int swap();
};


// override swap
struct classname(Bank_TPCSEQD) : public Bank
{
  INT16 sequence[VARLENGTH];

  int swap();
};

// override swap
struct classname(Bank_TPCCFGR) : public Bank
{
  char FEE_id[384];

  int swap();
};

struct BADR_entry
{
  u_char row;
  u_char pad;
};

// override swap
struct classname(Bank_TPCBADR) : public Bank
{
  BADR_entry badChannel[VARLENGTH];
  int swap();
};

struct classname(Bank_TPCPEDR) : public Bank
{
  INT32 NumEvents;
  char pedestal[196608];    // 348 pads/mez * 512 timebins

  int swap();
};

struct classname(Bank_TPCRMSR) : public Bank
{
  INT32 NumEvents;
  char pedRMSt16[196608];      // pedRMS * 16

  int swap();
};

struct GAINR_entry
{
  INT16 t0t16;        // t0 * 16
  char t0_RMSt16;     // t0_RMS * 16
  char rel_gaint64;   // rel gain ~n 1 * 64
};

struct classname(Bank_TPCGAINR) : public Bank
{
  INT32 NumEvents;
  INT32 MeanGain;
  GAINR_entry Gain[384];

  int swap();
};

// override swap
struct classname(Bank_TPCMZCLD) : public Bank
{
  INT32 NumRows;
  INT32 stuff[10]; // placeholder for actual bank contents
  int swap();
};

#endif

  

