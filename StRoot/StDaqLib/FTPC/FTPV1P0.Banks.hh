/***************************************************************************
 * $Id: FTPV1P0.Banks.hh,v 1.2 2001/06/24 22:27:25 jcs Exp $
 * Author: M.J. LeVine, J.Klay, H.Huemmler
 ***************************************************************************
 * Description:  Record Formats for Version 1.0
 *      
 *
 *   change log
 *
 ***************************************************************************
 * $Log: FTPV1P0.Banks.hh,v $
 * Revision 1.2  2001/06/24 22:27:25  jcs
 * cleanup comments
 *
 * Revision 1.1  2000/01/18 18:01:20  levine
 * Hummler's implementaiton of FTPC reader. Note that method
 *
 * FTPV1P0_ZS_SR::getFeeSequences(int Fee, int Pin, int *nSeq,
 * 				   Sequence **SeqData)
 *
 * causes exit() since the required #include file has not yet been
 * (correctly) implemented.
 *
 *
 *
 **************************************************************************/

#ifndef RECFORMATS_HH
#define RECFORMATS_HH


#define classname(x) x ## V1P0    //embed version number in bank name

#include <sys/types.h>
#include "StDaqLib/GENERIC/RecHeaderFormats.hh"

/* Used for variable length array declarations to get pointer type correct */
#define VARLENGTH 1

// default swap
struct classname(Bank_FTPP): public Bank
{

  Pointer Chamber[2];        // Two FTP chambers
};

// default swap
struct classname(Bank_FTPCHAP) : public Bank
{
  Pointer RcvBoard[10];	     // 10 rcvboards per chamber      
                             
};

// override swap
struct classname(Bank_FTPRBP) : public Bank
{
  Pointer Sector[3];             // 3 sectors per rcvboard
  char FiberHeader[64];

  int swap();
};

// default swap
struct classname(Bank_FTPAZIP) : public Bank
{
  Pointer Mz[3];              // [0]=mz a, [1]=mz b, [2]=mz c
                             
};

// Default swap
struct classname(Bank_FTPMZP) : public Bank
{
  Pointer FTPADCD;
  Pointer FTPSEQD;
  Pointer FTPADCX;
  Pointer FTPPADK;
  Pointer FTPCPPR;
  Pointer FTPADCR;
  Pointer FTPMZCLD;
  Pointer FTPCFGR;
  Pointer FTPPEDR;
  Pointer FTPRMSR;
  Pointer FTPGAINR;
  Pointer FTPBADR;
};
  
// FTPADCD - currently no compressed data.  This structure
// only works with uncompressed data.
//
// override swap
struct classname(Bank_FTPADCD) : public Bank  // zero suppressed data
{
  char ADC[VARLENGTH];
  int swap();
};

// override swap
struct classname(Bank_FTPADCR) : public Bank   // Mezzanine ADC Raw
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
struct classname(Bank_FTPPADK) : public Bank
{
  INT32 bpADC;
  INT32 bpCPP;
  INT32 bpPED;
  INT32 bpRMS;
  INT32 bpCFG;
  INT32 bpGAIN;
  Coordinates index[320];    

  int swap();
};

struct FTPADCX_entry
{
  INT32 pad_row;
  INT32 FTPADCD_offset;  // offset to padrow in FTPADCD
  INT32 FTPSEQD_offset;  // offset to padrow in FTPSEQD
};

// default swap
struct classname(Bank_FTPADCX) : public Bank  // Mezzanine Index Bank
{
  FTPADCX_entry entry[VARLENGTH]; 
};

// Already defined in StDaqLib/GENERIC/RecHeaderFormats.hh
// struct ASIC_params
// these params used to emulate ASIC behavior in the reader
// {
//   unsigned char thresh_lo;
//   unsigned char thresh_hi;
//   unsigned char n_seq_lo;
//   unsigned char n_seq_hi;
// };

struct CPPR_entry 
{
  INT16 start_time_bin;
  INT16 stop_time_bin;
};

// override swap
struct classname(Bank_FTPCPPR) : public Bank
{
  ASIC_params asic_params;   
  CPPR_entry entry[10240];

  int swap();
};

// override swap
struct classname(Bank_FTPSEQD) : public Bank
{
  INT16 sequence[VARLENGTH];

  int swap();
};

// override swap
struct classname(Bank_FTPCFGR) : public Bank
{
  char FEE_id[320];

  int swap();
};

struct BADR_entry
{
  u_char row;
  u_char pad;
};

// override swap
struct classname(Bank_FTPBADR) : public Bank
{
  BADR_entry badChannel[VARLENGTH];
  int swap();
};

struct classname(Bank_FTPPEDR) : public Bank
{
  INT32 NumEvents;
  char pedestal[163840];    // 320 pads/mez * 512 timebins

  int swap();
};

struct classname(Bank_FTPRMSR) : public Bank
{
  INT32 NumEvents;
  char pedRMSt16[163840];      // pedRMS * 16

  int swap();
};

struct GAINR_entry
{
  INT16 t0t16;        // t0 * 16
  char t0_RMSt16;     // t0_RMS * 16
  char rel_gaint64;   // rel gain ~n 1 * 64
};

struct classname(Bank_FTPGAINR) : public Bank
{
  INT32 NumEvents;
  INT32 MeanGain;
  GAINR_entry Gain[320];  

  int swap();
};

#endif

  

