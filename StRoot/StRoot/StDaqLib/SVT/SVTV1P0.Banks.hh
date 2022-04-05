/***************************************************************************
 *      
 * $Id: SVTV1P0.Banks.hh,v 1.6 2009/09/23 16:13:03 fine Exp $
 *      
 * Author: Marcelo Munhoz, J. Schambach
 ***************************************************************************
 *      
 * Description:  Record Formats for Version 2.0
 *      
 ***************************************************************************
 *      
 * $Log: SVTV1P0.Banks.hh,v $
 * Revision 1.6  2009/09/23 16:13:03  fine
 * fix pointer arithmetics and cpu type
 *
 * Revision 1.5  2004/03/01 18:05:47  fisyak
 * Account for new place for rts.h, add osf
 *
 * Revision 1.4  2003/10/28 20:58:30  perev
 *  Linux ==> __linux__
 *
 * Revision 1.3  2001/04/18 19:47:25  ward
 * StDaqLib/SVT stuff from Jo Schambach.
 *
 * Revision 1.2  2000/09/30 16:14:31  fisyak
 * Add hp
 *
 * Revision 1.1  2000/06/06 18:08:31  jml
 * Initial version of SVT Readers (author: marcello munholz, helen caines)
 *
 *      
 **************************************************************************/

#ifndef SVTV1P0BANKS_HH
#define SVTV1P0BANKS_HH

#define SVT_HYPERSECTORS 4
#define SVT_WAFERS 216
#define SVT_TOTALHYBRIDS 432
#define SVT_HYBRIDS 2
#define SVT_MZHYBRIDS 6
#define SVT_ANODES 240
#define SVT_MZANODES 256
#define SVT_MXSEQUENCE 8

#define classname(x) x ## V1P0    //embed version number in bank name

#include <sys/types.h>
#include "StDaqLib/GENERIC/RecHeaderFormats.hh"

/* Used for variable length array declarations to get pointer type correct */
#define VARLENGTH 1

// default swap
struct classname(Bank_SVTP): public Bank
{
  Pointer HyperSector[4];   // Hypersectors = 1 -> Rcv Boards 1-6 (VME crate = 1)
                            //                2 -> Rcv Boards 7-12(VME crate = 1)
                            //                3 -> Rcv Boards 1-6 (VME crate = 2)
                            //                4 -> Rcv Boards 7-12(VME crate = 2)
};

// default swap
struct classname(Bank_SVTSECP) : public Bank
{
  Pointer RcvBoard[12];      
};

// override swap
struct classname(Bank_SVTRBP) : public Bank
{
  Pointer Mz[3];             // [0]=mz a, [1]=mz b, [2]=mz c
  char FiberHeader[64];

  int swap();
};

// Default swap
struct classname(Bank_SVTMZP) : public Bank
{
  Pointer SVTADCD;
  Pointer SVTSEQD;
  Pointer SVTADCX;
  Pointer SVTANODK;
  Pointer SVTCPPR;
  Pointer SVTADCR;
  Pointer SVTMZCLD;
  Pointer SVTCFGR;
  Pointer SVTPEDR;
  Pointer SVTRMSR;
  Pointer SVTGAINR;
  Pointer SVTBADR;
};
  
// SVTADCD - currently no compressed data.  This structure
// only works with uncompressed data.
//
// override swap
struct classname(Bank_SVTADCD) : public Bank  // zero suppressed data
{
  char ADC[VARLENGTH];
  int swap();
};

// override swap
struct classname(Bank_SVTADCR) : public Bank   // Mezzanine ADC Raw
{
  char ADC[VARLENGTH];
  int swap();
};

// from this bank on, the SVT banks were not defined yet
// they were left here to be modfied

struct SVTCoordinates
{
#if defined (sparc) || defined (__hpux)
  u_char barrel;
  u_char ladder;
  u_char waferID;  // 4:hybrid, 4:wafer
  u_char hybridID;  // 1:not used, 2:transition board, 3:wafer, 2:hybrid
#elif defined(__i386__) || defined(__osf__) || defined(__x86_64__)
  u_char hybridID;  // 1:not used, 2:transition board, 3:wafer, 2:hybrid
  u_char waferID;  // 4:hybrid, 4:wafer
  u_char ladder;
  u_char barrel;    
#else
#error "Unknown machine type"
#endif
};

// override swap
struct classname(Bank_SVTANODK) : public Bank
{
  INT32 bpADC;
  INT32 bpCPP;
  INT32 bpPED;
  INT32 bpRMS;
  INT32 bpCFG;
  INT32 bpGAIN;
  SVTCoordinates index[6];

  int swap();
};

struct SVTADCX_entry
{
  INT32 hybrid;
  INT32 SVTADCD_offset;  // offset to hybrid in SVTADCD
  INT32 SVTSEQD_offset;  // offset to hybrid in SVTSEQD
};

// default swap
struct classname(Bank_SVTADCX) : public Bank  // Mezzanine Index Bank
{
  SVTADCX_entry entry[VARLENGTH];    // up to 4 repeats
};


struct SVTCPPR_entry
{
  INT16 start_time_bin;
  INT16 stop_time_bin;
};

// override swap
struct classname(Bank_SVTCPPR) : public Bank
{
  ASIC_params asic_params; 
  SVTCPPR_entry entry[12288];

  int swap();
};


// override swap
struct classname(Bank_SVTSEQD) : public Bank
{
  INT16 sequence[VARLENGTH];

  int swap();
};

// override swap
struct classname(Bank_SVTCFGR) : public Bank
{
  char FEE_id[6];

  int swap();
};

struct SVTBADR_entry
{
  u_char hybrid;
  u_char anode;
};

// override swap
struct classname(Bank_SVTBADR) : public Bank
{
  SVTBADR_entry badChannel[VARLENGTH];
  int swap();
};

struct classname(Bank_SVTPEDR) : public Bank
{
  INT32 NumEvents;
  char pedestal[196608];    // 6 hybrids/mez * 256 anodes * 128 timebins

  int swap();
};

struct classname(Bank_SVTRMSR) : public Bank
{
  INT32 NumEvents;
  char pedRMSt16[196608];      // pedRMS * 16

  int swap();
};

struct SVTGAINR_entry
{
  INT16 t0t16;        // t0 * 16
  char t0_RMSt16;     // t0_RMS * 16
  char rel_gaint64;   // rel gain ~n 1 * 64
};

struct classname(Bank_SVTGAINR) : public Bank
{
  INT32 NumEvents;
  INT32 MeanGain;
  SVTGAINR_entry Gain[6];

  int swap();
};

// override swap
struct classname(Bank_SVTMZCLD) : public Bank
{
  INT32 NumHybrids;
  INT32 stuff[10]; // placeholder for actual bank contents
  int swap();
};

#endif

  

