/***************************************************************************
 * $Id: RecHeaderFormats.hh,v 1.4 2000/01/19 16:20:22 levine Exp $
 * Author: M.W. Schulz, Jeff Landgraf, M.J. LeVine
 ***************************************************************************
 * Description: Bank header formats common to all detectors in STAR:
 *              declarations and definitions
 *
 * change log
 * 03-Jun-99 MJL add Pointer RICH to DATAP
 * 23-Jun-99 MJL reinstate Pointer TRG to DATAP (deleted accidentally)
 *
 ***************************************************************************
 * $Log: RecHeaderFormats.hh,v $
 * Revision 1.4  2000/01/19 16:20:22  levine
 * #define FTP_MZPADS suppiled in this version
 *
 * Revision 1.3  1999/07/02 04:37:42  levine
 * Many changes - see change logs in individual programs
 *
 *
 **************************************************************************/


#ifndef RECHEADERFORMATS_HH
#define RECHEADERFORMATS_HH

typedef int INT32;    // WORD according to data format
typedef short INT16;  // SHORT according to data format


#define TPC_SECTORS 24
#define TPC_TIMEBINS 512
#define TPC_PADROWS 45
#define TPC_MAXPADS 182
#define TPC_MZPADS 384
#define TPC_MXSEQUENCE 31

#define FTP_SECTORS 60
#define FTP_TIMEBINS 512
#define FTP_PADROWS 2
#define FTP_MAXPADS 160
#define FTP_MZPADS 320
#define FTP_MXSEQUENCE 31

char *name2str(char *type);
void dump_data(char *buffer, int size, int width = 8);

struct Logical_Record_Header
{
  char BankType[8];   // padded with trailing nulls
  INT32 BankLength;   // Logical Record Header + Body
  INT32 RunNumber;
  INT32 FormatVersion;  // 65536*major + minor version #
  INT32 ByteOrder;      // 0x04030201 (or it should be)
  INT32 reserved1;
  INT32 reserved2;
  INT32 reserved3;
  INT32 CRC;

  int swap();
  void print();
};

struct Logical_Record
{
  Logical_Record_Header header;
  INT32 RecordLength;   // From LRH to end of record including all banks
  INT32 BlockingFactor; // # of events / logical record
  char RecordType[8];   // Right Padded with spaces
  INT32 CRC;            // For Logical Record Header???

  int swap();           // Swap swaps.  The CRC is not automatically recalc.
  void set_CRC();       // Sets the CRC -- make sure the byte order has
                        // is 0x04030201 before using this function
                        // otherwise your CRC is byte reversed
  int test_CRC();       // This swaps only a copy of the bank.  This must be
                        // tested before swapping the actual bank
                        // The actual swapping treats shorts/bytes different
                        // than words and removes info needed to match up the
                        // CRC.  The proper procedure is:

                        // 1. test_CRC()
                        // 2. swap()
                        // 3. set_CRC() ....
  void print(int level=0);
};

struct Bank_Header
{
  char BankType[8];
  INT32 BankLength;     // Total length in words including header
  INT32 BankId;         
  INT32 FormatVersion;  // Format version, encoded as for LRH
  INT32 ByteOrder;      // 0x04030201
  INT32 FormatNumber;   // Bank dependent
  INT32 Token;
  INT32 reserved1;
  INT32 CRC;            // Whole bank minus this word
  
  int swap();
  void print();
};

struct Bank
{
  Bank_Header header;

  void set_CRC();  
  int test_CRC();
  int swap();
  void print(int level=0);  
};

struct Pointer          // Used repeatedly
{
  INT32 offset;
  INT32 length;
};

// Default swap
struct Bank_DATAP : public Bank
{
  INT32 EventLength;    // From first header word of DATAP
  INT32 Time;
  INT32 EventNumber;    // unique within run
  INT32 TriggerWord;
  INT32 TriggerInWord;
  INT32 DetectorPresence;  // 0-TPC, 1-SVT, 2-TOF, 3-EMC, 4-Shower Max
                           // 5-FTPC, 6-reserved, 7-reserved, 8-Trig
                           // 9-L3
  Pointer TPC;             // Offsets from first word of DATAP
  Pointer SVT;
  Pointer TOF;
  Pointer EMC;
  Pointer ShowerMax;
  Pointer FTPC;
  Pointer reserved_det;
  Pointer RICH;
  Pointer TRG;
  Pointer L3;
  INT32 reserved[102];
};

// some unavoidable detector-specific definitions:
//
struct ASIC_params 
  // these params used to emulate ASIC behavior in the reader
{
  unsigned char thresh_lo;
  unsigned char thresh_hi;
  unsigned char n_seq_lo;
  unsigned char n_seq_hi;
};



#endif
