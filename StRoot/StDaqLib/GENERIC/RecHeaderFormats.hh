// Record Formats for Version 1.10
// The headers

// change log
// 03-Jun-99 MJL add Pointer RICH to DATAP

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
  Pointer L3;
  INT32 reserved[102];
};

#endif
