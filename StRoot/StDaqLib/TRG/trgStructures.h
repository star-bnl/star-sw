/***************************************************************************************/
/* Header Name: trgStructures.h                                                        */
/* Header Number: x.y                                                                  */
/* Package Name: All                                                                   */
/* Created: z. milosevich 06Dec99                                                      */
/* Description: Global trigger structure header file.                                  */
/*              Contains definitions of trigger data structures                        */
/* History:                                                                            */
/*                                                                                     */
/*  06Dec99 zm  Created so offline can use and after modification for mod 8 DMA reads  */
/*  08Dec99 zm  Changed Event Descriptor Structure to previous length                  */
/*  03Feb00 zm  Changed "ushort ZDCDSM[8]" to "BYTE ZDC[16]" in L0_DSM_Data structure  */
/***************************************************************************************/


/* several shortcut definitions */

#ifndef uint
#define uint unsigned int
#endif

#ifndef ushort
#define ushort unsigned short
#endif

#ifndef ulong
#define ulong unsigned long
#endif

typedef unsigned char BYTE;
typedef unsigned int  WORD;

#define MAX_RAW_DATA_BLOCKS   11   /* Maximum number of Raw Data Blocks:  current + npre + npost */
                               
#define FORMAT_VERSION      0x11   /* Format Version number for trigger data */
#define EVT_HEAD_LEN           4   /* Trigger Event Header Length */
#define EV_DESC_LEN           28   /* Number of bytes in event descriptor */
#define L0DSM_DATA_LEN       144   /* Size of data block in L0 DSM Tree */
#define RAW_DET_DATA_LEN     408   /* Size of Raw Detector Data from CTB, MWC with headers */
#define TRG_SUM_LEN          432   /* Number of bytes in the trigger summary for DAQ with headers */

#define L1_DATA_LEN  (EVT_HEAD_LEN+EV_DESC_LEN+TRG_SUM_LEN)   /* Size of data passed from L1ANA to L1DC */ 

#define TRG_EVT_LEN  (L1_DATA_LEN+(MAX_RAW_DATA_BLOCKS*RAW_DET_DATA_LEN))  /* Max size of a trigger event */
#define TDI_EVT_LEN  (EV_DESC_LEN+TRG_SUM_LEN+(MAX_RAW_DATA_BLOCKS*RAW_DET_DATA_LEN)) /* size of event sent to TDI */

#define CTB_DATA_OFFSET        8  /* Number of bytes CTB Raw data is offset in raw trigger structure */
#define RAW_CTB_LEN          256  /* Number of bytes in raw CTB DSMs */

/********** trigger structures ***********/

/*
 *
 *  Trigger Data Headers
 *
 */

typedef struct {
  unsigned short TrgDataBytes;
  unsigned short TrgFiller;
  unsigned short TCUdataBytes;
  BYTE           TrgDataFmtVer;
  char           TCUEvtDesc;
  unsigned short TrgSumBytes;
  char           TrgSumHeader[2];
  unsigned short L0SumBytes;
  char           L0SumHeader[2];
  unsigned short L1SumBytes;
  char           L1SumHeader[2];
  unsigned short L2SumBytes;
  char           L2SumHeader[2];
  unsigned short RawDetBytes;
  char           RawDetHeader[2];
  unsigned short CTBdataBytes;
  char           CTBdataHeader[2];
  unsigned short MWCdataBytes;
  char           MWCdataHeader[2];
  unsigned short EMCdataBytes;
  char           EMCdataHeader[2];
} trgDataHeaders;

/* Trigger Event Header */

typedef struct {
  unsigned short TrgDataBytes;  /* total bytes in trigger data */
  unsigned short TrgFiller;
} TrgEvtHeader;     /* 4 Bytes total */

/* Event Descriptor Data Structures */

typedef union {      /*  The contents of Info Fifo 1 */
  struct {
    unsigned short   TrgToken;     /* Trigger Token */
    unsigned short   TrgActionWd;  /* Trigger Word */  
  } FIFO1;
  unsigned long      fifo1;
} Info1;             /* 32 bits total */


typedef union {      /* The contents of Info Fifo 2 */
  struct {
    unsigned short   DSMInput;      /*   Last DSM  */
    unsigned short   DSMAddress;    /*   DSM address */
  } FIFO2;
  unsigned long      fifo2;
} Info2;             /* 32 bits total */


typedef union {      /* The contents of Info Fifo 3 */
  struct {
    BYTE             DetectorBusy;      /* detector Busy Bits */
    BYTE             addBits;           /* filler Bits - bit 0=pileup; bit 1=priority; bit 7=1 is fake data */
    unsigned short   TriggerWd;         /* Trigger Word */
  } FIFO3;
  unsigned long      fifo3;
} Info3;             /* 32 bits total */

/* Data structure passed from L1CTL to L1ANA */

typedef struct {
  unsigned short TCUdataBytes;
  char           TCUEvtDesc;
  BYTE           TrgDataFmtVer;
  uint           bunchXing_hi;
  uint           bunchXing_lo;   /* Two parts of RHIC bunch crossing number */
  Info1          TCU1;           /* TCU Info Fifo's */  
  Info2          TCU2;
  Info3          TCU3;
  ushort         npre;
  ushort         npost;          /* Dummy to bring total size of struct to modulo 8 bytes */
} EvtDescData;          /* 28 bytes total */ 

/* Trigger Summary  Data Structures */

/* L0 DSM data structures */

typedef struct {
  ushort             CPA[32];        /* Contents of 4 CTB+MWC DSM Input Buffers (IB's) - coarse pixel array*/
  ushort             quadDSM[8];     /* Contents of 1 CTB+MWC DSM IB - outputs of previous 4 */
  ushort             lastDSM[8];     /* Contents of last DSM IB - results of all DSM trees */
  BYTE               ZDC[16];        /* Contents of ZDC DSM IB - raw data from ZDC */
  ushort             BCdata[16];     /* Contents of 2 Bunch Crossing DSMs IB's */
} L0_DSM_Data;          /* 144 bytes total */


/* summary data */

typedef struct {
  unsigned short TrgSumBytes;
  char           TrgSumHeader[2];
  uint           L1Sum[2];        /* L1 Summary */
  uint           L2Sum[2];        /* L2 Summary */
  unsigned short L0SumBytes;
  char           L0SumHeader[2];
  L0_DSM_Data    DSM;             /* L0 DSM Data from DSM Tree */
  unsigned short L1SumBytes;
  char           L1SumHeader[2];
  uint           L1Result[32];       /* Result from L1 CPU */
  unsigned short L2SumBytes;
  char           L2SumHeader[2];
  uint           L2Result[32];       /* Result from L2 CPU */
} TrgSumData;        /* 432 bytes total */


/* Data structure passed between L1ANA and L1DC */

typedef struct {
  TrgEvtHeader   TrgHead; 
  EvtDescData    EvtDesc;       /* L1 Event Descriptor Data */  
  TrgSumData     TrgSum;        /* summary data */
} L1dataType;           /* 448 bytes */
  
/* Raw Trigger detector data structures */

typedef struct {
  unsigned short RawDetBytes;
  char           RawDetHeader[2];
  unsigned short CTBdataBytes;
  char           CTBdataHeader[2];
  BYTE           CTB[256];         /* CTB raw data */
  unsigned short MWCdataBytes;
  char           MWCdataHeader[2];
  unsigned long  MWCfiller;        /* dummy to bring header to mod 8 */
  BYTE           MWC[128];         /* MWC raw data */
  unsigned short EMCdataBytes;
  char           EMCdataHeader[2];
  unsigned long  EMCfiller;        /* dummy to bring header to mod 8 */
} RawTrgDet;            /* 408 bytes total */

/*  Trigger Event Structure */

typedef struct {
  TrgEvtHeader   TrgHead; 
  EvtDescData    EvtDesc;       /* L1 Event Descriptor Data */  
  TrgSumData     TrgSum;        /* summary data */
  RawTrgDet      RAW[MAX_RAW_DATA_BLOCKS]; /* raw Detector Data with pre and post History */
} TrgDataType;          /* 4952 bytes */
