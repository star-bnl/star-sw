/***************************************************************************************/
/* Header Name: trgStructures.h                                                        */
/* Header Number: X.Y                                                                  */
/* Package Name: All                                                                   */
/* Created: z. milosevich 06Dec99                                                      */
/* Description: Global trigger structure header file.                                  */
/*              Contains definitions of trigger data structures                        */
/* History:                                                                            */
/*                                                                                     */
/*  06Dec99 zm  Created so offline can use and after modification for mod 8 DMA reads  */
/*  08Dec99 zm  Changed Event Descriptor Structure to previous length                  */
/*  03Feb00 zm  Changed "unsigned short ZDCDSM[8]" to "unsigned char ZDC[16]" in L0_DSM_Data structure  */
/***************************************************************************************/
#ifndef trgStructures2000_h
#define trgStructures2000_h

/* several shortcut definitions */

#define y0MAX_RAW_DATA_BLOCKS   11   /* Maximum number of Raw Data Blocks:  current + npre + npost */
                               
#define y0FORMAT_VERSION      0x11   /* Format Version number for trigger data */
#define y0EVT_HEAD_LEN           4   /* Trigger Event Header Length */
#define y0EV_DESC_LEN           28   /* Number of bytes in event descriptor */
#define y0L0DSM_DATA_LEN       144   /* Size of data block in L0 DSM Tree */
#define y0RAW_DET_DATA_LEN     408   /* Size of Raw Detector Data from CTB, MWC with headers */
#define y0TRG_SUM_LEN          432   /* Number of bytes in the trigger summary for DAQ with headers */

#define y0L1_DATA_LEN  (y0EVT_HEAD_LEN+y0EV_DESC_LEN+y0TRG_SUM_LEN)   /* Size of data passed from L1ANA to L1DC */ 

#define y0TRG_EVT_LEN  (y0L1_DATA_LEN+(y0MAX_RAW_DATA_BLOCKS*y0RAW_DET_DATA_LEN))  /* Max size of a trigger event */
#define y0TDI_EVT_LEN  (y0EV_DESC_LEN+y0TRG_SUM_LEN+(y0MAX_RAW_DATA_BLOCKS*y0RAW_DET_DATA_LEN)) /* size of event sent to TDI */

#define y0CTB_DATA_OFFSET        8  /* Number of bytes CTB Raw data is offset in raw trigger structure */
#define y0RAW_CTB_LEN          256  /* Number of bytes in raw CTB DSMs */

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
  unsigned char  TrgDataFmtVer;
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
} trgDataHeaders2000;

/* Trigger Event Header */

typedef struct {
  unsigned short TrgDataBytes;  /* total bytes in trigger data */
  unsigned short TrgFiller;
} TrgEvtHeader2000;     /* 4 Bytes total */

/* Event Descriptor Data Structures */

typedef union {      /*  The contents of Info Fifo 1 */
  struct {
    unsigned short   TrgToken;     /* Trigger Token */
    unsigned short   TrgActionWd;  /* Trigger Word */  
  } FIFO1;
  unsigned long      fifo1;
} Info12000;             /* 32 bits total */


typedef union {      /* The contents of Info Fifo 2 */
  struct {
    unsigned short   DSMInput;      /*   Last DSM  */
    unsigned short   DSMAddress;    /*   DSM address */
  } FIFO2;
  unsigned long      fifo2;
} Info22000;             /* 32 bits total */


typedef union {      /* The contents of Info Fifo 3 */
  struct {
    unsigned char    DetectorBusy;      /* detector Busy Bits */
    unsigned char    addBits;           /* filler Bits - bit 0=pileup; bit 1=priority; bit 7=1 is fake data */
    unsigned short   TriggerWd;         /* Trigger Word */
  } FIFO3;
  unsigned long      fifo3;
} Info32000;             /* 32 bits total */

/* Data structure passed from L1CTL to L1ANA */

typedef struct {
  unsigned short TCUdataBytes;
  char           TCUEvtDesc;
  unsigned char  TrgDataFmtVer;
  unsigned int           bunchXing_hi;
  unsigned int           bunchXing_lo;   /* Two parts of RHIC bunch crossing number */
  Info12000          TCU1;           /* TCU Info Fifo's */  
  Info22000          TCU2;
  Info32000          TCU3;
  unsigned short         npre;
  unsigned short         npost;          /* Dummy to bring total size of struct to modulo 8 bytes */
} EvtDescData2000;          /* 28 bytes total */ 

/* Trigger Summary  Data Structures */

/* L0 DSM data structures */

typedef struct {
  unsigned short             CPA[32];        /* Contents of 4 CTB+MWC DSM Input Buffers (IB's) - coarse pixel array*/
  unsigned short             quadDSM[8];     /* Contents of 1 CTB+MWC DSM IB - outputs of previous 4 */
  unsigned short             lastDSM[8];     /* Contents of last DSM IB - results of all DSM trees */
  unsigned char               ZDC[16];        /* Contents of ZDC DSM IB - raw data from ZDC */
  unsigned short             BCdata[16];     /* Contents of 2 Bunch Crossing DSMs IB's */
} L0_DSM_Data2000;          /* 144 bytes total */


/* summary data */

typedef struct {
  unsigned short TrgSumBytes;
  char           TrgSumHeader[2];
  unsigned int           L1Sum[2];        /* L1 Summary */
  unsigned int           L2Sum[2];        /* L2 Summary */
  unsigned short L0SumBytes;
  char           L0SumHeader[2];
  L0_DSM_Data2000    DSM;             /* L0 DSM Data from DSM Tree */
  unsigned short L1SumBytes;
  char           L1SumHeader[2];
  unsigned int           L1Result[32];       /* Result from L1 CPU */
  unsigned short L2SumBytes;
  char           L2SumHeader[2];
  unsigned int   L2Result[32];       /* Result from L2 CPU */
} TrgSumData2000;        /* 432 bytes total */


/* Data structure passed between L1ANA and L1DC */

typedef struct {
  TrgEvtHeader2000   TrgHead; 
  EvtDescData2000    EvtDesc;       /* L1 Event Descriptor Data */  
  TrgSumData2000     TrgSum;        /* summary data */
} L1dataType2000;           /* 448 bytes */
  
/* Raw Trigger detector data structures */

typedef struct {
  unsigned short RawDetBytes;
  char           RawDetHeader[2];
  unsigned short CTBdataBytes;
  char           CTBdataHeader[2];
  unsigned char           CTB[256];         /* CTB raw data */
  unsigned short MWCdataBytes;
  char           MWCdataHeader[2];
  unsigned long  MWCfiller;        /* dummy to bring header to mod 8 */
  unsigned char           MWC[128];         /* MWC raw data */
  unsigned short EMCdataBytes;
  char           EMCdataHeader[2];
  unsigned long  EMCfiller;        /* dummy to bring header to mod 8 */
} RawTrgDet2000;            /* 408 bytes total */

/*  Trigger Event Structure */

class TrgDataType2000 { public:
  TrgEvtHeader2000   TrgHead; 
  EvtDescData2000    EvtDesc;       /* L1 Event Descriptor Data */  
  TrgSumData2000     TrgSum;        /* summary data */
  RawTrgDet2000      RAW[y0MAX_RAW_DATA_BLOCKS]; /* raw Detector Data with pre and post History */
};          /* 4952 bytes */
#endif 
