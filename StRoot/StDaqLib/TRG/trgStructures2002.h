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
/*  22Jul00 egj Added L0RegBytes and L0RegHeader to trgDataHeaders and TrgSumData.     */
/*              Also added Mult_Reg, ZDC_Reg and Spare_Reg to TrgSumData, making a     */
/*              total of 16 extra bytes. I compensated for this by removing 16 bytes,  */
/*              (4 uints) from L2Result, wich is currently unused.                     */
/*  08Dec00 JMN Changed char hdr[2] in  TrgSummary to unsigned short, which is packed  */
/*  25Feb01 JMN Redesigned and expanded abbreviations to full.                         */
/*  12Mar01 JMN Added additional changes from ZM                                       */
/*  30May01 JMN NBNBNBNB Changed FORMAT version from 0x12 to 0x13                      */
/*  02Jun01 zm  defined bit 6 in addBits info FIFO3 for L2.5 abort                     */
/*  27Jun01 JMN Added definition of number of bytes used in trigger summary headers    */
/*  28Jun01 JMN Added definition of ADD_BITs                                           */
/***************************************************************************************/

#define y2MAX_RAW_DATA_BLOCKS   11              /* Maximum number of Raw Data Blocks:  current + npre + npost */
#define y2FORMAT_VERSION   0x13                 /* Format Version number for trigger data */
#define y2EVT_HEAD_LEN     sizeof(TrgEvtHeader) /* Trigger Event Header Length */
#define y2EV_DESC_LEN      sizeof(EvtDescData)  /* Number of bytes in event descriptor */
#define y2L0DSM_DATA_LEN   sizeof(L0_DSM_Data)  /* Size of data block in L0 DSM Tree */
#define y2RAW_DET_DATA_LEN sizeof(RawTrgDet)    /* Size of Raw Detector Data from CTB, MWC with headers */
#define y2TRG_SUM_LEN      sizeof(TrgSumData)   /* Number of bytes in the trigger summary for DAQ with headers */

#define y2L1_DATA_LEN  (y2EVT_HEAD_LEN+y2EV_DESC_LEN+y2TRG_SUM_LEN)   /* Size of data passed from L1ANA to L1DC */ 
#define y2TRG_EVT_LEN  (y2L1_DATA_LEN+(y2MAX_RAW_DATA_BLOCKS*y2RAW_DET_DATA_LEN))  /* Max size of a trigger event */
#define y2TDI_EVT_LEN  (y2EV_DESC_LEN+y2TRG_SUM_LEN+(y2MAX_RAW_DATA_BLOCKS*y2RAW_DET_DATA_LEN)) /* size of event sent to TDI */

#define y2L0_SUM_LEN           148              /* Number of bytes in L0 Summary + Header */
#define y2L1_SUM_LEN           132              /* Number of bytes in L1 Summary + Header */
#define y2L2_SUM_LEN           116              /* Number of bytes in L2 Summary + Header */
#define y2L0_REG_LEN            16              /* Number of bytes in L0 Register + Header */

#define y2CTB_DATA_OFFSET        8              /* Number of bytes CTB Raw data is offset in raw trigger structure */
#define y2RAW_CTB_LEN          256              /* Number of bytes in raw CTB DSMs */
#define y2MWC_DATA_OFFSET      272              /* Number of bytes MWC Raw data is offset in raw trigger structure */
#define y2RAW_MWC_LEN          128              /* Number of bytes in raw CTB DSMs */
#define y2BEMC_DATA_OFFSET     408              /* Number of bytes MWC Raw data is offset in raw trigger structure */
#define y2RAW_BEMC_LEN         128              /* Number of bytes in raw CTB DSMs */

#define y2ADD_BIT_PILEUP         0              /* Pileup bit in event descriptor add-bits */
#define y2ADD_BIT_PRIORITY       1              /* Priority bit */
#define y2ADD_BIT_FORCE          5              /* Force store of this event */
#define y2ADD_BIT_L2_5           6              /* Level 2.5 abort */
#define y2ADD_BIT_SIM            7              /* Simulated event - used by DAQ */
/********** Trigger Structures ***********/

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
  unsigned short TrgSumHeader;
  unsigned short L0SumBytes;
  unsigned short L0SumHeader;
  unsigned short L1SumBytes;
  unsigned short L1SumHeader;
  unsigned short L2SumBytes;
  unsigned short L2SumHeader;
  unsigned short L0RegBytes;
  unsigned short L0RegHeader;
  unsigned short RawDetBytes;
  char           RawDetHeader[2];
  unsigned short CTBdataBytes;
  char           CTBdataHeader[2];
  unsigned short MWCdataBytes;
  char           MWCdataHeader[2];
  unsigned short EMCdataBytes;
  char           EMCdataHeader[2];
} trgDataHeaders2002;

/* Trigger Event Header */

typedef struct {
  unsigned short TrgDataBytes;                /* Total bytes in trigger data */
  unsigned short TrgFiller;
} TrgEvtHeader2002;                               /* 4 Bytes total */

/* Event Descriptor Data Structures */

typedef union {                               /* The contents of Info Fifo 1 */
  struct {
    unsigned short   TrgToken;                /* Trigger Token */
    unsigned short   TrgActionWd;             /* Trigger Action Word */  
  } FIFO1;
  unsigned int       fifo1;
} Info12002;                                      /* 32 bits total */


typedef union {                               /* The contents of Info Fifo 2 */
  struct {
    unsigned short   DSMInput;                /* Last DSM  */
    unsigned short   DSMAddress;              /* DSM address */
  } FIFO2;
  unsigned int       fifo2;
} Info22002;                                      /* 32 bits total */


typedef union {                               /* The contents of Info Fifo 3 */
  struct {
    unsigned char    DetectorBusy;            /* Detector Busy Bits */
    unsigned char    addBits;                 /* Filler Bits - bit 0=pileup; bit 1=priority; bit 6=L2.5 abort; bit 7=1 is fake data */
    unsigned short   TriggerWd;               /* Trigger Word */
  } FIFO3;
  unsigned int       fifo3;
} Info32002;                                      /* 32 bits total */

/* Data structure passed from L1CTL to L1ANA */

typedef struct {
  unsigned short TCUdataBytes;
  char           TCUEvtDesc;
  unsigned char  TrgDataFmtVer;
  unsigned int   bunchXing_hi;
  unsigned int   bunchXing_lo;                /* Two parts of RHIC bunch crossing number */
  Info12002          TCU1;                        /* TCU Info Fifo's */  
  Info22002          TCU2;
  Info32002          TCU3;
  unsigned short npre;
  unsigned short npost;                       /* Dummy to bring total size of struct to modulo 8 bytes */
} EvtDescData2002;                                /* 28 bytes total */ 

/* Trigger Summary Data Structures */

/* L0 DSM data structures */

typedef struct {
  unsigned short     CPA[32];                 /* Contents of 4 CTB+MWC DSM Input Buffers (IB's) - coarse pixel array*/
  unsigned short     quadDSM[8];              /* Contents of 1 CTB+MWC DSM IB - outputs of previous 4 */
  unsigned short     lastDSM[8];              /* Contents of last DSM IB - results of all DSM trees */
  unsigned char      ZDC[16];                 /* Contents of ZDC DSM IB - raw data from ZDC */
  unsigned short     BCdata[16];              /* Contents of 2 Bunch Crossing DSMs IB's */
} L0_DSM_Data2002;                                /* 144 bytes total */

/* Summary data */

typedef struct {
  unsigned short TrgSumBytes;
  unsigned short TrgSumHeader;
  unsigned int   L1Sum[2];                    /* L1 Summary */
  unsigned int   L2Sum[2];                    /* L2 Summary */
  unsigned short L0SumBytes;
  unsigned short L0SumHeader;
  L0_DSM_Data2002    DSM;                         /* L0 DSM Data from DSM Tree */
  unsigned short L1SumBytes;
  unsigned short L1SumHeader;
  unsigned int   L1Result[32];                /* Result from L1 CPU */
  unsigned short L2SumBytes;
  unsigned short L2SumHeader;
  unsigned int   L2Result[28];                /* Result from L2 CPU */
  unsigned short L0RegBytes;
  unsigned short L0RegHeader;
  unsigned short Mult_Reg[3];                 /* The 3 multiplcity thresholds */
  unsigned short ZDC_Reg[2];                  /* The 2 ZDC thresholds */
  unsigned short Spare_Reg;                   /* A spare, brings total for Reg to 16 bytes */
} TrgSumData2002;                              /* 432 bytes total */

/* Data structure passed between L1ANA and L1DC */

typedef struct {
  TrgEvtHeader2002   TrgHead; 
  EvtDescData2002    EvtDesc;                     /* L1 Event Descriptor Data */  
  TrgSumData2002     TrgSum;                      /* Summary data */
} L1dataType2002;                                 /* 464 bytes */
  
/* Raw Trigger Detector data structures */

typedef struct {
  unsigned short RawDetBytes;
  char           RawDetHeader[2];
  unsigned short CTBdataBytes;
  char           CTBdataHeader[2];
  unsigned char  CTB[256];                    /* CTB raw data */
  unsigned short MWCdataBytes;
  char           MWCdataHeader[2];
  unsigned int   MWCfiller;                   /* Dummy to bring header to mod 8 */
  unsigned char  MWC[128];                    /* MWC raw data */
  unsigned short EMCdataBytes;
  char           EMCdataHeader[2];
  unsigned int   EMCfiller;                   /* Dummy to bring header to mod 8 */
  unsigned char  BEMC[128];                   /* BEMC raw data */ /* NOTE!!!! THIS IS TEMPORARY _ MAY CHANGE */
} RawTrgDet2002;                                  /* 536 bytes total */

/*  Trigger Event Structure */

struct TrgDataType2002{
  TrgEvtHeader2002   TrgHead; 
  EvtDescData2002    EvtDesc;                     /* L1 Event Descriptor Data */  
  TrgSumData2002     TrgSum;                      /* Summary data */
  RawTrgDet2002      RAW[MAX_RAW_DATA_BLOCKS];    /* Raw Detector Data with pre and post History */
};                            /* 6360 bytes */


