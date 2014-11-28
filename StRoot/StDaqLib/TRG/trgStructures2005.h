/*
 * Header Name: trgStructures2005.h                                                       
 * Header Number: x.y                                                                 
 * Package Name: All                                                                  
 * Created: z. milosevich 06Dec99                                                     
 * Description: Global trigger structure header file.                                 
 *              Contains definitions of trigger data structures                       
 * History:                                                                           
 *                                                                                    
 *  06Dec99 zm  Created so offline can use and after modification for mod 8 DMA reads 
 *  08Dec99 zm  Changed Event Descriptor Structure to previous length                 
 *  03Feb00 zm  Changed "ushort ZDCDSM[8]" to "BYTE ZDC[16]" in L0_DSM_Data structure 
 *  22Jul00 egj Added L0RegBytes and L0RegHeader to trgDataHeaders and TrgSumData.    
 *              Also added Mult_Reg, ZDC_Reg and Spare_Reg to TrgSumData, making a    
 *              total of 16 extra bytes. I compensated for this by removing 16 bytes, 
 *              (4 uints) from L2Result, wich is currently unused.                    
 *  08Dec00 JMN Changed char hdr[2] in  TrgSummary to unsigned short, which is packed 
 *  25Feb01 JMN Redesigned and expanded abbreviations to full.                        
 *  12Mar01 JMN Added additional changes from ZM                                      
 *  30May01 JMN NBNBNBNB Changed FORMAT version from 0x12 to 0x13                     
 *  02Jun01 zm  defined bit 6 in addBits info FIFO3 for L2.5 abort                    
 *  27Jun01 JMN Added definition of number of bytes used in trigger summary headers   
 *  28Jun01 JMN Added definition of ADD_BITs                                          
 *  10Apr02 zm  basic strawman for new structures with new TCU and added detectors
 *  22May02 zm  removed references to current non-existent trigger event header
 *  24May02 zm  incorporated eleanor's comments
 *  24May02 zm  the L0_DSM_Data instance changed from DSM to DSMdata to be able to compile on linux
 *  19Jun02 zm  removed or commented out references to TrgEvtHeader
 *  05Aug02 JMN changes in event descriptor - moved filler word to end
 *  15Aug02 HjC correct bytes for FPD
 *  21Nov02 zm  corrections based on eleanor's email: L0 layout for bunch xing, 
 *              special and fpd elements; fpd broken down for east/west nort/south 
 *              vs top/bottom and layer 1 or 0;
 *              also finished XXX_DATA_OFFSET and RAW_XXX_LEN for passing data from
 *              dsm
 *  30Aug03 JMN Added RAW_MAX_LEN being maximum length possible in chain block transfer
 *  14Sep03 JMN Added new dsmMemcpy2Buf
 *  02Nov03 JMN Modified dsmMemcpy2Buf to ensure dsmData is 8-byte aligned.
 *  04Nov03 EGJ Modified RawTrgDet to add in ZDCSMD[32]
 *  28Oct04 JMN Modified RawTrgDet to increased BBC storage from 80 to 96
 *              Format version changed from 0x21 to 0x22
 */

#ifndef trgStructures2005_h
#define trgStructures2005_h

#define y5MAX_L0_DATA_BLOCKS    11              /* Maximum number of L0 Data Blocks:  current + npre + npost */
#define y5MAX_RAW_DATA_BLOCKS   11              /* Maximum number of Raw Data Blocks:  current + npre + npost */
#define y5FORMAT_VERSION   0x22                 /* Format Version number for trigger data */
#define y5EV_DESC_LEN      sizeof(EvtDescData2005)  /* Number of bytes in event descriptor */
#define y5L0DSM_DATA_LEN   sizeof(L0_DSM_Data2005)  /* Size of data block in L0 DSM Tree */
#define y5RAW_DET_DATA_LEN sizeof(RawTrgDet2005)    /* Size of Raw Detector Data from CTB, MWC with headers */
#define y5TRG_SUM_LEN      sizeof(TrgSumData2005)   /* Number of bytes in the trigger summary for DAQ with headers */

#define y5L1_DATA_LEN  (y5EV_DESC_LEN+y5TRG_SUM_LEN)   /* Size of data passed from L1ANA to L2 */ 
#define y5TRG_EVT_LEN  (y5L1_DATA_LEN+(y5MAX_RAW_DATA_BLOCKS*y5RAW_DET_DATA_LEN))  /* Max size of a trigger event */
#define y5TDI_EVT_LEN  (y5EV_DESC_LEN+y5TRG_SUM_LEN+(y5MAX_RAW_DATA_BLOCKS*y5RAW_DET_DATA_LEN)) /* size of event sent to TDI */

#define y5L0_SUM_LEN           148              /* Number of bytes in L0 Summary + Header */
#define y5L1_SUM_LEN           132              /* Number of bytes in L1 Summary + Header */
#define y5L2_SUM_LEN           116              /* Number of bytes in L2 Summary + Header */
#define y5L0_REG_LEN            16              /* Number of bytes in L0 Register + Header */

#define y5RAW_MAX_LEN          272              /* Maximum length of any Chain Block Transfer */
//#define CTB_DATA_OFFSET        8              /* Number of bytes CTB Raw data is offset in raw trigger structure */
//#define RAW_CTB_LEN          256              /* Number of bytes in raw CTB DSMs */
//#define MWC_DATA_OFFSET      272              /* Number of bytes MWC Raw data is offset in raw trigger structure */
//#define RAW_MWC_LEN          128              /* Number of bytes in raw CTB DSMs */
//#define BCE_DATA_OFFSET      408              /* Number of bytes BMC Raw data is offset in raw trigger structure */
//#define RAW_BCE_LEN          240  
//#define BCW_DATA_OFFSET      648
//#define RAW_BCW_LEN          240 
//#define BC1_DATA_OFFSET      888
//#define RAW_BC1_LEN           96 
//#define EEC_DATA_OFFSET      992
//#define RAW_EEC_LEN          176 
//#define FPE_DATA_OFFSET     1176              /* everything after this will have to be re-done */
//#define RAW_FPE_LEN          208
//#define FPW_DATA_OFFSET     1384
//#define RAW_FPW_LEN          208
//#define BBC_DATA_OFFSET     1600
//#define RAW_BBC_LEN          176              /* Number of bytes in BBC and ZDC raw data structures */

#define L2RESULTS_OFFSET_TRG         0
#define L2RESULTS_OFFSET_EMC_CHECK   1
#define L2RESULTS_OFFSET_JPSI        2
#define L2RESULTS_OFFSET_UPS         8
#define L2RESULTS_OFFSET_DIJET       14
#define L2RESULTS_OFFSET_EMC_PED     19
#define L2RESULTS_OFFSET_PIG         20

#define y5ADD_BIT_PILEUP         0              /* Contamination/Pileup bit in event descriptor add-bits */
#define y5ADD_BIT_FORCE          5              /* Force store of this event */
#define y5ADD_BIT_L2_5           6              /* Level 2.5 abort */
#define y5ADD_BIT_SIM            7              /* Simulated event - used by DAQ */

/********** Trigger Structures ***********/

/* Event Descriptor Data Structures */


typedef struct {
  unsigned short TCUdataBytes;
  char           TCUEvtDesc;
  unsigned char  TrgDataFmtVer;
  unsigned int   bunchXing_hi;
  unsigned int   bunchXing_lo;                /* Two parts of RHIC bunch crossing number */
  unsigned short actionWdDetectorBitMask;     /* from Fifo 1 */
  unsigned char  actionWdTrgCommand;          /* from Fifo 1 */
  unsigned char  actionWdDaqCommand;          /* from Fifo 1 */  
  unsigned short TrgToken;                    /* from Fifo 2 */
  unsigned short addBits;                     /* from Fifo 2 - bit 0=Contamination; bit 6=L2.5 abort; bit 7=1 is fake data */
  unsigned short DSMInput;                    /* from Fifo 3 */
  unsigned short externalBusy;                /* from Fifo 3 */
  unsigned short modifiedBusyStatus;          /* from Fifo 4 */
  unsigned short physicsWord;                 /* from Fifo 4 */
  unsigned short TriggerWord;                 /* from Fifo 5 */
  unsigned short DSMAddress;                  /* from Fifo 6 */
  unsigned short contaminationBusyStatus;     /* from Fifo 6 */
  unsigned short npre;                        /* pre value for detector raw data */
  unsigned short npost;                       /* post value for detector raw data */
  unsigned short dummy;                       /* dummy - filler */
} EvtDescData2005;                            /* 40 bytes total */ 

/* Trigger Summary Data Structures */

/* L0 DSM data structures */

typedef struct {
  unsigned short     CPA[32];                 /* Contents of 4 CTB+MWC DSM Input Buffers (IB's) - coarse pixel array*/
  unsigned short     quadDSM[8];              /* Contents of 1 CTB+MWC DSM IB - outputs of previous 4 */
  unsigned short     lastDSM[8];              /* Contents of last DSM IB - results of all DSM trees */
  unsigned short     VTX[8];                  /* Separate ZDC and BBC DSMs have been replaced with this one */
  unsigned short     EMC[8];                  /* Contents of 1 EMC IB - results of separate BEMC and EEMC DSMs */
  unsigned short     BCdata[16];              /* Contents of 2 Bunch Crossing DSMs IB's */       
  unsigned short     specialTriggers[8];      /* Contents of 1 Special Trigger DSM - all the special trigger requests */
  unsigned short     FPD[8];                  /* Contents of 1 FPD IB - we are installing this DSM this year but it */
} L0_DSM_Data2005;                            /* 192 bytes total */


/* Summary data */

typedef struct {
  unsigned short TrgSumBytes;
  unsigned short TrgSumHeader;
  unsigned int   L1Sum[2];          /* L1 Summary */
  unsigned int   L2Sum[2];          /* L2 Summary */
  unsigned short L0SumBytes;
  unsigned short L0SumHeader;
  L0_DSM_Data2005 DSMdata;          /* L0 DSM Data from DSM Tree */
  unsigned short L1SumBytes;
  unsigned short L1SumHeader;
  unsigned int   L1Result[32];      /* Result from L1 CPU */
  unsigned short L2SumBytes;
  unsigned short L2SumHeader;
  unsigned int   L2Result[32];      /* Result from L2 CPU */
} TrgSumData2005;                   /* 480 bytes total */

/* Data structure passed between L1ANA and L2 */

typedef struct {
  EvtDescData2005    EvtDesc;           /* L1 Event Descriptor Data */  
  TrgSumData2005     TrgSum;            /* Summary data */
} L1dataType2005;                       /* 520 bytes */
  
/* Raw Trigger Detector data structures */

typedef struct {
  unsigned short RawDetBytes;
  char           RawDetHeader[2];
  unsigned short CTBdataBytes;
  char           CTBdataHeader[2];
  unsigned char  CTB[256];              /* CTB raw data */
  unsigned short MWCdataBytes;
  char           MWCdataHeader[2];
  unsigned int   MWCfiller;             /* Dummy to bring header to mod 8 */
  unsigned char  MWC[128];              /* MWC raw data */
  unsigned short BEMCdataBytes;
  char           BEMCdataHeader[2];
  unsigned int   BEMCfiller;            /* Dummy to bring header to mod 8 */
  unsigned char  BEMCEast[240];         /* next year there will be 15 DSMs covering the East half, all in one crate   */
  unsigned char  BEMCWest[240];         /* this year there will be 15 DSMs covering the West half, all in one crate   */
  unsigned short BEMClayer1[48];        /* there will be 6 DSMs at layer1 even if only 3 of them have input this year*/
  unsigned short EEMCdataBytes;
  char           EEMCdataHeader[2];
  unsigned int   EEMCfiller;            /* Dummy to bring header to mod 8 */
  unsigned short EEMClayer1[16];        /* the two layer1 DSMs are at the LHS of the crate so they get read first    */
  unsigned char  EEMC[144];             /* next there are the 9 layer0 DSMs, of which only 4-5 have input this year  */
  unsigned short FPDdataBytes;
  char           FPDdataHeader[2];
  unsigned int   FPDfiller;             /* Dummy to bring header to mod 8 */
  unsigned char  FPDEastNSLayer0[112];  /* fpd east north/south layer 0  */
  unsigned short FPDEastNSLayer1[8];    /* fpd east north/south layer 1  */
  unsigned char  FPDEastTBLayer0[64];   /* fpd east top/bottom  layer 0  */
  unsigned short FPDEastTBLayer1[8];    /* fpd east north/south layer 1  */
  unsigned char  FPDWestNSLayer0[112];  /* fpd east north/south layer 0  */
  unsigned short FPDWestNSLayer1[8];    /* fpd east north/south layer 1  */
  unsigned char  FPDWestTBLayer0[64];   /* fpd east north/south layer 0  */
  unsigned short FPDWestTBLayer1[8];    /* fpd east north/south layer 1  */
  unsigned short BBCdataBytes;
  char           BBCdataHeader[2];
  unsigned int   BBCfiller;             /* Dummy to bring header to mod 8 */
  unsigned char  BBC[96];               /* increased from 80 to 96 for the addition of a DSM after layer 0 DSMs */
  unsigned short BBClayer1[16];         /* this is the layer1 DSM that feeds the VTX DSM  */
  unsigned char  ZDCSMD[32];            /* this is the 2 layer0 DSM for the ZDC SMD detector */
  unsigned char  ZDC[16];               /* this is the original ZDC DSM   */
  unsigned short ZDClayer1[8];          /* this is the new layer1 ZDC DSM that also feeds the VTX DSM    */
} RawTrgDet2005;                        /* 1792 bytes total */

/*  Trigger Event Structure */

struct TrgDataType2005 {
  EvtDescData2005  EvtDesc;                                 /* L1 Event Descriptor Data : 40 bytes */  
  TrgSumData2005   TrgSum;                                  /* Summary data: 480 bytes */
  RawTrgDet2005    rawTriggerDet[y5MAX_RAW_DATA_BLOCKS];    /* Raw Detector Data with pre and post History: 11*1792 bytes */
} ;                                                         /* 20232 bytes */

#endif
