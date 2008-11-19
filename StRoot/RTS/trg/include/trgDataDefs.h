#ifndef TRG_DATA_DEFS
#define TRG_DATA_DEFS
/******
*
*     Layout of new Trigger Data Block
*
*     J.M. Nelson        November 2008
*
*     Notes:  This is illustrative only since the definition of the
*     Event Descriptor will change once the new TCU has been commissioned.
*     Similarly, the data from Level 1 (L1_DSM_Data) will be radically
*     different.   The amount of trigger summary data is also likely to
*     change.   
*
*     The data block structure will always begin with a 4 character
*     name, followed by the byte-count of data following.  The structure of
*     data will depend on the configuration of particular crates.  
*
*     Note:  PrePost data will only be available on local trigger disks and
*     will not be present in event files.
******************************************************************************/
#define FORMAT_VERSION        0x08111340      /* 13 Nov 2008; Version 4.0 Format: yymmddvv */
#define MAX_TRG_BLK_SIZE          100000      /* Estimated at 100k bytes including pre/post */
#define MAX_OFFLEN                    20      /* Depends on the number of crates in the system */

    /* Event Descriptor Data Structures */
    
typedef struct {
  char           name[3];                     /* Contains  EVD */
  char           TrgDataFmtVer;               /* Exception for use by DAQ */
  int            length;                      /* Byte count of data that follows */
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
  unsigned short dummy;                       /* dummy - filler since this structures must be zero (mod 4) */
} EvtDescData;

      /* L1 DSM data structures */

typedef struct {
  char               name[4];                 /* Contains  L1DS */
  int                length;                  /* Byte count of data that follows */
  unsigned short     TOF[8];                  /* TOF and MTD data */
  unsigned short     VTX[8];                  /* Separate VPD, ZDC and BBC DSMs have been replaced with this one */
  unsigned short     EMC[8];                  /* Contents of 1 EMC IB - results of separate BEMC and EEMC DSMs */
  unsigned short     BCdata[16];              /* Contents of 2 Bunch Crossing DSMs IB's */       
  unsigned short     specialTriggers[8];      /* Contents of 1 Special Trigger DSM - all the special trigger requests */
  unsigned short     FPD[8];                  /* Contents of 1 FMS and FPD IB */
  unsigned short     lastDSM[8];              /* Contents of last DSM IB - results of all DSM trees */
} L1_DSM_Data;

      /* Trigger Summary Data Structures */

typedef struct {
  char           name[4];                     /* Contains  TSUM */
  int            length;                      /* Byte count of data that follows */
  unsigned int   L1Sum[2];                    /* L1 Summary */
  unsigned int   L2Sum[2];                    /* L2 Summary */
  unsigned int   L1Result[32];                /* Result from L1 CPU */
  unsigned int   L2Result[64];                /* Result from L2 CPU */
  unsigned int   CResult[64];                 /* Result from last algorithm */
} TrgSumData;

typedef struct {
  char name[4];
  int length;                                 /* Byte count of data that follows */
  unsigned int data[1];                       /* NB: this definition is generic but would vary depending on actual data */
} DataBlock;

typedef struct {
  char name[4];
  int length;                                 /* Byte count of data that follows */
  unsigned short     CPA[16];                 /* Contents of 2 CTB DSM Input Buffers (IB's) - coarse pixel array */
  unsigned short     MTD[8];                  /* TAC values for MTD's MRPCs and overlapping CTB trays */
  unsigned short     VPD[8];                  /* ADC & TAC values for VPD detectors*/
  unsigned short     CTB[8];                  /* CTB ADC sums and topology for East & West combined */ 
  unsigned short     lastDSM[8];              /* Contents of last DSM IB - results of all DSM trees */
  unsigned short     VTX[8];                  /* Separate ZDC and BBC DSMs have been replaced with this one */
  unsigned short     EMC[8];                  /* Contents of 1 EMC IB - results of separate BEMC and EEMC DSMs */
  unsigned short     BCdata[16];              /* Contents of 2 Bunch Crossing DSMs IB's */       
  unsigned short     specialTriggers[8];      /* Contents of 1 Special Trigger DSM - all the special trigger requests */
  unsigned short     FPD[8];                  /* Contents of 1 FPD IB - we are installing this DSM this year but it */
} L0DataBlk;

typedef struct {
  char name[4];
  int length;                                 /* Byte count of data that follows */
  unsigned char  BBC[96];                     /* increased from 80 to 96 for the addition of a DSM after layer 0 DSMs */
  unsigned short BBClayer1[16];               /* this is the layer1 DSM that feeds the VTX DSM  */
} BBCBlock;

typedef struct {
  char name[4];
  int length;                                 /* Byte count of data that follows */
  int dataLoss;                               /* Byte count of data truncated due to buffer limitations */
  unsigned int data[1];                       /* NB: this definition is generic but would vary depending on actual data */
} QTBlock;

typedef struct {
  char name[4];
  int length;
  unsigned char BEMCEast[240];                /* 15 DSMs covering the East half of BEMC */
} BEastBlock; 

typedef struct {
  char name[4];
  int length;
  unsigned char BEMCWest[240];                /* 15 DSMs covering the West half of BEMC */
} BWestBlock;

typedef struct {
  char name[4];
  int length;
  unsigned short BEMClayer1[48];              /* 6 DSMs for BEMC at layer1 */
  unsigned short EEMClayer1[16];              /* 2 DSMs for EEMC at layer1 */
  unsigned char  EEMC[144];                   /* 9 DSMs for EEMC at layer0 */
} BELayerBlock;

typedef struct {
  char name[4];
  int length;
  unsigned char FMS[256];                     /* 16 DSMs for FMS */
} FMSBlock;

typedef struct {
  int offset;                                 /* Offset (in bytes) from the start of Trigger block to data */
  int length;                                 /* Length (in bytes) of data */
} TrgOfflen; 

typedef struct {
  int FormatVersion;                          /* Trigger Data Definition Version yymmddvv */
  int totalTriggerLength;                     /* Total length (bytes) of complete Trigger Block */
  int eventNumber;                            /* Event number in this run */
  TrgOfflen EventDesc_ofl;                    /* Offset/length pair to Event Descriptor */
  TrgOfflen L1_DSM_ofl;                       /* Offset/length pair to L1 DSM Data */
  TrgOfflen Summary_ofl;                      /* Offset/length pair to Summary Data */
  TrgOfflen MainX[MAX_OFFLEN];                /* Offset/length pairs for main crossing */
  int PrePostList[10];                        /* Offsets to offset/length pairs to (5) Pre and (5) Post crossing */
  int raw_data[MAX_TRG_BLK_SIZE/4];           /* Storage for raw data */
} TriggerDataBlk;
  

#endif

