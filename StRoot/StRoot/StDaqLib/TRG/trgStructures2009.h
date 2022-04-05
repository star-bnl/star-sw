#ifndef trgStructures2009_h
#define trgStructures2009_h
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
#define y9FORMAT_VERSION        0x09013040      /* 11 Dec 2008; Version 4.0 Format: yymmddvv */
#define y9MAX_TRG_BLK_SIZE          122880      /* Estimated at 100k bytes including pre/post */
#define y9MAX_OFFLEN                    20      /* Depends on the number of crates in the system */

#define   y9L1_CONF_NUM       1
#define  y9BC1_CONF_NUM       2
#define  y9MXQ_CONF_NUM       3
#define  y9MIX_CONF_NUM       4
#define  y9BCW_CONF_NUM       5
#define  y9BCE_CONF_NUM       6
#define  y9FEQ_CONF_NUM       7
#define  y9BBC_CONF_NUM       8
#define  y9BBQ_CONF_NUM       9
#define  y9FMS_CONF_NUM      10
#define  y9QT1_CONF_NUM      11
#define  y9QT2_CONF_NUM      12
#define  y9QT3_CONF_NUM      13
#define  y9QT4_CONF_NUM      14

#define y9ADD_BIT_PILEUP         0              /* Contamination/Pileup bit in event descriptor add-bits */
#define y9ADD_BIT_FORCE          5              /* Force store of this event */
#define y9ADD_BIT_L2_5           6              /* Level 2.5 abort */
#define y9ADD_BIT_SIM            7              /* Simulated event - used by DAQ */

#define L2RESULTS_2009_OFFSET_EMC_CHECK   1
#define L2RESULTS_2009_OFFSET_EMC_PED     2
#define L2RESULTS_2009_OFFSET_BGAMMA      3
#define L2RESULTS_2009_OFFSET_EGAMMA      6
#define L2RESULTS_2009_OFFSET_DIJET       9 
#define L2RESULTS_2009_OFFSET_UPSILON     17
#define L2RESULTS_2009_OFFSET_BEMCW       20
#define L2RESULTS_2009_OFFSET_BHIEN       42
#define L2RESULTS_2009_OFFSET_EHIEN       0
#define L2RESULTS_2009_OFFSET_BTOW_CAL    0
#define L2RESULTS_2009_OFFSET_ETOW_CAL    0

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
  unsigned short addBits;                     /* used by trigger/daq: bit 5=Force store; bit 6=L2.5 abort; bit 7=1 is fake data */
  unsigned short DSMInput;                    /* only for use with Mk1 TCU.  0 if Mk2 TCU is used */
  unsigned short externalBusy;                /* from Fifo 9 (Fifo 3 Mk1 TCU) */
  unsigned short internalBusy;                /* from Fifo 9 (Mk2 TCU) */
  unsigned short physicsWord;                 /* Fifo 4 Mk1 TCU. 0 if Mk2 TCU is used */
  unsigned short TriggerWord;                 /* Fifo 5 Mk1 TCU. 0 if Mk2 TCU is used */
  unsigned short DSMAddress;                  /* from Fifo 10 (Fifo 6 Mk1 TCU) */
  unsigned short TCU_Mark;                    /* TCU_Mark Mk1=1 Mk2=2 */
  unsigned short npre;                        /* pre value for detector raw data */
  unsigned short npost;                       /* post value for detector raw data */
  unsigned short res1;                        /* Reserved for future use */
} EvtDescData2009;

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
} L1_DSM_Data2009;

      /* Trigger Summary Data Structures */

typedef struct {
  char           name[4];                     /* Contains  TSUM */
  int            length;                      /* Byte count of data that follows */
  unsigned int   L1Sum[2];                    /* L1 Summary */
  unsigned int   L2Sum[2];                    /* L2 Summary */
  unsigned int   L1Result[32];                /* Result from L1 CPU */
  unsigned int   L2Result[64];                /* Result from L2 CPU */
  unsigned int   CResult[64];                 /* Result from last algorithm */
} TrgSumData2009;

typedef struct {
  char name[4];
  int length;                                 /* Byte count of data that follows */
  unsigned int data[1];                       /* NB: this definition is generic but would vary depending on actual data */
} DataBlock2009;

typedef struct {
  char name[4];                               /* Contains BBC */
  int length;                                 /* Byte count of data that follows */
  unsigned short BBClayer1[16];               /* This is the layer1 DSM that feeds the VTX DSM */
  unsigned short ZDClayer1[8];                /* This is the new layer1 ZDC DSM that also feeds the VTX DSM */
  unsigned short VPD[8];                      /* ADC & TAC values for VPD detectors*/
} BBCBlock2009;

typedef struct {
  char name[4];                               /* Contains MIX */
  int length;                                 /* Byte count of data that follows */
  unsigned short FPDEastNSLayer1[8];          /* FPD east north/south layer 1  */  
  unsigned char  MTD_P2PLayer1[16];           /* Data from MTD and PP2PP */
  unsigned short TOFLayer1[8];                /* This is TOF Layer 1 */
  unsigned short TOF[48];                     /* TOF data */
} MIXBlock2009;

typedef struct  {
  char name[4];
  int length;                                 /* Byte count of data that follows */
  int dataLoss;                               /* Byte count of data truncated due to buffer limitations */
  unsigned int data[1];                       /* NB: this definition is generic but would vary depending on actual data */
} QTBlock2009;

typedef struct {
  char name[4];
  int length;
  unsigned char BEMCEast[240];                /* 15 DSMs covering the East half of BEMC */
} BEastBlock2009; 

typedef struct {
  char name[4];
  int length;
  unsigned char BEMCWest[240];                /* 15 DSMs covering the West half of BEMC */
} BWestBlock2009;

typedef struct {
  char name[4];
  int length;
  unsigned short BEMClayer1[48];              /* 6 DSMs for BEMC at layer1 */
  unsigned short EEMClayer1[16];              /* 2 DSMs for EEMC at layer1 */
  unsigned char  EEMC[144];                   /* 9 DSMs for EEMC at layer0 */
} BELayerBlock2009;

typedef struct {
  char name[4];
  int length;
  unsigned char FMS[256];                     /* 16 DSMs for FMS */
} FMSBlock2009;

typedef struct {
  int offset;                                 /* Offset (in bytes) from the start of Trigger block to data */
  int length;                                 /* Length (in bytes) of data */
} TrgOfflen2009; 

typedef struct {
  int FormatVersion;                          /* Trigger Data Definition Version yymmddvv */
  int totalTriggerLength;                     /* Total length (bytes) of complete Trigger Block */
  int eventNumber;                            /* Event number in this run */
  TrgOfflen2009 EventDesc_ofl;                /* Offset/length pair to Event Descriptor */
  TrgOfflen2009 L1_DSM_ofl;                   /* Offset/length pair to L1 DSM Data */
  TrgOfflen2009 Summary_ofl;                  /* Offset/length pair to Summary Data */
  TrgOfflen2009 MainX[y9MAX_OFFLEN];          /* Offset/length pairs for main crossing */
  int PrePostList[10];                        /* Offsets to offset/length pairs to (5) Pre and (5) Post crossing */
  int raw_data[y9MAX_TRG_BLK_SIZE/4];         /* Storage for raw data */
} TriggerDataBlk2009;
  

#endif

