#ifndef TRG_DATA_DEFS
#define TRG_DATA_DEFS
/******
*
*     Layout of new Trigger Data Block
*
*     J.M. Nelson        30 January 2009
*
*     Notes:  The event descriptor will describe data from either
*     the Mk1 or Mk2 TCUs.   The variable TCU_Mark will be 1 for Mk1
*     and 2 for the Mk2 TCU.  Variables not used by one or other of the
*     TCUs will be zero.   
*
*     The data block structure will always begin with a 4 character
*     name, followed by the byte-count of data following.  The structure of
*     data will depend on the configuration of particular crates.  
*
*     Note:  PrePost data will only be available on local trigger disks and
*     will not be present in event files.
******************************************************************************/
#define FORMAT_VERSION        0x12112942      /* Format: yymmddvv */
#define MAX_TRG_BLK_SIZE          122896      /* Current total: 113.25k bytes for pre/post non-zero suppressed data.  Allow 120k */
#define MAX_OFFLEN                    20      /* Depends on the number of crates in the system */

#define ADD_BIT_FORCE          5              /* Force store of this event */
#define ADD_BIT_L2_5           6              /* Level 2.5 abort */
#define ADD_BIT_SIM            7              /* Simulated event - used by DAQ */

    /* Event Descriptor Data Structures */
    
typedef struct {
  char           name[3];                     /* Contains  EVD */
  char           TrgDataFmtVer;               /* Exception for use by DAQ (LS byte of FORMAT_VERSION) */
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
    union {
	struct {
	    unsigned short physicsWord;                 /* Fifo 4 Mk1 TCU. 0 if Mk2 TCU is used */
	    unsigned short TriggerWord;                 /* Fifo 5 Mk1 TCU. 0 if Mk2 TCU is used */
	};
	unsigned int tcuCtrBunch;
    }; 
  unsigned short DSMAddress;                  /* from Fifo 10 (Fifo 6 Mk1 TCU) */
  unsigned short TCU_Mark;                    /* TCU_Mark Mk1=1 Mk2=2 */
  unsigned short npre;                        /* pre value for detector raw data */
  unsigned short npost;                       /* post value for detector raw data */
  unsigned short res1;                        /* Reserved for future use */
} EvtDescData;

      /* L1 DSM data structures */

typedef struct {
  char               name[4];                 /* Contains  L1DS */
  int                length;                  /* Byte count of data that follows */
  unsigned short     TOF[8];                  /* TOF and MTD data */
  unsigned short     VTX[8];                  /* Separate VPD, ZDC and BBC DSMs have been replaced with this one */
  unsigned short     EMC[8];                  /* Contents of 1 EMC IB - results of separate BEMC and EEMC DSMs */
  unsigned short     TPCMask[8];              /* TPC mask for DAQ10K */
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
  unsigned int   C2Result[64];                /* Result from last algorithm */
} TrgSumData;

typedef struct {
  char name[4];
  int length;                                 /* Byte count of data that follows */
  unsigned int data[1];                       /* NB: this definition is generic but would vary depending on actual data */
} DataBlock;

typedef struct {
  char name[4];                               /* Contains BBC */
  int length;                                 /* Byte count of data that follows */
  unsigned short BBClayer1[16];               /* This is the layer1 DSM that feeds the VTX DSM */
  unsigned short ZDClayer1[8];                /* This is the new layer1 ZDC DSM that also feeds the VTX DSM */
  unsigned short VPD[8];                      /* ADC & TAC values for VPD detectors*/
} BBCBlock;

typedef struct {
  char name[4];                               /* Contains MIX */
  int length;                                 /* Byte count of data that follows */
  unsigned short FPDEastNSLayer1[8];          /* FPD east north/south layer 1  */  
  unsigned char  MTD_P2PLayer1[16];           /* Data from MTD and PP2PP */
  unsigned short TOFLayer1[8];                /* This is TOF Layer 1 */
  unsigned short TOF[48];                     /* TOF data */
  unsigned short TPCpreMask[24];              /* EMC, MTD, & TOF TPC Grid Masks */
} MIXBlock;

typedef struct  {
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
  int PrePostList[10];                        /* Offsets to offset/length pairs to Pre and Post crossing */
  int raw_data[MAX_TRG_BLK_SIZE/4];           /* Storage for raw data */
} TriggerDataBlk;



// jml - 9/12/11
// 
// These are the data blocks transferred from L1 --> L2 via stp
// Back engineered and relabled.
// Originally there were multiple instances of these structures in a very confusing set of 
// of local directories and trgStructures.h etc...   Many structs were not consistent
//
// The 2011 nomenclature is just to differentiate between these structs and the obsolete ones.  The 
// underlying format does not change in 2011

// These are the datum that L1 creates...  
typedef struct {
  EvtDescData    EvtDesc;                     /* L1 Event Descriptor Data */  
  L1_DSM_Data    L1_DSM;                      /* L1 DSM Data */
  TrgSumData     TrgSum;                      /* Summary data */
} L1DataType2011;

// This is the specialized L1 version of the dsmMemcpy2Buf packet
// which is shipped L1-->L2    Note that the size is 876 bytes which is not 8 byte aligned
// The start of this buffer has to be 8 byte aligned for STP sends, however...
typedef struct {
  int src_nodeId;                             /* Source nodeId */
  int cur_token;
  int Npre;
  int Npost;
  unsigned int localClock;                    /* Local DSM clock */   
  int numGroup;                               /* Number of DSMs in first group */
  int numDSM;                                 /* Total number of DSMs */
  int byteCount;                              /* Ensure alignment of long long */

  //long long dsmData[RAW_MAX_LEN*11/8];      /* Make this 8-byte aligned */
  L1DataType2011 l1Data;                     // This is the true format of the "dsmData" for L1
} L1_Data_Block2011;

// This structure is for internal use in L1.   This structure has a length that is 8-byte
// aligned so that elements of an array remain 8 byte aligned.
typedef struct {
  L1_Data_Block2011 l1;
  unsigned int currentBusy;    // keep padded though!
  // UINT32 pad;
} L1_Data_Storage2011;

#endif

