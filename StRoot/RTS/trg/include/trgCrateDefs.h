/******
*
*     Layout of DSM Crates
*
*     J.M. Nelson       9 August 2017
*
*****************************************************************************/
#define LAYOUT_VERSION        0x017080901     /* Format: yymmddvv */

typedef struct {
  char name[4];                               /* Contains INF with confNum 20 */
  int length;                                 /* Byte count of data that follows */
  int layout_version;                         /* Layout version */
  int runNumber;                              /* This block will be in a file read by L2 at config time */
  int timeStamp;                              /* It may carry additional information as needed */
  unsigned int data[10];                      /* 40 bytes available for use */
} INFBlock; 

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

typedef struct  {
  char name[4];
  int length;                                 /* Byte count of data that follows */
  unsigned int data[1];                       /* NB: this definition is generic but would vary depending on actual data */
} DSMBlock;

typedef struct  {
  char name[4];
  int length;                                 /* Byte count of data that follows */
  int dataLoss;                               /* Byte count of data truncated due to buffer limitations */
  unsigned int data[1];                       /* NB: this definition is generic but would vary depending on actual data */
} QTBlock;
