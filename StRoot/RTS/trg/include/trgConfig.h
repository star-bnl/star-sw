/******
*
*     Header file for Configuration information
*
*     J.M. Nelson              February 2001
*
*     24Mar01 JMN      Added pedestal information 
*                      Added RCC_cnf
*     22Aug02 JMN      Added new CONF_NUMs for new crates
*     21Oct02 JMN      Modified DSM_Data to include object number
*     28Oct02 zm       put crate num's here
*     30Aug03 JMN      Removed DSM_cnf.  Added numGroup in DSM_Data
*                      Changed BMC->BC1; BME->BCE; BMW->BCW
*                      Removed references to EEC and renumbered
*                      xx_CONF_NUM and xx_CRATE_NUM
*     23Oct03 JMN      Changes for Config 2004
*             JMN      Changes to DSM_Data, DSMConfigData to store information
*                      to allow Local Clock to be read.
*     05Nov03 JMN      Changed DSM_Data to allow for 17 instead of 16 DSMs
*     22Dec03 JMN      Replaced scaLxCounts record with definition from DB/scaler
*     21Apr04 JMN      Add number of ENG_REGs in DSM_Info
*     21Feb05 JMN      Added DSM_Def_Regs into DSM_Data to hold default registers
*     04Jan07 je       Changed MWC references to MIX
*     05Jan07 JMN      Removed ctb_mwc in DSM_Data.   Now unused_spare1.  L1
*                      flag res1 is not needed.  Now unused_spare2
*     01Feb07 JMN      Added QT1-4 in definitions of CONF_NUM
*                      Added QTConfigData structure 
*     12Mar07 JMN      Changes to L1_Params to reflect new RC_Config.h.  No longer
*                      have tokenMod, nTokensReturn and tokenTimer.   New
*                      tokenBitsOn/Off
*     07Jul07 JMN      Increased MAX_DEF_REGS
*     08Jul07 JMN      Need a version number for Configuration file to distinguish
*                      different behaviour on decoding.  QT register address problem.
*                      Reduce TIER_PADDING from 128 to 120 and insert config_version
*                      and 7 spare ints.   Current config_version is 0.
***********************************************************************************/

#include "RC_Config.h"
#include "DB/scaler/scaLxCounts.h"

#define  PW_BIT_LEN       12                      /* Bit width of Physics Word */
#define  NUM_PW_WORDS     (1 << PW_BIT_LEN)       /* Max Number of Physics Words */
#define  NUM_DET_BITS     (20 - PW_BIT_LEN)       /* Number of Detector bits */
#define  MAX_DEF_REGS     200                     /* Maximum number of DSM registers that can be stored at Config time */
#define  TW_MAX           65536

#define  CONFIG_FILE_VERS   1                     /* New version number for correct QT register definitions */
#define  TIER_PADDING     120
#define  MAX_CONF_NUM      16

#define   L1_CONF_NUM       0
#define  BC1_CONF_NUM       1
#define  MIX_CONF_NUM       2
#define  CTB_CONF_NUM       3
#define  BCW_CONF_NUM       4
#define  BCE_CONF_NUM       5
#define  BDC_CONF_NUM       6
#define  BBC_CONF_NUM       7
#define  FPE_CONF_NUM       8
#define  FMS_CONF_NUM       9
#define  RCC_CONF_NUM      10
#define  QT1_CONF_NUM      11
#define  QT2_CONF_NUM      12
#define  QT3_CONF_NUM      13
#define  QT4_CONF_NUM      14

#define  L1_CRATE_NUM       0              /* Crate number information for FPGA auto load code */
#define BBC_CRATE_NUM       1
#define BC1_CRATE_NUM       2
#define BCE_CRATE_NUM       3
#define BCW_CRATE_NUM       4
#define CTB_CRATE_NUM       5
#define FPE_CRATE_NUM       6
#define FMS_CRATE_NUM       7
#define MIX_CRATE_NUM       8

#define  MAX_LINE_LEN     128              /* Maximum length of line allowed in Configuration Files */

typedef struct L0_Block {
  int numTW;
  int numPW;
  char pre_post[65536];
} L0_info;

typedef struct t_header {
  unsigned int offset;
  unsigned int length;
} t_header;

typedef struct Tier {                      /* Length of header is fixed.  TIER_PADDING reduced */
  t_header offlen[MAX_CONF_NUM];           /* from 128 (ints) to 120 and 8 (ints) inserted */
  char dictionary[MAX_CONF_NUM*4];         /* to provide config_version and 7 spare ints */
  int padding [TIER_PADDING];
  int config_version;
  int spare[7];
} Tier;

typedef struct DSMConfigData {
  int  *Base_Address;                      /* Base Addresses of DSM board */
  char  FPGA_File[MAX_LINE_LEN];           /* Configuration File for FPGA */
  int   Input_Mem_Enable;                  /* Use (or not) Input Memory */
  int   Input_Mem_Play_Rec;                /* Play/Rec in Input Memory if using */
  char  Input_Mem_Data[4][MAX_LINE_LEN];   /* Input Memory Data File, if playing */
  int   Output_Mem_Enable;                 /* Use (or not) Output Memory */
  int   Output_Mem_Play_Rec;               /* Play/Rec in Output Memory if using*/
  char  Output_Mem_Data[MAX_LINE_LEN];     /* Output Memory Data File, if playing */
  char  LUT_Data[4][MAX_LINE_LEN];         /* LUT Data File */
  int   Board;                             /* Board number */
  int   Ped_Offset;                        /* Pedestal offset */
  int   Ped_Gain;                          /* Pedestal gain */
  int   DSM_Eng_Reg;                       /* Number of Eng_Reg entries */
  int   Reg_Mask[32];                      /* Mask for Engine registers */
  int   Reg_Data[32];                      /* Data for Engine registers */
  int   Clock_Reg[3];                      /* If present, addresses for DSM latch and clocks */
  int   Address_Increment;                 /* Input Buffer Address Increment */
  int   FIFO_Blanks;                       /* Number of blanks loaded in FIFO */
  int   Chain_Block_Position;              /* Position in chain block transfer */
  int   Run_Mode;                          /* Mode at end of Configuration */
} DSMConfigData;

typedef struct QTConfigData {
  int  *Base_Address;                      /* Base Addresses of QT board */
  char  FPGA_File[MAX_LINE_LEN];           /* Configuration File for FPGA */
  int   QT_MB_Reg;                         /* Number of Mother board registers */
  int   MB_Reg_Number[2];                  /* Mother board  register number mask */
  int   MB_Reg_Value[64];                  /* Data for Mother board registers */
  int   QT_DB_Reg;                         /* Number of registers for Daughter boards */
  int   DB_Reg_Number[2];                  /* Daughter board register number mask */
  int   DB_Reg_Value[64];                  /* Data for all Daughter board registers */
  int   QT_D1_Reg;                         /* Number of registers for Daughter board 1 */
  int   D1_Reg_Number[2];                  /* Daughter board  register number mask */
  int   D1_Reg_Value[64];                  /* Data for Daughter board registers */
  int   QT_D2_Reg;                         /* Number of registers for Daughter board 2 */
  int   D2_Reg_Number[2];                  /* Daughter board  register number mask */
  int   D2_Reg_Value[64];                  /* Data for Daughter board registers */
  int   QT_D3_Reg;                         /* Number of registers for Daughter board 3 */
  int   D3_Reg_Number[2];                  /* Daughter board  register number mask */
  int   D3_Reg_Value[64];                  /* Data for Daughter board registers */
  int   QT_D4_Reg;                         /* Number of registers for Daughter board 4 */
  int   D4_Reg_Number[2];                  /* Daughter board  register number mask*/
  int   D4_Reg_Value[64];                  /* Data for Daughter board registers */
  char  DB_LUT_Data[MAX_LINE_LEN];         /* LUT Data Files for all Daugter Boards */
  int   Board;                             /* Board number */
  int   Option1;                           /* First option */
  int   Option2;                           /* Second option */
} QTConfigData;

typedef struct L1_Params {
  int MaxTknInSystem;                      /* Maximum token value allowed in system */
  int MinTknInSystem;                      /* Minimum token value allowed in system */
  int Ntokens_avail;                       /* Number of available tokens */
  int tokenBitsOn;                         /* Bits that must be present for a valid token */
  int tokenBitsOff;                        /* Bits that must not be present for a valid token */
  int nToken;                              /* Number of tokens that are required */
  int TCU_Depth;                           /* Number of tokens on TCU fifo */
  int numTW;                               /* Number of Trigger Words defined */
  int tokens_avail[4095];                  /* Array containing the current set of available tokens */
  int eventsRequested;                     /* Number of events requested at start of run or run resume */
  int Tokens_in_use;                       /* Number of tokens issued, but not returned yet */
  int Events_Taken;                        /* Number of events taken since last START RUN */
  int Triggers_Issued;                     /* Number of triggers issued since last START RUN */
  int Last_Token_Issued;                   /* Number of last token to be issued on START_RUN or ADD_TOKEN */
  int L2_Pointer;                          /* Set if L2 is in the run, otherwise 0 */
  char Follow_Abort;                       /* L1 issues L1_Abort if Followed_Status is true */
  char TrgNodesIn[24];                     /* Flags for Trigger Nodes in the run. */
  trgState L1State;                        /* Current state of L1 */
} L1_Params;

typedef struct DSM_Data {
  int numDSM;                              /* Number of DSM boards to be read */
  int numGroup;                            /* Number of DSM boards in first group */
  int numEngReg[17];                       /* Number of Engine Registers from master file */
  int DSM_Obj_Num;                         /* DSM object number - to select entry in DSM_Reg */
  RegValue DSM_Def_Regs[MAX_DEF_REGS];     /* Set of default registers entered at Configuration time */
  RegValue *DSM_Reg;                       /* Address of DSM_ENG_REG from TRG_SETUP */
  int unused_spare1;                       /* Reserved for L1 */
  int unused_spare2;                       /* Reserved for L1 */
  int DSM_Base_Address[17];                /* Saved base addresses */
  int DSM_Addr_Incr[17];                   /* Saved address increments */
  int DSM_Clock_BaseAdr;                   /* Base address of DSM which has the local clock */
  int Clock_Reg[3];                        /* If present, addresses for DSM latch and clocks */
} DSM_Data;    

typedef struct QT_Data {
  int numQT;                               /* Number of QT board to be read */
  int QT_Obj_Num;
  int QT_Base_Address[17];                 /* Base Addresses of QT Boards */
  RegValue QT_Def_Regs[MAX_DEF_REGS];
  RegValue *QT_Reg;
} QT_Data;

typedef struct scalRecord {
  int hdr;                                 /* This *must* contain "DB::" */
  int scaLsize;                            /* sizeof(scaLxCounts) */
  int despatch_time;                       /* Unix time at despatch of this record */
  scaLxCounts scalData;                    /* The counter data */
} scalRecord;

typedef struct L1_Root {
  DSM_Data DSM_Info;
  L1_Params L1_Par;
} L1_Root;

typedef enum {
  DSMrealSrc,                              /* Use DSM boards to read data */
  DSMsimSrc,                               /* Use software to fill data */
  DSMundefined
} DataSrc;

typedef struct RCC_cnf {
  void         *RCCbaseAddress;            /* Base address of RCC */
  int           clockSource;               /* Block source - internal/RHIC clock */
  int           numClockTicks;             /* Number of clock tick errors before setting error bit */
  int           stopRunOnError;            /* Set to stop run or just restart clock */
  int           Delay[12];                 /* Delay values */
} RCC_cnf;
