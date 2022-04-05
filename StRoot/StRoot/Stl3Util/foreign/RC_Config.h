#ifndef RC_CONFIG_HH
#define RC_CONFIG_HH

#include <sys/types.h>

/***********************************************************************************
 * Configuration files for online
 *
 * The configuration files are organized into tiers
 *
 *     TIER 1 - Internal trigger configuration  (named)
 *     DICT   - Dictionary structure for trigger configurations
 *     SETUP  - Sets of parameters that can be choosen as a group
 *     RUN    - Parameters that are set independently each run
 *
 ************************************************************************************/

#include <stdio.h>
#include <string.h>

#define CONFIG_VERSION 902
#define RC_PARAM_FILE_BASE_PATH "/RTS/conf/handler/"
#define TRG_CONFIG_FILE_NAME ""
#define DAQ_CONFIG_FILE_NAME ""
#define SC_CONFIG_FILE_NAME ""
#define L3_CONFIG_FILE_NAME ""

#define TRG_BASE_PATH "/home/startrg/trg/handler/conf"
#define DAQ_BASE_PATH "/RTS/conf/handler"

#define MAX_TRG_DICT_ENTRIES 200
#define MAX_L0_DSM 16
#define MAX_TW 100
#define MAX_NUMBER_L1_ALG 20      /* Total Algorithms */
#define MAX_NUMBER_L2_ALG 20
#define MAX_L1_TRG 10             /* At a time */
#define MAX_L2_TRG 10             
#define MAX_TRG_ALG_PARAM 10      /* Number of parameters for an algorithm */
#define N_SPEC_TRG 20             /* For TCD's */
#define GL3_ALG_MAX_NUM 20

#define MAX_ID 16
#define MAX_SUB 4
#define MAX_INST 100
#define MAX_NODES 200
#define MAX_THREADS 400

#define MAX_STR_LEN 40

#define RTS_STAT_TOKENS_DONE 8
#define RTS_STAT_FORCE 9
#define RTS_STAT_REQUEST 10

#define RTS_ICCP_MSG_LEN 120

#define TRG_INVALID_WORD 0xffffffff

#define RTS_STARTRUN_FLAG    1
#define RTS_STOPRUN_FLAG     2
#define RTS_SENDCONFIG_FLAG  4

//
// What systems are in the run
//
struct TASK
{
  u_short node;
  u_char task;
  u_char inrun;                // Is the system in the run?
  u_char stoprun;              // Is the system allowed to stop the run?
  u_char start_order;          // start order
  u_char typically_inrun;
  u_char res2;
};
  
struct SUBSYS_TASKS
{
  TASK nodes[MAX_NODES];
};

//
// Global configuration
//
struct GLB_CAL              // Eventually to be passed from evp or similar?
{
  float t0;
  float driftVelocity;
  float driftLength;
  float szError;            // sz hit average error
  float xyError;            // xy hit average error
  float xVertex;            // x position primary vertex
  float yVertex;            // y position primary vertex
  float dxVertex;           // error in vertex position
  float dyVertex;
};

struct GLB_SETUP 
{
  char filename_base[MAX_STR_LEN];       // base filename for datafiles
  char GLB_SETUP_name[MAX_STR_LEN];
  char TRG_SETUP_name[MAX_STR_LEN];
  char DAQ_SETUP_name[MAX_STR_LEN];
  char L3_SETUP_name[MAX_STR_LEN];
  char SC_SETUP_name[MAX_STR_LEN];
};

struct GLB_RUN
{
  int run_number;
  int run_type;                          // copied from daq_setup
  GLB_CAL cal;
  char GLB_SETUP_name[MAX_STR_LEN];
  float bField;
};

//
// Trigger configuration
//

struct TRG_DICT_ENTRY 
{
    char field[MAX_STR_LEN];
    int objectIdx;
    int registerIdx;
    char label[MAX_STR_LEN];
};

struct TRG_DICT
{
    char name[MAX_STR_LEN];                               // Name of this dict file
    TRG_DICT_ENTRY entry[MAX_TRG_DICT_ENTRIES];           
    char comment[2000];
};


struct TW_DEF
{
  char trgLabel[MAX_STR_LEN];
  int TW;
  int AW;
  int PS;
  int nPre;
  int nPost;
  int off;
  int on;
  int enabled;
};

struct TRG_ALG
{
    int trgId;
    int preScale;
    int inputParam[MAX_TRG_ALG_PARAM];
    int threshold[MAX_TRG_ALG_PARAM];
};

struct TCD_SETUP
{
    int tcdId;
    int trgCmd;
    int seconds;
};

struct TRG_SETUP 
{
    char name[MAX_STR_LEN];                               // Name of this setup file
    char TIER1_name[MAX_STR_LEN];                         // Tier 1 filename
    char TRG_DICT_name[MAX_STR_LEN];                      // Name of corresponding dict
    
    int L1_DSM_Reg_Data_values[MAX_L0_DSM][32];
    int DC_DSM_Reg_Data_values[MAX_L0_DSM][32];
    int CTB_DSM_Reg_Data_values[MAX_L0_DSM][32];
    int BEMC_DSM_Reg_Data_values[MAX_L0_DSM][32];
    int MWC_DSM_Reg_Data_values[MAX_L0_DSM][32];
    
    TRG_ALG L1trg[MAX_L1_TRG];
    TRG_ALG L2trg[MAX_L2_TRG];

    TCD_SETUP special[N_SPEC_TRG];

    TW_DEF tw_def[MAX_TW];                      // Trigger word definitions
};

struct TRG_RUN 
{
    // L1CTL_User_Params
    int configOpt;         // Force configuration depth
    
    int l1LogOpt;          // Where log messages are written
    int l1LogThr;          // Severity of saved log messages

    int dcLogOpt;          // Where log messages are written
    int dcLogThr;          // Severity of saved log messages
    
    int ctbLogOpt;         // Where log messages are written
    int ctbLogThr;         // Severity of saved log messages

    int bemcLogOpt;        // Where log messages are written
    int bemcLogThr;        // Severity of saved log messages

    int l2LogOpt;          // Where log messages are written
    int l2LogThr;          // Severity of saved log messages

    int mwcLogOpt;
    int mwcLogThr;

    int rccLogOpt;
    int rccLogThr;

    int tcdLogThr;         // Severity of saved log messages

    int nTokenReturn;      // Number of tokens to time 
    int maxTknInSystem;    // Max token value allowed in system
    int minTknInSystem;    // Min token value allowed in system
    int tokenMod;          // Skip every n tokens
    int tokenTimer;        // turns of/on timing n tokens
    
    int clockSource;       // Clock source
    int numClockTicks;     // number of clock tick errors before setting flag
    int stopRunOnError;    // set to stop run/restart clock on error
    
    int l2DataWrite;       // L2 data writing switch
    int dataWriteTimer;    // L2 clock ticks between data writing
    int everyNEvents;      // how many events to skip when writing
    int nAbort;            // Frequency of test aborts
};

//
// DAQ configuration
//

struct DAQ_DAQ
{
  int force_reset;
  int evp_suppression;
  int event_suppression;
  int evb_log_level;
  int myri_log_level;
  int bb_log_level;
  int gb_log_level;
  int destination;
  int test_run;
  int write_pedestals;     // write pedestal events to evb?
  int force_det_announce;  // for dets to send det_announce token
};

struct DAQ_DET_SETUP
{
  int ped_mode;
  int gain_mode;
  int analysis;
  int default_format;

  u_char asic_seq_hi;
  u_char asic_seq_lo;
  u_char asic_thr_hi;
  u_char asic_thr_lo;
  int time_bin_lo;
  int time_bin_hi;
  int clust_charge_lo;
};

struct DAQ_SUBDET
{
  u_int rb_mask;
};

struct DAQ_DET
{
  int log_level;
  int use_sl3;

  int res1;
  int res2;
  int res3;
  int res4;

  DAQ_SUBDET sub[24];
};



struct DAQ_SETUP
{
  char DAQ_SETUP_name[MAX_STR_LEN];
  int run_type;

  DAQ_DET_SETUP detectors[MAX_ID];
};

struct DAQ_RUN
{ 
  DAQ_DAQ        daq;

  DAQ_DET        detectors[MAX_ID];
};

//
// Level 3 configuration
//
struct L3_GL3_RUN
{
  int write_daq;           // 0 means no, N means write the data every n'th accepted event
  int write_local;         // 0 means no, N means every nth event
  int log_level;

  int accept_triggered;    // Accept every N events that satisfy the gl3 algorithms 
  int accept_automatic;    // Accept every N events whether they satisfy the gl3 algorithm or not
};

struct L3_GL3_SETUP
{
  int formats;             // Bitfield.  0 means Gl3 decides
  int formats_local;
};

struct L3_ALGORITHMS
{
  int alg_id;
  int preScale;
  int postScale;
  int GI1;                  // Algorithm parameters
  int GI2;
  int GI3;
  int GI4;
  int GI5;
  float GF1;
  float GF2;
  float GF3;
  float GF4;
  float GF5;
};

struct L3_SL3_SETUP
{
  int algorithm;            // What tracker?
  int version;              // What version of the tracker?

  short minHitsPerTrack;    // Minimum hits for a track
  short nEta;               // # volumes in eta
  short nPhi;               // # volumes in phi
  short padding;            //-------------------------
  float maxChi2Primary;     // Maximum chi2 for a track to be primary
  short rowInnerMost;       // Innermost row considered
  short rowOuterMost;       // Outermost row considered
  short rowStart;           // Where to start the track search
  short rowEnd;             // Where to stop the track search
  float hitChi2Cut;         // Maximum chi2 for a hit to be part of a track
  float goodHitChi2;        // Chi2 to stop looking for next hit
  float trackChi2Cut;       // Maximum track chi2
  float deta;               // Eta search range
  float dphi;               // Phi search range
  float ptMinHelixFit;      // Minimum pt to apply helix fit
  float maxTime;            // Maximum time tracker may run

  int TX0;                  // Expansion parameters
  int TX1;
  int TX2;
  int TX3;
  int TX4;
  int TX5;
  int TX6;
  int TX7;
  int TX8;
  int TX9;
};

struct L3_SL3_RUN
{
  int log_level;
};

struct L3_SETUP 
{ 
  char name[MAX_STR_LEN];
  L3_GL3_SETUP     gl3;
  L3_ALGORITHMS    gl3_algorithms[GL3_ALG_MAX_NUM];
  L3_SL3_SETUP     sl3;
};


struct L3_RUN 
{ 
  L3_GL3_RUN       gl3;
  L3_SL3_RUN       sl3;
};

struct SC_SETUP { 
  char name[MAX_STR_LEN];
};
struct SC_RUN 
{
  int reserved;
};

/************************************************************************************
 * STAR_CFG fully defines the STAR run
 ************************************************************************************/

struct STAR_CFG
{
  SUBSYS_TASKS subsys_tasks;

  GLB_SETUP glb_setup;        // Modified the order so that referenced 
  GLB_RUN   glb_run;

  TRG_DICT  trg_dict;
  TRG_SETUP trg_setup;        // structs follow struct referencing
  TRG_RUN   trg_run;

  DAQ_SETUP daq_setup;
  DAQ_RUN   daq_run;

  L3_SETUP  l3_setup;
  L3_RUN    l3_run;

  SC_SETUP  sc_setup;
  SC_RUN    sc_run;
};
  
/***********************************************************************************
 * TRG_CFG is the config file to be read by trigger
 ***********************************************************************************/

struct TRG_CFG
{
  SUBSYS_TASKS subsys_tasks;
  GLB_RUN   glb_run;
  GLB_SETUP glb_setup;
  TRG_RUN   trg_run;
  TRG_SETUP trg_setup;
};

/***********************************************************************************
 * DAQ_CFG is the config file to be read by DAQ
 ***********************************************************************************/

struct DAQ_CFG
{
  SUBSYS_TASKS subsys_tasks;
  GLB_RUN   glb_run;
  GLB_SETUP glb_setup;
  DAQ_RUN   daq_run;
  DAQ_SETUP daq_setup;
};

/***********************************************************************************
 * L3_CFG is the config file to be read by L3
 ***********************************************************************************/

struct L3_CFG
{
  SUBSYS_TASKS subsys_tasks;
  GLB_RUN   glb_run;
  GLB_SETUP glb_setup;
  L3_RUN    l3_run;
  L3_SETUP  l3_setup;
};

/***********************************************************************************
 * SC_CFG is the config file to be read by SC
 ***********************************************************************************/

struct SC_CFG
{
  SUBSYS_TASKS subsys_tasks;
  GLB_RUN   glb_run;
  GLB_SETUP glb_setup;
  SC_RUN    sc_run;
  SC_SETUP  sc_setup;
};
#endif





