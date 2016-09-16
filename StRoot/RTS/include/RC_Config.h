#ifndef RC_CONFIG_HH
#define RC_CONFIG_HH

#include <sys/types.h>
#include <rtsSystems.h>
#include <netinet/in.h>

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
#include <rts.h>

#define CONFIG_VERSION 2004

#ifdef RTS_PROJECT_PP
#define RC_PARAM_FILE_BASE_PATH "/PP/conf/handler/"
#define TRG_BASE_PATH "/PP/conf/handler"
#define DAQ_BASE_PATH "/PP/conf/handler"
#else
#define RC_PARAM_FILE_BASE_PATH "/RTS/conf/handler/"
#define TRG_BASE_PATH "/home/startrg/trg/handler/conf"
#define DAQ_BASE_PATH "/RTS/conf/handler"
#endif


#define TRG_CONFIG_FILE_NAME ""
#define DAQ_CONFIG_FILE_NAME ""
#define SC_CONFIG_FILE_NAME ""
#define L3_CONFIG_FILE_NAME ""

#define MAX_REGISTERS 1500
#define CFG_MAX_LABELS 200
#define MAX_TRG_DICT_ENTRIES 1500

#ifdef RTS_PROJECT_PP
#define MAX_ID 32
#else
#define MAX_ID 32
#endif
#define MAX_SUB 4
#define MAX_INST 100
#define MAX_TCD 20

#ifdef RTS_PROJECT_PP 
#define MAX_NODES 20
#else
#define MAX_NODES 200
#endif

#define MAX_THREADS 400
#define MAX_STR_LEN 40

#define TRIGGERS_MAX 64
#define OLD_TRIGGERS_MAX 32

#define EVP_GROUP_MAX 32         // Number of EVP groups...
#define CONDPERTRG_MAX 4         // Conditions per Trigger 
//#define PWDEF_BYTES TRIGGERS_MAX / 8
//#define TWDEF_BYTES TRIGGERS_MAX / 8   // Assume at most 4 detector groupings

//#define MAX_TW (1<<16)    /* in principle max values for pw/tw */
//#define MAX_PW (1<<12)

// #define NUM_PHYSICS_BITS 16    // correct values but not used...
// #define NUM_DET_BITS 12

#define RTS_STAT_TOKENS_DONE 8
#define RTS_STAT_FORCE 9
#define RTS_STAT_REQUEST 10

#define RTS_STARTRUN_FLAG    1
#define RTS_STOPRUN_FLAG     2
#define RTS_SENDCONFIG_FLAG  4
#define RTS_STOPRUN_SECOND_FLAG 8


typedef unsigned int UINT32;
typedef unsigned short UINT16;
typedef unsigned char UINT8;
typedef unsigned long long int UINT64;

class UINT128 {
 public:
  UINT32 x[4];

  UINT128() {
    zero();
  }

  void zero() {
    x[0] = 0;
    x[1] = 0;
    x[2] = 0;
    x[3] = 0;
  }
  
  int is_set(int bit) {
    int byt=bit/32;
    bit = bit % 32;
    
    return (x[byt] & (1<<bit)) ? 1 : 0;
  }

  void set(int bit) {
    int byt=bit/32;
    bit = bit % 32;
    
    x[byt] |= (1<<bit);
  }

  void clear(int bit) {
    int byt=bit/32;
    bit = bit % 32;
    x[byt] &= (~(1<<bit));
  }

  char *tostring(char *out = (char *)NULL) {
    static char _out[70];
    
    if(out == NULL) out = _out;
    sprintf(out, "%08x-%08x-%08x-%08x", x[3], x[2], x[1], x[0]);
    return out;
  }

};


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

struct GLB_SETUP 
{
  char DICT_name[MAX_STR_LEN];
  //char GLB_SETUP_name[MAX_STR_LEN];

  char TRG_SETUP_name[MAX_STR_LEN];
  char TRG_RUN_name[MAX_STR_LEN];
  char TCD_SETUP_name[MAX_STR_LEN];
  char DAQ_SETUP_name[MAX_STR_LEN];
  char DAQ_RUN_name[MAX_STR_LEN];
};

// Automatically filled in by GUI...
struct GLB_RUN
{
  char GLB_SETUP_name[MAX_STR_LEN];
  int run_number;
  float bField;                
  float base_trg_rate;
  int destination;
  //
  float drift_velocity_east;
  float drift_velocity_west;
  unsigned int rhic_clock;
  float reserved[10];
};

struct TRG_DICT_ENTRY
{
  UINT32 object;
  UINT32 index;
  UINT32 reg;
  char label[MAX_STR_LEN];
  int dfault;
  char comment[MAX_STR_LEN];
};

struct TRG_DICT
{
  //char name[MAX_STR_LEN];
  TRG_DICT_ENTRY entry[MAX_TRG_DICT_ENTRIES];
};

struct TcdSetup
{
  UINT32 tcdId;
  UINT32 millisec;
};

/* struct Expansion */
/* { */
/*   UINT32 x[1000]; */
/* }; */

#define L1_DSM_OBJECT    1
#define BC1_DSM_OBJECT   2
#define MXQ_QT_OBJECT    3
#define MIX_DSM_OBJECT   4
#define BCW_DSM_OBJECT   5
#define BCE_DSM_OBJECT   6
#define EPQ_QT_OBJECT    7
#define BBC_DSM_OBJECT   8
#define BBQ_QT_OBJECT    9
#define FMS_DSM_OBJECT  10
#define QT1_FMS_OBJECT  11
#define QT2_FMS_OBJECT  12
#define QT3_FMS_OBJECT  13
#define QT4_FMS_OBJECT  14
#define TRG_OBJECT 29
#define DAQ_OBJECT 30           // Reserved for DAQ use..
#define PHYSICS_BIT_OBJECT 32
#define DETECTOR_BIT_OBJECT 33
#define TCD_OBJECT 34
#define L1_OBJECT 35
#define L2_OBJECT 36
#define L3_OBJECT 37            // Reserved for L3 use...
#define FQ1_QT_OBJECT 38
#define FQ2_QT_OBJECT 39

#define BIT_OBJECT 100          // Reserved for TCU bit labels

struct RegValue
{
  UINT32 object;            // zero is invalid
  UINT32 index;
  UINT32 reg;
  UINT32 value;
};

struct TrgLabels
{
  char label[MAX_STR_LEN];
  int object;
  int value;
  int dfault;
  char comment[MAX_STR_LEN];
};

struct LxAlgorithm
{
  UINT32 id;                // zero is invalid
  int    userInt[5];
  float  userFloat[5];
  int    specialProcessing;
};

struct TwCondition
{
  UINT32 Pw;           // set to zero for invalid record
  UINT32 detectorLiveOnBits;
  UINT32 detectorLiveOffBits;
};

struct PwCondition
{
  UINT32 used;       // set to zero for invalid record  
  union {
    UINT32 onbits;
    UINT32 onbits_ex[4];
  };
  union {
    UINT32 offbits;
    UINT32 offbits_ex[4];
  };
};

struct AwCondition
{
  UINT32 TW;
  UINT32 PS;
  UINT32 AW;
  UINT32 pre;
  UINT32 post;
};

struct TriggerData
{
  int offlineBit;

  int desiredTrgCmd;             // these are subject to a later consistency check.
  int desiredDaqCmd;
  int desiredPre;            
  int desiredPost;

  float expected_L0_fraction; // fraction of ZDC's
  float desired_L0_rate;      
  float expected_L1_fraction;
  float desired_L1_rate;
  float expected_L2_fraction;
  float desired_L2_rate;
  float expected_L3_fraction;
  float desired_L3_rate;

  int res;          // Suggestion for the PW... not guarenteed to be followed.
  int dataStream;             // 0 is default physics stream
  int tokenZero;
  int calibration;       // never copy to other streams...
};

// User interface to the triggers
struct Trigger
{
  int used;

  char name[MAX_STR_LEN];
  
  TriggerData userdata;
  PwCondition L0conditions[CONDPERTRG_MAX];

  int detectorLiveOnBits;    // required to be alive
  int detectorLiveOffBits;   // required to be off
  int detectorRequest;       // these fire!
  
  LxAlgorithm l1;
  LxAlgorithm l2;
  LxAlgorithm l3;

  TcdSetup tcd;
};

struct EvpGroup
{
  char name[MAX_STR_LEN];    // name & id from /RTS/conf/handler/evpGroup.txt
  int id;                    // 
  int definition[2];    // bitmask of triggers...
  float rate;
};

struct TRG_SETUP 
{
  //char name[MAX_STR_LEN];                               // Name of this setup file

  char TIER1_name[MAX_STR_LEN];                         // Tier 1 filename
  
  Trigger triggers[TRIGGERS_MAX];
  
  PwCondition contaminationDef;        //-------------------------------------

  char dataStreamNames[16][MAX_STR_LEN];

  TrgLabels labels[CFG_MAX_LABELS];
  RegValue registers[MAX_REGISTERS];

  //  L3_SETUP l3_setup;
  EvpGroup evpGroup[EVP_GROUP_MAX];
};

struct TRG_RUN 
{
  // L1 and L2 User_Parameters
  int configOpt;         // Force configuration depth
    
  int QT_Ped_Offset;     // Pedestal offset for all QTs
  int res2;

  int useFastDMA;        // If 0, use slow DAM
  int res4;
    
  int res5;
  int res6;

  int res7;
  int res8;

  int tcuType;           // TCU: 1 (or 0) for Mk1; 2 for Mk2
  int disableTOFmask;    // 4 bit mask to disable individual TOF fibers

  int l2Disable750;      // Flag for experts to disable 750Hz limit
  int l2MyriAcc;         // L2 will use MyriNet for Accept/abort and not STP

  int l2LogLevel;        // Debug level for L2
  int l2DisableAlgos;    // Flag for experts to disable algorithms

  int scaler_log_level;  // Debug level for scaler VME software 

  int res15;
  int nToken;            // Maximum number of tokens to be used in the system
  int tokenBitsOn;       // Bits in token that must be present
  int tokenBitsOff;      // Bits in token that must be absent
  int useSTPnetwork;     // Use STP network 
  int res17;
  
  int clockSource;       // Clock source
  int res18;
  int stopRunOnError;    // Set to stop run/restart clock on error
    
  int l2DataWrite;       // L2 data writing switch
  int dataWriteTimer;    // Not used in trigger
  int everyNEvents;      // How many events to skip when writing
  int nAbort;            // Not used in trigger

  int res19;
  int res20;
  int EvpPolicy;         // Not used in trigger
 
};

struct TCD_ENTRY
{
  int phase;
  int gg_width;
  int daq_busy;
  int delay4;
  int delay8;
  int res1;
  int res2;
  int res3;
  int res4;
};

struct TCD_SETUP
{
  int tcd_log_level;
  int res1;
  int res2;
  int res3;
  int res4;
  int res5;
  TCD_ENTRY tcd[MAX_TCD];
};

//
// DAQ configuration
//

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

  int raw_write;   // write raw data? (n means write every n)
  int cl_write;    // write clusters  (n means write every n)
  int cl_done;     // do cluster finding
};

struct DAQ_SUBDET
{
  u_int rb_mask;
};

struct DAQ_DET
{
  int log_level;
  int use_l2;
  int use_sl3;

  int res1;
  int res2;
  int res3;
  int res4;
  int res5;
  int res6;
  int res7;
  int res8;
  int res9;
  int res10;

  DAQ_SUBDET sub[24];
};

struct DAQ_SETUP
{
  //char DAQ_SETUP_name[MAX_STR_LEN];
  int run_type;

  DAQ_DET_SETUP detectors[MAX_ID];
};

struct DAQ_DAQ
{
  int force_reset;
  int evp_suppression;
  int event_suppression;
  int evb_log_level;
  int myri_log_level;
  int bb_log_level;
  int gb_log_level;
  int evp_log_level;
  int mon_log_level;
  int write_pedestals;     // write pedestal events to evb?
  int res1;           // 0 followSetup, 1 all events, 2 1hz, 3 none
  int res2;           // emulate TPX?
  int res3;
  int res4;
  int res5;
};

struct DAQ_RUN
{ 
  DAQ_DAQ        daq;
  DAQ_DET        detectors[MAX_ID];
};


/************************************************************************************
 * STAR_CFG fully defines the STAR run
 ************************************************************************************/

#ifndef RTS_PROJECT_PP

struct STAR_CFG
{
  SUBSYS_TASKS subsys_tasks;  // HANDLER --> GUI at gui start
  GLB_RUN   glb_run;          // HANDLER --> GUI at gui start
  TRG_DICT  dict;             // GUI loads at editor panel load

  GLB_SETUP glb_setup;        // Modified the order so that referenced 

  TRG_SETUP trg_setup;
  TRG_RUN   trg_run;

  TCD_SETUP tcd_setup;

  DAQ_SETUP daq_setup;
  DAQ_RUN   daq_run;
  
  //L3_RUN    l3_run;
  
  //Expansion expansion;
};

#endif // !PP

#ifdef RTS_PROJECT_PP
struct PP_SETUP {
  //char name[MAX_STR_LEN];
  char daq_setup[MAX_STR_LEN];
  char filename[MAX_STR_LEN];
  u_int enable_mask;
  u_int resync;
  u_int p3;
  u_int p4;
  u_int p5;
  u_int p6;
};

struct PP_RUN {
  char SETUP_name[MAX_STR_LEN];
  int run_number;
  u_int p1;    // skip n events
  u_int p2;
  u_int p3;
  u_int p4;
  u_int p5;
  u_int p6;
};

struct PP_CFG
{
  SUBSYS_TASKS subsys_tasks;

  PP_SETUP setup;  
  PP_RUN   run;
  DAQ_SETUP daq_setup;
  DAQ_RUN   daq_run;
};

typedef PP_CFG STAR_CFG;
#endif

typedef STAR_CFG DAQ_CFG;
typedef STAR_CFG TRG_CFG;
typedef STAR_CFG SC_CFG;
typedef STAR_CFG L3_CFG;

typedef STAR_CFG RC_Config;

// SWAPS...
#ifdef RTS_LITTLE_ENDIAN

void swapTASK(TASK *);
void swapSUBSYS_TASKS(SUBSYS_TASKS *);
void swapGLB_SETUP(GLB_SETUP *);
void swapGLB_RUN(GLB_RUN *);
void swapTRG_DICT_ENTRY(TRG_DICT_ENTRY *);
void swapTRG_DICT(TRG_DICT *);
void swapTcdSetup(TcdSetup *);
void swapRegValue(RegValue *);
void swapTrgLabels(TrgLabels *);
void swapLxAlgorithm(LxAlgorithm *);
void swapTwCondition(TwCondition *);
void swapPwCondition(PwCondition *);
void swapAwCondition(AwCondition *);
void swapTriggerData(TriggerData *);
void swapTrigger(Trigger *);
void swapTRG_SETUP(TRG_SETUP *);
void swapTRG_RUN(TRG_RUN *);
void swapTCD_SETUP(TCD_SETUP *);
void swapTCD_ENTRY(TCD_ENTRY *);
void swapDAQ_DET_SETUP(DAQ_DET_SETUP *);
void swapDAQ_DET(DAQ_DET *);
void swapDAQ_SETUP(DAQ_SETUP *);
void swapDAQ_DAQ(DAQ_DAQ *);
void swapDAQ_RUN(DAQ_RUN *);
void swapSTAR_CFG(STAR_CFG *);

#ifdef RTS_PROJECT_PP
void swapPP_SETUP(PP_SETUP *);
void swapPP_RUN(PP_RUN *);
void swapPP_CFG(PP_CFG *);
#endif

#else
#define swapTASK(yada)
#define swapSUBSYS_TASKS(yada)
#define swapGLB_SETUP(yada)
#define swapGLB_RUN(yada)
#define swapTRG_DICT_ENTRY(yada)
#define swapTRG_DICT(yada)
#define swapTcdSetup(yada)
#define swapRegValue(yada)
#define swapTrgLabels(yada)
#define swapL1Algorithm(yada)
#define swapLxAlgorithm(yada)
#define swapTwCondition(yada)
#define swapPwCondition(yada)
#define swapAwCondition(yada)
#define swapTriggerData(yada)
#define swapTrigger(yada)
#define swapTRG_SETUP(yada)
#define swapTRG_RUN(yada)
#define swapTCD_ENTRY(yada)
#define swapTCD_SETUP(yada)
#define swapDAQ_DET_SETUP(yada)
#define swapDAQ_DET(yada)
#define swapDAQ_SETUP(yada)
#define swapDAQ_DAQ(yada)
#define swapDAQ_RUN(yada)
#define swapSTAR_CFG(yada)
#define swapPP_SETUP(yada)
#define swapPP_RUN(yada)
#define swapPP_CFG(yada)
#endif

class SimpleXmlDoc;

// Configuration file functions...The functions get properly swapped
// versions of the configuration file.
//
// The file is stored big endian, even (stupidly?) on little endian 
// machines...
//

// fills filename
struct ic_msg;

char *cmd2name(int cmd);
char *getConfigBaseXml(char *filename, int port, int trg);
char *getConfigBaseXml(char *filename, ic_msg *m);
char *getConfigBase(char *filename, int port, int trg);
char *getConfigBase(char *filename, ic_msg *m);

// fills cfg, returns sizeof(cfg)
int getConfigFile(STAR_CFG *cfg, ic_msg *m);
int getConfigFile(STAR_CFG *cfg, char *filename);

// puts cfg into filename, returns sizeof(cfg)
int putConfigFile(STAR_CFG *cfg, char *filename);

///////////////////////////////////////////////////////////////
// Trigger Definition stuff
///////////////////////////////////////////////////////////////


//struct TrgCfg;
struct TrgPS;

UINT16 getTrgDetRequiredMask(char *node);

void writeRCCNF(char *fn, STAR_CFG *cfg);   // write rc def file...

bool node_inrun(int node, STAR_CFG *cfg);
bool system_inrun(int sys, STAR_CFG *cfg);
void maskDetectorsInRun(STAR_CFG *cfg);
bool system_inrun(int sys, SimpleXmlDoc *xml);
void maskDetectorsInRun(SimpleXmlDoc *xml);


bool cfgBuildPS(TrgPS *ps, RC_Config *rccfg);
bool cfgBuildPS(TrgPS *ps, SimpleXmlDoc *xml);



struct TrgPSEntry {
  int l0ps;
  float l1ps;    // This is the floating version of the L0PS.
  float l2ps;
  float l3ps;
};

struct TrgPS {
  TrgPSEntry ps[TRIGGERS_MAX];
};

struct EthServer
{
  UINT16 node;
  UINT16 task;
  UINT32 ip;
  UINT32 port;
};

struct EthServerName
{
  char name[40];
  EthServer server;
};

// Reads from all.conf
// returns -1 if no server, 0 if server exists.
int getEthServer(int node, int task, EthServer *eth);   
char *ReadAllDotConf(int node, int task, char *param, char *result=(char *)NULL, char *paramfilename="/RTS/conf/handler/all.conf");
int getAllEthServers(EthServerName *list, int max);

///////////////////////////////////////////////////

//
//  token based EVB determination for LXSB & GB
//

class EvbChooser
{
 public:
    EvbChooser() {weights_set = 0; };
  void configure(SimpleXmlDoc *xml);

  void configure(STAR_CFG *cfg, int legacy);

  // These return error if no evbs in run, but otherwise
  // return a valid EVB even if the token is invalid
  EthServer *choose(int token); // returns ptr (NULL on error)
  int chooseIdx(int token);     // returns idx (-1 on error)

  // This returns an error if the token is invalid.
  int chooseIdx_proper(int token);

  EthServer servers[50];        // linearized servers
  int nservers;

  int getIdxByNode(int evbnode);
  
  int getEvb4Token(int token) { return evb4token[token]; }

 private:
  int evbNodes[10][5];          // idx to server by evb/server
  int nevbserv[10];             // nservers by evb

  int evt_ctrs_by_server[10];
  int weights_by_evb[10];
  int weights_set;

  void readWeights();
  void clearWeightedEvbEvents();
  int nextWeightedServer(int seq, int token);
  
  int nevbs;
  int evb4token[4096];         
};

#define RHIC_TRG "RHICTRG\0"
#define TRG_UDP_PORT 8060

struct RHIC_Trigger {
  char marker[4];
  int tm;
  int evb;
  int count;
  int mask;
};

struct PrescaleReturnInfo {
    float ps[TRIGGERS_MAX];
    float measured_rate[TRIGGERS_MAX];
};

#endif





