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

#define CFG_MAX_LABELS_LEGACY 200
#define MAX_TRG_DICT_ENTRIES_LEGACY 1500

#define CFG_MAX_LABELS 2000
#define MAX_TRG_DICT_ENTRIES 6000

#define MAX_ID 64
#define MAX_SUB 4
#define MAX_INST 100
#define MAX_TCD 20

#define MAX_NODES 400
#define MAX_EVBS 15

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


#define L1_DSM_OBJECT    1
#define BC1_DSM_OBJECT   2
#define MXQ_QT_OBJECT    3
#define MIX_DSM_OBJECT   4
#define BCW_DSM_OBJECT   5
#define BCE_DSM_OBJECT   6
#define EQ3_QT_OBJECT    7      // Changed EPQ > EQ3. JMN 8/15/17
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
#define EQ1_QT_OBJECT 38        // Changed FQ1 > EQ1.  JMN 8/15/17
#define EQ2_QT_OBJECT 39        // Changed FQ2 > EQ2.  JMN 8/15/17

#define BIT_OBJECT 100          // Reserved for TCU bit labels


/************************************************************************************
 * STAR_CFG fully defines the STAR run
 ************************************************************************************/

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

SimpleXmlDoc *getConfigFileXml(SimpleXmlDoc *xml, ic_msg *m, int sz=0);
SimpleXmlDoc *getConfigFileXml(SimpleXmlDoc *xml, char *filename, int sz=0);

// fills cfg, returns sizeof(cfg)
//int getConfigFile(STAR_CFG *cfg, ic_msg *m);
//int getConfigFile(STAR_CFG *cfg, char *filename);

// puts cfg into filename, returns sizeof(cfg)
//int putConfigFile(STAR_CFG *cfg, char *filename);

///////////////////////////////////////////////////////////////
// Trigger Definition stuff
///////////////////////////////////////////////////////////////


//struct TrgCfg;
struct TrgPS;

char *confNum2String(UINT32 conf_num);
int string2ConfNum(char *str);
UINT32 getCrateMask4TrgDet(int det);
int getConfNumForNode(int node);
UINT16 getTrgDetRequiredMask(char *node, int board=-1);

//void writeRCCNF(char *fn, STAR_CFG *cfg);   // write rc def file...
void writeRCCNF(char *fn, SimpleXmlDoc *xml);

//bool node_inrun(int node, STAR_CFG *cfg);
//bool system_inrun(int sys, STAR_CFG *cfg);
//void maskDetectorsInRun(STAR_CFG *cfg);
bool system_inrun(int sys, SimpleXmlDoc *xml);
void maskDetectorsInRun(SimpleXmlDoc *xml);


//bool cfgBuildPS(TrgPS *ps, RC_Config *rccfg);
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
char *ReadAllDotConf(int node, int task, char *param, char *result=(char *)NULL, char *paramfilename=(char *)"/RTS/conf/handler/all.conf");
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

  //void configure(STAR_CFG *cfg, int legacy);

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

  // private:
  int evbNodes[MAX_EVBS][5];          // idx to server by evb/server
  int nevbserv[MAX_EVBS];             // nservers by evb

  int evt_ctrs_by_server[MAX_EVBS];
  int weights_by_evb[MAX_EVBS];
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
    char marker[8];
    int idx;           // which counter
    int type;          // 1 counter, 2 scaler
    int source;        // 0 - 20 (evb),   100 (L0)
    float value;       // the value...
};

struct PrescaleReturnInfo {
    float ps[TRIGGERS_MAX];
    float measured_rate[TRIGGERS_MAX];

    float py_ps[TRIGGERS_MAX];
    float py_desiredRate[TRIGGERS_MAX];
    float py_scalerRate[TRIGGERS_MAX];
    float py_predictedRate[TRIGGERS_MAX];
};

#define HLT_UDP_PORT 8062

struct HLT_UDP_struct {
    int run_number;
    int nprimary;
    float xvertex;
    float yvertex;
    float zvertex;
};
#endif





