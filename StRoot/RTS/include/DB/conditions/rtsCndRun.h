#ifndef _RTSCNDRUN_H_
#define _RTSCNDRUN_H_

#include "rtsDbConstants.h"

struct rtsCndRun
{ 
  //
  // concatentate GLB_RUN / GLB_SETUP / TRG_RUN / TRG_SETUP / 
  //              DAQ_RUN / DAQ_SETUP / L3_SETUP / L3_RUN
  //
  ///////////////
  int idx_rn;
  int cfg_time;
  ///////////////

  // GLB_RUN
  char GLB_SETUP_name[DB_MAX_STR_LEN];
  int run_number;
  float bField;                
  float base_trg_rate;
  int destination;

  // GLB_SETUP
  char DICT_name[DB_MAX_STR_LEN];
  char TRG_SETUP_name[DB_MAX_STR_LEN];
  char TRG_RUN_name[DB_MAX_STR_LEN];
  char TCD_SETUP_name[DB_MAX_STR_LEN];
  char DAQ_SETUP_name[DB_MAX_STR_LEN];
  char DAQ_RUN_name[DB_MAX_STR_LEN];
  char L3_RUN_name[DB_MAX_STR_LEN];
  char Expansion_name[DB_MAX_STR_LEN];
  
  // TRG_SETUP
  char TIER1_name[DB_MAX_STR_LEN];

  // TRG_RUN
  int configOpt;         // Force configuration depth

  int maxTknInSystem;    // Max token value allowed in system
  int minTknInSystem;    // Min token value allowed in system
  int tokenMod;          // Skip every n tokens
  int nTokensIn;
  int nTokenReturn;
  int tokenTimer;
  int clockSource;       // Clock source
  int numClockTicks;     // number of clock tick errors before setting flag
  int l2DataWrite;       // L2 data writing switch
  int dataWriteTimer;    // L2 clock ticks between data writing
  int everyNEvents;      // how many events to skip when writing

  int enableHalt;
  int enableContProt;
  int ContLength;

  // DAQ_SETUP
  int run_type;
 
  // DAQ_RUN
  int evp_suppression;
  int event_suppression;

  // L3_SETUP
  int gl3_formats;
  int gl3_formats_local;

  // L3_RUN
  int write_daq;
  int write_local;
  int l3_rate;

  // Hashes...
  unsigned int twhash;   // hash for picking up the twdefinitions

  // 10/04 -- these are now the index's for dict & stream name tables... 
  unsigned int dictHash;
  unsigned int dataStreamNamesHash;
  unsigned int tcdSetupHash;
};

#endif
