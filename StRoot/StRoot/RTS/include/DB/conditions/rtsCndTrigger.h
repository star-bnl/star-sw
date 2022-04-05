#ifndef _RTSCNDTRIGGER_H_
#define _RTSCNDTRIGGER_H_

#include "rtsDbConstants.h"

struct rtsCndTrigger
{
  //////////////////////////
  int idx_rn;
  int idx_trigger;               
  //////////////////////////

  char name[DB_MAX_STR_LEN];

  // From TriggerData struct
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

/*   int TW_suggestion;          // Suggestion for the PW... not guarenteed to be followed. */
  int dataStream;             // 0 is default physics stream
/*   int copyToAllStreams;       // write twice or only once... */
/*   int neverCopyStreams;       // never copy to other streams... */
  int tokenZero;                 // satisfied by token 0?
  int calibration;               // calibration trigger?
  // End TriggerData struct

  // from Trigger struct
  unsigned int detectorLiveOnBits;
  unsigned int detectorLiveOffBits;
  unsigned int detectorRequest;
  
  // from TcdSetup struct
  int tcdId;
  int millisec;

  // Versions...
  int trgNameVersion;
  int trgVersion;
  int threshVersion;
  int psVersion;

  // l1/l2/l3
  int l0ps;
  float eff_ps;

  int l1_id;
  int l1_special_processing;
  float l1ps;

  int l2_id;
  int l2_special_processing;
  float l2ps;

  int l3_id;
  int l3_special_processing;
  float l3ps;
};

#endif
