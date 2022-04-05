#ifndef _RTSCONDITIONS_H_
#define _RTSCONDITIONS_H_

/* RTS conditions database definitions */
/* JML: 3/2/01 */

#include "rtsDbConstants.h"
#include "rtsCndTasks.h"
#include "rtsCndTrgDictEntry.h"
#include "rtsCndTrigger.h"
#include "rtsCndLxUserFloat.h"
#include "rtsCndLxUserInt.h"
#include "rtsCndL1Rescale.h"
#include "rtsCndPwCondition.h"
#include "rtsCndPwLink.h"
#include "rtsCndTwLink.h"
#include "rtsCndDataStreamNames.h"
#include "rtsCndDaqDetSetup.h"
#include "rtsCndDaqSubdet.h"
#include "rtsCndEndRun.h"
#include "rtsHash.h"
#include "rtsCndRun.h"
#include "rtsCndTcdSetup.h"

struct rtsConditions 
{
  rtsCndTasks           nodes[DB_MAX_NODES];  // SUBSYS_TASKS

  rtsCndRun             run;         // GLB_SETUP

  rtsCndTrgDictEntry    dict[DB_MAX_DICT_ENTRIES];          // TRG_DICT

  rtsCndTrigger         triggers[DB_TRIGGERS_MAX];          // TRG_SETUP.triggers
  rtsCndLxUserFloat     lxFloats[15*DB_TRIGGERS_MAX];    // TRG_SETUP.triggers.lx.userFloat
  rtsCndLxUserInt       lxInts[15*DB_TRIGGERS_MAX];     // TRG_SETUP.triggers.lx.userInt

  rtsCndPwCondition     pwc[DB_PW_COND_MAX];                 // TRG_SETUP.triggers.L0conditions
  rtsCndDataStreamNames dataStreamNames[16];                // TRG_SETUP.dataStreamNames

  rtsCndDaqDetSetup     dets[DB_MAX_DETECTORS];  // DAQ_SETUP.detectors
  rtsCndDaqSubdet       subdets[DB_MAX_SUBDET];    // DAQ_RUN.detectors.subdet

  rtsCndEndRun          end;                // End of run

  // Hashes....
  rtsHash               threshHash;
  rtsHash               trgNameHash;
  rtsHash               trgHash;
  rtsHash               psHash;
  rtsHash               twHash;

  rtsCndTcdSetup        tcd[DB_MAX_DETECTORS];
  //------------------------------------
  // separate tables
  //------------------------------------
  //rtsCndL1Rescale       rescale[DB_TRIGGERS_MAX*DB_TW_MAX]; // TRG_SETUP.triggers.l1.eventTW...
  //rtsCndPwLink          pw[DB_PW_MAX];                     // TRG_SETUP.pwl
  //rtsCndTwLink          tw[DB_TW_MAX];                     // TRG_SETUP.twl
};

#endif
