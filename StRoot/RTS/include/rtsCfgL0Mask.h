#ifndef _RTSCFGL0MASK_H_
#define _RTSCFGL0MASK_H_

#include <trgConfNum.h>
#include <rtsSystems.h>

#define MAX_TRG_DETS 20

// Configuration file handling code
struct TrgDetMask {
    int id;                        // trigger id
    char trgname[20];              // trigger name
    //char name[MAX_CONF_NUM][20];   // crate names
    UINT32 mask[MAX_CONF_NUM];     // mask
};

int readDetToTriggerCrateFile(TrgDetMask *masks);



// The interface for the trigger nodes
// -1 if error
int configureTrgDetRequirements(int myConfNum);
UINT32 getBoardMaskForTrgDet(UINT32 trgDetMask);


#endif
