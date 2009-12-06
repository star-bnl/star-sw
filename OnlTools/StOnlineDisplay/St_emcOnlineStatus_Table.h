
#ifndef STAF_St_emcOnlineStatus_Table
#define STAF_St_emcOnlineStatus_Table

#include "TTable.h"

#include "emcOnlineStatus.h"

class St_emcOnlineStatus : public TTable
{
 public:
   ClassDefTable(St_emcOnlineStatus,emcOnlineStatus_st)
   ClassDef(St_emcOnlineStatus,2) //C++ wrapper for <emcOnlineStatus> table
};
#endif
