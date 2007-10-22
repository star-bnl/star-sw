// $Id: StVirtualTriggerSimu.cxx,v 1.2 2007/10/22 23:09:48 balewski Exp $
//STAR
#include <StMessMgr.h>

#include "StVirtualTriggerSimu.h"

void StVirtualTriggerSimu::Init() { /* no-op */ }

//void StVirtualTriggerSimu::InitRun(int runnumber) {   LOG_INFO <<"L2VirtTrigSimu::Init() " <<endm; }

void StVirtualTriggerSimu::Clear() { /* no-op */ }

/*****************************************************************************
 * $Log: StVirtualTriggerSimu.cxx,v $
 * Revision 1.2  2007/10/22 23:09:48  balewski
 * split L2 to generic and year specific, not finished
 *
 * Revision 1.1  2007/10/12 17:12:30  kocolosk
 * rename ABC class for subdetector trigger simulators
 * StTriggerSimu => StVirtualTriggerSimu
 *
 * Revision 1.1  2007/09/24 18:32:06  kocolosk
 * added ABC class defining an interface for subdetector simulators
 *
 *****************************************************************************/
