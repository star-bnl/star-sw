// $Id: StVirtualTriggerSimu.cxx,v 1.5 2009/12/22 18:10:57 pibero Exp $

#include <StMessMgr.h>

#include "StVirtualTriggerSimu.h"

ClassImp(StVirtualTriggerSimu)

bool StVirtualTriggerSimu::isTrigger(int trigId) {
    if (this->triggerDecision(trigId) == kYes) return true;
    return false;
}

void StVirtualTriggerSimu::Init() { /* no-op */ }

void StVirtualTriggerSimu::Clear() { /* no-op */ }

/*****************************************************************************
 * $Log: StVirtualTriggerSimu.cxx,v $
 * Revision 1.5  2009/12/22 18:10:57  pibero
 * Added ability to set input source (MuDst or StEvent) for BBC trigger simulator.
 *
 * Revision 1.4  2007/11/18 21:58:50  balewski
 * L2algos triggerId list fixed
 *
 * Revision 1.3  2007/11/08 20:59:34  kocolosk
 * subdet isTrigger returns a bool
 * triggerDecision returns enumerator including kDoNotCare
 *
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
