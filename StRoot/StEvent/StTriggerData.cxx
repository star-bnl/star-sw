/***************************************************************************
 *
 * $Id: StTriggerData.cxx,v 2.2 2004/02/11 01:39:49 ullrich Exp $
 *
 * Author: Akio Ogawa, Feb 2003
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTriggerData.cxx,v $
 * Revision 2.2  2004/02/11 01:39:49  ullrich
 * Use enumeration StBeamDirector for east/west. Add member for ZDC vertex.
 *
 * Revision 2.1  2003/04/16 17:47:41  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StTriggerData.h"

static const char rcsid[] = "$Id: StTriggerData.cxx,v 2.2 2004/02/11 01:39:49 ullrich Exp $";

ClassImp(StTriggerData)

StTriggerData::StTriggerData() : mYear(0), mZdcVertexZ(-999) { /* noop */ }

StTriggerData::~StTriggerData() { /* noop */}

int StTriggerData::prepostAddress(int prepost) const
{ 
    if(prepost == 0) return 0;
    int npre = -numberOfPreXing();
    if(prepost < 0 && -prepost <= npre) return 1+npre+prepost;
    int npost = -numberOfPostXing();
    if(prepost > 0 &&  prepost <= npost) return npre+prepost;
    return -1;
}

