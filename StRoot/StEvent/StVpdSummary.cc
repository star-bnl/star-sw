/***************************************************************************
 *
 * $Id: StVpdSummary.cc,v 1.2 1999/01/15 22:54:26 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVpdSummary.cc,v $
 * Revision 1.2  1999/01/15 22:54:26  wenaus
 * version with constructors for table-based loading
 *
 * Revision 1.2  1999/01/15 22:54:26  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#include "StEvent/StVpdSummary.hh"

static const char rcsid[] = "$Id: StVpdSummary.cc,v 1.2 1999/01/15 22:54:26 wenaus Exp $";

StVpdSummary::StVpdSummary()
{
    mMinimumTimeEast = 0;  
    mMinimumTimeWest = 0;  
    mVertexZ = 0;          
}

StVpdSummary::StVpdSummary(float ae, float aw, float s)
{
    mMinimumTimeEast = ae;  
    mMinimumTimeWest = aw;  
    mVertexZ = s;          
}

StVpdSummary::~StVpdSummary() { /* noop */ }

void StVpdSummary::setMinimumTime(StBeamDirection dir, float val)
{
    if (dir == east)
	mMinimumTimeEast = val;
    else
	mMinimumTimeWest = val;
}

void StVpdSummary::setVertexZ(float val) { mVertexZ = val; }     
