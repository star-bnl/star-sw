/***************************************************************************
 *
 * $Id: StVpdSummary.cxx,v 1.1 1999/01/30 03:58:10 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVpdSummary.cxx,v $
 * Revision 1.1  1999/01/30 03:58:10  fisyak
 * Root Version of StEvent
 *
 * Revision 1.3  1999/03/17 22:22:51  ullrich
 * Some cosmetics
 *
 * Revision 1.2  1999/01/15 22:54:26  wenaus
 * version with constructors for table-based loading
 *
static const Char_t rcsid[] = "$Id: StVpdSummary.cxx,v 1.1 1999/01/30 03:58:10 fisyak Exp $";

#ifdef __ROOT__
#include "StVpdSummary.h"

#endif
static const Char_t rcsid[] = "$Id: StVpdSummary.cxx,v 1.1 1999/01/30 03:58:10 fisyak Exp $";
ClassImp(StVpdSummary)

StVpdSummary::StVpdSummary()
{
    mMinimumTimeEast = 0;  
    mMinimumTimeWest = 0;  
StVpdSummary::StVpdSummary(Float_t ae, Float_t aw, Float_t s)
}
    mMinimumTimeEast = ae;  
    mMinimumTimeWest = aw;  
    mVertexZ = s;          
    mMinimumTimeEast = te;  
    mMinimumTimeWest = tw;  
    mVertexZ = z;          
}

StVpdSummary::~StVpdSummary() { /* noop */ }

void StVpdSummary::setMinimumTime(StBeamDirection dir, Float_t val)
{
    if (dir == east)
	mMinimumTimeEast = val;
    else
	mMinimumTimeWest = val;
}

void StVpdSummary::setVertexZ(Float_t val) { mVertexZ = val; }     
