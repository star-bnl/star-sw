/***************************************************************************
 *
 * $Id: StVpdSummary.cxx,v 1.4 1999/04/28 22:27:40 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVpdSummary.cxx,v $
 * Revision 1.4  1999/04/28 22:27:40  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.4  1999/04/28 22:27:40  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.4  1999/03/19 21:30:51  fisyak
 * Fix typo
 *
 * Revision 1.3  1999/03/17 22:22:51  ullrich
 * Some cosmetics
 *
 * Revision 1.2  1999/01/15 22:54:26  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#include "StVpdSummary.h"

static const Char_t rcsid[] = "$Id: StVpdSummary.cxx,v 1.4 1999/04/28 22:27:40 fisyak Exp $";
ClassImp(StVpdSummary)

StVpdSummary::StVpdSummary()
{
    mMinimumTimeEast = 0;  
    mMinimumTimeWest = 0;  
    mVertexZ = 0;          
}

StVpdSummary::StVpdSummary(Float_t te, Float_t tw, Float_t z)
{
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
