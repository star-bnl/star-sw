/***************************************************************************
 *
 * $Id: StZdcSummary.cc,v 1.2 1999/01/15 22:54:29 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StZdcSummary.cc,v $
 * Revision 1.2  1999/01/15 22:54:29  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#include "StEvent/StZdcSummary.hh"

static const char rcsid[] = "$Id: StZdcSummary.cc,v 1.2 1999/01/15 22:54:29 wenaus Exp $";

StZdcSummary::StZdcSummary()
{
    mAdcSumEast = 0;  
    mAdcSumWest = 0;  
    mAdcSum = 0;          
}

StZdcSummary::StZdcSummary(float ae, float aw, float s)
{
    mAdcSumEast = ae;  
    mAdcSumWest = aw;  
    mAdcSum = s;          
}

StZdcSummary::~StZdcSummary() { /* noop */ }

void StZdcSummary::setAdcSum(StBeamDirection dir, float val)
{
    if (dir == east)
	mAdcSumEast = val;
    else
	mAdcSumWest = val;
} 

void StZdcSummary::setAdcSum(float val) { mAdcSum = val; }     
