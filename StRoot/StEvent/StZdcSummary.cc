/***************************************************************************
 *
 * $Id: StZdcSummary.cc,v 1.1 1999/01/15 20:40:32 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StZdcSummary.cc,v $
 * Revision 1.1  1999/01/15 20:40:32  wenaus
 * Commit Thomas' original code
 *
 **************************************************************************/
#include "StZdcSummary.hh"

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
