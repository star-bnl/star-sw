/***************************************************************************
 *
 * $Id: StZdcSummary.cxx,v 1.4 1999/04/28 22:27:41 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StZdcSummary.cxx,v $
 * Revision 1.4  1999/04/28 22:27:41  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.4  1999/04/28 22:27:41  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.2  1999/01/15 22:54:29  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#include "StZdcSummary.h"

static const Char_t rcsid[] = "$Id: StZdcSummary.cxx,v 1.4 1999/04/28 22:27:41 fisyak Exp $";

ClassImp(StZdcSummary)

StZdcSummary::StZdcSummary()
{
    mAdcSumEast = 0;  
    mAdcSumWest = 0;  
    mAdcSum = 0;          
}

StZdcSummary::StZdcSummary(Float_t ae, Float_t aw, Float_t s)
{
    mAdcSumEast = ae;  
    mAdcSumWest = aw;  
    mAdcSum = s;          
}

StZdcSummary::~StZdcSummary() { /* noop */ }

void StZdcSummary::setAdcSum(StBeamDirection dir, Float_t val)
{
    if (dir == east)
	mAdcSumEast = val;
    else
	mAdcSumWest = val;
} 

void StZdcSummary::setAdcSum(Float_t val) { mAdcSum = val; }     
