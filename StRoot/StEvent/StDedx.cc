/***************************************************************************
 *
 * $Id: StDedx.cc,v 1.1 1999/01/15 20:39:39 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StDedx.cc,v $
 * Revision 1.1  1999/01/15 20:39:39  wenaus
 * Commit Thomas' original code
 *
 **************************************************************************/
#include "StDedx.hh"

StDedx::StDedx()
{
    mNumberOfPointsUsed = 0;  
    mMean = 0;                
    mVariance = 0;            
    mStatus = 0;              
}

StDedx::~StDedx() { /* noop */ }

void StDedx::setNumberOfPointsUsed(unsigned short val) { mNumberOfPointsUsed = val; }

void StDedx::setMean(float val) { mMean = val; }

void StDedx::setVariance(float val) { mVariance = val; }

void StDedx::setStatus(unsigned long val) { mStatus = val; }
