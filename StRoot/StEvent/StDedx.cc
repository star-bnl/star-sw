/***************************************************************************
 *
 * $Id: StDedx.cc,v 1.2 1999/01/15 22:53:30 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 *
 * History:
 * 01/14/1999 T. Wenaus  Add table-based constructor
 *
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StDedx.cc,v $
 * Revision 1.2  1999/01/15 22:53:30  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#include "StEvent/StDedx.hh"

static const char rcsid[] = "$Id: StDedx.cc,v 1.2 1999/01/15 22:53:30 wenaus Exp $";

StDedx::StDedx()
{
    mNumberOfPointsUsed = 0;  
    mMean = 0;                
    mVariance = 0;            
    mStatus = 0;              
}

StDedx::~StDedx() { /* noop */ }

StDedx::StDedx(dst_dedx_st* dedx)
{
  mNumberOfPointsUsed = dedx->ndedx;
  mMean = dedx->dedx[0];
  mVariance = dedx->dedx[1];
  mStatus = dedx->iflag;
}

void StDedx::setNumberOfPointsUsed(unsigned short val) { mNumberOfPointsUsed = val; }

void StDedx::setMean(float val) { mMean = val; }

void StDedx::setVariance(float val) { mVariance = val; }

void StDedx::setStatus(unsigned long val) { mStatus = val; }
