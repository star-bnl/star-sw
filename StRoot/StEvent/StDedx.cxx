/***************************************************************************
 *
 * $Id: StDedx.cxx,v 1.2 1999/02/09 19:59:03 fisyak Exp $
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
 * $Log: StDedx.cxx,v $
 * Revision 1.2  1999/02/09 19:59:03  fisyak
 * Import new Torre staff
 *
 * Revision 1.4  1999/04/28 22:27:29  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.2  1999/01/15 22:53:30  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#include "StDedx.h"
#ifdef __ROOT__

static const Char_t rcsid[] = "$Id: StDedx.cxx,v 1.2 1999/02/09 19:59:03 fisyak Exp $";
#endif

ClassImp(StDedx)

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

void StDedx::setNumberOfPointsUsed(UShort_t val) { mNumberOfPointsUsed = val; }

void StDedx::setMean(Float_t val) { mMean = val; }

void StDedx::setVariance(Float_t val) { mVariance = val; }

void StDedx::setStatus(ULong_t val) { mStatus = val; }
